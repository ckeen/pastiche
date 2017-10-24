(module pastiche (pastiche)

(import chicken scheme)

(use awful
     colorize
     html-parser
     intarweb
     miscmacros
     simple-sha1
     sql-de-lite
     spiffy
     tcp
     awful-sql-de-lite
     sql-de-lite
     files
     ports
     posix
     data-structures
     utils
     extras
     irregex
     (srfi 1 13)
     utf8
     uri-common)

;;;
;;; Utils
;;;
(define (tabularize data #!key header)
  (append '(table)
          (if header
              `((tr ,@(map (lambda (item) `(th ,item)) header)))
              '())
          (let ((body
                 (map (lambda (line)
                        (append '(tr)
                                (map (lambda (cell) `(td ,cell)) line)))
                      data)))
            body)))

(define (text-input id)
  `(input (@ (type "text")
             (name ,id)
             (maxlength 78)
             (id ,id))))

(define (hidden-input id value)
  `(input (@ (type "hidden")
             (id ,id)
             (name ,id)
             (value ,value))))

(define (preferred-languages accept-language-contents)
  (let ((qualities (fold (lambda (x s)
                           (cons
                            (cons (get-value x)
                                  (or
                                   (get-param 'q x)
                                   1))
                            s))
                         '()
                         accept-language-contents)))
    (sort qualities (lambda (a b)
                      (> (cdr a) (cdr b))))))

(define (find-espeak-languages executable data-dir)
  (let* ((raw-input (with-input-from-pipe (string-append executable
                                                         " --version")
                                          read-line))
         (derived-data-dir (and (not (eof-object? raw-input))
                                (last (string-split raw-input " ")))))
    (cond ((or (and data-dir (directory-exists? data-dir))
               (and derived-data-dir (directory-exists? derived-data-dir))) =>
           (lambda (dir)
             (find-files (make-pathname dir "voices") action: (lambda (f s) (cons (pathname-strip-directory f) s)) test: file-exists?)))
          (derived-data-dir ;; executable has been found, but directory does not exist
           '("en"))
          (else
           (error "audible captchas have been configured but the call to espeak did not work.")))))

(define (select-preferred-language available preferences)
  (or (find (cut member <> available) (map (lambda (p) (symbol->string (car p))) preferences)) "en"))

(define espeak-available-languages '())

(define (is-it-spam? nick title paste)
  (define (url? s)
    (and-let* ((uri (uri-reference s)))
              (uri-scheme uri)))
  (define (too-many? txt spam-ratio)
    (let* ((tokens (string-split txt))
           (100% (length tokens))
           (url-count (length (filter url? tokens))))
      (and (not (zero? 100%))
           (< spam-ratio (/ url-count 100%)))))
  (or (too-many? nick 0)
      (too-many? title 0)
      (too-many? paste 0.1)))


;;;
;;; Captchas
;;;
(define-record captcha string figlet)

(define (tool-exists? tool)
  (let ((paths (string-split (get-environment-variable "PATH")
                             (if (eq? (software-type) 'windows)
                                 ";"
                                 ":"))))
    (let loop ((paths paths))
      (if (null? paths)
          #f
          (let ((path (car paths)))
            (or (file-exists? (make-pathname path tool))
                (loop (cdr paths))))))))

(define (create-captchas num #!key (min-captcha-len 4) (max-captcha-len 8))
  ;; returns an alist mapping captcha hashes to captcha records

  (define chars '#(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                   #\n #\o #\p #\q #\r #\s #\t #\u #\v #\x #\y #\z))

  (define random-captcha
    (let ((chars-len (vector-length chars)))
      (lambda ()
        (list->string
         (let loop ((n (+ min-captcha-len
                          (random (- max-captcha-len
                                     min-captcha-len)))))
           (if (zero? n)
               '()
               (cons (vector-ref chars (random chars-len))
                     (loop (- n 1)))))))))

  (define (figlet str)
    (call-with-input-pipe (string-append "figlet " str) read-all))

  (let loop ((n (sub1 num)))
    (if (zero? n)
        '()
        (let ((captcha-string (random-captcha)))
          (cons
           (cons (string->sha1sum captcha-string)
                 (make-captcha captcha-string
                               (figlet captcha-string)))
           (loop (- n 1)))))))

(define (get-captcha captchas)
  (list-ref captchas (random (length captchas))))

(define (string-as-wav espeak-binary s preferred-languages)
  (let-values (((in out pid) (process espeak-binary `("-s 10" "--stdout" "-v"
                                                 ,(select-preferred-language espeak-available-languages preferred-languages)))))
    (fprintf out "~s" (list->string (intersperse (string->list s) #\space)))
    (close-output-port out)
    (let ((r (read-all in)))
      (close-input-port in)
      r)))

;;;
;;; Pastiche
;;;
(define (pastiche base-path db-file
                  #!key (vandusen-port 22722)
                        (vandusen-host "localhost")
                        (base-url "http://paste.call-cc.org")
                        (use-captcha? #t)
                        (audible-captcha? use-captcha?)
                        (espeak-binary "espeak")
                        (espeak-data-dir #f)
                        (num-captchas 500)
                        (browsing-steps 15)
                        force-vandusen-notification?
                        (awful-settings (lambda (_) (_))))

  (define (delete-and-refill-captchas clist captcha)
    (if (= 1 (length clist))
        (create-captchas num-captchas)
        (alist-delete captcha clist)))

  (define base-path-pattern
    (irregex (string-append (string-chomp base-path "/") "(/.*)*")))

  (define-app pastiche
    matcher: (lambda (path)
               (irregex-match base-path-pattern path))
    handler-hook: (lambda (handler)
                    (switch-to-sql-de-lite-database)
                    (parameterize ((app-root-path base-path)
                                   (enable-sxml #t)
                                   (db-credentials db-file)
                                   (page-css "//wiki.call-cc.org/chicken.css"))
                      (awful-settings handler)))

    (when (and use-captcha? (not (tool-exists? "figlet")))
      (print "WARNING: `use-captcha?' indicates that captchas are enabled but figlet "
             "doesn't seem to be installed. Disabling captchas.")
      (set! use-captcha? #f)
      (set! audible-captcha? #f))

    (when (and audible-captcha? (not (tool-exists? "espeak")))
      (print "WARNING: `audible-captcha?' indicates that audible captchas are enabled but espeak "
             "doesn't seem to be installed. Disabling audible captchas.")
      (set! audible-captcha? #f))

    (when audible-captcha?
      (set! espeak-available-languages (find-espeak-languages espeak-binary espeak-data-dir)))

    (when (and force-vandusen-notification?
               (or (not vandusen-host)
                   (not vandusen-port)))
      (error 'pastiche
             "`force-vandusen-notification?' requires both `vandusen-host' and `vandusen-port' to be set."))

    (define captchas (and use-captcha? (create-captchas num-captchas)))

    ;; The database needs to be initialised once
    (unless (file-exists? db-file)
      (let ((db (open-database db-file)))
        (exec (sql db "create table pastes(hash text, author text, title text, time float, paste text)"))
        (exec (sql db "create virtual table searchable using fts3(hash, author, title, time, paste)"))
        (close-database db)))

    (define (notify nick title url)
      (when vandusen-host
        (let ((cleaned-nick (with-input-from-string nick html-strip))
              (cleaned-title (with-input-from-string title html-strip)))
            (ignore-errors
             (let ((stuff (sprintf "#chicken ~s pasted ~s ~a"
                                   cleaned-nick cleaned-title (make-pathname base-url url))))
               (let-values (((i o) (tcp-connect vandusen-host vandusen-port)))
                           (display stuff o)
                           (newline o)
                           (close-input-port i)
                           (close-output-port o)))))))


    (define (matching-pastes query)
      ($db "select * from searchable where searchable match ? order by time desc" values: (list query)))

    (define (make-search-result-table results)
      (define (format-row r)
        (list (second r)                   ; Nickname
              `(a (@ (href ,(make-pathname base-path (string-append "/paste?id=" (first r))))
                     (id "paste-url"))
                  ,(third r))              ; title
              (prettify-time (fourth r)))) ; date

      `(div (@ (class "paste-table"))
            ,(or
              (tabularize (map format-row results)
                          header: '("Nickname" "Title" "Date"))
              '(p "No pastes found matching your query."))))


; old "select * from pastes order by time desc limit ?,?"
    (define (fetch-last-pastes count #!key (offset 0))
      (let ((r ($db "select * from pastes p where time = (select min(time) from pastes p2 where p2.hash=p.hash) order by time desc limit ?,?" values: (list offset count))))
        r))

    (define (make-post-table n #!key (offset 0))
      (define (format-row r)
        (list (second r)                   ; Nickname
              `(a (@ (href ,(make-pathname base-path (string-append "/paste?id=" (first r))))
                     (id "paste-url"))
                  ,(third r))              ; title
              (prettify-time (fourth r)))) ; date

      `(div (@ (class "paste-table"))
            ,(or
              (tabularize (map format-row (fetch-last-pastes n offset: offset))
                          header: '("Nickname" "Title" "Date"))
              '(p "No pastes so far."))))

    (define (navigation-links)
      `(div (@ (id "menu"))
            (ul ,@(map (lambda (m)
                         `(li (a (@ (href ,(make-pathname base-path (car m))))
                                 ,(cdr m))))
                       '(("" . "New Paste")
                         ("browse" . "Browse")
                         ("search" . "Search")
                         ("about" . "What is this?"))))))

    (define (recent-pastes n)
      `(div (@ (class "paste-list"))
            (h2 "The last " ,n " pastes so far: ")
            ,(make-post-table n)))

    (define (paste-form #!key annotate-id)
      (let* ((hash/captcha (and use-captcha? (get-captcha captchas)))
             (captcha-hash (and use-captcha? (car hash/captcha)))
             (captcha (and use-captcha? (cdr hash/captcha))))
        `(div (@ (class "paste-form"))
              (h2 "Enter a new " ,(if annotate-id " annotation:" " paste:"))
              (form (@ (method "post")
                       (action ,(make-pathname base-path "paste")))
                    ,(if use-captcha?
                         (hidden-input 'captcha-hash captcha-hash)
                         '())
                    ,(tabularize
                      (append
                       `(("Your nick: " ,(text-input 'nick))
                         ("The title of your paste:" ,(text-input 'title) )
                         (("Your paste " (i "(mandatory)" " :"))
                          (textarea (@ (id "paste")
                                       (name "paste")
                                       (cols 60)
                                       (rows 24)))))
                       (if use-captcha?
                           `(("Type in the text below:" ,(text-input 'captcha-user-answer))
                             ("" (pre (@ (id "captcha"))
                                      ,(captcha-figlet captcha)))
                             ,(if audible-captcha?
                                  `("Visually impaired? Let me spell it for you (wav file)"
                                    (audio (@ (src ,(make-pathname base-path
                                                                   (sprintf "captcha?hash=~a.wav" captcha-hash)))
                                              (preload  "metadata")
                                              (controls "controls")))
                                    (a (@ (href ,(make-pathname base-path
                                                                (sprintf "captcha?hash=~a.wav" captcha-hash))))
                                       "download WAV"))
                                  '()))
                           '())
                       `(("" ,(if force-vandusen-notification?
                                  (hidden-input 'notify-irc "yes")
                                  (if vandusen-host
                                      `(input (@ (name "notify-irc")
                                                 (type "checkbox")
                                                 (checked "checked"))
                                              "Please notify the #chicken channel on freenode.")
                                      '())))
                         (,(if annotate-id
                               (hidden-input 'id annotate-id)
                               '())
                          ((input (@ (type "submit")
                                     (value "Submit paste!"))))))))))))

    (define (fetch-paste id)
      (and id
           (let ((r ($db "select * from pastes where hash=? order by time desc" values: (list id))))
             (and (not (null? r)) r))))

    (define (update-paste id snippet)
      (insert-paste id snippet))

    (define (insert-paste id paste)
      (let ((author (first paste))
            (title (second paste))
            (time (third paste))
            (paste (fourth paste)))
        ($db "insert into pastes (hash, author, title, time, paste) values (?,?,?,?,?)"
             values: (list id author title time paste))
        ($db "insert into searchable (hash, author, title, time, paste) values (?,?,?,?,?)"
             values: (list id author title time paste))))

    (define (bail-out . reasons)
      `((h1 "Ooops, something went wrong")
        (br)
        (div (@ (id "failure-reason"))
             ,(fold (lambda (i r)
                      (sprintf "~a~a" r i))
                    "" reasons))
        "I am sorry for this, you better go back."))

    (define (prettify-time t)
      (let* ((delta (- (current-seconds) t))
             (fits
              (lambda (d l)
                (let ((r (inexact->exact (floor (/ delta d)))))
                  (if (and (< 0 r) (>= l r)) r #f)))))
        (cond ((fits (* 60 60 24) 3) =>
               (lambda (d) (sprintf "~a days ago" d)))
              ((fits (* 60 60) 24) =>
               (lambda (hrs)
                 (sprintf "~a hours ago" hrs)))
              ((fits 60 (* 60 5)) => (lambda (m) (sprintf "~a minutes ago" m)))
              ((fits 1 120) => (lambda (_) (sprintf "just now!")))
              (else (sprintf "on ~a" (seconds->string t))))))

    (define (print-snippet s #!key annotation? (count 0))
      `((div (@ (class "paste-header"))
             (h3 (a (@ (name ,(sprintf "a~A" count)))
                    ,(third s))
                 ,(if annotation? " added " " pasted ") " by " ,(second s) " "
                 ,(prettify-time (fourth s)))
             (div (@ (class "paste"))
                  (pre (tt (@ (class "highlight scheme-language"))
                           (literal ,(if (< (string-length (fifth s)) 5000) ;; only colorize if the paste isn't too long
                                         (html-colorize 'scheme (fifth s))
                                         (fifth s))))))
             (div (@ (class "paste-footer"))
                  " [ "
                  (a (@ (href ,(make-pathname base-path
                                              (sprintf "paste?id=~a#a~a" (first s) count))))
                     "permalink")
                  " | "
                  (a (@ (href ,(make-pathname base-path
                                              (sprintf "raw?id=~a&annotation=~a" (first s) count))))
                     "raw")
                  " ] "))))

    (define (format-all-snippets snippets)
      (fold (let ((c (length snippets)))
              (lambda (p s)
                (set! c (sub1 c))
                `(,(print-snippet p annotation?: (= c (- (length snippets) 1)) count: c)
                  ,s)))
            '()
            snippets))

    (define-page "/" ;; the main page, prefixed by base-path
      (lambda ()
        `(,(navigation-links)
          (div (@ (id "content"))
               (h1 (@ (id "heading")
                      (align "center"))
                   "Welcome to the CHICKEN Scheme pasting service")
               (,(or (and-let* ((id ($ 'id))
                                (annotate ($ 'annotate)))
                       (cond ((fetch-paste id)
                              => (lambda (p)
                                   `(,(format-all-snippets p)
                                     (h2 "Your annotation:")
                                     ,(paste-form annotate-id: id))))
                              (else (bail-out "Found no paste to annotate with this id."))))
                     (paste-form))))))
      title: "Pastiche: the CHICKEN Scheme pasting service")

    (define-page "paste"
      (lambda ()
        (let ((paste-title "Untitled paste"))
          (set-page-title! paste-title)
          (with-request-variables ((nick  (nonempty as-string))
                                   (title (nonempty as-string))
                                   (paste (nonempty as-string))
                                   (id    (nonempty as-string)))
            `(,(navigation-links)
	      (div (@ (id "content"))
                   ,(cond
                     ((and id (not paste))
                      (cond ((fetch-paste id)
                             => (lambda (p)
                                  (set! paste-title (third (last p)))
                                  (set-page-title! paste-title)
                                  `(,(format-all-snippets p)
                                    (div (@ (id "paste-footer"))
                                         (h2 (@ (align "center"))
                                             (a (@ (href ,(sprintf "~a?id=~a;annotate=t"
                                                                   base-path
                                                                   id)))
                                                "Annotate this paste!"))))))
                            (else (bail-out "Could not find a paste with this id: " id))))
                     (paste
                      (if (and use-captcha?
                               (not (equal? ($ 'captcha-user-answer)
                                            (and-let* ((hash ($ 'captcha-hash))
                                                       (captcha (alist-ref hash captchas equal?)))
                                              (captcha-string captcha)))))
                          (bail-out "Wrong captcha answer.")
                          (let* ((nick (or nick "anonymous"))
                                 (title (or title "no title"))
                                 (time (current-seconds))
                                 (hashsum (string->sha1sum
                                           (conc nick title time paste)))
                                 (url '())
                                 (snippet (map
                                           (lambda (i)
                                             (if (and (string? i) (string-null? i))
                                                 "anonymous"
                                                 i))
                                           (list nick title time paste))))
                            (cond ((string-null? paste)
                                   (bail-out "I am not storing empty pastes."))
                                  ((is-it-spam? nick title paste)
                                   `((h2 (@ (align "center")) "Thanks for your paste!")
                                      (p "Hi " ,nick ", thanks for pasting: " (em ,title) (br))))
                                  (else
                                    (cond ((fetch-paste id)
                                           => (lambda (p)
                                                (let ((count (+ 1 (length (cdr p)))))
                                                  (update-paste id snippet)
                                                  (set! url
                                                    (make-pathname
                                                      base-path
                                                      (conc "paste?id=" id "#a" count))))))
                                          (else (insert-paste hashsum snippet)
                                            (set! url
                                              (make-pathname base-path
                                                             (string-append "paste?id=" hashsum)))))
                                    (set! paste-title title)
                                    (when ($ 'notify-irc) (notify nick title url))
                                    (when use-captcha?
                                      (set! captchas
                                        (delete-and-refill-captchas captchas ($ 'captcha-hash))))
                                    `((h2 (@ (align "center")) "Thanks for your paste!")
                                      (p "Hi " ,nick ", thanks for pasting: " (em ,title) (br))
                                      (p (@ (align "center"))
                                         "Your paste can be reached with this url: "
                                         (a (@ (href ,url)) ,url))))))))
                     (else (bail-out "I am not storing empty pastes."))))))))
      css: (page-css)
      method: '(get head post))

    (define (convert-newlines text mode)
      (and text
           (irregex-replace/all "\r\n"
                                text
                                (case mode
                                  ((#:unix) "\n")
                                  ((#:dos) "\r\n")
                                  (else (error "unknown newline mode " mode))))))

    (define-page "raw"
      (lambda ()
        (awful-response-headers '((content-type "text/plain")))
        (let* ((id ($ 'id))
               (annotation ($ 'annotation as-number))
               (paste (fetch-paste id)))
          (or (and paste
                   annotation
                   (<= annotation (length paste))
                   `(literal
                     ,(convert-newlines (fifth (list-ref (reverse paste) annotation)) #:unix)))
              `(literal
                ,(convert-newlines paste #:unix))
              (bail-out "Could not find a paste with id " id))))
      no-template: #t)

    (define (number-of-posts)
      (let ((n ($db "select count(distinct(hash)) from pastes")))
        (and n (caar n))))

    (define-page "browse"
      (lambda ()
        (with-request-variables ((from as-number)
                                 (to as-number))
          (let* ((nposts (number-of-posts))
                 (from (if (and from (>= from 0) (<= from nposts)) from 0))
                 (to (if (and to (> to from) (<= to nposts)) to browsing-steps))
                 (older-to (min (+ to browsing-steps) nposts))
                 (older-from (+ from browsing-steps))
                 (newer-from (- from browsing-steps))
                 (newer-to (max (- to browsing-steps) browsing-steps))
                 (history-path (make-pathname base-path "browse")))
            `(,(navigation-links)
              (div (@ (id "content"))
                   (h2 (@ (align "center")) "Browsing pastes")
                   (div (@ (id "browse-navigation")
                           (align "center"))
                        ,(if (>= newer-from 0)
                             `(a (@ (href ,(sprintf "~a?from=~a;to=~a"
                                                    history-path
                                                    newer-from newer-to)))
                                 "< newer")
                             "< newer")
                        " ... "
                        ,(if (and (not (= to nposts)) (<= older-to nposts))
                             `(a (@ (href ,(sprintf "~a?from=~a;to=~a"
                                                    history-path
                                                    older-from
                                                    older-to)))
                                 "older >")
                             "older >")
                        ,(make-post-table browsing-steps offset: from))))))))

    (define-page "search"
      (lambda ()
        (with-request-variables ((query as-string))
          (let ((results (and query (matching-pastes query))))
             `(,(navigation-links)
               (div (@ (id "content")
                       (align "center"))
                    (form (@ (method "post")
                             (action ,(make-pathname base-path "search")))
                          (input (@ (type "text")
                                    (name query)
                                    (maxlength 78)
                                    (id query)
                                    (autofocus "autofocus")
                                    (size 40)
                                    (value ,query)))
                          (input (@ (type "submit")
                                    (value "Search!"))))
                    ,(if (and query (pair? results))
                         `((h2 (@ (align "center")) "Search results for '" ,query "' " ,(length results) " results")
                           ,(make-search-result-table results))
                         '(p "No results have matched.")))))))
      method: '(get head post))

    (define-page "about"
      (lambda ()
        `(,(navigation-links)
          (div (@ (id "content"))
               (h2 "You have reached the CHICKEN Scheme pasting service")
               (p "These pages are maintained by the CHICKEN Scheme
                   project team. Anyone that enters a correct CAPTCHA response is allowed
                   to post anything he likes. If you find objectionable content, feel
                   free to drop a mail at chicken-janitors <at> nongnu dot org")
               (p "The source code for these pages is
                   distributed under a BSD license at C-Keen's "
                  (a (@ (href "https://github.com/ckeen/pastiche")) "github repo"))
               (p "Our thanks go to chandler for the famous "
                  (a (@ (href "http://paste.lisp.org")) "lisppaste") " "
                  (a (@ (href "http://www.cliki.net/lisppaste")) "(cliki page)")
                  " bot and the same disclaimer applies:")
               (p "Lisppaste pastes can be made by anyone at
                   any time. Imagine a fearsomely comprehensive disclaimer of
                  liability. Now fear, comprehensively."))))
      title: "About Pastiche")

    (define-page "captcha"
      (lambda ()
        (if audible-captcha?
            (with-request-variables
             ((hash as-string))
             (let*
                 ((splitted (string-split hash "."))
                  (hash (car splitted))
                  (extension (or (null? (cdr splitted)) (cadr splitted))))
               (cond ((and
                       (equal? extension "wav")
                       (alist-ref (car (string-split hash ".")) captchas equal?)) =>
                       (lambda (c)
                         (awful-response-headers '((content-type "audio/wav")))
                         `(literal ,(string-as-wav espeak-binary
                                                   (captcha-string c)
                                                   (preferred-languages (header-contents 'accept-language (request-headers (current-request))))))))
                     (else (bail-out "Wrong captcha hash, please reload the page and try again")))))
            (bail-out "Audio captchas have been disabled in the configuration.")))
      no-template: #t)
    ) ;; end define-app

  ) ;; end pastiche

) ;; end module

