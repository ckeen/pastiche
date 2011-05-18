(module pastiche (pastiche)

(import chicken scheme)

(use awful
     colorize
     html-utils
     html-tags
     miscmacros
     simple-sha1
     sql-de-lite
     spiffy
     tcp
     awful-sql-de-lite
     sql-de-lite
     files
     posix
     data-structures
     utils
     extras
     (srfi 1 13))


;;;
;;; Captchas
;;;
(define-record captcha string figlet)

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


;;;
;;; Pastiche
;;;
(define (pastiche base-path db-file
                  #!key (vandusen-port 22722)
		        (vandusen-host "localhost")
                        (base-url "http://paste.call-cc.org")
                        (use-captcha? #t)
                        (num-captchas 500)
                        (awful-settings (lambda (_) (_))))

  (parameterize ((app-root-path base-path))

    (add-request-handler-hook!
     'awful-paste
     (lambda (path handler)
       (when (string-prefix? base-path path)
         (switch-to-sql-de-lite-database)
         (parameterize ((app-root-path base-path)
                        (db-credentials db-file)
                        (page-css "http://wiki.call-cc.org/chicken.css"))
           (awful-settings handler)))))

    (define figlet-installed?
      (handle-exceptions exn
        #f
        (system* "figlet -v 2>&1 > /dev/null")))

    (when (and use-captcha? (not figlet-installed?))
      (print "WARNING: `use-captcha?' indicates that captchas are enabled but figlet "
             "doesn't seem to be installed. Disabling captchas.")
      (set! use-captcha? #f))

    (define captchas (and use-captcha? (create-captchas num-captchas)))

    ;; The database needs to be initialised once
    (unless (file-exists? db-file)
      (let ((db (open-database db-file)))
        (exec (sql db "create table pastes(hash text, author text, title text, time float, paste text)"))
        (close-database db)))


    (define (notify nick title url)
      (when vandusen-host
	    (ignore-errors
	     (let ((stuff (sprintf "#chicken ~s pasted ~s ~a"
				   nick title (make-pathname base-url url))))
	       (let-values (((i o) (tcp-connect vandusen-host vandusen-port)))
			   (display stuff o)
			   (newline o)
			   (close-input-port i)
			   (close-output-port o))))))

    (define (fetch-last-pastes n)
      (let ((r ($db "select * from pastes order by time desc limit ?" values: (list n))))
        r))

    (define (make-post-table n)
      (define (format-row r)
        (list (second r)                ; Nickname
              (link (make-pathname base-path (string-append "/paste?id=" (first r)))
                    (third r))             ; title
              (seconds->string (fourth r)))) ;date

      (<div> class: "paste-table"
             (or
              (tabularize (map format-row (fetch-last-pastes n))
                          header: '("Nickname" "Title" "Date"))
              (<p> "No pastes so far."))))

    (define (recent-pastes n)
      (<div> class: "paste-list"
             (<h2> "The last " n " pastes so far: ")
             (make-post-table n)))

    (define (paste-form #!key annotate-id)
      (let* ((hash/captcha (and use-captcha? (get-captcha captchas)))
             (captcha-hash (and use-captcha? (car hash/captcha)))
             (captcha (and use-captcha? (cdr hash/captcha))))
        (<div> class: "paste-form"
               (<h2> "Enter a new " (if annotate-id " annotation:" " paste:"))
               (form
                (++ (if use-captcha?
                        (hidden-input 'captcha-hash captcha-hash)
                        "")
                    (tabularize
                     (append
                      `(( "Your nick: " ,(text-input 'nick))
                        ( "The title of your paste:" ,(text-input 'title) )
			( ,(++ "Your paste " (<i> "(mandatory)" " :"))
                          ,(<textarea> id: "paste" name: "paste"  cols: 60 rows: 24)))
			(if use-captcha?
                          `(( "Type in the text below:" ,(text-input 'captcha-user-answer))
                            ("" ,(<pre> id: "captcha" (captcha-figlet captcha))))
                          '())
                        `(("" ,(if vandusen-host
                                 (<input> name: "notify-irc"
                                          type: "checkbox"
                                          checked: "checked"
                                          "Please notify the #chicken channel on freenode.")
                                 ""))
                        ,(list (if annotate-id (hidden-input 'id annotate-id) "")
                               (submit-input value: "Submit paste!"))))))
                action: (make-pathname base-path "paste")
                method: "post"))))

    (define (fetch-paste id)
      (and id
           (let ((r ($db "select * from pastes where hash=? order by time desc" values: (list id))))
             (or (null? r) r))))

    (define (update-paste id snippet)
      (insert-paste id snippet))

    (define (insert-paste id paste)
      (let ((author (first paste))
            (title (second paste))
            (time (third paste))
            (paste (fourth paste)))
        ($db "insert into pastes (hash, author, title, time, paste) values (?,?,?,?,?)"
             values: (list id author title time paste))))

    (define (bail-out . reasons)
      (++ (<h1> "Ooops, something went wrong") (<br>)
          (<div> id: "failure-reason" (fold (lambda (i r)
                                              (++ r (sprintf "~a" i)))
                                            "" reasons))
          "I am sorry for his, you better go back."))


    (define (print-snippet s #!key annotation? (count 0))
      (++ (<div> class: "paste-header"
                 (second s) (if annotation? " added " " pasted ")
                 (<a> name: (if annotation? (->string count) "") (third s))
                 " on " (seconds->string (fourth s)))
          (<div> class: "paste"
                 (<pre> (<tt> class: "highlight scheme-language" (html-colorize 'scheme (fifth s)))))
          (<div> class: "paste-footer"
                 " [ "
                 (link (make-pathname base-path
                                      (string-append "paste?id=" (first s) "#" (->string count)))
                       "permalink")
                 " | "
                 (link (make-pathname base-path
                                      (string-append "raw?id=" (first s) "&annotation=" (->string count)))
                       "raw")
                 " ] ")))

    (define (format-all-snippets snippets)
      (fold (let ((c (length snippets)))
              (lambda (p s)
                (set! c (sub1 c))
                (++ (print-snippet p annotation?: (not (= c (- (length (car snippets)) 1))) count: c) s)))
            ""
            snippets))

    (define-page "/" ;; the main page, prefixed by base-path
      (lambda ()
        (<div> id: "content" (<h1> id: "heading" "Welcome to the chicken scheme pasting service")
               (<p> id: "subheading" (<small> "Home of lost parentheses"))
               (++ (or (and-let* ((id ($ 'id))
                                  (annotate ($ 'annotate)))
                         (cond ((fetch-paste id)
                                => (lambda (p)
                                     (++ (format-all-snippets p)
                                         (<h2> "Your annotation:")
                                         (paste-form annotate-id: id))))
                               (else (bail-out "Found no paste to annotate with this id."))))
                       (++ (recent-pastes 10)
                           (paste-form))))))
      title: "Pastiche: the Chicken Scheme pasting service")

    (define-page "paste"
      (lambda ()
        (with-request-variables ((nick (nonempty as-string))
                                 (title (nonempty as-string))
                                 paste
                                 id)
          (html-page
           (<div> id: "content"
                  (or (and-let* ((nick (and nick (htmlize nick)))
                                 (title (and title (htmlize title)))
                                 (time (current-seconds))
                                 (hashsum (string->sha1sum
                                           (++ nick title (->string time) paste)))
                                 (url '())
                                 (snippet (map
                                           (lambda (i)
                                             (if (and (string? i) (string-null? i))
                                                 "anonymous"
                                                 i))
                                           (list nick title time paste))))
                        (if (and use-captcha?
                                 (not (equal? ($ 'captcha-user-answer)
                                              (and-let* ((hash ($ 'captcha-hash))
                                                         (captcha (alist-ref hash captchas equal?)))
                                                (captcha-string captcha)))))
                            (bail-out "Wrong captcha answer.")
                            (if (string-null? paste)
                                (bail-out "I am not storing empty pastes.")
                                (begin (cond ((fetch-paste id)
                                              => (lambda (p)
                                                   (let ((count (length (cdr p))))
                                                     (update-paste id snippet)
                                                     (set! url (make-pathname
                                                                base-path
                                                                (++ "paste?id=" id "#" (->string count)))))))
                                             (else (insert-paste hashsum snippet)
                                                   (set! url (++ "paste?id=" hashsum))))
                                       (when ($ 'notify-irc) (notify nick title url))
                                       (++  (<h1> "Thanks for your paste!")
                                            "Hi " nick (<br>) "Thanks for pasting: " (<em> title) (<br>)
                                            "Your paste can be reached with this url: " (link url url))))))
                      (cond ((fetch-paste id)
                             => (lambda (p)
                                  (++
                                   (<h2> "Showing pastes for " id)
                                   (format-all-snippets p)
                                   (<div> id: "paste-footer"
                                          (<h2> (link (++ base-path "?id=" id
                                                          ";annotate=t") "Add an annotation to this paste!"))))))
                            (else (bail-out "Could not find a paste with this id:" id))))
                  (<p> (link base-path "Main page")))
           css: (page-css)
           title: (conc "Pastes for " id))))
      no-template: #t)

    (define-page "raw"
      (lambda ()
        (awful-response-headers '((content-type "text/plain")))
        (let* ((id ($ 'id))
               (annotation ($ 'annotation as-number))
               (paste (fetch-paste id)))
          (or (and paste annotation (<= annotation (length paste)) (fifth (list-ref (reverse paste) annotation)))
              paste
              (++ (bail-out "Could not find a paste with id " id)
                  (<p> (link base-path "Main page"))))))
      no-template: #t)
    )))
