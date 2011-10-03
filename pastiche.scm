(module pastiche (pastiche)

(import chicken scheme)

(use awful
     colorize
     html-utils
     html-parser
     html-tags
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
			(browsing-steps 15)
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

; old "select * from pastes order by time desc limit ?,?"
    (define (fetch-last-pastes from to)
      (let ((r ($db "select * from pastes p where time = (select min(time) from pastes p2 where p2.hash=p.hash) order by time desc limit ?,?" values: (list from to))))
        r))

    (define (make-post-table n #!optional (from 0))
      (define (format-row r)
        (list (second r)                ; Nickname
              (link (make-pathname base-path (string-append "/paste?id=" (first r)))
                    (third r))             ; title
              (prettify-time (fourth r)))) ;date

      (<div> class: "paste-table"
             (or
              (tabularize (map format-row (fetch-last-pastes from n))
                          header: '("Nickname" "Title" "Date"))
              (<p> "No pastes so far."))))

    (define (navigation-links)
      (<div> id: "menu"
	     (<ul>
	      (apply ++ (map (lambda (m)
			       (<li> (link (make-pathname base-path (car m))
					   (cdr m))))
			     '(("" . "New Paste")
			       ("browse" . "Browse pastes")
			       ("about" . "What is this?")))))))
    
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
	     (and (not (null? r)) r))))

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
      (++ (<div> class: "paste-header"
                 (<h3> (<a> name: (sprintf "a~A" count) (third s)))
                 (if annotation? " added " " pasted ") " by " (second s) " "
                 (prettify-time (fourth s)))
          (<div> class: "paste"
                 (<pre> (<tt> class: "highlight scheme-language" (html-colorize 'scheme (fifth s)))))
          (<div> class: "paste-footer"
                 " [ "
                 (link (make-pathname base-path
                                      (string-append "paste?id=" (first s) "#a" (->string count)))
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
                (++ (print-snippet p annotation?: (= c (- (length snippets) 1)) count: c) s)))
            ""
            snippets))

    (define-page "/" ;; the main page, prefixed by base-path
      (lambda ()
        (++ 
	 (<div> id: "content" (<h1> id: "heading" align: "center"
				    "Welcome to the chicken scheme pasting service")
		(++ (or (and-let* ((id ($ 'id))
				   (annotate ($ 'annotate)))
				  (cond ((fetch-paste id)
					 => (lambda (p)
					      (++ (format-all-snippets p)
						  (<h2> "Your annotation:")
						  (paste-form annotate-id: id))))
					(else (bail-out "Found no paste to annotate with this id."))))
			(paste-form))))
	 (navigation-links)))
      title: "Pastiche: the Chicken Scheme pasting service")

    (define-page "paste"
      (lambda ()
        (let ((paste-title "Untitled paste"))
          (with-request-variables ((nick  (nonempty as-string))
                                   (title (nonempty as-string))
                                   paste
                                   id)
                                  (html-page
                                   (++
                                    (<div> id: "content"
                                           (or (and-let* ((nick (or (and nick (htmlize nick)) "anonymous"))
                                                          (title (or (and title (htmlize title)) "no title"))
                                                          (time (current-seconds))
                                                          (paste (and (not (equal? "" paste)) paste))
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
                                                                            (let ((count (+ 1 (length (cdr p)))))
                                                                              (update-paste id snippet)
                                                                              (set! url (make-pathname
                                                                                         base-path
                                                                                         (++ "paste?id=" id "#a" (->string count)))))))
                                                                      (else (insert-paste hashsum snippet)
                                                                            (set! url (make-pathname base-path (++ "paste?id=" hashsum)))))
                                                                (set! paste-title title)
                                                                (when ($ 'notify-irc) (notify nick title url))
                                                                (++  (<h2> align: "center" "Thanks for your paste!")
                                                                     (<p> "Hi " nick ", thanks for pasting: " (<em> title) (<br>))
                                                                     (<p> align: "center") "Your paste can be reached with this url: " (link url url))))))
                                               (cond ((fetch-paste id)
                                                      => (lambda (p)
                                                           (set! paste-title (third (last p)))
                                                           (++
                                                            (format-all-snippets p)
                                                            (<div> id: "paste-footer"
                                                                   (<h2> align: "center" 
                                                                         (link (++ base-path "?id=" id
                                                                                   ";annotate=t") "Annotate this paste!"))))))
                                                     (else (bail-out "Could not find a paste with this id:" id)))))
                                    (navigation-links))
                                   css: (page-css)
                                   title: paste-title))))
      no-template: #t)

    (define-page "raw"
      (lambda ()
        (awful-response-headers '((content-type "text/plain")))
        (let* ((id ($ 'id))
               (annotation ($ 'annotation as-number))
               (paste (fetch-paste id)))
          (or (and paste annotation (<= annotation (length paste)) (fifth (list-ref (reverse paste) annotation)))
              paste
              (bail-out "Could not find a paste with id " id))))
      no-template: #t)

    (define (number-of-posts)
      (let ((n ($db "select count(hash) from pastes")))
	(and n (caar n))))    

    (define-page "browse"
      (lambda ()
	(with-request-variables
	 ((from as-number)
	  (to as-number))
	 (let* ((nposts (number-of-posts))
		(from (if (and from (>= from 0) (<= from nposts)) from 0))
		(to (if (and to (> to from) (<= to nposts)) to browsing-steps))
		(older-to (min (+ to browsing-steps) nposts))
		(older-from (+ from browsing-steps))
		(newer-from (- from browsing-steps))
		(newer-to (max (- to browsing-steps) browsing-steps))
		(history-path (make-pathname base-path "browse")))
	   (html-page
	    (++ (<div> id: "content"
		       (<h2> align: "center" "Browsing pastes")
		       (<div> id: "browse-navigation"
			      align: "center"
			      (if (>= newer-from 0) (link  (sprintf "~a?from=~a;to=~a" history-path newer-from newer-to)
							   "< newer")
				  "< newer")
			      " ... "
			      (if (and (not (= to nposts)) (<= older-to nposts))
				  (link (sprintf "~a?from=~a;to=~a" history-path older-from older-to)
					"older >")
				  "older >"))
		       (make-post-table to from))
		(navigation-links)))))))
    (define-page "about"
      (lambda ()
	(html-page
	 (++ (<div> id: "content"
		    (<h2> "You have reached the CHICKEN scheme pasting service")
		    (<p> (htmlize "These pages are maintained by the CHICKEN scheme
                      project team. Anyone that enters a correct CAPTCHA response is allowed
                      to post anything he likes. If you find objectionable content, feel
                      free to drop a mail at chicken-janitors <at> nongnu dot org"))
		    (<p> "The source code for these pages is
                          distributed under a BSD license at C-Keen's "
			 (link "https://github.com/ckeen/pastiche" "github repo"))
		    (<p> "Our thanks go to chandler for the famous "
			 (link "http://paste.lisp.org" "lisppaste") " "
			 (link "http://www.cliki.net/lisppaste" "(cliki page)")
			 " bot and the same disclaimer applies:")
		    (<p> "Lisppaste pastes can be made by anyone at
                          any time. Imagine a fearsomely comprehensive disclaimer of
                          liability. Now fear, comprehensively."))
	     (navigation-links))))))))


