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
     (srfi 1 13))


(define (pastiche base-path db-file
                  #!key (vandusen-port 22722)
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

    ;; The database needs to be initialised once
    (unless (file-exists? db-file)
      (let ((db (open-database db-file)))
        (exec (sql db "create table pastes(hash text, author text, title text, time float, paste text)"))
        (close-database db)))


    (define (notify nick title url)
      (ignore-errors
       (let ((stuff (sprintf "#chicken ~a posted \"~a\" ~a" nick title url)))
         (let-values (((i o) (tcp-connect "localhost" vandusen-port)))
           (display stuff o)
           (close-input-port i)
           (close-output-port o)))))

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
      (<div> class: "paste-form"
             (<h2> "Enter a new " (if annotate-id " annotation:" " paste:"))
             (form (tabularize
                    `(( "Your nick: " ,(text-input 'nick))
                      ( "The title of your paste:" ,(text-input 'title) )
                      ( ,(++ "Your paste " (<i> "(mandatory)" " :"))
                        ,(<textarea> id: "paste" name: "paste"  cols: 60 rows: 24))
                      ("" ,(<input> name: "notify-irc"
                                    type: "checkbox"
                                    checked: "checked"
                                    "Please notify the #chicken channel on freenode."))
                      ,(list (if annotate-id (hidden-input 'id annotate-id) "")
                             (submit-input value: "Submit paste!"))))
                   action: (make-pathname base-path "paste")
                   method: "post")))

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
          "I am sorry for his, you "
          (link base-path "better go back.")))


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
                                     (++  (format-all-snippets p)
                                          (<h2> "Your annotation:")
                                          (paste-form annotate-id: id))))
                               (else (bail-out "Found no paste to annotate with this id."))))
                       (++ (recent-pastes 10)
                           (paste-form)))))))

    (define-page "paste"
      (lambda ()
        (<div> id: "content"
               (or (and-let* ((nick  (and ($ 'nick) (htmlize ($ 'nick))))
                              (title (and ($ 'title) (htmlize ($ 'title))))
                              (paste ($ 'paste))
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
                     (if (string-null? paste)
                         (bail-out "I am not storing empty pastes.")
                         (begin (cond ((fetch-paste ($ 'id))
                                       => (lambda (p)
                                            (let ((count (length (cdr p))))
                                              (update-paste ($ 'id) snippet)
                                              (set! url (make-pathname
                                                         base-path
                                                         (++ "paste?id=" ($ 'id) "#" (->string count)))))))
                                      (else (insert-paste hashsum snippet)
                                            (set! url (++ "paste?id=" hashsum))))
                                (when ($ 'notify-irc) (notify nick title url))
                                (++  (<h1> "Thanks for your paste!")
                                     "Hi " nick (<br>) "Thanks for pasting: " (<em> title) (<br>)
                                     "Your paste can be reached with this url: " (link url url)))))
                   (cond ((fetch-paste ($ 'id))
                          => (lambda (p)
                               (++
                                (<h2> "Showing pastes for " ($ 'id))
                                (format-all-snippets p)
                                (<div> id: "paste-footer"
                                       (<h2> (link (++ base-path "?id=" ($ 'id)
                                                       ";annotate=t") "Add an annotation to this paste!"))))))
                         (else (bail-out "Could not find a paste with this id:" ($ 'id)))))
               (<p> (link base-path "Main page")))))

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
