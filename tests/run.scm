(use awful pastiche server-test)

(define pastiche-db "pastiche.db")

(define (run-pastiche! #!optional use-captcha?)
  (delete-file* pastiche-db)
  (pastiche "/" pastiche-db
            use-captcha?: use-captcha?
            awful-settings:
            (lambda (handler)
              (parameterize
                  ((page-css "http://wiki.call-cc.org/chicken.css")
                   (page-charset "UTF-8")
                   (page-doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))
                (handler)))))

;;;
;;; Without captcha
;;;
(with-test-server
 (lambda ()
   (awful-start run-pastiche!))
 (lambda ()
   (load "client.scm")))

(test-exit)
