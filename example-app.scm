(include "pastiche.scm")
(import pastiche)
(use awful doctype)

(pastiche "/" "paste.db"
          awful-settings:
          (lambda (handler)
            (parameterize
                ((debug-file "/tmp/paste")
                 (page-css "chicken.css")
                 (page-doctype doctype-html)
                 (page-charset "UTF-8"))
              (handler))))
                     
