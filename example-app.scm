(include "pastiche.scm")
(import pastiche)
(import awful)

(pastiche "/" "paste.db"
          awful-settings:
          (lambda (handler)
            (parameterize
                ((debug-file "/tmp/paste")
                 (page-css "chicken.css")
                 (page-doctype "<!DOCTYPE html>")
                 (page-charset "UTF-8"))
              (handler))))

