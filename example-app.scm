(use awful pastiche doctype)

(pastiche "/" "paste.db"
          awful-settings:
          (lambda (handler)
            (parameterize
                ((debug-file "/tmp/paste")
                 (page-css "http://wiki.call-cc.org/chicken.css")
                 (page-doctype doctype-html)
                 (page-charset "UTF-8"))
              (handler))))
                     
