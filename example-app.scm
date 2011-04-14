(use awful pastiche)

(pastiche "/" "paste.db"
          awful-settings:
          (lambda (handler)
            (parameterize
                ((debug-file "/tmp/paste")
                 (page-css "http://wiki.call-cc.org/chicken.css")
                 (page-charset "UTF-8")
                 (page-doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))
              (handler))))
                     
