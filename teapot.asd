;;;; teapot.asd

(asdf:defsystem #:teapot
  :description "Describe teapot here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:serapeum #:yacc)
  :components ((:file "package")
               (:file "teapot")))
