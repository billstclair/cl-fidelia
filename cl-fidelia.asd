(asdf:defsystem :cl-fidelia
  :description "Read the Fidelia current song playing"
  :author "Bill St. Clair <bill@billstclair.com>"
  :version "1.0.0"
  :license "Apache"
  :depends-on (s-xml)
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "fidelia")))))
