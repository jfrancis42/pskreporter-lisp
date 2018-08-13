;;;; pskreporter.asd

(asdf:defsystem #:pskreporter
  :description "Library for talking to pskreporter.info"
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :version "0.0.1"
  :serial t
  :depends-on (#:xmls #:drakma #:split-sequence #:local-time #:jeffutils)
  :components ((:file "package")
               (:file "pskreporter")))
