(asdf:defsystem #:kakeibo
  :description "Managing my personal household accounts"
  :author "Masaya Tojo"
  :license  "BSD"
  :version "0.0.0"
  :depends-on (#:coalton)
  :serial t
  :pathname "src/"
  :components ((:file "class/valid")
               (:file "entity/date")))
