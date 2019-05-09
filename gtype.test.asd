#|
  This file is a part of gtype project.
  Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage gtype.test-asd
  (:use :cl :asdf))
(in-package :gtype.test-asd)


(defsystem gtype.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of gtype"
  :license "LLGPL"
  :depends-on (:gtype
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :gtype)"))
))
