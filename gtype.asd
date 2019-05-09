;;;; Autogenerated ASD file for system "GTYPE"
;;;; In order to regenerate it, run update-asdf
;;;; from shell (see https://github.com/phoe-krk/asd-generator)
;;;; For those who do not have update-asdf,
;;;; run `ros install asd-generator` (if you have roswell installed)
;;;; There are also an interface available from lisp:
;;;; (asd-generator:regen &key im-sure)
(defsystem gtype
 :version "0.1"
 :author "Masataro Asai"
 :mailto "guicho2.71828@gmail.com"
 :license "LGPL"
 :defsystem-depends-on ()
 :depends-on (:trivialib.type-unify
              :trivial-cltl2
              :trivia
              :alexandria
              :iterate
              :type-r)
 :serial t
 :components ((:module "src"
               :components ((:file "package")
                            (:file "resolve"))))
 :description "C++/Julia-like parametric types in CL, based on CLtL2 extensions"
 :in-order-to ((test-op (test-op :gtype.test))))
