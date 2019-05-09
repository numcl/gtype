#|

This file is a part of NUMCL project.
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

NUMCL is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation,either version 3 of the License, or (at your option) any
later version.

NUMCL is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
NUMCL.  If not, see <http://www.gnu.org/licenses/>.

|#

(in-package :cl-user)
(defpackage gtype
  (:use :cl :trivialib.type-unify :trivial-cltl2 :trivia :alexandria :iterate :type-r)
  (:export
   #:gtype
   #:resolve
   #:resolving
   #:in-compile-time))
(in-package :gtype)

;; blah blah blah.

(defmacro in-compile-time ((&optional env) &body body)
  (with-gensyms (macro)
    `(macrolet ((,macro (&environment ,env)
                  ,@body))
       (,macro))))

#|
(proclaim '(declaration bogus))

(trivial-cltl2:define-declaration bogus (specifier env)
  (declare (ignorable env))
  ;; (print specifier)
  (values :declare specifier))

(defun bogus ()
  (declare (bogus a))
  (declare (bogus b))                   ;the second bogus will overwrite the value
   (in-compile-time (e)
     `',(print (declaration-information 'bogus e))))



(proclaim '(declaration bogus2))

(trivial-cltl2:define-declaration bogus2 (specifier env)
  (declare (ignorable env))
  ;; (print specifier)
  (ematch specifier
    ((list* 'bogus2 info)
     (values :declare (list* 'bogus2 (append info (declaration-information 'bogus2 env)))))))

(defun bogus2 ()
  (declare (bogus2 a))
  (declare (bogus2 b))                   ; this way you can manually append information
   (in-compile-time (e)
     `',(print (declaration-information 'bogus2 e))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun variablep (symbol)
  (match symbol
    ((symbol :name (string* #\?))
     t)))

(proclaim '(declaration gtype))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; design sketch

#|
;; the function should look like this:

(defun fn2 (a b)
  (declare (gtype (array ?t) a))
  (declare (gtype ?t b))
  (* a b))

;; ideally, defun, let, locally, lambda etc. should hook into it.

;; note: on sbcl, trivial LOCALLY is not removed

(defun fn (a b)
  (declare (optimize (speed 3)))
  ;; (declare (fixnum a b))
  (locally
      (declare (fixnum a b))
    (max a b)))

|#
