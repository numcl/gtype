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

#|

A strategy that separates gtype declarations and resolve macros
to add type checks

|#

(in-package :gtype)

(defun find-lexical-variables (env)
  #+sbcl
  (mapcar #'car
          (sb-c::lexenv-vars
           (sb-c::coerce-to-lexenv env)))
  #+ccl
  (let ((vars (ccl::lexenv.variables env)))
    (when (listp vars)
      (append (iter (for v in vars)
                    (for bits = (ccl::var-bits v))
                    (when (and (typep bits 'integer)
			       (not (logbitp ccl::$vbittemporary bits))
			       ;; (not (logbitp ccl::$vbitignoreunuused bits))
			       ;; (not (logbitp ccl::$vbitignore bits))
			       ;; (not (logbitp ccl::$vbitspecial bits))
                               )
                      (collect (ccl::var-name v))))
              (find-lexical-variables (ccl::lexenv.parent-env env)))))
  #-(or sbcl ccl)
  (error "this implementation is not supported. Pull request is welcome"))

(trivial-cltl2:define-declaration gtype (specifier env)
  (declare (ignorable env))
  ;; (print specifier)
  (ematch specifier
    ((list* 'gtype gtype variables)
     ;; If the first value is :VARIABLE or :FUNCTION then the second value should be a
     ;; list of elements of the form (BINDING-NAME KEY VALUE). conses (KEY . VALUE)
     ;; will be added to the alist returned by:
     (values :variable
             (iter (for v in variables)
                   (collecting
                    (list v 'gtype gtype))
                   ;; also add a regular type specifier
                   (collecting
                    (list v 'type (subst-if '* #'variablep gtype))))))))

(defun gvars (env)
  (iter
    (for v in (find-lexical-variables env))
    (for (values binding local-p alist) = (variable-information v env))
    (for key-value-pair = (assoc 'gtype alist))
    (when key-value-pair
      (collect v))))

(defun gtemplates (env)
  (iter
    (for v in (find-lexical-variables env))
    (for (values binding local-p alist) = (variable-information v env))
    (for key-value-pair = (assoc 'gtype alist))
    (when key-value-pair
      (collect (cdr key-value-pair)))))

(defmacro resolve (&environment env &body body)
  (multiple-value-bind (body decls) (alexandria:parse-body body)
    (let* ((other-decls nil)
           (gvars      (gvars env))
           (gtemplates (gtemplates env)))
      ;; replace templates with normal type declarations
      
      (iter (for decl in decls)
            ;; (declare (...) (...))
            (iter (for subdecl in (cdr decl))
                  (match subdecl
                    ((list* 'gtype template vars)
                     (iter (for var in vars)
                           (push var gvars)
                           (push template gtemplates))
                     (let ((normal (subst-if '* #'variablep template)))
                       (push (list* 'type normal vars)
                             other-decls)))
                    (_
                     (push subdecl other-decls)))))
      (setf gvars       (nreverse gvars)
            gtemplates  (nreverse gtemplates)
            other-decls (nreverse other-decls))
      
      (let ((parameters
             (remove-if-not #'variablep
                            (remove-duplicates
                             (flatten gtemplates)))))
        (with-gensyms (assignment unified-p)
          `(locally
               (declare ,@other-decls
                        ,@(mapcar (lambda (template var) `(gtype ,template ,var))
                                  gtemplates
                                  gvars))
             (multiple-value-bind (,assignment ,unified-p)
                 (type-unify ',parameters ',gtemplates (list ,@(mapcar (lambda (v) `(type-of ,v)) gvars)))
               ;; (format t "assignment: ~a" ,assignment)
               (assert ,unified-p nil "gtypes failed to unify to the actual types:~% ~a~%    against~% ~a"
                       ',gtemplates
                       (list ,@(mapcar (lambda (v) `(type-of ,v)) gvars)))
               (let ,(mapcar (lambda (p)
                               `(,p (cdr (assoc ',p ,assignment))))
                             parameters)
                 (declare (ignorable ,@parameters))
                 ,@body))))))))
        

(defmacro resolving (&body body)
  `(resolve ,@body))



;; no functions for now
#+(or)
(defun function-gtypes (env fns)
  (iter
    (for v in fns)
    (for (values binding local-p alist) = (function-information v env))
    (collect (cdr (or (assoc 'gtype alist)
                      (assoc 'ftype alist))))))

#+(or)
(defun find-lexical-funs (env)
  #+sbcl
  (mapcar #'car
          (sb-c::lexenv-funs
           (sb-c::coerce-to-lexenv env))))
