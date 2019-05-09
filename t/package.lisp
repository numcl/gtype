#|
  This file is a part of gtype project.
  Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :gtype.test
  (:use :cl
        :gtype
        :fiveam
        :trivialib.type-unify :trivial-cltl2 :trivia :alexandria :iterate))
(in-package :gtype.test)



(def-suite :gtype)
(in-suite :gtype)

;; run test with (run! test-name) 

(defun dot1 (a b)
  (resolve
    (declare (gtype (array ?t ?d) a b))
    (declare (optimize))
    (print (list a b ?t ?d))
    :ok))

(defun dot2 (a b)
  (resolve
    (declare (gtype (array fixnum ?d) a b))
    (print ?d)
    :ok))

(defun dot3 (a b)
  (resolve
    (declare (gtype (array ?t ?d) a))
    (declare (gtype (array ?t ?d) b))
    (print (list ?t ?d))
    :ok))

(defun dot4 (a b)
  (resolve
    (declare (gtype (array ?t ?d) a)
             (gtype (array ?t ?d) b))
    (print (list ?t ?d))
    :ok))

(defun mul (a b result)
  (resolve
    (declare (gtype (array fixnum ?d) a b result))
    (print (list ?d))
    :ok))

(defun gemm (a b c)
  (resolve
    (declare (gtype (array ?t (?n1 ?n2)) a)
             (gtype (array ?t (?n2 ?n3)) b)
             (gtype (array ?t (?n1 ?n3)) c))
    (print (list ?n1 ?n2 ?n3))
    :ok))

(defun conv2d (input kernel result)
  (resolve
    (declare (gtype (array ?t (?b ?h ?w ?c1)) input)
             (gtype (array ?t (?kh ?kw ?f)) kernel)
             (gtype (array ?t (?b ?h ?w ?c2)) result))
    (print (list ?t ?b ?h ?w ?c1 ?c2))
    :ok))

(defun a (type &optional (n 1))
  (make-array n :element-type type))

(test dot1

  (is (eq :ok (dot1 (a 'fixnum) (a 'fixnum))))
  (is (eq :ok (dot1 (a 'single-float) (a 'single-float))))
  (signals error
    (dot1 (a 'single-float) (a 'fixnum))))

(test dot2
  (is (eq :ok (dot2 (a 'fixnum) (a 'fixnum))))
  (signals error
    ;; this is due to fixnum specification
    (dot2 (a 'single-float) (a 'single-float)))
  (signals error
    (dot2 (a 'fixnum) (a 'fixnum 2))))

(test dot3
  (is (eq :ok (dot3 (a 'fixnum) (a 'fixnum))))
  (is (eq :ok (dot3 (a 'single-float) (a 'single-float))))
  (signals error
    (dot3 (a 'single-float) (a 'fixnum))))

(test dot4
  
  (is (eq :ok (dot4 (a 'fixnum) (a 'fixnum))))
  (is (eq :ok (dot4 (a 'single-float) (a 'single-float))))
  (signals error
    (dot4 (a 'single-float) (a 'fixnum))))

(test mul
  (is (eq :ok (mul (a 'fixnum) (a 'fixnum) (a 'fixnum))))
  (signals type-error
    ;; this is due to fixnum specification
    (mul (a 'single-float) (a 'fixnum) (a 'fixnum))))

(test gemm
  (is (eq :ok (gemm (a 'fixnum '(3 5))
                    (a 'fixnum '(5 4))
                    (a 'fixnum '(3 4)))))
  (is (eq :ok (gemm (a 'fixnum '(2 2))
                    (a 'fixnum '(2 2))
                    (a 'fixnum '(2 2)))))
  (is (eq :ok (gemm (a 'fixnum '(1 2))
                    (a 'fixnum '(2 1))
                    (a 'fixnum '(1 1)))))
  (is (eq :ok (gemm (a 'fixnum '(2 1))
                    (a 'fixnum '(1 2))
                    (a 'fixnum '(2 2)))))
  (signals error
    (gemm (a 'fixnum '(3 5))
          (a 'fixnum '(5 6))
          (a 'fixnum '(3 4))))
  (signals error
    (gemm (a 'fixnum '(2 2))
          (a 'fixnum '(2 2))
          (a 'fixnum '(2 3))))
  (signals error
    (gemm (a 'fixnum '(2 2))
          (a 'fixnum '(2 1))
          (a 'fixnum '(1 1))))
  (signals error
    (gemm (a 'fixnum '(2 1))
          (a 'fixnum '(1 2 4))
          (a 'fixnum '(2 2)))))
  
(test gemm2
  (signals error
    (gemm (a 'single-float  '(2 1))
          (a 'fixnum '(1 2))
          (a 'fixnum '(2 2))))
  (signals error
    (gemm (a 'fixnum '(2 1))
          (a 'single-float  '(1 2))
          (a 'fixnum '(2 2))))
  (signals error
    (gemm (a 'fixnum '(2 1))
          (a 'fixnum '(1 2))
          (a 'single-float  '(2 2))))
  (signals error
    (gemm (a 'fixnum '(2 1))
          (a 'single-float  '(1 2))
          (a 'single-float  '(2 2))))
  (signals error
    (gemm (a 'single-float  '(2 1))
          (a 'fixnum '(1 2))
          (a 'single-float  '(2 2))))
  (is (eq :ok (gemm (a 'single-float  '(2 1))
                    (a 'single-float  '(1 2))
                    (a 'single-float  '(2 2))))))



