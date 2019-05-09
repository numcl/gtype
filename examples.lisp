
(in-package :gtype)

(defun dot (a b)
  (resolve
    (declare (gtype (array ?t ?d) a b))
    ;; dummy work
    :ok))

(print (dot (make-array 2) (make-array 2)))
(print (dot (make-array 2) (make-array 3)))      ; error
(print (dot (make-array 2) (make-array '(3 3)))) ; error

(print (dot (make-array 2 :element-type 'fixnum)
            (make-array 2 :element-type 'fixnum)))
(print (dot (make-array 2 :element-type 'fixnum)
            (make-array 2 :element-type 'single-float))) ; error
(print (dot (make-array 2 :element-type 'fixnum)
            (make-array 3 :element-type 'fixnum))) ;error
(print (dot (make-array 2 :element-type 'fixnum)
            (make-array 3 :element-type 'float))) ;error

;; now the real fun starts here...

(defun gemm (a b c)
  (resolve
    (declare (gtype (array ?t (?n1 ?n2)) a)
             (gtype (array ?t (?n2 ?n3)) b)
             (gtype (array ?t (?n1 ?n3)) c))
    (dotimes (i ?n1)
      (dotimes (j ?n2)
        (dotimes (k ?n3)
          (setf (aref c i k) (* (aref a i j) (aref b j k))))))))

(let ((c (make-array '(2 2) :initial-element 0)))
  (gemm #2A((0 1) (2 3))
        #2A((0 1) (2 3))
        c)
  (print c))

;; -> #2A((2 3) (6 9)) 

(let ((c (make-array '(2 2) :initial-element 0)))
  (gemm #2A((0 1) (2 3))
        #2A((0 1 3) (2 3 3))
        c))
;; -| error!

(let ((c (make-array '(2 2) :initial-element 0)))
  (gemm #2A((0 1 3) (2 3 3))
        #2A((0 1) (2 3) (4 5))
        c)
  (print c))
;; -> #2A((12 15) (12 15))

(let ((c (make-array '(2 2) :initial-element 0)))
  (gemm #2A((0 1) (2 3) (4 5))
        #2A((0 1 3) (2 3 3))
        c)
  (print c))
;; -| error! should be a 3x3 matrix

(let ((c (make-array '(3 3) :initial-element 0)))
  (gemm #2A((0 1) (2 3) (4 5))
        #2A((0 1 3) (2 3 3))
        c)
  (print c))
;; -> #2A((2 3 3) (6 9 9) (10 15 15)) 



;; resolve/resolving also recognizes the gtype immediately inside it 

(defun fn5 (a b)
  (resolving
    (declare (gtype (array ?t ?d) a))
    (declare (gtype (array ?t ?d) b))
    (list a b ?t ?d)))

(print (fn5 (make-array 2 :element-type 'fixnum)
            (make-array 2 :element-type 'fixnum)))



;; nested resolve/resolving macro recognizes both the outer gtypes and inner gtypes.
;; Therefore, the type of C is matched against the type of A and B.

(defun fn6 (a b fn)
  (resolving
    (declare (gtype (array ?t ?d) a))
    (declare (gtype (array ?t ?d) b))
    (print (list :first ?t ?d))
    (let ((c (funcall fn a b)))
      (resolving
        (declare (gtype (array ?t ?d) c))
        (print (list :second ?t ?d))
        (list a b c)))))

(print (fn6 (make-array 2 :element-type 'fixnum)
            (make-array 2 :element-type 'fixnum)
            (lambda (a b)
              (if (zerop (random 1))
                  a
                  b))))

;; error!
(print (fn6 (make-array 2 :element-type 'fixnum)
            (make-array 2 :element-type 'fixnum)
            (lambda (a b)
              (declare (ignore a b))
              (make-array 2 :element-type 'single-float))))
