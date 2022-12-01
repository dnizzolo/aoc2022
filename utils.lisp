(in-package :aoc2022)

(defun parse-integers-from-string (string &key (radix 10))
  "Parse all the integers in STRING ignoring other contents and return
them in a list."
  (declare (type string string))
  (loop with index = 0
        with limit = (length string)
        with parse with next-index
        while (< index limit)
        do (multiple-value-setq (parse next-index)
             (parse-integer string :start index :radix radix :junk-allowed t))
        if parse
          collect it and do (setf index next-index)
        else
          do (incf index)))

(defun triangular (n)
  "Compute the sum of the naturals from 1 to N."
  (declare (type (integer 0) n))
  (* 1/2 (* n (1+ n))))

(defun bit-vector-to-integer (bit-vector &key (start 0) (end (length bit-vector)))
  "Convert the bit-vector BIT-VECTOR from START to END (not included) to
the corresponding integer."
  (loop with result = 0
        for i from start below end
        do (setf result (+ (* 2 result) (bit bit-vector i)))
        finally (return result)))

(defun extended-gcd (a b)
  "Compute the greatest common divisor and the coefficients of Bezout's
identity for two numbers A and B using the Extended Euclidean
Algorithm."
  (declare (type integer a b))
  (loop with old-rem = a
        with rem = b
        with old-bez-s = 1
        with bez-s = 0
        with bez-t = 0
        with quotient
        with new-rem
        until (zerop rem) do
          (multiple-value-setq (quotient new-rem) (floor old-rem rem))
          (psetf old-rem rem
                 rem new-rem
                 old-bez-s bez-s
                 bez-s (- old-bez-s (* quotient bez-s)))
        finally (unless (zerop b)
                  (setf bez-t (floor (- old-rem (* a old-bez-s)) b)))
                (return (values old-rem old-bez-s bez-t))))

(defun modular-inverse (a m)
  "Compute the inverse of A modulo M."
  (declare (type integer a m))
  (multiple-value-bind (gcd a-coeff m-coeff) (extended-gcd a m)
    (declare (ignorable m-coeff))
    (if (= gcd 1)
        (mod a-coeff m)
        (error 'integer-not-invertible-in-modulo :integer a :modulo m))))

(define-condition integer-not-invertible-in-modulo (arithmetic-error)
  ((integer
    :initarg :integer
    :reader integer-not-invertible-in-modulo-integer)
   (modulo
    :initarg :modulo
    :reader integer-not-invertible-in-modulo-modulo))
  (:default-initargs
   :integer (a:required-argument :integer)
   :modulo (a:required-argument :modulo))
  (:report
   (lambda (condition stream)
     (with-slots (integer modulo) condition
       (format stream "~&~a is not invertible modulo ~a." integer modulo)))))
