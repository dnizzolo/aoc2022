(defpackage :aoc2022.01
  (:documentation "Calorie Counting.")
  (:use :cl :aoc2022.utils))

(in-package :aoc2022.01)

(defun read-calories (&optional (relative-pathname #p"inputs/day01.txt"))
  (let ((filename (asdf:system-relative-pathname :aoc2022 relative-pathname)))
    (with-open-file (in filename)
      (loop while (listen in)
            collect (loop for line = (read-line in nil)
                          while (plusp (length line))
                          sum (parse-integer line))
              into els
            finally (return (make-array (length els) :initial-contents els))))))

(defun day01 ()
  (let ((cals (subseq (sort (read-calories) #'>) 0 3)))
    (values (svref cals 0) (reduce #'+ cals))))

(define-test 1
  (= 67016)
  (= 200116))
