(in-package :aoc2022)

(defun read-calories (&optional (relative-pathname #p"inputs/day01.txt"))
  (let ((filename (asdf:system-relative-pathname :aoc2022 relative-pathname)))
    (with-open-file (in filename)
      (loop while (listen in)
            collect (loop for line = (read-line in nil)
                          while (plusp (length line))
                          sum (parse-integer line))
              into els
            finally (return (make-array (length els) :initial-contents els))))))

(defun day01/part-1 ()
  (multiple-value-bind (v _) (day01)
    (declare (ignorable _))
    v))

(defun day01/part-2 ()
  (multiple-value-bind (_ v) (day01)
    (declare (ignorable _))
    v))

(defun day01 ()
  (let ((cals (subseq (sort (read-calories) #'>) 0 3)))
    (values (aref cals 0) (reduce #'+ cals))))
