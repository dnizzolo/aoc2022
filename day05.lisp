(defpackage :aoc2022.05
  (:documentation "Supply Stacks.")
  (:use :cl :aoc2022.utils))

(in-package :aoc2022.05)

(defun read-cargo-stacks-and-moves (&optional (relative-pathname #p"inputs/day05.txt"))
  (let ((filename (asdf:system-relative-pathname :aoc2022 relative-pathname))
        (stacks (make-hash-table)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while (plusp (length line))
            do (loop for i from 1 below (length line) by 4
                     for j from 1
                     for c = (char line i)
                     when (upper-case-p c)
                       do (let ((stack (gethash j stacks)))
                            (push c stack)
                            (setf (gethash j stacks) stack))))
      (loop for k being the hash-keys of stacks
            do (setf (gethash k stacks) (nreverse (gethash k stacks))))
      (values stacks (loop for line = (read-line in nil)
                           while line
                           collect (parse-integers-from-string line))))))

(defun top-of-cargo-stacks (stacks)
  (loop for i from 1
        for stack = (gethash i stacks)
        while stack collect (car stack) into result
        finally (return (coerce result 'string))))

(defun day05 (&aux stacks moves part-one)
  (multiple-value-setq (stacks moves) (read-cargo-stacks-and-moves))
  (loop for (amount from to) in moves
        do (dotimes (i amount)
             (push (pop (gethash from stacks)) (gethash to stacks)))
        finally (setf part-one (top-of-cargo-stacks stacks)))
  (multiple-value-setq (stacks moves) (read-cargo-stacks-and-moves))
  (loop for (amount from to) in moves
        do (let ((moved (subseq (gethash from stacks) 0 amount)))
             (setf (gethash from stacks) (nthcdr amount (gethash from stacks))
                   (gethash to stacks) (nconc moved (gethash to stacks))))
        finally (return (values part-one (top-of-cargo-stacks stacks)))))

(define-test 5
  (string= "DHBJQJCCW")
  (string= "WJVRLSJJT"))
