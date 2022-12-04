(in-package :aoc2022)

(defun read-rucksack-contents (&optional (relative-pathname #p"inputs/day03.txt"))
  (let ((filename (asdf:system-relative-pathname :aoc2022 relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line collect line))))

(defun rucksack-item-priority (item)
  (cond ((lower-case-p item) (1+ (- (char-code item) (char-code #\a))))
        ((upper-case-p item) (+ 27 (- (char-code item) (char-code #\A))))
        (t (error "Unexpected item ~a." item))))

(defun sum-of-common-item-priority (items)
  (loop for line in items
        for half = (/ (length line) 2)
        sum (loop for c across line
                  until (find c line :start half)
                  finally (return (rucksack-item-priority c)))))

(defun sum-of-triplets-priority (items)
  (loop for (e1 e2 e3) on items by #'cdddr
        while e3
        sum (loop for c across e1
                  until (and (find c e2) (find c e3))
                  finally (return (rucksack-item-priority c)))))

(defun day03 ()
  (let ((items (read-rucksack-contents)))
    (values (sum-of-common-item-priority items)
            (sum-of-triplets-priority items))))
