(defpackage :aoc2022.11
  (:documentation "Monkey in the Middle.")
  (:local-nicknames (:bq :bodge-queue))
  (:use :cl :aoc2022.utils))

(in-package :aoc2022.11)

(defstruct monkey
  items
  operation
  divisor
  target-true
  target-false)

(defun read-monkeys (&optional (relative-pathname #p"inputs/day11.txt"))
  (flet ((parse-initial-items (string)
           (list-to-queue (parse-integers-from-string string)))
         (parse-operation (string)
           (let ((int (car (parse-integers-from-string string)))
                 (mulp (find #\* string)))
             (if int
                 (if mulp
                     (lambda (old) (* int old))
                     (lambda (old) (+ int old)))
                 (if mulp
                     (lambda (old) (* old old))
                     (lambda (old) (+ old old))))))
         (parse-number (string)
           (car (parse-integers-from-string string))))
    (let ((filename (asdf:system-relative-pathname :aoc2022 relative-pathname)))
      (with-open-file (in filename)
        (loop with monkey
              for line = (read-line in nil)
              while line
              when (plusp (length line))
                do (let ((items (parse-initial-items (read-line in)))
                         (operation (parse-operation (read-line in)))
                         (divisor (parse-number (read-line in)))
                         (target-true (parse-number (read-line in)))
                         (target-false (parse-number (read-line in))))
                     (setf monkey (make-monkey :items items
                                               :operation operation
                                               :divisor divisor
                                               :target-true target-true
                                               :target-false target-false)))
                and collect monkey into monkeys
              finally (return (make-array (length monkeys)
                                          :initial-contents monkeys)))))))

(defun run-monkeys (monkeys &key (steps 20) worried)
  (let ((inspects (make-array (length monkeys) :initial-element 0))
        (lcm (apply #'lcm (map 'list #'monkey-divisor monkeys))))
    (dotimes (_ steps)
      (loop for mon across monkeys for i from 0 do
        (with-slots (items operation divisor target-true target-false) mon
          (loop until (bq:queue-empty-p items) do
            (let* ((item (bq:queue-pop items))
                   (worry (if worried
                              (mod (funcall operation item) lcm)
                              (floor (/ (funcall operation item) 3))))
                   (divisible (zerop (mod worry divisor))))
              (incf (svref inspects i))
              (bq:queue-push (monkey-items
                              (svref monkeys
                                     (if divisible
                                         target-true
                                         target-false)))
                             worry))))))
    (setf inspects (sort inspects #'>))
    (* (svref inspects 0) (svref inspects 1))))

(defun day11 ()
  (values (run-monkeys (read-monkeys))
          (run-monkeys (read-monkeys) :steps 10000 :worried t)))

(define-test 11
  (= 58786)
  (= 14952185856))