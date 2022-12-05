(in-package :aoc2022)

(defun read-rock-paper-scissors-strategy (&optional (relative-pathname #p"inputs/day02.txt"))
  (let ((filename (asdf:system-relative-pathname :aoc2022 relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            for opponent = (- (char-code (char line 0)) #.(1- (char-code #\A)))
            for mine = (- (char-code (char line 2)) #.(1- (char-code #\X)))
            collect (list opponent mine)))))

(defun rock-paper-scissors-outcome (p1 p2 &aux (dist (- p1 p2)))
  (cond ((or (= dist 2) (= dist -1)) (+ 6 p2))
        ((or (= dist -2) (= dist 1)) p2)
        ((zerop dist) (+ 3 p2))
        (t (error "Unexpected turn ~a VS ~a." p1 p2))))

(defun total-games-scores (strat)
  (loop for (o m) in strat sum (rock-paper-scissors-outcome o m)))

(defun convert-strategy (move outcome)
  (cond ((= outcome 2) (list move move))
        ((= outcome 1) (list move (if (= move 1) 3 (1- move))))
        ((= outcome 3) (list move (1+ (mod move 3))))
        (t (error "Unexpected outcome ~a." outcome))))

(defun day02 ()
  (let ((strat (read-rock-paper-scissors-strategy)))
    (values (total-games-scores strat)
            (total-games-scores (loop for (i r) in strat collect (convert-strategy i r))))))
