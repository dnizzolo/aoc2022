(in-package :aoc2022/test)

(define-test day01
  (is-values (day01)
    (= 67016)
    (= 200116)))

(define-test day02
  (is-values (day02)
    (= 15523)
    (= 15702)))

(define-test day03
  (is-values (day03)
    (= 7737)
    (= 2697)))

(define-test day04
  (is-values (day04)
    (= 562)
    (= 924)))

(define-test day05
  (is-values (day05)
    (string= "DHBJQJCCW")
    (string= "WJVRLSJJT")))
