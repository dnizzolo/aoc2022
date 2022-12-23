(asdf:defsystem :aoc2022
  :description "Advent of Code 2022"
  :author "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:parachute :alexandria :one-more-re-nightmare :bodge-queue)
  :components ((:file "utils")
               (:file "day01")
               (:file "day02")
               (:file "day03")
               (:file "day04")
               (:file "day05")
               (:file "day06")
               (:file "day07")
               (:file "day08")
               (:file "day09")
               (:file "day10")
               (:file "day11")
               (:file "day12")
               (:file "day13")
               (:file "day14")
               (:file "day15"))
  :in-order-to ((asdf:test-op (asdf:test-op :aoc2022/test))))

(asdf:defsystem :aoc2022/test
  :description "Test suite for Advent of Code 2022"
  :author "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :license "MIT"
  :depends-on (:aoc2022)
  :perform (test-op (op c) (uiop:symbol-call
                            :parachute
                            :test
                            (uiop:symbol-call
                             :parachute
                             :test-packages))))
