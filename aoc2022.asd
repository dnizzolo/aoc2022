(asdf:defsystem :aoc2022
  :description "Advent of Code 2022"
  :author "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "utils")
               (:file "day01")
               (:file "day02")
               (:file "day03")
               (:file "day04"))
  :in-order-to ((asdf:test-op (asdf:test-op :aoc2022/test))))

(asdf:defsystem :aoc2022/test
  :description "Test suite for Advent of Code 2022"
  :author "Daniele Nizzolo <dani.nizzolo@gmail.com>"
  :license "MIT"
  :depends-on (:aoc2022
               :parachute)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "main"))))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :aoc2022/test)))
