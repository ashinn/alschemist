
(define-library (chibi table-test)
  (import (scheme base)
          (scheme write)
          (srfi 231)
          (chibi table)
          (chibi test))
  (cond-expand
   (chibi
    (import (only (chibi) find-module-file)))
   (else
    (begin
      (define (find-module-file path)
        path))))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "(chibi table)")
      (let ((passengers
             (table '#(#("Braund, Mr. Owen Harris" male 22 7.25)
                       #("Cumings, Mrs. John Bradley" female 38 71.28)
                       #("Heikkinen, Miss. Laina" female 26 7.925)
                       #("Futrelle, Mrs. Jacques Heath" female 35 53.1)
                       #("Allen, Mr. William Henry" male 35 8.05))
                    '#(name sex age fare))))
        (test 5 (table-num-rows passengers))
        (test 4 (table-num-columns passengers))
        (test '(5 4) (table-shape passengers))
        (test "Heikkinen, Miss. Laina" (table-ref passengers 2 0))
        (test 38 (table-ref passengers 1 2))
        (test 7.25 (table-ref passengers 0 3))
        (test "Heikkinen, Miss. Laina" (table-ref passengers 2 'name))
        (test 38 (table-ref passengers 1 'age))
        (test 7.25 (table-ref passengers 0 'fare))
        )
      (let ((passengers
             (table '#(#("Braund, Mr. Owen Harris" male 22 7.25)
                       #("Cumings, Mrs. John Bradley" female 38 71.28)
                       #("Heikkinen, Miss. Laina" female 26 7.925)
                       #("Futrelle, Mrs. Jacques Heath" female 35 53.1)
                       #("Allen, Mr. William Henry" male 35 8.05))
                    '#(name sex age fare)
                    (vector generic-storage-class
                            generic-storage-class
                            u8-storage-class
                            f32-storage-class))))
        (test 5 (table-num-rows passengers))
        (test 4 (table-num-columns passengers))
        (test '(5 4) (table-shape passengers))
        (test "Heikkinen, Miss. Laina" (table-ref passengers 2 0))
        (test 38 (table-ref passengers 1 2))
        (test 7.25 (table-ref passengers 0 3))
        (test "Heikkinen, Miss. Laina" (table-ref passengers 2 'name))
        (test 38 (table-ref passengers 1 'age))
        (test 7.25 (table-ref passengers 0 'fare))
        )
      (let ((passengers
             (table-load-csv (find-module-file "chibi/table-example.csv")
                             'header-from-first-line?: #t)))
        (test "Heikkinen, Miss. Laina" (table-ref passengers 2 'Name)))
      (test-end))))
