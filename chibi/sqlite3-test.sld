
(define-library (chibi sqlite3-test)
  (export run-tests)
  (import (scheme base) (chibi sqlite3) (chibi test))
  (begin
    (define (run-tests)
      (test-begin "sqlite3")
      (let ((db (sqlite3-open ":memory:")))
        (sqlite3-exec db "CREATE TABLE animals (name varchar(64), color varchar(64), weight int);")
        (sqlite3-do db "INSERT INTO animals (name, color, weight) VALUES (?, ?, ?)" "cat" "black" 3)
        (sqlite3-do db "INSERT INTO animals (name, color, weight) VALUES (?, ?, ?)" "dog" "white" 4)
        (sqlite3-do db "INSERT INTO animals (name, color, weight) VALUES (?, ?, ?)" "elephant" "pink" 3000)
        (test '(#("black" "cat" 3)
                #("pink" "elephant" 3000)
                #("white" "dog" 4))
            (sqlite3-select db "SELECT color, name, weight FROM animals ORDER BY color;"))
        (sqlite3-exec db "CREATE TABLE flowers (name varchar(64), color varchar(64), smell float);")
        (let ((stmt (sqlite3-prepare db "INSERT INTO flowers (name, color, smell) VALUES (?, ?, ?);")))
          (sqlite3-do db stmt "rose" "red" 0.8)
          (sqlite3-do db stmt "gardenia" "white" 0.7)
          (sqlite3-do db stmt "wisteria" "purple" 0.5)
          (sqlite3-do db stmt "frangipani" "pink" 0.4))
        (test '(#("pink" "frangipani" 0.4)
                #("white" "gardenia" 0.7)
                #("red" "rose" 0.8)
                #("purple" "wisteria" 0.5))
            (sqlite3-select db "SELECT color, name, smell FROM flowers ORDER BY name;"))
        (test 3 (sqlite3-get db "SELECT COUNT(*) FROM flowers WHERE smell >= 0.5;"))
        (test '(#("frangipani" "elephant")
                #("gardenia" "dog"))
            (sqlite3-select db "SELECT flowers.name, animals.name FROM animals, flowers WHERE flowers.color = animals.color ORDER BY flowers.name;"))
        (test '(#("pink" "elephant")
                #("white" "dog"))
            (sqlite3-select db (ssql->sql '(select (columns color name)
                                                   (from animals)
                                                   (where (> name "cat"))
                                                   (order color)))))
        (test '(#("pink" "elephant")
                #("white" "dog"))
            (sqlite3-select db (ssql->sql '(select (columns color name)
                                                   (from animals)
                                                   (where (string-ci> name "Cat"))
                                                   (order color)))))
        (test '(#("gardenia" "dog")
                #("frangipani" "elephant"))
            (sqlite3-select
             db
             '(select (columns flowers.name animals.name)
                      (from animals flowers)
                      (where (= flowers.color animals.color))
                      (order (desc flowers.name)))))
        (test '(#("animals") #("flowers"))
            (sqlite3-select db '(select (columns name)
                                        (from sqlite_master)
                                        (where (= type "table"))
                                        (order name))))
        (let ((f (sqlite3-lambda db (select (columns name color)
                                            (from animals)
                                            (where (= weight ?)))
                   (string-append color " " name))))
          (test "black cat" (f 3))
          (test "white dog" (f 4))
          (test "pink elephant" (f 3000)))
        (let ((f (sqlite3-loop db (select (columns name color)
                                          (from animals)
                                          (where (< weight ?))
                                          (order (desc name)))
                   ((res '() (cons (string-append color " " name) res))))))
          (test '("black cat" "white dog")
              (f 10))))
      (test-end))))
