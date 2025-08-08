
(define-library (chibi show python-test)
  (import (scheme base) (srfi 166) (chibi show python) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "show python")

      (test "if 1:
    2
else:
    3
"
          (show #f (py-if 1 2 3)))

      (test "if y if x else z:
    2
else:
    3
"
          (show #f (py-if (py-if 'x 'y 'z) 2 3)))

      (test "if 1 + (y if x else z):
    2
else:
    3
"
          (show #f (py-if (py+ 1 (py-if 'x 'y 'z)) 2 3)))

      (test "if y if x else z:
    2
else:
    3
"
          (show #f (py-expr '(if (if x y z) 2 3))))

      (test "if y if x else z:
    2
else:
    3
"
          (show #f (py-expr '(%begin (if (if x y z) 2 3)))))

      (test "if y if x else z:
    2
else:
    if w:
        3
    else:
        4
"
          (show #f (py-expr '(if (if x y z) 2 (if w 3 4)))))

      (test "if y if x else z:
    2
else:
    if w:
        if u:
            3
        else:
            4
    else:
        5
"
          (show #f (py-expr '(if (if x y z) 2 (if w (if u 3 4) 5)))))

      (test "def square(x):
    return x * x
"
          (show #f (py-def '(square x) (py-return (py* 'x 'x)))))

      (test "def foo(x, y, z):
    return 2 if (y if x else z) else 3
"
          (show #f (py-def '(foo x y z)
                          (py-return (py-if (py-if 'x 'y 'z) 2 3)))))

      (test "some_function(shape, x, y + 1, z)\n"
          (show #f (py-expr '(some_function shape x (+ y 1) z))))

      (test "if y < 255 and pred(shape, x, y, z) == 0:
    2
else:
    3
"
          (show #f (py-expr '(if (&& (< y 255) (== (pred shape x y z) 0)) 2 3))))

      (test "a * x + b * y == c\n"
          (show #f (py== (py+ (py* 'a 'x) (py* 'b 'y)) 'c)))
      (test "a * x + b * y == c\n"
          (show #f (py-expr '(== (+ (* a x) (* b y)) c))))

      (test "(a + x) * (b + y) == c\n"
          (show #f (py-expr '(== (* (+ a x) (+ b y)) c))))

      (test "1 - (3 + 2)\n"
          (show #f (py-expr '(- 1 (+ 3 2)))))
      (test "1 - (3 - 2)\n"
          (show #f (py-expr '(- 1 (- 3 2)))))
      (test "1 - 3 - 2\n"
          (show #f (py-expr '(- 1 3 2))))
      (test "1 + (3 + 2)\n"
          (show #f (py-expr '(+ 1 (+ 3 2)))))
      (test "1 + 3 + 2\n"
          (show #f (py-expr '(+ 1 3 2))))
      (test "x**y\n"
          (show #f (py-expr '(** x y))))
      (test "config.base**2\n"
          (show #f (py-expr '(** config.base 2))))
      (test "2**-5\n"
          (show #f (py-expr '(** 2 -5))))
      (test "2 ** get_exponent()\n"
          (show #f (py-expr '(** 2 (get_exponent)))))
      (test "a[1:5]\n"
          (show #f (py-expr '(%slice a 1 5))))
      (test "a[1:5]\n"
          (show #f (py-expr '(vector-ref a (: 1 5)))))
      (test "a[1:2 + 3]\n"
          (show #f (py-expr '(vector-ref a (: 1 (+ 2 3))))))
      (test "a[1:]\n"
          (show #f (py-expr '(vector-ref a (: 1)))))
      (test "a[:5]\n"
          (show #f (py-expr '(vector-ref a (: #f 5)))))
      (test "a[:]\n"
          (show #f (py-expr '(vector-ref a (: #f)))))

      (test "x == 0 && (y == 2 || y == 3)\n"
          (show #f (py-expr '(%and (== x 0) (%or (== y 2) (== y 3))))))

      (test
          "((abracadabra____ + xylophone____) *
    (bananarama____ + yellowstonepark____) *
    (cryptoanalysis + zebramania))
"
          (show #f (py-expr '(* (+ abracadabra____ xylophone____)
                                (+ bananarama____ yellowstonepark____)
                                (+ cryptoanalysis zebramania)))))

      (test
          "abracadabra(xylophone,
            bananarama,
            yellowstonepark,
            cryptoanalysis,
            zebramania,
            delightful,
            wubbleflubbery)\n"
          (show #f (py-expr '(abracadabra xylophone
                                         bananarama
                                         yellowstonepark
                                         cryptoanalysis
                                         zebramania
                                         delightful
                                         wubbleflubbery))))

      (test "    # abc 2 + 2\n    #  def\n    # ghi\n"
          (show #f (each "    " (py-comment "abc 2 + 2" nl " def" nl "ghi"))))

      (test "class Node:
    def __init__(self, data):
        self.data = data
        self.next = None
"
          (show #f (py-class 'Node
                             (py-def '(__init__ self data)
                                     (py= 'self.data 'data)
                                     (py= 'self.next 'None)))))

      (test "@dataclass
class Node:
    data
    next
"
          (show #f (each (py-expr '@dataclass)
                         (py-class 'Node 'data 'next))))

      (test "class Color(Enum):
    RED = 0
    GREEN = 1
    BLUE = 2
"
          (show #f (py-enum 'Color 'RED 'GREEN 'BLUE)))

      (test "class Color(Enum):
    RED = 1
    GREEN = 3
    BLUE = 4
"
          (show #f (py-enum 'Color '(RED 1) '(GREEN 3) 'BLUE)))

      (test "while True:
    dostuff()
    domorestuff()
"
          (show #f (py-while #t '(dostuff) '(domorestuff))))

      (test "for key, val in x.items():
    print(f\"{key}: {val}\")
"
          (show #f (py-for '(key val) '(x.items) '(print (%f "{key}: {val}")))))

      (test "f(key, val) for key, val in x.items()"
          (show #f (py-in-expr (py-for '(key val) '(x.items) '(f key val)))))
      (test "list(f(key, val) for key, val in x.items())"
          (show #f (py-in-expr '(list (for (key val) (x.items) (f key val))))))
      (test "[f(key, val) for key, val in x.items()]"
          (show #f (py-in-expr '(%list (for (key val) (x.items) (f key val))))))

      (test "foo(bar=baz, qux=frob)\n"
          (show #f (py-expr '(foo (= bar baz) (= qux frob)))))

      (test "import numpy as np\n"
          (show #f (py-expr '(import numpy np))))
      (test "from math import pi\n"
          (show #f (py-expr '(from math pi))))

      (test "try:
    foo()
except Exception as exn:
    print(\"oh no!\")
"
          (show #f (py-expr '(try
                              (foo)
                              (except (Exception exn)
                                      (print "oh no!"))))))

      (test-end))))
