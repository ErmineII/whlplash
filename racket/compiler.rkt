#lang racket

(require racket/format)
(require racket/match)

(define movement-cmd-prefix #\.)

; An immutable vector of mutable vectors of all commands that can be reached (keys are (x y))
(define (reachable-commands prog)
  
  ; Adds a command to the accumulator, what else did you think it did?
  (define (add-cmd acc cmd x y)
    (vector-set! (vector-ref acc y) (/ x 2) cmd))
  
  ; Wraps x horizontally, returns null if you try to make it wrap vertically
  (define (wrap x y should-err)
    (if (or (< y 0) (>= y (vector-length prog)))
        (if should-err
            (raise (format "Tried to wrap vertically at x=~a, y=~a" x y))
            '())
        (let* ([line (vector-ref prog y)]
               [line-len (string-length line)]
               [wrapped-x (cond
                            [(< x 0) (+ line-len x)]
                            [(>= x line-len) (- x line-len)]
                            [#t x])])
          (list wrapped-x y))))
  
  (define (helper acc dx dy x y)
    (define (cardinal-cmd acc dx dy x y cmd-name)
      (add-cmd acc x y cmd-name)
      (let ([new-xy (wrap (+ dx x) (+ dy y))])
        (helper acc dx dy (first new-xy) (second new-xy))))
    
    (let* ([line (vector-ref prog y)]
           [prefix (string-ref line x)]
           [cmd (string-ref line (+ 1 x))])
      (if (equal? prefix movement-cmd-prefix)
          (match cmd
            [#\> (cardinal-cmd 2 0 x y)]
            [#\< (cardinal-cmd -2 0 x y)]
            [#\v (cardinal-cmd 0 1 x y)]
            [#\^ (cardinal-cmd 0 -1 x y)]
            ; todo this
            [#\@ '()])
          ; todo do this
          ( '() ))))
  
  (let* ([make-row (lambda (line) (make-vector (string-length line) '()))]
         ; The accumulator initially has all cells as null/unused
         [init-acc (map make-row prog)])
    ; Start out in the top left corner, moving to the right 2 cells at a time
    (helper init-acc 2 0 0 0)))

(define (binary-op symbol)
  (list symbol
        (format "stack = stack.head ~s stack.tail.head;" symbol)))

(define arithmetic-commands
  (hash
   (map binary-op (list "+" "-" "*" "/"))))

(define stack-commands
  (hash
   ("foo" "bar")))

(define command-groups
  (hash
   "+" arithmetic-commands
   ":" stack-commands))