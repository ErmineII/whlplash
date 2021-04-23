#lang racket

(require racket/format)
(require racket/match)

(define movement-cmd-prefix #\.)

(define types '(num char stack))

(struct stack-effect (before after))

(define (binary-op symbol effect)
  (list symbol
        (format "stack.push(stack.pop() ~a stack.pop());" effect)))

(define binary-num-op-effect (stack-effect '(num num) '(num)))
(define unary-num-op-effect (stack-effect '(num) '(num)))

(define arithmetic-commands  
  (hash
   (map (lambda (op) (binary-op op binary-num-op-effect)) (list #\+ #\- #\* #\/))))

(define stack-commands
  (hash
   ("foo" "bar")))

(define command-groups
  (hash
   #\+ arithmetic-commands
   #\: stack-commands))

(define (get-cmd prefix suffix)
  (define (read-char char)
    (- (char->integer char) 48))
  (if (char<=? #\0 prefix #\9)
      (+ (* (read-char prefix) 10) (read-char suffix))
      (hash-ref (hash-ref command-groups prefix) suffix)))

; An immutable vector of mutable vectors of all commands that can be reached (keys are (x y))
(define (reachable-commands prog)
  
  ; Adds a command to the vecumulator, what else did you think it did?
  (define (add-cmd vec cmd x y)
    (vector-set! (vector-ref vec y) (/ x 2) cmd))
  
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
  
  (define (helper vec dx dy x y)
    (define (cardinal-cmd vec dx dy x y cmd-name)
      (add-cmd vec x y cmd-name)
      (match (wrap (+ dx x) (+ dy y))
        [(list new-x new-y)
         (helper vec dx dy new-x new-y)]))
    
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
  
  (let* ([make-row (lambda (line) (make-vector (/ (string-length line) 2) '()))]
         ; The accumulator initially has all cells as null/unused
         [init-vec (map make-row prog)])
    ; Start out in the top left corner, moving to the right 2 cells at a time
    (helper init-vec 2 0 0 0)))
