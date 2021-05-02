#lang typed/racket

(require racket/format)
(require racket/match)

(define-type Effect (Pair (Listof Type) (Listof Type)))

(define-type Command (U SimpleCmd MovementCmd))
(struct SimpleCmd ([effect : Effect] [c : String]))
; todo define properly
(struct MovementCmd ([effect : Effect]))

(define-type Commands (Vectorof Command))

(define movement-cmd-prefix #\.)

(define types '(num char stack))

(: binary-op (-> Char Effect Command))
(define (binary-op symbol effect)
  (list effect
        (format "stack.push(stack.pop() ~a stack.pop());" symbol)))

(: binary-num-op-effect Effect)
(define binary-num-op-effect (list '(num num) '(num)))
(: unary-num-op-effect Effect)
(define unary-num-op-effect (list '(num) '(num)))

(define-type CmdGroup (Immutabale-HashTable Char command))

(: stack-commands CmdGroup)
(define arithmetic-commands  
  #hash( (#\+ . (binary-op "+" binary-num-op-effect)) ))

(: stack-commands CmdGroup)
(define stack-commands
  #hash())

(define command-groups
  (hash
   #\+ arithmetic-commands
   #\: stack-commands))

(: get-cmd (-> Char Char Integer))
(define (get-cmd prefix suffix)
  (: read-char (-> Char Integer))
  (define (read-char char)
    (- (char->integer char) 48))
  (if (char<=? #\0 prefix #\9)
      (+ (* (read-char prefix) 10) (read-char suffix))
      (hash-ref (hash-ref command-groups prefix) suffix)))

; An immutable vector of mutable vectors of all commands that can be reached (keys are (x y))
(: reachable-commands (-> (Vectorof String) (Vectorof Command)))
(define (reachable-commands prog)
  
  ; Adds a command to the vecumulator, what else did you think it did?
  (: add-cmd (-> (Vectorof (Vectorof Command)) Command Integer Integer))
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

  (: helper (-> (Vectorof (Vectorof Command)) Number Number Number Number))
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
  
  (let* ([make-row (lambda (line : String) (make-vector (/ (string-length line) 2) '()))]
         ; The accumulator initially has all cells as null/unused
         [init-vec (map make-row prog)])
    ; Start out in the top left corner, moving to the right 2 cells at a time
    (helper init-vec 2 0 0 0)))
