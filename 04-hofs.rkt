#lang racket

(require racket/trace)


#|-----------------------------------------------------------------------------
;; Higher-order functions (HOFs)

HOFs either take a function as an argument or return a function.

Some useful built-in HOFs and related functions:

- `apply`: apply a function to a list of arguments

- `curry`: returns a version of a function that can be partially applied

- `compose`: returns a function that is the composition of two other functions

- `eval`: evaluates a sexp
-----------------------------------------------------------------------------|#

;; `apply` applies a function to lists

(define (sum . ns)
  (+ 2 (apply + ns)))


;; `curry` gives us partial application

(define (repeat n x)
  (if (= n 0)
      '()
      (cons x (repeat (sub1 n) x))))

(define thrice (curry repeat 3))


;; compose is a simple but powerful form of "functional "glue"

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define safe-sqrt (compose sqrt abs))

(define (flip f)
  (lambda (x y) (f y x)))

(define even?
  (compose (curry = 0)
           (curry (flip remainder) 2)))


;; eval is like having access to the Racket compiler in Racket!

(define (my-if test e1 e2)
  (eval `(cond [,test ,e1]
               [else ,e2])))

(define (repeatedly n sexp)
  (eval (cons 'begin (repeat n sexp))))


#|-----------------------------------------------------------------------------
;; Some list-processing HOFs

- `map`: applies a function to every element of a list, returning the results

- `filter`: collects the values of a list for which a predicate tests true

- `foldr`: implements *primitive recursion*

- `foldl`: like `foldr`, but folds from the left; tail-recursive
-----------------------------------------------------------------------------|#

;; `map` examples
(define (map f lst)
  (if (empty? lst)
      '()
      (cons (f (first lst)) (map f (rest lst)))))

; (trace map)

#; (values
   (map add1 (range 10))

   (map (curry * 2) (range 10))
 
   (map string-length '("hello" "how" "is" "the" "weather?")))


;; `filter` examples
(define (filter p lst)
  (cond [(empty? lst) '()]
        [(p (first lst)) (cons (first lst) (filter p (rest lst)))]
        [else (filter p (rest lst))]))

; (trace map)

#; (values 
   (filter even? (range 10))
   
   (filter (curry < 5) (range 10))

   (filter (compose (curry equal? "hi")
                    car)
           '(("hi" "how" "are" "you")
             ("see" "you" "later")
             ("hi" "med" "low")
             ("hello" "there"))))


;; `foldr` examples
(define (foldr f baseval lst)
  (if (empty? lst)
      baseval
      (f (first lst) (foldr f baseval (rest lst)))))

#; (values
    (foldr + 0 (range 10))

    (foldr cons '() (range 10))

    (foldr cons '(a b c d e) (range 5))

    (foldr (lambda (x acc) (cons x acc)) ; try trace-lambda
           '()
           (range 5)))

(define sum2 (curry foldr + 0))

(define copy-list (curry foldr cons '()))

(define (concatenate l1 l2) (foldr cons l2 l1))

;; `foldl` examples
(define (foldl f acc lst)
  (if (empty? lst)
      acc
      (foldl f (f (first lst) acc) (rest lst))))

 (values
    (foldl + 0 (range 10))
    
    (foldl cons '() (range 10))
    
    (foldl cons '(a b c d e) (range 5))
    
    (foldl (lambda (x acc) (cons x acc)) ; try trace-lambda
           '()
           (range 5)))

(define reverse (curry foldl cons '()))

(define sum3 (curry foldl + 0))

(define (partition x lst)
  (foldl (lambda (y acc)
           (if (< y x)
               (list (cons y (first acc))
                     (second acc))
               (list (first acc)
                     (cons y (second acc)))))
         '(() ())
         lst))


#|-----------------------------------------------------------------------------
;; Lexical scope

- A free variable is bound to a value *in the environment where it is defined*, 
  regardless of when it is used

- This leads to one of the most important ideas we'll see: the *closure*
-----------------------------------------------------------------------------|#

(define (simple n)
  (let ([loc 10])
    (+ n loc)))

(define (weird n)
  (let ([loc n])
    (lambda ()
      (println loc))))

(define (weird2 n)
  (lambda ()
    (println n)))

(define (make-adder n)
  (lambda (x) (+ x n)))

(define (make-obj)
  (let ([attr 0])
    (lambda (cmd)
      (case cmd
        ['inc (set! attr (add1 attr))]
        ['dec (set! attr (sub1 attr))]
        ['show (println attr)]))))