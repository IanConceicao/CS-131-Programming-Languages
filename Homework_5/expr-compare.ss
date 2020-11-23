#lang racket
(provide expr-compare)

;Doesn't work with binding.

(define lambda (string->symbol "\u03BB"))

;binds 2 symbols, a b, as a!b
;This was never used in my implementation because I never 
; figured out how to do binding or map variables
(define (bind a b) 
	(let ((astring (symbol->string a)) (bstring (symbol->string b)))
		(string->symbol (string-append astring "!" bstring))))

;Recursively break down lists
(define (compare_list x y)
    (if (equal? x '())
      '()
      (if (equal? y '())
	    '()
	     (cons (expr-compare (car x) (car y)) (compare_list (cdr x) (cdr y)))
  	  )
    )
)


(define (compare_quote x y)
    (if (equal? x y) 
      x 
      (list 'if '% x y)
    )
)

;If two lists both start with quote or list
(define (same_first x y)
	(case (car x)
		('quote (compare_quote x y))
		('list (compare_list x y))
		;(else (compare_procedure x y))
		(else (compare_list x y))
	)
)

(define (expr-compare x y)
  (cond [(equal? x y) x] ;Same expression!
        [(and (boolean? x) (boolean? y)) 
         (if x '% '(not %))] ;Both booleans
        [(or (equal? lambda x) (equal? lambda y)) 
			lambda]
        [(or (not (list? x))  ; variable and a list
             (not (list? y)))
         (list 'if '% x y)] 
        [#t ;(2 lists)) 
        	(same_first x y)]
  )
)

; My test-expr-compare doesn't seem to ever work! This is the one
; from the hint code and it still doesn't work
(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

(define test-expr-y '(list "my_y" (quote ('lambda (a b) (+ a b)))))
(define test-expr-x '(list "my_x" (quote (lambda (a c) (+ a p)))))


(expr-compare test-expr-x test-expr-y)









