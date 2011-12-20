(define (fact n)
  (cond ((eq? n 0) 1)
        (else (* n (fact (- n 1))))))
(print (fact 10))
