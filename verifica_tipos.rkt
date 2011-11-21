#lang racket

(define (ehOperador? char)
   (cond 
      [(eq? (char->integer char) 43) #t] ; +
      [(eq? (char->integer char) 45) #t] ; -
      [(eq? (char->integer char) 92) #t] ; \
      [(eq? (char->integer char) 42) #t] ; *
      (else #f)
   )   
 )

(define (ehNumero? char)
  
     (and (< (- (char->integer char) 48) 10)
          (<= 48 (char->integer char))
     )
)
