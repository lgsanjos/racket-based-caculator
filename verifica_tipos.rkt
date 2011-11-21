#lang racket

(define (ehOperador? char)
   (cond 
      [(null? (char->simbolo char)) #f ]
      [else #t]
   )
 )

(define (char->simbolo char)
   (cond 
      [(eq? (char->integer char) 42) '* ] ; * 
      [(eq? (char->integer char) 43) '+ ] ; +
      [(eq? (char->integer char) 45) '- ] ; -
      [(eq? (char->integer char) 47) '/ ] ; \
      [ else null ]
   )
)

(define (ehNumero? char)
  
     (and (< (char->numero char) 10)
          (<= 48 (char->integer char))
     )
)

(define (char->numero char)
  (- (char->integer char) 48)
)


(provide ehOperador?
         ehNumero?
         char->numero
         char->simbolo
)