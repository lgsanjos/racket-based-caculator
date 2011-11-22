#lang racket

(define (ehOperador? char)
   (not (null? (char->simbolo char)))
 )

(define (ehParenteses? char)
   (not (null? (char->parenteses char)))
)

(define (ehNumero? char)
  
     (and (< (char->numero char) 10)
          (<= 48 (char->integer char))
     )
)


(define (char->simbolo char)
   (cond 
      [ (eq? (char->integer char) 42) '* ]
      [ (eq? (char->integer char) 43) '+ ] 
      [ (eq? (char->integer char) 45) '- ] 
      [ (eq? (char->integer char) 47) '/ ] 
      [ (eq? (char->integer char) 94) '^ ] 
      [ else null ]
   )
)

(define (char->symbol chr)
  (list->string (list chr))
)
   
(define (char->parenteses char)
   (cond 
      [ (eq? (char->integer char) 40) (string->symbol "(") ] 
      [ (eq? (char->integer char) 41) (string->symbol ")") ] 
      [ else null ]
   )
)

(define (char->numero char)
  (- (char->integer char) 48)
)


(provide ehOperador?
         ehParenteses?
         ehNumero?
         char->numero
         char->simbolo
         char->parenteses
)
