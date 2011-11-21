#lang racket

(require "verifica_tipos.rkt")

(define (limpaCaracteres lista)
  (cond 
    [ (empty? lista) '() ]
    [ else (cons (limpaChar (first lista)) (limpaCaracteres (rest lista))) ]
  )
)

(define (limpaChar char)
  (cond
    [ (ehOperador? char) (char->simbolo char) ]
    [ (ehNumero? char) (char->numero char) ]
    [ else '() ]
  )
)

(define (parse-expression stringDeEntrada)
  (limpaCaracteres(string->list stringDeEntrada))
)

(define (infix-to-prefix exp-list)
  '())

(define (eval-infix input-string)
  '())

(provide parse-expression
         infix-to-prefix
         eval-infix)


; (limpaCaracteres '(#\1 #\2 #\+ #\3 #\4))
; (parse-expression "89123")
; (parse-expression "4+5")