#lang racket

(require "verifica_tipos.rkt")
(require "tokenizador.rkt")


(define (parse-expression stringDeEntrada)
  (define tokenizado (tokenizar (string->list stringDeEntrada)))
  (define abs (montaTokenLista tokenizado))
  abs
)

(define (infix-to-prefix exp-list)
  '()
)

(define (eval-infix input-string)
  '()
)

(provide parse-expression
         infix-to-prefix
         eval-infix)


; (parse-expression "89123")
; (parse-expression "4+5")
; (parse-expression "4+5 - 1 ( 2 * 22)")
; (parse-expression "4+5 - 1 ( 2 * 22) + 2")
; (parse-expression "4+ (5 ^ 2) - 1 * ( 2 * 22) + 2")
; (parse-expression "((3*((1) + 26))/(4+5))")