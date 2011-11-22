#lang racket

(require "verifica_tipos.rkt")


(define (numerosDeUmaLista lista)
  (cond 
        [ (null? lista) '() ] 

        [ (ehNumero? (first lista))
            (cons (first lista) (numerosDeUmaLista (rest lista)))
        ]

        [ else '() ]
  )    
)


(define (tokenizar-se-operador lista)
  (cons (limpaChar(first lista)) (tokenizar (rest lista)))
)
  
(define (tokenizar-se-numero lista)
  (cons (string->number (list->string (numerosDeUmaLista lista)))
        (tokenizar (list-tail lista (length (numerosDeUmaLista lista)))))
)
  
(define (tokenizar-se-abre-parenteses lista)
   (cond 
     [ (eq? (first lista) #\)) '() ]
     [ else (tokenizar (rest lista)) ]
       
   )
)

(define (tokenizar listaDeChar)
  (cond 
        [ (null? listaDeChar) '() ]
        
        [ (eq? (first listaDeChar) #\()
             (cons (tokenizar-se-abre-parenteses listaDeChar) (tokenizar (rest listaDeChar)))
        ]        
        
        [ (ehOperador? (first listaDeChar))
             (tokenizar-se-operador listaDeChar)
        ]
        
        [ (ehNumero? (first listaDeChar))
             (tokenizar-se-numero listaDeChar)
        ]
        
        [ else (tokenizar (rest listaDeChar)) ]
  )
)

(define (limpaChar char)
  (cond
    [ (ehOperador? char) (char->simbolo char) ]
    [ (ehNumero? char) (char->numero char) ]
    [ (ehParenteses? char) (char->parenteses char) ]
    [ else '() ]
  )
)

(define (parse-expression stringDeEntrada)
  (tokenizar(string->list stringDeEntrada))
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