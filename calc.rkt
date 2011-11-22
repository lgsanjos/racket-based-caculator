#lang racket

(require "verifica_tipos.rkt")


(define (blocoDeNumeros lista)
  (cond 
        [ (null? lista) '() ] 

        [ (ehNumero? (first lista))
            (cons (first lista) (blocoDeNumeros (rest lista)))
        ]

        [ else '() ]
  )    
)

(define (blocoDeParanteses lista)
  (cond 
        [ (null? lista) '() ] 
        [ (not (eq? #\) (first lista)))
            (cons (first lista) (blocoDeParanteses (rest lista)))
        ]
        [ else '() ]
        
  )    
)

;; ---------------

(define (tokenizar-caso-operador lista)
  (cons (limpaChar(first lista)) (tokenizar (rest lista)))
)
  
(define (tokenizar-caso-numero lista)
  (cons (string->number (list->string (blocoDeNumeros lista)))
        (tokenizar (list-tail lista (length (blocoDeNumeros lista)))))
)
  
(define (tokenizar-caso-abre-parenteses lista)
   (cond 
     [ (eq? (first lista) #\)) '() ]
     ;[ (eq? (first lista) #\() (tokenizar lista) ]
     [ else (tokenizar (rest lista)) ]
   )
)


(define (tokenizar listaDeChar)
  (cond 
        [ (null? listaDeChar) '() ]
        
        [ (eq? (first listaDeChar) #\( )
          (cons (tokenizar-caso-abre-parenteses listaDeChar) (tokenizar (list-tail listaDeChar (length(blocoDeParanteses listaDeChar)))))
        ]
        
        [ (ehOperador? (first listaDeChar))
             (tokenizar-caso-operador listaDeChar)
        ]
        
        [ (ehNumero? (first listaDeChar))
             (tokenizar-caso-numero listaDeChar)
        ]
        
        [ else (tokenizar (rest listaDeChar)) ]
  )
)

;; ---------------

(define (limpaChar char)
  (cond
    [ (ehOperador? char) (char->simbolo char) ]
    [ (ehNumero? char) (char->numero char) ]
    [ (ehParenteses? char) (char->parenteses char) ]
    [ else '() ]
  )
)

;; ---------------

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