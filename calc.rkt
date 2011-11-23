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

;; ---------------
  
(define (montaTokenLista lista)
  
  (define (montaTokenLista-rec lista)
  
        (cond
          
          [ (null? lista) '() ]   
                    
          [ (ehOperador? (first lista))
            (cons (limpaChar(first lista)) (montaTokenLista-rec (rest lista)))            
          ]
          
          [ (ehNumero? (first lista))
            (cons (string->number (list->string (blocoDeNumeros lista)))
                  (montaTokenLista-rec (list-tail lista (length (blocoDeNumeros lista)))))
          ]
                    
          [ (eq? (first lista) #\))  
            '()
          ]
          
          [ else (montaTokenLista-rec (rest lista)) ]
          

     )
   )
  
  (montaTokenLista-rec lista)
)  

;; ---------------
  
(define (tokenizar listaDeChar)
  
  (define tamanho 1)
  
  (define (tokenizar-rec listaDeChar)    
    (cond 
      [ (null? listaDeChar) '() ]
      
      [ (ehOperador? (first listaDeChar))
           (set! tamanho (+ 1 tamanho))           
           (cons (limpaChar(first listaDeChar)) (tokenizar-rec (rest listaDeChar)))
      ]
      
      [ (ehNumero? (first listaDeChar))
           (set! tamanho (+ 1 tamanho))           
           (cons (string->number (list->string (blocoDeNumeros listaDeChar)))
              (tokenizar-rec (list-tail listaDeChar (length (blocoDeNumeros listaDeChar)))))
      ]
      
      [ (eq? (first listaDeChar) #\( )
        (cons (montaTokenLista listaDeChar)             
              ;(tokenizar-rec (list-tail listaDeChar (+ tamanho (length (montaTokenLista listaDeChar)))))
              (tokenizar-rec (list-tail listaDeChar (- (length listaDeChar) (length (montaTokenLista listaDeChar)))))
        )
      ]        
      
      [ else (tokenizar-rec (rest listaDeChar)) ]
    )
  )
  
  (tokenizar-rec listaDeChar)
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
; (parse-expression "4+5 - 1 ( 2 * 22) + 2")
; (parse-expression "4+ (5 ^ 2) - 1 ( 2 * 22) + 2")