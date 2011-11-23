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

(define (quantidadeDeTokensEmUmaLista lista)
  
)  

;; ---------------
  
(define (montaTokenLista lista)
    
  (define (outraFunc lista)
    (cond
          [ (null? lista) '() ]                      

          [ (eq? (first lista) #\))
              '()
          ]

          [ (eq? (first lista) #\( )
            (define nova-sub-lista (montaTokenLista-rec lista))
            (define rabo (list-tail (rest lista) (length nova-sub-lista)))            
            (cons nova-sub-lista (outraFunc rabo))
          ]
          
          [ else (cons (first lista) (outraFunc (rest lista))) ]
      )    
  )
  
  (define (montaTokenLista-rec lista)
        
       (cond
          [ (null? lista) '() ]

          [ (eq? (first lista) #\( )
            (define nova-sub-lista (outraFunc (rest lista)))
            (define rabo (list-tail (rest lista) (length nova-sub-lista)))
            (cons nova-sub-lista (montaTokenLista-rec rabo))
          ]

          [ (eq? (first lista) #\))
              (montaTokenLista-rec (rest lista))
          ]

          [ else (cons (first lista) (montaTokenLista-rec (rest lista))) ]
       )
   )

  (montaTokenLista-rec lista)
)  

;; ---------------
  
(define (tokenizar listaDeChar)
  
  (define (tokenizar-rec listaDeChar)    
    (cond 
      [ (null? listaDeChar) '() ]
      
      [ (or (ehOperador? (first listaDeChar)) (ehParenteses? (first listaDeChar)))
           (cons (limpaChar(first listaDeChar)) (tokenizar-rec (rest listaDeChar)))
      ]
      
      [ (ehNumero? (first listaDeChar))          
           (cons (string->number (list->string (blocoDeNumeros listaDeChar)))
              (tokenizar-rec (list-tail listaDeChar (length (blocoDeNumeros listaDeChar)))))
      ]
      
      [ else (tokenizar-rec (rest listaDeChar)) ]
    )
  )
  
  (tokenizar-rec listaDeChar)
)


(provide tokenizar
         montaTokenLista)