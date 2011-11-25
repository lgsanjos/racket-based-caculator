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

(define (numeroDeParentesesEmLista lista)
  (cond 
        [ (null? lista) 0 ] 

        [ (list? (first lista)) (+ 2 (numeroDeParentesesEmLista (first lista))) ]
        
        [ (eq? (first lista) #\( ) (+ 1 (numeroDeParentesesEmLista (rest lista))) ]
        [ (eq? (first lista) #\) ) (+ 1 (numeroDeParentesesEmLista (rest lista))) ]
        
        
        [ else (+ 0 (numeroDeParentesesEmLista (rest lista))) ]
  ) 
)

;; ---------------

(define (quantidadeDeTokensEmUmaLista lista)
  (cond
    [ (null? lista) 0 ]
    [ (if (list? (first lista))
          (+ (quantidadeDeTokensEmUmaLista (first lista)) (quantidadeDeTokensEmUmaLista (rest lista)))
          (+ 1 (quantidadeDeTokensEmUmaLista (rest lista)))
       )
    ]
    [ else 1 ]
  )
)  

;; ---------------
  
(define (identificaECriaSubListas lista)
  
  (define listaCompleta lista)
    
  (define (reconhece-sub-lista lista)
    (cond
          [ (null? lista) '() ]                      

          [ (eq? (first lista) #\))
            '()
          ]

          [ (eq? (first lista) #\( )
            (identificaECriaSubListas-rec lista)
          ]
          
          [ else (cons (first lista) (reconhece-sub-lista (rest lista))) ]
      )    
  )
  
  (define (identificaECriaSubListas-rec lista)
        
       (cond
          [ (null? lista) '() ]
          
          [ (eq? (first lista) #\) )
            
          (identificaECriaSubListas-rec (rest lista)) 
          ]
 
          [ (eq? (first lista) #\( )
                                    
            (define nova-sub-lista (reconhece-sub-lista (rest lista)))

            (display lista)
            (display #\newline)
            
            (display nova-sub-lista)
            (display #\newline)
            
            (display (+ 2 (numeroDeParentesesEmLista nova-sub-lista) (quantidadeDeTokensEmUmaLista nova-sub-lista)))
            (display #\newline)
            
            (define rabo (list-tail lista (+ 2 (numeroDeParentesesEmLista nova-sub-lista) (quantidadeDeTokensEmUmaLista nova-sub-lista))))
            (display rabo)
            (display #\newline)
             (display #\newline)
            
            (cons nova-sub-lista (identificaECriaSubListas-rec rabo))
          ]

          [ else (cons (first lista) (identificaECriaSubListas-rec (rest lista))) ]
       )
   )

  (identificaECriaSubListas-rec lista)
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
         identificaECriaSubListas)