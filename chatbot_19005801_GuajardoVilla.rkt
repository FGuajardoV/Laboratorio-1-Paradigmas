#lang r6rs
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs io simple (6))
        (srfi :19)           ;Libreria tiempo (documentacion https://srfi.schemers.org/srfi-19/srfi-19.html)  
        (srfi :27))          ;Libreria Random (documentacion https://srfi.schemers.org/srfi-27/srfi-27.html)

;TDA chatbot
(define vendedorPeliculas (list "Bot: Saludos y bienvenido a la tienda, que pelicula desea comprar?"
                          "Bot: Perfecto, en que calidad prefiere, BluRayDisc o DVD?"
                          (cons "Bot: Okey, el precio es de 50,999, Con que medio desea pagar?" "Bot: Okey, el precio es de 39,990, Con que medio desea pagar?")
                          "Bot: Muchas gracias por su compra, hasta luego y que la disfrute"
                          (cons "Bot: No existe tal titulo" "Bot: No tenemos stock")
              )
        )
(define vendedorMangas (list "Bot: Saludos y bienvenido a la tienda, que Manga desea comprar?"
                       "Bot: Perfecto, actualmente tenemos este manga en ingles y español, cual desea?"
                       (cons "Bot: Okey, el tomo cuesta 10,990, Con que medio desea pagar?" "Bot: Ok, este tomo sale 10,990, Con que medio desea pagar?")
                       "Bot: Muchas gracias por su compra, hasta luego y que disfrute su lectura"
                       (cons "Bot: No existe tal titulo" "Bot: No tenemos stock")
              )
        )
(define vendedorVideojuegos (list "Bot: Saludos y bienvenido a la tienda, que juego desea comprar?"
                            "Bot: Perfecto, para que consola lo desea?"
                            (cons "Bot: Okey, el precio es de 39,999, Con que medio desea pagar?" "Bot: Okey, el precio es de 45,990, Con que medio desea pagar?")
                            "Bot: Muchas gracias por su compra, hasta luego y que comience la aventura!"
                            (cons "Bot: No existe tal titulo" "No tenemos stock")
              )
        )

;Constructor del TDA Chatbot
(define chatbot (list vendedorPeliculas vendedorMangas vendedorVideojuegos))

;Selector del TDA Chatbot
(define (selectorProducto chatbot seed)
  (cond [(= 1 seed) (car chatbot)]
        [(= 2 seed) (car (cdr chatbot))]
        [(= 3 seed) (car (cdr (cdr chatbot)))]
        )
  )

;Pertenencia del TDA Chatbot
(define (chatbot? chatbot)
  (cond [(and (= 3 (length chatbot)) (list? (car chatbot)) (list? (car (cdr chatbot))) (list? (car (cdr (cdr chatbot))))) #t]
        [else #f]
        )
  )

;Se crea el log base
(define (log) (list (cons "begin" (numeroRandom))))

;se define la fecha para iniciar el log con el dia en el cual se inicio la conversacion
(define (fecha) (date->string (current-date) "~5"))

;se define un numero random el cual sera el identificador de la conversacion
(define (numeroRandom) (random-integer 10000))

#|Funcion de informe de error
>Descripcion: informa al usuario algun error de entrada en la funcion beginDialog
>Parametros:  *num: El cual informa a la maquina que error tuvo el usuario
>Dominio:     *num - un integer en el rango [1, 2]
>Recorrido:   *Display en pantalla, no retorna nada
|#

(define (error num)
  (cond [(= 1 num) (display "Error en el chatbot ingresado, asegurese de ingresar bien el tipo de dato")]
        [(= 2 num) (display "Error al ingresar semilla, asegurese de ingresar un numero entero entre 1 y 3 incluidos")]
        )
  )

#| Funcion buildAnswer
>Descripcion: Funcion que busca en el chatbot la respuesta adecuada dependiendo de la situacion
              entregada por el usuario
>Parametros:  *chatbot: es el tda chatbot el cual incluyo los 3 vendedores
              *seed: en este caso es el vendedor el cual el usuario escogio
              *aux: un auxiliar para ubicar a la maquina en que parte de la conversacion se encuentra
>Dominio:     *Chatbot - todo elemento que sea chatbot (pasa por funcion pertenencia
              *seed - Un integer que se mueve en el intervalo [1, 3]
              *aux - debe de ser un integer entre [2, 8]
>Recorrido:   *String - contenedor del mensaje que retornara el chatbot, debe de pertenecer a el TDA
|#

(define (buildAnswer chatbot seed aux)
  (cond [(= aux 2) (car (cdr (selectorProducto chatbot seed)))]
        [(= aux 4) (cond  [(= 1 (random-integer 2)) (car (car (cdr (cdr (selectorProducto chatbot seed)))))]
                          [else (cdr (car (cdr (cdr (selectorProducto chatbot seed)))))])]
        [(= aux 6) (car (cdr (cdr (cdr (selectorProducto chatbot seed)))))]
        [(= aux 8) (car (cdr (cdr (cdr (cdr (selectorProducto chatbot seed))))))]
        )
  )

#| Funcion largoLista
>Descripcion: Esta funcion retorna el largo de una lista
>Parametros:  *log: es la lista a analizar
>Dominio:     *log - lista de elementos
>Recorrido:   *contador - un integer natural positivo
>Recursion:   En esta funcion se usa una recursion de cola
>
|#

(define (largo log contador)
  (cond [(null? log) contador]
        [else (largo (cdr log) (+ 1 contador))]
        )
  )
(define (largoLog log) (largo log 0))

#| Funcion beginDialog
>Descripcion: Funcion que inicia un nuevo dialogo entre el usuario y el chatbot, siendo
              este ultimo el que inicia la conversacion con un mensaje de bienvenida.
              A su vez este se inicia con un identificador, el cual es random, para luego
              poder evaluar la conversacion.
>Parametros:  *chatbot: es el tda chatbot el cual incluyo los 3 vendedores
              *log: es el registro de la conversacion
              *seed: en este caso es el vendedor el cual el usuario escogio
>Dominio:     *Chatbot - todo elemento que sea chatbot (pasa por funcion pertenencia
              *log - lista de listas, debe de estar vacia o con el identificador de inicio
              *seed - Un integer que se mueve en el intervalo [1, 3]
>Recorrido:   *log reconfigurado - debe de ser la lista con el identificador de inicio de la conversacion
              y el mensaje de bienvenida
|#

(define (beginDialog chatbot log seed)
  (cond [(or (not (integer? seed)) (> seed 3) (= 0 seed)) (error 2)]
        [(not (chatbot? chatbot)) (error 1)]
        [else (append (log) (list (cons (fecha) (car (selectorProducto chatbot seed)))))]
        )
  )
        
#| Funcion sendMessage
>Descripcion: Permite realizar envios de mensajes desde el usuario al chatbot
>Parametros:  *msg: corresponde a un string con el mensaje formulado por el usuario
              *chatbot: es el tda chatbot el cual incluyo los 3 vendedores
              *log: es el registro de la conversacion
              *seed: en este caso es el vendedor el cual el usuario escogio
>Dominio:     *msg - Un string ("ejemplo")
              *Chatbot - todo elemento que sea chatbot (pasa por funcion pertenencia
              *log - lista de lista.
              *seed - Un integer que se mueve en el intervalo [1, 3]
>Recorrido:   *log reconfigurado - una lista de listas contenedora del registro
|#

(define (sendMessage msg chatbot log seed)
  (append log (list (cons (fecha) msg) (cons (fecha) (buildAnswer chatbot seed (largoLog log)))))
  )

#| Funcion endDialog
>Descripcion: Permite dar cierre a un dialogo, donde el mensaje final es siempre ofrecido por el chatbot
>Parametros:  *chatbot: es el tda chatbot el cual incluyo los 3 vendedores
              *log: es el registro de la conversacion
              *seed: en este caso es el vendedor el cual el usuario escogio
>Dominio:     *Chatbot - todo elemento que sea chatbot (pasa por funcion pertenencia
              *log - lista de listas
              *seed - Un integer que se mueve en el intervalo [1, 3]
>Recorrido:   *log reconfigurado - debe de ser la lista con el identificador de final de la conversacion
              y el mensaje de despedida
|#

(define (endDialog chatbot log seed)
  (append log (list "EndDialog" (fecha)))
  )

#| Funcion rate|#
(define (rate chatbot score f log) (display "no hace nada"))

;creamos las listas de usuario para hacer uso de la funcion test
(define user1 (list "Begin" "Harry Potter" "DVD" "Tarjeta"))              ;user seed 1
(define user3 (list "begin" "FinalFantasy 7" "PS1" "Efectivo"))           ;user seed 3
(define user2 (list "Begin" "Shingeki no Kyojin" "Ingles" "Credito"))     ;user seed 2

#| Funcion test
>Descripcion: Permite simular una conversacion user-chatbot ya predicha por un registro historico
>Parametros:  *user: lista de mensajes provistos para generar una conversacion fluida con el bot
              *chatbot: es el tda chatbot el cual incluyo los 3 vendedores
              *log: es el registro de la conversacion
              *seed: en este caso es el vendedor el cual el usuario escogio
>Dominio:     *user - lista de strings
              *Chatbot - todo elemento que sea chatbot (pasa por funcion pertenencia
              *log - lista de listas
              *seed - Un integer que se mueve en el intervalo [1, 3]
>Recorrido:   *log reconfigurado - una lista de listas contenedora del registro
>Recursion:   Esta presente la recursion natural
|#

(define (test user chatbot log seed)
  (cond [(= 4 (largoLog user)) (test (cdr user) chatbot (beginDialog chatbot log seed) seed)]
        [(= 3 (largoLog user)) (test (cdr user) chatbot (sendMessage (car user) chatbot log seed) seed)]
        [(= 2 (largoLog user)) (test (cdr user) chatbot (sendMessage (car user) chatbot log seed) seed)]
        [(= 1 (largoLog user)) (test (cdr user) chatbot (sendMessage (car user) chatbot log seed) seed)]
        [(= 0 (largoLog user)) (test (list 1 2 3 4 5 6 7 8 9 0) chatbot (endDialog chatbot log seed) seed)]
        [else log])
  )

#| Funcion displayLog|#
(define (displayLog chatbot log)
  (display log))

