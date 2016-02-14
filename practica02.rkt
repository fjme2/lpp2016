#lang r6rs
(import (rnrs base)
        (rnrs io simple)
        (schemeunit))

;;EJERCICIO 1
;;Demostración
(display "\n\nEjercicio 1.a\n")
(display "Funcionan de forma distinta porque cond va teniendo en cuenta las variables conforme \nlas va utilizando mientras que new-cond tiene en cuenta las variables desde el principio.\n")
(display "\nEjercicio 1.b\n")
(display "No son funciones porque no evaluan todos los argumentos.")

;;EJERCICIO 2
(define (minimo lista)
  (if (equal? (cdr lista) '())
      (car lista)
      (if  (< (car lista) (car (cdr lista)))
       (minimo (cons (car lista) (cdr (cdr lista))))
       (minimo (cdr lista)))))
       

;;Demostración
(display "\n\nEjercicio 2\n")
(display "El menor de 9 8 6 4 y 3 es: ")
(display (minimo '(9 8 6 4 3)))

;;Pruebas
(check-equal? (minimo '(9 8 6 4 3)) 3)
(check-equal? (minimo '(9 8 3 6 4)) 3)
(check-equal? (minimo '(9 2 6 2 3)) 2)
(check-equal? (minimo '(0 9 6 3 3)) 0)

;;EJERCICIO 3
(define (ordenada-decreciente? lista)
  (if (equal? (cdr lista) '())
      #t
      (if (> (car (cdr lista)) (car lista))
          #f
          (ordenada-decreciente? (cdr lista)))))
          
;;Demostración
(display "\n\nEjercicio 3\n")
(display "La lista 99 59 45 23 y -1 es decreciente? ")
(display (ordenada-decreciente? '(99 59 45 23 -1)))

;;Pruebas
(check-true (ordenada-decreciente? '(99 59 45 23 -1)))
(check-false (ordenada-decreciente? '(12 50 -1 293 1000)))
(check-true (ordenada-decreciente? '(3)))
(check-false (ordenada-decreciente? '(1000 500 -1 293)))

;;EJERCICIO 4.a
(define i1 (cons 4 9))
(define i2 (cons 3 10))
(define i3 (cons 12 15))
(define i4 (cons 8 19))

(define (engloban-intervalos? a b)
  (if (or (equal? a 'vacio) (equal? b 'vacio))
      #t
      (if (equal? (or a b) 'vacio )
          #t
          (if (> (car b) (car a))
              (if (<= (cdr b) (cdr a))
                  #t
                  #f)
              (if (<= (cdr a) (cdr b))
                  #t
                  #f)))))

;;Demostración      
(display "\n\nEjercicio 4.a\n")
(display "Las parejas [5,9] y [4,8] se engloban? ")
(display (engloban-intervalos? (cons 5 9) (cons 4 8)))

;;Pruebas
(check-false (engloban-intervalos? (cons 5 9) (cons 4 8)))
(check-true (engloban-intervalos? i1 i2))
(check-true (engloban-intervalos? i3 'vacio))
(check-false (engloban-intervalos? i1 i4))

;;EJERCICIO 4.b
(define (union-intervalos a b)
    (cond ((equal? a 'vacio) b) ((equal? b 'vacio) a)
          (else (cons (min (car a) (car b)) (max (cdr a) (cdr b))))))

;;Demostración      
(display "\n\nEjercicio 4.b\n")
(display "Que pareja engloba las parejas [4,10] y [3,8]? ")
(display (union-intervalos (cons 4 10) (cons 3 8)))

;;Pruebas
(check-equal? (union-intervalos (cons 4 10) (cons 3 8)) (cons 3 10))
(check-equal? (union-intervalos i2 i3) (cons 3 15))
(check-equal? (union-intervalos 'vacio i4) (cons 8 19))
(check-equal? (union-intervalos (cons 8 12) (cons 3 4)) (cons 3 12))

;;EJERCICIO 4.c
;;funcion engloba? P1
(define (engloba? a b)
  (if (> (car b) (car a))
      (if (<= (cdr b) (cdr a))
          #t
          #f)
      (if (<= (cdr a) (cdr b))
          #t
          #f)))
;;funcion intersectan? P1
(define (intersectan? a b)
  (if (engloba? a b)
      #t
      (if (or (> (car a) (cdr b)) (< (cdr a) (car b)))
          #f
          #t)))

;;funcion principal
(define (interseccion-intervalos a b)
  (if (or (equal? a 'vacio) (equal? b 'vacio))
      'vacio
      (if (intersectan? a b)
          (cons (max (car a) (car b)) (min (cdr a) (cdr b)))
          'vacio)))

;;Demostración      
(display "\n\nEjercicio 4.c\n")
(display "Cual es la interseccion entre las parejas [4,10] y [8,15]? ")
(display (interseccion-intervalos (cons 4 10) (cons 8 15)))

;;Pruebas
(check-equal? (interseccion-intervalos (cons 4 10) (cons 8 15)) (cons 8 10))
(check-equal? (interseccion-intervalos i1 i3) 'vacio)
(check-equal? (interseccion-intervalos 'vacio i4) 'vacio)
(check-equal? (interseccion-intervalos (cons 4 10) (cons 3 8)) (cons 4 8))

;;EJERCICIO 5
(define (union-lista-intervalos lista)
    (if (null? lista) 'vacio
        (union-intervalos (car lista) (union-lista-intervalos (cdr lista)))))

(define (interseccion-lista-intervalos lista)
    (if (null? (cdr lista))
        (car lista) (interseccion-intervalos (car lista) (interseccion-lista-intervalos (cdr lista)))))

;;Demostración      
(display "\n\nEjercicio 5\n")
(display "Cual es la pareja resultante de la union de la lista '((2 . 12) (-1 . 10) (8 . 20))? ")
(display (union-lista-intervalos (list (cons 2 12) (cons -1 10) (cons 8 20))))

;;Pruebas
(check-equal? (union-lista-intervalos (list (cons 2 12) (cons -1 10) (cons 8 20))) (cons -1 20))
(check-equal? (interseccion-lista-intervalos (list (cons 12 30) (cons 13 35) (cons -8 20))) (cons 13 20))
(check-equal? (interseccion-lista-intervalos (list (cons 25 30) (cons 13 35) (cons -8 20))) 'vacio)
(check-equal? (union-lista-intervalos (list (cons 10 12) (cons 15 16) (cons 18 20))) (cons 10 20))
