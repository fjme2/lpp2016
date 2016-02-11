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

;;EJERCICIO 4
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

      
(display "\n\nEjercicio 4.a\n")
(display "Las parejas [5,9] y [4,8] se engloban? ")
(display (engloban-intervalos? (cons 5 9) (cons 4 8)))

;;Pruebas
(check-false (engloban-intervalos? (cons 5 9) (cons 4 8)))
(check-true (engloban-intervalos? i1 i2))
(check-true (engloban-intervalos? i3 'vacio))
(check-false (engloban-intervalos? i1 i4))
