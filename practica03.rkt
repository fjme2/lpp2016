#lang r6rs
(import (rnrs base)
        (rnrs io simple)
        (schemeunit))

;;Ejercicio1
;;a
;;P1
(display "\nEjercicio 1.a:")
(display "\nP1 -> ")
(display (list (cons 1 2) 3))

;;P2
(display "\nP2 -> ")
(display (list (list 'a 'b)))

;;P3
(display "\nP3 -> ")
(display (cons 4 (cons (list (cons 1 2) 3) (cons (list (list 'a 'b)) 5))))

;;b
(display "\n\nEjercicio 1.b: ")
(display "\nP1 y P2 si que son listas porque acaban en lista vacia mientras que p3 no lo es porque no lo hace")

;;c
(define P3 (cons 4 (cons (list (cons 1 2) 3) (cons (list (list 'a 'b)) 5))))
(display "\n\nEjercicio 1.c:\n")
(display (car (cdr (car (cdr P3)))))
(display "\n")
(display (cdr (cdr (cdr P3))))


;;Ejercicio 2
;;a
(define (intercambia-elem pareja)
  (cons (cdr pareja) (car pareja)))

;;Demostración
(display "\n\nEjercicio 2.a:\n")
(display "Intercambia la pareja [10,5]: ")
(display (intercambia-elem (cons 10 5)))

;;Pruebas
(check-equal? (intercambia-elem (cons 10 5)) (cons 5 10))
(check-equal? (intercambia-elem (cons 20 30)) (cons 30 20))
(check-equal? (intercambia-elem (cons -1 5)) (cons 5 -1))
(check-equal? (intercambia-elem (cons 6 5)) (cons 5 6))

;;b
(define (suma-izq n pareja)
  (cons (+ n (car pareja)) (cdr pareja)))
(define (suma-der n pareja)
  (cons  (car pareja) (+ n (cdr pareja))))

;;Demostración
(display "\n\nEjercicio 2.b:\n")
(display "Sumale 5 a la parte izquierda de la pareja [10,20]: ")
(display (suma-izq 5 (cons 10 20)))
(display "\nSumale 6 a la parte derecha de la pareja [10,20]: ")
(display (suma-der 6 (cons 10 20)))

;;Pruebas
(check-equal? (suma-izq 5 (cons 10 20)) (cons 15 20))
(check-equal? (suma-der 6 (cons 10 20)) (cons 10 26))
(check-equal? (suma-der 20 (cons 10 5)) (cons 10 25))
(check-equal? (suma-izq 15 (cons -1 5)) (cons 14 5))

;;c
(define (suma-impares-pares lista-num)
  (if (null? lista-num)
      (cons 0 0)
      (if (odd? (car lista-num))
          (suma-izq (car lista-num) (suma-impares-pares (cdr lista-num)))
          (suma-der (car lista-num) (suma-impares-pares (cdr lista-num))))))

;;Demostracion
(display "\n\nEjercicio 2.c:\n")
(display "Suma los pares a la derecha y los impares a la izquierda de la lista '(3 2 1 4 8 7 6 5): ")
(display (suma-impares-pares '(3 2 1 4 8 7 6 5)))

;;Pruebas
(check-equal? (suma-impares-pares '(3 2 1 4 8 7 6 5)) (cons 16 20))
(check-equal? (suma-impares-pares '(3 1 5)) (cons 9 0))
(check-equal? (suma-impares-pares '(15 5 6 8 9)) (cons 29 14))
(check-equal? (suma-impares-pares '(8 4 1 2 5 6)) (cons 6 20))

;;Ejercicio 3
(define (multiplo n1 n2)
  (if (< n2 n1)
      #f
      (if (= 0 (mod n2 n1))
      #t
      #f)))

(define (multiplo-de n lista-nums)
  (if (null? lista-nums)
      lista-nums
      (if (multiplo n (car lista-nums) )
          (cons #t (multiplo-de n (cdr lista-nums)))
          (cons #f (multiplo-de n (cdr lista-nums))))))

;;Demostracion
(display "\n\nEjercicio 3:\n")
(display "Son multiplos de 10 los elementos dela lista '(100 23 10 300 48 7)?: ")
(display (multiplo-de 10 '(100 23 10 300 48 7)))

;;Pruebas
(check-equal? (multiplo-de 10 '(100 23 10 300 48 7)) '(#t #f #t #t #f #f))
  