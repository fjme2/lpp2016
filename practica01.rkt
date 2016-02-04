#lang r6rs
(import (rnrs base)
        (rnrs io simple)
        (schemeunit))

;;EJERCICIO 1
(define (mayor-de-tres n1 n2 n3)
  (if  (> n1 n2)
       (if (> n1 n3)
           n1
           n3)
       (if (> n2 n3)
           n2
           n3)))

;;Demostración
(display "\n\nEjercicio 1\n")
(display "El mayor de 2,8 y 1 es: ")
(display (mayor-de-tres 2 8 1))

;;Pruebas
(check-equal? (mayor-de-tres 2 8 1) 8)
(check-equal? (mayor-de-tres 3 0 3) 3)
(check-equal? (mayor-de-tres 4 4 2) 4)
(check-equal? (mayor-de-tres 0 5 9) 9)


;;EJERCICIO 2

;;funciones auxiliares
;;funcion para hacer potencia cuadrada
(define (al-cuadrado x)
  (* x x))

;; Fijamos el margen de error (precisión) para comparar números reales
(define precision 0.000001)

;; función auxiliar que compara números reales teniendo en cuenta una determinada precisión
(define (iguales-reales? x y)
  (< (abs (- x y)) precision))

;;funcion principal
(define (distancia-euclidea p1 p2)
  (sqrt (+ (al-cuadrado (- (cdr p2) (cdr p1))) (al-cuadrado (- (car p2) (car p1))))))

;;Demostracion
(display "\n\nEjercicio 2\n")
(display "La distancia euclidea entre [0,4] y [0,10] es: ")
(display (distancia-euclidea (cons 0 4) (cons 0 10)))

;;Pruebas
(check-true (iguales-reales? (distancia-euclidea (cons 0 4) (cons 0 10)) 6.0))
(check-true (iguales-reales? (distancia-euclidea (cons -2 5) (cons 9 7)) 11.180339))
(check-true (iguales-reales? (distancia-euclidea (cons 1 3) (cons 7 9)) 8.485281))
(check-true (iguales-reales? (distancia-euclidea (cons 0 0) (cons 5 4)) 6.403124))

;;EJERCICIO 3
(define (engloba? a1 a2 b1 b2)
  (if (> b1 a1)
      (if (<= b2 a2)
          #t
          #f)
      (if (<= a2 b2)
          #t
          #f)))

;;Demostracion
(display "\n\nEjercicio 3\n")
(display "La parejas [4,10] y [5,9] se engloban? ")
(display (engloba? 4 10 5 9))

;;Pruebas
(check-true (engloba? 4 10 5 9))
(check-true (engloba? 4 9 4 15))
(check-false (engloba? 2 6 4 8))
(check-true (engloba? 5 9 6 9))

      