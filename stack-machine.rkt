#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)

(define current car)
(define next cdr)

;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack '())
(define (make-stack) '())

(define (push element stack)
  (cons element stack))
(define (top stack)
  (if (null? stack) '() (car stack)))
(define (pop stack)
  (if (null? stack) '() (cdr stack)))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine)
  (cadr stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine)
  (caddr stack-machine))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine)
  (cadddr stack-machine))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine)
  (cadddr (cdr stack-machine)))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine)
  (car stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine)
  (cadddr (cdr (cdr stack-machine))))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (cond
    ((equal? symbol (car symbols)) 0)
    ((equal? symbol (cadr symbols)) 1)
    ((equal? symbol (caddr symbols)) 2)
    ((equal? symbol (cadddr symbols)) 3)
    ((equal? symbol (cadddr (cdr symbols))) 4)
    ((equal? symbol (cadddr (cdr(cdr symbols)))) 5) 
    ))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (cond
    ((equal? (get-symbol-index symbol) 0) (make-stack-machine item (get-varnames stack-machine)
                                                              (get-consts stack-machine) (get-names stack-machine)
                                                              (get-code stack-machine) (get-IC stack-machine)))
    ((equal? (get-symbol-index symbol) 1) (make-stack-machine (get-stack stack-machine) item
                                                              (get-consts stack-machine) (get-names stack-machine)
                                                              (get-code stack-machine) (get-IC stack-machine)))
    ((equal? (get-symbol-index symbol) 2) (make-stack-machine (get-stack stack-machine) (get-varnames stack-machine)
                                                              item (get-names stack-machine)
                                                              (get-code stack-machine) (get-IC stack-machine)))
    ((equal? (get-symbol-index symbol) 3) (make-stack-machine (get-stack stack-machine) (get-varnames stack-machine)
                                                              (get-consts stack-machine) item
                                                              (get-code stack-machine) (get-IC stack-machine)))
    ((equal? (get-symbol-index symbol) 4) (make-stack-machine (get-stack stack-machine) (get-varnames stack-machine)
                                                              (get-consts stack-machine) (get-names stack-machine)
                                                              item (get-IC stack-machine)))
    ((equal? (get-symbol-index symbol) 5) (make-stack-machine (get-stack stack-machine) (get-varnames stack-machine)
                                                              (get-consts stack-machine) (get-names stack-machine)
                                                              (get-code stack-machine) item))
    ))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (make-stack-machine (cons value (get-stack stack-machine)) (get-varnames stack-machine)
                      (get-consts stack-machine) (get-names stack-machine)
                      (get-code stack-machine) (get-IC stack-machine)))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (make-stack-machine (cdr (get-stack stack-machine)) (get-varnames stack-machine)
                      (get-consts stack-machine) (get-names stack-machine)
                      (get-code stack-machine) (get-IC stack-machine)))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (run-stack-machine stack-machine)
  (cond
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'POP_TOP)
     (run-stack-machine (make-stack-machine
                         (pop (get-stack stack-machine)) (get-varnames stack-machine)
                         (get-consts stack-machine) (get-names stack-machine)
                         (get-code stack-machine) (+ 1 (get-IC stack-machine)))))

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'LOAD_CONST)
     (run-stack-machine (make-stack-machine
                         (push (hash-ref (get-consts stack-machine) (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))
                               (get-stack stack-machine))
                         (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine)
                         (get-code stack-machine) (+ 1 (get-IC stack-machine))))) 
 
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'LOAD_GLOBAL)
     (run-stack-machine (make-stack-machine
                         (push (hash-ref (get-names stack-machine) (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))
                               (get-stack stack-machine))
    
                         (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine)
                         (get-code stack-machine) (+ 1 (get-IC stack-machine))))) 

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'STORE_FAST)
     (run-stack-machine (make-stack-machine
                         (pop (get-stack stack-machine)) (hash-set (get-varnames stack-machine)
                                                                   (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))
                                                                   (car (get-stack stack-machine)))
                         (get-consts stack-machine) (get-names stack-machine)
                         (get-code stack-machine) (+ 1 (get-IC stack-machine))))) 

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'LOAD_FAST)
     (run-stack-machine (make-stack-machine
                         (push (hash-ref (get-varnames stack-machine) (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))
                               (get-stack stack-machine))
                         (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine)
                         (get-code stack-machine) (+ 1 (get-IC stack-machine))))) 
    
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'BINARY_ADD)
     (run-stack-machine (make-stack-machine
                         (push (+ (car (cdr (get-stack stack-machine))) (car (get-stack stack-machine)))
                               (cdr (cdr (get-stack stack-machine))))
                         (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine)
                         (get-code stack-machine) (+ 1 (get-IC stack-machine))))) 
    
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'BINARY_SUBTRACT)
     (run-stack-machine (make-stack-machine
                         (push (- (car (cdr (get-stack stack-machine))) (car (get-stack stack-machine)))
                               (cdr (cdr (get-stack stack-machine))))
                         (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine)
                         (get-code stack-machine) (+ 1 (get-IC stack-machine))))) 
    
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'BINARY_MODULO)
     (run-stack-machine (make-stack-machine
                         (push (modulo (car (cdr (get-stack stack-machine))) (car (get-stack stack-machine)))
                               (cdr (cdr (get-stack stack-machine))))
                         (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine)
                         (get-code stack-machine) (+ 1 (get-IC stack-machine))))) 
    
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'INPLACE_ADD)
     (run-stack-machine (make-stack-machine
                         (cons (+ (car (get-stack stack-machine)) (car (cdr (get-stack stack-machine))))
                               (cdr (cdr (get-stack stack-machine))))
                         (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine)
                         (get-code stack-machine) (+ 1 (get-IC stack-machine))))) 
    
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'INPLACE_SUBTRACT)
     (run-stack-machine (make-stack-machine
                         (push (- (car (cdr (get-stack stack-machine))) (car (get-stack stack-machine)))
                               (cdr (cdr (get-stack stack-machine))))
                         (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine)
                         (get-code stack-machine) (+ 1 (get-IC stack-machine))))) 

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'INPLACE_MODULO)
     (run-stack-machine (make-stack-machine
                         (push (modulo (car (cdr (get-stack stack-machine))) (car (get-stack stack-machine)))
                               (cdr (cdr (get-stack stack-machine))))
                         (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine)
                         (get-code stack-machine) (+ 1 (get-IC stack-machine))))) 

    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'JUMP_ABSOLUTE)
     (run-stack-machine (make-stack-machine
                         (get-stack stack-machine) (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine)
                         (get-code stack-machine) (/ (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))) 2))))
     
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'COMPARE_OP)
     (run-stack-machine (make-stack-machine
                         (push ((get-cmpop (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))))
                                (car (cdr (get-stack stack-machine))) (car (get-stack stack-machine)))
                               (cdr (cdr (get-stack stack-machine))))
                         (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine)
                         (get-code stack-machine) (+ 1 (get-IC stack-machine))))) 
  
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'POP_JUMP_IF_FALSE)
     (run-stack-machine (if (eq? (car (get-stack stack-machine)) #f)
                            (make-stack-machine (pop (get-stack stack-machine)) (get-varnames stack-machine)
                                                (get-consts stack-machine) (get-names stack-machine) (get-code stack-machine)
                                                (/ (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))) 2))
                            
                            (make-stack-machine (cdr (get-stack stack-machine)) (get-varnames stack-machine)
                                                (get-consts stack-machine) (get-names stack-machine)
                                                (get-code stack-machine) (+ 1 (get-IC stack-machine))))))
    
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'POP_JUMP_IF_TRUE)
     (run-stack-machine (if (eq? (car (get-stack stack-machine)) #t)
                            (make-stack-machine (cdr (get-stack stack-machine)) (get-varnames stack-machine)
                                                (get-consts stack-machine) (get-names stack-machine) (get-code stack-machine)
                                                (/ (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))) 2))
                            
                            (make-stack-machine (cdr (get-stack stack-machine)) (get-varnames stack-machine)
                                                (get-consts stack-machine) (get-names stack-machine)
                                                (get-code stack-machine) (+ 1 (get-IC stack-machine))))))
    ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'FOR_ITER)
     (run-stack-machine (if (null? (top (get-stack stack-machine)))
                              (make-stack-machine
                                     (cdr (get-stack stack-machine)) (get-varnames stack-machine)
                                     (get-consts stack-machine) (get-names stack-machine)
                                     (get-code stack-machine)
                                     (+ (/ (+ (cdr (list-ref (get-code stack-machine) (get-IC stack-machine))) 2) 2)
                                                 (get-IC stack-machine)))
                              (make-stack-machine
                                     (cons (current (top (get-stack stack-machine)))
                                            (cons (next (top (get-stack stack-machine)))
                                                          (cdr (get-stack stack-machine))))
                                     (get-varnames stack-machine)
                                     (get-consts stack-machine) (get-names stack-machine)
                                     (get-code stack-machine)
                                     (+ 1 (get-IC stack-machine)))
                         )))
     ; implementare bonus pentru sqrt, print si range, cazul un singur argument pentru functie
     ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'CALL_FUNCTION)
       (run-stack-machine (make-stack-machine
                           (push ((get-function (car (cdr (get-stack stack-machine)))) (top (get-stack stack-machine)))
                                                                  (pop (pop (get-stack stack-machine))))
                           (get-varnames stack-machine) (get-consts stack-machine) (get-names stack-machine)
                           (get-code stack-machine)
                           (+ 1 (get-IC stack-machine)))))
                                   
       
      ((equal? (car (list-ref (get-code stack-machine) (get-IC stack-machine))) 'RETURN_VALUE)
               stack-machine)

      (else (run-stack-machine (make-stack-machine
                                (get-stack stack-machine) (get-varnames stack-machine)
                                (get-consts stack-machine) (get-names stack-machine)
                                (get-code stack-machine) (+ 1 (get-IC stack-machine)))))
   )
 )