(module pp-base racket
  (provide find-in-list make-selector-function accumulate-right)

  (define (find-in-list pred lst)
    (cond ((null? lst) #f)
          ((pred (car lst)) (car lst))
          (else (find-in-list pred (cdr lst)))))
  
  (define (make-selector-function n)
    (lambda (lst) (list-ref lst (- n 1))))
  
  (define (accumulate-right f init lst)
    (if (null? lst)
        init
        (f (car lst) (accumulate-right f init (cdr lst)))))

)