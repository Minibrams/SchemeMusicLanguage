#lang racket
; Function for getting object methods
(define (method-lookup object selector)
  (cond ((procedure? object)
         (let ((result (object selector)))
           (if (procedure? result)
               result
               (error "Did not find any method")))
        )
        (else
         (error "Inappropriate object in method-lookup: " object))))

; Function for calling object methods with parms
(define (send message obj . par)
  (let ((method (method-lookup obj message)))
    (apply method par)))

; Function for creating new instances of classes
(define (new-instance class . parms)
  (apply class parms))

; Note class definition
(define (note tone octave length instrument)
  (let ((tone       tone)
        (octave     octave)
        (length     length)
        (instrument instrument))
    
    (define (gettone)       tone)
    (define (getoctave)     octave)
    (define (getlength)     length)
    (define (getinstrument) instrument)

    (define (gettype) 'note)

    (define (self message)
      (cond ((eqv? message 'gettone)       gettone)
            ((eqv? message 'getoctave)     getoctave)
            ((eqv? message 'getlength)     getlength)
            ((eqv? message 'getinstrument) getinstrument)
            ((eqv? message 'gettype)       gettype)
            (else (error "Message not understood: " message))))
    
    self)) ; Return the handler when object is instantiated

(define (C# octave length)
  (lambda (instrument)
  (new-instance note 'C# octave length instrument)))

(define (sequence instrument . list-of-notes)
  (map (lambda (unfinished-note) (unfinished-note instrument))
       list-of-notes))

(define (parallel list-of-sequences)
  )


(define music (sequence 'guitar (C# 2 1/4) (C# 3 1/5) (C# 1 1/4)))
