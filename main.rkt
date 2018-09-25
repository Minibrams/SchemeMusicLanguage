#lang racket

;; -------------------------------------------
;; OOP Utility Functions
;; -------------------------------------------
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

;; -------------------------------------------
;; Music Utility Functions
;; -------------------------------------------
(define (get-note-duration length bpm)
  (cond ((= length 1/2) (/ 120 bpm))
        ((= length 1/4) (/ 60  bpm))
        ((= length 1/8) (/ 30  bpm))
        ((= length 1/16)(/ 15  bpm))
        (else (error "Cannot calculate duration for an invalid time assignment: " length))
        )
  )

(define (instrument-to-channel instrument)
  (cond ((eqv? instrument 'piano)      1)
        ((eqv? instrument 'orgran)     2)
        ((eqv? instrument 'guitar)     3)
        ((eqv? instrument 'violin)     4)
        ((eqv? instrument 'flute)      5)
        ((eqv? instrument 'trumpet)    6)
        ((eqv? instrument 'helicopter) 7)
        ((eqv? instrument 'telephone)  8)
        )
  )

(define (note-to-midi-number note octave)
  (let ((base (cond ((eqv? note 'C)  24)
                    ((eqv? note 'C#) 25)
                    ((eqv? note 'D)  26)
                    ((eqv? note 'D#) 27)
                    ((eqv? note 'E)  28)
                    ((eqv? note 'F)  29)
                    ((eqv? note 'F#) 30)
                    ((eqv? note 'G)  31)
                    ((eqv? note 'G#) 32)
                    ((eqv? note 'A)  33)
                    ((eqv? note 'A#) 34)
                    ((eqv? note 'B)  35))))
      (+ base (* 12 octave))
    )
  )
         
;; -------------------------------------------
;; Music Construction Functions
;; -------------------------------------------
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

; Function for creating a C# note
(define (C# octave length)
  (lambda (instrument)
  (new-instance note 'C# octave length instrument)))

(define (sequence instrument . list-of-notes)
  (map (lambda (unfinished-note) (unfinished-note instrument))
       list-of-notes))



