#lang racket


;; -------------------------------------------
;; Music Utility Functions
;; -------------------------------------------

; Convert fraction-durations to MIDI time units
(define (get-note-duration length bpm)
  (let ((seconds-per-measure (* 60 (/ 4 bpm)))) ; Assuming 4/4 measures
    (*(* length seconds-per-measure) 960) ; Seconds to MIDI time units
    )
  )

; Map a known instrument to a MIDI channel
(define (instrument-to-channel instrument)
  (cond ((eqv? instrument 'piano)      1)
        ((eqv? instrument 'organ)      2)
        ((eqv? instrument 'guitar)     3)
        ((eqv? instrument 'violin)     4)
        ((eqv? instrument 'flute)      5)
        ((eqv? instrument 'trumpet)    6)
        ((eqv? instrument 'helicopter) 7)
        ((eqv? instrument 'telephone)  8)
        )
  )

; Map a musical note to a MIDI number
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
;; OOP Utility Functions
;; -------------------------------------------
; Function for getting object methods
(define (method-lookup object selector)
  (cond ((procedure? object) (object selector))
        (else (error "Inappropriate object in method-lookup: " object))))

; Function for calling object methods with parms
(define (send message obj . par)
  (let ((method (method-lookup obj message)))
    (cond ((procedure? method) (apply method par))
          ((null? method) (error "Message not understood: " message))
          (else (error "Inappropriate result of method look-up: " method)))))

; Function for creating new instances of classes
(define (new-instance class . parms)
  (let ((instance (apply class parms)))
    (virtual-operations instance)
    instance))

; Virtual operations - propagate set-self through inheritance tree
(define (virtual-operations object)
  (send 'set-self! object object))

; Function for creating new parts of classes
(define (new-part class . parms)
  (apply class parms))

;; -------------------------------------------
;; Classes
;; -------------------------------------------

; Music element class - this is the base for Notes, Sequences, and Parallel.
(define (music-element start-time duration . music-elements)
  (let ((super '())
        (self 'nil))

    (let ((start-time       start-time)
          (duration         duration))
      
      (define (set-self! object-part)
        (set! self object-part))
      
      (define (get-type)       'music-element)
      (define (get-start-time) start-time)
      (define (get-duration)   duration)
      (define (get-duration-absolute) (get-note-duration duration 100))
      (define (get-info)(list  (get-type)
                               (get-start-time)
                               (get-duration)))

      (define (get-midi)
        (map (lambda (element) (send 'get-midi element)) (car music-elements)))
    
      (define (dispatch message)
        (cond ((eqv? message 'set-self!)      set-self!)
              ((eqv? message 'get-type)       get-type)
              ((eqv? message 'get-start-time) get-start-time)
              ((eqv? message 'get-duration)   get-duration)
              ((eqv? message 'get-info)       get-info)
              ((eqv? message 'get-midi)       get-midi)
              ((eqv? message 'get-duration-absolute) get-duration-absolute)
              (else '())))

      (set! self dispatch)
      )
    
    self))


; Pause class
(define (pause start-time length)
  (let ((super (new-part music-element start-time length))
        (self 'nil))

    (define (get-type) 'pause)
    (define (get-info) (list (send 'get-start-time super)
                             (send 'get-duration super)
                             (get-type)))
    
    (define (set-self! object-part)
        (set! self object-part)
        (send 'set-self! super object-part))

    (define (get-midi)
        (cons 'note-abs-time-with-duration (list
              (get-note-duration start-time 100) ; Absolute start time
              1  ; Channel no.
              0  ; Note no.
              0  ; Velocity (constant)
              (get-note-duration length 100)))) ; Duration

    (define (dispatch message)
      (cond ((eqv? message 'get-info)  get-info)
            ((eqv? message 'get-type)  get-type)
            ((eqv? message 'get-midi)  get-midi)
            (else (method-lookup super message))))

    (set! self dispatch)

    self))

; Note class
(define (note start-time tone octave length instrument)
  (let ((super (new-part music-element start-time length))
        (self 'nil))

    (let ((tone       tone)
          (octave     octave)
          (length     length)
          (instrument instrument))

      (define (get-tone)       tone)
      (define (get-octave)     octave)
      (define (get-instrument) instrument)
      (define (get-type)       'note)
      (define (get-info) (list (send 'get-start-time super)
                               (get-tone)
                               (get-octave)
                               (send 'get-duration super)
                               (get-instrument)
                               (get-type)))
      
      (define (get-midi)
        (cons 'note-abs-time-with-duration (list
              (get-note-duration start-time 100)  ; Absolute start time
              (instrument-to-channel instrument)  ; Channel no.
              (note-to-midi-number tone octave)   ; Note no.
              80                                  ; Velocity (constant)
              (get-note-duration length 100))))    ; Duration
      
      
      (define (set-self! object-part)
        (set! self object-part)
        (send 'set-self! super object-part))

      (define (dispatch message)
        (cond ((eqv? message 'get-tone)         get-tone)
              ((eqv? message 'get-octave)       get-octave)
              ((eqv? message 'get-instrument)   get-instrument)
              ((eqv? message 'get-midi)         get-midi)
              ((eqv? message 'get-type)         get-type)
              ((eqv? message 'get-info)         get-info)
              ((eqv? message 'set-self!)        set-self!)
              (else (method-lookup super message))))
      (set! self dispatch)
    )
    self)) ; Return self handler


; Sequence class
(define (sequential-music-element instrument start-time length . music-elements)
  (let ((super (new-part music-element start-time length (car music-elements)))
        (self 'nil))

    (define (get-type) 'sequence)
    (define (get-info) (map (lambda (element) (send 'get-info element)) (get-elements)))
    (define (get-elements) (car music-elements))

    (define (set-self! object-part)
        (set! self object-part)
        (send 'set-self! super object-part))

    (define (dispatch message)
      (cond ((eqv? message 'get-info)     get-info)
            ((eqv? message 'get-elements) get-elements)
            ((eqv? message 'get-type)     get-type)
            (else (method-lookup super message))))
    (set! self dispatch)

    self))

; Parallel composition class
(define (parallel-music-element instrument start-time length . music-elements)
  (let ((super (new-part music-element start-time length (car music-elements)))
        (self 'nil))

    (define (get-type) 'parallel)
    (define (get-info) (map (lambda (element) (send 'get-info element)) (get-elements)))
    (define (get-elements) (car music-elements))

    (define (set-self! object-part)
        (set! self object-part)
        (send 'set-self! super object-part))

    (define (dispatch message)
      (cond ((eqv? message 'get-type)     get-type)
            ((eqv? message 'get-info)     get-info)
            ((eqv? message 'get-elements) get-elements)
            (else (method-lookup super message))))

    (set! self dispatch)
    self))


; Note construction functions
; ---------------------------
; Curry functions allowing us to supply the notes
; with an instrument and start-time at a later point.
(define (C length octave)
  (lambda (instrument start-time)
    (new-instance note start-time 'C octave length instrument)))

(define (C# length octave)
  (lambda (instrument start-time)
    (new-instance note start-time 'C# octave length instrument)))

(define (D length octave)
  (lambda (instrument start-time)
    (new-instance note start-time 'D octave length instrument)))

(define (D# length octave)
  (lambda (instrument start-time)
    (new-instance note start-time 'D# octave length instrument)))

(define (E length octave)
  (lambda (instrument start-time)
    (new-instance note start-time 'E octave length instrument)))

(define (F length octave)
  (lambda (instrument start-time)
    (new-instance note start-time 'F octave length instrument)))

(define (F# length octave)
  (lambda (instrument start-time)
    (new-instance note start-time 'F# octave length instrument)))

(define (G length octave)
  (lambda (instrument start-time)
    (new-instance note start-time 'G octave length instrument)))

(define (G# length octave)
  (lambda (instrument start-time)
    (new-instance note start-time 'G# octave length instrument)))

(define (A length octave)
  (lambda (instrument start-time)
    (new-instance note start-time 'A octave length instrument)))

(define (A# length octave)
  (lambda (instrument start-time)
    (new-instance note start-time 'A# octave length instrument)))

(define (B length octave)
  (lambda (instrument start-time)
    (new-instance note start-time 'B octave length instrument)))

(define (Pause length)
  (lambda (instrument start-time)
    (new-instance pause start-time length)))


; Sequence construction functions
; ---------------------------
; Curry functions allowing us to supply the sequences
; with an instrument and start-time at a later point.
(define (sequence . music-elements)
  (lambda (instrument start-time)
    
    (define (reduce)
      (reduce-helper music-elements start-time '())) ; Start time increases for each subsequence element 

    (define (reduce-helper music-elements acc-time acc)
      (cond ((null? music-elements) (cons acc (- acc-time start-time))) ; Return both the accumulated music elements and the accumulated time of the sequence.
            (else (let ((element ((car music-elements) instrument acc-time))) ; Uncurry the sequence
                    (reduce-helper (cdr music-elements) (+ acc-time (send 'get-duration element)) (append acc (list element)))))))
    
    (let ((reduction (reduce))) ; reduction = (accumulated duration . music elements)
      (new-instance sequential-music-element instrument start-time (cdr reduction) (car reduction))
      )
    )
  )

; Parallel construction functions
; ---------------------------
; Curry functions allowing us to supply the parallel compositions
; with an instrument and start-time at a later point.
(define (parallel . music-elements)
  (lambda (instrument start-time)

    (define (reduce)
      (reduce-helper music-elements '() 0)) ; Start time is 0 because we are in parallel

    (define (reduce-helper music-elements acc max-length)
      (cond ((null? music-elements) (cons acc max-length)) ; Return the accumulated music elements as well as the longest duration
            (else (let ((element ((car music-elements) instrument start-time))) ; Uncurry the parallel composition
                    (let ((duration (send 'get-duration element)))
                      (reduce-helper (cdr music-elements)
                                     (append acc (list element))
                                     (cond ((> duration max-length) duration) ; Which sequence in the parallel composition has the longest duration?
                                           (else                    max-length))))))
            )
      )

    (let ((reduction (reduce))) ; reduction = (longest duration . music elements)
    (new-instance parallel-music-element instrument start-time (cdr reduction) (car reduction))
      )
    )
  )


; Instrument construction functions
; ---------------------------
; When this function is called, the curry
; functions are envoked fully, creating the music elements.
(define (instrument instrum music-element)
  (music-element instrum 0))


(define (song . music-elements)
  (let ((max-duration (apply max (map (lambda (x) (send 'get-duration x)) music-elements))))
    (new-instance music-element 0 max-duration music-elements)))


(define (flatten song)
  (cond ((null? song) '())
        ((eqv? (car song) 'note-abs-time-with-duration) (list song))
        ((pair? (car song))
         (append (flatten (car song))
                 (flatten (cdr song))))
        (else (cons (car song) (flatten (cdr song))))))


; Misc functions


; Chord construction function - simply constructs a parallel composition with a given length and predefined notes.
(define (chord tone length)
  (cond ((eqv? tone 'Cd) (parallel (C length 2)
                                   (E length 3)
                                   (G length 3)
                                   (C length 3)
                                   (E length 4)))
        
        ((eqv? tone 'Cm) (parallel (C length 2)
                                   (D# length 3)
                                   (G length 3)
                                   (C length 3)
                                   (E length 4)))

        ((eqv? tone 'Gd) (parallel (G length 2)
                                   (B length 3)
                                   (D length 3)
                                   (G length 3)
                                   (D length 4)
                                   (G length 5)))
        
        ((eqv? tone 'Dd) (parallel (D length 2)
                                   (A length 3)
                                   (D length 3)
                                   (F# length 4)))
        ))

(define (sea-shanty-intro)
  (sequence (A 1/8 4)   (Pause 1/8) (E 1/8 4) (D 1/8 4)  (C# 1/4 4) (Pause 1/8)
            (C# 1/8 4)  (D 1/8 4)   (E 1/8 4) (F# 1/8 4) (G# 1/8 4) (E 1/4 4)   (Pause 1/4)
            (F# 1/8 4)  (Pause 1/8) (E 1/8 4) (D 1/8 4)  (C# 1/8 4) (Pause 1/8) (C# 1/8 4) (Pause 1/8) (B 1/8 3) (Pause 1/8) (C# 1/8 4) (Pause 1/8) (D 1/4 4) (Pause 1/4)
            (A 1/8 4)   (Pause 1/8) (E 1/8 4) (D 1/8 4)  (C# 1/4 4) (Pause 1/8)
            (Pause 1/8) (C# 1/8 4)  (D 1/8 4) (E 1/8 4)  (F# 1/8 4) (D 1/4 4)   (Pause 1/4)
            (F# 1/8 4)  (E 1/8 4)   (D 1/8 4) (C# 1/8 4) (B 1/8 3)  (C# 1/8 4)  (D 1/8 4)  (F# 1/8 4)  (E 1/8 4) (D 1/8 4)   (C# 1/8 4) (B 1/8 3)   (A 1/4 3) (Pause 1/4)))

(define (sea-shanty-verse)
  (parallel (sequence (C# 1/8 4) (B 1/8 3)   (C# 1/8 4) (Pause 1/8) (C# 1/8 4) (Pause 1/8) (C# 1/8 4) (B 1/8 3)   (C# 1/8 4) (Pause 1/8) (B 1/8 3)  (Pause 1/8) (C# 1/4 4) (Pause 1/4)
                      (A 1/8 3)  (B 1/8 3)   (C# 1/8 4) (D 1/8 4)   (C# 1/8 4) (Pause 1/8) (B 1/8 3)  (Pause 1/8) (C# 1/2 4) (Pause 1/2)
                      (C# 1/8 4) (B 1/8 3)   (C# 1/8 4) (Pause 1/8) (C# 1/8 4) (Pause 1/8) (D 1/8 4)  (C# 1/8 4)  (B 1/8 3)  (Pause 1/8) (E 1/8 4)  (Pause 1/8) (C# 1/4 4) (Pause 1/4)
                      (A 1/8 3)  (B 1/8 3)   (C# 1/8 4) (D 1/8 4)   (C# 1/8 4) (Pause 1/8) (A 1/8 3)  (Pause 1/8) (C# 1/2 4) (Pause 1/2)
                      (E 1/8 4)  (D 1/8 4)   (E 1/8 4)  (Pause 1/8) (E 1/8 4)  (Pause 1/8) (E 1/8 4)  (D 1/8 4)   (E 1/8 4)  (Pause 1/8) (F# 1/8 4) (Pause 1/8) (E 1/4 4)  (Pause 1/4)
                      (G# 1/8 4) (Pause 1/8) (G# 1/8 4) (F# 1/8 4)  (E 1/8 4) (Pause 1/8) (D 1/8 4) (Pause 1/8) (E 1/2 4) (Pause 1/2)
                      (C# 1/8 4) (B 1/8 3)   (C# 1/8 4) (Pause 1/8) (C# 1/8 4) (Pause 1/8) (B 1/8 3) (C# 1/8 4) (D 1/8 4) (Pause 1/8) (B 1/8 3) (Pause 1/8) (C# 1/4 4) (Pause 1/4)
                      (C# 1/8 4) (B 1/8 3)   (C# 1/8 4) (D 1/8 4) (C# 1/8 4) (Pause 1/8) (A 1/8 3) (Pause 1/8) (A 1/4 2))

            (sequence (A 1/4 1) (Pause 1/4) (E 1/4 2) (Pause 1/4) (A 1/4 1) (Pause 1/4) (A 1/4 0) (E 1/4 1) (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/2 1) (Pause 1/2)
                      (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/4 1) (Pause 1/4) (A 1/4 0) (E 1/4 1) (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/2 1) (Pause 1/2)
                      (E 1/4 1) (Pause 1/4) (B 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (E 1/4 0) (B 1/4 1) (E 1/4 1) (Pause 1/4) (B 1/4 1) (Pause 1/4) (E 1/2 1) (Pause 1/2)
                      (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/4 1) (Pause 1/4) (A 1/4 0) (E 1/4 1) (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/2 1) (Pause 1/2))))
  


; Demo
(define the-song (song (instrument 'guitar (sequence (sea-shanty-intro)
                                                     (sea-shanty-verse)))
                       (instrument 'piano (sequence (sea-shanty-intro)
                                                     (sea-shanty-verse)))))

(send 'get-duration-absolute the-song)
(flatten(send 'get-midi the-song))


