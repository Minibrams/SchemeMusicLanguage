#lang racket

;; -------------------------------------------
;; General Utility Functions
;; -------------------------------------------

; Used for replacing key-value pairs in association lists
(define (replace symbol value ass-list)
  (replace-helper symbol value ass-list '()))

(define (replace-helper symbol value ass-list acc)
  (cond ((null? ass-list) acc)
        (else (let ((first-pair (car ass-list)))
                (cond ((eqv? (car first-pair) symbol) (replace-helper symbol value (cdr ass-list) (cons (cons symbol value) acc)))
                      (else (replace-helper symbol value (cdr ass-list) (cons first-pair acc))))))))

; So we can see whether we have an atom or something else (list, pair, etc.)
(define (atom? x) (not (or (pair? x) (null? x))))


;; -------------------------------------------
;; Music Utility Functions
;; -------------------------------------------

; Reinstrument a list of association lists
(define (reinstrument flat-midi instrument)
  (map (lambda (ass-list) (replace 'channel-no (instrument-to-channel instrument) ass-list)) flat-midi))

; Scale a list of association lists
(define (scale-start-time flat-midi factor)
  (map (lambda (ass-list) (let ((start-time (cdr(assoc 'abs-start-time ass-list))))
                            (replace 'abs-start-time (exact-floor(/ start-time factor)) ass-list))) flat-midi))

(define (scale-duration flat-midi factor)
  (map (lambda (ass-list) (let ((duration (cdr(assoc 'duration ass-list))))
                            (replace 'duration (exact-floor(/ duration factor)) ass-list))) flat-midi))

(define (scale flat-midi factor)
  (scale-start-time (scale-duration flat-midi factor) factor))

; Transpose a list of association lists
(define (transpose flat-midi amount)
  (map (lambda (ass-list) (let ((note-no (cdr(assoc 'note-no ass-list))))
                            (replace 'note-no (+ amount note-no) ass-list))) flat-midi))

; Convert a list of assofication lists to note-abs-time-with-duration forms
(define (linearize flat-midi)
  (map (lambda (ass-list) (list 'note-abs-time-with-duration
                               (cdr(assoc 'abs-start-time ass-list))
                               (cdr(assoc 'channel-no ass-list))
                               (cdr(assoc 'note-no ass-list))
                               (cdr(assoc 'velocity ass-list))
                               (cdr(assoc 'duration ass-list)))) flat-midi))

; Convert fraction-durations to MIDI time units
(define (get-note-duration length bpm)
  (let ((seconds-per-measure (* 60 (/ 4 bpm)))) ; Assuming 4/4 measures
    (exact-floor (*(* length seconds-per-measure) 960)) ; Seconds to MIDI time units
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
(define (music-element bpm start-time duration . music-elements)
  (let ((super '())
        (self 'nil))

    (let ((start-time       start-time)
          (duration         duration))
      
      (define (set-self! object-part)
        (set! self object-part))
      
      (define (get-type)       'music-element)
      (define (get-start-time) start-time)
      (define (get-duration)   duration)
      (define (get-duration-absolute) (get-note-duration duration bpm))
      (define (get-elements) (cond ((null? music-elements) '() )
                                   (else (car music-elements))))
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
              ((eqv? message 'get-elements) get-elements)
              (else '())))

      (set! self dispatch)
      )
    
    self))


; Pause class
(define (pause bpm start-time length)
  (let ((super (new-part music-element bpm start-time length))
        (self 'nil))

    (define (get-type) 'pause)
    (define (get-info) (list (send 'get-start-time super)
                             (send 'get-duration super)
                             (get-type)))
    
    (define (set-self! object-part)
        (set! self object-part)
        (send 'set-self! super object-part))

    (define (get-midi)
        (list
              (cons 'abs-start-time (get-note-duration start-time bpm)) ; Absolute start time
              (cons 'channel-no 1)  ; Channel no.
              (cons 'note-no 0)  ; Note no.
              (cons 'velocity 0)  ; Velocity (constant)
              (cons 'duration (get-note-duration length bpm)))) ; Duration

    (define (dispatch message)
      (cond ((eqv? message 'get-info)  get-info)
            ((eqv? message 'get-type)  get-type)
            ((eqv? message 'get-midi)  get-midi)
            (else (method-lookup super message))))

    (set! self dispatch)

    self))

; Note class
(define (note bpm start-time tone octave length instrument)
  (let ((super (new-part music-element bpm start-time length))
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
        (list
              (cons 'abs-start-time (get-note-duration start-time bpm))  ; Absolute start time
              (cons 'channel-no (instrument-to-channel instrument))  ; Channel no.
              (cons 'note-no (note-to-midi-number tone octave))   ; Note no.
              (cons 'velocity 80)                                  ; Velocity (constant)
              (cons 'duration (get-note-duration length bpm))))    ; Duration
      
      
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
(define (sequential-music-element bpm instrument start-time length . music-elements)
  (let ((super (new-part music-element bpm start-time length (car music-elements)))
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
(define (parallel-music-element bpm instrument start-time length . music-elements)
  (let ((super (new-part music-element bpm start-time length (car music-elements)))
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
  (lambda ( bpm instrument start-time)
    (new-instance note bpm start-time 'C octave length instrument)))

(define (C# length octave)
  (lambda ( bpm instrument start-time)
    (new-instance note bpm  start-time 'C# octave length instrument)))

(define (D length octave)
  (lambda ( bpm instrument start-time)
    (new-instance note bpm  start-time 'D octave length instrument)))

(define (D# length octave)
  (lambda ( bpm instrument start-time)
    (new-instance note bpm  start-time 'D# octave length instrument)))

(define (E length octave)
  (lambda ( bpm instrument start-time)
    (new-instance note bpm  start-time 'E octave length instrument)))

(define (F length octave)
  (lambda ( bpm instrument start-time)
    (new-instance note bpm  start-time 'F octave length instrument)))

(define (F# length octave)
  (lambda ( bpm instrument start-time)
    (new-instance note bpm  start-time 'F# octave length instrument)))

(define (G length octave)
  (lambda ( bpm instrument start-time)
    (new-instance note bpm  start-time 'G octave length instrument)))

(define (G# length octave)
  (lambda ( bpm instrument start-time)
    (new-instance note bpm  start-time 'G# octave length instrument)))

(define (A length octave)
  (lambda ( bpm instrument start-time)
    (new-instance note bpm  start-time 'A octave length instrument)))

(define (A# length octave)
  (lambda ( bpm instrument start-time)
    (new-instance note bpm  start-time 'A# octave length instrument)))

(define (B length octave)
  (lambda ( bpm instrument start-time)
    (new-instance note bpm  start-time 'B octave length instrument)))

(define (Pause length)
  (lambda ( bpm instrument start-time)
    (new-instance pause bpm  start-time length)))


; Sequence construction functions
; ---------------------------
; Curry functions allowing us to supply the sequences
; with an instrument and start-time at a later point.
(define (sequence . music-elements)
  (lambda ( bpm instrument start-time)
    
    (define (reduce)
      (reduce-helper music-elements start-time '())) ; Start time increases for each subsequence element 

    (define (reduce-helper music-elements acc-time acc)
      (cond ((null? music-elements) (cons acc (- acc-time start-time))) ; Return both the accumulated music elements and the accumulated time of the sequence.
            (else (let ((element ((car music-elements) bpm  instrument acc-time))) ; Uncurry the sequence
                    (reduce-helper (cdr music-elements) (+ acc-time (send 'get-duration element)) (append acc (list element)))))))
    
    (let ((reduction (reduce))) ; reduction = (accumulated duration . music elements)
      (new-instance sequential-music-element bpm  instrument start-time (cdr reduction) (car reduction))
      )
    )
  )

; Parallel construction functions
; ---------------------------
; Curry functions allowing us to supply the parallel compositions
; with an instrument and start-time at a later point.
(define (parallel . music-elements)
  (lambda ( bpm instrument start-time)

    (define (reduce)
      (reduce-helper music-elements '() 0)) ; Start time is 0 because we are in parallel

    (define (reduce-helper music-elements acc max-length)
      (cond ((null? music-elements) (cons acc max-length)) ; Return the accumulated music elements as well as the longest duration
            (else (let ((element ((car music-elements) bpm  instrument start-time))) ; Uncurry the parallel composition
                    (let ((duration (send 'get-duration element)))
                      (reduce-helper (cdr music-elements)
                                     (append acc (list element))
                                     (cond ((> duration max-length) duration) ; Which sequence in the parallel composition has the longest duration?
                                           (else                    max-length))))))
            )
      )

    (let ((reduction (reduce))) ; reduction = (longest duration . music elements)
    (new-instance parallel-music-element bpm  instrument start-time (cdr reduction) (car reduction))
      )
    )
  )


; Instrument construction functions
; ---------------------------
; When this function is called, the curry
; functions are envoked fully, creating the music elements.
(define (instrument instrum music-element)
  (lambda (bpm)
    (music-element bpm instrum 0)))

(define (song bpm  . music-elements)
  (let ((max-duration (apply max (map (lambda (x) (send 'get-duration x)) music-elements))))
    (new-instance music-element bpm  0 max-duration music-elements)))

(define (new-song bpm . music-elements)
  (let ((instruments (map (lambda (x) (x bpm)) music-elements)))
    (let ((max-duration (apply max (map (lambda (x) (send 'get-duration x)) instruments))))
    (new-instance music-element bpm  0 max-duration instruments))))


(define (flatten tree)
    (cond
       ((null? tree) '())
       ((atom? (car(car(car tree)))) (cons (car tree) (flatten (cdr tree)))) ; Extract association lists from the tree
       (else
          (append (flatten (car tree))
                  (flatten (cdr tree)))))
)


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
                      (G# 1/8 4) (Pause 1/8) (G# 1/8 4) (F# 1/8 4)  (E 1/8 4)  (Pause 1/8) (D 1/8 4)  (Pause 1/8) (E 1/2 4)  (Pause 1/2)
                      (C# 1/8 4) (B 1/8 3)   (C# 1/8 4) (Pause 1/8) (C# 1/8 4) (Pause 1/8) (B 1/8 3)  (C# 1/8 4)  (D 1/8 4)  (Pause 1/8) (B 1/8 3)  (Pause 1/8) (C# 1/4 4) (Pause 1/4)
                      (C# 1/8 4) (B 1/8 3)   (C# 1/8 4) (D 1/8 4)   (C# 1/8 4) (Pause 1/8) (A 1/8 3)  (Pause 1/8) (A 1/4 2))

            (sequence (A 1/4 1) (Pause 1/4) (E 1/4 2) (Pause 1/4) (A 1/4 1) (Pause 1/4) (A 1/4 0) (E 1/4 1) (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/2 1) (Pause 1/2)
                      (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/4 1) (Pause 1/4) (A 1/4 0) (E 1/4 1) (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/2 1) (Pause 1/2)
                      (E 1/4 1) (Pause 1/4) (B 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (E 1/4 0) (B 1/4 1) (E 1/4 1) (Pause 1/4) (B 1/4 1) (Pause 1/4) (E 1/2 1) (Pause 1/2)
                      (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/4 1) (Pause 1/4) (A 1/4 0) (E 1/4 1) (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/2 1) (Pause 1/2))))
  


; Demo
(define the-song (new-song 120 (instrument 'guitar (sequence (sea-shanty-intro)
                                                     (sea-shanty-verse)))
                               (instrument 'piano (sequence  (sea-shanty-intro)
                                                     (sea-shanty-verse)))))

(send 'get-duration-absolute the-song)
;(send 'get-info (car(cdr(send 'get-elements the-song))))
(send 'get-type (car (send 'get-elements the-song)))
(define flat-midi (flatten(send 'get-midi the-song)))




(linearize(transpose(scale (reinstrument flat-midi 'guitar) 1.5) 5))