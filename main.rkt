; Anders Højlund Brams
; 20143514
; abrams14@student.aau.dk

#lang racket
(require "music-base.rkt")


;; -------------------------------------------
;; General Utility Functions
;; -------------------------------------------

(define (>= x y)
  (or (> x y) (= x y)))

; Used for replacing key-value pairs in association lists.
;
; Parameters:
; 1. symbol: A symbol representing the key to change the value of.
; 2. value: The value that will replace the old value of <symbol>.
; 3. ass-list: An association list.
(define (replace symbol value ass-list)
  (replace-helper symbol value ass-list '()))

(define (replace-helper symbol value ass-list acc)
  (cond ((null? ass-list) acc)
        (else (let ((first-pair (car ass-list)))
                (cond ((eqv? (car first-pair) symbol)
                       ; Is this the symbol we should revalue?
                       ; Continue traversing the list, but put the revalued association into the accumulator.
                       (replace-helper symbol value (cdr ass-list) (cons (cons symbol value) acc)))
                      (else
                       ; Continue as normal
                       (replace-helper symbol value (cdr ass-list) (cons first-pair acc))))))))


; So we can see whether we have an atom or something else (list, pair, etc.).
(define (atom? x) (not (or (pair? x) (null? x))))

; When music elements unroll themselves into association lists after 'get-midi'
; is called, they are still situated in a tree structure of nested lists.
; Use this function to flatten that list so it can be converted to abs-time-notes
; immediately.
;
; Parameters:
; 1. ass-list-tree: A list of nested lists that we expect to
;                   have association lists at the bottom.
(define (flatten ass-list-tree)
    (cond
       ((null? ass-list-tree) '())
       ((atom? (car(car(car ass-list-tree)))) ; If we reach an atom three elements down, we have an association list!
        (cons (car ass-list-tree) (flatten (cdr ass-list-tree)))) ; Cons it to the rest of the (hopefully flat) list.
       (else
          (append (flatten (car ass-list-tree)) ; Dive further down into the tree, combine whatever comes 
                  (flatten (cdr ass-list-tree)))))) ; up with the rest of the list.


;; -------------------------------------------
;; Music Utility Functions
;; -------------------------------------------

; Reinstrument a list of association lists.
;
; Parameters:
; 1. flat-midi:  List of association lists created from unrolling music elements
;               with 'get-midi'.
; 2. instrument: Symbol representing an instrument.
(define (reinstrument flat-midi instrument)
  (map (lambda (ass-list) (replace 'channel-no (instrument-to-channel instrument) ass-list)) flat-midi))

; Scale a list of association lists.
;
; Parameters:
; 1. flat-midi:  List of association lists created from unrolling music elements
;               with 'get-midi'.
; 2. factor: Number all durations will be multiplied by. 
(define (scale-start-time flat-midi factor)
  (map (lambda (ass-list) (let ((start-time (cdr(assoc 'abs-start-time ass-list))))
                            (replace 'abs-start-time (exact-floor(/ start-time factor)) ass-list))) flat-midi))

(define (scale-duration flat-midi factor)
  (map (lambda (ass-list) (let ((duration (cdr(assoc 'duration ass-list))))
                            (replace 'duration (exact-floor(/ duration factor)) ass-list))) flat-midi))

(define (scale flat-midi factor)
  (scale-start-time (scale-duration flat-midi factor) factor))

; Transpose a list of association lists.
;
; Parameters:
; 1. flat-midi: List of association lists created from unrolling music elements
;               with 'get-midi'.
; 2. amount: Integer representing the midi-number range to transpose by.
(define (transpose flat-midi amount)
  (map (lambda (ass-list) (let ((note-no (cdr(assoc 'note-no ass-list))))
                            (replace 'note-no (+ amount note-no) ass-list))) flat-midi))

; Convert a list of assofication lists to note-abs-time-with-duration forms.
;
; Parameters:
; 1. flat-midi: List of association lists created from unrolling music elements
;               with 'get-midi'.
(define (linearize flat-midi)
  (map (lambda (ass-list) (list 'note-abs-time-with-duration
                               (cdr(assoc 'abs-start-time ass-list))
                               (cdr(assoc 'channel-no ass-list))
                               (cdr(assoc 'note-no ass-list))
                               (cdr(assoc 'velocity ass-list))
                               (cdr(assoc 'duration ass-list)))) flat-midi))


; Convert time signatures to MIDI time units.
;
; Parameters:
; 1. length: a time signature (rational number)
; 2. bpm: BPM (Beats Per Minute) (integer) 
(define (get-note-duration length bpm)
  (let ((seconds-per-measure (* 60 (/ 4 bpm)))) ; Assuming 4/4 measures
    
    (cond ((not (integer?  bpm)) (error "BPM must be an integer, but was: " bpm))
          
          (else (exact-floor (*(* length seconds-per-measure) 960)))))) ; Seconds to MIDI time units

; Map a known instrument to a MIDI channel.
;
; Parameters:
; 1. instrument: A symbol representing an instrument.
(define (instrument-to-channel instrument)
  (cond ((eqv? instrument 'piano)      1)
        ((eqv? instrument 'organ)      2)
        ((eqv? instrument 'guitar)     3)
        ((eqv? instrument 'violin)     4)
        ((eqv? instrument 'flute)      5)
        ((eqv? instrument 'trumpet)    6)
        ((eqv? instrument 'helicopter) 7)
        ((eqv? instrument 'telephone)  8)
        (else (error "Instrument not recognized: " instrument))))

; Map a musical note to a MIDI number.
;
; Parameters:
; 1. note: A symbol representing a specific note.
; 2. octave: The note's octave. 
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
                    ((eqv? note 'B)  35)
                    (else (error "Note not recognized: " note)))))
    (let ((midi-number (+ base (* 12 octave))))
      (cond ((> 128 midi-number) midi-number)
            (else (error "Midi number can be at most 127, but was: " midi-number))))))


;; -------------------------------------------
;; OOP Utility Functions
;; -------------------------------------------
; Function for getting object methods.
; Typically used with 'self' as the object (see music-elements).
;
; Parameters:
; 1. object: Object from which a method is wanted.
; 2. selector: 
(define (method-lookup object selector)
  (cond ((procedure? object) (object selector))
        (else (error "Inappropriate object in method-lookup: " object))))

; Function for calling object methods with parameters.
;
; Parameters:
; 1. message: Typically a symbol representing a method in an object.
; 2. obj: The object to which the message is sent.
; 3. par: Arbitrary number of parameters that will be passed to the
;         method call.
(define (send message obj . par)
  (let ((method (method-lookup obj message)))
    (cond ((procedure? method) (apply method par))
          ((null? method) (error "Message not understood: " message))
          (else (error "Inappropriate result of method look-up: " method)))))

; Function for creating new instances of classes.
;
; Parameters:
; 1. class: Class from which a new instance will be created.
; 2. parms: Arbitrary number of parameters for instantiation.
(define (new-instance class . parms)
  (let ((instance (apply class parms)))
    (virtual-operations instance)
    instance))

; Virtual operations - propagate set-self through inheritance tree.
;
; Parameters:
; 1. object: Object for which 'self' will be set to itself so its
;            methods can be called.
(define (virtual-operations object)
  (send 'set-self! object object))

; Function for creating new parts of classes.
;
; Parameters:
; 1. class: Class from which a new part will be created.
; 2. parms: Arbitrary number of parameters for instantiation.
(define (new-part class . parms)
  (apply class parms))

;; -------------------------------------------
;; Classes
;; -------------------------------------------

; Music element class - this is the base for Notes, Sequences, and Parallel.
;
; Parameters:
; 1. bpm: Beats Per Minute.
; 2. start-time: The starting time (rational) for this music element.
; 3. duration: The duration (rational) for this music element.
; 4. music-elements: Arbitrary number of child music elements.
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

      ; Return a list of all child music elements converted to their midi versions.
      (define (get-midi)
        (flatten(map (lambda (element) (send 'get-midi element)) (car music-elements))))
    
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


; Pause class.
;
; Parameters:
; 1. bpm: Beats Per Minute
; 2. start-time: Starting time (rational) for this pause.
; 3. length: Duration (rational) for this pause. 
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

    ; Convert this object to an association list
    (define (get-midi)
        (list
              (cons 'abs-start-time (get-note-duration start-time bpm)) ; Absolute start time
              (cons 'channel-no 1)  ; Channel no.
              (cons 'note-no 0)     ; Note no.
              (cons 'velocity 0)    ; Velocity (constant)
              (cons 'duration (get-note-duration length bpm)))) ; Duration

    (define (dispatch message)
      (cond ((eqv? message 'get-info)  get-info)
            ((eqv? message 'get-type)  get-type)
            ((eqv? message 'get-midi)  get-midi)
            (else (method-lookup super message))))

    (set! self dispatch)

    self))

; Note class.
;
; Parameters:
; 1. bpm: Beats Per Minute
; 2. tone: The tone (symbol) of this note (e.g. C#, F, A).
; 3. octave: The octave of this note.
; 4. length: The duration of this note.
; 5. instrument: The instrument playing this note. 
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

      ; Convert this object to an association list
      (define (get-midi)
        (list
              (cons 'abs-start-time (get-note-duration start-time bpm))  ; Absolute start time
              (cons 'channel-no (instrument-to-channel instrument))      ; Channel no.
              (cons 'note-no (note-to-midi-number tone octave))          ; Note no.
              (cons 'velocity 80)                                        ; Velocity (constant)
              (cons 'duration (get-note-duration length bpm))))          ; Duration
      
      
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


; Sequence class.
;
; Parameters:
; 1. bpm: Beats Per Minte
; 2. instrument: The instrument playing this sequence.
; 3. start-time: The starting time (rational) for this sequence.
; 4. length: The total length of this sequence.
; 5. music-elements: Arbitrary number of child music elements.
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
;
; Parameters:
; 1. bpm: Beats Per Minte
; 2. instrument: The instrument playing this parallel comp.
; 3. start-time: The starting time (rational) for this parallel comp.
; 4. length: The longest length in this parallel comp.
; 5. music-elements: Arbitrary number of child music elements.
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

; Predicate for determining whether or not a note length
; and octave result in valid notes. 
(define (valid-note? length octave)
  (and (rational? length) (integer? octave) (> length 0)))

; Curry functions allowing us to supply the notes
; with an instrument and start-time at a later point.

(define (C length octave)
  (cond ((valid-note? length octave)
        (lambda ( bpm instrument start-time)
          (new-instance note bpm start-time 'C octave length instrument)))
        (else (error "Invalid length or octave: " length octave))))
  

(define (C# length octave)
  (cond ((valid-note? length octave)
        (lambda ( bpm instrument start-time)
          (new-instance note bpm start-time 'C# octave length instrument)))
        (else (error "Invalid length or octave: " length octave))))

(define (D length octave)
  (cond ((valid-note? length octave)
        (lambda ( bpm instrument start-time)
          (new-instance note bpm start-time 'D octave length instrument)))
        (else (error "Invalid length or octave: " length octave))))

(define (D# length octave)
  (cond ((valid-note? length octave)
        (lambda ( bpm instrument start-time)
          (new-instance note bpm start-time 'D# octave length instrument)))
        (else (error "Invalid length or octave: " length octave))))

(define (E length octave)
  (cond ((valid-note? length octave)
        (lambda ( bpm instrument start-time)
          (new-instance note bpm start-time 'E octave length instrument)))
        (else (error "Invalid length or octave: " length octave))))

(define (F length octave)
  (cond ((valid-note? length octave)
        (lambda ( bpm instrument start-time)
          (new-instance note bpm start-time 'F octave length instrument)))
        (else (error "Invalid length or octave: " length octave))))

(define (F# length octave)
  (cond ((valid-note? length octave)
        (lambda ( bpm instrument start-time)
          (new-instance note bpm start-time 'F# octave length instrument)))
        (else (error "Invalid length or octave: " length octave))))

(define (G length octave)
  (cond ((valid-note? length octave)
        (lambda ( bpm instrument start-time)
          (new-instance note bpm start-time 'G octave length instrument)))
        (else (error "Invalid length or octave: " length octave))))

(define (G# length octave)
  (cond ((valid-note? length octave)
        (lambda ( bpm instrument start-time)
          (new-instance note bpm start-time 'G# octave length instrument)))
        (else (error "Invalid length or octave: " length octave))))

(define (A length octave)
  (cond ((valid-note? length octave)
        (lambda ( bpm instrument start-time)
          (new-instance note bpm start-time 'A octave length instrument)))
        (else (error "Invalid length or octave: " length octave))))

(define (A# length octave)
  (cond ((valid-note? length octave)
        (lambda ( bpm instrument start-time)
          (new-instance note bpm start-time 'A# octave length instrument)))
        (else (error "Invalid length or octave: " length octave))))

(define (B length octave)
  (cond ((valid-note? length octave)
        (lambda ( bpm instrument start-time)
          (new-instance note bpm start-time 'B octave length instrument)))
        (else (error "Invalid length or octave: " length octave))))

(define (Pause length)
  (cond ((and (rational? length) (> length 0))
        (lambda ( bpm instrument start-time)
          (new-instance pause bpm start-time length)))
        (else (error "Invalid length: " length))))


; Sequence construction functions
; ---------------------------
; Curry functions allowing us to supply the sequences
; with an instrument and start-time at a later point.
(define (sequence . music-elements)

  ; Parameters:
  ; 1. bpm: Beats Per Minute.
  ; 2. instrument: Instrument playing this sequence.
  ; 3. start-time: Starting time of this sequence.
  (lambda ( bpm instrument start-time)

    ; Propagates the starting time of this sequence through all its
    ; child music elements. When the propagtion finishes, the total
    ; sum of child durations are returned along with the child elements.
    (define (propagate)
      (propagate-helper music-elements start-time '())) ; Start time increases for each subsequence element 

    (define (propagate-helper music-elements acc-time acc)
      (cond ((null? music-elements) (cons acc (- acc-time start-time))) ; Return both the accumulated music elements and the accumulated time of the sequence.
            (else (let ((element ((car music-elements) bpm  instrument acc-time))) ; Uncurry the sequence
                    (propagate-helper (cdr music-elements) (+ acc-time (send 'get-duration element)) (append acc (list element)))))))
    
    (let ((propagation (propagate))) ; propagation = (music elements . accumulated duration)
      (new-instance sequential-music-element bpm  instrument start-time (cdr propagation) (car propagation)))))

; Parallel construction functions
; ---------------------------
; Curry functions allowing us to supply the parallel compositions
; with an instrument and start-time at a later point.
(define (parallel . music-elements)

  ; Parameters:
  ; 1. bpm: Beats Per Minute
  ; 2. instrument: The instrument playing this parallel comp.
  ; 3. start-time: The starting time of this parallel comp. 
  (lambda ( bpm instrument start-time)

    ; Propagates the starting time of this parallel sequence through
    ; all its child elements. When the propgation finishes, the longest
    ; duration out of any of the child elements is returned along with
    ; the child elements.
    (define (propagate)
      (propagate-helper music-elements '() 0)) ; Start time is 0 because we are in parallel

    (define (propagate-helper music-elements acc max-length)
      (cond ((null? music-elements) (cons acc max-length)) ; Return the accumulated music elements as well as the longest duration
            (else (let ((element ((car music-elements) bpm  instrument start-time))) ; Uncurry the parallel composition
                    (let ((duration (send 'get-duration element)))
                      (propagate-helper (cdr music-elements)
                                     (append acc (list element))
                                     (cond ((> duration max-length) duration) ; Which sequence in the parallel composition has the longest duration?
                                           (else                    max-length))))))))

    (let ((propagation (propagate))) ; propagation = (music elements . longest duration)
    (new-instance parallel-music-element bpm  instrument start-time (cdr propagation) (car propagation)))))


; Instrument construction functions
; ---------------------------
; When this function is called, the curry
; functions are envoked fully, creating the music elements.
;
; Parameters:
; 1. instrum: Symbol representing an instrument.
; 2. music-element: An incomplete music element. 
(define (instrument instrum music-element)
  (lambda (bpm)
    (music-element bpm instrum 0)))

; Used for creating a song which is actualized as complete
; music elements. Takes BPM and a series of instrumentalized
; incomplete music elements as input.
;
; Parameters:
; 1. bpm: Beats Per Minute
; 2. music-elements: Incomplete (but instrumentalized) music elements. 
(define (song bpm . music-elements)
  (let ((instruments (map (lambda (x) (x bpm)) music-elements)))
    (let ((max-duration (apply max (map (lambda (x) (send 'get-duration x)) instruments))))
    (new-instance music-element bpm  0 max-duration instruments))))


; Predicate functions
; ---------------------------
(define (music-element? music-element)
  (let ((type (send 'get-type music-element)))
    (or (eqv? type 'music-element)
        (eqv? type 'parallel)
        (eqv? type 'sequence)
        (eqv? type 'note)
        (eqv? type 'pause))))

(define (parallel? music-element)
  (eqv? (send 'get-type music-element) 'parallel))

(define (sequence? music-element)
  (eqv? (send 'get-type music-element) 'sequence))

(define (note? music-element)
  (eqv? (send 'get-type music-element) 'note))

(define (pause? music-element)
  (eqv? (send 'get-type music-element) 'pause))




; Polyphony and monophony
; ---------------------------

; Extract start and end time pairs from an association list
; Parameters:
; 1. list-ass-lists: List of association lists
; Output:
; 1. List of time pairs e.g. '(('start . 0) ('end 480) ('start 240) ('end 1200))
(define (extract-start-end-times list-ass-lists)
  (extract-start-end-times-helper list-ass-lists '()))

(define (extract-start-end-times-helper list-ass-lists acc)
  (cond ((null? list-ass-lists) acc)
        (else
         
         (let* ((ass-list (car list-ass-lists))
         (start (cdr (assoc 'abs-start-time ass-list)))
         (end (+ start (cdr(assoc 'duration ass-list))))
         (pause? (= 0 (cdr (assoc 'velocity ass-list)))))

           (cond (pause?  (extract-start-end-times-helper (cdr list-ass-lists) acc))             ; Pauses don't count
                 (else    (extract-start-end-times-helper (cdr list-ass-lists) (append acc (list ; ... but notes do
                                                                                            (cons 'start start)
                                                                                            (cons 'end end))))))))))
; Comparison function used when sorting starting and ending time pairs
(define (less-than-abs-times x y)
  (let ((x-time (cdr x))
        (y-time (cdr y)))
    (< x-time y-time)))

; Calculates the degree of polyphony over a list of association lists.
; Parameters:
; 1. list-of-association-lists: List of association lists (the result of 'get-midi from a music element).
; Output:
; 1. An integer represent the maximum number of notes playing simultaneously. Does not include pauses. 
(define (degree-of-polyphony list-of-association-lists)
  (let ((start-end-list (extract-start-end-times list-of-association-lists)))
    (degree-of-polyphony-helper (sort start-end-list less-than-abs-times) 0 0)))

; Increment counter when we see a start note, decrement when we see an end note. Save the largest counter. 
(define (degree-of-polyphony-helper sorted-time-list counter max-counter)
  (cond ((null? sorted-time-list) max-counter)
        (else
         (let* ((time-type (car (car sorted-time-list)))
                (counter (cond ((eqv? 'start time-type) (+ counter 1))
                               (else                    (- counter 1)))))
           (degree-of-polyphony-helper (cdr sorted-time-list) counter (cond ((> counter max-counter) counter)
                                                                            (else max-counter)))))))

; Predicate function for whether or not more than one notes are playing at the same time. 
(define (monophonic? list-of-association-lists)
  (>= 1 (degree-of-polyphony list-of-association-lists)))





; Misc functions
; ---------------------------
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
                                   (F# length 4)))))










; -------------------------
; --------- DEMO ----------
; -------------------------

; Define some sequences and parallel compositions that we can throw together in a song later.
; Sequences and parallel compositions can take an arbitrary number of (incomplete) music elements 
; as input. That includes sequences and parallel compositions.
; However, the song will be boring without notes and pauses. Notes can be created by calling
; the function with a given note as its name (C through B#) and passing its duration and octave
; as input. Pauses take just a duration as input.

(define (sea-shanty-cello)
  (sequence (E 16/4 1) (A 16/4 1)))

(define (sea-shanty-bass)
         (sequence    (A 1/4 1) (Pause 1/4) (E 1/4 2) (Pause 1/4) (A 1/4 1) (Pause 1/4) (A 1/4 0) (E 1/4 1) (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/2 1) (Pause 1/2)
                      (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/4 1) (Pause 1/4) (A 1/4 0) (E 1/4 1) (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/2 1) (Pause 1/2)
                      (E 1/4 1) (Pause 1/4) (B 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (E 1/4 0) (B 1/4 1) (E 1/4 1) (Pause 1/4) (B 1/4 1) (Pause 1/4) (E 1/2 1) (Pause 1/2)
                      (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/4 1) (Pause 1/4) (A 1/4 0) (E 1/4 1) (A 1/4 1) (Pause 1/4) (E 1/4 1) (Pause 1/4) (A 1/2 1) (Pause 1/2)))

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
                      (C# 1/8 4) (B 1/8 3)   (C# 1/8 4) (D 1/8 4)   (C# 1/8 4) (Pause 1/8) (A 1/8 3)  (Pause 1/8) (A 1/4 2))))
  

; Define the song by:
; 1. Instrumentalizing the sequences and parallel compositions we defined above.
;    Instrumentalizing also provides the first music elements with a starting time 
;    (0) from which starting times will propagate through the tree of music elements.
; 2. Providing the song with a BPM (Beats Per Minute). The higher the BPM, the
;    faster the song.

(define the-song (song 120     (instrument 'guitar (sequence (sea-shanty-intro)
                                                             (parallel (sea-shanty-bass)
                                                                       (sea-shanty-verse))
                                                             (parallel (sea-shanty-bass) ; Canon re-entry
                                                                       (sea-shanty-verse)
                                                                       (sea-shanty-intro))))
                           
                               (instrument 'piano (sequence  (sea-shanty-intro)
                                                             (parallel (sea-shanty-bass)
                                                                       (sea-shanty-verse))
                                                             (parallel (sea-shanty-bass) ; Canon re-entry
                                                                       (sea-shanty-verse)
                                                                       (sea-shanty-intro))))

                               (instrument 'violin (sequence (Pause 128/4) ; Late entry
                                                             (sea-shanty-cello)))))


; By defining the song, we now have a tree structure of complete music elements.
; We can access the parts of all the music elements by sending messages to them.

; Get the type (should just be a music element):
(writeln "The type of the song is:")
(send 'get-type the-song)
(writeln "---------------------------------")

; Get the type of the first music element in the song (should be the guitar sequence above):
(writeln "The type of the first element in the song (should be the first guitar sequence): ")
(send 'get-type (car (send 'get-elements the-song)))
(writeln "---------------------------------")

; Get the duration, either as the sum of the time signatures or the absolute time in time ticks:
(writeln "Duration as sum of time signatures: ")
(send 'get-duration the-song) ; Time signature total time
(writeln "... and in time ticks: ")
(send 'get-duration-absolute the-song) ; Time ticks
(writeln "---------------------------------")

; Auxiliary predicate functions allow us to figure out the type of an element:
(define first-sequence (car (send 'get-elements the-song)))

(writeln "Is the first sequence a sequence? ")
(sequence? first-sequence)      ; Yes
(writeln "Is it a music element? ")
(music-element? first-sequence) ; Also yes
(writeln "Is it a note? ")
(note? first-sequence)          ; No
(writeln "---------------------------------")

; Before we can export the song as a MIDI file, it must first be converted to a more easily
; manageable association list by calling the 'get-midi' method that all music-elements provide:
(define flat-midi (send 'get-midi the-song))

; From here, we can transpose, scale, and reinstrument the song if so needed:

; Play the song twice as slowly:   (scale flat-midi 0.5)
; Play the song one octave higher: (transpose flat-midi 12)
; Play the song only with guitars: (reinstrument flat-midi 'guitar)

; We can also determine the degree of polyphony of the song from its flat-midi association list representation:
(writeln "What is the degree of polyphony? ")
(degree-of-polyphony flat-midi)
; .. whether or not it is monophonic:
(writeln "Is it monophonic? ")
(monophonic? flat-midi)
(writeln "---------------------------------")

; Linearize the song into 'note-abs-time-with-duration' forms:
(define abs-time-notes-with-duration (linearize flat-midi))

; Export the song as a MIDI file:
(writeln "Exporting MIDI to midi.mid...")
(transform-to-midi-file-and-write-to-file! abs-time-notes-with-duration "midi.mid")

