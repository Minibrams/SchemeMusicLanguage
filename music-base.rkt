; A #lang phrase is harmful here...

(module music-base racket
  (require "pp-base.rkt")
  (provide tag-it tag-name contents     ; reprovides from pp-base
           note-abs-time-with-duration transform-to-midi-file-and-write-to-file! find-in-list make-selector-function accumulate-right)

  ; ---------------------------------------------------------------------------------------------------------------
  ; Function definitions for the Scheme Music Language assignment.
  ; Kurt Normark, 2017.
  ; Assume that the following functions exist: file-exists? delete-file.
  
  ; ...............................................................................................................
  ; THE EXTERNAL PARTS:
  
  ; Construct the low-level internal representation of a note (with note-number) bound to a particular absolute time (in time ticks), and with a given duration (in time ticks).
  ; The channel parameter encodes the instrument used. Velocity represents the strengths of the node.
  (define (note-abs-time-with-duration abs-time channel note-number velocity duration)
    (if (valid-input-to-note-abs-time-with-duration? abs-time channel note-number velocity duration)
        (tag-it 'note-abs-time-with-duration (list abs-time channel note-number velocity duration))
        (error "note-abs-time-with-duration: Invalid construction.")))

  ; Validation predicate.
  (define (valid-input-to-note-abs-time-with-duration? abs-time channel note-number velocity duration)
    (and (and (integer? abs-time)
              (>= abs-time 0))

         (and (integer? channel)
              (>= channel 1 ) (<= channel 16))

         (and (integer? note-number)
              (>= note-number 0 ) (<= note-number 127)) 

         (and (integer? velocity)
              (>= velocity 0 ) (<= velocity 127))

         (and (integer? duration)
              (>= duration 0))))
  
  
  ; Absolute time low-level note selectors:
  (define low-level-note-abs-time    (make-selector-function 2 ))
  (define low-level-note-channel     (make-selector-function 3 ))
  (define low-level-note-note-number (make-selector-function 4 ))
  (define low-level-note-velocity    (make-selector-function 5 ))
  (define low-level-note-duration    (make-selector-function 6 ))
  
  ; Transform a list of low-level absolute timed noteon objects to MIDI, and write it to a file named filename.
  (define (transform-to-midi-file-and-write-to-file! low-level-event-list filename)
    (let* ((sorted-low-level-event-list (stable-sort-list low-level-event-list (lambda (x y) (<= (low-level-note-abs-time x) (low-level-note-abs-time y)))))
           (sorted-midi-noteon-list (map (lambda (lle)
                                           (midi-noteon
                                             (low-level-note-abs-time lle) (low-level-note-channel lle)
                                             (low-level-note-note-number lle) (low-level-note-velocity lle))) 
                                         sorted-low-level-event-list))
           (sorted-duration-list (map (lambda (lle) (low-level-note-duration lle)) sorted-low-level-event-list))
           (midi-noteoff-list (map (lambda (noteon-event duration) 
                                            (midi-noteoff
                                              (+ (midi-noteonoff-abs-time noteon-event) duration) (midi-noteonoff-channel noteon-event)
                                              (midi-noteonoff-notenumber noteon-event) (midi-noteonoff-velocity noteon-event)) )
                                          sorted-midi-noteon-list sorted-duration-list))
           (sorted-midi-noteoff-list (stable-sort-list midi-noteoff-list (lambda (x y) (<= (midi-noteonoff-abs-time x) (midi-noteonoff-abs-time y)))))
           (sorted-midi-noteon-and-noteoff-list 
                              (abs-time-merge sorted-midi-noteon-list sorted-midi-noteoff-list)
  ;                            (stable-sort-list (append sorted-midi-noteon-list midi-noteoff-list)
  ;                                       (lambda (x y) (<= (low-level-note-abs-time x) (low-level-note-abs-time y))))
           )
           (instrument-program-change-list 
              (list
                (midi-programchange 0 1 0)    ; Piano
                (midi-programchange 0 2 16)   ; Organ
                (midi-programchange 0 3 24)   ; Guitar
                (midi-programchange 0 4 40)   ; Violi
                (midi-programchange 0 5 73)   ; Flute
                (midi-programchange 0 6 57)   ; Trumpet
                (midi-programchange 0 7 125)  ; Helicopter
                (midi-programchange 0 8 124)  ; Telephone
              )
           )
           (instrument-time-vlq-list   (map unparse-integer-as-variable-length-quantity (make-list 8 0))) 
           (instrument-event-list (map convert-event-to-byte-string instrument-program-change-list))
  
           (delta-time-list-notes
              (cons initial-delta-time
                    (map (lambda (n1 n2) (- (midi-noteonoff-abs-time n1) (midi-noteonoff-abs-time n2)))
                         (cdr sorted-midi-noteon-and-noteoff-list)
                         (butlast sorted-midi-noteon-and-noteoff-list))))
  
  
  
           (delta-time-vlq-list (map unparse-integer-as-variable-length-quantity delta-time-list-notes))   ; a list of byte strings
           (note-event-list (map convert-event-to-byte-string sorted-midi-noteon-and-noteoff-list)) ; also a list of byte strings
  
           (end-of-track-events (map convert-event-to-byte-string (list (midi-meta 0 47 the-empty-byte-string))))  ; a list of byte string
           (end-of-track-delta-time-list (map unparse-integer-as-variable-length-quantity (list final-delta-time)))  ; a list of byte string. Allow final-delta-time time units until end of track event.
                
          )
       (when (file-exists? filename) (delete-file filename)) 
       (let* ((op (open-output-file filename #:mode 'binary #:exists 'truncate/replace)))  ; RACKET
         (begin
         (write-midi-header! op)
         (write-track-header! (+ (accumulated-byte-length instrument-time-vlq-list) (accumulated-byte-length instrument-event-list)
                                 (accumulated-byte-length delta-time-vlq-list) (accumulated-byte-length note-event-list)
                                 (accumulated-byte-length end-of-track-delta-time-list) (accumulated-byte-length end-of-track-events)
                              )
                              op)
          
         ; Write all delta times and events, as binary strings, to the output port op       
         (for-each (lambda (dt ev) 
                (write-byte-string-to-port! dt op) (write-byte-string-to-port! ev op))
               (append instrument-time-vlq-list delta-time-vlq-list end-of-track-delta-time-list)
               (append instrument-event-list note-event-list end-of-track-events))
  
         (write-track-footer! op)
         (write-midi-footer! op)
  
         (close-output-port op))  
       )
  ))
  
  
  
  ; ...............................................................................................................
  ; THE INTERNAL PARTS: It is not necessary to examine or understand the details below.
  
  ; Tagging aliases:
  (define tag-it cons)
  (define tag-name car)
  (define contents cdr)
  
  
  ; ---------------------------------------------------------------------------------------------------------------
  ; TRANSFORMATION TO MIDI - A DIRECT APPROACH.
  
  ; Representation of selected MIDI messages, with absolute timing. Constructors.
  (define (midi-noteon abs-time channel note-number velocity)
    (tag-it 'noteon (list abs-time channel note-number velocity)))
  
  (define (midi-noteoff abs-time channel note-number velocity)
    (tag-it 'noteoff (list abs-time channel note-number velocity)))
  
  (define (midi-programchange abs-time channel program-number)
    (tag-it 'programchange (list abs-time channel program-number)))
  
  (define (midi-meta abs-time type contents)
    (tag-it 'meta (list abs-time type contents)))
  
  
  ; Selectors, MIDI messages, with absolute timing.
  ; Selectors both for noteon and noteoff:
  (define midi-noteonoff-abs-time (make-selector-function 2 ))
  (define midi-noteonoff-channel (make-selector-function 3 ))
  (define midi-noteonoff-notenumber (make-selector-function 4 ))
  (define midi-noteonoff-velocity (make-selector-function 5 ))
  
  (define midi-programchange-abs-time (make-selector-function 2 ))
  (define midi-programchange-channel (make-selector-function 3 ))
  (define midi-programchange-program-number (make-selector-function 4))
  
  (define midi-meta-abstime (make-selector-function 2 ))
  (define midi-meta-type (make-selector-function 3 ))
  (define midi-meta-contents (make-selector-function 4))
  
  ; ---------------------------------------------------------------------------------------------------------------
  ; Transformation to the binary level.
  
  (define initial-delta-time 100)
  (define final-delta-time 20000) 
  
  (define (abs-time-merge noteon-list-1 noteoff-list-2)
    (abs-time-merge-iter noteon-list-1 noteoff-list-2 '())
  )
  
  ; Helping function to abs-time-merge, for iteration purposes. For same abs-time, take the noteoff before the noteon event.
  (define (abs-time-merge-iter noteon-list-1 noteoff-list-2 res)
    (cond ((null? noteon-list-1) (append (reverse res) noteoff-list-2))
          ((null? noteoff-list-2) (append (reverse res) noteon-list-1))
          (else (cond ((= (midi-noteonoff-abs-time (car noteon-list-1)) (midi-noteonoff-abs-time (car noteoff-list-2)))
                          (abs-time-merge-iter noteon-list-1 (cdr noteoff-list-2) (cons (car noteoff-list-2) res)))
                      ((< (midi-noteonoff-abs-time (car noteon-list-1)) (midi-noteonoff-abs-time (car noteoff-list-2)))
                          (abs-time-merge-iter (cdr noteon-list-1) noteoff-list-2 (cons (car noteon-list-1) res)))
                      (else
                          (abs-time-merge-iter noteon-list-1 (cdr noteoff-list-2) (cons (car noteoff-list-2) res)))))))
  
  (define (write-midi-header! op)
    (let ((format 0)
          (number-of-tracks 1)
          (ppqn 960)
         )
      (write-byte-string-to-port! 
        (byte-string-append 
                       (apply bytevector (map as-number (list #\M #\T #\h #\d)))  ;  "MThd" 
                       (int10-to-binary 6 4)                            ; Length of remaining: always 6
                       (int10-to-binary (as-number format) 2)           ; First 2 bytes of the 6 bytes
                       (int10-to-binary (as-number number-of-tracks) 2) ; Next 2 bytes of the 6 bytes
                       (int10-to-binary (as-number ppqn) 2)             ; Final 2 bytes of the 6 bytes
        )
        op)))
  
  (define (write-track-header! track-byte-length op)
    (write-byte-string-to-port! 
      (byte-string-append 
                     (apply bytevector (map as-number (list #\M #\T #\r #\k)))  ;  "MTrk"
                     (int10-to-binary track-byte-length 4) ; 4 bytes length of data
       )
       op))
  
  (define (write-track-footer! op)
    'nothing-to-do)  ; the end of track event is handled as the very last (meta) event. 
  
  (define (write-midi-footer! op)
    'nothing-to-do)
  
  ; Return the byte lengts of all the byte strings in list-of-byte-strings.
  (define (accumulated-byte-length list-of-byte-strings)
    (accumulate-right + 0 (map byte-string-length list-of-byte-strings)))
  
  ; Convert a midi event (one of the constructed midi value objects) to a byte string.
  (define (convert-event-to-byte-string midi-event)
    (cond ((eq? (tag-name midi-event) 'noteon)
             (let ((channel (midi-noteonoff-channel midi-event))
                   (note-number (midi-noteonoff-notenumber midi-event))
                   (velocity (midi-noteonoff-velocity midi-event)))
               (byte-string-append 
                 (make-byte-string-from-hex-2 9 (- channel 1))         ; status byte: status half byte + channel number
                 (int10-to-binary note-number 1)                       ; one byte, note
                 (int10-to-binary (between 0 127 velocity) 1)          ; one byte, velocity
               )))
          ((eq? (tag-name midi-event) 'noteoff)
             (let ((channel (midi-noteonoff-channel midi-event))
                   (note-number (midi-noteonoff-notenumber midi-event))
                   (velocity (midi-noteonoff-velocity midi-event)))
               (byte-string-append 
                 (make-byte-string-from-hex-2 8 (- channel 1))         ; status byte: status half byte + channel number
                 (int10-to-binary note-number 1)                       ; one byte, note
                 (int10-to-binary (between 0 127 velocity) 1)          ;one byte, velocity
               )))
          ((eq? (tag-name midi-event) 'programchange)
             (let ((channel (midi-programchange-channel midi-event))
                   (program-number (midi-programchange-program-number midi-event)))
               (byte-string-append 
                 (make-byte-string-from-hex-2 12 (- channel 1))        ; status byte: status half byte + channel number
                 (int10-to-binary program-number 1)                    ; one byte
               )))
          ((eq? (tag-name midi-event) 'meta) 
             (let* ((type (midi-meta-type midi-event))
                    (contents (midi-meta-contents midi-event))
                    (vlq-length (unparse-integer-as-variable-length-quantity (byte-string-length contents))))
               (byte-string-append 
                 (make-byte-string-from-hex-2 15 15)                   ; one byte  FF.
                 (int10-to-binary type 1)                              ; one data byte. The type of the meta event
                 vlq-length                                            ; length of contents, as a variable length quantity
                 contents                                              ; the meta bytes as such
              )))
          (else (error "convert-event-to-byte-string: Unknown tag name of midi event"))
    ))
  
  
  ; ---------------------------------------------------------------------------------------------------
  ;;; Byte level functions. Here R6RS, implemented as byte vectors.
  
  (define byte-string-append bytes-append) ; RACKET
  
  ; RACKET
  (define (bytevector . bytes-input)
    (apply bytes bytes-input)
  )
  
  ; RACKET
  (define byte-string-length bytes-length)
  
  ; RACKET
  (define byte-string-ref bytes-ref)
  
  (define the-empty-byte-string (bytevector))
  
  ; Native in RACKET:
  ;(define (write-byte byte port)
  ;   (put-u8 port byte))
  
  (define (make-bytevector lgt byte)
    (make-bytes lgt byte))
  
  ;; Convert a decimal integer n to a binary quantity, represented as a byte string of length number-of-bytes.
  ;; In the resulting binary string, the most significant byte comes first. This corresponds to big endian byte order.
  ;; If n is too large to be represented in number-of-bytes, an error occurs.
  ;; n is the integer to convert.
  ;; number-of-bytes:  The desired number of bytes.
  ;; Returns an R6RS byte vector
  (define (int10-to-binary n number-of-bytes)
    (let* ((byte-list (binary-bytes-of-decimal-integer n))
           (lgt-byte-list (length byte-list)))
      (if (> lgt-byte-list number-of-bytes)
          (error "int10-to-binary: Number does not fit")
          (list->bytes (append (make-list (- number-of-bytes lgt-byte-list) 0) byte-list)))))
  
  (define (binary-bytes-of-decimal-integer n)
    (reverse (binary-bytes-of-decimal-integer-1 n)))
  
  (define (binary-bytes-of-decimal-integer-1 n)
   (let ((rem (remainder n 256))
         (rest (quotient n 256)))
    (if (= rest 0)
        (list rem)
        (cons rem (binary-bytes-of-decimal-integer-1 rest)))))
           
      
  ;; Make a byte vector, with single byte, from two hex numbers. 
  ;; hx1 An decimal integer number between 0 and 15
  ;; hx2 An decimal integer number between 0 and 15
  ;; returns A string of length one.
  (define (make-byte-string-from-hex-2 hx1 hx2)
    (list->bytes (list (make-char-2-hex hx1 hx2))))
  
  ;; Make an integer from two hex numbers
  ;; hx1 An decimal integer number between 0 and 15
  ;; hx2 An decimal integer number between 0 and 15
  ;; Returns A integer.
  ;; The name of this function is slightly misleading. It converts two (2) hex numbers (integer decimals) to a character.\
  ;; The -2- part is NOT an abbreviation of -to-.
  (define (make-char-2-hex hx1 hx2 )
    (+ (* hx1 16) hx2))  ; was as-char...
  
  ; Return a byte string which represents n as a variable length quantiy.
  ; n is a non-negative integer.
  (define (unparse-integer-as-variable-length-quantity n)
    (let ((low (remainder n 128))
          (rest (quotient n 128)))
      (byte-string-append (unparse-integer-as-variable-length-quantity-highs rest) (make-bytevector 1 low))))
  
  ; Produce bytes in which the high bits are all set to 1.  
  (define (unparse-integer-as-variable-length-quantity-highs n)
    (if (= n 0)
        the-empty-byte-string
        (let ((low (remainder n 128))
              (rest (quotient n 128)))
          (byte-string-append (unparse-integer-as-variable-length-quantity-highs rest) (make-bytevector 1 (+ 128 low))) ;    + 128  enforces one as first bit.
          )))
  
  ; Convert x to a string.
  (define (as-string x)
    (cond ((number? x) (number->string x))
          ((symbol? x) (symbol->string x))
          ((string? x) x)
          ((boolean? x) 
              (if x "true" "false"))   ; consider "#t" and "#f" as alternatives
          ((char? x) (make-string 1 x))
          ((list? x)
              (byte-string-append "(" 
                 (string-merge (map as-string x) (make-list (- (length x) 1) " "))
                 ")"))
          ((vector? x)
            (let ((lst (vector->list x)))
              (byte-string-append "#(" 
                 (string-merge (map as-string lst) (make-list (- (length lst) 1) " "))
                 ")")))
          (else "??")))
  
  (define (string-merge str-list-1 str-list-2)
   (cond ((null? str-list-1) (apply string-append str-list-2))
         ((null? str-list-2) (apply string-append str-list-1))
         (else (string-append
                (car str-list-1) (car str-list-2)
                (string-merge (cdr str-list-1) (cdr str-list-2))))))
  
  
  ;; Convert x to a number. Strings, numbers, chars and booleans are supported.
  ;; Strings with digits are converted using string->number, chars are converted with char->integer, true is converted to 1, and false to 0.
  (define (as-number x)
    (cond ((string? x) (string->number x))
          ((number? x) x)
          ((char? x) (char->integer x))
          ((boolean? x) (if x 1 0))  ; false -> 0, true -> 1
          (else
           (error
            (string-append "Cannot convert to number "
                           (as-string x))))))
  
  
  ;; Convert x to a character. Integers, strings, booleans and symbols are supported.
  ;; If x is an integer between 0 and 255 return ASCII char number x. If x is a string return the first character in the string (which is supposed to be non-empty).
  ;; If x is a boolean return the character #\t for true and #\f for false. If x is a symbol return the first character of the print name of the string. Else return #\?.
  (define (as-char x)
    (cond ((char? x) x)
          ((integer? x) 
             (if (and (>= x 0) (<= x 255)) 
                 (integer->char x)
                 #\?))
          ((string? x) (string-ref x 0))
          ((boolean? x) (if x #\t #\f))
          ((symbol? x)  (as-char (as-string x)))
          (else #\?)))
  
  ;; Return all but the last element of a list.
  ;; .pre-condition lst is not empty.
  (define (butlast lst)
    (cond ((null? lst)
             (error "Cannot apply butlast on an empty list"))
          ((null? (cdr lst))  ; a list of a single element
             '())
          (else (cons (car lst) (butlast (cdr lst))))))
  
  ; Write the (byte) string str to (the open) port.
  (define (write-byte-string-to-port! str port)
    (write-byte-string-to-port-1 0 (byte-string-length str) str port))
  
  (define (write-byte-string-to-port-1 i max str port)
    (when (< i max)
      (begin
        (let ((ch (byte-string-ref str i)))
         (write-byte ch port)
         (write-byte-string-to-port-1 (+ i 1) max str port)))))
  
  ; A list sorting function which (maybe) is stable. A trick...
  (define (stable-sort-list lst compare-fn)
    (sort (reverse lst) compare-fn))
  
  ;; Return a list of n elements, each being el
  (define (make-list n el)
    (if (<= n 0) '() (cons el (make-list (- n 1) el))) )
  
  ; Return n. But if n > ma, return ma. If n < mi return mi.
  (define (between mi ma n)
    (max (min n ma) mi))


)