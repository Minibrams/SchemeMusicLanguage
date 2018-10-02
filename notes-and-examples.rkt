#lang racket

(instrument 'guitar (parallel (sequence (C# 2 1/4) (D# 2 1/4))
                              (sequence (C# 1 1/4) (D# 1 1/4))
                              (sequence (E  1 1/4) (F  1 1/4))
                              (sequence (parallel (sequence (C# 2 1/4))
                                                  (sequence (C# 2 1/4)))
                                        (parallel (sequence (C# 2 1/4))
                                                  (sequence (C# 2 1/4))))))



; ---------- Tasks -------------
; The MusicElement class should contain the basic information that all elements must have: start time and duration.
; These values will be overriden by specialized classes in the following ways:
;
; - Note/Pause: Start time is inferred from the instrument function. The start time must be assigned correctly
;               by propagating through Parallel and Sequence objects.
;
; - Sequence:   Start time is inferred from the instrument function like the Note objects. Duration is the
;               start time + duration of the last music element in the sequence.
;
; - Parallel:   Start time is inferred from the instrument function like the Note objects. Duration is the
;               duration of the longest music element in the parallel construction.
;
; In addition, the MusicElement class must take a list of music elements as input to its constructor.
; It must also provide 'Actualize(instrument, start-time)' that recursively joins the result of 'Actualize' from all input music elements
; into a list. 
;
; Must have a full Note class that contains all information (start time, note, octave, length, instrument).
;
; The instrument function should call all its music elements 'Actualize' method passing the
; instrument and start-time = 0 as arguments.
;
; The note methods C#, D#, etc. should return unfinished Note objects that require a start time and an instrument
; to be fully actualized.
;
; A Pause method should exist that requires an instrument and a start time, while having a velocity of 0.
;
; The Sequence and Parallel classes should return unfinished objects that require a start time and an instrument 
; before being fully actualized. When they receive the instrument and start time, they should use that information to fully 
; actualize every MusicElement object they contain, returning them as a list of full Note objects.
;
; The Sequence class on 'Actualize' should actualize every underlying music element with the current start time + the
; duration of the previous music element as the start time of the current music element.
;
; The Parallel class on 'Actualize' should actualize every underlying music element with the same start time as the
; Parallel object itself.
;
; As the final step, the instrument function should map the list of Note objects to a list of linearized absolute-time forms.

