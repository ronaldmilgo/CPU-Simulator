#lang racket

(provide hours gate1 sel-ckt eq1-ckt eq2-ckt latch-ckt eq1-config1x eq1-config2x
         sel-config1x sel-config2x latch-config1x latch-config2x clock-ckt seq-or-ckt one-one-ckt
          gate gate? gate-type gate-inputs gate-output
         ckt ckt? ckt-inputs ckt-outputs ckt-gates
         good-gate? good-circuit?
         all-wires find-gate
         ha-ckt fa-ckt
         next-value
         next-config
         stable? all-stable-configs output-values init-config
         simulate
         final-config
         add-ckt
         dff-ckt
         timing-ckt)




; Please do not edit lines above this one

;**********************************************************
; CS 201b HW #5, due 11:59 pm Wednesday, April 3rd.
; using gradescope.
;**********************************************************
; Name: Ronald Milgo
; Email address: ronald.milgo@yale.edu
;**********************************************************

; Computer science topics: gates and circuits

; Unless the problem specifies otherwise:
; * You may solve the problem using any method 
; and any Racket constructs, except mutators (set! and its relatives.)
; EXCEPT you may use hash mutators, e.g., hash-set!
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;**********************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 2)

;**********************************************************

; Wires and gates.

; A wire is identified by a Racket symbol, for example 'x, 'y0, or 'next.
; Strings (eg, "x" or "next") are not permitted wire names.

; A gate is a struct with three fields: 
; 1) a symbol indicating the type of gate, one of:
;       'not, 'and, 'or, 'xor, 'nand, 'nor
; 2) a list of the wire identifiers of the inputs
; 3) the output wire identifier

(struct gate (type inputs output) #:transparent)

; Examples of gates:

(define gate1 (gate 'and '(x y) 'z))
(define gate2 (gate 'or '(x v) 'v))
(define gate3 (gate 'not '(s) 't))
(define gate4 (gate 'nand '(x x) 'z))

;**********************************************************

; Circuits.

; A circuit is a struct with three fields
; (1) a list of input wire identifiers
; (2) a list of output wire identifiers
; (3) a list of gates

(struct ckt (inputs outputs gates) #:transparent)

;**********************************************************
; Examples of circuits

; Here is a circuit to compare the values of its two inputs
; and output 1 if they are equal, 0 if they are unequal.
; This computes one-bit compare for equality, and implements
; the sum-of-products representation.  This is a combinational
; circuit (no cycle of wires and gates.)

(define eq1-ckt
  (ckt
   '(x y)
   '(z)
   (list
    (gate 'not '(x) 'cx)
    (gate 'not '(y) 'cy)
    (gate 'and '(x y) 't1)
    (gate 'and '(cx cy) 't2)
    (gate 'or '(t1 t2) 'z))))
 
; This is interpreted as follows:
; the inputs of the circuit are the wires x and y,
; the outputs of the circuit consist of just the wire z,
; there are five gates specified as follows:
; wire cx is the output of a NOT gate with input x,
; wire cy is the output of a NOT gate with input y,
; wire t1 is the output of an AND gate with inputs x and y,
; wire t2 is the output of an AND gate with inputs cx and cy,
; wire z is the output of an OR gate with inputs t1 and t2.

; Here is another implementation of comparing two bits for equality.
; This uses the implementation as the NOT of (x XOR y).
; This is also a combinational circuit.
; The inputs and output of this circuit are named as in eq1-ckt.

(define eq2-ckt
  (ckt
   '(x y)
   '(z)
   (list
    (gate 'xor '(x y) 'w)
    (gate 'not '(w) 'z))))

; Here is a two-bit selector whose Boolean expressions are as follows.

; z_1 = x_1 * s' + y_1 * s
; z_0 = x_0 * s' + y_0 * s

; For this circuit, z_1 and z_0 are
; equal to x_1 and x_0 if s = 0, and
; z_1 and z_0 are equal to y_1 and y_0
; if s = 1.

; This is also a combinational circuit.

(define sel-ckt
  (ckt
   '(x1 x0 y1 y0 s)
   '(z1 z0)
   (list
    (gate 'not '(s) 'sc)
    (gate 'and '(x1 sc) 'u1)
    (gate 'and '(y1 s) 'v1)
    (gate 'or '(u1 v1) 'z1)
    (gate 'and '(x0 sc) 'u0)
    (gate 'and '(y0 s) 'v0)
    (gate 'or '(u0 v0) 'z0))))

; This is a NAND latch, used to store one bit.
; It is a sequential (not combinational) circuit,
; because it has a loop from a wire to itself through
; other wires and gates.

(define latch-ckt
  (ckt
   '(x y)
   '(q u)
   (list
    (gate 'nand '(x u) 'q)
    (gate 'nand '(y q) 'u))))

; The following is also a sequential circuit, with
; an OR gate one of whose inputs is its output.
; (The "Garden of Eden" circuit.)

(define seq-or-ckt
  (ckt
   '(x)
   '(z)
   (list
    (gate 'or '(x z) 'z))))

; The next is also a sequential circuit.
; It could serve as a clock.
; Note that this circuit has *no* inputs, but
; does have an output.

(define clock-ckt
  (ckt
   '()
   '(z)
   (list
    (gate 'not '(z) 'z))))

;**********************************************************
; ** problem 1 ** (9 points)
; Write two procedures:

; (good-gate? value)
; (good-circuit? value)

; (good-gate? value) takes an arbitrary value
; and returns #t if it is a well-formed gate,
; and #f otherwise.
; To be a well-formed gate, the value must be
; a gate struct whose three fields satisfy
; (1) the type field is one of the gate symbols:
;     'not, 'and, 'or, 'xor, 'nand, 'nor,
; (2) the inputs field is a list of wire identifiers
; (3) the output field is a single wire identifier

; In addition, the number of inputs should be correct for each gate type.
; A gate of type 'not has 1 input, while
; gates of types 'and, 'or, 'xor, 'nand, 'nor have 2 inputs.

; (good-circuit? value) takes an arbitrary value and
; returns #t if value is a well-formed circuit, 
; and returns #f otherwise.

; To be a well-formed circuit, it must be a ckt struct
; and its inputs field must be a list of wires,
; its outputs field must be a list of wires, and
; its gates field must be a list of gates that
; are well-formed according to the good-gate? procedure.

; In addition, the circuit must satisfy the conditions:
; (1) no input of the circuit is the output of a gate,
; (2) every input of a gate is either 
; an input of the circuit or the output of a gate,
; (3) no wire is the output of two or more gates,
; (4) every output of the circuit is either an input
; of the circuit or the output of a gate.

; Examples
; (good-gate? gate1) => #t
; (.. and similarly for gate2, gate3, gate4)
; (good-gate? (gate 'not 'x 'y)) => #f
; (good-gate? (gate 'nor '("x" "y") "z")) => #f
; (good-gate? (gate 'and '(1 2) 3)) => #f
; (good-gate? (gate 'equal '(x y) 'z)) => #f
; (good-gate? (gate 'or '(w x y) 'z)) => #f

; (good-circuit? sel-ckt) => #t
; (.. and similarly for eq1-ckt, eq2-ckt, latch-ckt, seq-or-ckt, clock-ckt)
; (good-circuit? 'hi) => #f
; (good-circuit? (ckt '() '() '())) => #t
; (good-circuit? (ckt '(x y) '(z) (list (gate 'and '(x y) 'x) (gate 'or '(x y) 'z)))) => #f
; (good-circuit? (ckt '(x y) '(z) (list (gate 'nor '(x y) 'z) (gate 'nand '(x y) 'z)))) => #f
; (good-circuit? (ckt '(x y) '(u z) (list (gate 'or '(x y) 'z)))) => #f
;**********************************************************


(define (all_symbols? lst)
  (if (list? lst)
      (andmap symbol? lst)
      #f))


(define (good-gate? value)
  (and (gate? value)
       (member (gate-type value) '(and not or nor nand xor))
       (let ((inputs (gate-inputs value)))
         (and (list? inputs)
              (case (gate-type value)
                ((not) (= (length inputs) 1))
                (else (= (length inputs) 2)))))
       (all_symbols? (gate-inputs value))
       (symbol? (gate-output value))))

             
(define (wire-equal-gate-output? wire gates)
  (ormap (lambda (x) (eq? wire (gate-output x))) gates))

(define (good-circuit? value)
 (and (ckt? value)
      (all_symbols? (ckt-inputs value))
      (all_symbols? (ckt-outputs value))
      (andmap good-gate? (ckt-gates value))
      (not (ormap (lambda (input) (wire-equal-gate-output? input (ckt-gates value))) (ckt-inputs value)))
      (andmap (lambda (gate) (andmap (lambda (input) (or (member input (ckt-inputs value)) (wire-equal-gate-output? input (ckt-gates value)))) (gate-inputs gate))) (ckt-gates value)); Check that every gate input is either a circuit input or another gate's output 
      ;check that no wire is the output of more than one gate
      (let ((outputs (map gate-output (ckt-gates value))))
        (and (list? outputs) (= (length outputs) (length (remove-duplicates outputs)))))
      ; Check every output of the circuit is valid
      (andmap (lambda (output) (or (member output (ckt-inputs value)) (wire-equal-gate-output? output (ckt-gates value)))) (ckt-outputs value))))

;**********************************************************
; ** problem 2 ** (10 points)
; Write two procedures.

; (all-wires circuit) to return the list of all the wire names that appear
;      in the circuit, as circuit inputs, circuit outputs, gate
;      inputs or gate outputs, in that order, with duplicates removed.
; (find-gate wire circuit) to return the gate in the circuit with the given
;      output wire, or #f if there is no such gate.

; You may assume that circuit is a well-formed circuit; in particular,
; a wire is the output of *at most one* gate.

; Examples:

; (all-wires eq1-ckt) => '(x y z cx cy t1 t2)
; (all-wires sel-ckt) => '(x1 x0 y1 y0 s z1 z0 sc u1 v1 u0 v0)
; (find-gate 't2 eq1-ckt) => (gate 'and '(cx cy) 't2)
; (find-gate 'w eq2-ckt) => (gate 'xor '(x y) 'w)
; (find-gate 'y sel-ckt) => #f
;**********************************************************

(define (all-wires circuit)
      (remove-duplicates (append (append (ckt-inputs circuit) (ckt-outputs circuit)) (map gate-output (ckt-gates circuit))))) 

(define (find-gate wire circuit)
  (define (gate-helper gates)
      (if (null? gates)
        #f
        (if (equal? wire (gate-output (car gates)))
            (car gates)
            (gate-helper (cdr gates)))))
  (gate-helper (ckt-gates circuit)))

;**********************************************************
; ** problem 3 ** (10 points)
; Define circuits for a half-adder and a full-adder in the representation described above.

; Your half-adder should be called ha-ckt
; and should have input wires: x and y and output wires: z and co, 
; where z is the exclusive or of x and y, and co is 1 if both x and y are 1.

; Your full-adder should be called fa-ckt 
; and should have input wires: x, y, and ci and output wires: z and co,
; where the value of z is 1 if the sum of x, y, and ci is odd,
; and the value of co is 1 if and only if at least two of x, y, and ci are 1.

; The order and names of the circuit input and output wires should be as specified above,
; but the number and names of internal wires (wires that are neither circuit inputs 
; nor circuit outputs) are up to you.

; Examples
; (good-circuit? ha-ckt) => #t
; (good-circuit? fa-ckt) => #t
; (ckt-inputs ha-ckt) => '(x y)
; (ckt-outputs ha-ckt) => '(z co)
; (ckt-inputs fa-ckt) => '(x y ci)
; (ckt-outputs fa-ckt) => '(z co)

; (output-values ha-ckt (final-config ha-ckt (init-config ha-ckt '(1 1)))) => (0 1)
; (output-values fa-ckt (final-config fa-ckt (init-config fa-ckt '(1 1 1)))) => (1 1)

; For the last two tests, your procedures output-values, final-config, init-config must be working.
;**********************************************************

(define ha-ckt
  (ckt '(x y)
       '(z co)
       (list
        (gate 'xor '(x y) 'z)
        (gate 'and '(x y) 'co))))

(define fa-ckt
  (ckt '(x y ci)
       '(z co)
       (list
        (gate 'xor '(x y) 'a)
        (gate 'xor '(a ci) 'z)
        (gate 'and '(y ci) 'b)
        (gate 'and '(x ci) 'c)
        (gate 'and '(x y) 'd)
        (gate 'or '(b c) 'e)
        (gate 'or '(e d) 'co))))

;**********************************************************

; A configuration of a circuit is a hash table giving a value (0 or 1)
; for each wire in the circuit.  A table is a list key-value pairs,
; such that the key is the symbol for the wire and the value is either
; 0 or 1.


; Examples

; Two configurations of the wires of the eq1-ckt Note that the order
; of wires in the configuration is that returned by (all-wires
; eq1-ckt).

(define eq1-config1x
  (make-hash '((x . 0) (y . 1) (z  . 0) (cx  . 0)
	       (cy  . 0) (t1  . 0) (t2  . 0))))

(define eq1-config2x
  (make-hash '((x . 0) (y . 0) (z  . 0) (cx  . 1)
	       (cy  . 1) (t1  . 0) (t2  . 0))))

; Two configurations of the wires of the sel-ckt

(define sel-config1x
  (make-hash '((x1 . 0) (x0 . 1) (y1 . 1) (y0 . 0) (s . 1) 
	(z1 . 0) (z0 . 0) (sc . 0) (u1 . 0) (v1 . 0)
	(u0 . 0) (v0 . 0))))

(define sel-config2x 
  (make-hash '((x1 . 1) (x0 . 1) (y1 . 0) (y0 . 0) (s . 0) 
	(z1 . 0) (z0 . 0) (sc . 1) (u1 . 0) (v1 . 0)
	(u0 . 0) (v0 . 0))))


; Two configurations of the wires of the latch-ckt

(define latch-config1x 
  (make-hash '((x . 0) (y . 0) (q . 0) (u . 0) )))

(define latch-config2x 
  (make-hash '((x . 0) (y . 1) (q . 1) (u . 0) )))

;**********************************************************
; ** problem 4 ** (10 points)
; Write a procedure 

;(next-value wire circuit config)

; that returns the value on the given wire of the given circuit,
; *after one gate delay* starting with the given configuration config
; of the circuit.

; You may assume that
; (1) circuit is a well-formed circuit, according to the specifications in problem 1,
; (2) the given wire is one of the wires of circuit, and
; (3) the given configuration config specifies a value for every wire in the circuit.

; If the given wire is an input wire of the circuit,
; its next value is just its value in the configuration config.

; If the given wire is the output wire of a gate, its next value is
; obtained by finding the gate of circuit for which it is the output
; wire, looking up the values of the input wires of the gate in the
; configuration config, and applying the appropriate function of the
; gate to the input values.

; Note that this doesn't compute the *eventual* value (if any) of the wire, 
; just the *next* value of the wire, after one gate delay.

; You may want to write an auxiliary procedure to look up the value of a wire in a configuration.

; Examples
 ;(next-value 'cx eq1-ckt eq1-config1x) => 1
; (next-value 't2 eq1-ckt eq1-config1x) => 0
; (next-value 'z eq1-ckt eq1-config2x) => 0
; (next-value 'x0 sel-ckt sel-config1x) => 1
; (next-value 'v1 sel-ckt sel-config1x) => 1
; (next-value 'v0 sel-ckt sel-config2x) => 0
;**********************************************************

(define (wire_value wire config)
  (hash-ref config wire))

(define (gate_value gate-type inputs)
  (case gate-type
    ('not (if (equal? (car inputs) 1) 0 1))
    ('and (if (andmap (lambda (x) (equal? x 1)) inputs) 1 0))
    ('or (if (ormap (lambda (x) (equal? x 1)) inputs) 1 0))
    ('xor (if (= (length (filter (lambda (x) (equal? x 1)) inputs)) 1) 1 0))
    ('nand (if (andmap (lambda (x) (equal? x 1)) inputs) 0 1))
    ('nor (if (ormap (lambda (x) (equal? x 1)) inputs) 0 1))
    (else 0)))
    
    
    

(define (next-value wire circuit config)
  (if (member wire (ckt-inputs circuit))
      (wire_value wire config)
      (let ((gate (find-gate wire circuit)))
        (if gate
            (let ((inputs (map (lambda (input-wire)
                                 (wire_value input-wire config))
                               (gate-inputs gate))))
              (gate_value (gate-type gate) inputs))
            0))))




;**********************************************************
; ** problem 5 ** (10 points)
; Write a procedure 

; (next-config circuit config)

; that takes a circuit and a current configuration config and returns
; the "next" configuration of the circuit, after *one gate delay* has
; elapsed.

; In the "next" configuration of the circuit the value of each wire is
; the result of applying the next-value procedure to the wire, circuit
; and the configuration config.  Note that only the values of wires in
; config are used for inputs, not the new values.

; Thus, values on the input wires do not change, and each wire that is
; the output of a gate has the value determined by its gate function
; applied the values of its input wires in the configuration config.

; This is a rather simplified model of the time-varying behavior of
; wires and gates.

; Examples
;> (next-config eq1-ckt eq1-config1x)
; '#hash((cx . 1) (cy . 0) (t1 . 0) (t2 . 0) (x . 0) (y . 1) (z . 0))

;> (next-config eq1-ckt eq1-config2x)
; '#hash((cx . 1) (cy . 1) (t1 . 0) (t2 . 1) (x . 0) (y . 0) (z . 0))

;> (next-config sel-ckt sel-config1x)
; '#hash((s . 1) (sc . 0) (u0 . 0) (u1 . 0) (v0 . 0) (v1 . 1)
;        (x0 . 1) (x1 . 0) (y0 . 0) (y1 . 1) (z0 . 0) (z1 . 0))

;> (next-config sel-ckt (next-config sel-ckt sel-config1x))
; '#hash((s . 1) (sc . 0) (u0 . 0) (u1 . 0)  (v0 . 0) (v1 . 1)
;        (x0 . 1) (x1 . 0) (y0 . 0) (y1 . 1) (z0 . 0) (z1 . 1))

;> (next-config latch-ckt latch-config1x)
; '#hash((q . 1) (u . 1) (x . 0) (y . 0))

;> (next-config latch-ckt latch-config2x) 
; '#hash((q . 1) (u . 0) (x . 0) (y . 1))

;**********************************************************

(define (next-config circuit config)
  (define result (hash-copy config))
  (map (lambda (wire)
         (let ((next_value (next-value wire circuit config)))
           (hash-set! result wire next_value)))
         (all-wires circuit))
       result);return updated hash-table

;**********************************************************
; ** problem 6 ** (10 points)
; Write four procedures

; (stable? circuit config)
; (all-stable-configs circuit)
; (output-values circuit config)
; (init-config circuit input-values)

; (stable? circuit config)
; returns #t if the next configuration of the circuit after the
; configuration config is the same as config, ie, this configuration
; is stable for the circuit.

; (all-stable-configs circuit)
; returns a list of all the stable configurations of the circuit.  The
; wires in the configurations should be listed in the same order as
; (all-wires circuit), and the values in the configurations list
; should be in increasing order, considered as binary numbers.

; (output-values circuit config)
; returns a list giving the Boolean values of each of the output wires
; of the circuit in the configuration config.  The order is the same
; as the list of output wires of the circuit.

; (init-config circuit input-values)
; takes a circuit and a list input-values of Boolean values which has
; the same length as the number of inputs of the circuit and returns a
; configuration in which the circuit input wires have the values
; specified (in order) by the list inputs, and all other wires have
; the value 0.

; Examples

; (stable? eq1-ckt (make-hash '((x . 0) (y . 0) (z . 1) (cx . 1) 
;    (cy . 1) (t1 . 0) (t2 . 1)))) => #t

; (stable? eq1-ckt (make-hash '((x . 0) (y . 0) (z . 0)
;    (cx . 1) (cy . 0) (t1 . 1) (t2 . 0)))) => #f

;> (all-stable-configs eq2-ckt)
;'(#hash((w . 0) (x . 0) (y . 0) (z . 1))
;  #hash((w . 1) (x . 0) (y . 1) (z . 0))
;  #hash((w . 1) (x . 1) (y . 0) (z . 0))
;  #hash((w . 0) (x . 1) (y . 1) (z . 1)))

;> (all-stable-configs seq-or-ckt)
; '(#hash((x . 0) (z . 0)) #hash((x . 0) (z . 1)) #hash((x . 1) (z . 1)))

; (output-values eq1-ckt eq1-config2x) => '(0)
; (output-values latch-ckt latch-config2x) => '(1 0)

; (init-config eq1-ckt '(1 0)) => 
; '#hash((cx . 0) (cy . 0) (t1 . 0) (t2 . 0) (x . 1) (y . 0) (z . 0))

; (init-config clock-ckt '()) => '#hash((z . 0))

;**********************************************************

(define (stable? circuit config)
   (let ((next_config (next-config circuit config)))
     (equal? next_config config)))

(define (binary-configs num-wires)
  (if (= num-wires 0)
      '(())
      (let ((previous (binary-configs (- num-wires 1))))
        (append (map (lambda (config) (cons 0 config)) previous)
                (map (lambda (config) (cons 1 config)) previous)))))


(define (all-configs configs wires)
  (map (lambda (config)
         (let ((hash (make-hash)))
           (for-each (lambda (wire value)
                       (hash-set! hash wire value))
                     wires
                     config)
           hash))
       configs))

(define (all-stable-configs circuit)
  (let* ((wires (all-wires circuit))
         (num-wires (length wires))
         (binary-configurations (binary-configs num-wires))
         (hash-configurations (all-configs binary-configurations wires)))
    (filter (lambda (config) (stable? circuit config)) hash-configurations)))


;; Hint: you may want to define all-configs, which generates all
;; configurations for a given number of wires.  Much like your old
;; friend power-set.

(define (output-values circuit config)
  (map (lambda (output-wire) (hash-ref config output-wire)) (ckt-outputs circuit)))


(define (init-config circuit input-values)
  (let ((config (make-hash)))
    (map (lambda (input-wire value) 
                (hash-set! config input-wire value))
              (ckt-inputs circuit)
              input-values)
    (map (lambda (wire) 
                (unless (hash-has-key? config wire)
                  (hash-set! config wire 0)))
              (all-wires circuit))
    config))


; *********************************************************
; ** problem 7 ** (10 points)
; Write a procedure 

; (simulate circuit config n)

; which simulates the given circuit from the given configuration by
; repeatedly calling next-config until either the configuration
; reached is stable, or next-config has been called n times, whichever
; occurs first.

; Examples
;> (simulate clock-ckt (make-hash '((z . 0))) 4)
; '(#hash((z . 0)) #hash((z . 1)) #hash((z . 0)) #hash((z . 1)) #hash((z . 0)))

;> (simulate eq1-ckt eq1-config1x 5) =>
; '(#hash((cx . 0) (cy . 0) (t1 . 0) (t2 . 0) (x . 0) (y . 1) (z . 0))
;   #hash((cx . 1) (cy . 0) (t1 . 0) (t2 . 0) (x . 0) (y . 1) (z . 0)))

;> (simulate sel-ckt sel-config1x 5) =>
;'(#hash((s . 1) (sc . 0) (u0 . 0) (u1 . 0) (v0 . 0) (v1 . 0)
;    (x0 . 1)  (x1 . 0) (y0 . 0) (y1 . 1) (z0 . 0) (z1 . 0))
;  #hash((s . 1) (sc . 0) (u0 . 0) (u1 . 0) (v0 . 0) (v1 . 1)
;    (x0 . 1) (x1 . 0) (y0 . 0) (y1 . 1) (z0 . 0) (z1 . 0))
;  #hash((s . 1) (sc . 0) (u0 . 0) (u1 . 0) (v0 . 0) (v1 . 1)
;    (x0 . 1) (x1 . 0) (y0 . 0) (y1 . 1) (z0 . 0) (z1 . 1)))

;> (simulate latch-ckt latch-config2x 3) =>
; '(#hash((q . 1) (u . 0) (x . 0) (y . 1)))

;> (simulate eq2-ckt (init-config eq2-ckt '(0 1)) 5) =>
;'(#hash((w . 0) (x . 0) (y . 1) (z . 0))
;  #hash((w . 1) (x . 0) (y . 1) (z . 1))
;  #hash((w . 1) (x . 0) (y . 1) (z . 0)))

;**********************************************************

(define (simulate circuit config n)
  (define (loop current-config configs iterations)
    (if (or (>= iterations n) (stable? circuit current-config))
        (reverse configs)
        (let ((next-cfg (next-config circuit current-config)))
          (loop next-cfg (cons next-cfg configs) (+ iterations 1)))))
  (loop config (list config) 0))





;**********************************************************
; ** problem 8 ** (10 points)
; Write a procedure

; (final-config circuit config)

; that takes a circuit and a configuration config for the circuit.  If
; the circuit would eventually reach a stable configuration from
; config, then (final-config circuit config) returns the stable
; configuration of the circuit that would be reached.

; Otherwise, (final-config circuit config) returns the symbol 'none.

; Examples

; (final-config clock-ckt (make-hash '((z . 0)))) => 'none

; (final-config eq1-ckt
;	      (make-hash '((x . 1) (y . 1) (z . 0) (cx . 0)
;			   (cy . 0) (t1 . 0) (t2 . 0)))) =>
; (make-hash '((x . 1) (y . 1) (z . 1) (cx . 0)
;    	     (cy . 0) (t1 . 1) (t2 . 0))))

; (final-config sel-ckt 
;	      (make-hash '((x1 . 0) (x0 . 0) (y1 . 1) (y0 . 0)
;			   (s . 0) (z1 . 1) (z0 . 1) (sc . 0)
;			   (u1 . 1) (v1 . 1) (u0 . 0) (v0 . 1)))) =>
;(make-hash '((x1 . 0) (x0 . 0) (y1 . 1) (y0 . 0)
;	     (s . 0) (z1 . 0) (z0 . 0) (sc . 1)
;	     (u1 . 0) (v1 . 0) (u0 . 0) (v0 . 0)))
      
; (final-config latch-ckt (make-hash '((x . 1) (y . 1) (q . 0) (u . 0)))) =>
;       'none

; (final-config latch-ckt (make-hash '((x . 1) (y . 1) (q . 0) (u . 1)))) =>
; (make-hash '((x . 1) (y . 1) (q . 0) (u . 1))))


;**********************************************************

(define (final-config circuit config)
  (define (simulate current-config seen-cfgs iterations)
    (if (stable? circuit current-config)
        current-config  ;if stable return it
        (let ((next-cfg (next-config circuit current-config)))
          (if (or (member next-cfg seen-cfgs)
                  (> iterations 100)) ; Arbitrary limit to prevent infinite loops(exit loop and return none)
              'none
              (simulate next-cfg (cons next-cfg seen-cfgs) (+ iterations 1))))))
  (simulate config '() 0))


;**********************************************************
; ** problem 9 ** (10 points)
; Define a 4-bit ripple-carry adder circuit as described in lecture
; using the circuit representation developed above.

; Please name it: add-ckt

; Its inputs are x3, x2, x1, x0, y3, y2, y1, y0  (in order)
; Its outputs are z4, z3, z2, z1, z0 (in order)
; What it computes is the sum of the two 4-bit binary numbers
; represented by the x's and the y's.
; For example, if the inputs are 
; x3 = 1, x2 = 0, x1 = 0, x0 = 1    (representing 9 in binary)
; y3 = 1, y2 = 1, y1 = 0, y0 = 1    (representing 13 in binary)
; then the output should be
; z4 = 1, z3 = 0, z2 = 1, z1 = 1, z0 = 0 (representing 22 in binary)

; Examples:
; (good-circuit? add-ckt) => #t
; (ckt-inputs add-ckt) => '(x3 x2 x1 x0 y3 y2 y1 y0)
; (ckt-outputs add-ckt) => '(z4 z3 z2 z1 z0)
; (output-values add-ckt (final-config add-ckt (init-config add-ckt '(1 0 0 1 1 1 0 1)))) => '(1 0 1 1 0)
; (output-values add-ckt (final-config add-ckt (init-config add-ckt '(0 1 1 1 0 1 1 0)))) => '(0 1 1 0 1)

; For the last two tests, your procedures output-values, final-config,
; init-config must be working.

; You may construct the circuit entirely by hand,
; or you may choose to write procedures to construct your circuit.
;**********************************************************

;; By hand

(define add-ckt
  (ckt '(x3 x2 x1 x0 y3 y2 y1 y0)
       '(z4 z3 z2 z1 z0)
       (list
        ;first bit add
        (gate 'xor '(x0 y0) 'z0)
        (gate 'and '(x0 y0) 'co0)

        ;second bit add
        (gate 'xor '(x1 y1) 'a1)
        (gate 'xor '(a1 co0) 'z1)
        (gate 'and '(y1 co0) 'b1)
        (gate 'and '(x1 co0) 'c1)
        (gate 'and '(x1 y1) 'd1)
        (gate 'or '(b1 c1) 'e1)
        (gate 'or '(e1 d1) 'co1)
        ;third bit add
        (gate 'xor '(x2 y2) 'a2)
        (gate 'xor '(a2 co1) 'z2)
        (gate 'and '(y2 co1) 'b2)
        (gate 'and '(x2 co1) 'c2)
        (gate 'and '(x2 y2) 'd2)
        (gate 'or '(b2 c2) 'e2)
        (gate 'or '(e2 d2) 'co2)
        ;fourth bit add
        (gate 'xor '(x3 y3) 'a3)
        (gate 'xor '(a3 co2) 'z3)
        (gate 'and '(y3 co2) 'b3)
        (gate 'and '(x3 co2) 'c3)
        (gate 'and '(x3 y3) 'd3)
        (gate 'or '(b3 c3) 'e3)
        (gate 'or '(e3 d3) 'co3)

        (gate 'and '(co3 co3) 'z4))))


;**********************************************************
; ** problem 10 ** (5 points)

; Define a D-flipflop as described in lecture, using the given
; representation of circuits.  Please name it: dff-ckt.

; It has inputs:  s, d (in order) and outputs q, qc (in order)

; Examples
; (good-circuit? dff-ckt) => #t
; (ckt-inputs dff-ckt) => '(s d)
; (ckt-outputs dff-ckt) => '(q qc)
;> (output-values dff-ckt (final-config dff-ckt (init-config dff-ckt '(1 0))))
;'(0 1)
;> (output-values dff-ckt (final-config dff-ckt (init-config dff-ckt '(1 1))))
;'(1 0)
;**********************************************************

(define dff-ckt
  (ckt '(s d)
       '(q qc)
       (list
        (gate 'nand '(d s) 'n1)
        (gate 'nand '(s n1) 'n2)
        (gate 'nand '(n1 n3) 'q)    
        (gate 'nand '(n2 q) 'n3)  
        (gate 'nand '(q q) 'qc))))


;**********************************************************
; ** problem 11 ** (5 points)

; Design a circuit using the given representation that has no inputs
; and one output 't.  When the circuit is started in the initial (all
; zero) configuration, after a few configurations, the output is 1 for
; one step, then 0 for 4 steps, then 1 for one step, then 0 for 4
; steps, then 1 for one step, and so on.

; Please name your circuit timing-ckt.

; Examples
; (good-circuit? timing-ckt) => #t
; (ckt-inputs timing-ckt) => '()
; (ckt-outputs timing-ckt) => '(t)
;> (map (lambda (config) (output-values timing-ckt config)) (simulate timing-ckt (init-config timing-ckt '()) 20))
;'((0) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1))
;**********************************************************

(define timing-ckt empty)

;**********************************************************
; ** problem 12 ** (5 points)

; Design a circuit using the given representation that has no inputs
; and one output 't.  When the circuit is started in the initial (all
; zero) configuration, after a few configurations, the output is 1 for
; one step, then 0 forever after.

; Please name your circuit one-one-ckt.

; Examples
; (good-circuit? one-one-ckt) => #t
; (ckt-inputs one-one-ckt) => '()
; (ckt-outputs one-one-ckt) => '(t)
;> (map (lambda (config) (output-values one-one-ckt config)) (simulate one-one-ckt (init-config one-one-ckt '()) 20))
;'((0) (0) (1) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) (0) )
;**********************************************************

(define one-one-ckt
  (ckt '()  
       '(t) 
       (list
        (gate 'not '(p) 'p-not)
        (gate 'and '(p p-not) 'q)
        (gate 'nand '(p-not q) 'p)
        (gate 'not '(r) 'r)
        (gate 'and '(p p-not) 't))))

; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

(define (testold name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    '***OK***
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((OK (if (procedure? expected)
		    	(expected got)
			(equal? got expected)))
		(prefix (if OK
			    '***OK***
			    '***X***)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))

(define (runtests)
  (list

   (test 'hours hours (lambda (x) (> x 0)))

   (test 'good-gate? (good-gate? gate1) #t)
   (test 'good-gate? (good-gate? gate2) #t)
   (test 'good-gate? (good-gate? gate3) #t)
   (test 'good-gate? (good-gate? gate4) #t)
   
   (test 'good-gate? (good-gate? (gate 'not 'x 'y)) #f)
   (test 'good-gate? (good-gate? (gate 'nor '("x" "y") "z")) #f)
   (test 'good-gate? (good-gate? (gate 'and '(1 2) 3)) #f)
   (test 'good-gate? (good-gate? (gate 'equal '(x y) 'z)) #f)
   (test 'good-gate? (good-gate? (gate 'or '(w x y) 'z)) #f)

   (test 'good-circuit? (good-circuit? sel-ckt) #t)
   (test 'good-circuit? (good-circuit? eq1-ckt) #t)
   (test 'good-circuit? (good-circuit? eq2-ckt) #t)
   (test 'good-circuit? (good-circuit? latch-ckt) #t)
   (test 'good-circuit? (good-circuit? seq-or-ckt) #t)
   (test 'good-circuit? (good-circuit? clock-ckt) #t)
   
   (test 'good-circuit? (good-circuit? 'hi) #f)
   (test 'good-circuit? (good-circuit? (ckt '() '() '())) #t)
   (test 'good-circuit? (good-circuit? (ckt '(x y) '(z) (list (gate 'and '(x y) 'x) (gate 'or '(x y) 'z)))) #f)
   (test 'good-circuit? (good-circuit? (ckt '(x y) '(z) (list (gate 'nor '(x y) 'z) (gate 'nand '(x y) 'z)))) #f)
   (test 'good-circuit? (good-circuit? (ckt '(x y) '(u z) (list (gate 'or '(x y) 'z)))) #f)
   
   
   (test 'all-wires (all-wires eq1-ckt) '(x y z cx cy t1 t2))
   (test 'all-wires (all-wires sel-ckt) '(x1 x0 y1 y0 s z1 z0 sc u1 v1 u0 v0))
   (test 'find-gate (find-gate 't2 eq1-ckt) (gate 'and '(cx cy) 't2))
   (test 'find-gate (find-gate 'w eq2-ckt) (gate 'xor '(x y) 'w))
   (test 'find-gate (find-gate 'y sel-ckt) #f)
   
   "
(test 'ha-ckt (good-circuit? ha-ckt) #t)
(test 'fa-ckt (good-circuit? fa-ckt) #t)
(test 'ha-ckt (ckt-inputs ha-ckt) '(x y))
(test 'ha-ckt (ckt-outputs ha-ckt) '(z co))
(test 'fa-ckt (ckt-inputs fa-ckt) '(x y ci))
(test 'fa-ckt (ckt-outputs fa-ckt) '(z co))

(test 'ha-ckt (output-values ha-ckt 
			     (final-config ha-ckt 
					   (init-config ha-ckt '(1 1)))) 
      '(0 1))
(test 'fa-ckt (output-values fa-ckt (final-config fa-ckt (init-config fa-ckt '(1 1 1)))) '(1 1))
"

   (test 'next-value (next-value 'cx eq1-ckt eq1-config1x) 1)
   (test 'next-value (next-value 't2 eq1-ckt eq1-config1x) 0)
   (test 'next-value (next-value 'z eq1-ckt eq1-config2x) 0)
   (test 'next-value (next-value 'x0 sel-ckt sel-config1x) 1)
   (test 'next-value (next-value 'v1 sel-ckt sel-config1x) 1)
   (test 'next-value (next-value 'v0 sel-ckt sel-config2x) 0)


   (test 'next-config (next-config eq1-ckt eq1-config1x)
	 (make-hash '((x . 0) (y . 1) (z . 0) (cx . 1) (cy . 0) (t1
								 . 0) (t2 . 0))))

   (test 'next-config (next-config eq1-ckt eq1-config2x)
	 (make-hash '((x . 0) (y . 0) (z . 0) (cx . 1) (cy . 1) (t1 . 0) (t2 . 1))))

   (test 'next-config (next-config sel-ckt sel-config1x)
	 (make-hash '((x1 . 0) (x0 . 1) (y1 . 1) (y0 . 0) (s . 1) (z1 . 0) (z0 . 0)
		      (sc . 0) (u1 . 0) (v1 . 1) (u0 . 0) (v0 . 0))))

   (test 'next-config (next-config sel-ckt (next-config sel-ckt sel-config1x))
	 (make-hash '((x1 . 0) (x0 . 1) (y1 . 1) (y0 . 0) (s . 1) (z1 . 1) (z0 . 0)
		      (sc . 0) (u1 . 0) (v1 . 1) (u0 . 0) (v0 . 0))))

   (test 'next-config (next-config latch-ckt latch-config1x)
	 (make-hash '((x . 0) (y . 0) (q . 1) (u . 1))))
   
   
   (test 'next-config (next-config latch-ckt latch-config2x)
	 (make-hash '((x . 0) (y . 1) (q . 1) (u . 0))))



   (test 'stable? (stable? eq1-ckt (make-hash '((x . 0) (y . 0) (z . 1)
						(cx . 1) (cy . 1) (t1 . 0) (t2 . 1)))) #t)

   (test 'stable? (stable? eq1-ckt (make-hash '((x . 0) (y . 0) (z . 0)
						(cx . 1) (cy . 0) (t1 . 1) (t2 . 0)))) #f)

   (test 'all-stable-configs (all-stable-configs eq2-ckt)
	 (list
	  (make-hash '((x . 0) (y . 0) (z . 1) (w . 0)))
	  (make-hash '((x . 0) (y . 1) (z . 0) (w . 1)))
	  (make-hash '((x . 1) (y . 0) (z . 0) (w . 1)))
	  (make-hash '((x . 1) (y . 1) (z . 1) (w . 0)))))
   
   (test 'all-stable-configs (all-stable-configs seq-or-ckt)
	 (list
	  (make-hash '((x . 0) (z . 0)))
	  (make-hash '((x . 0) (z . 1)))
	  (make-hash '((x . 1) (z . 1)))))

   (test 'output-values (output-values eq1-ckt eq1-config2x) '(0))
   (test 'output-values (output-values latch-ckt latch-config2x) '(1 0))

   (test 'init-config (init-config eq1-ckt '(1 0))
	 (make-hash '((x . 1) (y . 0) (z . 0) (cx . 0) (cy . 0) (t1 . 0) (t2 . 0))))
   
   (test 'init-config (init-config clock-ckt '()) (make-hash '((z . 0))))
   
   (test 'simulate (simulate clock-ckt (make-hash '((z . 0))) 4)
	 (list (make-hash '((z . 0)))
	       (make-hash '((z . 1)))
	       (make-hash '((z . 0)))
	       (make-hash '((z . 1)))
	       (make-hash '((z . 0)))))
   
   (test 'simulate (simulate eq1-ckt eq1-config1x 5)
	 (list
	  (make-hash '((x . 0) (y . 1) (z . 0) (cx . 0) (cy . 0) (t1 . 0) (t2 . 0)))
	  (make-hash '((x . 0) (y . 1) (z . 0) (cx . 1) (cy . 0) (t1 . 0) (t2 . 0)))))
   
   (test 'simulate (simulate sel-ckt sel-config1x 5)
	 (list
	  (make-hash '((x1 . 0) (x0 . 1) (y1 . 1) (y0 . 0) (s . 1) (z1 . 0)
		       (z0 . 0) (sc . 0) (u1 . 0) (v1 . 0) (u0 . 0) (v0 . 0)))
	  (make-hash '((x1 . 0) (x0 . 1) (y1 . 1) (y0 . 0) (s . 1) (z1 . 0)
		       (z0 . 0) (sc . 0) (u1 . 0) (v1 . 1) (u0 . 0) (v0 . 0)))
	  (make-hash '((x1 . 0) (x0 . 1) (y1 . 1) (y0 . 0) (s . 1) (z1 . 1)
		       (z0 . 0) (sc . 0) (u1 . 0) (v1 . 1) (u0 . 0) (v0 . 0)))))

   (test 'simulate (simulate latch-ckt latch-config2x 3)
	 (list
	  (make-hash '((x . 0) (y . 1) (q . 1) (u . 0)))))
   
   (test 'simulate (simulate eq2-ckt (init-config eq2-ckt '(0 1)) 5)
	 (list
	  (make-hash '((x . 0) (y . 1) (z . 0) (w . 0)))
	  (make-hash '((x . 0) (y . 1) (z . 1) (w . 1)))
	  (make-hash '((x . 0) (y . 1) (z . 0) (w . 1)))))
   
   (test 'final-config (final-config clock-ckt (make-hash '((z . 0))))
	 'none)
   
   (test 'final-config (final-config eq1-ckt
				     (make-hash '((x . 1) (y . 1) (z . 0) (cx . 0)
						  (cy . 0) (t1 . 0) (t2 . 0))))
	 (make-hash '((x . 1) (y . 1) (z . 1) (cx . 0)
		      (cy . 0) (t1 . 1) (t2 . 0))))
   
   (test 'final-config (final-config
			sel-ckt 
			(make-hash '((x1 . 0) (x0 . 0) (y1 . 1) (y0 . 0)
				     (s . 0) (z1 . 1) (z0 . 1) (sc . 0)
				     (u1 . 1) (v1 . 1) (u0 . 0) (v0 . 1))))
	 (make-hash '((x1 . 0) (x0 . 0) (y1 . 1) (y0 . 0)
		      (s . 0) (z1 . 0) (z0 . 0) (sc . 1)
		      (u1 . 0) (v1 . 0) (u0 . 0) (v0 . 0))))
   
   (test 'final-config (final-config latch-ckt
				     (make-hash '((x . 1) (y . 1) (q . 0) (u . 0))))
	 'none)

   (test 'final-config (final-config
			latch-ckt
			(make-hash '((x . 1) (y . 1) (q . 0) (u . 1))))
	 (make-hash '((x . 1) (y . 1) (q . 0) (u . 1))))
   ))

;**************  end of hw # 5  ************************************
