#lang racket

;; Note: you will need to update the provide statement once James
;; blesses the autograder.

(provide hours
         entry entry? entry-key entry-value
         ram-read ram-write diff-rams
         extract bits->int int->bits int->bits-width
         conf conf? conf-cpu conf-ram
         diff-configs incr-pc do-load do-store
         do-add do-sub
         do-input do-output
         do-jump do-skipzero do-skippos do-skiperr
         do-loadi do-storei do-shift
	 do-and do-xor
         next-config
         init-config symbol-table assemble
         simulate encrypt-prog reverse-prog
	 power-prog)


;************************************************************
; CS 201 HW #6  DUE Wednesday April 17th at 11:59 pm, 
; via gradescope
;************************************************************
; Name: Ronald Milgo
; Email address: ronald.milgo@yale.edu
;************************************************************

; Computer science topics: TC-201 assembler and simulator,
; assembly language programs for encrypt, reverse, and power.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 19)

; ********************************************************
; ** problem 00 ** (1 fairly easy point)

; Below is a UNIX transcript with one command replaced by XXXX

(define transcript "

bash-4.4$ pwd
/home/accts/sbs5/cs201/www/Fall_2019/lectures/test
bash-4.4$ ls
file  file2
bash-4.4$ xxxx
file: ASCII text
bash-4.4$ cat file
hello world
")

; define xxxx below to be the correct UNIX command.

(define xxxx "file file")


;************************************************************

; A table is a list of entries, where each entry has two fields: key and value.
; The constructor for entries is entry, the type predicate is entry?, and the
; two selectors are entry-key and entry-value.

(struct entry (key value) #:transparent)

; Random access memory (RAM)

; We represent the contents of a memory register as
; a list of 16 bits, each either 0 or 1.
; The contents of the RAM are represented as a list giving
; the contents of memory register 0, memory register 1,
; and so on, up to some address n, where n is at most 4095.
; Those memory registers whose contents are not explicitly
; listed are assumed to contain 16 zeroes.

; Examples of RAMs.

(define ram-ex1
  '((0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1)
    (0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  1 0 1 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)))
 
(define ram-ex2
  '((0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1)
    (0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 1)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 1 0  0 1 0 0)))

;************************************************************
; ** problem 1 ** (8 points)
; Write three procedures

; (ram-read address ram)
; (ram-write address contents ram)
; (diff-rams ram1 ram2)

; (ram-read address ram)
; takes a memory address and a ram
; and returns a list of 16 bits giving the contents
; of the memory register in ram with the given address.

; (diff-rams ram1 ram2)
; takes two RAMs and returns a list indicating the memory registers 
; which have different contents in the two RAMs.
; The format of the list is a list of triples giving
; a memory address, the contents of that memory register
; in ram1, and the contents of that memory register
; in ram2.  The addresses should be in increasing order.

; (ram-write address contents ram)
; takes a memory address (address), a list of 16 bits (contents) and a ram,
; and returns a ram representing the result of copying the contents 
; into the memory register of ram specified by the memory address.

; Examples

;> (ram-read 0 ram-ex1)
;'(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)

;> (ram-read 6 ram-ex2)
;'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

;> (diff-rams ram-ex1 ram-ex2)
;'((1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;  (3 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0)))

;> (diff-rams '() '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
;'()

;> (diff-rams ram-ex1 (ram-write 2 '(0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1) ram-ex1))
;'((2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1)))

;> (diff-rams ram-ex2 (ram-write 5 '(1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0) ram-ex2))
;'((5 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0)))

;> (diff-rams ram-ex1 (ram-write 1 '(0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1) ram-ex1))
;'((1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1)))

;************************************************************

(define (ram-read address ram)
  (if (or (< address 0) (>= address (length ram)))
      (make-list 16 0)
      (list-ref ram address)))

(define (ram-write address contents ram)
  (let ((len (length ram)))
    (cond ((< address 0) ram)  
          ((< address len)
           (append (take ram address)
                   (list contents)
                   (drop ram (+ 1 address))))
          (else  
           (append ram
                   (make-list (- address len) (make-list 16 0))  
                   (list contents))))))
    
(define (diff-rams ram1 ram2)
  (define (zeroes? lst)
    (andmap zero? lst))

 (let loop ((index 0) (diffs '()))
   (cond ((and (>= index (length ram1)) (>= index (length ram2))) (reverse diffs))
         ((>= index (length ram2))
          (let ((read1 (ram-read index ram1)))
            (if (zeroes? read1)
                (loop (+ index 1) diffs)
                (loop (+ index 1) (cons (list index read1 (make-list 16 0) diffs))))))
         ((>= index (length ram1))
          (let ((read2 (ram-read index ram2)))
            (if (zeroes? read2)
                (loop (+ index 1) diffs)
                (loop (+ index 1) (cons (list index (make-list 16 0) read2) diffs)))))
         (else
          (let ((read1 (ram-read index ram1))
                (read2 (ram-read index ram2)))
            (if (or (equal? read1 read2) (and (zeroes? read1) (zeroes? read2)))
                (loop (+ index 1) diffs)
                (loop (+ index 1) (cons (list index read1 read2) diffs))))))))

;************************************************************
; ** problem 2 ** (10 points)
; Write four procedures:

; (extract i j lst)
; (bits->int lst) 
; (int->bits n)
; (int->bits-width n w)

; (extract i j lst) 
; takes nonnegative integers i and j and a list lst
; and returns the list of elements of lst indexed i through j.
; You may assume i and j are at least 0 and less than the
; length of the list, and i is less than or equal to j.
; As in list-ref, list elements are indexed starting with 0.

; (bits->int lst) takes a list of bits lst
; and returns the value of the nonnegative number 
; represented in binary by those digits.

; (int->bits n) takes a nonnegative integer n
; and returns the list of bits representing n in 
; unsigned binary.
; Note that for 0 the answer is (0) but for
; all other numbers the answer starts with 1.

; (int->bits-width n w) takes a nonnegative integer n
; and returns a list of w bits representing n in 
; unsigned binary.
; If n cannot be correctly represented in binary using
; w bits, the string "field too small" should be returned.

; Examples

;> (extract 1 3 '(a b c d e))
;'(b c d)

;> (extract 4 4 '(a b c d e))
;'(e)

;> (bits->int '(0))
;0

;> (bits->int '(0 0 0 1 1 0))
;6

;> (int->bits 0)
;'(0)

;> (int->bits 6)
;'(1 1 0)

;> (int->bits-width 14 8)
;'(0 0 0 0 1 1 1 0)

;> (int->bits-width 14 3)
;"field too small"

;************************************************************

(define (extract i j lst)
  (let ((tail (list-tail lst i)))
    (take tail (+ 1 (- j i)))))

(define (bits->int lst)
  (if (null? lst)
      0
      (+ (* (car lst) (expt 2 (- (length lst) 1))) (bits->int (cdr lst)))))

(define (int->bits n)
  (if (= n 0)
      '(0) 
      (let ((bits (reverse (int->bits-helper n))))
        (if (null? bits)
            '(0)
            bits))))

(define (int->bits-helper n)
  (if (= n 0)
      '()
      (cons (remainder n 2) (int->bits-helper (quotient n 2)))))

(define (int->bits-width n w)
  (let ((bits (int->bits n)))
    (if (< w  (length bits))
        "field too small"
        (append (make-list (- w (length bits)) 0) bits))))
        

;************************************************************
; Next we develop a simulator for the TC-201

; For the TC-201 Central Processing Unit (CPU), the contents of the registers 
; are represented by a table with entries giving the contents of the CPU 
; registers ** in this order **.

; the accumulator (acc)
; the program counter (pc)
; the run flag (rf)
; the arithmetic error bit (aeb)

; Each entry is a list containing 
; a symbol (one of 'acc, 'pc, 'rf, 'aeb)
; a list of bits of the correct length,
; namely, 16 bits for the acc, 12 bit for
; the pc, and 1 bit each for rf and aeb.

; Examples

(define cpu-ex1 
  (list
   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
   (entry 'pc '(0 0 0 0 0 0 0 0 0 1 1 1))
   (entry 'rf '(1))
   (entry 'aeb '(0))))

(define cpu-ex2 
  (list
   (entry 'acc '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
   (entry 'pc '(0 0 0 0 0 0 0 0 0 1 1 1))
   (entry 'rf '(1))
   (entry 'aeb '(1))))

; A configuration of the TC-201 is a struct with two fields:
; (1) the contents of the CPU registers, in the above format,
; (2) the contents of the RAM, in the format of problem 1.

(struct conf (cpu ram) #:transparent)

; Note that the constructor is conf, the type-predicate
; is conf?, and the selectors are conf-cpu, conf-ram.

; Examples

(define config-ex1 (conf cpu-ex1 ram-ex1))
(define config-ex2 (conf cpu-ex2 ram-ex2))

;************************************************************
; ** problem 3 ** (10 points)
; Write four procedures

; (diff-configs config1 config2)
; (incr-pc n config)
; (do-load address config)
; (do-store address config)

; (diff-configs config1 config2)
; takes two configurations and returns a list showing where they differ, 
; as a list of triples, giving the name (or address) of the
; register, the contents in config1 and the contents in config2.  
; The order should be CPU registers first (in order: acc, pc, rf, aeb) 
; and then memory registers in increasing order of addresses.

; (incr-pc n config)
; takes a nonnegative integer n and a TC-201 configuration config
; and returns the TC-201 configuration that is obtained by adding n 
; to the value of pc.  Note that the sum should be taken modulo 4096.  
; (Racket has a modulo procedure.)

; (do-load address config)
; takes a memory address and a TC-201 configuration, and returns the TC-201 
; configuration that is obtained by copying the contents
; of the given memory address into the accumulator.
; The values of all other registers (including the pc) are unchanged.

; (do-store address config)
; takes a memory address and a TC-201 configuration, and returns the TC-201 
; configuration that is obtained by copying the contents of the accumulator 
; into the given memory address.
; The values of all other registers (including the pc) are unchanged.

; Examples

;> (diff-configs config-ex1 config-ex2)
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
;  (aeb (0) (1))
;  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;  (3 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0)))

; The first result is shown in full -- you may produce an equivalent
; configuration.  Subsequent results are shown using diff-configs.

;> (incr-pc 1 config-ex1)
;(conf
; (list
;  (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;  (entry 'pc '(0 0 0 0 0 0 0 0 1 0 0 0))
;  (entry 'rf '(1))
;  (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;> (diff-configs config-ex2 (incr-pc 4090 config-ex2))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1)))

;> (diff-configs config-ex1 (do-load 1 config-ex1))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)))

;> (diff-configs config-ex2 (do-load 12 config-ex2))
;'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;> (diff-configs config-ex1 (do-store 5 config-ex1))
;'((5 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)))

;>  (diff-configs config-ex2 (do-store 0 config-ex2))
;'((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)))

;************************************************************

(define (lookup ent cpu)
  (cond ((null? cpu) #f)
        ((equal? ent (entry-key (car cpu))) (entry-value (car cpu)))
        (else
          (lookup ent (cdr cpu)))))

(define (diff-configs config1 config2)
  (let* ((cpu1 (conf-cpu config1))
         (cpu2 (conf-cpu config2))
         (ram1 (conf-ram config1))
         (ram2 (conf-ram config2))
         (cpu-diffs (diff-cpu cpu1 cpu2))
         (ram-diffs (diff-rams ram1 ram2)))
    (append cpu-diffs ram-diffs)))

(define (diff-cpu cpu1 cpu2)
  (apply append
         (map (lambda (ent)
                (let* ((val1 (lookup ent cpu1))
                       (val2 (lookup ent cpu2)))
                  (if (equal? val1 val2)
                      '()
                      (list (list ent val1 val2)))))
              '(acc pc rf aeb))))
                       


(define (incr-pc n config)
  (let* ((cpu (conf-cpu config))
         (ram (conf-ram config))
         (bits (lookup 'pc cpu))
         (int (bits->int bits))
         (new_int (modulo (+ int n) 4096))
         (new_bits (int->bits-width new_int 12))
         (new_cpu (map (lambda (ent)
                         (if (equal? 'pc (entry-key ent))
                             (entry 'pc new_bits)
                             ent))
                       cpu)))
    (conf new_cpu ram)))


(define (do-load address config)
  (let* ((cpu (conf-cpu config))
         (ram (conf-ram config))
         (cont (ram-read address ram))
         (new_cpu (map (lambda (ent)
                    (if (equal? 'acc (entry-key ent))
                        (entry 'acc cont)
                        ent))
                  cpu)))
    (conf new_cpu ram)))

(define (do-store address config)
  (let* ((cpu (conf-cpu config))
         (ram (conf-ram config))
         (acc-cont (lookup 'acc cpu))
         (new_ram (ram-write address acc-cont ram)))
    (conf cpu new_ram)))

;************************************************************
; ** problem 4 ** (10 points)
; Write two procedures

; (do-add address config)
; (do-sub address config)

; (do-add address config)
; takes a memory address and a TC-201 configuration
; and returns a TC-201 configuration in which
; the contents of the memory register addressed has
; been added to the contents of the accumulator.

; (do-sub address config) is similar, except that the
; contents of the memory register addressed has
; been subtracted from the contents of the accumulator.

; Note that if the result is zero, the answer should
; be +0, not -0.

; If the result can be correctly represented in 16-bit sign/magnitude
; then the arithmetic error bit (aeb) should also be set to 0.

; If the result cannot be correctly represented in 16-bit sign/magnitude
; then the arithmetic error bit (aeb) should also be set to 1.
; In this case, the result in the accumulator should be 
; 16 zeroes, representing +0.

; The contents of registers other than the accumulator and the
; arithmetic error bit should be unchanged.

; Examples

;> (diff-configs config-ex1 (do-add 3 config-ex1))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1)))

;> (diff-configs config-ex2 (do-add 3 config-ex2))
;'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1))
;  (aeb (1) (0)))

;> (diff-configs config-ex1 (do-sub 3 config-ex1))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1)))

;> (diff-configs config-ex2 (do-sub 3 config-ex2))
;'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (1 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1))
;  (aeb (1) (0)))

;>  (let ((config (conf cpu-ex1 '((0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))) (diff-configs config (do-add 0 config)))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (aeb (0) (1)))

;************************************************************
(define (sm-int->bits n)
  (if (negative? n)
      (append '(1) (int->bits-width (abs n) 15))
      (int->bits-width n 16)))

(define (sm-bits->int lst)
  (if (equal? (car lst) 1)
      (- (bits->int (cdr lst)))
      (bits->int lst)))

(define (overflow? n)
  (if (> (abs n) (expt 2 15))
      #t
      #f))

(define (update reg new cpu)
  (map (lambda (ent)
         (if (equal? reg (entry-key ent))
             (entry reg new)
             ent))
       cpu))

(define (do-add address config)
  (let* ((cpu (conf-cpu config))
         (ram (conf-ram config))
         (addcont (ram-read address ram))
         (int-addcont (sm-bits->int addcont))
         (acccont (lookup 'acc cpu))
         (int-acccont (sm-bits->int acccont))
         (sum (+ int-addcont int-acccont))
         (overflow (overflow? sum))
         (sum-bits (sm-int->bits sum))
         (updated-cpu (if overflow
                          (update 'acc (make-list 16 0) (update 'aeb '(1) cpu))
                          (update 'acc sum-bits (update 'aeb '(0) cpu)))))
    (conf updated-cpu ram)))
         
                               
                                     
(define (do-sub address config)
  (let* ((cpu (conf-cpu config))
         (ram (conf-ram config))
         (subcont (ram-read address ram))
         (int-subcont (sm-bits->int subcont))
         (acccont (lookup 'acc cpu))
         (int-acccont (sm-bits->int acccont))
         (sub (- int-acccont int-subcont ))
         (overflow (overflow? sub))
         (sub-bits (sm-int->bits sub))
         (updated-cpu (if overflow
                           (update 'acc (make-list 16 0) (update 'aeb '(1) cpu))
                           (update 'acc sub-bits (update 'aeb '(0) cpu)))))
         
    (conf updated-cpu ram)))

;************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (do-input config)
; (do-output config)

; Each takes a TC-201 configuration and performs the appropriate action 
; (reading a number from the user or writing a number out to the user)
; and *returns* the resulting TC-201 configuration.

; For input, the new configuration has the value read in the 
; accumulator, and all other registers unchanged.
; To read in a value, you may use the following
; let construct:
; (let ((value (begin (display "input = ") (read)))) ...)

; To ensure the number typed by the user is in the correct range, 
; you may take its remainder on division by 2^(15).

; For output, the new configuration is returned *unchanged*.  
; If the integer value from the accumulator is in 
; value-from-accumulator, then the output to the user can be 
; produced by:

; (display "output = ")
; (display value-from-accumulator)
; (newline)

; Examples (these show how the interaction looks)

; The lines input = .. and output = .. show the interaction between 
; TC-201 and user.  The TC-201 configuration shows the value
; returned by the procedure.

;> (diff-configs config-ex1 (do-input config-ex1))
;input = 22
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0)))

;> (diff-configs config-ex1 (do-output config-ex1))
;output = 15
;'()

;************************************************************

(define (do-input config)
  (let* ((value (begin (display "input = ") (read)))
         (update-value (remainder value (expt 2 15)))
         (new-acc (sm-int->bits update-value)) 
         (cpu (conf-cpu config))
         (ram (conf-ram config))
         (updated-cpu (update 'acc new-acc cpu)))  
    (conf updated-cpu ram)))  

(define (do-output config)
  (let* ((acc-bits (lookup 'acc (conf-cpu config)))
         (acc-int (sm-bits->int acc-bits))) 
    (display "output = ")
    (display acc-int)
    (newline)
    config))

;************************************************************
; ** problem 6 ** (10 points)
; Write four procedures

; (do-jump address config)
; (do-skipzero config)
; (do-skippos config)
; (do-skiperr config)


; (do-jump address config)
; takes a memory address and a TC-201 configuration, and
; returns a TC-201 configuration in which the program counter
; (pc) is set to the given address.  All other registers are
; unaffected.

; (do-skipzero config)
; takes a TC-201 configuration config and returns
; a TC-201 configuration in which the program counter (pc)
; is increased by 2 if the accumulator contains +0 or -0,
; and is increased by 1 otherwise.  All registers other than
; the pc are unaffected.

; (do-skippos config)
; takes a TC-201 configuration config and returns
; a TC-201 configuration in which the program counter (pc)
; is increased by 2 if the accumulator contains a nonzero
; positive number, and is increased by 1 otherwise.  
; All registers other than the pc are unaffected.

; (do-skiperr config)
; takes a TC-201 configuration config and returns
; a TC-201 configuration in which the program counter (pc)
; is increased by 2 if the arithmetic error bit contains 1
; and is increased by 1 if the arithmetic error bit contains 0.
; In either case, in the new configuration, the arithmetic
; error bit is set to 0.
; All registers other than the pc and the aeb are unaffected.

; Examples

;> (diff-configs config-ex1 (do-jump 5 config-ex1))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 0 1 0 1)))

;> (diff-configs config-ex2 (do-skipzero config-ex2))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 0)))

;> (diff-configs config-ex1 (do-skippos config-ex1))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 1)))

;> (diff-configs config-ex2 (do-skiperr config-ex2))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 1)) (aeb (1) (0)))

;************************************************************

(define (do-jump address config)
 (let* ((abs-address (abs address))
        (cpu (conf-cpu config))
        (ram (conf-ram config))
        (bits (int->bits-width abs-address 12)))
   (conf (update 'pc bits cpu) ram)))
        

(define (do-skipzero config)
 (let* ((cpu (conf-cpu config))
        (acc (lookup 'acc cpu))
        (int (sm-bits->int acc)))
   (if (or (equal? int +0) (equal? int -0))
       (incr-pc 2 config)
       (incr-pc 1 config))))
       
        

(define (do-skippos config)
 (let* ((cpu (conf-cpu config))
        (acc (lookup 'acc cpu))
        (int (sm-bits->int acc)))
   (if (> int 0)
       (incr-pc 2 config)
       (incr-pc 1 config))))
             

(define (do-skiperr config)
  (let* ((cpu (conf-cpu config))
         (ram (conf-ram config))
         (aeb (lookup 'aeb cpu))
         (int (bits->int aeb)))
    (if (equal? int 1)
        (conf (update 'aeb '(0) (conf-cpu (incr-pc 2 config))) ram)
        (conf (update 'aeb '(0) (conf-cpu (incr-pc 1 config))) ram))))
           
;************************************************************
; ** problem 7 ** (10 points)
; Write three procedures

; (do-loadi address config)
; (do-storei address config)
; (do-shift address config)

; (do-loadi address config)
; takes a memory address and a TC-201 configuration and returns a TC-201 
; configuration that reflects the result of doing a "load indirect" from the
; given memory address to the accumulator.
; That is, the low-order 12 bits of the contents of the memory register 
; addressed by address are extracted and used as the memory address
; from which the contents are loaded into the accumulator.
; All other registers are unaffected.

; (do-storei address config)
; takes a memory address and a TC-201 configuration and returns a TC-201 
; configuration that reflects the result of doing a "store indirect" to the
; given memory address from the accumulator.
; That is, the low-order 12 bits of the contents of the memory register 
; addressed by address are extracted and used as the memory address
; to which the contents of the accumulator are copied.
; All other registers are unaffected.

; (do-shift address config)
; takes a memory address and a TC-201 configuration and returns a TC-201 
; configuration that reflects the result of doing a shift of accumulator
; left or right by the number of bits given in the specified memory address.
; A positive number shifts the accumulator to the left.
; A negative number shifts the accumulator to the right.


; This example is useful for loadi and storei testing.

(define ram-ex3
  '((0 0 0 0  0 0 0 0  0 0 0 0  0 1 1 0)
    (1 1 1 1  0 0 0 0  0 0 0 0  0 1 0 1)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 1 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  1 0 0 0)
    (0 0 0 0  1 1 1 1  0 0 0 0  1 1 1 1)
    (0 1 0 1  0 1 0 1  0 1 0 1  0 1 0 1)
    (1 0 0 0  0 0 0 0  0 0 0 0  0 0 1 0)))

(define config-ex3 (conf cpu-ex1 ram-ex3))

; Examples

;> (diff-configs config-ex3 (do-loadi 1 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)))

;> (diff-configs config-ex3 (do-loadi 2 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1)))

;> (diff-configs config-ex3 (do-storei 1 config-ex3))
;'((5 (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)))

;> (diff-configs config-ex3 (do-storei 2 config-ex3))
;'((4 (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)))

;> (diff-configs config-ex3 (do-shift 2 config-ex3))
; '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0)))

;> (diff-configs config-ex3 (do-shift 3 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0)))

;> (diff-configs config-ex3 (do-shift 6 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)))
;************************************************************

(define (do-loadi address config)
  (let* ((cpu (conf-cpu config))
         (ram (conf-ram config))
         (addcont (ram-read address ram))
         (cut-addcont (extract 4 15 addcont))
         (int-addcont (bits->int cut-addcont))
         (new-addcont (ram-read int-addcont ram))
         (updated-cpu (update 'acc new-addcont cpu)))
    (conf updated-cpu ram)))

(define (do-storei address config)
  (let* ((cpu (conf-cpu config))
         (ram (conf-ram config))
         (addcont (ram-read address ram))
         (cut-addcont (extract 4 15 addcont))
         (int-addcont (bits->int cut-addcont))
         (acc-cont (lookup 'acc cpu))
         (update-ram (ram-write int-addcont acc-cont ram)))
    (conf cpu update-ram)))
              

(define (do-shift address config)
  (let* ((cpu (conf-cpu config))
         (ram (conf-ram config))
         (addcont (ram-read address ram))
         (int-addcont (sm-bits->int addcont))
         (acc (lookup 'acc cpu))
         (update-acc (if (> int-addcont 0)
                         (shift-left acc int-addcont)
                         (shift-right acc (abs int-addcont)))))
    (conf (update 'acc update-acc cpu) ram)))
         

(define (shift-left bits n)
  (list-tail (append bits (make-list n 0)) n))

(define (shift-right bits n)
  (take (append (make-list n 0) bits) 16))



;************************************************************
; ** problem 8 ** (10 points)
; Write two procedures

; (do-and address config)
; (do-xor address config)

; (do-and address config)
; takes a memory address and a TC-201 configuration and returns a
; TC-201 configuration that reflects the result of doing and of the
; contents of the given memory address and the accumulator.  The
; result is stored in the accumulator.  All other registers are
; unaffected.


; (do-xor address config)
; takes a memory address and a TC-201 configuration and returns a
; TC-201 configuration that reflects the result of doing an exclusive
; or of the contents of the given memory address and the accumulator.
; The result is stored in the accumulator.
; All other registers are unaffected.

; Examples:

;> (diff-configs config-ex2 (do-and 1 config-ex2))
;'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))

;> (diff-configs config-ex3 (do-and 1 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1)))

;> (diff-configs config-ex3 (do-xor 1 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 1 1 1 0 0 0 0 0 0 0 0 1 0 1 0)))

;> (diff-configs config-ex3 (do-xor 5 config-ex3))
; '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 0)))
;************************************************************

(define (and-helper acc add)
  (map (lambda (x y) (if (and (= x 1) (= y 1)) 1 0))
       acc
       add))

(define (xor-helper acc add)
  (map (lambda (x y) (if (or (and (= x 1) (= y 1)) (and (= x 0) (= y 0))) 0 1))
       acc
       add))

(define (do-and address config)
  (let* ((cpu (conf-cpu config))
         (ram (conf-ram config))
         (acc-cont (lookup 'acc cpu))
         (add-cont (ram-read address ram))
         (apply-and (and-helper acc-cont add-cont )))
    (conf (update 'acc apply-and cpu) ram)))
         
         

(define (do-xor address config)
  (let* ((cpu (conf-cpu config))
         (ram (conf-ram config))
         (acc-cont(lookup 'acc cpu))
         (add-cont (ram-read address ram))
         (apply-xor (xor-helper acc-cont add-cont)))
    (conf (update 'acc apply-xor cpu) ram)))


;************************************************************
; ** problem 9 ** (10 points)
; Write one procedure

; (next-config config)

; that takes a TC-201 configuration and returns the next TC-201 configuration,
; after one iteration of the fetch/execute cycle.

; If the run flag (rf) is 0, then the configuration config is returned unchanged,
; because the machine is halted.

; The instructions that should be implemented are:

; halt, load, store, add, sub, input, output, jump
; skipzero, skippos, skiperr, loadi, storei, shift, and, xor.

; These are opcodes 0000 through 1111, respectively.

; For a halt instruction, in the returned configuration the run flag is 0 and all
; other registers are unchanged.

; Otherwise, the program counter (pc) contains a memory address, and the TC-201 
; instruction at that location is fetched and executed, and the resulting 
; configuration is returned.  Note that all instructions result in a configuration
; being returned, even input and output.

; This example is useful for testing next-config.

(define cpu-ex4
  (list
   (entry 'acc '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
   (entry 'pc  '(0 0 0 0  0 0 0 0  0 0 0 0))
   (entry 'rf '(1))
   (entry 'aeb '(0))))

(define ram-ex4
  '((0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1)
    (0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 1  0 1 0 1)
    (1 1 1 1  1 1 1 1  0 0 0 0  0 0 0 0)))

(define config-ex4 (conf cpu-ex4 ram-ex4))

; Examples
; (Your configurations may be equivalent.)

;> (next-config config-ex4)
;(conf
; (list
;  (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;  (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 1))
;  (entry 'rf '(1))
;  (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;   (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;> (next-config (next-config config-ex4))
;(conf
; (list
;  (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;  (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
;  (entry 'rf '(1))
;  (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))

;> (next-config (next-config (next-config config-ex4)))
;(conf
; (list
;  (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;  (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
;  (entry 'rf '(0))
;  (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))

;************************************************************

(define (halt config)
  (let* ((cpu (conf-cpu config))
         (ram (conf-ram config))
         (updated-cpu (update 'rf '(0) cpu)))
    (conf updated-cpu ram)))

(define (next-config config)
  (let* ((cpu (conf-cpu config))
         (ram (conf-ram config))
         (pc-cont (lookup 'pc cpu))
         (int-pc-cont (bits->int pc-cont))
         (add-cont (ram-read int-pc-cont ram))
         (opcode (extract 0 3 add-cont))
         (int-opcode (bits->int opcode))
         (addf (extract 4 15 add-cont))
         (int-addf (bits->int addf)))

    (if (equal? (lookup 'rf cpu) '(0))
        config  
        (let ((new-config (case int-opcode
                            [(0) (halt config)]
                            [(1) (do-load int-addf config)]
                            [(2) (do-store int-addf config)]
                            [(3) (do-add int-addf config)]
                            [(4) (do-sub int-addf config)]
                            [(5) (do-input config)]
                            [(6) (do-output config)]
                            [(7) (do-jump int-addf config)]
                            [(8) (do-skipzero config)]
                            [(9) (do-skippos config)]
                            [(10) (do-skiperr config)]
                            [(11) (do-loadi int-addf config)]
                            [(12) (do-storei int-addf config)]
                            [(13) (do-shift int-addf config)]
                            [(14) (do-and int-addf config)]
                            [(15) (do-xor int-addf config)]
                            )))
          (if (member int-opcode '(7 8 9 10 0)) 
              new-config
              (incr-pc 1 new-config))))))


;************************************************************
; ** problem 10 ** (10 points)
; Write three procedures

; (init-config lst)
; (symbol-table prog)
; (assemble prog)

; (init-config lst)
; takes a list lst of 16 bit patterns, and returns a TC-201 configuration 
; in which those patterns are loaded into RAM starting with address 0, 
; and the CPU registers are initialized so that the accumulator has
; value +0, the program counter has address 0, the run flag has 
; value 1, and the arithmetic error bit has value 0.

; (symbol-table prog)
; takes a TC-201 assembly language program prog (in the format specified below) 
; and returns a table of entries in which the key is a symbol that is a label 
; in prog and the value is the corresponding memory address for that
; instruction or data value (when the program is loaded into memory starting 
; at address 0.)  

; The addresses in the table should be in increasing order.

; (assemble prog)
; translates a TC-201 assembly language program prog 
; into a list of 16-bit patterns to be loaded into the TC-201 memory.

; The symbolic opcodes are: halt, load, store, add, sub, input, output
; jump, skipzero, skippos, skiperr, loadi, storei, shift, and, xor.

; There is also a data statement.

; An assembly language program is a list of "lines", where
; each line is a list of two or three elements representing
; an instruction or a data statement.  If the line has
; three elements, the first one is a symbolic label that
; should appear in the symbol table for the program.
; The remaining two elements (or the only two elements,
; if the line has just two elements) are either a symbol
; representing an opcode and an address, or the symbol 'data
; and a data value.  The address field of an instruction may
; be a number in the range 0 to 4095 inclusive, or a symbolic
; label, in which case the address is the numeric value of the
; label in the symbol table.  The value field of a data statement
; may be a number in the range -32767 to 32767 inclusive, or
; a symbolic label, in which case the value used is the numeric
; value of the label in the symbol table.

; You may assume that numeric addresses and data values will
; be in the correct ranges.

; Note that even instructions like halt, input, and skipzero, which
; ignore their address fields, must have an address specified.
; (A typical choice is 0 for the address fields of such instructions.)

; Example TC-201 assembly language programs

; a program with only instructions, numeric addresses, and no labels

(define prog1
  '((load 3)
    (store 4)
    (halt 0)))


; a program with only data statements, three labels, and both numeric
; and symbolic data values

(define prog2
  '((x data 1)
    (y data -2)
    (z data y)))

; a version of the program we wrote in lecture to sum up
; a zero-terminated sequence of numbers, output the sum, and halt.

(define prog3
  '((start  load constant-zero)
   (        store sum)
   (next    input 0)
   (        skipzero 0)
   (        jump add-num)
   (        load sum)
   (        output 0)
   (        halt 0)
   (add-num add sum)
   (        store sum)
   (        jump next)
   (sum     data 0)
   (constant-zero data 0)))

; Examples of init-config, symbol-table and assemble

;> (init-config ram-ex2)
;(conf
; (list (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
;       (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 0)) 
;       (entry 'rf '(1)) 
;       (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0)))

;> (symbol-table prog1)
;'()

;> (symbol-table prog2)
;(list (entry 'x 0) (entry 'y 1) (entry 'z 2))

;> (symbol-table prog3)
;(list
; (entry 'start 0)
; (entry 'next 2)
; (entry 'add-num 8)
; (entry 'sum 11)
; (entry 'constant-zero 12))

;> (assemble prog1)
;'((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;  (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

;> (assemble prog2)
;'((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
;  (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

;> (assemble prog3)
;'((0 0 0 1 0 0 0 0 0 0 0 0 1 1 0 0)
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
;  (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 1 1 1 0 0 0 0 0 0 0 0 1 0 0 0)
;  (0 0 0 1 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

;************************************************************

; initial configuration construction

(define (init-config lst)
  (conf cpu-ex4 lst))

; symbol table construction

(define (symbol-table prog)
  (let loop ((prog prog)
             (addr 0)
             (table '()))
    (cond ((null? prog) (reverse table))
          ((list? (car prog))
           (let ((line (car prog)))
             (if (= (length line) 3)  
                 (loop (cdr prog)
                       (+ addr 1)
                       (cons (entry (car line) addr) table))
                 (loop (cdr prog)
                       (+ addr 1)
                       table))))
          (else (loop (cdr prog) (+ addr 1) table)))))

; assemble program

(define (assemble prog)
  (let ((symboltable (symbol-table prog)))
    (define (helper n)
      (cond ((= n (length prog)) '())
            (else
              (let ((current-line (list-ref prog n)))
                (cond ((= (length current-line) 2)
                       (cond ((number? (list-ref current-line 1))
                              (cons (append (lookup (list-ref current-line 0) opcode-table)
                                            (int->bits-width (list-ref current-line 1) 12))
                                    (helper (+ n 1))))
                             ((symbol? (list-ref current-line 1))
                              (cons (append (lookup (list-ref current-line 0) opcode-table)
                                            (int->bits-width (lookup (list-ref current-line 1) symboltable) 12))
                                    (helper (+ n 1))))))
                      ((equal? (list-ref current-line 1) 'data)
                       (cond ((number? (list-ref current-line 2))
                              (cons (sm-int->bits (list-ref current-line 2)) 
                                    (helper (+ n 1))))
                             ((symbol? (list-ref current-line 2))
                              (cons (sm-int->bits (lookup (list-ref current-line 2) symboltable)) 
                                    (helper (+ n 1))))))
                      ((lookup (list-ref current-line 1) opcode-table)
                       (cond ((number? (list-ref current-line 2))
                              (cons (append (lookup (list-ref current-line 1) opcode-table)
                                            (int->bits-width (list-ref current-line 2) 12))
                                    (helper (+ n 1))))
                             ((symbol? (list-ref current-line 2))
                              (cons (append (lookup (list-ref current-line 1) opcode-table)
                                            (int->bits-width (lookup (list-ref current-line 2) symboltable) 12))
                                    (helper (+ n 1)))))))))))
    (helper 0)))

; table of symbolic opcodes

(define opcode-table
  (list
   (entry 'halt '(0 0 0 0))
   (entry 'load '(0 0 0 1))
   (entry 'store '(0 0 1 0))
   (entry 'add '(0 0 1 1))
   (entry 'sub '(0 1 0 0))
   (entry 'input '(0 1 0 1))
   (entry 'output '(0 1 1 0))
   (entry 'jump '(0 1 1 1))
   (entry 'skipzero '(1 0 0 0))
   (entry 'skippos '(1 0 0 1))
   (entry 'skiperr '(1 0 1 0))
   (entry 'loadi '(1 0 1 1))
   (entry 'storei '(1 1 0 0))	
   (entry 'shift '(1 1 0 1))
   (entry 'and '(1 1 1 0))
   (entry 'xor '(1 1 1 1))))

;************************************************************
; ** problem 11 ** (10 points)
; Write one procedure and one program for the TC-201

; (simulate n config)
; encrypt-prog

; (simulate n config)
; simulates the TC-201 computer from the configuration config until
; either it halts (the run flag is 0) or n iterations of the fetch/execute
; cycle have been performed, whichever is first.
; The result returned should be a list of the successive configurations 
; of the TC-201 starting with the config.

; You are strongly advised to use your simulate procedure to help you test 
; your implementation of the instructions more extensively than the test cases 
; in the assignment.

; encrypt-prog
; reads in a positive integer from the user, which is the encryption
; key.  Then it loops, reading in a positive integer and outputting
; that integer xor'd with the key.  The loop continues until the user
; enters a non-positive integer.

; Examples 
; (This program stops after executing 3 instructions, returning
; 4 configurations, including the initial one.)

;> (simulate 5 config-ex4)
;(list
; (conf
;  (list
;   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 'rf '(1))
;   (entry 'aeb '(0)))
;  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;    (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))
; (conf
;  (list
;   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 1))
;   (entry 'rf '(1))
;   (entry 'aeb '(0)))
;  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;    (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))
; (conf
;  (list
;   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
;   (entry 'rf '(1))
;   (entry 'aeb '(0)))
;  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))
; (conf
;  (list
;   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
;   (entry 'rf '(0))
;   (entry 'aeb '(0)))
;  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))))

; Examples of run-time behavior of encrypt-prog interacting 
; with user.  We "capture" the returned list of configurations 
; by assigning it to be the value of the symbol results.

;; below the key is 13 (1101).  Note that encrypt is also decrypt.
; > (define results (simulate 100 (init-config (assemble encrypt-prog))))
; input = 13
; input = 8
; output = 5
; input = 15
; output = 2
; input = 2
; output = 15
; input = 5
; output = 8
; input = 0

; > (define results (simulate 100 (init-config (assemble encrypt-prog))))
; input = 511
; input = 78
; output = 433
; input = 999
; output = 536
; input = 536
; output = 999
; input = 433
; output = 78
; input = 0


;************************************************************

(define (simulate n config)
  (let loop ((i 0)
             (current-config config)
             (hist (list config)))  
    (if (or (>= i n) 
            (equal? (lookup 'rf (conf-cpu current-config)) '(0))) 
        (reverse hist)  
        (let ((next-conf (next-config current-config)))  
          (loop (+ i 1) next-conf (cons next-conf hist))))))  

(define encrypt-prog
  '(
    (input 0)
    (store key)      
    (start input 0)   
    (skipzero done)  
    (skippos process)
    (jump done)       
    (process xor key) 
    (output 0)        
    (jump start)     
    (done halt 0)    
    (key data 0)
  ))

;************************************************************
; ** problem 12 ** (5 points)
; Write one program for the TC-201

; reverse-prog

; that reads in a zero-terminated sequence of numbers from
; the user, prints them out in reverse, and halts.
; The terminating 0 is not printed out.
; You need not worry about running out of memory.

; Examples

; Example of run-time behavior of reverse-prog interacting with user.
; We "capture" the sequence of configurations returned
; by simulate by making it the value of the symbol results.

;> (define results (simulate 100 (init-config (assemble reverse-prog))))
;input = 13
;input = -44
;input = 16
;input = 0
;output = 16
;output = -44
;output = 13
;************************************************************

(define reverse-prog
  '((start input 0)
    (skipzero 0)
    (jump st_num)
    (load pointer)
    (sub one)
    (store pointer)
    (jump ret_num)
    (st_num storei pointer)
    (load pointer)
    (add one)
    (store pointer)
    (jump start)
    (ret_num loadi pointer)
    (skipzero 0)
    (jump prnt)
    (halt 0)
    (prnt output 0)
    (load pointer)
    (sub one)
    (store pointer)
    (jump ret_num)
    (pointer data table)
    (one data 1)
    (market data 0)
    (table data 0)))

    

; ********************************************************
; ** problem 13 ** (5 points)
; Write another program for the TC-201

; power-prog

; that reads in a positive integer and an exponent for 2
; and prints out the integer multiplied by 2 to the power of the given exponent

; Examples

; Example of run-time behavior of power-prog interacting with user.

; > (define results (simulate 100 (init-config (assemble power-prog))))
; input = 20 
; input = -2
; output = 5
; > (define results (simulate 100 (init-config (assemble power-prog))))
; input = 15
; input = 3
; output = 120

(define power-prog
  '((input 0)
    (store ky)
    (input 0)
    (store powr)
    (load ky)
    (shift powr)
    (output 0)
    (halt 0)
    (ky data 0)
    (powr data 0)))

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

   (test 'ram-read (ram-read 0 ram-ex1) '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))

   (test 'ram-read (ram-read 6 ram-ex2) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
   
   (test 'diff-rams (diff-rams ram-ex1 ram-ex2) '((1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1)) (3 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0))))
   
   (test 'diff-rams (diff-rams '() '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))) '())
   
   (test 'diff-rams (diff-rams ram-ex1 (ram-write 2 '(0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1) ram-ex1)) '((2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1))))
   
   (test 'diff-rams (diff-rams ram-ex2 (ram-write 5 '(1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0) ram-ex2)) '((5 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0))))
   
   (test 'diff-rams (diff-rams ram-ex1 (ram-write 1 '(0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1) ram-ex1)) '((1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1))))
   
   
   (test 'extract (extract 1 3 '(a b c d e)) '(b c d))
   
   (test 'extract (extract 4 4 '(a b c d e)) '(e))
   
   (test 'bits->int (bits->int '(0)) 0)
   
   (test 'bits->int (bits->int '(0 0 0 1 1 0)) 6)
   
   (test 'int->bits (int->bits 0) '(0))
   
   (test 'int->bits (int->bits 6) '(1 1 0))
   
   (test 'int->bits-width (int->bits-width 14 8) '(0 0 0 0 1 1 1 0))
   
   (test 'int->bits-width (int->bits-width 14 3) "field too small")
   
   (test 'diff-configs (diff-configs config-ex1 config-ex2) '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
							      (aeb (0) (1))
							      (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
							      (3 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0))))
   
   (test 'incr-pc (incr-pc 1 config-ex1)
	 (conf
	  (list
	   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
	   (entry 'pc '(0 0 0 0 0 0 0 0 1 0 0 0))
	   (entry 'rf '(1))
	   (entry 'aeb '(0)))
	  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
	    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
   
   (test 'diff-configs (diff-configs config-ex2 (incr-pc 4090 config-ex2))
	 '((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1))))
   
   (test 'load-store (diff-configs config-ex1 (do-load 1 config-ex1))
	 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))))
   
   (test 'load-store (diff-configs config-ex2 (do-load 12 config-ex2))
	 '((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
   
   (test 'load-store (diff-configs config-ex1 (do-store 5 config-ex1))
	 '((5 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))))

   (test 'load-store  (diff-configs config-ex2 (do-store 0 config-ex2))
	 '((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))))


   (test 'add-sub (diff-configs config-ex1 (do-add 3 config-ex1))
	 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1))))

   (test 'add-sub (diff-configs config-ex2 (do-add 3 config-ex2))
	 '((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1))
	   (aeb (1) (0))))

   (test 'add-sub (diff-configs config-ex1 (do-sub 3 config-ex1))
	 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1))))

   (test 'add-sub (diff-configs config-ex2 (do-sub 3 config-ex2))
	 '((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (1 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1))
	   (aeb (1) (0))))

   (test 'add-sub  (let ((config (conf cpu-ex1 '((0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))) (diff-configs config (do-add 0 config)))
	 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
	   (aeb (0) (1))))


   (test 'skip-jump (diff-configs config-ex1 (do-jump 5 config-ex1))
	 '((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 0 1 0 1))))

   (test 'skip-jump (diff-configs config-ex2 (do-skipzero config-ex2))
	 '((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 0))))

   (test 'skip-jump (diff-configs config-ex1 (do-skippos config-ex1))
	 '((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 1))))

   (test 'skip-jump (diff-configs config-ex2 (do-skiperr config-ex2))
	 '((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 1)) (aeb (1) (0))))


   (test 'loadi-storei (diff-configs config-ex3 (do-loadi 1 config-ex3))
	 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1))))

   (test 'loadi-storei (diff-configs config-ex3 (do-loadi 2 config-ex3))
	 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1))))
   
   (test 'loadi-storei (diff-configs config-ex3 (do-storei 1 config-ex3))
	 '((5 (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))))
   
   (test 'loadi-storei (diff-configs config-ex3 (do-storei 2 config-ex3))
	 '((4 (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))))
   
   (test 'shift  (diff-configs config-ex3 (do-shift 2 config-ex3))
	 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0))))
   
   (test 'shift (diff-configs config-ex3 (do-shift 3 config-ex3))
	 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0))))
   
   (test 'shift (diff-configs config-ex3 (do-shift 6 config-ex3))
	 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))))
   
   (test 'and (diff-configs config-ex2 (do-and 1 config-ex2))
	 '((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))))
   
   (test 'and (diff-configs config-ex3 (do-and 1 config-ex3))
	 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1))))

   (test 'xor (diff-configs config-ex3 (do-xor 1 config-ex3))
	 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 1 1 1 0 0 0 0 0 0 0 0 1 0 1 0))))

   (test 'xor (diff-configs config-ex3 (do-xor 5 config-ex3))
	 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 0))))


   (test 'next-config (next-config config-ex4)
	 (conf
	  (list
	   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
	   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 1))
	   (entry 'rf '(1))
	   (entry 'aeb '(0)))
	  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
	    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
	    (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0))))
   
   (test 'next-config (next-config (next-config config-ex4))
	 (conf
	  (list
	   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
	   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
	   (entry 'rf '(1))
	   (entry 'aeb '(0)))
	  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
	    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
	    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))))
   
   (test 'next-config (next-config (next-config (next-config config-ex4)))
	 (conf
	  (list
	   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
	   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
	   (entry 'rf '(0))
	   (entry 'aeb '(0)))
	  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
	    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
	    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))))
   
   
   
   (test 'init-config (init-config ram-ex2)
	 (conf
	  (list (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
		(entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 0)) 
		(entry 'rf '(1)) 
		(entry 'aeb '(0)))
	  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
	    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0))))
   
   (test 'symbol-table (symbol-table prog1)
	 '())
   
   (test 'symbol-table (symbol-table prog2)
	 (list (entry 'x 0) (entry 'y 1) (entry 'z 2)))
   
   (test 'symbol-table (symbol-table prog3)
	 (list
	  (entry 'start 0)
	  (entry 'next 2)
	  (entry 'add-num 8)
	  (entry 'sum 11)
	  (entry 'constant-zero 12)))
   
   (test 'assemble (assemble prog1)
	 '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
	   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
   
   (test 'assemble (assemble prog2)
	 '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
	   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
   
   (test 'assemble (assemble prog3)
	 '((0 0 0 1 0 0 0 0 0 0 0 0 1 1 0 0)
	   (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
	   (0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
	   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 1 1 1 0 0 0 0 0 0 0 0 1 0 0 0)
	   (0 0 0 1 0 0 0 0 0 0 0 0 1 0 1 1)
	   (0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 1)
	   (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
	   (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
   
   (test 'simulate (simulate 5 config-ex4)
	 (list
	  (conf
	   (list
	    (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
	    (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 0))
	    (entry 'rf '(1))
	    (entry 'aeb '(0)))
	   '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
	     (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
	     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	     (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
	     (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))
	  (conf
	   (list
	    (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
	    (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 1))
	    (entry 'rf '(1))
	    (entry 'aeb '(0)))
	   '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
	     (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
	     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	     (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
	     (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))
	  (conf
	   (list
	    (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
	    (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
	    (entry 'rf '(1))
	    (entry 'aeb '(0)))
	   '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
	     (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
	     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	     (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
	     (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))
	  (conf
	   (list
	    (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
	    (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
	    (entry 'rf '(0))
	    (entry 'aeb '(0)))
	   '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
	     (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
	     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	     (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
	     (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))))
   ))

   

;********************** end of hw6.rkt **********************
