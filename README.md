# CL-SMT -- Common Lisp smt-lib Integration

CL-SMT is a minimal package providing an SMT object encapsulating an
SMT solver process with input and output streams, as well as support
for reading case sensitive smt-lib forms into lisp and writing these
forms to an SMT solver process.

The `make-smt` function takes a program name and command line
arguments and returns an smt object holding the process and the input
and output streams.  This process may be read from and written to like
any other stream.

The `#!` reader macro enables case-sensitive reading of forms.

The `write-to-smt` function facilitates writing case-sensitive forms
to the solver.

The following example demonstrates the use of CL-SMT to launch a
solver process, write a query to the solver, and read back the
results.

```
CL-SMT> (enable-preserving-case-syntax)
T
CL-SMT> (defvar smt (make-smt "cvc4" '("--lang=smt2")))
SMT
CL-SMT> smt
#<SMT
  :PROCESS #<SB-IMPL::PROCESS 24691 :RUNNING>
  :INPUT-STREAM #<SB-SYS:FD-STREAM for "descriptor 10" {1002F3AB13}>
  :OUTPUT-STREAM #<SB-SYS:FD-STREAM for "descriptor 9" {1002F3A713}>>
CL-SMT> (write-to-smt smt
                      (let ((range 8))
                        #!`((set-option :produce-models true)
                            (set-logic QF_BV)

                            (define-fun hamming-weight ((bv (_ BitVec ,RANGE)))
                                (_ BitVec ,RANGE)
                              ,(REDUCE (LAMBDA (ACC N)
                                         `(bvadd ,ACC ((_ zero_extend ,(1- RANGE))
                                                       ((_ extract ,N ,N) bv))))
                                       (LOOP :FOR I :UPFROM 1 :BELOW (1- RANGE) :COLLECT I)
                                       :INITIAL-VALUE
                                       `((_ zero_extend ,(1- RANGE)) ((_ extract 0 0) bv))))
                            (declare-const example1 (_ BitVec ,RANGE))
                            (declare-const example2 (_ BitVec ,RANGE))
                            (assert (= (_ bv3 ,RANGE) (hamming-weight example1)))
                            (assert (= (_ bv3 ,RANGE) (hamming-weight example2)))
                            (assert (distinct example1 example2))
                            (check-sat)
                            (get-model))))
NIL
CL-SMT> (read smt)
SAT
CL-SMT> (read smt)
(MODEL (DEFINE-FUN EXAMPLE1 NIL (_ BITVEC 8) (_ BV97 8))
 (DEFINE-FUN EXAMPLE2 NIL (_ BITVEC 8) (_ BV225 8)))
```

Since `write-to-smt` takes any stream as it's first argument you can
preview the text sent to the smt solver by passing `t` as the first
argument.
```
CL-SMT> (write-to-smt t
                      (let ((range 8))
                        #!`((set-option :produce-models true)
                            (set-logic QF_BV)

                            (define-fun hamming-weight ((bv (_ BitVec ,RANGE)))
                                (_ BitVec ,RANGE)
                              ,(REDUCE (LAMBDA (ACC N)
                                         `(bvadd ,ACC ((_ zero_extend ,(1- RANGE))
                                                       ((_ extract ,N ,N) bv))))
                                       (LOOP :FOR I :UPFROM 1 :BELOW (1- RANGE) :COLLECT I)
                                       :INITIAL-VALUE
                                       `((_ zero_extend ,(1- RANGE)) ((_ extract 0 0) bv))))
                            (declare-const example1 (_ BitVec ,RANGE))
                            (declare-const example2 (_ BitVec ,RANGE))
                            (assert (= (_ bv3 ,RANGE) (hamming-weight example1)))
                            (assert (= (_ bv3 ,RANGE) (hamming-weight example2)))
                            (assert (distinct example1 example2))
                            (check-sat)
                            (get-model))))
(set-option :produce-models true)
(set-logic QF_BV)
(define-fun hamming-weight ((bv (_ BitVec 8))) (_ BitVec 8)
 (bvadd
  (bvadd
   (bvadd
    (bvadd
     (bvadd
      (bvadd ((_ zero_extend 7) ((_ extract 0 0) bv))
       ((_ zero_extend 7) ((_ extract 1 1) bv)))
      ((_ zero_extend 7) ((_ extract 2 2) bv)))
     ((_ zero_extend 7) ((_ extract 3 3) bv)))
    ((_ zero_extend 7) ((_ extract 4 4) bv)))
   ((_ zero_extend 7) ((_ extract 5 5) bv)))
  ((_ zero_extend 7) ((_ extract 6 6) bv))))
(declare-const example1 (_ BitVec 8))
(declare-const example2 (_ BitVec 8))
(assert (= (_ bv3 8) (hamming-weight example1)))
(assert (= (_ bv3 8) (hamming-weight example2)))
(assert (distinct example1 example2))
(check-sat)
(get-model)
NIL
CL-SMT> 
```
