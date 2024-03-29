# CL-SMT-LIB -- Common Lisp SMT-Lib Integration

CL-SMT-LIB is a minimal package providing an SMT object encapsulating
any SMT solver process supporting
[SMT-LIB](http://smtlib.cs.uiowa.edu/about.shtml) with input and
output streams.  CL-SMT-LIB provides a reader macro to support reading
case sensitive SMT-LIB forms into lisp and writing these forms to an
SMT solver process.

The `make-smt` function takes a program name and command line
arguments and returns an smt object holding the process and the input
and output streams.  This process may be read from and written to like
any other stream.

The `#!` reader macro, defined in the `:cl-smt-lib` read table using
the [NAMED-READTABLES](https://github.com/melisgl/named-readtables)
package, enables case-sensitive reading of forms into common lisp.

The `write-to-smt` function facilitates writing case-sensitive forms
to the solver.

The following example demonstrates the use of CL-SMT-LIB to launch a
solver process, write a query to the solver, and read back the
results.

```
CL-SMT-LIB> (in-readtable :cl-smt-lib)
T
CL-SMT-LIB> (defparameter smt (make-smt "z3" "-in" "-smt2"))
SMT
CL-SMT-LIB> smt
#<SMT
  :PROCESS #<SB-IMPL::PROCESS 24691 :RUNNING>
  :INPUT-STREAM #<SB-SYS:FD-STREAM for "descriptor 10" {1002F3AB13}>
  :OUTPUT-STREAM #<SB-SYS:FD-STREAM for "descriptor 9" {1002F3A713}>>
CL-SMT-LIB> (write-to-smt smt
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
CL-SMT-LIB> (read smt)
SAT
CL-SMT-LIB> (read smt)
(MODEL (DEFINE-FUN EXAMPLE2 NIL (_ BITVEC 8) 44)
 (DEFINE-FUN EXAMPLE1 NIL (_ BITVEC 8) 97))
```

Since `write-to-smt` takes any stream as its first argument you can
preview the text sent to the smt solver by passing `t` as the first
argument.
```
CL-SMT-LIB> (write-to-smt t
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
CL-SMT-LIB> 
```

The special variable `*smt-debug*` may be used to copy smt input and
output to a stream for debugging.  Set `*smt-debug*` to `t` to echo
all input and output to STDOUT.

The following options should work to define smt objects for popular
SMT solvers.

[Z3](https://github.com/Z3Prover/z3)
:   `(make-smt "z3" '("-in" "-smt2"))`

[CVC4](http://cvc4.cs.stanford.edu/web/)
:   `(make-smt "cvc4" '("--lang=smt2"))`

## Acknowledgment

The project or effort depicted was sponsored by the Air Force Research
Laboratory (AFRL) and the Defense Advanced Research Projects Agency
(DARPA) under contract no. FA8750-15-C-0113. Any opinions, findings,
and conclusions or recommendations expressed in this material are
those of the author(s) and do not necessarily reflect the views of
AFRL or DARPA.
