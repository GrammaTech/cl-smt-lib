# CL-SMT -- Common Lisp smt-libv2 Integration

The CL-SMT package provides the `#!` reader macro for case-sensitive
reading of forms (suitable for building queries in the case-sensitive
smt-libv2 language).  It also provides `program-stream` suitable for
calling an SMT solver and wrapping it in a two-way stream allowing the
writing of smt-libv2 forms to the solver and the reading of results.
The included `smt` function facilitates writing these forms to the
solver.

The following example demonstrates the use of CL-SMT to write a query
to a running smt solver process (in this case CVC4), and read back the
resulting model.

```
CL-SMT> (enable-preserving-case-syntax)
T
CL-SMT> (setf *stream* (program-stream "cvc4" '("--lang=smt2")))
#<TWO-WAY-STREAM
  :INPUT-STREAM #<SB-SYS:FD-STREAM for "descriptor 63" {1004BF10D3}>
  :OUTPUT-STREAM #<SB-SYS:FD-STREAM for "descriptor 62" {1004BF0C13}>>
CL-SMT> (smt *stream*
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
CL-SMT> (read *stream*)
SAT
CL-SMT> (read *stream*)
(MODEL (DEFINE-FUN EXAMPLE1 NIL (_ BITVEC 8) (_ BV208 8))
 (DEFINE-FUN EXAMPLE2 NIL (_ BITVEC 8) (_ BV193 8)))
```
