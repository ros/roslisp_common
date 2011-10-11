(defpackage :cl-utils
  (:documentation "Useful macros and utility functions for Ansi Common Lisp.")
  (:use common-lisp)
  (:export

   ;; Types
   :non-null

   ;; Vars
   :defvars

   ;; Symbols
   :build-symbol-name :intern-compound-symbol

   ;; Binding
   :with-gensyms :with-readers :with-struct :condlet  

   ;; Assertions
   :verify-type :check-not-null

   ;; Destructuring
   :dbind 

   ;; Control flow
   :while :till :repeat-until :repeat :for-loop

   ;; Abbreviations
   :abbrev :mvbind :dsbind :mvsetq :unbind-slot :unbind-slots

   ;; CLOS
   :def-symmetric-method

   ;; Anaphoric macros
   :it :aif :awhen :awhile :aand

   ;; Setf
   :_f :toggle :orf

   ;; Randomized
   :generate-histogram :if-rand :when-rand :hist-count

   ;; Output
   :bind-pprint-args

   ;; Functional 
   :compose :nth-arg-fn :fn :defaggregator :partial :rcurry

   ;; Hash tables
   :hash-table-has-key :do-hash-entries :hash-keys :pprint-hash-table
   :hash-table-to-alist :alist-to-hash-table

   ;; Sequences
   :blocks :is-prefix :slast :groups-of-size :tokenize

   ;; Lists
   :filter :is-permutation :length-equals

   ;; Alists
   :lookup-alist

   ;; Sequences
   :blocks

   ;; Arrays
   :make-adjustable-vector :do-vec

   ;; Iteration
   :do-iterator :map-iterator :realize :iterator :take :range :take-until :concat
   :elements-satisfying :iter

   ;; Math
   :close-to :*tol* :mean :std :quantile

   ;; Linear algebra
   :m+ :mv* :inner-product 

   ;; Printing
   :force-format
))