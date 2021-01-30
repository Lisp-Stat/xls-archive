
;;;;  SIMULATE.LSP
;;;;
;;;;     Simulation Objects
;;;;               12 Nov 91 --> 27 Feb 92  Group recorders into sim cells.
;;;;                             28 May 92  Renamed.
;;;;                              6 Aug 92  Add some interface things.
;;;;                             20 Sep 92  Need to add tags. Have opts.
;;;;                             22 Sep 92  Introduce streams.
;;;;                              1 Oct 92  Data recovery put back.
;;;;

(require "compare")
(require "recorder")
(require "simcell")
(require "funcdict")
(require "streams")

;;;;

(provide "simulate")

;;;;


#||
      These objects manage the seeds and accumulation of
      simulation results.  The simulation may, for example, be

                - interrupted and written to a file,
                - restored from the file, and
                - resumed where computations were interrupted.

      The results are identical to those that would have been 
      obtained by running the simulation without interruption.

;;;;  Example

(def mean-list
     (list (cons 'two  #'(lambda (x) (+ x 2)))           ; dot pairs!!!
           (cons 'five #'(lambda (x) (+ x 5)))
           (cons 'nine #'(lambda (x) (+ x 9)) )))
(def location-factor (make-function-dictionary mean-list
                                               :name 'mean))
(def scale-list
     (list  (cons 'one  #'(lambda (x) (* x 1)))
            (cons '1K   #'(lambda (x) (* x 1000)))  ))
(def scale-factor (make-function-dictionary scale-list
                                            :name 'scale))
(def size-list
     (list (cons 'some #'(lambda (x) (select x (iseq 10))))
           (cons 'all  #'(lambda (x) x))  ))
(def size-factor (make-function-dictionary size-list
                                           :name 'size))


(def estimator-options
     (make-set '(mean median midrange)  :name ':estOpt))

(def more-options
     (make-set '(low  high) :name ':moreOpt))



(defun center (x &key estOpt)
   (case estOpt
     ('mean     (mean x))
     ('median   (median x))
     ('trim     (trimmed-mean x))
     ('midrange (mean (list (min x) (max x))) (min x))))

(defun l&s (x &key estOpt moreOpt)
   (case estOpt
     ('mean     (list (mean x)   (standard-deviation x)))
     ('median   (list (median x) (interquartile-range x)))
     ('trim     (list (trimmed-mean x) (max x)))
     ('midrange (list (mean (list (min x) (max x))) (min x)))  ))
     

(def sim (make-simulator 20 #'l&s
                         (list scale-factor size-factor)
                         (list estimator-options more-options)
                         :tags '(loc scl)
                         :proto memorizer-proto))

(send sim :run 3)
(send sim :run 10)



(def cell (first (send sim :cell-list)))
(send cell :label)
(def cp (send cell :plot-summary))

(def rec  (send cell :recorder))
(format t "list of the recorder is ~a~%" (send rec :list))

(send sim :select-cells '((size . small) (size . large)
                           (dist . normal) (:estopt . trim)))


||#


;;;
;;;     __________  SIMULATOR COLLECTIONS  __________
;;;
;;;      A simulator is a collection of simulation cells driven
;;;      by a common simulation seed and primitive generator.
;;;

(defproto simulator-proto
  '(seedCurr      ; seed to use when simulation is resumed
    seedList      ; seeds s[0], s[skipFactor], s[2 skipFactor]...
    seedSkipFactor; defaults to 5

    n             ; sample size for each replication
    generator     ; uniform generator by default
    nTrials       ; cumulative number trials to date

    comment       ; description of the simulation
    prolog        ; functions run and the start
    epilog        ;    and at the finish of the simulation

    estimator     ; function that does the work
    estimatorFunc ; lambda function for application to blocks
    outputTags    ; tags that specialize recording of sim results
    blocks        ; function dictionary of blocking factors
    options       ; set of options for estimator routine
                  
    proto         ; prototype(s) used for recording results in cells
    cellList      ; list of simulation cells
  ))

;;     _____  CREATOR  _____

(defun make-simulator (n estimator blocks options
                         &key tags (generator #'uniform-rand)
                         prolog epilog (proto condenser-proto)
                         comment (skipFactor 5))
  (send simulator-proto :new n estimator blocks options
        :prolog prolog :epilog epilog :tags tags
        :generator generator :proto proto
        :comment comment :skipFactor skipFactor))

(defmeth simulator-proto :ISNEW (n estimator blocks options
                    &key tags generator prolog epilog proto
                                   comment skipFactor)
  "Set proto to the recorder(s) for cells of array."
  (setf (slot-value 'nTrials)    0)
  (setf (slot-value 'n)          n)
  (setf (slot-value 'estimator)  estimator)
  (setf (slot-value 'outputTags) tags)
  (setf (slot-value 'blocks)     blocks)
  (setf (slot-value 'options)    options)
  (setf (slot-value 'generator)  generator)
  (setf (slot-value 'prolog)     prolog)
  (setf (slot-value 'epilog)     epilog)
  (setf (slot-value 'proto)      proto)
  (setf (slot-value 'comment)    comment)
  (setf (slot-value 'seedSkipFactor) skipFactor)
  (send self :build-estimator-func)
  (let ((seed (make-random-state t)))
    (setf (slot-value 'seedCurr) (make-random-state seed))
    )
  )


;;     _____  ACCESSORS  _____

(defmeth simulator-proto :COMMENT ()
  (slot-value 'comment))

(defmeth simulator-proto :N-TRIALS ()
  (slot-value 'nTrials))

(defmeth simulator-proto :BLOCKS ()
  (slot-value 'blocks))

(defmeth simulator-proto :ESTIMATOR ()
  (slot-value 'estimator))

(defmeth simulator-proto :N-BLOCKS ()
  (length (slot-value 'blocks)))

(defmeth simulator-proto :BLOCK-NAMES ()
  (mapcar #'(lambda (b) (send b :name))
          (slot-value 'blocks))  )

(defmeth simulator-proto :N-BLOCK-LEVELS ()
  (reduce #'* (mapcar #'(lambda (b) (send b :size))
                      (slot-value 'blocks)))  )


(defmeth simulator-proto :OPTIONS ()
  (slot-value 'options))

(defmeth simulator-proto :N-OPTIONS ()
  (length (slot-value 'options)))

(defmeth simulator-proto :OPTION-NAMES ()
  (mapcar #'(lambda (o) (send o :name))
          (slot-value 'options)))

(defmeth simulator-proto :N-OPTION-LEVELS ()
  (reduce #'* (mapcar #'(lambda (o) (send o :size))
                      (slot-value 'options)))  )


(defmeth simulator-proto :FACTORS ()
  (append (slot-value 'blocks) (slot-value 'options)))

(defmeth simulator-proto :N-FACTORS ()
  (+ (send self :n-blocks) (send self :n-options)))

(defmeth simulator-proto :CELL-LIST ()
  (slot-value 'cellList))


;;     _____  SEED MANAGEMENT  _____

(defmeth simulator-proto :INITIAL-SEED ()
  (make-random-state (first (slot-value 'seedList)))
  )

(defmeth simulator-proto :CURRENT-SEED ()
  (make-random-state (slot-value 'seedCurr))
  )

(defmeth simulator-proto :RECORD-CURR-SEED ()
  (push (make-random-state *random-state*) (slot-value 'seedList)))
 

;;     _____  SAMPLE RECOVERY  _____

(defun SUBSET? (a b)
  (cond
    ((endp a) t)
    ((member (first a) b :test #'equal) (subset? (rest a) b))
    (   t     nil)))

(defmeth simulator-proto :RECOVER-BLOCK-DATA (trial aList)
  "Compute block associated with given trial; aList identifies block."
  (format t "Rebuilding trial ~d for cell ~a~%" trial aList)
  (let* ((noise ())  (blocks ())
         (nOptLev   (send self :n-option-levels))
         (seeds     (slot-value 'seedList))
         (seedIndex (floor (/ trial (slot-value 'seedSkipFactor))))
         (seedIndex (- (length seeds) (1+ seedIndex))); rev order
         (rem       (mod trial (slot-value 'seedSkipFactor)))
         (cellIndex (which (send self :cells-do 
                                 #'(lambda (c)
                                     (subset? aList (send c :label))
                                     ))))
         (blkIndex  (/ cellIndex (if (< 0 nOptLev) nOptLev 1)))  )
    (format t "Cell[~a] using Seed[~a] ~%" cellIndex seedIndex)
    (setf *random-state* (make-random-state (select seeds seedIndex)))
    (format t "Generating trials...")
    (dotimes (i (1+ rem))
             (format t "~d " (+ i (- trial rem)))
             (setf noise (funcall (slot-value 'generator) (slot-value 'n)))
             (setf blocks (make-list-from-stream ; force eval of each
                           (send self :block-stream noise))  ))
    (format t "Done.  Data in global *BLOCK*.~%")
    (def *BLOCK* (select blocks  blkIndex))                      
    ))

#||
(send sim :recover-block-data 0 '((:estopt . mean) (dist . normal)))
||#


 

;;     _____  STREAMS  _____

(defmeth simulator-proto :RANDOM-SAMPLE-STREAM (n)
  "Returns the stream of n random samples."
  (if (< n 1)
      the-empty-stream
      (make-stream (funcall (slot-value 'generator) (slot-value 'n))
                   (send self :random-sample-stream (- n 1))
                   )))

(defmeth simulator-proto :BLOCK-STREAM (data)
  "Return stream of blocks in canonical order."
  (send self :map-functions
        (slot-value 'blocks) 
        (make-stream data the-empty-stream)))
  
    

;;     _____  RUNNING  _____


(defmeth simulator-proto :PROLOG (silent?)
  (unless (slot-value 'cellList)
          (send self :build-cell-list))
  (setf *random-state* (slot-value 'seedCurr))
  (setf *dPrint* (not silent?))
  (if (slot-value 'prolog) (funcall (slot-value 'prolog)))
  )


(defmeth simulator-proto :RUN (nReps &key (silent? t))
  (send self :prolog silent?)
  (let ((soFar  (- (slot-value 'nTrials) 1))
        (skip   (slot-value 'seedSkipFactor))
        (blocks () )
        (cells  (list-stream (slot-value 'cellList)))
        (tags   (slot-value 'outputTags))  )
    (def saveNoise ())
    (dotimes (trial nReps)
             (setf soFar (+ 1 soFar))
             (format t "Starting trial #~d~%" soFar)
             (if (= 0 (mod soFar skip)) (send self :record-curr-seed))
             (setf noise  (funcall (slot-value 'generator) (slot-value 'n)))
             (push (first noise) saveNoise)
             ;   (format t "Leading noise is ~a~%" (first noise))
             (setf blocks (send self :block-stream noise))
             ;   (format t "First block is  ~a~%" (head-stream blocks))
             (setf stats  (send self :map-estimator blocks))
             ;   (format t "Head of stats is  ~a~%" (head-stream stats))
             (force-stream
              (map-pair-stream #'(lambda (c x) (send c :add x :to tags))
                               cells stats))
             )
        (setf (slot-value 'nTrials) (+ (slot-value 'nTrials) nReps))
    (send self :epilog silent?)
    ))

#||
    (setf noise  (funcall (send sim :slot-value 'generator) 30))
    (setf blocks (send sim :block-stream noise))
    (setf opts   (send sim :build-option-stream))
    (setf stats  (send sim :map-estimator blocks))
    (setf cells  (list-stream (send sim :slot-value 'cellList)))
    (setf cell0  (first       (send sim :slot-value 'cellList)))
    (setf cell1  (second      (send sim :slot-value 'cellList)))

    (setf tags   (send sim :slot-value 'outputTags))

    (force-stream
     (map-pair-stream #'(lambda (c x) (send c :add x :to tags))
                      cells stats))
||#

(defmeth simulator-proto :EPILOG (silent?)
  (setf (slot-value 'seedCurr) (make-random-state nil))
  (if (slot-value 'epilog) (funcall (slot-value 'epilog)))
  (if silent? (setf *dPrint* t))
  (format t "Simulation run completed...~%")
  )

  
;;     _____  MAPPING FACTORS  _____

(defmeth simulator-proto :MAP-FUNCTIONS (fDict data)
  "Return stream from mapping function dicts in input list over data stream."
  (if (endp fDict) data
      (send self :map-functions (rest fDict)
            (flat-map-stream #'(lambda (x)
                                 (send (first fDict) :apply-stream x))
                             data)))) 

(defmeth simulator-proto :MAP-ESTIMATOR (blocks)
  "Return stream of stats applied to block stream,
   with varying options from stream of options."
  (if (slot-value 'options)
      (flat-map-stream  (slot-value 'estimatorFunc) blocks)
      (map-stream (slot-value 'estimatorFunc) blocks)))
  

(defmeth simulator-proto :BUILD-BLOCK (assocList data)
  "Build the block for given (name.assoc) list to input data."
  (dolist (factor (slot-value 'blocks))
          ;  (format t "Applying ~a~%" (cdr (first assocList)))
          (setf data (send factor :apply data
                           :label (cdr (first assocList))))
          (setf assocList (rest assocList))    )
  data)

(defmeth simulator-proto :BUILD-ESTIMATOR-FUNC ()
  "Build the lambda function to apply to blocks."
  (let ((opts (send self :build-option-stream)))
    (setf (slot-value 'estimatorFunc)
       (if opts
           #'(lambda (b) 
               (map-stream #'(lambda (o) (apply (slot-value 'estimator) b o))
                           opts))
           #'(lambda (b) (funcall (slot-value 'estimator) b))
           ))))

                                                  

;;     _____  CREATION OF SIMULATION CELL LIST  _____


(defun gop (a b)      ; Utility for generalized outer product
  (if (endp a) b
      (gop (rest a)
           (mapcan #'(lambda (x)
                       (mapcar
                        #'(lambda (y) (cons x y))
                          b))
                   (first a)))
      ))

(defmeth simulator-proto :BUILD-CELL-LABEL-LIST ()
  (let ((bLab (reverse 
                (mapcar #'(lambda (b) (send b :tagged-list))
                        (send self :factors)))))
    (gop (rest bLab) (mapcar #'list (first bLab)))
    ))

(defmeth simulator-proto :BUILD-OPTION-STREAM ()
  (let ((opts (reverse 
               (mapcar #'(lambda (s) (send s :tagged-list :asConsPair nil))
                       (send self :options)) )))
    (list-stream
     (case (length opts)
       (0 nil)
       (1 (first opts))
       (t (mapcar #'combine
                  (gop (rest opts) (mapcar #'list (first opts)))
                  ))))))


(defmeth simulator-proto :BUILD-CELL-LIST ()
  "Called at initialization to create list of simulation cells."
  (let ((tags (slot-value 'outputTags)))
    (setf (slot-value 'cellList)
          (mapcar ; NEED to include features for each cell
                  #'(lambda (l)
                      (make-sim-cell self
                                     :label l
                                     :recorderProto (slot-value 'proto)
                                     :features tags))
                  (send self :build-cell-label-list))
          )))
    
        
;;  _____  SELECTION OF CELLS  _____

(defmeth simulator-proto :SELECT (pred)
  (mapcan #'(lambda(c) (if (funcall pred c) (list c)))
          (slot-value 'cellList)))


(defmeth simulator-proto :SELECT-CELLS (aList)  
  "Choose cells whose labels appear in the association list."
  (send self :select
        #'(lambda(c)
            (not (member nil 
                         (mapcar #'(lambda (p)
                                     (member p aList :test #'equal))
                                 (send c :label))
                         )))))
                        


(defmeth simulator-proto :CELLS-DO (func)
  (mapcar func (slot-value 'cellList))  )


(defmeth simulator-proto :AREF-CELL (index)
  (aref (coerce (slot-value 'cellList) 'vector) index))


;;     _____  CELL SUMMARIES  _____


(defmeth simulator-proto :PLOT-SUMMARY (plotFunc which)
  "Plot aList selected cells, with plotFunc drawing for each cell."
  )

(defmeth simulator-proto :PRINT-SUMMARY (file)
  "Write annotated one-line summary of each simulation cell."
  (let ((cl   (slot-value 'cellList))
        (comm (if (slot-value 'comment)
                  (slot-value 'comment) "No comment"))    )
    (format t "Simulator Summary (~a, ~a Trials)~%" 
            comm (slot-value 'nTrials))
    (format t "~a~%" 
            (mapcar #'car (send (first cl) :label)))
    (send self :cells-do
          #'(lambda(c)
              (let ((l&s (cdr (send c :lands)))  )
                (dolist (l (mapcar #'cdr (send c :label)))
                        (format t "~a " l))
                (dolist (loc (first l&s))  (format t "~a " loc))
                (dolist (scl (second l&s)) (format t "~a " scl))
                (format t "~%")   )))
    nil))

 
(defmeth simulator-proto :PRINT-SEEDS ()
  (format t "  Seed 0 ~a ~%" (send self :initial-seed))
  (format t "  Seed n ~a ~%" (send self :current-seed))
  )


(defmeth simulator-proto :WRITE-ANALYSIS-FILE (fileName)
  (with-open-file (fStream fileName :direction :output)
                  (format fStream "~a~%" (send self :analysis-file-header))
                  (mapcar #'(lambda (c) (send c :append-to-stream fStream))
                          (send self :cell-list))
                  )) 

(defmeth simulator-proto :ANALYSIS-FILE-HEADER ()
  "Variable names for subsequent interpretation of columns."
  (let ((cell  (first (send self :cell-list)))  )
    (apply #'strcat
           (append
            (mapcar #'(lambda (l)
                        (format nil " ~s" (car l)))
                    (send cell :label))
            (list " FEATURE VALUE ")))))

(defmeth simulator-proto :WRITE-TO-FILE (fileName)
  (with-open-file (file filename :direction :output)
                  (print (send self :comment) file)
                  (print (send self :initial-seed) file)
                  (print (send self :current-seed) file)
                  )
  nil)


(defmeth simulator-proto :READ-FROM-FILE (fileName)
  (with-open-file (file fileName)
                  (setf (slot-value 'comment)  (read file))
                  (setf (slot-value 'seedInit) (read file))
                  (setf (slot-value 'seedCurr) (read file))
                  )
  nil)




;;     _____  Some Utilities  _____

(def *dPrint* t)
(defun dPrint (&rest args)
  (if *dPrint*
      (apply #'format t args)
      ))

