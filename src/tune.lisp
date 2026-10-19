;;;; tune.lisp -- work out each level's order and par, and how hard it is.
;;;;
;;;;   sbcl --script src/tune.lisp [name ...]     (a _ in a name for a space)
;;;;
;;;; For each level, or each one named, it deals the queue from the level's
;;;; seed -- or the next seed whose deal can be won -- collects orders the
;;;; solver finds that win when played out in the real model, and takes the
;;;; fastest; then it moves one tap at a time, a few places either way,
;;;; keeping each move that makes the order faster and still wins.  An order
;;;; wins only if it wins at each of *CHECK-STEPS*, and its time is the
;;;; longest of them.  It prints the :seed, :order and :par to put in the
;;;; level.
;;;;
;;;; How hard: it plays the round many times in the solver's model, by a
;;;; player who taps any column at random and by one who taps only boxes
;;;; that can take a cube now (when there are any), and prints how often each
;;;; loses.  The careful player's losses are the level's traps.

(defpackage #:uikit (:use #:cl))
(let ((here (directory-namestring (or *load-truename* *default-pathname-defaults*))))
  (load (merge-pathnames "package.lisp" here))
  (load (merge-pathnames "model.lisp" here)))

(in-package #:ants)

(defparameter *candidates* 8 "Winning orders to collect before improving the fastest.")
(defparameter *search-tries* 60 "Orders to play out while collecting them.")
(defparameter *improve-tries* 300 "Orders to play out while improving.")
(defparameter *reach* 4 "Places either way a tap may move.")
(defparameter *playouts* 400 "Rounds each made-up player plays.")

(defun candidates (level columns seed)
  "Orders for the deal that the solver finds and that win when played out,
as (time . order), fastest first: half of them trying the boxes that take
most first, half the columns from the left."
  (let ((found '())
        (game (make-round level columns seed)))
    (dolist (greedy '(t nil))
      (let ((wins 0)
            (tries 0))
        (block search
          (solvable-p (parse-picture (level-rows level)) columns (level-slots level)
                      :greedy greedy
                      :budget *deal-budget*
                      :accept (lambda (order)
                                (multiple-value-bind (won time) (play-out-checked game order)
                                  (when won
                                    (incf wins)
                                    (push (cons time order) found)))
                                (when (or (>= wins (/ *candidates* 2))
                                          (>= (incf tries) (/ *search-tries* 2)))
                                  (return-from search))
                                nil)))))
    (sort found #'< :key #'first)))

(defun move-tap (order from to)
  (let ((v (coerce order 'vector)))
    (let ((tap (aref v from)))
      (if (< from to)
          (replace v v :start1 from :start2 (1+ from) :end2 (1+ to))
          (replace v v :start1 (1+ to) :start2 to :end2 from))
      (setf (aref v to) tap))
    (coerce v 'list)))

(defun improve (game time order)
  "Faster orders than ORDER, which takes TIME, by moving single taps."
  (let ((tries 0)
        (n (length order)))
    (loop
      (let ((better nil))
        (loop for from below n
              until better
              do (loop for to from (max 0 (- from *reach*)) to (min (1- n) (+ from *reach*))
                       until better
                       unless (or (= to from) (eql (nth from order) (nth to order)))
                         do (when (> (incf tries) *improve-tries*)
                              (return-from improve (values time order)))
                            (let ((new (move-tap order from to)))
                              (multiple-value-bind (won new-time) (play-out-checked game new)
                                (when (and won (< new-time (- time 1d-6)))
                                  (setf better (cons new-time new)))))))
        (unless better
          (return (values time order)))
        (setf time (car better) order (cdr better))))))

(defun abstract-round (grid columns slots rng careful)
  "Play a round in the solver's model, tapping columns at random -- or, if
CAREFUL, columns whose front box can take a cube now, when there are any.
:WON or :LOST."
  (let* ((grid (copy-grid grid))
         (columns (coerce (mapcar #'copy-list columns) 'vector))
         (w (array-dimension grid 1))
         (size (cell-size (array-dimension grid 0) w))
         (deployed '()))
    (loop
      (setf deployed (settle grid deployed))
      (when (zerop (cube-count grid)) (return :won))
      (let ((slot (loop for slot below slots
                        unless (find slot deployed :key #'fourth) return slot))
            (open (loop for column across columns for k from 0 when column collect k)))
        (when (or (null slot) (null open)) (return :lost))
        (let* ((reachable (reachable-colours grid))
               (useful (remove-if-not (lambda (k) (member (car (first (aref columns k))) reachable))
                                      open))
               (choices (if (and careful useful) useful open))
               (k (nth (rng-next rng (length choices)) choices))
               (box (pop (aref columns k))))
          (push (list (car box) (cdr box) (slot-entry-column slot slots w size) slot) deployed))))))

(defun loss-rate (grid columns slots careful)
  (let ((rng (make-rng 7)))
    (/ (loop repeat *playouts*
             count (eq :lost (abstract-round grid columns slots rng careful)))
       (float *playouts*))))

(defun tune (level)
  (let ((start (get-internal-real-time))
        (grid (parse-picture (level-rows level))))
    (multiple-value-bind (columns seed)
        (deal-winnable grid level
                       :accept (lambda (columns seed order)
                                 (play-out-checked (make-round level columns seed) order)))
      (unless seed
        (format t "~&~a: no deal from seed ~d on wins~%" (level-name level) (level-seed level))
        (return-from tune))
      (let* ((game (make-round level columns seed))
             (found (candidates level columns seed)))
        (destructuring-bind (first-time . first-order) (first found)
          (multiple-value-bind (time order) (improve game first-time first-order)
            (format t "~&LEVEL	~a	~d	~{~d~}	~d	~,2f	~,2f~%"
                    (level-name level) seed order (ceiling time)
                    (loss-rate grid columns (level-slots level) nil)
                    (loss-rate grid columns (level-slots level) t))
            (format t "  ~a: seed ~d, ~d orders found, fastest ~,1fs, improved to ~,1fs; ~,1fs~%"
                    (level-name level) seed (length found) first-time time
                    (/ (- (get-internal-real-time) start) internal-time-units-per-second))
            (finish-output)))))))

(let ((names (mapcar (lambda (name) (substitute #\Space #\_ name))
                     (rest #+sbcl sb-ext:*posix-argv* #+ecl (ext:command-args)))))
  (dolist (level *levels*)
    (when (or (null names) (member (level-name level) names :test #'string-equal))
      (tune level))))
