;;;; test.lisp -- play every level headless, on the host.
;;;;
;;;;   sbcl --script src/test.lisp      (or ecl --shell src/test.lisp)
;;;;
;;;; The model has no Objective-C in it, so it loads without objc; the
;;;; package's UI nickname wants a UIKIT package to exist, and an empty one
;;;; will do.  Each level is played: in its stored order (which must win at
;;;; each of *CHECK-STEPS*, the longest in its par time), by a player who
;;;; taps only what the hint button says (which must win too), by one who
;;;; deploys a box only when its ants have something to reach, by one who
;;;; deploys the first box it sees (which may well lose), and by one who taps
;;;; a few boxes at random and then asks for hints -- which have to find
;;;; their way back, and are tallied.

(defpackage #:uikit (:use #:cl))
(let ((here (directory-namestring (or *load-truename* *default-pathname-defaults*))))
  (load (merge-pathnames "package.lisp" here))
  (load (merge-pathnames "model.lisp" here)))

(in-package #:ants)

(defun useful-column (game)
  "A column whose front box has a cube of its colour within reach."
  (loop for column across (game-columns game)
        for k from 0
        when (and column (reachable-cubes (game-grid game) (box-colour (first column))
                                          (game-reserved game)))
          return k))

(defun random-column (game rng)
  (let ((open (loop for column across (game-columns game) for k from 0 when column collect k)))
    (when open (nth (rng-next rng (length open)) open))))

(defun play (level &key (careful t) hinted (dt (/ 1d0 30)) (rng (make-rng 1)))
  (let* ((game (new-game level))
         (time 0d0))
    (loop while (and (eq (game-state game) :playing) (< time 3600))
          do (when (free-slot game)
               (let ((k (or (and hinted (let ((hint (hint game)))
                                          (and (eq (first hint) :deploy) (second hint))))
                            (and careful (useful-column game))
                            ;; Nothing useful: a careful player still has to
                            ;; deploy something once the board is idle.
                            (and (not hinted) (or (not careful) (null (game-ants game)))
                                 (random-column game rng)))))
                 (when k (deploy game k))))
             (step-game game dt)
             (take-events game)
             (incf time dt))
    (values (game-state game) time (game-cubes-left game) (game-seed game))))

(defun astray (level n seed)
  "Tap N columns at random, then only what the hints say.  What the first
hint after said, and how the round ended: :WON, :LOST, or :GAVE-UP when a
hint found nothing to tap."
  (let ((game (new-game level)) (rng (make-rng seed)) (said nil) (k 0))
    (loop while (and (eq (game-state game) :playing) (< (game-time game) 3600))
          do (when (free-slot game)
               (if (< k n)
                   (let ((k* (random-column game rng)))
                     (when k* (deploy game k*) (incf k)))
                   (let ((hint (hint game)))
                     (unless said (setf said (first hint)))
                     (case (first hint)
                       (:deploy (deploy game (second hint)))
                       ((:lost :unsure) (return-from astray (list said :gave-up)))))))
             (step-game game *replay-dt*)
             (take-events game))
    (list said (game-state game))))

(defvar *failures* '())
(defvar *astray* (make-hash-table :test 'equal))

(dolist (level *levels*)
  (let ((start (get-internal-real-time)))
    (new-game level)
    (format t "~&deal ~a: ~,2fs~%" (level-name level)
            (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
  (let ((boxes (loop for column in (deal (parse-picture (level-rows level)) (level-columns level) 1)
                     sum (length column))))
    (let ((game (new-game level)))
      (unless (level-order level)
        (push (format nil "~a has no order" (level-name level)) *failures*))
      (multiple-value-bind (won time) (play-out-checked game (game-plan game))
        (unless won (push (level-name level) *failures*))
        ;; The par must be this time: stale, it gives stars too easily or never.
        (unless (eql (level-par level) (ceiling time))
          (format t "   par ~a, but its order takes ~d~%" (level-par level) (ceiling time))
          (push (format nil "~a's par" (level-name level)) *failures*))
        (format t "~10a ~3d cubes ~2d boxes seed ~a~%   its order: ~:[lost~;won~] after ~,1fs~%"
                (level-name level) (cube-count (parse-picture (level-rows level))) boxes
                (game-seed game) won time)))
    (multiple-value-bind (state time left) (play level :hinted t)
      (format t "   following hints: ~a after ~,1fs, ~d left~%" state time left)
      (unless (eq state :won) (push (format nil "~a's hints" (level-name level)) *failures*)))
    (multiple-value-bind (state time left) (play level)
      (format t "   careful: ~a after ~,1fs, ~d left~%" state time left))
    (format t "  random player won ~d of 10~%"
            (loop for trial below 10
                  count (eq :won (play level :careful nil :rng (make-rng (+ 1000 trial))))))
    (dotimes (trial 3)
      (let ((start (get-internal-real-time))
            (result (astray level 3 (+ 50 trial))))
        (incf (gethash result *astray* 0))
        (format t "  astray ~d: ~{~(~a~)~^ then ~} (~,1fs)~%" trial result
                (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))

(format t "~&Hints after three random taps, first hint and how it ended:~%")
(maphash (lambda (result n) (format t "  ~{~(~a~)~^ then ~}: ~d~%" result n)) *astray*)

;;; Every deal must be one the solver vouched for, and its order must win.
(cond (*failures*
       (format t "~&FAILED: ~{~a~^, ~}~%" (reverse *failures*))
       #+sbcl (sb-ext:exit :code 1) #+ecl (ext:quit 1) #-(or sbcl ecl) (error "failed"))
      (t (format t "~&All ~d levels won in their orders.~%" (length *levels*))))
