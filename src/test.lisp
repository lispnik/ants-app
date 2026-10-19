;;;; test.lisp -- play every level headless, on the host.
;;;;
;;;;   sbcl --script src/test.lisp      (or ecl --shell src/test.lisp)
;;;;
;;;; The model has no Objective-C in it, so it loads without objc; the
;;;; package's UI nickname wants a UIKIT package to exist, and an empty one
;;;; will do.  Each level is played four times: in the order the solver
;;;; found when it dealt (which must win, in its par time), by a player who
;;;; taps only what the hint button says (which must win too), by one who
;;;; deploys a box only when its ants have something to reach, and by one who
;;;; deploys the first box it sees (which may well lose).

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

(defun play (level &key (careful t) solved hinted (dt (/ 1d0 30)) (rng (make-rng 1)))
  (let* ((game (new-game level))
         (order (when solved
                  (nth-value 1 (solvable-p (game-grid game)
                                 (loop for column across (game-columns game)
                                       collect (loop for box in column
                                                     collect (cons (box-colour box) (box-count box))))
                                 (level-slots level)))))
         (time 0d0))
    (loop while (and (eq (game-state game) :playing) (< time 3600))
          do (when (free-slot game)
               (let ((k (or (and solved (pop order))
                            (and hinted (let ((hint (hint game)))
                                          (and (eq (first hint) :deploy) (second hint))))
                            (and careful (useful-column game))
                            ;; Nothing useful: a careful player still has to
                            ;; deploy something once the board is idle.
                            (and (not solved) (not hinted) (or (not careful) (null (game-ants game)))
                                 (random-column game rng)))))
                 (when k (deploy game k))))
             (step-game game dt)
             (take-events game)
             (incf time dt))
    (values (game-state game) time (game-cubes-left game) (game-seed game))))

(defvar *failures* '())

(dolist (level *levels*)
  (let ((start (get-internal-real-time)))
    (new-game level)
    (format t "~&deal ~a: ~,2fs~%" (level-name level)
            (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
  (let ((boxes (loop for column in (deal (parse-picture (level-rows level)) (level-columns level) 1)
                     sum (length column))))
    (multiple-value-bind (state time left seed) (play level :solved t)
      (unless (and seed (eq state :won)) (push (level-name level) *failures*))
      ;; The par must be this time: stale, it gives stars too easily or never.
      (unless (eql (level-par level) (ceiling time))
        (format t "   par ~a, but the solver's order takes ~d~%" (level-par level) (ceiling time))
        (push (format nil "~a's par" (level-name level)) *failures*))
      (format t "~10a ~3d cubes ~2d boxes seed ~a~%   solver's order: ~a after ~,1fs, ~d left~%"
              (level-name level) (cube-count (parse-picture (level-rows level))) boxes seed
              state time left))
    (multiple-value-bind (state time left) (play level :hinted t)
      (format t "   following hints: ~a after ~,1fs, ~d left~%" state time left)
      (unless (eq state :won) (push (format nil "~a's hints" (level-name level)) *failures*)))
    (multiple-value-bind (state time left) (play level)
      (format t "   careful: ~a after ~,1fs, ~d left~%" state time left))
    (format t "  random player won ~d of 10~%"
            (loop for trial below 10
                  count (eq :won (play level :careful nil :rng (make-rng (+ 1000 trial))))))))

;;; Every deal must be one the solver vouched for, and its order must win.
(cond (*failures*
       (format t "~&FAILED: ~{~a~^, ~}~%" (reverse *failures*))
       #+sbcl (sb-ext:exit :code 1) #+ecl (ext:quit 1) #-(or sbcl ecl) (error "failed"))
      (t (format t "~&All ~d levels won in the solver's order.~%" (length *levels*))))
