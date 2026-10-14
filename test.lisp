;;;; test.lisp -- play every level headless, on the host.
;;;;
;;;;   sbcl --script test.lisp      (or ecl --shell test.lisp)
;;;;
;;;; The model has no Objective-C in it, so it loads without objc; the
;;;; package's UI nickname wants a UIKIT package to exist, and an empty one
;;;; will do.  Each level is played twice: by a player who deploys a box only
;;;; when its ants have something to reach (which should win), and by one who
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

(defun play (level &key (careful t) (dt (/ 1d0 30)) (rng (make-rng 1)))
  (let ((game (new-game level))
        (time 0d0))
    (loop while (and (eq (game-state game) :playing) (< time 3600))
          do (when (free-slot game)
               (let ((k (or (and careful (useful-column game))
                            ;; Nothing useful: a careful player still has to
                            ;; deploy something once the board is idle.
                            (and (or (not careful) (null (game-ants game)))
                                 (random-column game rng)))))
                 (when k (deploy game k))))
             (step-game game dt)
             (take-events game)
             (incf time dt))
    (values (game-state game) time (game-cubes-left game) (game-seed game))))

(dolist (level *levels*)
  (let ((start (get-internal-real-time)))
    (new-game level)
    (format t "~&deal ~a: ~,2fs~%" (level-name level)
            (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
  (let ((boxes (loop for column in (deal (parse-picture (level-rows level)) (level-columns level) 1)
                     sum (length column))))
    (multiple-value-bind (state time left seed) (play level)
      (format t "~10a ~3d cubes ~2d boxes seed ~a~%   careful: ~a after ~,1fs, ~d left~%"
              (level-name level) (cube-count (parse-picture (level-rows level))) boxes seed
              state time left))
    (format t "  random player won ~d of 10~%"
            (loop for trial below 10
                  count (eq :won (play level :careful nil :rng (make-rng (+ 1000 trial))))))))
