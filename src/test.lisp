;;;; test.lisp -- play every level headless, on the host.
;;;;
;;;;   sbcl --script src/test.lisp      (or ecl --shell src/test.lisp)
;;;;
;;;; The model has no Objective-C in it, so it loads without objc; the
;;;; package's UI nickname wants a UIKIT package to exist, and an empty one
;;;; will do.  Each level is played three times: in the order the solver
;;;; found when it dealt (which must win), by a player who deploys a box only
;;;; when its ants have something to reach, and by one who deploys the first
;;;; box it sees (which may well lose).

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

(defun winning-order (grid columns slots)
  "The columns, in deployment order, of a way to clear GRID; SOLVABLE-P's
search, remembering its moves."
  (let ((seen (make-hash-table :test 'equal))
        (columns (coerce columns 'vector)))
    (labels ((search-from (grid pointers deployed)
               (cond ((zerop (cube-count grid)) (values t '()))
                     ((gethash pointers seen) nil)
                     (t
                      (setf (gethash pointers seen) t)
                      (when (< (length deployed) slots)
                        (loop for k from 0
                              for p in pointers
                              for column = (aref columns k)
                              when (< p (length column))
                                do (let* ((grid (copy-grid grid))
                                          (box (nth p column))
                                          (deployed (settle grid (cons (cons (first box) (rest box))
                                                                       (copy-alist deployed))))
                                          (pointers (copy-list pointers)))
                                     (incf (nth k pointers))
                                     (multiple-value-bind (won moves) (search-from grid pointers deployed)
                                       (when won (return (values t (cons k moves))))))))))))
      (nth-value 1 (search-from (copy-grid grid) (make-list (length columns) :initial-element 0) '())))))

(defun play (level &key (careful t) solved (dt (/ 1d0 30)) (rng (make-rng 1)))
  (let* ((game (new-game level))
         (order (when solved
                  (winning-order (game-grid game)
                                 (loop for column across (game-columns game)
                                       collect (loop for box in column
                                                     collect (cons (box-colour box) (box-count box))))
                                 (level-slots level))))
         (time 0d0))
    (loop while (and (eq (game-state game) :playing) (< time 3600))
          do (when (free-slot game)
               (let ((k (or (and solved (pop order))
                            (and careful (useful-column game))
                            ;; Nothing useful: a careful player still has to
                            ;; deploy something once the board is idle.
                            (and (not solved) (or (not careful) (null (game-ants game)))
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
    (multiple-value-bind (state time left seed) (play level :solved t)
      (format t "~10a ~3d cubes ~2d boxes seed ~a~%   solver's order: ~a after ~,1fs, ~d left~%"
              (level-name level) (cube-count (parse-picture (level-rows level))) boxes seed
              state time left))
    (multiple-value-bind (state time left) (play level)
      (format t "   careful: ~a after ~,1fs, ~d left~%" state time left))
    (format t "  random player won ~d of 10~%"
            (loop for trial below 10
                  count (eq :won (play level :careful nil :rng (make-rng (+ 1000 trial))))))))
