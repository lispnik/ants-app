;;;; model.lisp -- the rules, with no Objective-C in them.
;;;;
;;;; A picture of coloured cubes sits on a plate.  Below it is a row of
;;;; deployment slots, and below that a queue of ant boxes in columns.  A box
;;;; tapped at the front of its column goes to a free slot; its ants walk out,
;;;; find a cube of their colour they can reach -- through the gaps other ants
;;;; have already made -- carry it to a hole and jump in.  Each ant that jumps
;;;; takes one off the box's count, and a box at zero leaves its slot.
;;;;
;;;; Everything here is plain Lisp over numbers, so a whole round can be
;;;; played headless on the host (see test.lisp).  The view reads the game's
;;;; geometry and drains its events; it decides nothing.

(in-package #:ants)

;;; ------------------------------------------------------------------
;;; geometry of the scene, in SpriteKit's points (y up)

(defparameter *scene-width* 390d0)
(defparameter *scene-height* 800d0)
(defparameter *plate-top* 742d0 "Top edge of the picture.")
(defparameter *plate-room* 280d0 "Height the picture may take below *PLATE-TOP*.")
(defparameter *slot-y* 372d0)
(defparameter *slot-step* 70d0)
(defparameter *queue-y* 282d0 "Centre of the front row of the queue.")
(defparameter *queue-step* 64d0)
(defparameter *column-step* 100d0)
(defparameter *queue-visible* 4 "Rows of each queue column drawn.")
(defparameter *box-width* 60d0)
(defparameter *box-height* 52d0)

;;; ------------------------------------------------------------------
;;; pacing

(defparameter *ant-speed* 150d0 "Points per second, for an ant of average pace.")
(defparameter *pace-spread* 0.15d0 "How much faster or slower than average an ant may be.")
(defparameter *wander* 0.3d0
  "How far from a cell's centre, in cells, an ant's route may pass.  Under a
half, so a straight line between two neighbouring cells stays in them.")
(defparameter *spawn-interval* 0.2d0 "Seconds between ants leaving one box.")
(defparameter *max-out* 4 "Ants of one box abroad at once.")
(defparameter *rest-chance* 20 "Percent of ants that rest once on the way home.")
(defparameter *rest-seconds* '(2d0 . 4d0) "Shortest and longest rest.")
(defparameter *stuck-delay* 0.8d0
  "Seconds the board must be stuck before the round is lost, so that the
moment between one ant jumping and the next leaving does not count.")

;;; ------------------------------------------------------------------
;;; colours and pictures

(defparameter *palette*
  '((#\R 0.91 0.24 0.22)                ; red
    (#\O 0.98 0.55 0.16)                ; orange
    (#\Y 0.99 0.84 0.22)                ; yellow
    (#\G 0.33 0.72 0.30)                ; green
    (#\C 0.45 0.82 0.93)                ; sky
    (#\B 0.20 0.45 0.90)                ; blue
    (#\P 0.96 0.50 0.74)                ; pink
    (#\T 0.95 0.84 0.66)                ; tan
    (#\N 0.52 0.33 0.19)                ; brown
    (#\W 0.97 0.97 0.95)                ; white
    (#\K 0.20 0.18 0.22))               ; black
  "A picture's characters and their colours; #\\. is no cube.")

(defun colour-rgb (char)
  (rest (assoc char *palette*)))

(defun light-colour-p (char)
  (destructuring-bind (r g b) (colour-rgb char)
    (> (+ (* 0.3 r) (* 0.59 g) (* 0.11 b)) 0.7)))

(defstruct level
  name
  rows                                  ; the picture, top row first
  (slots 5)                             ; deployment slots
  (columns 3)                           ; queue columns
  (difficulty 0)                        ; how strongly buried colours come first
  (seed 1))                             ; first seed tried for the queue

(defparameter *levels*
  (list
   (make-level :name "Mushroom" :slots 3 :seed 11
               :rows '("....RRRR...."
                       "..RRWWRRRR.."
                       ".RRWWWWRRRR."
                       ".RRRWWRRWWR."
                       "RRRRRRRWWWWR"
                       "RWWRRRRRWWRR"
                       "RWWWRRRRRRRR"
                       ".RRRRRRRRRR."
                       "...TTTTTT..."
                       "...TKTTKT..."
                       "...TTTTTT..."
                       "....TTTT...."))
   (make-level :name "Heart" :difficulty 0.25 :slots 3 :seed 30
               :rows '("BBBBBBBBBBBBB"
                       "BBRRRBBBRRRBB"
                       "BRRWRRBRRRRRB"
                       "BRWWRRRRRRRRB"
                       "BRRWRRRRRRRRB"
                       "BRRRRRRRRRRRB"
                       "BBRRRRRRRRRBB"
                       "BBBRRRRRRRBBB"
                       "BBBBRRRRRBBBB"
                       "BBBBBRRRBBBBB"
                       "BBBBBBRBBBBBB"
                       "BBBBBBBBBBBBB"))
   (make-level :name "Flower" :difficulty 0.5 :slots 4 :seed 5
               :rows '("CCCCCCCCCCCCCC"
                       "CCCCCPPPPCCCCC"
                       "CCCPPPPPPPPCCC"
                       "CCPPPPYYPPPPCC"
                       "CCPPPYYYYPPPCC"
                       "CCPPPYYYYPPPCC"
                       "CCPPPPYYPPPPCC"
                       "CCCPPPPPPPPCCC"
                       "CCCCCPPPPCCCCC"
                       "CCGGCCCGCCCCCC"
                       "CCCGGCCGCCGGCC"
                       "CCCCGGCGCGGCCC"
                       "CCCCCGGGGGCCCC"
                       "NNNNNNNGNNNNNN"))
   (make-level :name "Fish" :difficulty 0.75 :slots 4 :seed 3
               :rows '("BBBBBBBBBBBBWB"
                       "BBBBBBYYYBBWBB"
                       "BBBBBOOOOOBBBB"
                       "BBBBOOOOOOOBBB"
                       "BOBOOOOOOWKOBB"
                       "BOOOOYOOOWWOOB"
                       "BOBOOYOOOOOOOB"
                       "BBBBOOOOOOOOBB"
                       "BBBBBOOOOOOBBB"
                       "BBBBBBYYYBBBBB"
                       "GBBBBBBBBBBBBG"
                       "GGBBBGBBBBGBGG"
                       "NGGNNGGNNNGGGN"))))

(defun parse-picture (rows)
  "A 2D array (rows columns) of colour characters, NIL where there is no cube."
  (let* ((h (length rows))
         (w (length (first rows)))
         (grid (make-array (list h w) :initial-element nil)))
    (loop for row in rows
          for r from 0
          do (assert (= (length row) w) () "Row ~d of the picture is ~d wide, not ~d." r (length row) w)
             (loop for char across row
                   for c from 0
                   unless (char= char #\.)
                     do (assert (colour-rgb char) () "No colour for ~s in the picture." char)
                        (setf (aref grid r c) char)))
    grid))

(defun copy-grid (grid)
  (let ((new (make-array (array-dimensions grid))))
    (dotimes (i (array-total-size grid) new)
      (setf (row-major-aref new i) (row-major-aref grid i)))))

(defun cube-count (grid)
  (loop for i below (array-total-size grid) count (row-major-aref grid i)))

;;; ------------------------------------------------------------------
;;; a pseudo-random sequence of our own, so a level deals the same queue on
;;; every restart and on every Lisp

(defstruct (rng (:constructor make-rng (state))) state)

(defun rng-next (rng limit)
  "An integer in [0, LIMIT), from the generator's high bits.  The low bits of
a power-of-two linear congruential generator repeat with short periods -- bit
k with period 2^(k+1) -- so taking the state MOD LIMIT made shuffles visibly
unfair; scaling by the whole state lets the good high bits decide."
  (setf (rng-state rng) (mod (+ (* (rng-state rng) 1103515245) 12345) 2147483648))
  (floor (* (rng-state rng) limit) 2147483648))

(defun shuffle (vector rng)
  (loop for i from (1- (length vector)) downto 1
        do (rotatef (aref vector i) (aref vector (rng-next rng (1+ i)))))
  vector)

;;; ------------------------------------------------------------------
;;; walking the plate
;;;
;;; Cells are (r c) with r down from the top.  The picture is r in [0,h),
;;; c in [0,w); around it is a ring of margin cells, r = -1 or h, c = -1 or w,
;;; where ants walk freely.  Inside, an ant may walk only where a cube was.
;;; Cubes are only ever removed, so a path, once found, stays walkable.

(defparameter *directions* '((-1 0) (1 0) (0 -1) (0 1)))

(defun walkable-p (grid r c)
  (destructuring-bind (h w) (array-dimensions grid)
    (and (<= -1 r h) (<= -1 c w)
         (or (= r -1) (= r h) (= c -1) (= c w)
             (null (aref grid r c))))))

(defun cell-index (w r c)
  (+ (* (+ r 1) (+ w 2)) (+ c 1)))

(defun index-cell (w i)
  (multiple-value-bind (q rem) (floor i (+ w 2))
    (values (1- q) (1- rem))))

(defun flood (grid r0 c0)
  "Breadth-first from (R0 C0) through walkable cells.  Two vectors indexed by
CELL-INDEX: the distance to each cell and the cell it was reached from, -1
where it was not reached."
  (destructuring-bind (h w) (array-dimensions grid)
    (let* ((n (* (+ h 2) (+ w 2)))
           (dist (make-array n :initial-element -1))
           (parent (make-array n :initial-element -1))
           (queue (make-array n :initial-element 0))
           (head 0)
           (tail 1))
      (setf (aref queue 0) (cell-index w r0 c0)
            (aref dist (aref queue 0)) 0)
      (loop while (< head tail)
            do (let ((i (aref queue head)))
                 (incf head)
                 (multiple-value-bind (r c) (index-cell w i)
                   (loop for (dr dc) in *directions*
                         for nr = (+ r dr)
                         for nc = (+ c dc)
                         when (walkable-p grid nr nc)
                           do (let ((j (cell-index w nr nc)))
                                (when (= (aref dist j) -1)
                                  (setf (aref dist j) (1+ (aref dist i))
                                        (aref parent j) i
                                        (aref queue tail) j)
                                  (incf tail)))))))
      (values dist parent))))

(defun trace-path (parent w goal)
  "The cells from the flood's start to GOAL, as a list of (r c)."
  (let ((path '()))
    (loop for i = goal then (aref parent i)
          until (= i -1)
          do (push (multiple-value-list (index-cell w i)) path))
    path))

(defun reachable-cubes (grid colour &optional reserved (r0 -1) (c0 -1))
  "The cubes of COLOUR an ant starting at (R0 C0) can reach, nearest first,
as a list of (distance r c neighbour-index), skipping any RESERVED.  The
start must be walkable; a margin cell always is, and all the margin is
connected, so the default start reaches whatever any start can."
  (destructuring-bind (h w) (array-dimensions grid)
    (let ((dist (flood grid r0 c0))
          (found '()))
      (dotimes (r h)
        (dotimes (c w)
          (when (and (eql (aref grid r c) colour)
                     (not (and reserved (aref reserved r c))))
            (let ((best nil))
              (loop for (dr dc) in *directions*
                    for nr = (+ r dr)
                    for nc = (+ c dc)
                    when (walkable-p grid nr nc)
                      do (let* ((j (cell-index w nr nc))
                                (d (aref dist j)))
                           (when (and (>= d 0) (or (null best) (< d (first best))))
                             (setf best (list d r c j)))))
              (when best (push best found))))))
      (sort found #'< :key #'first))))

;;; ------------------------------------------------------------------
;;; is a queue winnable?
;;;
;;; An abstract round: a deployed box at once takes every cube of its colour
;;; it can reach, again and again as the taking uncovers more, until it is
;;; empty or nothing more is reachable.  A depth-first search over which
;;; column to deploy from next, remembered by how far into each column it
;;; has got, says whether some order clears the plate.  It is not exactly the
;;; game -- there, two boxes of one colour share cubes as their ants happen to
;;; arrive -- but it is close enough to keep the dealer from handing out a
;;; queue nobody could win.

(defun settle (grid deployed)
  "Let the DEPLOYED boxes, a list of (colour . remaining), take what they can
from GRID, which is changed.  The boxes still not empty."
  (loop for progress = nil
        do (dolist (box deployed)
             (loop while (plusp (rest box))
                   for cubes = (reachable-cubes grid (first box))
                   while cubes
                   do (loop for (nil r c) in cubes
                            while (plusp (rest box))
                            do (setf (aref grid r c) nil)
                               (decf (rest box))
                               (setf progress t))))
        while progress)
  (remove-if #'zerop deployed :key #'rest))

(defun solvable-p (grid columns slots)
  "Whether some order of deployment from COLUMNS, each a list of
(colour . count), clears GRID with SLOTS slots."
  (let ((seen (make-hash-table :test 'equal))
        (columns (coerce columns 'vector)))
    (labels ((search-from (grid pointers deployed)
               (cond ((zerop (cube-count grid)) t)
                     ((gethash pointers seen) nil)
                     (t
                      (setf (gethash pointers seen) t)
                      (and (< (length deployed) slots)
                           (loop for k from 0
                                 for p in pointers
                                 for column = (aref columns k)
                                 thereis (when (< p (length column))
                                           (let* ((grid (copy-grid grid))
                                                  (box (nth p column))
                                                  (deployed (settle grid (cons (cons (first box) (rest box))
                                                                               (copy-alist deployed))))
                                                  (pointers (copy-list pointers)))
                                             (incf (nth k pointers))
                                             (search-from grid pointers deployed)))))))))
      (search-from (copy-grid grid) (make-list (length columns) :initial-element 0) '()))))

;;; ------------------------------------------------------------------
;;; dealing the boxes

(defparameter *box-sizes* '(5 . 12) "Smallest and largest usual box.")

(defun split-count (n rng)
  "N ants of one colour as a list of box sizes, mostly within *BOX-SIZES*."
  (let ((sizes '())
        (low (car *box-sizes*))
        (high (cdr *box-sizes*)))
    (loop while (> n high)
          do (let ((size (+ low (rng-next rng (1+ (- high low))))))
               (push size sizes)
               (decf n size)))
    (if (and sizes (< n 4))
        (incf (first sizes) n)
        (push n sizes))
    sizes))

(defun colour-depths (grid)
  "An alist of each colour to the mean distance of its cubes from the edge."
  (destructuring-bind (h w) (array-dimensions grid)
    (let ((sums '()))
      (dotimes (r h)
        (dotimes (c w)
          (let ((colour (aref grid r c)))
            (when colour
              (let ((entry (or (assoc colour sums)
                               (first (push (list colour 0 0) sums)))))
                (incf (second entry) (min (1+ r) (1+ c) (- h r) (- w c)))
                (incf (third entry)))))))
      (loop for (colour sum n) in sums
            collect (cons colour (/ sum n))))))

(defun deal (grid columns seed &optional (difficulty 0))
  "COLUMNS lists of (colour . count): exactly as many ants of each colour as
there are cubes of it, shuffled by SEED.  The more DIFFICULTY, the more the
colours buried deepest in the picture are dealt to the front, where they
fill slots with ants that have nothing to reach yet."
  (let* ((rng (make-rng seed))
         (depths (colour-depths grid))
         (boxes '()))
    (dolist (entry *palette*)
      (let ((n (loop for i below (array-total-size grid)
                     count (eql (row-major-aref grid i) (first entry)))))
        (when (plusp n)
          (dolist (size (split-count n rng))
            (push (cons (first entry) size) boxes)))))
    (let* ((keyed (map 'vector
                       (lambda (box)
                         (cons (+ (* difficulty (rest (assoc (first box) depths)))
                                  (/ (rng-next rng 1000) 500))
                               box))
                       (shuffle (coerce (nreverse boxes) 'vector) rng)))
           (boxes (map 'vector #'rest (stable-sort keyed #'> :key #'first)))
           (dealt (make-list columns)))
      (loop for box across boxes
            for i from 0
            do (push box (nth (mod i columns) dealt)))
      (mapcar #'nreverse dealt))))

(defun deal-winnable (grid level)
  "A deal that SOLVABLE-P accepts, trying seeds from the level's own; the
seed used is the second value."
  (loop for seed from (level-seed level)
        repeat 60
        for columns = (deal grid (level-columns level) seed (level-difficulty level))
        when (solvable-p grid columns (level-slots level))
          do (return-from deal-winnable (values columns seed)))
  (values (deal grid (level-columns level) (level-seed level)) nil))

;;; ------------------------------------------------------------------
;;; the game

(defstruct box
  colour
  count                                 ; ants still to jump down a hole
  (out 0)                               ; ants abroad
  (clock 0d0)                           ; until the next ant may leave
  slot                                  ; deployment slot, once deployed
  node label)                           ; the view's

(defstruct ant
  box
  x y
  path                                  ; points still to walk, as (x . y)
  (heading 0d0)
  phase                                 ; :out, or :home with a cube
  cube-r cube-c                         ; the cube it is going for
  cell                                  ; the cell beside it, (r c)
  hole                                  ; the hole it is going to, (x . y)
  (pace 1d0)                            ; its speed, as a multiple of *ANT-SPEED*
  (walked 0d0)                          ; distance so far, for the view's gait
  (gait 0d0)                            ; where in its stride it started
  rest-at                               ; WALKED at which it will rest, or NIL
  (rest 0d0)                            ; seconds of rest left
  node load dropped)                    ; the view's: the ant, its cube carried, and set down

(defstruct game
  level
  grid reserved w h
  cell-size left
  columns                               ; vector of lists of boxes, front first
  slots                                 ; vector of box or NIL
  (ants '())
  cubes-left
  (state :playing)                      ; :playing :won :lost
  (stuck 0d0)
  seed
  rng                                   ; for the ants' wandering
  (events '()))

(defun emit (game &rest event)
  (push event (game-events game)))

(defun take-events (game)
  (prog1 (nreverse (game-events game))
    (setf (game-events game) '())))

;;; Where things are.

(defun cell-x (game c)
  (+ (game-left game) (* (+ c 0.5d0) (game-cell-size game))))

(defun cell-y (game r)
  (- *plate-top* (* (+ r 0.5d0) (game-cell-size game))))

(defun cell-point (game r c)
  (cons (cell-x game c) (cell-y game r)))

(defun slot-x (game slot)
  (+ (/ *scene-width* 2)
     (* (- slot (/ (1- (length (game-slots game))) 2)) *slot-step*)))

(defun column-x (game column)
  (+ (/ *scene-width* 2)
     (* (- column (/ (1- (length (game-columns game))) 2)) *column-step*)))

(defun queue-y (row)
  (- *queue-y* (* row *queue-step*)))

(defun hole-point (game side)
  "The hole at the bottom-left (SIDE :left) or bottom-right corner of the plate."
  (cons (cell-x game (if (eq side :left) -1 (game-w game)))
        (- (cell-y game (game-h game)) (game-cell-size game) 8)))

(defparameter *hole-rim* 16d0
  "How far above a hole's centre an ant stops to jump in: the hole's half
height and the ant's, so that it reaches the edge without walking onto it.")

(defun hole-rim (game side)
  "Where an ant coming down from the corner of the margin stands to jump."
  (destructuring-bind (x . y) (hole-point game side)
    (cons x (+ y *hole-rim*))))

(defun entry-column (game slot)
  "The column of the plate's bottom margin nearest SLOT, where its ants step on."
  (max 0 (min (1- (game-w game))
              (round (- (/ (- (slot-x game slot) (game-left game)) (game-cell-size game)) 0.5d0)))))

(defun new-game (level)
  (let* ((grid (parse-picture (level-rows level)))
         (h (array-dimension grid 0))
         (w (array-dimension grid 1))
         (size (min 26d0 (/ 370d0 (+ w 2)) (/ *plate-room* h))))
    (multiple-value-bind (columns seed) (deal-winnable grid level)
      (make-game :level level
                 :grid grid
                 :reserved (make-array (list h w) :initial-element nil)
                 :w w :h h
                 :cell-size size
                 :left (- (/ *scene-width* 2) (/ (* w size) 2))
                 :columns (map 'vector
                               (lambda (column)
                                 (loop for (colour . count) in column
                                       collect (make-box :colour colour :count count)))
                               columns)
                 :slots (make-array (level-slots level) :initial-element nil)
                 :cubes-left (cube-count grid)
                 :seed seed
                 :rng (make-rng (or seed (level-seed level)))))))

;;; Taps.

(defun free-slot (game)
  (position nil (game-slots game)))

(defun deploy (game column)
  "Send the front box of COLUMN to the first free slot.  The box, or NIL when
there is no box or no slot."
  (let ((box (first (aref (game-columns game) column)))
        (slot (free-slot game)))
    (when (and box slot (eq (game-state game) :playing))
      (pop (aref (game-columns game) column))
      (setf (aref (game-slots game) slot) box
            (box-slot box) slot
            (box-clock box) 0.3d0)
      (emit game :deploy box slot column)
      box)))

;;; Ants.

(defun box-point (game box)
  "Where a deployed box's ants come out: its door."
  (cons (slot-x game (box-slot box)) (+ *slot-y* (/ *box-height* 2) -4)))

(defun find-target (game box)
  "The nearest cube an ant of BOX can reach, and the cells to walk to it."
  (let* ((grid (game-grid game))
         (w (game-w game))
         (r0 (game-h game))
         (c0 (entry-column game (box-slot box)))
         (best (first (reachable-cubes grid (box-colour box) (game-reserved game) r0 c0))))
    (when best
      (destructuring-bind (d r c j) best
        (declare (ignore d))
        (multiple-value-bind (dist parent) (flood grid r0 c0)
          (declare (ignore dist))
          (values r c (trace-path parent w j)))))))

(defun random-between (rng low high)
  (+ low (* (- high low) (/ (rng-next rng 10000) 10000d0))))

(defun wander-point (game cell)
  "Somewhere near the middle of CELL, (r c): a waypoint no two ants share."
  (destructuring-bind (x . y) (apply #'cell-point game cell)
    (let ((rng (game-rng game))
          (spread (* *wander* (game-cell-size game))))
      (cons (+ x (random-between rng (- spread) spread))
            (+ y (random-between rng (- spread) spread))))))

(defun send-ant (game box)
  (multiple-value-bind (r c cells) (find-target game box)
    (when r
      (setf (aref (game-reserved game) r c) t)
      (let* ((door (box-point game box))
             (beside (first (last cells)))
             (from (apply #'cell-point game beside))
             (to (cell-point game r c))
             ;; Stop at the cube's face, not in its middle.
             (reach (cons (+ (car from) (* 0.45d0 (- (car to) (car from))))
                          (+ (cdr from) (* 0.45d0 (- (cdr to) (cdr from))))))
             (rng (game-rng game))
             (ant (make-ant :box box :x (car door) :y (cdr door)
                            :phase :out :cube-r r :cube-c c :cell beside
                            :pace (random-between rng (- 1 *pace-spread*) (+ 1 *pace-spread*))
                            :gait (random-between rng 0d0 (* 2 pi))
                            :path (append (mapcar (lambda (cell) (wander-point game cell)) cells)
                                          (list reach)))))
        (incf (box-out box))
        (push ant (game-ants game))
        (emit game :spawn ant)
        ant))))

(defun path-home (game ant)
  "From beside the cube to the edge of the nearer hole, by the bottom corner
of the margin.  The jump in from the edge is the view's to draw."
  (destructuring-bind (r c) (ant-cell ant)
    (let* ((w (game-w game))
           (side (if (< c (/ w 2)) :left :right))
           (exit-c (if (eq side :left) -1 w)))
      (setf (ant-hole ant) (hole-point game side))
      (multiple-value-bind (dist parent) (flood (game-grid game) r c)
        (declare (ignore dist))
        (append (mapcar (lambda (cell) (wander-point game cell))
                        (trace-path parent w (cell-index w (game-h game) exit-c)))
                (list (hole-rim game side)))))))

(defun path-length (x y path)
  "How far it is from (X Y) along PATH."
  (loop for (px . py) in path
        sum (prog1 (sqrt (+ (expt (- px x) 2) (expt (- py y) 2)))
              (setf x px y py))))

(defun plan-rest (game ant)
  "Decide, as the ant turns for home, whether and where along the way it
stops to rest."
  (let ((rng (game-rng game)))
    (when (< (rng-next rng 100) *rest-chance*)
      (setf (ant-rest-at ant)
            (+ (ant-walked ant)
               (* (random-between rng 0.2d0 0.7d0)
                  (path-length (ant-x ant) (ant-y ant) (ant-path ant))))))))

(defun walk (game ant dt)
  "One frame of one ant: resting, or walking -- and starting to rest when it
has come far enough, unless only the last step to the hole is left."
  (cond ((plusp (ant-rest ant))
         (when (<= (decf (ant-rest ant) dt) 0)
           (setf (ant-rest ant) 0d0)
           (emit game :rest-end ant)))
        ((advance ant (* *ant-speed* (ant-pace ant) dt))
         (arrive game ant))
        ((and (ant-rest-at ant)
              (>= (ant-walked ant) (ant-rest-at ant)))
         (setf (ant-rest-at ant) nil)
         (when (rest (ant-path ant))
           (setf (ant-rest ant) (random-between (game-rng game) (car *rest-seconds*) (cdr *rest-seconds*)))
           (emit game :rest-start ant)))))

(defun advance (ant distance)
  "Walk DISTANCE along the ant's path.  True when the path is done."
  (loop while (and (plusp distance) (ant-path ant))
        do (destructuring-bind (tx . ty) (first (ant-path ant))
             (let* ((dx (- tx (ant-x ant)))
                    (dy (- ty (ant-y ant)))
                    (d (sqrt (+ (* dx dx) (* dy dy)))))
               (when (> d 1d-6)
                 (setf (ant-heading ant) (atan dy dx)))
               (incf (ant-walked ant) (min d distance))
               (if (<= d distance)
                   (setf (ant-x ant) tx
                         (ant-y ant) ty
                         distance (- distance d)
                         (ant-path ant) (rest (ant-path ant)))
                   (setf (ant-x ant) (+ (ant-x ant) (* dx (/ distance d)))
                         (ant-y ant) (+ (ant-y ant) (* dy (/ distance d)))
                         distance 0)))))
  (null (ant-path ant)))

(defun arrive (game ant)
  "The ant is at the end of its path: at its cube, or at a hole."
  (ecase (ant-phase ant)
    (:out
     (let ((r (ant-cube-r ant)) (c (ant-cube-c ant)))
       (setf (aref (game-grid game) r c) nil
             (aref (game-reserved game) r c) nil)
       (decf (game-cubes-left game))
       (setf (ant-phase ant) :home
             (ant-path ant) (path-home game ant))
       (plan-rest game ant)
       (emit game :pickup ant r c)))
    (:home
     (let ((box (ant-box ant)))
       (setf (game-ants game) (remove ant (game-ants game)))
       (decf (box-out box))
       (decf (box-count box))
       (emit game :escape ant)
       (when (zerop (box-count box))
         (setf (aref (game-slots game) (box-slot box)) nil)
         (emit game :box-done box))))))

(defun stuck-p (game)
  "Nothing is moving, no box can be deployed, and no deployed box can reach
a cube of its colour."
  (and (plusp (game-cubes-left game))
       (null (game-ants game))
       (or (null (free-slot game))
           (every #'null (game-columns game)))
       (loop for box across (game-slots game)
             never (and box (find-target game box)))))

(defun step-game (game dt)
  (when (eq (game-state game) :playing)
    ;; Boxes send ants.
    (loop for box across (game-slots game)
          when box
            do (decf (box-clock box) dt)
               (when (and (<= (box-clock box) 0)
                          (< (box-out box) (min *max-out* (box-count box))))
                 (setf (box-clock box) *spawn-interval*)
                 (send-ant game box)))
    ;; Ants walk.
    (dolist (ant (game-ants game))
      (walk game ant dt))
    ;; The round ends.
    (cond ((and (zerop (game-cubes-left game)) (null (game-ants game)))
           (setf (game-state game) :won)
           (emit game :won))
          ((stuck-p game)
           (when (>= (incf (game-stuck game) dt) *stuck-delay*)
             (setf (game-state game) :lost)
             (emit game :lost)))
          (t (setf (game-stuck game) 0d0)))))
