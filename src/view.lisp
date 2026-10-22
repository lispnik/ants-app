;;;; view.lisp -- the game drawn by SpriteKit.
;;;;
;;;; The scene is a Lisp subclass of SKScene whose update: steps the model
;;;; and then drains its events: a box deployed, an ant sent, a cube picked
;;;; up, an ant down a hole, a box emptied, the round over.  Each becomes a
;;;; node added, moved or removed.  Ants are the only thing moved every
;;;; frame; everything else moves by SKAction, once.
;;;;
;;;; A tap is a UITapGestureRecognizer on the SKView, its point converted
;;;; into the scene's coordinates and hit-tested here, in Lisp.

(in-package #:ants)

(defvar *view* nil "The SKView.")
(defvar *scene* nil "The scene, an ANT-SCENE.")
(defvar *world* nil "The node holding everything in the current round.")
(defvar *game* nil)
(defvar *level-index* 0)
(defvar *cubes* nil "The cube nodes, an array like the game's grid.")
(defvar *status* nil)
(defvar *overlay* nil)
(defvar *more-labels* nil "Per queue column, the label counting boxes not drawn.")
(defvar *last-time* nil)
(defvar *shown-left* nil "The cube count the status line last showed.")
(defvar *timer* nil "The label under the picture showing the round's time.")
(defvar *shown-seconds* nil "The whole seconds the timer last showed.")
(defvar *picker* nil "The level picker, while it is open.")
(defvar *settings* nil "The settings panel, while it is open.")
(defvar *fast* nil "Whether the ants go at double speed; the 2x button toggles it.")
(defvar *fast-button* nil)
(defvar *hint-button* nil)
(defvar *hints-left* 0 "Hints still to be had this round.")
(defvar *hinted* nil "Whether this round has had a hint, which costs it a star.")
(defparameter *hints-per-round* 3)
(defparameter *hint-x* 362d0 "Where the hint button is, right of the slots.")
(defparameter *settings-x* 28d0 "Where the settings button is, left of the slots.")
(defun restart-y () "The restart button, under the settings button, level with the queue's front." (queue-y 0))

;;; ------------------------------------------------------------------
;;; progress, kept in NSUserDefaults across launches
;;;
;;; "unlocked" is how many levels may be played, from the first; clearing a
;;; level unlocks the next.  It is kept uncapped, and also worked out from the
;;; levels won, so that pictures added after the last one was cleared open
;;; up.  "level" is the one being played, so the app opens where it was left
;;; -- or on the next one, if that was the newest level cleared.

(defun defaults ()
  (objc:invoke "NSUserDefaults" "standardUserDefaults"))

(defun saved (key)
  (objc:invoke (defaults) "integerForKey:" key))

(defun save (key value)
  (objc:invoke (defaults) "setInteger:forKey:" value key))

(defun cleared-p (index)
  (and (< -1 index (length *levels*))
       (plusp (best-stars (nth index *levels*)))))

(defun unlocked ()
  (let ((newest (or (position-if #'plusp *levels* :key #'best-stars :from-end t) -1)))
    (max 1 (min (length *levels*) (max (saved "unlocked") (+ newest 2))))))

(defun unlock (count)
  (when (> count (saved "unlocked"))
    (save "unlocked" count)))

(defun saved-level ()
  (let ((level (saved "level")))
    (when (and (cleared-p level) (not (cleared-p (1+ level))))
      (incf level))
    (max 0 (min level (1- (unlocked))))))

(defun best-stars (level)
  "The most stars LEVEL has been won with, 0 if never.  By name, so that
adding pictures does not shuffle them."
  (saved (format nil "stars ~a" (level-name level))))

(defun record-stars (level stars)
  (when (> stars (best-stars level))
    (save (format nil "stars ~a" (level-name level)) stars)))

(defun scoring-p ()
  "Whether rounds are scored, by time against par in stars.  Saved as its
opposite, so that it is on until turned off."
  (zerop (saved "no scoring")))

(defun star-string (stars)
  (with-output-to-string (out)
    (dotimes (i 3) (write-string (if (< i stars) "★" "☆") out))))

;;; ------------------------------------------------------------------
;;; nodes

(defun point (x y)
  (vector (float x 1d0) (float y 1d0)))

(defun rgb (char &optional (shade 1))
  (destructuring-bind (r g b) (colour-rgb char)
    (ui:color (* r shade) (* g shade) (* b shade))))

(defun add (parent child)
  (objc:invoke parent "addChild:" child)
  child)

(defun at (node x y)
  (objc:invoke node "setPosition:" (point x y))
  node)

(defun z (node z)
  (objc:invoke node "setZPosition:" (float z 1d0))
  node)

(defun rect-sprite (colour width height)
  (objc:invoke "SKSpriteNode" "spriteNodeWithColor:size:" colour (point width height)))

(defun rounded (width height radius fill &optional stroke (line-width 2))
  (let ((node (objc:invoke "SKShapeNode" "shapeNodeWithRect:cornerRadius:"
                           (vector (- (/ width 2d0)) (- (/ height 2d0))
                                   (float width 1d0) (float height 1d0))
                           (float radius 1d0))))
    (objc:invoke node "setFillColor:" fill)
    (objc:invoke node "setStrokeColor:" (or stroke (ui:color 0 0 0 0)))
    (objc:invoke node "setLineWidth:" (float line-width 1d0))
    node))

(defun ellipse (width height fill stroke)
  (let ((node (objc:invoke "SKShapeNode" "shapeNodeWithEllipseOfSize:" (point width height))))
    (objc:invoke node "setFillColor:" fill)
    (objc:invoke node "setStrokeColor:" stroke)
    (objc:invoke node "setLineWidth:" 2d0)
    node))

(defun text (string size colour &key (font "AvenirNext-Bold"))
  (let ((node (objc:invoke "SKLabelNode" "labelNodeWithFontNamed:" font)))
    (objc:invoke node "setText:" string)
    (objc:invoke node "setFontSize:" (float size 1d0))
    (objc:invoke node "setFontColor:" colour)
    (objc:invoke node "setVerticalAlignmentMode:" 1) ; centre
    node))

;;; SKActions.

(defun action (name &rest arguments)
  (apply #'objc:invoke "SKAction" name arguments))

(defun run (node &rest actions)
  "Run ACTIONS on NODE one after another."
  (objc:invoke node "runAction:"
               (if (rest actions)
                   (action "sequence:" (coerce actions 'vector))
                   (first actions))))

(defun move-to (x y &optional (seconds 0.2d0))
  (action "moveTo:duration:" (point x y) (float seconds 1d0)))

(defun scale-to (scale &optional (seconds 0.15d0))
  (action "scaleTo:duration:" (float scale 1d0) (float seconds 1d0)))

(defun vanish (node &optional (seconds 0.15d0))
  "Shrink NODE away and take it out of the scene.  Nothing may touch it after."
  (run node (scale-to 0 seconds) (action "removeFromParent")))

(defun jump-in (node hole)
  "NODE, standing at the edge of HOLE, drops into it: down to the middle,
shrinking and fading as it goes, then out of the scene."
  (z node 1)                                  ; under the ants still walking
  (run node
       (action "group:" (vector (move-to (car hole) (cdr hole) 0.22d0)
                                (scale-to 0.2d0 0.22d0)
                                (action "fadeOutWithDuration:" 0.22d0)))
       (action "removeFromParent")))

(defun shake (node)
  (run node
       (action "moveByX:y:duration:" 6d0 0d0 0.04d0)
       (action "moveByX:y:duration:" -12d0 0d0 0.08d0)
       (action "moveByX:y:duration:" 6d0 0d0 0.04d0)))

(defun vibration-p ()
  "Whether to buzz; the settings can turn it off."
  (zerop (saved "no vibration")))

;;; The feedback generators, made once each: a fresh one per buzz is slow to
;;; wake and is never freed.

(defvar *impacts* (make-hash-table) "A UIImpactFeedbackGenerator per style.")
(defvar *notifier* nil "The UINotificationFeedbackGenerator.")

(defun haptic (&optional (style 0) intensity)
  "A tap: STYLE 0 light, 1 medium, 2 heavy; INTENSITY, from 0 to 1, softens it."
  (when (vibration-p)
    (let ((generator (or (gethash style *impacts*)
                         (setf (gethash style *impacts*)
                               (ui:keep (objc:invoke (objc:invoke "UIImpactFeedbackGenerator" "alloc")
                                                     "initWithStyle:" style))))))
      (if intensity
          (objc:invoke generator "impactOccurredWithIntensity:" (float intensity 1d0))
          (objc:invoke generator "impactOccurred")))))

(defun notify (kind)
  "The system's pattern for KIND, :success or :error."
  (when (vibration-p)
    (objc:invoke (or *notifier*
                     (setf *notifier* (ui:keep (objc:invoke (objc:invoke "UINotificationFeedbackGenerator" "alloc")
                                                            "init"))))
                 "notificationOccurred:" (ecase kind (:success 0) (:error 2)))))

;;; Sounds, from res/sounds: tools/make-sounds.py.  The audio session is
;;; ambient, so they mix with the player's music and the ring/silent switch
;;; silences them.

(defvar *sounds* (make-hash-table :test 'equal) "An SKAction per sound, made once.")
(defvar *last-plop* 0)

(defun sound (name)
  (when (zerop (saved "no sound"))
    (let ((play (or (gethash name *sounds*)
                    (setf (gethash name *sounds*)
                          (ui:keep (action "playSoundFileNamed:waitForCompletion:"
                                           (format nil "~a.wav" name) nil))))))
      (objc:invoke *scene* "runAction:" play))))

(defun plop ()
  "An ant down a hole: at most one plop, and one soft tick, in 60ms, or a
crowd of them is a buzz."
  (let ((now (get-internal-real-time)))
    (when (> (- now *last-plop*) (* 0.06 internal-time-units-per-second))
      (setf *last-plop* now)
      (haptic 0 0.45)
      (sound "plop"))))

;;; ------------------------------------------------------------------
;;; the round's pieces

(defun make-cube (char size)
  "A cube: its colour, with a lighter top edge and a darker bottom one."
  (let* ((face (- size 2))
         (node (rect-sprite (rgb char) face face)))
    (add node (at (rect-sprite (ui:color 1 1 1 0.30) face (* face 0.22)) 0 (* face 0.39)))
    (add node (at (rect-sprite (ui:color 0 0 0 0.18) face (* face 0.16)) 0 (* face -0.42)))
    node))

(defun text-colour (char)
  (if (light-colour-p char) (ui:color 0.15 0.12 0.12) (ui:color 1 1 1)))

(defun make-box-node (box)
  (let* ((char (box-colour box))
         (node (rounded *box-width* *box-height* 10 (rgb char) (rgb char 0.6) 3)))
    ;; The door the ants come out of.
    (add node (at (rounded 22 9 4 (ui:color 0.12 0.09 0.08)) 0 (- (/ *box-height* 2) 9)))
    (let ((label (text (princ-to-string (box-count box)) 22 (text-colour char))))
      (add node (at label 0 -5))
      (setf (box-label box) label))
    (setf (box-node box) node)
    node))

(defun ant-scale ()
  "How big ants are drawn: full size down to cubes of 14 points, and smaller
with the cubes after that, so that one fits down a one-cube tunnel."
  (min 1d0 (/ (game-cell-size *game*) 14d0)))

(defun make-ant-node (ant)
  (let* ((char (box-colour (ant-box ant)))
         (node (rect-sprite (rgb char 0.75) 5 7))     ; thorax
         (butt (rect-sprite (rgb char 0.9) 7 9)))
    ;; Legs, under the body; a head in front of it.
    (add node (z (at (rect-sprite (ui:color 0.1 0.08 0.08) 12 1.5d0) 0 2) -1))
    (add node (z (at (rect-sprite (ui:color 0.1 0.08 0.08) 12 1.5d0) 0 -2) -1))
    (add node (at (rect-sprite (rgb char 0.55) 5 5) 0 6))
    ;; The abdomen, hinged at the waist so that it can swing: see WIGGLE.
    (objc:invoke butt "setAnchorPoint:" (point 0.5 1))
    (objc:invoke butt "setName:" "butt")
    (add node (at butt 0 -3))
    (objc:invoke node "setScale:" (ant-scale))
    (z (at node (ant-x ant) (ant-y ant)) 20)
    (setf (ant-node ant) node)
    node))

(defun layout-queue (game &key (animate t))
  "Put every queued box where it belongs: the front row bright and
tappable, the rows behind dimmed, the rest hidden and counted."
  (loop for column across (game-columns game)
        for k from 0
        for x = (column-x game k)
        do (loop for box in column
                 for row from 0
                 for node = (box-node box)
                 for y = (queue-y row)
                 do (objc:invoke node "setHidden:" (>= row *queue-visible*))
                    (objc:invoke node "setAlpha:" (if (zerop row) 1d0 0.55d0))
                    (if animate (run node (move-to x y 0.18d0)) (at node x y)))
           (let ((more (- (length column) *queue-visible*))
                 (label (aref *more-labels* k)))
             (objc:invoke label "setText:" (if (plusp more) (format nil "+~d more" more) "")))))

(defun update-status (game)
  (unless (eql *shown-left* (game-cubes-left game))
    (setf *shown-left* (game-cubes-left game))
    (objc:invoke *status* "setText:"
                 (format nil "~a  ·  ~d left" (level-name (game-level game)) (game-cubes-left game))))
  (let ((seconds (floor (game-time game))))
    (unless (eql *shown-seconds* seconds)
      (setf *shown-seconds* seconds)
      (objc:invoke *timer* "setText:"
                   (let ((par (level-par (game-level game))))
                     (format nil "~a~@[  /  par ~a~]" (clock seconds) (and par (clock par))))))))

(defun build-round (game)
  "Every node of a fresh round, under a new world node."
  (when *world* (objc:invoke *world* "removeFromParent"))
  (setf *world* (add *scene* (objc:invoke "SKNode" "node")))
  (fit-world t)
  (setf *overlay* nil
        *picker* nil
        *settings* nil
        *shown-left* nil
        *shown-seconds* nil
        *last-time* nil)
  (let* ((size (game-cell-size game))
         (w (game-w game))
         (h (game-h game))
         (grid (game-grid game))
         (plate-w (* (+ w 0.7d0) size))
         (plate-h (* (+ h 0.7d0) size)))
    ;; Title, and the 2x button.
    (setf *status* (add *world* (at (text "" 18 (ui:color 1 1 1)) (/ *scene-width* 2) 776)))
    ;; The level counter, which opens the picker.
    (add *world* (at (rounded 64 28 8 (ui:color 1 1 1 0.15)) 40 776))
    (add *world* (at (text (format nil "~d/~d" (1+ *level-index*) (length *levels*)) 14
                           (ui:color 1 1 1) :font "AvenirNext-DemiBold")
                     40 776))
    (setf *fast-button* (add *world* (at (rounded 56 28 8 (ui:color 1 1 1 0.15)) 351 776)))
    (add *fast-button* (text "2×" 15 (ui:color 1 1 1) :font "AvenirNext-DemiBold"))
    (show-speed)
    ;; The hint button, right of the slots.
    (setf *hints-left* *hints-per-round*
          *hinted* nil
          *hint-button* (add *world* (at (rounded 40 40 20 (ui:color 1 1 1 0.15) (ui:color 1 1 1 0.4) 1.5d0)
                                         *hint-x* *slot-y*)))
    (add *hint-button* (at (text "?" 22 (ui:color 1 1 1)) 0 0))
    ;; The settings button, left of the slots.
    (let ((button (add *world* (at (rounded 40 40 20 (ui:color 1 1 1 0.15) (ui:color 1 1 1 0.4) 1.5d0)
                                   *settings-x* *slot-y*))))
      (add button (at (text "⚙︎" 22 (ui:color 1 1 1) :font "AvenirNext-Medium") 0 1)))
    ;; The restart button, under it.
    (let ((button (add *world* (at (rounded 40 40 20 (ui:color 1 1 1 0.15) (ui:color 1 1 1 0.4) 1.5d0)
                                   *settings-x* (restart-y)))))
      (add button (at (text "↻" 22 (ui:color 1 1 1) :font "AvenirNext-DemiBold") 0 0)))
    (let ((badge (add *hint-button* (at (rounded 18 18 9 (ui:color 0.95 0.75 0.20)) 15 15))))
      (objc:invoke badge "setName:" "badge")
      (let ((count (add badge (text "" 11 (ui:color 0.15 0.12 0.12)))))
        (objc:invoke count "setName:" "count")))
    (show-hints)
    ;; The plate and the picture on it.
    (add *world* (at (rounded plate-w plate-h 8 (ui:color 0.80 0.78 0.74) (ui:color 0.55 0.53 0.50) 2)
                     (/ *scene-width* 2) (- *plate-top* (/ (* h size) 2))))
    (setf *cubes* (make-array (list h w) :initial-element nil))
    (dotimes (r h)
      (dotimes (c w)
        (let ((char (aref grid r c)))
          (when char
            (setf (aref *cubes* r c)
                  (add *world* (z (at (make-cube char size) (cell-x game c) (cell-y game r)) 5)))))))
    ;; Holes.
    (dolist (side '(:left :right))
      (destructuring-bind (x . y) (hole-point game side)
        (add *world* (at (ellipse 30 17 (ui:color 0.05 0.04 0.03) (ui:color 0.30 0.22 0.14)) x y))))
    ;; The timer, midway between the picture's plate and the slots.
    (setf *timer* (add *world* (at (text "" 14 (ui:color 1 1 1 0.75) :font "AvenirNext-DemiBold")
                                   (/ *scene-width* 2)
                                   (/ (+ (- *plate-top* (/ (* h size) 2) (/ plate-h 2))
                                         *slot-y* (/ (+ *box-height* 6) 2))
                                      2))))
    (objc:invoke *timer* "setHidden:" (not (scoring-p)))
    ;; Deployment slots.
    (dotimes (slot (length (game-slots game)))
      (add *world* (at (rounded (+ *box-width* 6) (+ *box-height* 6) 12
                                (ui:color 0 0 0 0.18) (ui:color 1 1 1 0.35) 1.5d0)
                       (slot-x game slot) *slot-y*)))
    ;; The queue.
    (setf *more-labels* (make-array (length (game-columns game))))
    (loop for column across (game-columns game)
          for k from 0
          do (dolist (box column)
               (add *world* (z (make-box-node box) 10)))
             (setf (aref *more-labels* k)
                   (add *world* (at (text "" 13 (ui:color 1 1 1 0.7) :font "AvenirNext-DemiBold")
                                    (column-x game k) (- (queue-y *queue-visible*) -26)))))
    (layout-queue game :animate nil)
    (update-status game)))

(defun pick-up (ant)
  "The ant's cube, carried in front of its head."
  (setf (ant-load ant)
        (add (ant-node ant) (at (make-cube (box-colour (ant-box ant)) 9) 0 11))))

(defun butt (ant)
  (objc:invoke (ant-node ant) "childNodeWithName:" "butt"))

(defun wiggle (ant)
  "Swing the ant's abdomen from side to side, about its waist, until STILL."
  (let ((swing (action "rotateByAngle:duration:" 0.4d0 0.07d0))
        (back (action "rotateByAngle:duration:" -0.8d0 0.14d0)))
    (objc:invoke (butt ant) "runAction:withKey:"
                 (action "repeatActionForever:" (action "sequence:" (vector swing back swing)))
                 "wiggle")))

(defun still (ant)
  (let ((butt (butt ant)))
    (objc:invoke butt "removeActionForKey:" "wiggle")
    (objc:invoke butt "setZRotation:" 0d0)))

(defun set-down (ant)
  "The ant stops to rest: the cube it carries goes on the ground in front of
it, under the ants walking by, and it shakes its abdomen until REST-END."
  (wiggle ant)
  (when (ant-load ant)
    (objc:invoke (ant-load ant) "removeFromParent")
    (setf (ant-load ant) nil))
  (let ((heading (ant-heading ant)))
    (setf (ant-dropped ant)
          (add *world* (z (at (make-cube (box-colour (ant-box ant)) (* 9 (ant-scale)))
                              (+ (ant-x ant) (* 12 (ant-scale) (cos heading)))
                              (+ (ant-y ant) (* 12 (ant-scale) (sin heading))))
                          15)))))

(defun show-overlay (title subtitle colour &key stars detail)
  "The round's end: TITLE, and under it STARS and a DETAIL line when it was
won, and what a tap does."
  (let ((panel (z (at (rounded 300 (if stars 210 150) 18 (ui:color 0.08 0.08 0.10 0.92) colour 3)
                      (/ *scene-width* 2) 560)
                  100))
        (top (if stars 52 22)))
    (add panel (at (text title 34 colour) 0 top))
    (when stars
      (add panel (at (text (star-string stars) 30 (ui:color 0.98 0.80 0.25)) 0 (- top 42)))
      (add panel (at (text detail 14 (ui:color 1 1 1 0.7) :font "AvenirNext-Medium") 0 (- top 74))))
    (add panel (at (text subtitle 16 (ui:color 1 1 1 0.85) :font "AvenirNext-DemiBold") 0 (if stars -78 -28)))
    (objc:invoke panel "setScale:" 0.6d0)
    (add *world* panel)
    (run panel (scale-to 1 0.2d0))
    (setf *overlay* panel)))

;;; ------------------------------------------------------------------
;;; events from the model

(defun handle (game event)
  (destructuring-bind (kind &rest arguments) event
    (ecase kind
      (:deploy
       (destructuring-bind (box slot column) arguments
         (declare (ignore column))
         (drop-ring)
         (let ((node (box-node box)))
           (objc:invoke node "setAlpha:" 1d0)
           (run node (move-to (slot-x game slot) *slot-y* 0.22d0)))
         (layout-queue game)))
      (:spawn
       (add *world* (make-ant-node (first arguments))))
      (:pickup
       (destructuring-bind (ant r c) arguments
         (let ((cube (aref *cubes* r c)))
           (setf (aref *cubes* r c) nil)
           (when cube (vanish cube 0.12d0)))
         (pick-up ant)))
      (:rest-start
       (set-down (first arguments)))
      (:rest-end
       (let ((ant (first arguments)))
         (still ant)
         (when (ant-dropped ant)
           (vanish (ant-dropped ant) 0.1d0)
           (setf (ant-dropped ant) nil))
         (pick-up ant)))
      (:escape
       (let* ((ant (first arguments))
              (box (ant-box ant)))
         (jump-in (ant-node ant) (ant-hole ant))
         (plop)
         (setf (ant-node ant) nil)
         (objc:invoke (box-label box) "setText:" (princ-to-string (box-count box)))))
      (:box-done
       (let ((box (first arguments)))
         (haptic 1)
         (vanish (box-node box) 0.25d0)
         (setf (box-node box) nil (box-label box) nil)))
      (:won
       (notify :success)
       (sound "won")
       (unlock (+ *level-index* 2))
       (let ((stars (stars game *hinted*)))
         (record-stars (game-level game) stars)
         (show-overlay "Cleared!"
                       (if (< (1+ *level-index*) (length *levels*)) "Tap for the next picture" "Tap to play again")
                       (ui:color 0.55 0.95 0.55)
                       :stars (and (scoring-p) stars)
                       :detail (format nil "~a, par ~a~:[~;, with a hint~]"
                                       (clock (game-time game)) (clock (level-par (game-level game)))
                                       *hinted*))))
      (:lost
       (notify :error)
       (sound "lost")
       (show-overlay "Stuck!" "No ants can reach a cube. Tap to retry." (ui:color 1.0 0.55 0.45))))))

(defun clock (seconds)
  (multiple-value-bind (minutes seconds) (floor (round seconds) 60)
    (format nil "~d:~2,'0d" minutes seconds)))

(defparameter *sway* 1.6d0 "Points an ant sways to either side as it walks.")
(defparameter *stride* 0.35d0 "Radians of sway per point walked.")
(defparameter *wobble* 0.18d0 "Radians an ant's heading swings as it walks.")

(defun draw-ants (game)
  "Each ant where the model has it, swaying across its line of travel and
swinging its head in time with its stride, so no two walk in lockstep.
The sway is drawn only: it never takes an ant off its route."
  (dolist (ant (game-ants game))
    (let ((node (ant-node ant)))
      (when node
        (let* ((heading (ant-heading ant))
               (step (+ (ant-gait ant) (* *stride* (ant-walked ant))))
               (sway (* *sway* (sin step))))
          (objc:invoke node "setPosition:"
                       (point (- (ant-x ant) (* sway (sin heading)))
                              (+ (ant-y ant) (* sway (cos heading)))))
          ;; The ant's drawing faces up the screen; its heading is from +x.
          (objc:invoke node "setZRotation:"
                       (+ (- heading (/ pi 2)) (* *wobble* (cos step)))))))))

(defun pace ()
  (if *fast* 2 1))

(defun show-speed ()
  "Light the 2x button when it is on, and run the round's animations -- jumps,
rests, pickups -- at the same pace as the model."
  (objc:invoke *fast-button* "setFillColor:"
               (if *fast* (ui:color 0.95 0.75 0.20 0.9) (ui:color 1 1 1 0.15)))
  (objc:invoke *world* "setSpeed:" (float (pace) 1d0)))

(defun toggle-speed ()
  (setf *fast* (not *fast*))
  (save "fast" (if *fast* 1 0))
  (haptic 0)
  (show-speed))

(defun show-hints ()
  "The hint button's badge counts the hints left; with none, it dims."
  (let ((badge (objc:invoke *hint-button* "childNodeWithName:" "badge")))
    (objc:invoke (objc:invoke badge "childNodeWithName:" "count") "setText:"
                 (princ-to-string *hints-left*))
    (objc:invoke *hint-button* "setAlpha:" (if (plusp *hints-left*) 1d0 0.4d0))))

(defun say (string)
  "A note over the slots for a couple of seconds.  The note."
  (let ((note (z (at (rounded 300 36 12 (ui:color 0.08 0.08 0.10 0.92) (ui:color 1 1 1 0.4) 1.5d0)
                     (/ *scene-width* 2) (+ *slot-y* 48))
                 90)))
    (add note (text string 14 (ui:color 1 1 1) :font "AvenirNext-DemiBold"))
    (add *world* note)
    (run note (action "waitForDuration:" 2.4d0) (action "fadeOutWithDuration:" 0.3d0)
         (action "removeFromParent"))
    note))

(defvar *ring* nil "The ring POINT-AT put round a box, until it fades or a box is tapped.")

(defun point-at (box)
  "Pulse BOX, ringed in green, to say: tap this one."
  (let* ((node (box-node box))
         (ring (add node (rounded (+ *box-width* 10) (+ *box-height* 10) 14
                                  (ui:color 0 0 0 0) (ui:color 0.55 0.95 0.55) 3))))
    (drop-ring)
    ;; Retained, because the ring removes itself: once its fade ends, its
    ;; parent was the only owner (+shapeNodeWithRect: hands back a node nobody
    ;; owns), so it was freed while *RING* still pointed at it, and the next
    ;; tap's DROP-RING messaged freed memory -- a pointer authentication trap
    ;; in object_getClass, on TestFlight build 202610192349.
    (setf *ring* (objc:retain ring))
    (run ring (action "waitForDuration:" 2.4d0) (action "fadeOutWithDuration:" 0.3d0)
         (action "removeFromParent"))
    (run node (action "repeatAction:count:"
                      (action "sequence:" (vector (scale-to 1.12 0.18d0) (scale-to 1 0.18d0)))
                      4))))

(defun drop-ring ()
  "Take the ring off, if there is one, and give up POINT-AT's hold on it.
-removeFromParent on a ring that has already faded and removed itself does
nothing."
  (when *ring*
    (let ((ring *ring*))
      (setf *ring* nil)
      (objc:invoke ring "removeFromParent")
      (objc:release ring))))

(defvar *thinking* nil "Set while a hint is being worked out.")

(defun ask-hint (game)
  "The hint button: point at the box to tap, or say why there is none.  Only
a box pointed at uses a hint up.  Off the level's order, the answer can take
a second or two to work out, so a note says so first: the work starts on a
timer, once the note has been drawn."
  (cond ((not (plusp *hints-left*))
         (shake *hint-button*))
        (*thinking*)
        ((or (game-plan game) (every #'null (game-columns game)))
         (answer-hint game (hint game)))
        (t
         (let ((note (say "Thinking…")))
           (setf *thinking* t)
           (ui:after-every 0.05d0
                           (lambda (timer)
                             (declare (ignore timer))
                             (setf *thinking* nil)
                             (objc:invoke note "removeFromParent")
                             ;; Unless the round was left meanwhile.
                             (when (eq game *game*)
                               (answer-hint game (hint game))))
                           :repeats nil)))))

(defun answer-hint (game hint)
  (destructuring-bind (what &optional column) hint
    (ecase what
      (:deploy
       (decf *hints-left*)
       (setf *hinted* t)
       (show-hints)
       (sound "hint")
       (point-at (first (aref (game-columns game) column))))
      (:wait (say "Wait for a box to empty"))
      (:none (say "Nothing left to tap"))
      (:lost (say "No way to win from here: try again"))
      (:unsure (say "No winning move found from here")))))

(defun frame (dt)
  (let ((game *game*))
    ;; The round holds still while the picker or the settings are open.
    (when (and game (not *picker*) (not *settings*))
      (step-game game (* dt (pace)))
      (dolist (event (take-events game))
        (handle game event))
      (draw-ants game)
      (update-status game))))

;;; ------------------------------------------------------------------
;;; the scene

(objc:define-objc-class ant-scene () ()
  (:objc-class-name "LispAntScene")
  (:objc-superclass-name "SKScene"))

(defvar *failed* nil "Set when a frame signals, so one error is reported once.")

;;; The scene takes the view's size (resize fill), and the world, laid out
;;; on a 390x800 board, is scaled to fit and centred on it: the board shows
;;; whole on any phone, with the scene's green, not black bars, around it.

(defvar *fitted* nil "The scene size the world was last fitted to.")

(defun fit-world (&optional force)
  (when (and *scene* *world*)
    (let* ((size (objc:invoke *scene* "size"))
           (w (aref size 0))
           (h (aref size 1)))
      (when (and (plusp w) (plusp h)
                 (or force (not (equalp size *fitted*))))
        (setf *fitted* size)
        (let ((scale (min (/ w *scene-width*) (/ h *scene-height*))))
          (objc:invoke *world* "setScale:" scale)
          (objc:invoke *world* "setPosition:"
                       (point (/ (- w (* scale *scene-width*)) 2)
                              (/ (- h (* scale *scene-height*)) 2))))))))

(objc:define-objc-method ("update:" :void)
    ((self ant-scene) (time :double))
  (let ((dt (if *last-time* (min 0.05d0 (max 0d0 (- time *last-time*))) (/ 1d0 60))))
    (setf *last-time* time)
    (fit-world)
    (unless *failed*
      (handler-case (frame dt)
        (error (condition)
          (setf *failed* t)
          (format *error-output* "~&ants: frame failed: ~a~%" condition)
          (finish-output *error-output*))))))

;;; ------------------------------------------------------------------
;;; rounds and taps

(defun start-level (index)
  (setf *level-index* (mod index (length *levels*))
        *game* (new-game (nth *level-index* *levels*))
        *failed* nil)
  (save "level" *level-index*)
  (build-round *game*)
  (format t "ANTS: level ~a, ~d cubes, ~d boxes, seed ~a~%"
          (level-name (game-level *game*)) (game-cubes-left *game*)
          (reduce #'+ (game-columns *game*) :key #'length) (game-seed *game*))
  (finish-output))

(defun within (x y cx cy half-width half-height)
  (and (<= (abs (- x cx)) half-width) (<= (abs (- y cy)) half-height)))

(defun tap-at (x y)
  (let ((game *game*))
    (cond ((null game))
          (*picker* (picker-tap x y))
          (*settings* (settings-tap x y))
          ((within x y 40 776 36 20)
           (show-picker))
          ((within x y *settings-x* *slot-y* 24 24)
           (show-settings))
          ((within x y *settings-x* (restart-y) 24 24)
           (haptic 0)
           (sound "tap")
           (start-level *level-index*))
          (*overlay*
           (start-level (if (eq (game-state game) :won) (1+ *level-index*) *level-index*)))
          ((within x y 351 776 32 20)
           (toggle-speed))
          ((within x y *hint-x* *slot-y* 24 24)
           (ask-hint game))
          (t
           (loop for column across (game-columns game)
                 for k from 0
                 for cx = (column-x game k)
                 do (loop for box in column
                          for row below *queue-visible*
                          when (within x y cx (queue-y row) (/ *box-width* 2) (/ *queue-step* 2))
                            do (cond ((and (zerop row) (deploy game k))
                                      (haptic 0)
                                      (sound "tap"))
                                     (t (shake (box-node box))))
                               (return-from tap-at)))))))

;;; ------------------------------------------------------------------
;;; the level picker: every picture, the unlocked ones playable

(defparameter *picker-page-size* 20 "Pictures on each page of the picker.")
(defvar *picker-page* 0 "The page of the picker showing.")

(defun picker-pages ()
  (ceiling (length *levels*) *picker-page-size*))

(defun picker-page-levels ()
  "The indexes of the pictures on the page showing."
  (loop for index from (* *picker-page* *picker-page-size*)
        below (min (length *levels*) (* (1+ *picker-page*) *picker-page-size*))
        collect index))

(defun picker-rows ()
  "Pictures down each of the picker's two columns."
  (ceiling (min *picker-page-size* (length *levels*)) 2))

(defun picker-step ()
  "Rows 62 points apart, closer when there are too many for that."
  (min 62 (/ 600 (picker-rows))))

(defun picker-row-y (index)
  "Rows centred on the middle of the panel's room."
  (- (+ 390 (* (1- (picker-rows)) (picker-step) 1/2))
     (* (mod (mod index *picker-page-size*) (picker-rows)) (picker-step))))

(defun picker-x (index)
  (+ (/ *scene-width* 2) (if (< (mod index *picker-page-size*) (picker-rows)) -84 84)))

(defun picker-title-y ()
  (- (picker-top) 34))

(defun picker-arrow-x (step)
  "Where the arrow to the page STEP away is: -1 left, 1 right."
  (+ (/ *scene-width* 2) (* step 145)))

(defun picker-top ()
  "Above the first row, with room for the title and, over one page, the page count."
  (+ (picker-row-y 0) (if (> (picker-pages) 1) 95 75)))

(defun picker-bottom ()
  (- (picker-row-y (1- (picker-rows))) 40))

(defun show-picker (&optional (page (floor *level-index* *picker-page-size*)))
  (close-picker)
  (setf *picker-page* (mod page (picker-pages)))
  (let* ((open (unlocked))
         (top (picker-top))
         (bottom (picker-bottom))
         (picker (z (objc:invoke "SKNode" "node") 200)))
    ;; A shade over the round; a tap on it closes the picker.
    (add picker (at (rect-sprite (ui:color 0 0 0 0.45) (* 3 *scene-width*) (* 3 *scene-height*))
                    (/ *scene-width* 2) (/ *scene-height* 2)))
    (add picker (at (rounded 360 (- top bottom) 18 (ui:color 0.08 0.08 0.10 0.95) (ui:color 1 1 1 0.45) 2)
                    (/ *scene-width* 2) (/ (+ top bottom) 2)))
    (add picker (at (text "Pictures" 22 (ui:color 1 1 1)) (/ *scene-width* 2) (picker-title-y)))
    (when (> (picker-pages) 1)
      (add picker (at (text (format nil "~d/~d" (1+ *picker-page*) (picker-pages)) 13
                            (ui:color 1 1 1 0.6) :font "AvenirNext-DemiBold")
                      (/ *scene-width* 2) (- (picker-title-y) 20)))
      (dolist (step '(-1 1))
        (let ((arrow (add picker (at (rounded 44 34 10 (ui:color 1 1 1 0.14) (ui:color 1 1 1 0.3) 1.5d0)
                                     (picker-arrow-x step) (picker-title-y)))))
          (add arrow (text (if (minusp step) "‹" "›") 26 (ui:color 1 1 1))))))
    (loop for row in (picker-page-levels)
          for level = (nth row *levels*)
          for playable = (< row open)
          for current = (= row *level-index*)
          do (let ((button (rounded 156 (min 50 (- (picker-step) 8)) 12
                                    (ui:color 1 1 1 (if playable 0.14 0.04))
                                    (if current (ui:color 0.55 0.95 0.55) (ui:color 1 1 1 (if playable 0.3 0.1)))
                                    (if current 3 1.5d0))))
               (objc:invoke button "setName:" (format nil "row~d" row))
               (let ((stars (if (and playable (scoring-p)) (best-stars level) 0)))
                 (add button (at (text (format nil "~d   ~a" (1+ row) (if playable (level-name level) "Locked"))
                                       15 (ui:color 1 1 1 (if playable 1 0.35)) :font "AvenirNext-DemiBold")
                                 0 (if (plusp stars) 6 0)))
                 (when (plusp stars)
                   (add button (at (text (star-string stars) 11 (ui:color 0.98 0.80 0.25)) 0 -11))))
               (add picker (at button (picker-x row) (picker-row-y row)))))
    (setf *picker* (add *world* picker))))

(defun close-picker ()
  (when *picker*
    (objc:invoke *picker* "removeFromParent")
    (setf *picker* nil)))

(defun picker-tap (x y)
  "A tap on an unlocked picture plays it; on a locked one, shakes it; outside
the panel, closes the picker."
  (let ((row (loop for row in (picker-page-levels)
                   when (within x y (picker-x row) (picker-row-y row) 78 (/ (picker-step) 2))
                     return row)))
    (cond ((and (> (picker-pages) 1) (within x y (picker-arrow-x -1) (picker-title-y) 26 20))
           (haptic 0)
           (show-picker (1- *picker-page*)))
          ((and (> (picker-pages) 1) (within x y (picker-arrow-x 1) (picker-title-y) 26 20))
           (haptic 0)
           (show-picker (1+ *picker-page*)))
          ((and row (< row (unlocked)))
           (haptic 0)
           (start-level row))
          (row
           (shake (objc:invoke *picker* "childNodeWithName:" (format nil "row~d" row))))
          ((not (within x y (/ *scene-width* 2) (/ (+ (picker-top) (picker-bottom)) 2)
                        180 (/ (- (picker-top) (picker-bottom)) 2)))
           (close-picker)))))

;;; ------------------------------------------------------------------
;;; settings

(defparameter *settings-y* 430d0 "The middle of the settings panel.")

(defparameter *settings-rows*
  '(("Time and stars" "Score each round against par" "no scoring")
    ("Sound" "Plops, taps and chimes" "no sound")
    ("Vibration" "Taps as the ants go home" "no vibration"))
  "Each switch: its label, what it means, and the default that turns it off.")

(defun settings-row-y (index) (- (+ *settings-y* 75) (* index 70)))
(defun settings-done-y () (- *settings-y* 135))

(defun show-settings ()
  (close-settings)
  (let ((panel (z (objc:invoke "SKNode" "node") 200)))
    ;; A shade over the round; a tap on it closes the settings.
    (add panel (at (rect-sprite (ui:color 0 0 0 0.45) (* 3 *scene-width*) (* 3 *scene-height*))
                   (/ *scene-width* 2) (/ *scene-height* 2)))
    (add panel (at (rounded 320 360 18 (ui:color 0.08 0.08 0.10 0.95) (ui:color 1 1 1 0.45) 2)
                   (/ *scene-width* 2) *settings-y*))
    (add panel (at (text "Settings" 22 (ui:color 1 1 1)) (/ *scene-width* 2) (+ *settings-y* 141)))
    ;; Each row: a label, what it means, and a switch.
    (loop for (title subtitle key) in *settings-rows*
          for index from 0
          for y = (settings-row-y index)
          for on = (zerop (saved key))
          do (loop for (string size alpha font dy) in `((,title 17 1 "AvenirNext-DemiBold" 9)
                                                        (,subtitle 12 0.6 "AvenirNext-Medium" -12))
                   do (let ((label (text string size (ui:color 1 1 1 alpha) :font font)))
                        (objc:invoke label "setHorizontalAlignmentMode:" 1) ; left
                        (add panel (at label 52 (+ y dy)))))
             (let ((switch (add panel (at (rounded 54 32 16 (if on (ui:color 0.30 0.78 0.40) (ui:color 1 1 1 0.2))
                                                   (ui:color 1 1 1 0.35) 1.5d0)
                                          300 y))))
               (add switch (at (ellipse 26 26 (ui:color 1 1 1) (ui:color 0 0 0 0.15)) (if on 11 -11) 0))))
    ;; Done.
    (let ((done (add panel (at (rounded 120 40 12 (ui:color 1 1 1 0.14) (ui:color 1 1 1 0.3) 1.5d0)
                               (/ *scene-width* 2) (settings-done-y)))))
      (add done (text "Done" 17 (ui:color 1 1 1) :font "AvenirNext-DemiBold")))
    (setf *settings* (add *world* panel))))

(defun close-settings ()
  (when *settings*
    (objc:invoke *settings* "removeFromParent")
    (setf *settings* nil)))

(defun toggle-setting (key)
  (save key (if (zerop (saved key)) 1 0))
  (objc:invoke *timer* "setHidden:" (not (scoring-p)))
  (show-settings))

(defun settings-tap (x y)
  "A tap on a row flips its switch; on Done or outside the panel, closes."
  (let ((row (position-if (lambda (index) (within x y (/ *scene-width* 2) (settings-row-y index) 150 28))
                          (loop for index below (length *settings-rows*) collect index))))
    (cond (row
           (toggle-setting (third (nth row *settings-rows*)))
           (haptic 0))
          ((or (within x y (/ *scene-width* 2) (settings-done-y) 60 20)
               (not (within x y (/ *scene-width* 2) *settings-y* 160 180)))
           (close-settings)))))

(defun tap-view-point (in-view)
  "A tap at IN-VIEW, a point in the SKView's coordinates (y down), on the board."
  (let ((in-scene (objc:invoke *world* "convertPoint:fromNode:"
                               (objc:invoke *scene* "convertPointFromView:" in-view) *scene*)))
    (tap-at (aref in-scene 0) (aref in-scene 1))))

(defun tapped (recognizer)
  (tap-view-point (objc:invoke recognizer "locationInView:" *view*)))

;;; ------------------------------------------------------------------
;;; autoplay, for watching a round without touching the screen
;;;
;;;   SIMCTL_CHILD_ANTS_AUTOPLAY=1 xcrun simctl launch booted org.lisp.ants-app
;;;
;;; Every half second it taps, through TAP-AT, the front box of a column
;;; whose colour has a cube within reach, or any box once the board is idle;
;;; and the overlay when a round ends.  ANTS_AUTOPLAY=careless taps a
;;; column at random, which usually loses; ANTS_AUTOPLAY=hints presses the
;;; hint button before each move while it has hints, and taps where it points;
;;; ANTS_AUTOPLAY=astray taps two columns at random first, so the hints must
;;; find their own way; ANTS_LEVEL=n starts at level n,
;;; from 1, instead of the saved one; ANTS_PICKER=1 opens the level picker,
;;; and ANTS_SETTINGS=1 the settings;
;;; ANTS_FAST=1 starts at double speed.

(defun autotap (x y)
  "Tap the board point (X Y) as a finger would: by way of the view."
  (tap-view-point (objc:invoke *scene* "convertPointToView:"
                               (objc:invoke *world* "convertPoint:toNode:" (point x y) *scene*))))

(defvar *overlay-ticks* 0)
(defvar *careless* nil)
(defvar *hinting* nil "ANTS_AUTOPLAY=hints: press the hint button before each move.")
(defvar *hint-pressed* nil "Set between pressing the hint button and following it.")
(defvar *astray* 0 "ANTS_AUTOPLAY=astray: columns to tap at random before asking.")

(defun autoplay-tick ()
  (let ((game *game*))
    (cond ((or (null game) *picker* *settings*))
          (*overlay*
           ;; Leave the result up long enough to be read.
           (when (>= (incf *overlay-ticks*) 5)
             (setf *overlay-ticks* 0)
             (autotap (/ *scene-width* 2) 560)))
          ((and (plusp *astray*) (free-slot game))
           (decf *astray*)
           (let ((open (loop for column across (game-columns game)
                             for k from 0 when column collect k)))
             (when open (autotap (column-x game (nth (random (length open)) open)) (queue-y 0)))))
          (*thinking*)
          ((and *hinting* (free-slot game) (plusp *hints-left*) (not *hint-pressed*))
           (setf *hint-pressed* t)
           (autotap *hint-x* *slot-y*))
          ((free-slot game)
           (let ((k (or (and *hint-pressed*
                             (destructuring-bind (what &optional column) (hint game)
                               (setf *hint-pressed* nil)
                               (and (eq what :deploy) column)))
                        (and *careless*
                             (let ((open (loop for column across (game-columns game)
                                               for k from 0 when column collect k)))
                               (when open (nth (random (length open)) open))))
                        (loop for column across (game-columns game)
                              for k from 0
                              when (and column (reachable-cubes (game-grid game) (box-colour (first column))
                                                                (game-reserved game)))
                                return k)
                        (and (null (game-ants game))
                             (position-if #'identity (game-columns game))))))
             (when k (autotap (column-x game k) (queue-y 0))))))))

(defun start ()
  (objc:ensure-objc-initialized)
  (let* ((root (ui:root-view))
         (safe (objc:invoke root "safeAreaLayoutGuide"))
         (view (ui:new "SKView"))
         (background (ui:color 0.26 0.34 0.22)))
    (objc:invoke root "setBackgroundColor:" background)
    (objc:invoke root "addSubview:" view)
    (ui:pin view "topAnchor" safe "topAnchor")
    (ui:pin view "bottomAnchor" safe "bottomAnchor")
    (ui:pin view "leadingAnchor" safe "leadingAnchor")
    (ui:pin view "trailingAnchor" safe "trailingAnchor")
    (let ((scene (objc:invoke (make-instance 'ant-scene) "initWithSize:"
                              (point *scene-width* *scene-height*))))
      (objc:invoke scene "setBackgroundColor:" background)
      (objc:invoke scene "setScaleMode:" 3)        ; resize fill: see FIT-WORLD
      (setf *view* (ui:keep view)
            *scene* (ui:keep scene))
      (setf *fast* (or (= 1 (saved "fast")) (and (ext:getenv "ANTS_FAST") t)))
      (objc:invoke (objc:invoke "AVAudioSession" "sharedInstance") "setCategory:error:"
                   "AVAudioSessionCategoryAmbient" nil)
      (start-level (let ((n (ext:getenv "ANTS_LEVEL")))
                     (if n (1- (or (parse-integer n :junk-allowed t) 1)) (saved-level))))
      (when (ext:getenv "ANTS_PICKER")
        (show-picker))
      (when (ext:getenv "ANTS_SETTINGS")
        (show-settings))
      (objc:invoke view "presentScene:" scene))
    (objc:invoke view "addGestureRecognizer:"
                 (ui:keep (objc:invoke (objc:invoke "UITapGestureRecognizer" "alloc")
                                       "initWithTarget:action:"
                                       (ui:action-target #'tapped) "fire:")))
    (setf *careless* (equal (ext:getenv "ANTS_AUTOPLAY") "careless")
          *hinting* (member (ext:getenv "ANTS_AUTOPLAY") '("hints" "astray") :test #'equal)
          *astray* (if (equal (ext:getenv "ANTS_AUTOPLAY") "astray") 2 0))
    (when (ext:getenv "ANTS_AUTOPLAY")
      (ui:after-every 0.5 (lambda (timer) (declare (ignore timer)) (autoplay-tick))))
    (values)))
