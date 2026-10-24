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
(defparameter *rest-seconds* '(0.3d0 0.6d0 2.5d0)
  "Shortest rest, the mean of what is added to it, and longest.  The addition
is exponentially distributed: most rests are a moment, a few are long.")
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
  (seed 1)                              ; first seed tried for the queue
  order                                 ; the columns to tap, as a string of digits
  par)                                  ; seconds ORDER takes: see STARS

(defparameter *levels*
  (list
   (make-level :name "Mushroom" :slots 3 :seed 11 :par 49
               :order "102020221110"
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
   (make-level :name "Strawberry" :difficulty 0.6 :slots 4 :seed 1 :par 64
               :order "011110120022020212210"
               :rows '("CCCCCGGGCCCCC"
                       "CCCGGGGGGGCCC"
                       "CCRRRGGGRRRCC"
                       "CRRRRRRRRRRRC"
                       "CRRYRRRYRRRRC"
                       "CRRRRRRRRYRRC"
                       "CRRRYRRRRRRRC"
                       "CCRRRRRYRRRCC"
                       "CCRRYRRRRRRCC"
                       "CCCRRRRRYRCCC"
                       "CCCCRRRRRCCCC"
                       "CCCCCRRRCCCCC"
                       "CCCCCCRCCCCCC"))
   (make-level :name "Heart" :difficulty 0.25 :slots 3 :seed 30 :par 68
               :order "00001101011112222202"
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
   (make-level :name "Rocket" :difficulty 0.8 :slots 4 :seed 1 :par 63
               :order "0000222112110211100022"
               :rows '("BBBBBBRBBBBBB"
                       "BBBBBRRRBBBBB"
                       "BBBBBWWWBBBYB"
                       "BBBBWWWWWBBBB"
                       "BYBBWWCWWBBBB"
                       "BBBBWCCCWBBBB"
                       "BBBBWWCWWBBYB"
                       "BBBBWWWWWBBBB"
                       "BBBRWWWWWRBBB"
                       "BBRRWWWWWRRBB"
                       "BBRRBOYOBRRBB"
                       "BBBBBOYOBBBBB"
                       "BYBBBBOBBBBBB"))
   (make-level :name "Flower" :difficulty 0.5 :slots 4 :seed 5 :par 74
               :order "000002001212211212112"
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
   (make-level :name "Fish" :difficulty 0.75 :slots 4 :seed 3 :par 91
               :order "000010212222211111000221"
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
                       "NGGNNGGNNNGGGN"))
   (make-level :name "Cat" :difficulty 0.7 :slots 4 :seed 1 :par 72
               :order "00000010101112122112222"
               :rows '("PPPPPPPPPPPPPP"
                       "POPPPPPPPPPPOP"
                       "POOPPPPPPPPOOP"
                       "POOOOOOOOOOOOP"
                       "POOOOOOOOOOOOP"
                       "POOGKOOOOGKOOP"
                       "POOGKOOOOGKOOP"
                       "POOOOOOOOOOOOP"
                       "PWWOOOPPOOOWWP"
                       "POOOOOWWOOOOOP"
                       "PPOOOWOOWOOOPP"
                       "PPPOOOOOOOOPPP"
                       "PPPPPPPPPPPPPP"))
   (make-level :name "House" :difficulty 0.9 :slots 4 :seed 1 :par 73
               :order "1111101000020020222221"
               :rows '("CCCCCCCCCCNNCC"
                       "CCCCCCRRCCNNCC"
                       "CCCCCRRRRCNNCC"
                       "CCCCRRRRRRRCCC"
                       "CCCRRRRRRRRRCC"
                       "CCRRRRRRRRRRRC"
                       "CCCWWWWWWWWWCC"
                       "CCCWYYWWWYYWCC"
                       "CCCWYYWWWYYWCC"
                       "CCCWWWNNWWWWCC"
                       "CCCWWWNNWWWWCC"
                       "CCCWWWNNWWWWCC"
                       "GGGGGGGGGGGGGG"))
   (make-level :name "Butterfly" :difficulty 0.3 :slots 4 :seed 48 :par 213
               :order "2222220000000000000000000111111111111111112222221222222"
               :rows '("CCCCCCCCCCCCCCCCCCCCCC"
                       "CCCCCCCCKCCCCKCCCCCCCC"
                       "CCCPPPPPCKCCKCPPPPPCCC"
                       "CCPPPPPPPCKKCPPPPPPPCC"
                       "CPPPRRRPPPKKPPPRRRPPPC"
                       "CPPRRYRRPPKKPPRRYRRPPC"
                       "CPPRRRRYPPKKPPYRRRRPPC"
                       "CPPYRRRRPPKKPPRRRRYPPC"
                       "CPPRRRRRPPKKPPRRRRRPPC"
                       "CPPPRRRPPPKKPPPRRRPPPC"
                       "CCPPPPPPPCKKCPPPPPPPCC"
                       "CCCPPPPPCCKKCCPPPPPCCC"
                       "CCCCPPPPPCKKCPPPPPCCCC"
                       "CCCPPOOOPPKKPPOOOPPCCC"
                       "CCCPOOOOOPKKPOOOOOPCCC"
                       "CCCPOOYOOPKKPOOYOOPCCC"
                       "CCCPOOOOOPKKPOOOOOPCCC"
                       "CCCPPOOOPPKKPPOOOPPCCC"
                       "CCCCPPPPPCKKCPPPPPCCCC"
                       "CCCCCCCCCCKKCCCCCCCCCC"
                       "CCCCCCCCCCCCCCCCCCCCCC"
                       "CCCCCCCCCCCCCCCCCCCCCC"))
   (make-level :name "Planet" :difficulty 0.3 :slots 4 :seed 20 :par 189
               :order "20220000000000000000111110111110111112212112222212222222"
               :rows '("KKKKKKKKKKKKKKKKKKKKKK"
                       "KKWKKKKKKKKKKWKKKKKKKK"
                       "KKKKKKKKKKKKKKKKKKWKKK"
                       "KKKKKKKKKKKKKKKKKKKKKK"
                       "KKKKKKKKOOOOOOKKKKKKKK"
                       "KWKKKKYYYYYYYYYYKKKKKK"
                       "KKKKKYYYYYYYYYYYYKKKKK"
                       "KKKKKNNNNNNNNNNNNKKKKK"
                       "KKKKNNNNNNNNNNNNNNKKKK"
                       "KKTTOOOOOOOOOOOOOOTTKK"
                       "KTTKOOOOOOOOOOOOOOKTTK"
                       "TTTKYYYYYYYYYYYYYYKTTT"
                       "KTTKYYYYYYYYYYYYYYKTTW"
                       "KKTTTNNNNNNNNNNNNTTTKK"
                       "KKKKKNTTTTTTTTTTNKKKKK"
                       "KKWKKOOOOOOOOOOOOKKKKK"
                       "KKKKKKOOOOOOOOOOKKKKKK"
                       "KKKKKKKKYYYYYYKKKKKKKK"
                       "KKKKKKKKKKKKKKKKKKKKWK"
                       "KKKKKKKKKKKKKKKWKKKKKK"
                       "KKKKWKKKKKKKKKKKKKKKKK"
                       "KKKKKKKKKKKKKKKKKKKKKK"))
   (make-level :name "Rainbow" :difficulty 0.9 :slots 3 :seed 1 :par 219
               :order "000000000000000010101111111112111121212212222222222222"
               :rows '("CCCCCCCCCCCCCCCCCCCCCC"
                       "CCWWCCCCCCCCCCCCCCWWCC"
                       "CWWWWCCCCCCCCCCCCWWWWC"
                       "CWWWWWCCCCCCCCCCWWWWWC"
                       "CCCCCCCCCCCCCCCCCCCCCC"
                       "CCCCCCCCCCCCCCCCCCCCCC"
                       "CCCCCCCPPPPPPPPCCCCCCC"
                       "CCCCPPPPPPPPPPPPPPCCCC"
                       "CCPPPPBBBBBBBBBBPPPPCC"
                       "CPPPBBBBGGGGGGBBBBPPPC"
                       "PPBBBGGGGGGGGGGGGBBBPP"
                       "PBBBGGGYYYYYYYYGGGBBBP"
                       "BBGGGYYYYOOOOYYYYGGGBB"
                       "BBGGYYOOOOOOOOOOYYGGBB"
                       "BGGYYOOORRRRRROOOYYGGB"
                       "GGYYOORRRRRRRRRROOYYGG"
                       "GYYOORRRRRRRRRRRROOYYG"
                       "GYYOORRRRCCCCRRRROOYYG"
                       "GYOORRRRCCCCCCRRRROOYG"
                       "YYOORRRCCCCCCCCRRROOYY"
                       "GGGGGGGGGGGGGGGGGGGGGG"
                       "NNNNNNNNNNNNNNNNNNNNNN"))
   (make-level :name "Target" :difficulty 0.8 :slots 4 :seed 46 :par 200
               :order "22222222222222111211011111101100010000000000000111112220"
               :rows '("CCCCCCCBBBBBBBBCCCCCCC"
                       "CCCCCBBWWWWWWWWBBCCCCC"
                       "CCCCBWWWRRRRRRWWWBCCCC"
                       "CCCBWWRRRRRRRRRRWWBCCC"
                       "CCBWWRRGGGGGGGGRRWWBCC"
                       "CBWWRRGGGYYYYGGGRRWWBC"
                       "CBWRRGGYYYYYYYYGGRRWBC"
                       "BWWRGGYYBBBBBBYYGGRWWB"
                       "BWRRGGYBBWWWWBBYGGRRWB"
                       "BWRRGYYBWWKKWWBYYGRRWB"
                       "BWRRGYYBWKKKKWBYYGRRWB"
                       "BWRRGYYBWKKKKWBYYGRRWB"
                       "BWRRGYYBWWKKWWBYYGRRWB"
                       "BWRRGGYBBWWWWBBYGGRRWB"
                       "BWWRGGYYBBBBBBYYGGRWWB"
                       "CBWRRGGYYYYYYYYGGRRWBC"
                       "CBWWRRGGGYYYYGGGRRWWBC"
                       "CCBWWRRGGGGGGGGRRWWBCC"
                       "CCCBWWRRRRRRRRRRWWBCCC"
                       "CCCCBWWWRRRRRRWWWBCCCC"
                       "CCCCCBBWWWWWWWWBBCCCCC"
                       "CCCCCCCBBBBBBBBCCCCCCC"))
   (make-level :name "Spiral" :difficulty 0.6 :slots 4 :seed 23 :par 314
               :order "2222222222222222221111011100110000000000000000111111111"
               :rows '("RRRRRRRRRRRRRRRRRRRRRR"
                       "KKKKKKKKKKKKKKKKKKKKKO"
                       "GBBBBBBBBBBBBBBBBBBBKO"
                       "GKWWWWWWWWWWWWWWWWWPKO"
                       "GKOYYYYYYYYYYYYYYYWPKO"
                       "GKOWNNNNNNNNNNNNNGWPKO"
                       "GKOWPRRRRRRRRRRRNGWPKO"
                       "GKOWPNTTTTTTTTTONGWPKO"
                       "GKOWPNGBBBBBBBTONGWPKO"
                       "GKOWPNGTCCCCCPTONGWPKO"
                       "GKOWPNGTOYYYCPTONGWPKO"
                       "GKOWPNGTOCKGCPTONGWPKO"
                       "GKOWPNGTOCCCCPTONGWPKO"
                       "GKOWPNGTRRRRRPTONGWPKO"
                       "GKOWPNGTTTTTTTTONGWPKO"
                       "GKOWPNYYYYYYYYYONGWPKO"
                       "GKOWPNNNNNNNNNNNNGWPKO"
                       "GKOWBBBBBBBBBBBBBGWPKO"
                       "GKOWWWWWWWWWWWWWWWWPKO"
                       "GKRRRRRRRRRRRRRRRRRPKO"
                       "GKKKKKKKKKKKKKKKKKKKKO"
                       "YYYYYYYYYYYYYYYYYYYYYO"))
   (make-level :name "Sunflower" :difficulty 0.3 :slots 4 :seed 9 :par 201
               :order "22222222021122222020000011000010110100100010111111211122"
               :rows '("CCCCCCCCCCCCCCCCCCCCCC"
                       "CCCCCCCCCCOYCCCCCCCCCC"
                       "CCCCCCYOCOOYYCYOCCCCCC"
                       "CCCCCCYOOOOYYYYOCCCCCC"
                       "CCCCCCYYOOOYYYOOCCCCCC"
                       "CCCYYYYYOOKKYYOOOOOCCC"
                       "CCCOOYYYKKTNKKOOOYYCCC"
                       "CCCCOOOKNTNTNTKYYYCCCC"
                       "CCCOOOOKTNTNTNKYYYYCCC"
                       "CCOOOOKTNTNTNTNKYYYYCC"
                       "CCYYYYKNTNTNTNTKOOOOCC"
                       "CCCYYYYKNTNTNTKOOOOCCC"
                       "CCCCYYYKTNTNTNKOOOCCCC"
                       "CCCYYOOOKKNTKKYYYOOCCC"
                       "CCCOOOOOYYKKOOYYYYYCCC"
                       "CCCCCCOOYYYOOOYYCCCCCC"
                       "GGGGGGOYYYYOOOOYGGGGGG"
                       "GGGGGGOYGYYOOGOYGGGGGG"
                       "GGGGGGGGGGGGGGGGGGGGGG"
                       "GGGGGGGGGGGGGGGGGGGGGG"
                       "GGGGGGGGGGGGGGGGGGGGGG"
                       "GGGGGGGGGGGGGGGGGGGGGG"))
   (make-level :name "Waves" :difficulty 0.3 :slots 4 :seed 39 :par 203
               :order "222212221011111100111001000000000000002222222222221111111"
               :rows '("WWWWWWWWWWWWWWWWWWWWWW"
                       "WBCCCBBBBBBBBCCCBBBBBW"
                       "WCCCCCBBBBBBCCCCCCBBBW"
                       "WCWWWCCBBBBCCWWWCCCBBW"
                       "WWWWWWCCCCCCWWWWWWCCCW"
                       "WWBBBWWCCCCWWBBBWWWCCW"
                       "WBBBBBWWWWWWBBBBBBWWWW"
                       "WBGGGBBWWWWBBGGGBBBWWW"
                       "WGGGGGBBBBBBGGGGGGBBBW"
                       "WGCCCGGBBBBGGCCCGGGBBW"
                       "WCCCCCGGGGGGCCCCCCGGGW"
                       "WCYYYCCGGGGCCYYYCCCGGW"
                       "WYYYYYCCCCCCYYYYYYCCCW"
                       "WYBBBYYCCCCYYBBBYYYCCW"
                       "WBBBBBYYYYYYBBBBBBYYYW"
                       "WBBBBBBYYYYBBBBBBBBYYW"
                       "WBBBBBBBBBBBBBBBBBBBBW"
                       "WBCCCBBBBBBBBCCCBBBBBW"
                       "WCCCCCBBBBBBCCCCCCBBBW"
                       "WCWWWCCBBBBCCWWWCCCBBW"
                       "WWWWWWCCCCCCWWWWWWCCCW"
                       "WWWWWWWWWWWWWWWWWWWWWW"))
   (make-level :name "Diamond" :difficulty 0.3 :slots 4 :seed 28 :par 174
               :order "222222222001110111101111111100000000000001002012222221122221"
               :rows '("KKKKKKKKKKCCKKKKKKKKKK"
                       "KKKKKKKKKCWWCKKKKKKKKK"
                       "KKKKKKKKCWWWWCKKKKKKKK"
                       "KKKKKKKCWWOOWWCKKKKKKK"
                       "KKKKKKCWWOPPOWWCKKKKKK"
                       "KKKKKCWWOPPPPOWWCKKKKK"
                       "KKKKCWWOPPGGPPOWWCKKKK"
                       "KKKCWWOPPGBBGPPOWWCKKK"
                       "KKCWWOPPGBBBBGPPOWWCKK"
                       "KCWWOPPGBBYYBBGPPOWWCK"
                       "CWWOPPGBBYRRYBBGPPOWWC"
                       "CWWOPPGBBYRRYBBGPPOWWC"
                       "KCWWOPPGBBYYBBGPPOWWCK"
                       "KKCWWOPPGBBBBGPPOWWCKK"
                       "KKKCWWOPPGBBGPPOWWCKKK"
                       "KKKKCWWOPPGGPPOWWCKKKK"
                       "KKKKKCWWOPPPPOWWCKKKKK"
                       "KKKKKKCWWOPPOWWCKKKKKK"
                       "KKKKKKKCWWOOWWCKKKKKKK"
                       "KKKKKKKKCWWWWCKKKKKKKK"
                       "KKKKKKKKKCWWCKKKKKKKKK"
                       "KKKKKKKKKKCCKKKKKKKKKK"))
   (make-level :name "Quilt" :difficulty 0.9 :slots 3 :seed 7 :par 273
               :order "00100100100000000000000111111222211111111122112222222222222"
               :rows '("RRRRRRBBBBBPPPPPCCCCCC"
                       "RRRRRRBBBBBPPPPPCCCCCC"
                       "RRGGGRBPPPBPOOOPCCCCCC"
                       "RRGOGRBPCPBPOWOPCCRCCC"
                       "RRGGGRBPPPBPOOOPCCCCCC"
                       "RRRRRRBBBBBPPPPPCCCCCC"
                       "YYYYYYGGGGGOOOOOWWWWWW"
                       "YYCCCYGWWWGORRROWYYYWW"
                       "YYCCCYGWWWGORRROWYYYWW"
                       "YYCCCYGWWWGORRROWYYYWW"
                       "YYYYYYGGGGGOOOOOWWWWWW"
                       "BBBBBBPPPPPCCCCCRRRRRR"
                       "BBYYYBPBBBPCGGGCRPPPRR"
                       "BBYWYBPBRBPCGYGCRPBPRR"
                       "BBYYYBPBBBPCGGGCRPPPRR"
                       "BBBBBBPPPPPCCCCCRRRRRR"
                       "GGGGGGOOOOOWWWWWYYYYYY"
                       "GGPPPGOOOOOWCCCWYWWWYY"
                       "GGPRPGOOYOOWCBCWYWGWYY"
                       "GGPPPGOOOOOWCCCWYWWWYY"
                       "GGGGGGOOOOOWWWWWYYYYYY"
                       "GGGGGGOOOOOWWWWWYYYYYY"))
   (make-level :name "Mandala" :difficulty 0.9 :slots 3 :seed 14 :par 276
               :order "000000000000000001101111111111111211222212222222222222"
               :rows '("KKKKKKKYCCCWWWPKKKKKKK"
                       "KKKKKYCCCCCWWWWWPKKKKK"
                       "KKKKCCCCWWWBBBWWWWKKKK"
                       "KKKCCCWWWWWBBBBBWWRKKK"
                       "KKOOCWWWPPPOOOBBBRRRKK"
                       "KCOOPWPPPPPOOOOOOORRWK"
                       "KOOPPGPPOOORRROCCOORRK"
                       "COOPPGGOOOBGRRGCCOORRW"
                       "OOPPGGBBBBBGGPGGCCOORR"
                       "OOPPGGBBYBGCPPGGCCOORR"
                       "OOPPGGBYYRGYYPPGCCOORR"
                       "PPYYBBWRROOWPOOCWWRRYY"
                       "PPYYBBWWRRBWCOCCWWRRYY"
                       "PPYYBBWWRWWCCCCCWWRRYY"
                       "GPPYYBBWPPWCYYYWWRRYYB"
                       "KPPYYBBYPPPYYYRRWRRYYK"
                       "KGPPYYYYYYYRRRRRGRYYBK"
                       "KKPPPCCCYYYRRRGGGBYYKK"
                       "KKKPGGCCCCCGGGGGBBBKKK"
                       "KKKKGGGGCCCGGGBBBBKKKK"
                       "KKKKKRGGGGGBBBBBOKKKKK"
                       "KKKKKKKRGGGBBBOKKKKKKK"))
   (make-level :name "Starry Night" :difficulty 0.9 :slots 3 :seed 8 :par 510
               :order "000000000000000000000000001000000110111111111111111111111111111111222222222222222222222222222222222"
               :rows '("BBBBBBCCCBBBBBWWWBBWCCCBBBBBBB"
                       "BBBBBGKCWWWBBWWYWWBBCCCWOOOOOB"
                       "BBBBBKKWWYWWBWYYYWBBWCCOOYYYOO"
                       "BBWWWCGWYYYWBWWYWWBBBCCOYYYYYO"
                       "BWWYWWKWWYWWBBWWWBBBBCCOYYYYYO"
                       "BWYYYKKBWWWBBBBBWWWWBWCOYYYYYO"
                       "BWWYKKGBBBBBBBBBWWYWWBCOOYYYOO"
                       "BBWWKGKBBBBBBBBWWYYYWBCWWWOOOB"
                       "WBBBGKBBBBBBBBBWWWYWWWWWYWWBBB"
                       "CBBBKKBBBBBWWWBBBWWWBCWYYYWBBB"
                       "CBBKKGKBBBWWYWWBBBBBWCWWYWWBBB"
                       "CBBKGKKBBBWYYYWBBBBWCCBWWWBBBB"
                       "CCBGKKGBBBWWYWWBBBCCCCBBBBBBBB"
                       "CCBBKGKBBBBWWWBBCCCCCBBBBBBBBB"
                       "CCBBGKKGBBBBBCCCCCCCBBBBBBBBBB"
                       "CCWBKKGKKBBBWCCCCCCBBBBBBBBBBB"
                       "BCWKKGKKGBBBBWCCCBBBBBBBBBBBBB"
                       "BBBBGKKGGGGGGGGGGGBBBBBBBBBBBG"
                       "BBBBKKGKGGGGGGGGGGGGBBBBBBBGGG"
                       "GGGKKGKKGGGGGGGGGGGGGGGGGGGGGG"
                       "NNKKGKKGNNNNNNNNNNNNNNNNNNNNNN"
                       "NKKGKKGKNYWYNNNNNNNYWYNNNNNNNN"
                       "NNGKKGKNNNNNNYWYWNNNNNNYWYWNNN"
                       "NNKKGKKNNNNNNNNNNNNNNNNNNNNYWN"
                       "NNKGKKGKNNNNNNNNNNNNNNNNNNNNNN"
                       "NNGKKGKKGNNNNNNNNNNNNNNNNNNNNN"))
   (make-level :name "Great Wave" :difficulty 0.5 :slots 3 :seed 13 :par 441
               :order "11011111111111111111111111111110020002000020000000000000000022202200022222222222222222222221"
               :rows '("TTTTTTTTTTTTTTTTTTTTTTTTTTTTTT"
                       "TTTTTTTTTTTTTTWWWTTTTTTTTTTTTT"
                       "TTTTTTWTTTTTWWTTTWWTTTTTTTTTTT"
                       "TTTTWWBWTTTTTTTTTTTWTTTTTTTTTT"
                       "TTTWWWBBTTTTTTTTTTTTTTTTTTTTTT"
                       "TTWWBBBBTTTTTTTTTTTTTTTTTTTTTT"
                       "TTWWBBCCCTTTTTTTTTTTTTTTTTTTTT"
                       "TWBWBCCCCTTTTTTTTTTTTTTTTTTTTT"
                       "TWBBBCCCTTTTTTTTTTTTTTTTTTTTTT"
                       "TWWBCCCTTTTTTTTTTTTTTTTTTTTTTT"
                       "TWBBCCCTTTTTTTTTTTTTTTTTTTTTTT"
                       "WBBBCCCTTTTTTTTTTTTTTTTTTTTTTT"
                       "TWBBCCCCCCCCCCTTTTTTTTTTTTTTTT"
                       "TWWBCCCCCCCCCCCCCTTTTTTTTTTTTT"
                       "TWBBBCCCCCCCCCCCBBBWTWWWTTTTTT"
                       "BWBWBCCCCCCCCCCCBWBWWWWWWTTTTT"
                       "BBWWBBCCCCCCCCCBBWWBBBBBBBTTTT"
                       "BBWWBBBBCCCCCBBBBWWBBCCCBBBTTT"
                       "BBBWWWBBBBBBBBBWWWBCCBBBBBBBTT"
                       "BBBBWWBWBBBBWWBWWTBBBBBBBBBTTT"
                       "CCCBBBWWWWWWWWWBBBBBBBBBCCCBBB"
                       "BBBBBBBBBCWCBBBBBBBBBCCCBBBBBB"
                       "BBBBBBCCCBBBBBBBBBCCCBBBBBBBBB"
                       "BBBCCCBBBBBBBBBCCCBBBBBBBBBCCC"
                       "CCCBBBBBBBBBCCCBBBBBBBBBCCCBBB"
                       "BBBBBBBBBCCCBBBBBBBBBCCCBBBBBB"))
   (make-level :name "Boogie Woogie" :difficulty 0.9 :slots 3 :seed 5 :par 485
               :order "000000000010000000001000001100200001211111111111111111111111112222222222222222222222222222"
               :rows '("WYWWYWWWWYWWWTWWWYWWBWWWWYWWYW"
                       "BYRYBRYRRYYYRBBBYBYYBYYRYYYYBY"
                       "WYWWYRRRWYWWWYTTTYWWBWWWWYWWRW"
                       "WYWWYRTRWTWWWYTBTBWWBWWWWBWWYW"
                       "WYWWBRRRWBWWWRTTTBWWYWWWWRWWBW"
                       "YBYTYYYRRTRYYTTYYTYYYYTYYYRYRY"
                       "WYWWYWWWWYWWWYWWWYWWRWYYYBWWBW"
                       "WYWWYWWWWRWWWYWWWYWWYWYRYBWWYW"
                       "WYWWYWWWWYWWWBWWWYWWYWYYYYWWYW"
                       "YYTBTYYRYYYYRBYYRBYBRTYBYYYYRY"
                       "WBWWBWWWWBWWWBBBBTWWYWWWWYWWYW"
                       "WYWWYWWWWRWWWYBYBBWWYWWWWYWWYW"
                       "WRWWTWWWWYWWWYBBBBWWYWWWWYWWRW"
                       "YYYBYRBYYBBYTYYYRTYYRYYYYYTRTR"
                       "WBWWRWWWWYWWWYWWWYWWRRRRRYWWYW"
                       "WYWWBWWWWBWWWBWWWRWWTRYYRBWWTW"
                       "WTWWYWWWWBWWWYWWWRWWYRRRRTWWYW"
                       "YTYYTYYYYBYYRBRYYTYYYYYBYRYYYY"
                       "WYWWTBBBBYWWWYWWWRWWYWWWWYRRBW"
                       "WYWWBBRRBYWWWRWWWYWWRWWWWBRRYW"
                       "WTWWBBRRBBWWWYWWWBWWYWWWWTRRBW"
                       "WBWWYBBBBTWWWTWWWBWWYWWWWYRRYW"
                       "YYRYYYYTRYYBBRYRYYYRYYBYYBTBYR"
                       "WRWWYWWWWYWWWYWWWYWWRWWWWYWWYW"
                       "YTYYYYYYBBRYYBBYYYRYRTRYYTYYYY"
                       "WYWWYWWWWYWWWYWWWYWWYWWWWYWWYW"))
   (make-level :name "The Scream" :difficulty 0.9 :slots 3 :seed 1 :par 521
               :order "00000000000000000000000000000011011111111111111111111111111122212212222222222222222222222222"
               :rows '("RRRRRRRRRRRRRRRRRRRRRRRRRRRRRR"
                       "RROOOOORRRRRRRRRRROOOORRRRRRRR"
                       "OOOOOOOORRRRRRRROOOOOOOORRRRRR"
                       "OOYYYYYOOORRRROOOOYYYYOOOORRRR"
                       "YYYYYYYYOOOOOOOOYYYYYYYYOOOOOO"
                       "YYOOOOOYYYOOOOYYYYOOOOYYYYOOOO"
                       "OOOOOOOOYYYYYYYYOOOOOOOOYYYYYY"
                       "OORRRRROOOYYYYOOOORRRROOOOYYYY"
                       "RRRRRRRROOOOOOOORRRRRRRROOOOOO"
                       "KKYYYYYRRROOOORRRRYYYYRRRROOOO"
                       "TYKKYYYYRRRRRRRRYYYYYYYYRRRRRR"
                       "NNCCKKCYYYRRRRYYYYBNNNYYYYRRRR"
                       "NNNTBBKKYYYYYYYYBNNNNNNNYYYYYY"
                       "NNTNNNBBKKYYYYBBNNNBBBBNNNYYYY"
                       "NTNNNNTNBBKBBBTTTBBBBBBBBBNNBB"
                       "TNNNNTNNNNBKKNKTKBBBBBBBBBBBBB"
                       "NNNNTNNNNTNNNKTTTBBBBBBBBBBBBB"
                       "NNNTNNNNTNNNNBTKTBBCCCCBBBBBBB"
                       "NNTNNNNTNNNNTNTTTKKCCCCCCCBBCC"
                       "NTNNNNTNNNNTNNKKKCCKBBBCCCCCCC"
                       "TNNNNTNNNNTNNNKKKNNBBBBBBBCCBB"
                       "NNNNTNNNNTNNNNKKKNNTNNNBBBBBBB"
                       "NNNTNNNNTNNNNKKKKKTNNNNNNNBBNN"
                       "NNTNNNNTNNNNTKKKKKNNBBBNNNNNNN"
                       "NTNNNNTNNNNTKKKKKKKNBBBBBBNNBB"
                       "TNNNNTNNNNTNKKKKKKKNBBBBBBBBBB"))
   (make-level :name "Kandinsky" :difficulty 0.9 :slots 3 :seed 44 :par 415
               :order "000000010100010011000000110000000001111111111111111111222222222221122222222222222222"
               :rows '("YYPPYYRROORRPPCCPPCCNNCCWWRRWW"
                       "YPKKPYROWWORPCGGCPCNPPNCWROORW"
                       "PKRRKPOWYYWOCGBBGCNPKKPNROGGOR"
                       "PKRRKPOWYYWOCGBBGCNPKKPNROGGOR"
                       "YPKKPYROWWORPCGGCPCNPPNCWROORW"
                       "YYPPYYRROORRPPCCPPCCNNCCWWRRWW"
                       "GGYYGGOOBBOOTTKKTTNNWWNNBBGGBB"
                       "GYCCYGOBNNBOTKRRKTNWYYWNBGKKGB"
                       "YCPPCYBNPPNBKROORKWYCCYWGKNNKG"
                       "YCPPCYBNPPNBKROORKWYCCYWGKNNKG"
                       "GYCCYGOBNNBOTKRRKTNWYYWNBGKKGB"
                       "GGYYGGOOBBOOTTKKTTNNWWNNBBGGBB"
                       "CCPPCCWWOOWWYYCCYYRRNNRRPPRRPP"
                       "CPKKPCWOGGOWYCGGCYRNPPNRPROORP"
                       "PKRRKPOGYYGOCGBBGCNPKKPNROWWOR"
                       "PKRRKPOGYYGOCGBBGCNPKKPNROWWOR"
                       "CPKKPCWOGGOWYCGGCYRNPPNRPROORP"
                       "CCPPCCWWOOWWYYCCYYRRNNRRPPRRPP"
                       "NNYYNNBBKKBBGGKKGGOOWWOOTTGGTT"
                       "NYCCYNBKNNKBGKRRKGOWYYWOTGBBGT"
                       "YCGGCYKNPPNKKROORKWYCCYWGBNNBG"
                       "YCGGCYKNPPNKKROORKWYCCYWGBNNBG"
                       "NYCCYNBKNNKBGKRRKGOWYYWOTGBBGT"
                       "NNYYNNBBKKBBGGKKGGOOWWOOTTGGTT"))
   (make-level :name "Mona Lisa" :difficulty 0.6 :slots 3 :seed 27 :par 366
               :order "0222222220200000000000000000000000000110110111111111111111111111111121222222222222222222220"
               :rows '("CTGCCTCGGTGGTGCKGGGGGTCCGTTGGG"
                      "GTGGTGCCGCGCKKKKKKKCCTCGCCTGGG"
                      "GGGTCGTGCGCKKKKKKKKKTGCGCGCCTC"
                      "GCCCGTCGTCKKKKKTKKKKKGCCGGGCGG"
                      "CCCCTTGGGKKKKTTTTTKKKKGCGCCCGG"
                      "GGGCGGGCGKKKTTTTTTTKKKGCGGGCGG"
                      "GGCGGGCGKKKKTTTTTTTKKKKGGGCGGG"
                      "GCGGGCGGKKKTOOTTTOOTKKKGGCGGGC"
                      "CGGGCGGGKKKTTNTTTNTTKKKGCGGGCG"
                      "GNGGGGNNNKKTTTTTTTTTKNNGGGNGGG"
                      "NGGGTNGNNKKTTTTTTTTTKNNGGNGGGG"
                      "NGGGGTGNNKKTTTTOTTTTKNNGGNGGGG"
                      "GGGTNGGNNKKTTRTTTRTTKNNGNTTTTN"
                      "GGGGTGGNNKKKTTRRRTTKKNNGNGGGGN"
                      "GGGNGTGNNKKKTTTTTTTKKNNNGGGGNG"
                      "NNGTNNNNNNKKKTTTTTKKNNNGNNNNNN"
                      "NNNNGNNNNNNKKTTTTTKKNNNNNGNNNN"
                      "NNNNNNGNNKTTTTTTTTTTTKNNNNNGNN"
                      "NGNNNNNNKKKKTTTTTTTKKKKNNNNNNG"
                      "NNNGNNNKKKKNKTTTTTKKKKKNGNNNNN"
                      "NNNNNGKKKKNKKKKKNKKKKKNKKNGNNN"
                      "GNNNNKKKKOOOOKKNKKKKKNKKKKNNGN"
                      "NNGNKKKKNKTTTTTTTTTTNKKKKKNNNN"
                      "NNNKKKKNKKTTTTTTTTTTKKKKKNKKNN"
                      "NNKKKKNKKKTOTTOTTOTTKKKKNKKKKN"
                      "NKKKKNKKKKKNKKKKKNKKKKKNKKKKKN"))
   (make-level :name "Pearl Earring" :difficulty 0.3 :slots 3 :seed 27 :par 501
               :order "111110000000000000000000000000000011011111011111111111111121111222222222222222222222222222222"
               :rows '("KKKKKKKKKKKKBKKKKKKKKKKKKKKKKK"
                      "KKKKKKKCBBBBBBBBBKYYYYKKKKKKKK"
                      "KKKKKKBBCCCCCCCBBBYYYYOYKKKKKK"
                      "KKKKKBBCCCCCCCCCBBBYYOYYKKKKKK"
                      "KKKKKBCCCCCCCCCCCBBYOYYYKKKKKK"
                      "KKKKBBBCCCCCCCCCBBBOYYYOKKKKKK"
                      "KKKKKBBBCCCCCCCBBBBBYYOYKKKKKK"
                      "KKKKKBBBBBBBBBBBBBBBYOYYKKKKKK"
                      "KKKKKKBBBBBBBBBBBBBTOYYYKKKKKK"
                      "KKKKKKKNBBBBBBBBBTTTYYYOKKKKKK"
                      "KKKKKKKNTTTKWTTTWKTTTYOYKKKKKK"
                      "KKKKKKKNTTTTTTTTTTTTTOYYKKKKKK"
                      "KKKKKKKNTTTTTTTTTTTTTYYYKKKKKK"
                      "KKKKKKKNTTTTTOOOTTTTTYYOKKKKKK"
                      "KKKKKKKNTTTTTTTTTTTTTYOKKKKKKK"
                      "KKKKKKKNKTTTTTTTTTTTYOYKKKKKKK"
                      "KKKKKKKNKTTTRRRRTTTTOYKKKKKKKK"
                      "KKKKKKKCWKTTTRRTTTTOYYKKKKKKKK"
                      "KKKKKKKWWKKTTTTTTTKKKKKKKKKKKK"
                      "KKKKKKKKNKKKTTTTTKKKKKKKKKKKKK"
                      "KKKKKKKKWWWWWWWWWWWWWWKKKKKKKK"
                      "KKKKKKKKWWWWWWWWWWWWWWKKKKKKKK"
                      "KKKKKKKOOOONOOOONOOOONOOOONKKK"
                      "KKKKKKOOONOOOONOOOONOOOONOOKKK"
                      "KKKKKOONOOOONOOOONOOOONOOOOKKK"
                      "KKKKONOOOONOOOONOOOONOOOONOKKK"))
   (make-level :name "The Kiss" :difficulty 0.9 :slots 3 :seed 2 :par 503
               :order "21212000000000000022222022111111000000000000000022222222222222222221121111111111111111111110"
               :rows '("NNNONYYNONNOOYOOONNNNONNNOONNN"
                      "OONNONOONNOOOGGGNNNONNONNNONOO"
                      "ONNNNNONOONKKGGGYYYYOYONNNOOYO"
                      "NONNNOOONYNKKKKKYYYYNNYNNNNYYN"
                      "ONNOONOONNYKKKKKPRPRPONNNYOYNN"
                      "OOYNOONNNNKKKKKKYTTTYONNNNNONN"
                      "ONNNNYNNOYKKYKYYYTTTYYNNNNNONY"
                      "NNNYNNONNYKKYWWYYTTTGYNNNONNNO"
                      "NNNONOONYYYYYWWYYTTTWGYNOONYON"
                      "NNNONONNYWWYYYYYYTTYGYYNONNNNN"
                      "NNNONOYYYWWYYYYYYTTYYYYYNONOOO"
                      "ONNONONYYWWYYYYYYRWRYYYYNNONNN"
                      "ONYOONOYYWWYKKYYYYRYYYYYYONNYO"
                      "YNNONNNYYYYYKKYYYYYYBYYYNONOON"
                      "ONNOONNYYYKKKKYYYYYBWBYYNNOONY"
                      "OOOONOOYYYKKYYYYYYYYBYGYNNONNN"
                      "ONYNYOYYYYKKYYYYYYPYYGWGONONON"
                      "NONNONNYYYYYWWYYYPWPYYGYNNNONN"
                      "NNNNNNNYKKYYWWYYYYPYYYYYNNNOYO"
                      "ONNNNYOYKKYYYYYYYYYYYRYYYNNNYO"
                      "GGGGWPPYKKYYYYYYYYYYRWRYPGGGGG"
                      "GGWGGGGYKKKKKYYYYBYYYRYYGGGYGG"
                      "GGGGGGGYYYKKKYYYBWBYYYYYGGGPGG"
                      "YGGGWGWYYYKKKYYYYBYYYYYYGGGWGW"
                      "GWGGGGGYYYYYYYYYYYYYYYYYGGGYWG"
                      "PYGGGGGGGPGYGYGGGGGWGGGGGGYGGG"))
   (make-level :name "Café Terrace" :difficulty 0.9 :slots 3 :seed 1 :par 394
               :order "201022000001111112222111121112122211121112222122221111111112222222222000000000000000000000000"
               :rows '("OYYYYOYYYYOYBBBBBBBBBBBBBBBBBB"
                      "YYYYOYYRROYYBBWYBBBBBBBBBBBBWY"
                      "YYGGYYYRRYYYBBYBBBBBBBBWYBBBYB"
                      "YYGGYYYRRYYYBBBBBBWYBBBYBBBBBB"
                      "YOGGYYOYYYYOBBBWYBYBBBBBBBBBBB"
                      "OYGGYOYYYYOYBBBYBBBBBBBBBBBWYB"
                      "YYYYYYYYYYYYYBBBBBBBBWYBBBBYBB"
                      "OOOOOOOOOOOOOOOBBBBKKKKKBBBBBB"
                      "YYYYYYYYYYYYYYYYYBBKKKKKBBBBBB"
                      "OOOOOOOOOOOOOOOOOOOKOKOKBBBBBB"
                      "OYYYYOYYYYOYBBBBBBBKOKOKKKKKKK"
                      "YYYYOYYYYOYYBBBBBBBKKKKKKKOKOK"
                      "YYYOYYKYOYYYBBBKBBBKOKOKKKOKOK"
                      "YYOYYYKOYYYKBBBKBBBKOKOKKKKKKK"
                      "YOYWWYKYWWYKBBBKBBBKKKKKKKOKOK"
                      "OYYNYOYYNYOKBWWBBBBKKOKKKKOKOK"
                      "OOOOOOOOOOOOONOOOBBKKOKKKKKKKK"
                      "OOOOOWWOOOOWWOOOOBBKKKKKKKKKKK"
                      "OOOOONOOOOONOOOOOBBKKKKKKKKKKK"
                      "BBCBBTCBTBCTBBCBBTCBTBCTBBCBBT"
                      "CTBBCBBTCBTBCTBBCBBTCBTBCTBBCB"
                      "TBCTBBCBBTCBTBCTBBCBBTCBTBCTBB"
                      "CBTBCTBBCBBTCBTBCTBBCBBTCBTBCT"
                      "BTCBTBCTBBCBBTCBTBCTBBCBBTCBTB"
                      "CBBTCBTBCTBBCBBTCBTBCTBBCBBTCB"
                      "BBCBBTCBTBCTBBCBBTCBTBCTBBCBBT"))
   (make-level :name "Water Lilies" :difficulty 0.9 :slots 3 :seed 1 :par 369
               :order "02111011111111111111111122111211111112222222222222222222222202002000000000000000000000200000"
               :rows '("GGGGYGGYGGYGGGGGGYGGGGGGYGGGGY"
                      "GGGGGGGYGGYGGYGGGGGYYGYYGGGGGY"
                      "GGGGYGGGGGGGGGGGYGGGGYGGGGGGGY"
                      "GGGGGGYGYGYGGGGGGGGGGGGGGGGGYG"
                      "GGGGYYGGGGGNNNNNNNNGGGGGGGGGYY"
                      "GGGYGGYGNNNGNGGNYGNNNNGYGGGGGY"
                      "GGGGYNNNGNGGNYGNGGNGGNNNNGYGGG"
                      "YGGNNGNGGNGNNNNNNNNGGNGGNNNGYY"
                      "GNNNGGNGNNNKKKKKKKKNNNGGNGGNNG"
                      "NGGNGNNNKKKGGGGGGGGKKKNNNGGNGN"
                      "NYYNNKKKGGGGGGGGGGGGGGKKKNNNGG"
                      "NNNKKGGGGGGGGYGGGYGGGGGGGKKNNG"
                      "NKKBCCBCCBCCBCCBCCBCCBCCBCCKKN"
                      "KCBCCBCCBCCBCCBCCBCCBCCBCCBCCK"
                      "CBCGPPCBCCBCCBGGBCCBCCBCGGCCBC"
                      "GGGGCCBCCBGGGPWBCCBCCGGGGCCBCC"
                      "GGGCCBCCGGGGGCBCCBGGGGPPCCBCCG"
                      "CBPPBCGGGGBCCBCCBGGGCCBCCBCGGG"
                      "BCCBCCBCCBCCBCCBCCBCCBCCBCCBCC"
                      "CCBCCBCCBPWBCCBCCBCCBCCBCCBCCB"
                      "CBCCBCCBCCBCCBCCBCPPCCBCCBCCBC"
                      "BCCBCCBCCGGCBCCBCCBCGBCCBPWBCC"
                      "CCBCCGGGGGCBCCBCGGGGGCCBCCGGGG"
                      "CBCGGGGGCCBCCGPPGGGBCCBCGGGGGC"
                      "BCGGGPWCCBCCGGGGCCBCCBCGGGCBCC"
                      "CCBCCBCCBCCBCCBCCBCCBCCBCCBCCB"))
   (make-level :name "Grande Jatte" :difficulty 0.9 :slots 3 :seed 52 :par 484
               :order "111112112111111111111112222222222111221111112222222200000000000000000000000000000220222222"
               :rows '("GNNGNGNYGYYGGGGYNGGGGGNNYNGGYN"
                      "GGGGGGGNGYGNNGGGNGGGGYGGYNYNGG"
                      "GYNGGNYNGNNGGGGNGNGYYYGYYGYYGG"
                      "GGGGYGGGGGGNGGGYGNGYGGNYNGGGYG"
                      "GGGGYGYYGGGGGGYGGGCBCWBCCBWBWB"
                      "GNGYGNNYGGGGGYNYGNBBBCCWRRRRRR"
                      "GGOOOOOOGGGNGGGGGNCWWCWBBCWWCC"
                      "YNYGGGGYYNNGNRRRRRRBWBBWWWTTWB"
                      "GYGYTTGRRRRRRGNNGGCCCCWCCBKKBB"
                      "GYGBKKGGYGYBNGGTTGGYGGBNYYNRGG"
                      "GYGGBBGYGTTGGGGKRGYRRRRRRGKRGG"
                      "YGGGKKYYYOOGGNBKKGGGBGGNYGNNYG"
                      "GGNGKKGYGOOGGNYKKGGGGTTGYYKNNG"
                      "GGGGKKYYYBOGBGGKKGGGGNNYGGKKBG"
                      "GGNGBRGGGOBNBGGKKYGBGBRGGYKKGB"
                      "GGGGNKGGYOOGBYGKNGBGNPPGYGKKNG"
                      "GGNNKNGGGOOGBYGKRGGNGPNGYYKRBG"
                      "GGGNKKGGGOOGGYYKKYGGGBPYGGKKGG"
                      "GGGYBKYGGOOGGGGNKBGBGPPYGGKKBG"
                      "GGNGNKGGGOONGGYKNGGGGPPGGGKKGB"
                      "GGGYKKGYGOONYGYKKGBGGPPYGYKRGG"
                      "NNGGKKGGBOOGGGYKKGGNGPPYGGKKGG"
                      "GGGYKKGGNOOGGYYKRGBBGNPYYGKNGG"
                      "GGGYRKGBBONGYGGKRNGGGBPGGGKKGG"
                      "GGYYKNGNBGGYGYGKKGGYGPPGGGBGGY"
                      "GGYYKBNGBGGGGGGGGGBGYYGYGNBBGY"))
   (make-level :name "Birth of Venus" :difficulty 0.2 :slots 3 :seed 7 :par 526
               :order "112221001000000000000000000000000000011101111111111112221222221112222222222111111122222222221"
               :rows '("CWCCWCCCCCCCCWCCCCCCCCCCCWCCCC"
                      "CWCCCCCCCCCCCCCCCWWCCCCCCCCCCC"
                      "CCCCTWWCCCCCCCWCCCCCWCCCCCGGGG"
                      "CWCTNTWCCCCWPOYTWCCWWOCCWCGYGG"
                      "CBBTTTTCWPWCOYTTTCCCCTTCCWGGGG"
                      "CBBTTTTCCCCWCYTNNWCCWTTCCCGGGG"
                      "CBBTTTCCCCCCOPYRTCCCWWWWWCGGYG"
                      "CBBTTCCCCCCPWYTTTCCCWWWWCCGGGG"
                      "CBBTTCCCCCCCOYTTTCCCWWWRPRGGGG"
                      "CBBTTCCCCCCCCOYTTTCCWYWRRRYGGG"
                      "CCCCTCCCCCPWOYTTTTCCWWWRPRGGGG"
                      "WCCCCCWCCCCCWYTTTTCCWYWRRRGGGG"
                      "CCCCCCCCPWCCOTYTTTCCWWWRPRWCGG"
                      "CCCCCCCCCCCCCYTTTTWCWYWRRRCCGG"
                      "WCBBCCCWBBCCOYTTTTCCWWWRPRBBGG"
                      "BBCCCCBBCCCCBOYTTTBBWYWRRRCWGG"
                      "CCCCBBCCCCBBOYTTTTCWWWWRPRWCGG"
                      "CCBBWCCCBBCWCYTTTTWCWYWRRRBBGG"
                      "BBCWCCBBCCWCOBYTTWBBWWWRPRCCGG"
                      "CCWCBBCCCWBBCCTTTBCCWYWWCCCCGG"
                      "CWBBCCCCBBPPPPPTPPPPPBWCCCBBGG"
                      "BBCCCCBBCPPPPPPPPPPPPPCCBBCCGG"
                      "CCCCBBWCWWPPPPPPPPPPPWWBCCCWGG"
                      "CCBBCWCCBOWOWOWOWOWOWOCCCCBBGG"
                      "BBCCWCBBCCWWWWWWWWWWWCCCBBCCGG"
                      "CCCWBBCCCCBBCCCWBBCCCCBBWCCCGG"))
   (make-level :name "Van Gogh" :difficulty 0.1 :slots 3 :seed 7 :par 439
               :order "22211212211110111111111110110111111000000011122222212222222222222222220000000000000000000001"
               :rows '("CBBBBCCCCCCCCCCCCCWWWWCCCBBBCC"
                      "BBBBCCCCCCCCOOOOOOOWWCCCCCCCCC"
                      "BBBCCCCCCCBOOOOOOOOOCCCCCCCCWC"
                      "CCCCCCCCBBOOOOOOOOOOOCCCCCWWWW"
                      "CCCCCCCCCBTOOOOOOOOOCCCCCCWWWW"
                      "CCCWWWCCCCBTOOOOOOOTCBBCCCCWWW"
                      "CCCCWWWWCCGTOOTTTOOTCBBBBCCCWW"
                      "CCCCCWWWWWTTWGTTTGWTTCBBBBBCCC"
                      "CCCCCCCWWWTTTTTTTTTTTCCCBBBBBC"
                      "CCCCCCCCCWTGTTTNTTTTTCCCCBBBBB"
                      "CCCCCBCCCCTRTTTRTTTORCCCCCCBBB"
                      "CCCCCBBBCCRTTTTTTTTROCCWWCCCBB"
                      "CCCCCBBBBCGOTTRRRTTOOCWWWWCCCC"
                      "CCCCCBBBBCCROOROOROOCCWWWWCCCC"
                      "WCCCCBBBCCCOOROOROORCCWWWCCCCC"
                      "WCCCCCCCBBBBROOROORBBCCCCCCCCC"
                      "CCCCCCCCBBBBCOROORBBBCCCCCCCCW"
                      "CCCCCWWBBBBCCCCTBBBCCCCCBBCCCW"
                      "CCCCWWCCCCCKKKWCWCCCKKKCCCCCCC"
                      "BCCCWCCCCKKKKKWCWCKKKKKCCCCCCC"
                      "BBCCCCCCCKKKKWCCCWKKKKCCCCCCCC"
                      "BBCCBBBCCCKCCBBBCCCKCCBBBCCCCC"
                      "BBBBBBBBCCCBBBBBBCCCBBBBBCCCCC"
                      "CBBBBBBCCCBBBBBBCCCBBBBBBCCCBB"
                      "CCBBBCCCCCCBBBCCCCCCBBBCCCCCCB"
                      "CCCCCCKKKCCCCCCKKKCCCCCCKKKCCC"))
   (make-level :name "Hunters in Snow" :difficulty 0.4 :slots 3 :seed 6 :par 499
               :order "002221121222211000000000222022000000100000000000011111111111122112111111111122222222222222"
               :rows '("GCCCGCCCGCCCGCCCGCCCGCCCGCCCGC"
                      "CCCGCCCGCCCGCCCGCCCGCCCGCCCGCC"
                      "CCGCCCGCCCGCCCGCCCGCCCGCCCGCCC"
                      "CGKNCGCCCGCCCGCCCGCCCGCCCGCCCG"
                      "GCCNKCCCGCCCKNCCGCCCGCCCGCCCGC"
                      "WCKNWCCKNCCWCNKWKNWWCCWCCWWCCW"
                      "CWWNKWCCNKCCKNCCWNKWWCCWWCCWCC"
                      "CCKNCCWKNCWCCNKCKNWCCWCCWWCCWW"
                      "WWCNKCCWNKCWKNCWCNKWCCWWCCWCCW"
                      "GWKNGGWKNWWWGNKGKNWNWGGWGWWWGW"
                      "WWWNKGWWNKWWKNWWGNKWWWWWGGCWGW"
                      "WWKNGGGKNWWGWNKWKNCCCGGCCCNCCC"
                      "GGGNKWWWNKGGKNCCCNKCKCCWWNCWWG"
                      "WWKNWWWKNWWWWNKGKNCNCGWCCCCCNG"
                      "WWWNKWWWNKWWWNWWWNKWWGCCNCCRCW"
                      "WWWNWWWWNWWWWNWWWNWWWWGCCCCCWG"
                      "WWWNWWWWNWWWTNWWWNWRWWWRGGNRWW"
                      "WWWNWWWWNWTWRNWWWNOOWWOOWWOOWW"
                      "WWWNTWWTNWKWRNWWWNOOWWOOWWOOWW"
                      "WWWNKWWNNWKWRNWWWNWWWWWWWWWWWW"
                      "WWWNKWWNNWKWRNWWWNWWWWWWWWWWWW"
                      "WWWNKWWNNWKWWNWWWNWWWWWWWWWWWW"
                      "WWWNKWWNNWWWWNWWWNWWWWWWWWWWWW"
                      "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWW"
                      "WWKNWKNWKNWKNWKNWWWWWWWWWWWWWW"
                      "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWW"))
   (make-level :name "Tree of Life" :difficulty 0.9 :slots 3 :seed 1 :par 347
               :order "222222200000000000000000000000000000110111111222121111111111111111111111222222222222222222220"
               :rows '("YYYYYYYKONNNYYYYYYONNNYYYYYYYY"
                      "OYYYOYKKONNNNYYYYYYNNNNYYYYYOY"
                      "YYYNNNNNNNNNNNNNNNNNNNNNNNNNOY"
                      "YYYNYOYNNNNNNNYYYNNKNNNNYYYNNY"
                      "YYNNNNNNNNYNNNNNNNKKYNNNNNNNNN"
                      "OYNYNNYNYNOYNYNNYNONYYNYNNONYN"
                      "YYNNYNNNNNYONNYNNNNNOYNNYNNNNN"
                      "YYYNNNNYNYYYYNNNNYNYYYYNKNNYNO"
                      "YYYYOYONNYYYYYYYONNYYYYKKYYNNO"
                      "YYYYOKNNNNNNYYNNNNYYNNNNYYNNYY"
                      "YOYYKKYNNYYNNYNNKYYNNYYNNOYOYY"
                      "YYYYYYONNNNYNYNNKYYNNNNONOYOYY"
                      "YOYOYOYNNNYNNONNNYYNNNYNNYYYYO"
                      "YYYYWYYNYONNNYNNKYYNYYNNNYYOYY"
                      "YYYWBWYNNNNYNONNKYYNNNNONWYYYY"
                      "OYYYWYYYOONNYYNNNYYYYYNNWBWYYY"
                      "YNNNNNYYYYOYYONNKOOYYYOYYWNNNN"
                      "YNNNNNNOYYYYOYNNKYYYYYYYYNNNNN"
                      "YNNNNYNOYOYWOYNNNYYYWYOYYNNNNY"
                      "YNNYNNNYYYWBWYNNKYYWBWYOYNNYNN"
                      "YYNNYNYYYYYWYYNNKYYYWOYYYYNNON"
                      "RRWBGGBPGGWGGBNNNWYGRRYWGGPGGG"
                      "GBGWGWGPBGPYWBNNKGGRWGGYGYYRYG"
                      "WGBBPYGYPGPYWRNNKYBYWBBGWBGGRG"
                      "GRRRRGBBYGBRGBYYOBWBYPPGBGGBGW"
                      "GYRGYGYGYYGPPYYYYGPRBBPGBPWRPW"))
   (make-level :name "The Bedroom" :difficulty 0.9 :slots 3 :seed 22 :par 672
               :order "2222222000010111220000000000000000000000000011111111111111111111111111122222222222222222222220"
               :rows '("CBBBBCBCCCCCCCCWBCWCCCCCCCCCCC"
                      "BBBBGCCWCCCCCCCCCCCCCCCCCCCCCG"
                      "BBBBBBCCCGGGGYGCCOOOCCCONOGCCC"
                      "BBBBBCGCCGBCCCNCCOTOBCCOTOCCCC"
                      "BBBBBCWCCGGCCCGBCYOOCCCOOOCGCC"
                      "BKCBBCCCCGCBCCGCCCCCYOOCCCGCCG"
                      "BBCBBCWCCGCCCCGCCCBCOTOCCCBCCC"
                      "BGBBBCCBCGCCCCCCCCWBONRBCCCCCC"
                      "BBBBBCCCCGGGCGGCCWCCBCCCCCCCCC"
                      "BBKGBCCCCCCCCCCCCCOYYYYOYYYYYC"
                      "BGBBBCCYCCBGWCCCCCYWWWYWWWWYYC"
                      "BKBOBCONOBWCCCCGCCYWWWWWWWWYYC"
                      "BBBOBCOOOCCCCCCBCYYWWWWWYWWYYC"
                      "BBKOBCCGCCCCCCCCCYYWYTYYYYYYYC"
                      "BGBOBBCCCOCCCCCRRRRRONPRRRNRYC"
                      "BBBTYYYCCOCCCWCRRRRRRRRRRRRRYC"
                      "GNNOGNNNNONNNNNRRRRRRRRRRRRRYN"
                      "TTNONTNGTONGNNNNRRRPRRRRRRRRYN"
                      "NNGONTONNYYTYNGRRRRRORRRRORRWN"
                      "NGTKNGNNNONNNGNRRRRRRRRRRROROG"
                      "GNNNGNNNGYNNGNTRRORPRRRRPRRRYN"
                      "NNNNNNNGNONGONYPRRRRRRRRRORRYO"
                      "TTCNNNGNNNGNONWRRRRRRRRRRRNRYN"
                      "NGNONGNNNYTNNGYYYOTYYWYYYYYYYG"
                      "NNNNGNNNGNNNGNONGNNNGKNNGNTNYN"
                      "NNNGNNNGNNNGNNNGNNOGTNNGNNNGOO"))
   (make-level :name "Irises" :difficulty 0.9 :slots 3 :seed 3 :par 478
               :order "000211110111111111111111000111111111111222000020200222220000222222222222220200000000000022222"
               :rows '("YYOYYYYYYTYYYYYYYYOYOOOOOOOOOO"
                      "YYYYYYYYYYYYYYYYYYOOOOOOOOOOOO"
                      "YYYYYYYYYYYYYYYYYYOOOOYONOOOON"
                      "WYYYYBWBYOYTYYOYYTYOROOOOOOOOR"
                      "YYYYYBKKYYYYYYYYYYYYYYYOTYYYOY"
                      "YYYYYBBBYOYYYOOYCCBYYYYYYYYYWY"
                      "YYYYOYYBCGYOYYWTBKBWWYWYYYYYYO"
                      "NGGGGNGGKBGGGGGGBBBGGGGGGGGGCG"
                      "GGGGGBCKBBGGGGGGGGGGGGGGGBCBCB"
                      "GGGWWTKBGGGGGGGGGGGGGGGKCBKBBB"
                      "GGGWTWCBGGGNGGGGGGGGGGGGKBBBBC"
                      "GGGWWWKBGCGBCGGYGGGGGBCBBWBBGG"
                      "GGNGGBGBGGGBCBCGGGGGGBKGBKBBYG"
                      "GGGYGBBBGGGBKBGGGGGGGBBBBBBGGG"
                      "GGGYYGGGGGGBBBGGGGGYGGCGGGGGGG"
                      "GYGCGNGNGGGGBCBGGGGGGGGGGGYGGN"
                      "GGGGGGGGCGNGBKCGNGNGGGGGCNYGGG"
                      "OGOGOORGOGOGBBBYRGOGOGOOOGRGNO"
                      "OGYOGOOYGGYGNORGOGOGOGOORGOGOR"
                      "ONROCOOGGGOOGOOGOGOGGGROOGOGGO"
                      "RGOOGRGYGGROGOOGGNOGGGOOOGRGGO"
                      "OOGRGOGOGGONGROGCORGGOGRNGNGGO"
                      "ORGOCOGRNRGOGONOGNCOGNGOGOCOGO"
                      "GOGOCOGOGRNNGOGOGONRGOGYGOGRNR"
                      "GONOGOGNGOGOGNGOGRGOGOGOGRCRGO"
                      "GOGOGRGOGOGRGNGRGOGOGOGOGRGONO"))
   (make-level :name "Impression" :difficulty 0.5 :slots 3 :seed 25 :par 599
               :order "1111111111111111111111111101111222212222222222222200202222202222222000000000000000000002000000001"
               :rows '("WCWCWBCCCCCCCWCWCWCBCCBCCCWCWC"
                      "CCCWBCCWCWCWCCCCCCWCWCWCWCCCCC"
                      "CYCCCWCCCGCCCCWCWBYCCCCCCCCWWW"
                      "CCCBGCCBWCWCWCCCCCGCCWBWCCBWCC"
                      "CCWBWCWBCGCGCCCWCWWWCCBCGCBCWG"
                      "WCCBBBCGBKCWCWCRWCGCCCBBBCBBBC"
                      "CGCCGWBBCWBCCCRRRCWCWCBCCBBCCB"
                      "CWGBCCCBCWWBWOOONRCWCCBWCYGWBW"
                      "CCCBWCWBWCCCCCRPRCCWCWBCGCBCCC"
                      "WGWBCCCBCCCWCCCRCCCGCCGGTCBBWC"
                      "CCCBCWCBCWCCCCCCCCWCWCBCCCBBGC"
                      "CWCBCCCBCCWCWCWCWCCCCCBCGTBWBW"
                      "CBCCBBCBBCBOOOOOYBCKBCOBCGBCBB"
                      "KKGBGCBBCBBCBBCBBWBKCBKCBBCBGW"
                      "BCBBCBNCCBCBBOOONOOCBKKKKCKBCB"
                      "CGBCBBKBKCBBCBBCBBCBBCBBCBBCCB"
                      "GBCBKKKKKBKCOYOROOBKCBBGBCCBBC"
                      "BCBBBBBCBBWBBWBBCBBCBBCBCCBBCB"
                      "GBBBBBCBBCGKOYOOBBBBBCBBCBBCKB"
                      "BGCGCCGCCBBCKBCBBCBBCBBCBBCBBC"
                      "BCBBCBBCBBKKKKOOYOKGBCCBGCBBCB"
                      "CKBCBBCBBCBBBKBCBBCBBCBBBBBWKB"
                      "BBWBBCBBGBBBBOROOCBBCGBCBBCCBC"
                      "BCBBCBBCBBCBBCBCCGBCKGCBKCBBWB"
                      "CBBCBBCBBCBBCOOCKBCBBCCBBBCCBB"
                      "BBCBBCBBCBBCBBCBCCBBCBKCBBCBGW"))
   (make-level :name "Temeraire" :difficulty 0.3 :slots 3 :seed 7 :par 523
               :order "22222200022222222222222122222002000222111001110111010111200000000000000000111111111111111110"
               :rows '("BKBCBBBBBBCCBCGYYYYYYYYTYYYYYY"
                      "BBBKTBBBTBCCTCBYYYYYTYYYYYYYYY"
                      "BCBBTBCBOBCCTCWYYYYYYTYYWYYYYY"
                      "BBWWWWWCCWWWWWWTYYYYYYYYYYYYYW"
                      "BBBBWBBGTBCCTCBYYYYYYYYYYYYYYY"
                      "BBBBTBBBTBCCTCCYYYYYYYYYOYYYYY"
                      "CCYWWWWTYWWWYWWYYYYYTYYYYYYYYY"
                      "CCGCTCWCTCBWWCCYKNNYYYOYYYYYYY"
                      "CCCGTCBCTCCCTCCOTNNOOOYOOROOOO"
                      "CCWWWYWWWWWWWWWONNNOOOOOOOOOOO"
                      "CCCCTGGCTCWCTCCYYKOOOOOOOOYORN"
                      "CCCCTCCCTCCCTCCOYKOOOORROOOOOY"
                      "CCCCTCCCTCCCTCCONKOOOORRROOOOO"
                      "CCCWWWWWWWWYYWWWRGRRNRORRNRNPR"
                      "CCWWWWWWYWWWWWWWPKRRRRRRRRRPRR"
                      "CCWTTTTTTTTTTYTKKKBKOYOYYOOYOO"
                      "CCWWWWWWCWWWTWWKNKKKYOOYYOYOOY"
                      "CCCCBCCCCBCCCCOKKKKKOOTYOYOOYO"
                      "BCWCCCWCCWWCCGKYOOKOOYOOYOOYOO"
                      "CCGCCYBCCWCGBWWOOKOOYYOYNOYOOY"
                      "CCBCWCCCWCBCWCNOKOOYORYOOYOOTN"
                      "CCBWCCCWCCCWCCOKOOYKOYOOYOOYOO"
                      "CCWCCCWCCCWWBBKOOYKOYOOYOOOYOY"
                      "CBCCCWWWCWCCCWROYNRYOOYOOYOOYO"
                      "CCWCWCCCWCCGWCOYKOYOOYOOYOOYOO"
                      "CCCWCCCWWCCWCCYKOYOKYOOTOOOOOY"))
   (make-level :name "Wanderer" :difficulty 0.9 :slots 3 :seed 11 :par 469
               :order "21122212222222220000000020222000000222222222222221111111111111111111111111110100000000000000001"
               :rows '("WWWWCCWCCBWCWCCWCCCCGCBCCWWWCC"
                      "CCCCCCCWWCWWCCCWCCCCWCWWCCCCCC"
                      "CCCBWWWWCCGCCCCCWWWTCCCCCBCCWW"
                      "YWWWCCCCGWCCWWOONWCCCCCCWWWWCW"
                      "CCCCCCCCWWTWCCNNNCCCWWWWGCCCCC"
                      "CCCCWWWWCCCCBCNNNYWWCCCCCCCCCW"
                      "WWWWCCCCCBCCWKKNKCWCCCCCWWWWCC"
                      "CCWBBBGCWWCWCNKKKCCCWWWWCCCCCC"
                      "CBKBBBGBCBCGCKKBKNWWCCCCCBCCWW"
                      "BBBBBKBBGWTWWKKKKNCCWWWBKBWCCC"
                      "WCCCWWYWWWWYWBKNKWNWTBBBBBCCCW"
                      "CWTWTYWWBBGWWKKKKWKBBBCBBBCBGC"
                      "WWWWWYGBBBBBBGKKKWNWWWWCCBBBWW"
                      "YWWCCBBBBBBBBKKKKWOCCCBBBCBBBC"
                      "WWWGCCCCGBCWWKKKKGCNCCGBBCBBBG"
                      "CCCCCWBCCCTWCKKKKCCNCCBWWCGCCC"
                      "CBCCCCCWCWCCGNNKKKCCCWCCCWCCWC"
                      "CCCCCCWWWCCBKBKKGKKWWWCCGCCCGC"
                      "CCWWWTWTWCCKTKKKKOKKYCCCCCCWWW"
                      "WWWWWWTWBCNNKKKKNKKKNCGCWWYTWW"
                      "WWWWWWWBWKNKKKNNKKKKNGWCWWWWWW"
                      "WWWWCCCWBNKKKKNKKKKNKBKWWWWWWW"
                      "YCCCCCWKNKBKNNKKKKTKBGKNWWCCCC"
                      "CBCWCCBNKKKKNKKNKNKKKKNKKCGCBC"
                      "CCGCWKNKKBKNNKKKNBKKKKKKKKCCCB"
                      "BCWKKNNKKGOKKKNNKKKKNKNKKNKKCB"))
   (make-level :name "Night Watch" :difficulty 0.15 :slots 3 :seed 29 :par 582
               :order "22222222222222112222212111100011111111111110111111111112000000000000000000000000000222222222"
               :rows '("NKKKGNKNKBNKNKNKKKKNKNKOKKKKNN"
                      "NKNKKNKNKNNKBBBGBKGKKNKNKKKKNK"
                      "KKNKNNKKKNNKYYYYBKNKKKKNGNKKNG"
                      "GKKKNNNKKKNBYYYYBKNKNKKNKNKNNK"
                      "KNKKNKNKNNKBTYYWBKKKTNOKKKKKKN"
                      "KNKNYNKKTNNBWYYYKNKKKNNKNKKNGN"
                      "KKKTNNYKNNNBBBBTBNKNKGNKNKNTKN"
                      "NKKKNNKNKNKKNKKNKGYWYOYKKKNNNN"
                      "NKKKNKGNKNKKKKNTNGYYYYYTGKKNNN"
                      "KTNKNKBNKNKNKGTTTKNKTTKNKNKNKN"
                      "GKKKNKNKKNTNKBKOKKNTTTNKKNKNKK"
                      "KNKNKKNTNKNGKKKKGKYTTYNKKKNNOT"
                      "KTNNKKTTTGTKBRGKKNOYWYNKTTNKNN"
                      "GKNNKNYTYKNKNKRKKNYYTYKKNKNKNK"
                      "NKKNKNYWYKKKNKRGBGYYTYGKKKNKNK"
                      "NKNKKYYYYYNBKKKPKKYYWOKNKKKNNK"
                      "KKNKNBYCWNBNKKKKRKYYTYKNKNKKKK"
                      "KKKKNKYOYKGOKKKKRKYYTYKKKNGNKK"
                      "KNKKNKNYNKKKKKKKGRTYWYNKKKKNKO"
                      "KNKNNNKKOKNKKKNKKNYYTYNKNKKNKN"
                      "KKKNGKGKKKTKNKKKKNYYTYBGNKNKKK"
                      "NGKKKNKNKKKBNKKKBKYYWYKKKKNKNK"
                      "KNNKNTOOONTNNTNNNNTNNNNKNNNNNN"
                      "NNKNNNNNNNNNNNTNTONNNNNNNNNNNT"
                      "ONNNNNNNNKNNNNTNOONNNNNNTNNNNN"
                      "NNNNNNNNNNNNNNNNNNNNNNNNNNONNN"))
   (make-level :name "Earthly Delights" :difficulty 0.9 :slots 3 :seed 56 :par 571
               :order "02222222222022200100022222111122222222221221111111111111111111111110000000000000000000000001"
               :rows '("CCCGGGCWBCGBCCGCCBCBCCCGCCCBCC"
                      "CCCPPPPCWCCCCCWPPPCCGCCCPPPPCC"
                      "CCGPPPPCCCCCCCPPPPCCCCCWPPPPCC"
                      "GGGPPPPCGGGNGGPPPPGGGGGGPPPOYG"
                      "GGGYWWGGGCGGGYGWPGGGGGGGGWPCGG"
                      "GGGGPPGGGGGGGGGPPGGGGGGGGRPGGG"
                      "BBBBPOKBBBBBBBBPPBBBCBBBBPPBBB"
                      "GBBBPPBBBBBBBBGPPBBBBBBBKPPBCB"
                      "BBBBPPGCBBBBBBBPPBBBBBBBBPPBBB"
                      "YGGGWGGYGGGGCGGGGGGGGGGGTGNGGG"
                      "GGGYGCTGGGGGTGGTGTNTGGGTYROOGG"
                      "GGGNGWGGGNGGGGGGGGWGGWTGNGRTGG"
                      "TGGGGGGNYNTTCWCWGCTGGGGNGGGGGG"
                      "CCGRRPGNYGCGCCCBBBCCCGYGGGWNGW"
                      "GNGTRGWYTCCCYCCCCCCCCGTGTGGGGG"
                      "GNGNGYGGGGWPROTCWCCGCGGCNTGGGG"
                      "WGNGTGGGCGGGOCCCCCCCOGYWGGGGGG"
                      "GGGYNGRRRRRGGGGTGGGGNRRCGBBBGG"
                      "GGGGTGGNGRGGNTGNYGNGGRGGCGBGGG"
                      "GGGGGGGYGGTGGGTGGGGCGGGGGGGGGN"
                      "WGGWYTGGGOGGGCGGGGGOGWGTTGGCGG"
                      "CTGYTGGCGGGCWGGGGTGRRRGTGGGGGC"
                      "YTYGGGGGYGGGGGNGGGGYRGGYGGGGNG"
                      "GGGGGCGGGWGGYGGGGGGGGGGNGGGYYG"
                      "GTGTGGGGGTGGGGWGYGWRRRGGGYGYGG"
                      "TGCGGGGGCGTTGGGTGGGGRGGGGGGCGG"))
   (make-level :name "Vertumnus" :difficulty 0 :slots 3 :seed 43 :par 453
               :order "0200222112200002221222211111212211111111100000000000000000000011111111222220000222222222111112"
               :rows '("KKGNKKKKKKKKKNKKKKKKKNKKNKKKKK"
                      "KNKGKKKKKKKKGGKBKKKKKKKKKKKKKK"
                      "KBKKKKKKKKKKBWBBYBBKBKKNKKKKKK"
                      "GNKKKBKKBKBGYBBOBBYBGYKKKBKKKN"
                      "KGGKBKKKKBBYBBYBBYBKYBKKKKKKKK"
                      "KKKKKKKKKBYBBYBBYBBYBBKKGKKKKK"
                      "KKKKKKKKGGGGPWWROGGGGGGGKKBKKK"
                      "GKKNKNKGGGGBGPWRRNGRGGNGNNKKKG"
                      "KKKKKKGYGGKBBCCORORRRGGGKKKKKK"
                      "KBKKKKYYYGGKPBYYONRNGGGGKKKKKK"
                      "KKKBKKKYGGGGGGOYGGYGGGGCKKKKKK"
                      "KKKKKKKGGGGRGGYYGYORGGGGKNKKKN"
                      "KKKKNKGGGNNRRYYYNRRRRGGYCKKNKK"
                      "KGKKKNKGGNRRRRYYWRRRRRYYYNKKKK"
                      "KKKKKKKGGORORGWYWTRRRBCYKKKKKK"
                      "KKKKKBKGGGRRPPGGWGGRCGGGKKGKKK"
                      "KKKKKKKGGGGNPOWGGGGGGGGBKKKKKK"
                      "KKKNKKKGGWGGORRORRGYGGBBKBKKKK"
                      "BKKKNKKKWWWGGYYYGYWGGGGBKKKKKK"
                      "BKKGKBKKKWGNGNYGCWWYGWKNNKKKKK"
                      "KKKWKKKKBYKGGGCGGGWGYYYKKBKNKK"
                      "KGKTNOOOONOONOOWOONOONRONROKKB"
                      "KKKNOONONNOONWONOONOONOONOOKKK"
                      "KKGNOONOWNOONOONOOTOONOONOOKKK"
                      "KKGNNONNONONNNYNOONOOYOONROKKG"
                      "KKKNOOOOOOOONORNCOKOOOOONOOKKK"))
   (make-level :name "Adele" :difficulty 0 :slots 3 :seed 16 :par 397
               :order "1020201111111112111001111100002000022220000000000000001001111111111222222222222222222222"
               :rows '("YGNNGYGGGGYGGGGOGGGGYGRGGYGGGG"
                      "GNGGWGYGGTNGGGYYGGGYYGNNYCGGCY"
                      "YTYYYYYYYYYYTKKKYYYYOYOYNGGCYG"
                      "YYYYYYYYYYYYKKKNKYOOYYOCGGGYGG"
                      "WYYYYYYYYYYNKYTTKKYYYYNGGGYNGG"
                      "YYYTYYOYTYTKTWTTTKYYTOOGYOYGGG"
                      "OYYWYYYYWYYNWGTKTKYYYYOGYGGYGY"
                      "YYYYYYOYYYYYTTTOTYYYYYOWGGGGYG"
                      "YYYYYYTWYYYYYTRYYYYYYYNNGGGYCG"
                      "WBYWKYYWKWWKKOKWWKWYYYRCGGYNGG"
                      "YYOWKWWKBWYYWKWTTYYWYYOGYYCGGG"
                      "YYTYWKWTWKWKTBCTTOKYYYOGYGGGGW"
                      "WYYYYWYYYWKCRYYYYYYYYTOYGGGGYN"
                      "YOTYYOKTYYTYYYYYWKWYYYOGGGGYCG"
                      "YYYYWYYCKWYWWYWKWYWYYYRGGGYGGG"
                      "YTYYYYWKWYYYWNWYWYOYYWOWGYGGNC"
                      "YYYYYOYYYYYYYWKWYYYWYYNGYGGCGY"
                      "YYYYYOTYYYOGWYYYOYOYYYOYGGGGYG"
                      "YYYYTYYOYYYYWYYYYOYYRYOGGGGYGY"
                      "WKWOYYTYWTYYYOYYYYYYYYOGCGYGGG"
                      "YYYORYYYWGWYYYYYTYYYYYOGGYGGNG"
                      "YYWYYYOYYYYYYYYOYYYRYYOGYGGNGY"
                      "YYYWKWBTYYYYYYYYYYYYYYOYGGGGYG"
                      "YYWYYYNYYWWOYTTYYYYWBWOGGGGYGG"
                      "OYYYWKTYYYYBYYYYWTYYYYOGGGYYGG"
                      "YTYWYYYYYYOYOYYYYYYYOYOGGYGGGC"))
   (make-level :name "Moulin Galette" :difficulty 0.9 :slots 3 :seed 1 :par 570
               :order "1011110000000020000200111111111111111111111112222222220220022222222222222222101000000000000"
               :rows '("GGGGGGGGGGGGGYWGGGYGGYGGYWGGGG"
                      "GGGGGGGGGGCWGGGTGGOWGWGGGGNGCG"
                      "GGGGGCGGCGGGGGGGGGGGGGYGGGGGGG"
                      "GGYGCYGGCCGGGWGGGWGGWYGGGGGGGG"
                      "GGNCGGGGNGGCYYGGGGGWGGGGGGGNCG"
                      "GGWCGGGGGGGGGGGGGGGGGGNGGGGGGG"
                      "NYWYCGGGGYGGGCGGGGOYYGGGGGGGGC"
                      "GGYGGYGGNGGGGGCGGGWTGGGGGGGGGG"
                      "BBTBBBCBBBBGBBBGBCBYBBBBCBBBBB"
                      "NBBBBCBGBBBCBWBBBKWWCBBKBKBKKK"
                      "BBBBBBBBBBBBCCBBBBOPBBBYKBYYYB"
                      "BBBKYYYTYYYYBCKBBBWPBBBYBBBTTB"
                      "WKBBBOYBTBWBBBYBBBPPBBBBGBBWRC"
                      "PBBBBTCCTBWBBKNKBBWOBBBBBBWPPB"
                      "WBBYWWBYOKNKWWTBBBPWBYYBBBPCPB"
                      "PBBCPWBPPBTTBBOBBBWPBBBBBBWPCB"
                      "WBGBWPBWPBTTBKBYBBWPBCBBBBPWPG"
                      "PBGBPPBPPBBBWKKBBBWPBBYBBGWPPC"
                      "WBBBWPYWPBBBBKKBBBPPBBBBBBPWPB"
                      "PWBBWPBPPBBBBKKBBBWPBWBBBYWYRC"
                      "WBBBWPBWPBWBBKKKBBPRBCBYBBPWCB"
                      "RGBBPPBPPCKBBKCBBBWPBGBBBBWPWB"
                      "WBCBWCBTPWBBBKKBCBPPBBBBGBPWPB"
                      "PBBBWPBYPBBBYKKKYBWPBGBBCBWPWK"
                      "WKBGWWBTPBBYBNGBWBPPBGBBWBPWPB"
                      "PBWBPBOPPBBBBNKWBBWPBBCCCBWWPB"))
   (make-level :name "Red Fuji" :difficulty 0.9 :slots 3 :seed 1 :par 485
               :order "00000000001000020200000000000011111111111111111111111200222201111111222222222222222222202222"
               :rows '("WWCCCCYWCWCGWWCCCCWWBBCCWWBCCC"
                      "CGCCCCWGCCCCCCBCWCCGGBCWCBCWCG"
                      "GGCWWCCGCWTCCCBTYCBBCYWCGCCWCC"
                      "GBGCWCCCBCGCGCCCCWCCCBCWCBGCWW"
                      "WWCCBCWWCGCCWWCGCYWTCCCCWWCCCC"
                      "CCGWCCCCCCCGGBGGCRCCGBCCCCWCWC"
                      "CBCWWWCCBWCWBCCWRNWCCWYGWCCCWW"
                      "BCCBCCCCCWGWCBWRRWRNCGCCBCCCCC"
                      "WWGCCCCWCCCCWWRRTRNCOCGCWCCCCC"
                      "CCCCCCCCCGCCCRRWORWRNCCBCCCCCC"
                      "GCCWWWWCCWWWNROONNRNNRRCCCCYWC"
                      "BCCBCCCGCCGRROROKORRRRNNBCCCCC"
                      "WWBBBBYWBBOORNONRRRRNRRNOTBBBB"
                      "CBGBBBBGBRKRRRRRRNRRRRPRORBKBB"
                      "BBBBBBBBRRNOPRNRPPRRRNRRRNRBBB"
                      "BBBBBBBPRRNORRORRRNPRNRORNRNGB"
                      "CBGBCBPRNRNRRRRORORRRRTRRRRROB"
                      "BGCKBNNORORRNPRORRRNRRRRORNRRR"
                      "BBBKORRRRNPNRRRRNRNRRNRNRRNRRR"
                      "GGKCGNGKNGGNKCGYYKGGGGGGGGYKNG"
                      "GGGGGKGGGGBGGGGKGGGGKGGGGKGGGN"
                      "NGYKGYGGKYGGCKYCYGKGGGGKGGGGKG"
                      "GKGGYGNGGGGNGYGGKCGYGGNGNNKYGC"
                      "GGGGGGGGGKGCNGKGGGGKGGYGBGGGCB"
                      "GGNGNNNKGCGGNNCGGKGGYGGGCCGNYG"
                      "KGGGYKGGGGBGGGNKYGGCBGGGNKGGGN"))
   (make-level :name "Liberty" :difficulty 0.3 :slots 3 :seed 3 :par 454
               :order "2022222222002200000000000000000000000000001111111112111111111111111111111122222222222222222220"
               :rows '("WWTWWTTTWTYTWTTWTTTWTWWWTTWTTT"
                      "TTWWTTWYTTWTYKBWWRRTWWTTWWTTCT"
                      "TWTYWTOTWTTTWNBWWRRWTTTWTTWWTT"
                      "TOTWOTWWYTWTTNBWWPRTTWTTTWTOWW"
                      "TWTTTWTTWWTTWNTTYTWWWTTWOTTWTT"
                      "WTTWTOTWWTWWTNWTTTWYTWWTOWTTTW"
                      "YWWTTWTTWWTTWNWTWTTTWTTCWOOWTT"
                      "OTCCNGCOTTTOTNOOTTOTTYKKKOOOTO"
                      "TOBCTKKKKOWTTRWYOOTTOTKKKTTOOT"
                      "OTCCOKKNKTTOTYTTTTOOWTOTTTOTTO"
                      "TTGCTCCTTYRTTNTYTOTTOOTTTTYTOT"
                      "OOGCOCTTTTTOONYYYTTOTTKTKTOTYT"
                      "TTORTKKTKTOTTNYYYOTTTOKKKOTTOT"
                      "TNTTOKKKKTTTYTYYYTTOTWNKKTOOTW"
                      "NNNNNKGKKNNOOYYTYYNNNNKKKNKNNN"
                      "NNNNNKKKKNNKNOOTYYNNNNKKKNNNNN"
                      "NNNNTKKKKNONNYYYYYNONNKKKKNNNN"
                      "NNKNNBKKKNKNYYYYYYYNNNGKKNNNNN"
                      "NNNNNKKKKNNNTYYYYWYNNNBKKNONNN"
                      "NNNNNKKKKNTTOYYYYYONONGKKNKNKN"
                      "ONKONKKKKNNYTYYYYYYWNNKKKNOKNN"
                      "NNNNNNKNNNNYYYYOYYYYNKNONONONO"
                      "NNWWWNWCWTWWTNCWWKWWWNWWWTWWCK"
                      "TNWTWNWCYKWWWNWWWNWWCNWWWNWWWN"
                      "KNNONNNNNNNNNNNNNNNTKNNNNNNNNN"
                      "TNNTNKNNKNNNNNNOOTNNNNNNKNNNNN"))
   (make-level :name "American Gothic" :difficulty 0.9 :slots 3 :seed 1 :par 483
               :order "22222222222222222222222222221121111111111112111111100011111110000000000000010000010001000000"
               :rows '("CCCCCCBCCBCBCCWWWCCCCCCCCCCCCC"
                      "GCCGCCGCCCCCCCWKWCCCCWCCCCCBCC"
                      "CCGGGCCCCCCCWWWKYWWCCCCCBCCCCC"
                      "CGGGCCCCCWWWTWKKKWWWYCCWCCGCCC"
                      "CGGCGYCWWWWWWKNNKKWCTWWCGGGYCC"
                      "YGGGCCGCCWWWWWKKKWCWWCCGGYGGNC"
                      "CGGGGGCCCTWWWWBNKWWWWCCCGGGGGC"
                      "CGGGYGCBCWWWWWWWNNNYYCGGGGYGGC"
                      "RRRRRCCNONNNWWWWNNTWYWWWGGGGGC"
                      "RRRRRCCCTOTWWYWWNNKYTTTCGGGGYC"
                      "RRRRRCCWTTTWWWWWTTNWOTTCCGGGCC"
                      "RORRRCCTOTYTTWWWTCWTTWTTWCGWCC"
                      "RORRRCCCTTTWWTWWWNWWTTTCCCCCCC"
                      "GGGGGGCGTTTWWWWWWNWWTWTGGGGCCG"
                      "GGGGCNNOWWWNNOGGGTKKCWWKKKGGNG"
                      "GGGGGNNNWOWNNNGGGNKKWKWKKKGGGC"
                      "GGGGCNNNWWWNNOGGGNBKCTYKKKGGGN"
                      "GGGGGNNNWOWNNTGGGNKKYKTKKNGNGG"
                      "GGGGGNNOWWWNOKGGGNKKWTWNKKGGGG"
                      "YGGGGNONWOWONNGGGKKKYKWKKBGGGG"
                      "GGGGGNKNYWWNNNGGGKKKWWWKKKGGGG"
                      "GCGGGNNNWOWNNNGGYNKKWKWKKKGGGC"
                      "GGGGYNNNWWWNNNCGGNBGWWWKKKGGGG"
                      "GGGGGNNNWOWKNNGGGNKKWKWKKKNGGG"
                      "CYGGGNNOWWCNNNGGGNKBWCWKKKYCGC"
                      "GGGGGNNNCOYNNNGGGTKBWGWKKKGGNG"))
   (make-level :name "Surprised!" :difficulty 0.3 :slots 3 :seed 3 :par 433
               :order "222222222220010000000222000000000000000000100011111111111111111111111111121221222222222222220"
               :rows '("YCCCCCWCCCCBCCWCCCCCCBTWCCCCCC"
                      "WCGCCCCCCCCCCCCCBCGCCWCCCCCCCC"
                      "CCCCCCCCCCGCCCCCCCCCTBCCCCCCCW"
                      "CNCCCCWCGCCCCCCCCCCWWCCCCCCCBC"
                      "CWKCCCBGGBCCCCCCCCCCWWGCCCCBCC"
                      "CGNWCWCCGYCCGCBCWCCWWCCBCCCCRC"
                      "CGOCWCCGCCGWGCCCCCCCBWCYCCCCGC"
                      "GGGNWGGGGGGGNGGGCGGBNGGGGWGGGG"
                      "GGGWGGGGGGWGNGGYGYGGNNGGWYGGCN"
                      "GGGWGGGGNGNGNGGGTWGTCGTYWGGGGC"
                      "GGWGGGGGOWYGGNCGYGNGGGYWGGGBGY"
                      "GGWNGGROOOOGGCCGYGNGGYCWGNGNGG"
                      "GGYGGGOYNWYGGCGYGGGGYGCGNGNGGW"
                      "GWYGGOONONOOCCOCGGGCYYCYGYNNYW"
                      "WOGGGGOGWKYKOKWBOGOKGKGGGGNGWG"
                      "GGGGGCOWRKOKOKWBNKONOKCGGNGGWG"
                      "GNGGGNWBOKOKOKOKONOKWGCGGGGGGN"
                      "GGGNYGWOBOKNKOKONOKOKOOGGGGWRG"
                      "NGGGYGKOKOKOKOKOKOKWKOGCGGGGGG"
                      "GGGGGCKGKOKOKOROKOKTNGGGYGGGGG"
                      "GGYGWGKGKOKOBOKYKOWOKOROOWOGNC"
                      "RGYGGYGYYGGGYYOBGGYCGOOOOWOGGN"
                      "YGGWGGGGYYGGYGGNGWCYGGGGWGGCNG"
                      "GGGGPGGGGGGYYGYGCCCGNGGNGGGGCG"
                      "GCGNGYGNCWGGYGGGWGGGGGGWGCGNCG"
                      "GGGGGGGGGWGTGGGCWGYGGNGGGNGGCG"))
   (make-level :name "Arnolfini" :difficulty 0.9 :slots 3 :seed 1 :par 547
               :order "00000000000000000000000000000110111111111111111111111111111112222222222222222222222222222220"
               :rows '("TYWYYTOTTTTTOYTYWYTTTTTTTTTWYY"
                      "TYTTWTYTKTTYTWTYYYTOTOTOTTWTOO"
                      "WWWWTBGKKKKGTYYTTYOTTYTTTOYTTT"
                      "WCCWKNKKKKKKKTTTTTTTYWTTWTTTTY"
                      "WCCWTKKKKKKKOOTOTTTYYWWTTTWTTT"
                      "YCCWTTTTTTTTTWTTTYTTWTWRPRRRRP"
                      "WCCWTWYTTTTTTTOTYTYOYTWRRORRRR"
                      "WCCWTTTTKTTTTWTTTTTYTWTRRRRRRR"
                      "WGCWTTTYNOTTYTNCNTTOTTTRORNNRR"
                      "WCWWTTYTKTYWTTCCCTTYYGBRRRNNPR"
                      "WWCWOTNNNNNTTTNCKTOTGCTONRRPRR"
                      "WWWWYTNNNKNTTTTTTTYTCCTORNRNRN"
                      "WWTTTTNNNNNTTTOYTTTTCBGRRRORRP"
                      "TTOTWWNNNNTTWWTTYTTTGCCRRNORRO"
                      "TTWTTTKNNNNTWTWTTTTTYGNORRRRRO"
                      "TTTWTTNNNTNTTOOTYTTGGGBGRRRROR"
                      "TTOOTWNNNKNTTTTTOTTGGGGYRRRRRR"
                      "OYTTTTKONTTYTTTTTYCGYGGGGORRPR"
                      "TOTWYGTNNNNBTTTTYTGCGGCGGRRPRR"
                      "OOTTOKKNNNNKOTTWTTGGGGGYGORORR"
                      "NONNOBOKNNKKNONNOGGCGGCGYCTNON"
                      "RNNRKKKNNNNKKNNONGGGGGKGGCORNN"
                      "NTNOGKNNONNKKTNNNGNGGCGCNYOTNN"
                      "TNNOGKNKONNKONNNNGYGCNCCGYNTOK"
                      "ONOKKKKKNKNKKNNNGGGGGCGGNNGONN"
                      "NNYNNOKKRNNOKNNYGNGGGGCGGNGCNR"))
   (make-level :name "Tower of Babel" :difficulty 0.9 :slots 3 :seed 21 :par 606
               :order "000221221220000021111121111111112111111111111111222220000222222222222222000000000000000000220"
               :rows '("WWCCCCCCWWWCCBCCCCWTCCCCCCWWWC"
                      "CBCCCCWWCGCNNNOWYGBCCCBCWWCCCC"
                      "CCBWWCBCWTTNNTNYOTCCCWYCCCCBGB"
                      "WTCCCCCCCKTTKOTKTTWWCCCCGGCYWC"
                      "CCWCWCWWCTTTTTYYYTCCCCCCWWCCCC"
                      "CCCWWCCBPRRRRRRRRORCBWWCCCCCWW"
                      "WTCCCCCCKRRNRRKPRKRWCBCCCCBWWC"
                      "CCCCCGWCRRRRPRRRRRRBCCCCWYCCCC"
                      "CBCWWCTTTYWTTTTTTTOTTWWCCCCCCC"
                      "CWCCCCTBTWKTTKTTKTTKTCCBCCCWWW"
                      "CCCCCCTTTWTTTTTWTTWTTCBCCCCCCC"
                      "CCCBCRRNRRORRRRRRRRRRRGCCCCCCC"
                      "CCCCCRKRRBRRKRRKRRKORKCCCGCCCC"
                      "CCCCGRRRRRRRONRRORRRRRCCCCCCCC"
                      "GGGTTTTTTTTTTTTTTYTTTTYTGGNGGG"
                      "GGGTTKTTKTTKTTKTTKTTKTTKGGGGGG"
                      "GGNYTTTTTTTTTTTTTOTTTTTTGGGGGG"
                      "GRRRRRRRPORRRRRRRRRRRRRORRNCGG"
                      "GKRRKRRKRRKRRKRRNROKRRGRNGGGGC"
                      "YRRRPRRORPRRRRONRPRPORTRPNYGNG"
                      "TOWTTTTTTYTTTTTTTTTTTTNTTNTGTC"
                      "KTTKTTKTTKTTKTTKTWKTOKNTKNTGNG"
                      "OTTTTTTTTTRPWYWTTOTTTBNBBNGBNB"
                      "GGRRWGGGGGRRGGGGGGRPWKBKKBKKBK"
                      "GGRPGGRRWGGGYGGRNWRNYBBBGKBBBB"
                      "GGGGGYRRGGGGGGGRRGGGGBBBBBBBBB"))
   (make-level :name "Milkmaid" :difficulty 0.5 :slots 3 :seed 38 :par 526
               :order "22222222211122222122222121222222222200111111100000011111111111110000001111100000000000000000"
               :rows '("CCKCCCWCWWTWWWWTYCWWTWWWWTWTWW"
                      "CCKCCCWWTTWWWWTWWWWKWWWYTWWYWT"
                      "WGKCCCYWYWTWWTWWWWWYWWWTWWWWTW"
                      "CCKCCCWTTWWWTWTWWTWWYWTTWWWTWW"
                      "KKKKNKTWWWWTWWWOWTWWCTWTWWTWWW"
                      "CCKCCWWWYYTWWWTTTWWYTYWWCTWWWC"
                      "WCKCCCWWWTWWWWTTWWWTWWWWTWWWWT"
                      "CCKCCCCWWWWWYWYYOYYWWWCTWWWWTW"
                      "CCKCCCWTWYGGYYYYYYYGGWTWWWWYWW"
                      "CCKCCWTWWWGGYYYYYYYGGTWTCWWWTW"
                      "TWWWWYYWWCTWYYYWYYYWTKWWWTWYWT"
                      "WWWWTWWWTKCWYYYOYYWTWWWWYWWYWT"
                      "WWWTWWWONNTWYYYYYYYWWWWTWWWWTW"
                      "WWOWCWONNNWWYYYTYYYWWWTWWWWTWC"
                      "WONOWOTOWCRRBBBBBBBRROWWYWTWWT"
                      "GGOCYNOGGWRRBBBBBBBRRRWCWTWWTW"
                      "GBBBBOBBBGRRBBBBBBBRRRTYTWYCWO"
                      "GBBBBBBBBGORBBBGBBBRRRKTYWWWTC"
                      "GBGBBGBBBGROBBBBBBBRRRTWWWKTTW"
                      "GGBBKKBBBCRRBBKGBBBRRRWWWWYWWW"
                      "GCCBBBBBKGRRBKBBKBBRPRWWWTWWWT"
                      "GGGGGGGGGNRRBBBBBBBRRRWWTWWWWT"
                      "WWWTWWWWTTRRBBBBCBBRNRWTCWTCTW"
                      "WWTWWWWTWWRPBBBBBBBRRROWWYWWWW"
                      "OTTTTWTTTTTTTTTTWTTWTTTTTTTTTT"
                      "TTOTTYTTTTTTWTTTTTTWTTTTTYTTTT"))
   (make-level :name "Sudden Shower" :difficulty 0.9 :slots 3 :seed 4 :par 495
               :order "111111111111111111111111111111112222222222222222222222222220200200000000000000200000220000000000"
               :rows '("KKKKKKKKKKKKKKKKKKKKKKKKKKBKKK"
                      "KKKKKBKKKKKKKKGKGKKKKKKKKKKKBK"
                      "KKKKKKKGKBKKKKKKBKKKKKKKKKKKKG"
                      "KKGKKKKKKKKKKKKKKNKKKKKKKGKKBB"
                      "WWCKCCCCKWCWCWCGCWKCCCCGWCCCKW"
                      "KCCCCKCCCCCCCCGKCCCCGCCCCKCCCC"
                      "CCKCCCCKWCCCCGCCCWCCCCKGCCCCCC"
                      "CCCWCCCCCKCGCCKBWCCNCCCCCCCCCK"
                      "KKKBKGKKKKKKKKKKKKKKKKKKKKKKKK"
                      "KKKKYYYYBKKKKKKGKKKKKKKKKKKKKK"
                      "GGBGGKKYYYKYGGBKGBGGKGGBGGKGGB"
                      "GBKGBBKBGKKYYYYGBGNKGYKGGBGKBG"
                      "KGGBWKKGNWGGYYWBGYKYYYYGBGYBGC"
                      "NWNNTNGGBKKBYYBCGBGGBWGYWYYGGB"
                      "NNNNNNNNNNNNYWGGBCGGCBKGGBGGBG"
                      "NYGBNWNNNNWNNONNNNBGWBGGBWGBGG"
                      "NGBGNBGWNGGCNNNNNWNNKNWOBCBGGB"
                      "NCGGNGYBNGBGNBGGNGNWNNNNKTNNNW"
                      "NCNBNYBGNCGWNGGBNGBGNWNGNNWNNN"
                      "GGBWCBGYWYGCNGBGKGWGNNYWNGBGTB"
                      "WKGGBCGBCGBGKBYWNGGBNGCGNWGGNG"
                      "BGWBNGKWGBGGWNGKCWBGNBWGNNGWNN"
                      "GGBNNTNNNNNTGGWGGGCWKGGBNGBGNW"
                      "GWGGBGGBGGBWGBNGWGGKGWBGGBWGBG"
                      "BGGWGNKGGBGGBYGBYGWGGBGGBGNGWG"
                      "CGBGYBGGBNWBGGBYGBGGWGGBGWBGGB"))
   (make-level :name "Flaming June" :difficulty 0.15 :slots 3 :seed 49 :par 456
               :order "2222222222220000000000000000000000000000001010111111111111111111111111211112222222222222222221"
               :rows '("CBBBBBBWKBBKBGCCKBBBBGGGGGGGGP"
                      "BBBCBGBBBBBBCBBBBBCWBBGGGGPGGP"
                      "BKBCCBBBBBBBBBGBBCBBBBGGGNGGGG"
                      "KCBBBBKBCBBBBBBCBBBBBCGGGGGPGG"
                      "BBGBBBCKBBBBBCBBBBBBCPPOCGGPGC"
                      "KGBBCBBBBBBCBBBBCBCBBBGCYPGCGO"
                      "BBYBBBBBBOBBBBBBYBCBBBGGGGGGGG"
                      "OBBBCKBYBBGKBKOBBGBBBYGGGGNGGG"
                      "BBBBCNNNBOOOROOOYOOYBBBBBBYGBB"
                      "WWWTNNNNNYOOOROOOPOOOYWTWWYTWY"
                      "WWTWWNNNTTROOOYYOOROOOPWWWTWWW"
                      "WTYWWOTTTOOROORROOOYRYORWTWWWT"
                      "TWWWTOOTROOOYOOORORNRNOOTWWWTW"
                      "WCWTTTTTOROOYROOOYOOOROOOWWTWC"
                      "WWTWTTTTOOYOOOROONPOOOYNWWWWWW"
                      "WTWWWOOROOOROOOYOOOROOORWOWWWT"
                      "TWWCTWOOYOORRONOPOYYYOOWTWWWTW"
                      "WWWOWTWOOOOOOYRYRNOOORWTYWWTWW"
                      "WWTWWWTWWOOOOOROROWOWWTWWWTWWW"
                      "WTWWWTWWWTWWWTOWWTTTWTWWWTWWWT"
                      "TWTTTTTTTTTWTTTTTYTTTTOTYTTTTW"
                      "WTTTTWTOTTYYTOTWTTTTWTTTTWTTTO"
                      "YTTTTWTTTTWTTTTWTTTTWTTTTTYTTT"
                      "WTTTOWTTTTWTOTTWYTTTWTTTOWTTTT"
                      "WTTTWWTTYTWYTTTWOTTTWTTTOWTTTT"
                      "WTTTTWTTTTWTTOTWTTTOTYYTYWTOTT"))
   (make-level :name "Ophelia" :difficulty 0.15 :slots 3 :seed 10 :par 405
               :order "22111111110000000020000000000001010111000222222220000002222222222222221212211111111111111121"
               :rows '("GGGGGGNGNGYGCGNGGGGGNGYTNGCYYG"
                      "YYGGGGNGNNGGGGGGGYGYGNGNYGGGCN"
                      "NGGGNGGGGGNGWGGGGGGYYGYGGGGYYG"
                      "GNGGGYGYGGGCGGNCCGGGGNNGYTGGGG"
                      "GYGGYGGNNCGNGNGGGGGGCGGGGNGKGG"
                      "GGGGGGCGYGGYGGGGGGGGGGGGGGGCGG"
                      "GNGGGGNGGNNGGGGGYGYGGGGNCGYYYN"
                      "KKBNKBKKBKKBKKBBKKKNBKKBKKBKGB"
                      "KBKKBKKBKKOKKBBKBNKBKKBKKBKKBK"
                      "BKKBGKBKKBKKBKKBKKBKKBBGBKKBKK"
                      "GKBKKBKWBBKBKNGNKBKKCKKBKKBKKB"
                      "NCKRBGKBBTBKKBKNBKKBTKBKNBKKBK"
                      "CGKBKNYNKBWCCGCCCWCCGCKKBKGBKK"
                      "GKBKKTTTCCCCCBCCPCCCCCWCKKBKKB"
                      "KBKKBTTTCWCCCWCCCWCWCCCWCBKKBK"
                      "BKWBKKBCBCCCGRWBCCBCCWBCBKKBKG"
                      "KRPKKBBKBYCGCBBCCBWCCKBBKKKBKB"
                      "KBKKBKKBGKBGKBKKBKNBKKBWKBBKBR"
                      "BKNBKKBKGBKKBKKBBRBNKBKNCKKBKK"
                      "KKBKKCKKBPKKKKCKKBKKBPKBKPBKKG"
                      "KGKKBKPBKKBKGBKKBKKBKKBKKBKKBK"
                      "KGKNNKBKYNKKBKYRNKBYKBKBBKKNYK"
                      "GGGGKGGBGGGNGGGGGKNNGYGGCGYGGN"
                      "TNGNGGGGBGGNGCGGGGGKNNRGGYGGNG"
                      "BGGGGGNGGGGGGNYGGRGGYGGGGGGNGG"
                      "CRGGBGGGGKGNNGRCRYGGRGGYNGCGGG"))
   (make-level :name "Almond Blossom" :difficulty 0.9 :slots 3 :seed 22 :par 618
               :order "1111211111111111111211111111111122210002222222222220222202020202200000000000000002222220000000001"
               :rows '("WWNKCCCCCCCCBCCCCBCCCWCGNKCCCC"
                      "WCWNNCCCCCCCCBCCCCCCCCCCKCGCCC"
                      "CGCCNKCCCBCCCBCCGCCCCCCNKCBCCC"
                      "CCCCWNKCCCCCCCCCCCCCBCNCNCCCCC"
                      "CBCGCCNKCCCCCCCCCGBCCCNKWCCCCC"
                      "CCWCBCNKCGCBCCBBCCCCCCKBCCCCCG"
                      "CCCCCCBNKCBCWCBGCCCCCNKCCCCBCW"
                      "CCCCCCBNKBCWCWGCCCCCNCKWPPCCWC"
                      "WBBCCCGCNKCCCCCCCCCBNWWYYGCCCC"
                      "CCCCCCCCNGCCCCBBCCYPKWNGCBCCCC"
                      "CCCCCCBBNNCBCCCCOKTCWWCNKKKCBC"
                      "CCCCCCCBCNKCCCKKKCCCCCBCBCNCPC"
                      "CGCBCCCCCNKWPYWNKCWWBWCCCCCWKK"
                      "GCBCCCWCCCNWKYCCWWYCCCCCCCGCCC"
                      "CWCCCCGCTWNKBBCCTNKKCWWBCCCBCB"
                      "CBCCCCCCCNKCCCCCCCCNKWCCCPCCCG"
                      "CCWCBCCWWKKCCCCCCCCCKKCCWCCCCB"
                      "BCGCCCCYPKCCWCWCCBCCBKKCCCCCBC"
                      "GWCCWCNWKKCCYWCCGCCCCCNKWCCCCC"
                      "CCTKKKKNKBCCWCCGCCCBCCGCCCBCCC"
                      "NKKWCBCNKCCCCCCCCCCCCCBCCCWCCC"
                      "WBCWCYPWPCCCCCCCCCBCCCCCCCCCCC"
                      "CCCCCWWWCCCCCCBCCCCCCCCGBCCCCG"
                      "CGCCCNKWGCCCCCCCCCBCCCCCCCCBCC"
                      "CCWPCNKCCCCCCCCCCCCCCBWBCCCCCC"
                      "WCWCNKCCCCWCCCCCCWCBCCWCCCCCGW"))
   (make-level :name "Wheatfield" :difficulty 0.9 :slots 3 :seed 16 :par 505
               :order "000000000000000001000000011111000011011111111111112211111112222222222212222222222222222"
               :rows '("GBBBWBBGCBBKCBCBCBBGCBBBGKBKCB"
                      "KKNNBBBCBBBCKKKKKBBCBBBCKKKKKC"
                      "KBCBBBWBBKGKNBBKKGCBBBCKNKCBBB"
                      "CCBBKCKBKBKBBCBBKCCBBBBBGCBBBC"
                      "WBBBWKKBBBBBGBBKGBBBCGBGCKBBCB"
                      "BBBCKBBCBGBKBCBCKCKCBBKCKCBCBB"
                      "KBKKKKKBCGKBGBNNKGKKBBCBBBGKKK"
                      "BKBBBWKBBCBBCCKBBCBNKBBBCGGKBC"
                      "CBCBCBBBGCCKNBKCCBBGKBBCCBBCCB"
                      "BBBBBGBCBGBWBKBCGKBBBKBCBGGCGB"
                      "BBGBBGCKBNWBKBCBBBCBKKKBBGCBBB"
                      "YOYOYYYKOKYWYOYYWYRYYYWOOWWYNY"
                      "YOTYTYYOKYYOYTTYOWYYWKONTYNYOY"
                      "YWYTOOYTOYYYWYOYTYYOYYKYOYYYWN"
                      "YYOYYYYOYYWWOYYTWOYYYTNYWWTYYW"
                      "YYYYYOYOYYOYYYYOOYYWOYWYYOYYYY"
                      "YYYOYTYWNYYYYOYYYTYYYYWNYOYYYY"
                      "CNGYYOOTYWYOWOYWOYTTYNYYYYYGGN"
                      "NGGNOYYWYOYYOYNYYYYOYYYYNYYCGG"
                      "TGCGNNYOOYYYOYYYYOYYYYOYYNCGGY"
                      "YNGYRRNTYYOYYTYOYYYYOTYNNONGGY"
                      "TTGGCYRPNTYYYOYOYYOYYONRRNGCNY"
                      "YOGGNYOYRRNNYYWYRYWTNRRTYGGYYO"
                      "OYYYGGYTTOORNNNYTNORRWYYCGGYYO"
                      "YYNGGGYYTYYYORNKNPRYYYOWGCGROY"
                      "OYYTYGGTYOOWYYPRRYYYOOYGGYYTYY"))
   (make-level :name "Las Meninas" :difficulty 0.9 :slots 3 :seed 23 :par 561
               :order "111110000000000000000000000000010010101111111111111111111112122122222222222222222222222222220"
               :rows '("NNNNKGNKKNKKTKNNKKNKKNKKNKKKKK"
                      "NNNNKNOOOKKNBKNKKNKKOONNBKNKKN"
                      "TTONNKYOOKNKKNKKNKKNOOOKKNKKTK"
                      "TTTTKNOOONKKNKKNKKTKOORKNKKNKB"
                      "TTTNKNNNOKNKKKNKKNKKOOONKKNKKN"
                      "TTONNKROOBNNKNYYYKKTOOOKBNKKNK"
                      "TTTNKKOOONKKNBYYYKNKOORKNKKNKK"
                      "TTONKNOOOKKNKKYTTNKKOOONKKNKKN"
                      "TTTNOKOOOKNKGNWKKKKNOYOKKNKGNK"
                      "TTTOKTOKBNKKNNYTYKNKGNKKNKKNKK"
                      "TTTNNKKNNNNNNNYYBNNNNNNNNNNNNN"
                      "TTTTNKKNNTTNNNTYTNTNTTNNNNNNNN"
                      "TYTNNKKKNTTONNNONNNNTYNNNNNNNN"
                      "TYTONKKNOTTNNNOCNNNNTTONNNNNNN"
                      "WTTNNKKNNCCNNCCCCCNNWPTNNTONNN"
                      "TTTNNKKKWGCCWGBCCCWPPPPNNNNNTN"
                      "TTWNOKKNCCCCWCCCCCCPPPPNNNNNNN"
                      "TTTNNGKKCCCCCWWCWWWPPPPNKNNNNN"
                      "TTTNNKKNCGCCWWYWCWWPPPPNNTNNNN"
                      "TWTOKNKCCCCWCCYWWWPWPPPPNNNNNN"
                      "TTTNNKKNCCCCWWWWWCWPOOPNORNRRN"
                      "TTTNNNKNCCCGWWWWWYWPPPPTRRORRN"
                      "TTYNNKKOCCCCWWWWWWTPPPPNOOOOYN"
                      "OTTNNKNTBCCBWWWYWWWPPPPYOOOONR"
                      "NNNKNKKONCCTNWTWWWNNPONNOOOOON"
                      "NNNNOKKNNTNNNNNCTNNNNNNNRROPRN"))
   (make-level :name "Creation of Adam" :difficulty 0.7 :slots 3 :seed 40 :par 551
               :order "2222222222222222220022220222111111111101100010111111112222211111111000000000000000000000001"
               :rows '("WWWCCCCBBGCCBCCWWWCCCCCCGCBCCC"
                      "CCCCGCCCBCCCTTWCBBCCGCCCCCCYYW"
                      "CCCCBCCCGWYWBCCGCCCCCCCCWWWCCC"
                      "CCCCWCWWWCCCCCCCCWGCCYWRCCCCCC"
                      "GCCTWWCBCCBCCCCCCCWWRRPPRRRCCC"
                      "WWWBCCCCCCCCCCCWWWRWCWPPPPRRRC"
                      "GCCCCCCCCCCCWWWCCRPWWWPRPTPPRO"
                      "CCCCCCCCCTWWCBCWCROWYWPPRPPPRR"
                      "CCCCBGWWWBCCCCCCCNPPPPPPPPPRPR"
                      "CCCWWWCCCCCWCCCCRRTPOPWOPPOPPO"
                      "WWWCCCBWCCCWCBGWTTPPPPRPTPPPPR"
                      "CBTNTCGCCCCCWTCTCRRPPPPRPPOTRR"
                      "CCNNNCCCCWTTTCCBCRRPPWTPPRPRRR"
                      "GCNNKTTTTTTCCCCCCCRRNPPPPPRRRC"
                      "GGOTWTTTTTTTTCCCCCWWRRRRRRRCGC"
                      "GTTTYTTTTTTTTTGTYWCCCCCRCCCCCC"
                      "GGTTOTYTTTTTTWCGCWCCCCCCCCCWWC"
                      "GGGYTTTTYOTWCCCCCCCCCCCCWWWCCC"
                      "GCGCNGWWWCCGCCCWCCCCCWWWCCCCCC"
                      "GGGGGGGGCCCCCCCCCCWWWCGCCCCCCC"
                      "GGGGGGGGGCCCCCCWWWCCCCCWBCWCCC"
                      "GGGGGGGYGGCGWWWCCCCCCCCCCCCWCW"
                      "CGGGGYGYGGGWCBCCCCCCBCGWWWWCCC"
                      "GGGNGGGGGGGGCCCCCWCCGWWWBCCCCC"
                      "GGGGGNGGYGGGGCCCCCWWWCCCCWCCCC"
                      "GGGGGGGCGGGGGGCWWWGCCCCCBCCCCC"))
   (make-level :name "Last Supper" :difficulty 0.15 :slots 3 :seed 45 :par 532
               :order "0112000000010001001100000000000000000001111111112222222222222222222222121121111112222211111112"
               :rows '("TTTTTTTTTTYTTTTTTTTTTTTTTOTTYT"
                      "TTTTTTTTTTTTTTTTOTTTTTTTTTTTTT"
                      "TTTTTTYTTTTTTTWTTTTTTWTOTTTTTT"
                      "GNKNKNKKNTNCCCWWWCCNNNBTKNGKKN"
                      "NONKOKNNNNNCBCWWWWCNNNNGNONKOK"
                      "KNKTONNNNNNCGCWWCCCNONKNKOKNON"
                      "KONKOKNKNNNCCWTWCCCNNNOKNONKOK"
                      "NRNNONKNNTNCCCWWCCCNNNKNKOKNRN"
                      "NONNOGNKNNNCCCWWCGCNNNNBNONKON"
                      "KOGNONKNNNNCCWWWCCCNNNKNKOBNON"
                      "NNTKOBNKNNTTTTTTCTTYTNNKNOTTTT"
                      "KOKKONKNKNOYPPTTYBBGGTYNTTYYRR"
                      "NTTTTYTBYTOOPRRBBBBGGBBKYYYYRR"
                      "KGGGGPPNOYOOPORBBBBGNBBNTYYYOR"
                      "NGGGGPPKYYOOPRRBBBBGGBBKYYYYOR"
                      "KGGGGPPOYYONPRNBBBBGGBGNYYYYRR"
                      "KGGNGPPGYYOOPRRCBCBGGBBKYOYYRR"
                      "WRWWOCYYWWYWWOCWOWWOWCOWWOWWNW"
                      "CWWWWWWWWWWWWTWCWWTYWWTWWWWWCW"
                      "WWWWWWWCWWWWWCWWWWWWWWWWCTWWWW"
                      "NNONNNNNKNNNNNNTNONNKNNNNNNNNN"
                      "TNONNNONNTNKNNNNNNNNKNNNNNNNNN"
                      "KNKNKNKNKNKNGNKNKNKNKNKNKNKOGN"
                      "NTNNTNNNNNNNNNNNKNKNNNNNNNKNNN"
                      "TNKNONNTNNNNKNNONNNNNNNNNNNNTN"
                      "NNNONNNNNNNNNNTNNNNNNKNNTNNNNN"))
   (make-level :name "School of Athens" :difficulty 0.9 :slots 3 :seed 12 :par 527
               :order "11111111111111000111110011111122122221211122222200000002222202000002020022222220000000002021"
               :rows '("NOTOOONONONTWTTOTTWTNYTONONONO"
                      "ONONONONONTWTTTTTWTTTNONONOOOK"
                      "NONONOKOKTWBGCCCCCCYTONOKONOKO"
                      "ONONYNONWWTCCWWWWCCTTWONNNONON"
                      "OONONONWWYTCCCYWWCCTYTTTKNNONR"
                      "ONYWOOTWTTTCCWWWWCGWTTTTWNTWOK"
                      "OOWWNTWTTTTCWWWWWCGWTTTTWTWWNN"
                      "ONWWTTTTOTWCCWWWWCCYYTTWTWWWON"
                      "TTTWWTTTTTWCCWWWYCCTTTWYTTWWWT"
                      "TTWTTTTTTWTBCTTWTTCTWWTTTTWWTT"
                      "OTWWTTTTWTTCCRRTCBCTWTTTTTTCTT"
                      "TWWOWTOWTTYCCRRTBGCWTTOOTWWWTT"
                      "TTTYTTWTTWYTWRRBBBWYTTOTWTTWTT"
                      "WTTOWWTOTTTTTRRKBBTTTTOWTTTTTW"
                      "WTTTWTTOTTWTTRRGBBTTTYTTTTTYWT"
                      "WWWWWWTWWWWWWWWWWWWWWWCWWWWYWW"
                      "WWTWWWWWWWWWWWWTWWWWWWWWWYWWCW"
                      "WWTTTTTWTTTTTWWTTTTWTWTTTOTYTT"
                      "WWKTOTWTTTWYWTTOTTWTWYTTWWTTTW"
                      "CWKWWCWWYWWWWWYRWWWWWWWWWWWPWW"
                      "TTKTWYYTYTWTTTTRWTYTOTWTTWTPWT"
                      "WTTWTTOTTWTTTTOWYTTTTTTTTWTPTT"
                      "TTWTYTYTWTYTTOWTTBTTWBTTWKWYTT"
                      "WWTTTOYWTTTTTWTWTBTWTBYTPBPTTT"
                      "WTTWTTYTTTTTTTTTOGWTTBYTPBPTTO"
                      "TOTYTWTTTTTYTYTWYWWTTTYWPWPTTW"))
   (make-level :name "The Gleaners" :difficulty 0.7 :slots 3 :seed 11 :par 438
               :order "111111111111111110111111001101100011100000001000000000000002222222222222222222222222200002222221"
               :rows '("WWWWWWYWWWWWWWWWWWTCCWWWWWWWWW"
                      "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWW"
                      "WTWWWWWWWWWWWWWWWYWWWWWCWWWWWW"
                      "WWYWWWWWWCWWYWWTWWWWWWWWWWWWWT"
                      "WWCWWTYWWWWWWWWWWWWWWWYWWWWWWC"
                      "WWWTWCWWTWWWWWWTWWWWWWWWWYWWTW"
                      "YYWYYYWYYTYYYYYYYYYYYOYOYYYYYY"
                      "YYYYOYYYYYYYYYYYYYOYYNYYYYYYYO"
                      "YYYYOYYTYOYYYYTKOYYYNOOYYOYYYY"
                      "YYOOOOYYYOYYOYNNNNYYOOOYYOYYYY"
                      "YYYOYOYYOOOYYYNNNNYONOOOOOOYYO"
                      "WYNYOOOOOOOOYOOYYYWOOROOOONOYY"
                      "OTTTNTTTOTTTOTTTNWTTOYTTOTWTOT"
                      "TOWTTYTTTOTTOOYTTOTTTOOTTOTTTO"
                      "TTYTTTBTTOOTTTORTTOTYTOTNTOTTT"
                      "TTTOBBBBBTWOORRRRROOTTNNNNNOTT"
                      "OTTBBBBBBBTTRRRPRNRTONNNNNNKOT"
                      "TOTBBBBBBBTTRRRRRRRTTNNNNNNNTO"
                      "TTORBBKBBBOTBRRORRRTTYNNNNNNOT"
                      "TTYTTBBBBTTTTYPRRNTNTTTNTNNOTT"
                      "OTTTOBBCKTTTTTNRRRYTOTTNNONTOT"
                      "TOTYTBBBBOYTOORRRRTTTTONNNNTTO"
                      "TTOTTTNNTTOTTWOOKTOTTTTTNNOTTT"
                      "TTTOTTNNWTTOTTTNNTTOTTTRNOTOTT"
                      "OTTTTWNNRWTTOWTNTTOTNTTTOTYOOW"
                      "TOTTTOTTTOTTTRTYTOWTOOTTTOTTTO"))
   (make-level :name "The Swing" :difficulty 0.9 :slots 3 :seed 11 :par 561
               :order "0000000000000000000000000010010011111111112111111110111111111112222222222222222222222222222202"
               :rows '("YGGGGYGGGYGGCGNWCWWYYYGGNGKKKN"
                      "GGGCGGGGYGGGGGNYGCKCYWYWNNGKKK"
                      "GGGGGGGGGCNGCCGNCYWKOKCNWGGGKG"
                      "GGGGGGGKKKGYGGWNWKKKKYCNYYGYGG"
                      "GGGGGGBKKKKKGYWNCBKCCWYNCWCYGG"
                      "GGGGGGKKBKGWCGWNCGWWYGYNWYGCGG"
                      "GGGGGKKKKKKGCCYGNTKGCGNCCYRGGG"
                      "GGGGNKKKKBKWGGYYNWGWWWNGWWGGKK"
                      "KGGGGKKKKKKGGYYYNWGYGYNYWGGGKK"
                      "KCYGGBKKKKCYYGGGNNTTTCNYGYGKKB"
                      "KGGGGNKKKGGGGGGTWPPTWPGCGGKKKG"
                      "KGGCGGGGGGYGGWWGPPPPOPPGGGKKNK"
                      "GGGGGGGGGGGYGGWPPPPPPPPPGGKNKK"
                      "GGGWGGGGCGGGGGGYWPWPWPWGGGKKKN"
                      "GGWWWNNGGGGGNGGGKPOPWPGGGGGKKK"
                      "GGWWNGGGKKGGGGGGKKKOKKGGGGGGGN"
                      "GGWWGGGKBKKGGGGKKKKKKGGGGGGGGG"
                      "GGWYGGKKGKKKGGNGGKKKKGGGNGGGGG"
                      "GGWWGGKBBNKKGGGGNWKBGGPGGGGNGG"
                      "CGWWCKKKKKBGGGGGGGGGYGGYGGGGGW"
                      "KGWWPKKPPTWGGWNGGGGYGGGCGGRGKN"
                      "KGPPGKKKYTTGGRGGGGGGGCGGGGGKKK"
                      "BGWWGGKYYYYGGPGWYGGGCGGGNNGKKK"
                      "KGGYGGYYWYYGGRGGGGGKGGWNNGKBKK"
                      "KGGGGGGTYYOYGGGGGGKKKGYGGGKKKK"
                      "GCGWGGGNGGGGGGGGNKKKKKGGGGOPKB"))
   (make-level :name "View of Delft" :difficulty 0.9 :slots 3 :seed 9 :par 406
               :order "00011110011000202220022220000000011111111211111111000000000000011111111112222222222222222222222"
               :rows '("WWWYWWWCWWCCCCWWCWTWYWWWWTCBCC"
                      "WWWWWWWCCWCCCCWWWWWWWWWCCCCCWW"
                      "WCTCCCCCCCCCWWWWWWCBCCWCBCWCCC"
                      "WCCCCCCWCWCCCGCCCCCCWCCCCGCGCC"
                      "CBGCCCCCCCCWCCGCGCCCCCCCCBCCCC"
                      "CCGCGCCWWBCCCCCCCCCBCCCWCCCCGC"
                      "CCWWWKWWWCCCCCBCCTWNNCYWCCCCCC"
                      "WWWTWNNCCCGCCWWWCWCKNWWWKCCCCW"
                      "TWWWWNNBCCWWWCWWWWWNNCCCNNWWTW"
                      "TCCCCNNWRWRCWBBWWWRNNRWRNNWWRW"
                      "CCCRRNNCKWKWRBBRRRONNNCKNNRWNC"
                      "CRRNKNTCNRKCNBBOKNNNNNBNNNTCKR"
                      "RNNKNONRNNNRNBBKNNKNNGRONNNRNN"
                      "KNNNNKNNBNNKNGBNNKNNKNNKNNKNNB"
                      "NKNNKNNKNKKNNKNNBNNKNTKNNKNNKO"
                      "KNOKNNKNNKNNKNNKNNKNNKNNKNNKNN"
                      "CKBBBBBBKBBCBBGKBBBBBBKGBBCBBN"
                      "BCBBBKCBBBBBKBBBBKBKBCKBBBKBBC"
                      "BGKBBBBBCKBCBBBBKBBBBBBNBBBBBB"
                      "BBBBBBKBBBBBBKBBBBBBKKKKKKKKGB"
                      "BBCTBTBBBTKGBBBBBKGBNKKKGKBBBB"
                      "KGBTBTGKBTBGBBBBBBBBBKBCBBBBKB"
                      "YTTKTBTTTBTTTTTTTTTTTTTTTYTTTT"
                      "TTTKTBTTTBTTTTTTTWOTWTTTTTTYTT"
                      "TTTTTTTTTTTTWTTTTTTTTTTTTOTYOT"
                      "TTTTWTTTTOTTTTTTTTTTTTWTTTTTTT"))
   (make-level :name "Plum Garden" :difficulty 0.9 :slots 3 :seed 3 :par 615
               :order "2000002020011100000011000021221111121111111111000000000000010111111112222222222222222222222221"
               :rows '("RRRRRRRPRRNRRRRRRRKRRRRRRRRRRR"
                      "RRRRRRPRPRPRRRRRRPKRRORRRROORR"
                      "NRRRORRRNRORRORORKRRRRRRRRRRRR"
                      "RRRRRRROORRRRRRRRKRROORPRRRKKR"
                      "RRRRRRRRRRRRRORRRKRRRRRRRKKNNR"
                      "RRRRRRRRWWRRRRRRKRRRRWWKKNNORR"
                      "RRRRRRRORKKRORRRKRRRKKNWPRRRRO"
                      "TWRWYRWTPKKRWWRCWPKKNNTNWCRTWR"
                      "WRTWRYWRKKNWWPWKWPNNWWRYWRWWRW"
                      "RCWRWWRWKBNWRNGKWPRWWOWPOWWRWW"
                      "WWOWWRWKKNGBKNNWWTWWRWNWPYRNWR"
                      "WWWWWPWKKKTKTWWWWWWWWWWYWWWWTW"
                      "WNNWPYWONOCNNNWPNNNWWNNNWWWPNY"
                      "GNNNGGNNNGYNTNGGNONGGNNTYGNTNG"
                      "NNNONNKBNNKWPNNNNWPTNNNONNNNTW"
                      "GGGGGKKNGGGKGNCGGGGGKCGGGGWOGG"
                      "GGGGGGKNYCGBGNGGYGGGBGGGGGGGGC"
                      "GGGGGKNGGGGCGGGGGGGGGCGGGNGGGG"
                      "GGNGKNNGGGGGGYGGGGGNGGGGGGGGYG"
                      "GGWPPNGGGYGGGGGGYGGGYGGGGGGGGG"
                      "GGGKKNGGGCGGGGGGGGGYGGGYGGGGGG"
                      "GGGNKNGYGGGGCGGGGGGGGGGGGGYGGG"
                      "GGKNNYGGGNYGGGGGGGNGGGGGGCGGGG"
                      "GGKKNGGGGGGGGGGYGGGGGGGGGGGGNY"
                      "GBKKGGGGGGNGNGGGGGGGGNGGGGYGGC"
                      "GKKNGCGCGGGNGGGGGGGGGGGGGGGCGG"))
   (make-level :name "Bathers" :difficulty 0.9 :slots 3 :seed 22 :par 511
               :order "1111111211222122121111111112121111111111122222222222222222222220000000000000000000000000000000002"
               :rows '("WWWWWWWWWWWWWWWWWWWWWWWWWWCWWW"
                      "WWWWWWWWWWWWWWWWWTWWWWWWWWWWWW"
                      "WWWWWWCWWWWWWWWTWWWNWWNWTNWWWT"
                      "WCWWWWYWWWWWWWWWWWWNWWTWWNCWWW"
                      "YWWWWTWWWWWWYWWWWWCNWYNWWNWWWW"
                      "TWWWWWWWWWWYWWWWWWWNWWNTWNWWWW"
                      "BCCBCCBCCBCCBCGCGGGNNCGGGGGGGG"
                      "BCBCCBCWBCCBWCBWCBBCBCCBCCWCCB"
                      "CBCCBCGBCCBCWWCCBCCBCWBCCBWWBB"
                      "BGGBCCBGCBCBWWCBCCKCCBCCBCWWCW"
                      "CCBCCBCCGCCBWYBCGBGCBCCCGCWWCB"
                      "CBCCBCCYYCBCTWCCGCGBCWBCCGWWBC"
                      "YYGGTCBTWBCCBCCBCCBCCBCCBCCBCC"
                      "GGGYGGCYWCCBCCBCCBCCBCCBBCBCCB"
                      "GGYGGGYWWCTCCBCCBCWBCCBWWGWCBC"
                      "GYYYYYGTTTOTTTCBCCBCCBTWBCCBCC"
                      "YGGGYGOTTTTWTTTBCBCCBTTTCCBBCB"
                      "GGNYTGGTTTOTTTCCGBCBCOOOCBCCBC"
                      "GGYWWGYGGGTGGGGBCGBCCRYOBBCBCC"
                      "GYGWWYGGGYGGGYGCCKCCBCCBCCBCCB"
                      "YGNWWCYCYGGGYWNGYCCBCCBCCBCCBB"
                      "GGGYNGGYGGGYWWWYGGBCCBCCBCCBCC"
                      "NGYGGGYGGGYGGWYGGGYGBCCBCCBCCB"
                      "GWGGCYGGGYGNGYGGGYGGCCBCCBBCBC"
                      "YGGCYGGGYGYGOGGGYCGNYGGCBCBBCG"
                      "GCGWYGGYGGGYGYGYGCGYGGGTCCBCCG"))
   (make-level :name "Parasol" :difficulty 0.9 :slots 3 :seed 2 :par 504
               :order "111112111211112222222222222222222222202220202000000000000000000000000000211111111111111111110"
               :rows '("BCCWWWWWCCBCCCCBCCCCBCGCWWCCWC"
                      "CCCCWWWWWWYWCCBCCCGBBCWGWWYWWW"
                      "CCCBWWYYWWTWWWWGGCBCCCCBCWWWWW"
                      "CCBCCCWWWWYGGGGGGGGGGWBCGCBWWW"
                      "CBCCCCBWWGGGNGGGGGYGGGCCWCBBWW"
                      "WBGCCBCCNGGYYTYYYYWYGGGCBBCCCC"
                      "WWCCBCCGYYYYYYYNOYYYYYGGBGCCGB"
                      "WCWBCCCWOYYYYYYNYYYYYYYYCCCCBC"
                      "WWWCGCCBCYYYYYYNYYYYYTTWCCCBCC"
                      "CBCCCCBCCCGWWWWWYYYYGCCCCCBWCC"
                      "BCCCCBCCCCBCCTCNWWCCBCCCCBCCCC"
                      "CWCGBWCCCBCCTTTNCCWWGCCCBCCCCB"
                      "CCCBCCCCBCCCWWCNCCBCWWCBCGBCGC"
                      "CCCGBCCBCGCCBWCCCGCGCCWWCCCBCC"
                      "CBCCBCBCCCGBWWYCBCCCCKBCWWBCCC"
                      "BCCCCBCCCCCCWWYBCCBCBBCCCBCCBC"
                      "WCCCBBCCCBCWWWWCYWYWGCCCBCCCCB"
                      "WWCBCCCCBCCTWCWWWWWWWWWBCCCCBC"
                      "WWWWTCCBCCWWCWWWWWWCWWWWWTCBCC"
                      "WTWWWTWWCCTWBWWWYWWWWWWWWTWYWC"
                      "WWWWWWCWWWWWWWWWCWCWWWWWWOWWWW"
                      "WWWWWWWWCWWCWWWWWWCBCTWWWNWWWW"
                      "TGGNNYGGGGYGGGGYGGGYYGGGGNGGGG"
                      "YNGGGYGGYGYGCGCYYGGGONNGGYCGGG"
                      "YGGGGYGGGGYGNGGYGGGGYGGGGYGGGG"
                      "YYYYYYYYOYTOYYYYOYYYYYYYTYYWYT"))
   (make-level :name "Poppy Field" :difficulty 0.9 :slots 3 :seed 1 :par 543
               :order "111111111111111121111112222222022222222222221111111222211222222000000000000000000000000000000"
               :rows '("YCCCWCWWCWCWWWCWWWWCCCCCCCTCWC"
                      "CWWWWWCBCCCCWCWTYCWWCCWWWWWWWC"
                      "CWCCWCCCCWTCCCWCWCWCCCWCCCWCWW"
                      "WGWWWCCCWWWWWCWWWWYWWCYWWCCWCW"
                      "WCCCWCCGCWWWWWWCCCCWCWTCWWWWCW"
                      "CWWWYWWCBCWCWCWCGWWWCWWTWNWCCC"
                      "CCWCCCGGGCCCWCTYWWCWCCCCYNCCWC"
                      "WWCWCGGGGCWCWCBCCCWWWGCWKTKCWB"
                      "CKGGNGGNGGKGYKGGKGBKGGKGNNNGKG"
                      "KGGKGGGGGGGYKGGKBBBBBBGNNNONGG"
                      "NGKGNGGGGGNGGGKYGKTGKGNNNNNNNK"
                      "NRGGGRGRYYYRGRGRYGTRNGRCGCGYNR"
                      "GRGRGNGGGGRGGYGYRRKGTRTRRRNYYG"
                      "GGYRRGGRCGRGGYYGRGKYRGGYYNNGYY"
                      "RYRGRGGGGGGRGCGGGGKGWRGNRGGCGR"
                      "RGGRGGRYYGGNRGORGGRGGWGRGGGGYY"
                      "ORYRGGYGRRRNGRGRGGTGGGBGGOGGGT"
                      "GNYOYYGGGGNCGCROGRNRBBGBBTGRCR"
                      "YRRGGYYOGGGRRYRGYYYGRGTGGRGYYG"
                      "RRGRYGNRRRRPGRRGGGGGRRYGRYYGGG"
                      "RRRRGRRRGRRRRRCRRYOGNRBOTYGGGW"
                      "RGGRGRGGGGGGCNRRGNRNGGKWRGGGRG"
                      "RRGYRYRGGRGGRYGGYRGYGGKGWYGCGY"
                      "GGRGGYGYPRGRRGYYGYCGGRGGCGGYGY"
                      "GRRGGYRGRGRYGGGGYGYYGGYGGCPYGN"
                      "RGRRRPYGGORYRGGRGNGGYRRGYYGGNR"))
   (make-level :name "Dance Class" :difficulty 0.9 :slots 3 :seed 23 :par 543
               :order "0000000000000002220200000200000220202022222222222220111111111111111222221111111111111112220"
               :rows '("YTYYGGGGWGYGCNGGCGGGCGWWWWWWBG"
                      "YYYYGGGCCCCBGNGCGGGCNGTWYYYWGG"
                      "YYYOGGGGCGCGGGCGGGCGYGWYYYYTGY"
                      "YYYYGBGGGCGGYCGGGCGGGCWYYYYWNC"
                      "YYYYBGGGCGGGCGGGWGGGBGWWYYYTCG"
                      "YYYYYGGCGYGCGGGCGGGCYGTYYYYWGG"
                      "YOTYCGCGGGCGGGCNGYCGGGWYYYYWGG"
                      "YYYTCCGGGCGGGCGGGCNGGCWTYYTWGC"
                      "YYYYCGGGCGGGGGGGCGGGCGYYYYYWCG"
                      "YYYWGGGTGGGTYGGCGGGTTGWWTTYWGG"
                      "YYYBGGWOGGCWGGGGYCGKNGWYTTYWGC"
                      "WOWWWCGTGCCWGBNGGCGGKCWWWCWWGC"
                      "OWWYWWNPONORNNYTOONKGOOONYONTO"
                      "RNWPWNWWWOWWWNOYNOOKKNNOWWYOTO"
                      "OOTROWWWWTWCCWNWOOOKBNOWWWWWWO"
                      "TROPOOWPWNWPYOOYOONKKNYOWPWNYO"
                      "ONNONOOPOONPONWWWOOKKNNOOPOTWW"
                      "OONOONOPNYRPOWWWTWOKKONNOPWYWW"
                      "NYOKORNROKOONYWPWONGBNTONOOWPW"
                      "ONOONOONOONONKOPKNOKKONOOOOOPO"
                      "OONOONOONOONOONPONOKKONNOONOPN"
                      "NOONOYNYONORNORNYONNONNONOYNOO"
                      "ONORNRYNOONOOKOOTOONOONOONOOTO"
                      "OONOONOONOONOYNONNOONRONYONOON"
                      "NOONOONOONOOTOONOONOOOOONOONOO"
                      "ONYONOOTNONOONOONOONONNOONOONO"))
   (make-level :name "Card Players" :difficulty 0.15 :slots 3 :seed 60 :par 478
               :order "22211220220200000001211111111111111111111111111111222200000000000000000000002222222222222222202"
               :rows '("BNKNNBNNNNGKNNNBNNNKBNNNNBNNNN"
                      "NNBNNNNBNNNNBNNNNBNNNNBNNNKBNK"
                      "NNNNBNNNNCKNNOBNNNNBNNNNBNNNTB"
                      "NBTNNNBONNNCNNNTBNONNBNTNNCNNN"
                      "NNNBNNNNBNNNNBNNNNBNNONCNTNNBN"
                      "BNNNNNNNNKBNNNNBNNNNBNKKKKKKKN"
                      "NNBTNNNNTNNNBNNNNBNNNNBKKKKKNT"
                      "NNNNTTYTNBNNTNBNNNNBTNNTYTTNTB"
                      "NOOOTWTWNOOOONOONOOONONTTTTOTO"
                      "OOONTWTTROONOOONOONNONOTWTTNOO"
                      "ORKKKKBKBNNOOONOOONOOYYYYYYTOO"
                      "ONKKKKKKKNOOONKNONROOYYYYTWYYN"
                      "NOKKKKKKKOOONOWKNOOONYTYYYYYNR"
                      "OOKKKKKKKRONOOKKOOONOYYYYYYYOO"
                      "OYKKKKKKKONOOOKKRRNYOYYYYYTYOO"
                      "ONKKKNKNKKOOONKKYNOORYYYYYYYOK"
                      "NOKKKKKKCWWWNOKGKOWWWWTYYYYYKO"
                      "OYKKKKKKRRRRRRRRORRRRRWYYYYYYO"
                      "NOKKKKKNNRRRRRRRRPRRRRYWYYYYOO"
                      "ONKKKKKKKNONNONNNNNNNYYYYTYOON"
                      "OOKKKKKKKNNNNTTNNNNNNYYYYYYYNO"
                      "OOONNYONNNNNNNNNNNNNNOONOYONOO"
                      "OONNOONOOKNNNNNNNTNNNYNOOONOOO"
                      "ONOOYNORONNNNNNNNNNNNNROONOONN"
                      "NOONNOOOOONNNTNNNNNNNORONOOONO"
                      "OOONOOONONNNNNNNNNNNNOONOOOTOO"))
   (make-level :name "St Matthew" :difficulty 0.5 :slots 3 :seed 21 :par 729
               :order "111111111111111111111112222122220000200000000000000000000000000022222222222111112221222222221"
               :rows '("KKKNWTOTTOTTOTTOYTRTTOTOOTTOTT"
                      "GGKNNNNONNNNNTOTTROTOTOOYTOTTO"
                      "KKKNTTYYNTOTNOTTOYWOTTOTTOTTOW"
                      "KKKNTTTTNTOTNTTOTTOTTYTWOTTRTT"
                      "KKKNTTOTTTTTNTOTTRTTOYTOTTOTTO"
                      "KNBNTTYONTTWNYTTOTTOTTOYTOTWYT"
                      "GKKNNNONONNNNTTOOTOWTOOONTTOTT"
                      "KKKKKKKKNKTOTTOWTOTONTTOTTOTTN"
                      "NNBKKKKKKKKTTOTTOTWOTTOYYYTTOT"
                      "KKWWWKNRRKNKKTKKKYOTTOTTYTTOTT"
                      "KKKTKKKTGKBTKTOYTTTTOTRRRRRTTO"
                      "NKTTTKTTTKTTTKTTTTTOOTYRRRRTOT"
                      "KKBBBKWYYKORRKGNGYOTTOPRRRROTT"
                      "NKGGBKYYYKOROKGGGOTTOTRRORRTTO"
                      "KNBYBKYWWKRYRKGYCKKKKKRRRRRKGK"
                      "KKBBBKYYYKRRRKGGGKKKKKPRRRRKKK"
                      "KKBBGKTYYKRRRKGGGKKKKKRRPRRGKB"
                      "KKBBBKTYYBRNRKGGGKKKKKRORORKKK"
                      "WWWWWWWWWWWWWWWCWWKKGKRRRRRKGK"
                      "NNNNTNNNKNNNNNNNNNNKKKRRPORKKK"
                      "NNNKNNTNOTONNTNNNNKNKKRRRRRKKK"
                      "NNNKONNNNONNNNNNNNKKBKNNRRRKKK"
                      "NKKNNKNKKNNKNNKNNKNNKNNKNNKNNK"
                      "NKNNGNNKNNNNNKOOKNTKNTKNNKTOGN"
                      "KNKNNNKKNKNNKONKNNKNNKNNKNKKNN"
                      "NNKNNKNNKNNKNTKTNKNOKTNKONGNNK"))
   (make-level :name "Peasant Wedding" :difficulty 0.9 :slots 3 :seed 5 :par 530
               :order "000002000000000000000000000000001111111111111111111111111111222122212222222222222222222222221"
               :rows '("YYWYWYNNYNNNYNKNYNNNYNNNYNNNYN"
                      "YYYYYYKYNNNYNNOYNNNONNNYNNOYON"
                      "YYYOYOYNTNYNNNYNNNONNOONNNYNNN"
                      "YYYYYYNNNTNTNGYNGGGNNYNTNYKNNO"
                      "YYYTYYNNYNNNYGNYYGGNYNNNYNNNYN"
                      "OTOYWYNYNNNYNGGYGGGWNNNWNNNYNN"
                      "WYYYYYYNNNYTNGCCGGGONOONNNYNKN"
                      "YYYYYYNNNYNTNGGGGGGNNYNNNYNNNY"
                      "NNNNNNNNKNNONNGGGGGNNNNNNNNNKN"
                      "NNKNNNNTNNTNOTGGTGCWNNYNNTTNON"
                      "NNNNNNNTNNTNKTGTWTGTNNTKNTNNTN"
                      "NNNONOYYYNGYKKKBKKKKKGKKBBBWWW"
                      "OOOOOOWWWWWWYTWWWWWYTWCWWWWWWW"
                      "OOONOOCCWWWWWCOTYWTWYWOWTYOWYW"
                      "OOOOOOWWWCWWWTWWWTWWWWWYWWWNWW"
                      "OOOOONOOOOOOOOOOONYOOYOOYONOOO"
                      "ONOOOOOYNOOOROOOOYNOOOTTRRNOOO"
                      "OWTOOOOOOOOOOTTOOONORORRONOOYO"
                      "ORROOOOOOOOOOBBROROOOORRNOOOOO"
                      "ORRWOWOTOWOYOBBONOOOOONROOOOOO"
                      "ORRKNKNNNNNNNBBOOOOOOORROOOOOY"
                      "ORROOOYOROOOOBBOOOOOOORROOOOOY"
                      "ORROOROONORONBBOOOOOOORROOOOOO"
                      "YNROOYOOOOOOOBBONORYOORNROOOOO"
                      "NRROOOOOROOOOBBOOOOOOORROOOOYO"
                      "NNROOOOOOYOOYBBYOOOOOYORROONOO"))
   (make-level :name "The Dream" :difficulty 0.9 :slots 3 :seed 18 :par 428
               :order "11111111122111112222222220222221111221111111112122222222122211000000000000000000000000000000"
               :rows '("CCWCWCCCCCCCCCBCCCCCCCCCCCCCCC"
                      "CCCCCCBCCCCCCCWCCCCBCWWWCCWCWC"
                      "CCCBKBCCGCCCCCCCCCBCCTCYCCCCCC"
                      "CCCCKKCCCGCGCCBGCCCCCWWTCCBCCB"
                      "CCCGCRPNCCCCGCGOCYCCCCCGYNCCCC"
                      "YNYKCGKNGGGGGGYYGYGCGGGCYNGGGG"
                      "CGGGGGGGNBONCGWWGGGCGCYGGGGGYG"
                      "GGGGGGGGNGGNCGYYGGGGGGGGGYGGYG"
                      "GGGGGGGGYYGCGGGGGGGGGGGGYGBOOG"
                      "GGYGGGGGYGGGCCCGGGCYGGNGYGNYCG"
                      "GGGGGGGGYGGGYCGGYGGGGGGOGGGYGG"
                      "RRNCGCGYGNGYGCGCGGGRGNGGGGGCGG"
                      "RNNNTTTTTGGGGCGGGGGNCGYGGGGGCG"
                      "RRNYTTWWTTTGGYGCGGNCGGPGCNGGGG"
                      "RRRRTTTTTOGKGCGGGCGGCGGNGCGGGC"
                      "RRRRRNRRRRGKGGGGONYGCWTGBYYCGG"
                      "RNRRRRRONRGGBGGGGYNGGKKYWCGGCG"
                      "RORRRRRRRRGGKGYYGGNNWKKCGGGGBG"
                      "GGYGGGGGGPGGGGGWGWYGYKKGGGNGKC"
                      "GPGGGGGCGNGNGGYGNGNGGKGGGNGGCC"
                      "GYGYGOGGGNGGNGYGGNOGGKKGGNGCCN"
                      "YGGGGGYGOCGGNYGGGCGGGKKGNGGCCN"
                      "GGGGGCGGTCCNNGGGCGGGGKKGGGCCGG"
                      "YNGGGYGGCGGNGKGNNGCYGGGNCGGGGG"
                      "NGGGGGGGWGGCGKGNGGGGGGKGGGGGYG"
                      "NNCGGGGGGYGGNGGYNGGGGYYGCGGNGN"))
   (make-level :name "Rain Steam Speed" :difficulty 0.9 :slots 3 :seed 3 :par 353
               :order "11111100010000111001111111111111111111011010000000000000000000002222222222222222222222222222222"
               :rows '("COYYYOOWWCOOWWYOCYCYBCYYWCBCWW"
                      "OYYOCOYYYOOWWCOOYYYWCWCYCCWWWC"
                      "YYYOOYTYGOYYTOOYYCCCOWYCGYCYCC"
                      "TYOCYWYROWWYCOYYYCCWOCGCYCTCCY"
                      "YOOYYTYCWWYORTYYCCYYYWCWYCCCTY"
                      "ONCYWOYWWWOCWYYOCYYWCCWWWCBTWC"
                      "OWYWOOCWWORWWWRCYWWCCWWWCWWWTC"
                      "WCWOOWWWORCTWOOWWWCCWWWCCWWWCC"
                      "YWWYTCWWWWWWYYCYTWTWWWWGWWWWYY"
                      "CWWYWWWWWCYYYYYTYWCWTWWWWOYCYT"
                      "NTNWCWWYYYYOYCYTYOWWTCCWYYYYYY"
                      "NNNTNNWYWYYTYOYYYCYWYYYYWYGYOY"
                      "NOOCNNKNNYYYCYYYYYTTYCYYYYYWYY"
                      "NTTTNTNCNNKTYTOTGTYTTYYYKBYYYY"
                      "NTCTNTTYNNNCOONTTTTTCTWYKKYYTC"
                      "NTTONTCTNYTTKNNCNNTTTTKKCKKKKT"
                      "NCTTKTTWNTCTNTWNNNNCNTKBKROKCT"
                      "TTTTNCYYOYTWNTCTNTNNNNKCKKKKKT"
                      "CTTYNYYYNCYWNTTTNTCONNKKKBNCBT"
                      "NYYYCYYYKYYTNCTTNYYYNYCGGKBKKN"
                      "CGKBCCTCCBCBBCBBCCYONYYYTYGNNN"
                      "CCBWKKNGGCBGCBBCBBYYOCYYKYYTOY"
                      "GBGBBCBCCBBCGBCBCCYYNYYYNCTWNT"
                      "CCCBCBBCBBCCBCBCCBYYGYTTNTTONC"
                      "CBBCBBCBBCBBCBBCBBYTNTTTCTTTTT"
                      "BWCBBCBBCBCCBBCBGCTCNTTTNTTTBO"))
   (make-level :name "The Dance" :difficulty 0.5 :slots 3 :seed 7 :par 511
               :order "222222122222222222222122222121222211111111111111111111101100000000000000000110000100000000012"
               :rows '("BBBBBBCBBBBBBBKBBBBBBBCBBBBBBB"
                      "BBBCBBBBBBBBBBBBBBBBKBBBBBBBBB"
                      "GBBBBGBBBBOCBBBBBBBBBBBBBBBBBB"
                      "BKBBCBGBBOOBBCBBBGBBBOKBBBKBBB"
                      "BBBBBBKBBBOKBBBBKBBBBOOKGCBBBB"
                      "BBBBBBCGCBOOKOKOKNGOKRORGBBBBB"
                      "GGBKBBBBBKOOBBBBBBBBGCOOCBBBBB"
                      "BBBBBBBBNBOOBBBBBBBBBBOYBBBBCB"
                      "BBCBBBBBRBOOBGBBBBBKBBOOBBBKBB"
                      "BBBBBOKOCBRRBBBBGBBBBGROBBGBBK"
                      "BBBBBROBBBRBRBBBBBBBBBRORKBBBB"
                      "BBCCBONBCRBBRBGBBKCBBRBBOOKBBB"
                      "BBBBCOOKRBGBPGBBBBBBOBBKOOBBKB"
                      "BBBBBOOBROBBCRKCCBBBROGBORBCCB"
                      "GGCCBOYBBBOKKBOGKBOKOBGBOOBBGY"
                      "YGGGGRRGNBBBKROONOBBBBGYOOGGGG"
                      "GGYGRGRGNGGGGGOOCGGGGNGGRRGGGG"
                      "CGGGRGGRGYGGGCORNGGYGGGCRYRYGG"
                      "GGGGRGGCPGGYGGROGGGGGGGRGGRGGG"
                      "GYCRNGGGRGGGGGOOGGGGNGRGGGRGGG"
                      "GGGGYGGGGGGGGGRRGGGGGGRGGGCRYG"
                      "GGNGCGGGGCGGGGRGRGCGCGGYGGYGGN"
                      "GGGGGYGGGGGGGRGGRGGGYGGGGGGNGN"
                      "GYGGGGGGGGGGRGGGRGGCCGGGGNGGGG"
                      "GGGGGGGGGGYGRYGGGRCGYGGGGGGGGG"
                      "GGGGGGGGGGGYGGYGGCGGCGGGGGGGGG"))
   (make-level :name "Ejiri" :difficulty 0.9 :slots 3 :seed 48 :par 599
               :order "0000000000000000000000000000001111111111111111111111111122221222222222222222222222222112"
               :rows '("TWWWWWWWWWWWWCWWTYWTWYWWWWWWWW"
                      "WWWWWWTWWWTYKWWKWWWYCWWWWTWWCW"
                      "YWWYWWWWWWWWCTTWCWKWWWWWYWWWWW"
                      "YWWYYCWWTWYCWCTWYWWWTCWWCWWCWW"
                      "WWWWWWWWWWWYTWWCWWNNWWWWYWTWYW"
                      "WYWCWWTWWWYWWWWWWWWWNWWWWWYNNW"
                      "WWWTWTWCYWWCWWWWKCYWWKTKWWKKNW"
                      "WWWTWWWOWWWWWYCWWWWYYWWYKYNWOW"
                      "WWWYWWWWWWWWWYWWWWTWWWCKWYNNWC"
                      "WWWWYWYCWWWYYWWWWWCWWWWWWWNNCW"
                      "YWWWWWWTTWCWWWWWTWTKKCTWTWKNWW"
                      "YWWOYYYCNNTNWKKKKYYYYTWWWYNNWC"
                      "OTTBTYCWCYWTTTTTTTTYTTOTTNTTTT"
                      "TTBGBBGCCNNTTTKKTTOKKTTOTNNTYT"
                      "KNNNBBKNGNNGCTKKNTNKKOKNTNKYTT"
                      "YNGYBBYGNNNNNOKNGCYKKYGGTNTNNN"
                      "GGYGCBGGYNNYGGKKGWYKKGYYNNYGNO"
                      "GWGGYGCYNGYGGYGGYGGYGGYGNNGGYG"
                      "YGGYGYYGGYGGYCGOGYYGGYGYNNGYGC"
                      "GGWGGYCGTGYYGGYGGYGYYNGWNNYGGT"
                      "GYGGYGGYGGTNGYGGYGGYGNTKTYGYOG"
                      "YGCYGGYGGYGGYGGYGGTGGYGNKGGWGC"
                      "GGYYGYGGYGYYYCYYGYCGYGGOONOGGY"
                      "GYGGYGCYGGYGGYCGYGGYGGNGNYGNOG"
                      "YGGYGGYGGYGGYGYYGGYGGYNNYYCYGC"
                      "NGYGGYGGYGGTGGYNGYGGYGNNCNYGGY"))
   (make-level :name "Proverbs" :difficulty 0.5 :slots 3 :seed 1 :par 391
               :order "000000000000000000000000000000110111111111111111111111111111222222212222222222222222222222221"
               :rows '("TGGGGTGGNGTGRGGTGGCCCCCCCCCCCC"
                      "CGGGWGGGGTGRRRTGGGCCCCCCCCCBCG"
                      "GGGYRGGGTRRORRRRGGCCWGCCCCCWBC"
                      "GCRNRRRTGNNNNNNKCTWCBBBBBBBBBB"
                      "RRRRRRRRRNYNYNYNTGCGBBBBBBBKBB"
                      "NNNNNNNNGNTOKNNNYGCCBBBBBBBBBB"
                      "NYNYNYTYGTTGTGTGGGGTBBBBTBBBBB"
                      "NNNNNNNNWGRGTTTYGGTGGGGOTGGGTG"
                      "NNNNNNNKGGRGKGOGGOGGTGTGOGRTGG"
                      "NNNNNNNNGGGTKGOTTCGYOTGGORRNRC"
                      "NTNTNNONGTTGGTCTGGGGRGNORRRNRR"
                      "NYKTNOTNGTGWGTTKGGGTRGNNNYNNNN"
                      "GYGYGTTGTWGTGBTKGGOGGGOYNTNYTY"
                      "GYTYGBTTGYGWTBTGCWGGGGNNNKNNTN"
                      "GTGOGBKTCGGWTGWGTGGGGTNNOKNNGN"
                      "TCGGGOKTGTTGRGWTGGTGTGNNKTKNGN"
                      "GGGTYGGGGTGGRGWGTTTTGCGGTGGYGT"
                      "GGRWPGTNOYGGGTYGTWPGGGGTTNGTTG"
                      "RRRBRRTTGYCGTGGTWTRCGGTGTGNWGG"
                      "NNNBNNKGGGGTGGTTWWGGGTGNGNOWGG"
                      "NYNYNWKGGGTCGGTTGGGGTGGGGTGWNC"
                      "NNNNNNGGGTYGCCRTCGCTTTGGTCGGTT"
                      "NTNNNNNGTNGGGTRWGGOGTTGTGGGGTG"
                      "NNKNNNCTGGGGTNYWCTGGBKTGGGGTWG"
                      "GTGGGGTGGGGTGCGGTGGGBKGGGGOGWG"
                      "TCCGGTGGGGTGGCGTGGGGTGGGGYGGGN"))
   (make-level :name "Tahitian Women" :difficulty 0.5 :slots 3 :seed 3 :par 531
               :order "2222222222222111121221111111111111222222222110111111121121220000000000000000000000000000001022"
               :rows '("WGGGGGGGGGYNGGGGGGYGGGCGNGNGGG"
                      "GGYYGNGBGGGGYGGGNGGGGGGGGGGGGG"
                      "GNGYGGYGYGGYGGCYGNGGGGGNGYGGGG"
                      "GGCGGYNGGGGNCGGGGGCGGCGGGGGGGG"
                      "GCYGGGGGGGGGGGGGYGGGGGGGGGGGCG"
                      "GGNGGCYNGGGGYYGGGYGCGGNCGGGGNY"
                      "WWWWWWNNNWWWWWWWWYCWWWWTWWCWWW"
                      "YWCWWWNOTTCWWWWCWWWWWKWWTCWWWW"
                      "OPOOOONTTTPROYOOWRORKKKOROPOOO"
                      "OROPNOPOTOPWOPOOOOPOYTTPOOOOPR"
                      "POOOOPPWPPOROOOPOROOWTOONPOORO"
                      "OOROPPPPPPPPPOOOOPRRWTTRROOPOR"
                      "OOOYPPWRPPPPPOPOORWRRWRRWROONR"
                      "OPOOPWPWPPORPTTTTWRRWROWRRPOOO"
                      "ONOPRPPPWRPPPTTOORPWRRCORWONRO"
                      "POOPPPPPRRPPPTTTTRWRRYRRCRROOR"
                      "OOPOOPPWPPRPPOYORWRRWRRWPRWPOO"
                      "OOOOWRRWPPPPPRPRWRPWORWRRWRRNP"
                      "OPOOPOWPPPPPRONYRRWRRWPRWRROOO"
                      "OOOPOPPPPWPPPPOORWNRCRRWRRWOPO"
                      "ROOOOPPPWWRPOOOPWRRWROWRRWOORO"
                      "OOPOORWWPPPRPOOOORWRRCRRWROPOO"
                      "OOOOPOPWPPPOONPROWRRWRPWRROOOP"
                      "NPNOOOWOROYPYOOOPORWRRWRRNPROO"
                      "OONPOOOOPOOOOPOOOOPOOROPOOOOPO"
                      "PONOOPOORRPORROWONOYPOOOOPOOON"))
   (make-level :name "Rhone Night" :difficulty 0.9 :slots 3 :seed 4 :par 491
               :order "222222202222222222221200000122221111211211111111111111111111111122222200000000000000000000000000"
               :rows '("KGBBKGBBNBBBKBBBKBBBKBBBKBBBGB"
                      "BBBKBBBKYBBGBBBKYBBKGBBNBBBKBB"
                      "BBKBBBKYWYKBYBKYWYKBBBKKBBKBBB"
                      "BBBBBBBBTBBYWYBBYBBWKBBBBBBBBB"
                      "BBBBBBBBBBBBYKBBBBYCYBBBBOBBBB"
                      "BBBBBBBBBBBBBBBGBBKYBYBBYWYBBB"
                      "BBBBBGBBBBBBBBBBBBBBYCYBYYBBBK"
                      "BBBBBBBBBBBBBBBBBBBBBYBYWTBBBB"
                      "BBBBKBBBBBBBBBBBBBGBBBBBYBBBBB"
                      "BOBBRBGOBBNBBOGBOBBOBBOBCOBBOB"
                      "KYKKWKKYGKYKKYKKYKKYKKWKKYNKYK"
                      "KKKKKKKKKKKKKGKKKKKKKKKKGKKKGK"
                      "CYBBCBBYBBBBCTBBCBBYCBBKCYKCCB"
                      "BBBCBYBCBCKYCCBCBYBWKBCYBBBCBY"
                      "GYCBBCCYBBCBBYCBBBCYBBCGBYCBBB"
                      "BCBBBOBCBCBYBBBBBTBBCCBYBCBBBY"
                      "COBBCBGOCKBBCOBBCBGOCBBBCOBBCB"
                      "BBBCBOBCBBGOBBBCBOBCTTBTWBBGBO"
                      "BOCBBGCOBBGGBYCKBBCRBKCYWOCGBB"
                      "KCBNNONNBNNOBCCBBOBKBBBYYCCBBR"
                      "GNGNONNNGNNGGNGGNYGNBBNYYNGGNG"
                      "NGGNGGNGGNGGNGGNGCNGBBGYOGGNGG"
                      "GGNGGTGGNGYNYGOCGNGNBBGOYGNCGN"
                      "GNGGNYGKGGNGGNGGNGGNBBNYYNGGNG"
                      "NGGNGGNGGNGGNYGNGGNGCBGYYGGNGG"
                      "GGNCGNGGNGYNGGOGGNGGBBGYYYNGYT"))
   (make-level :name "Night Cafe" :difficulty 0.9 :slots 3 :seed 13 :par 575
               :order "000000000000000000000000000000011111111122111111111111111111112222222221222222222222222222221"
               :rows '("GGGGGGGGNGNGGGGGCGGGYGGGGGGGGG"
                      "GGGGGYGGNGGGGNGGGGGGGGGGGCGGGG"
                      "GGGGGYGNGGGNGGGGGCGGGGGGGGGGGG"
                      "GGGGNGGGNGGNGGGGGNGGGGGYGGGNGG"
                      "YCGGOYRGGGGRYYGGGGOYONGGGOYOGG"
                      "NRROYWYOPROYWYORRRYWYORROYWOOR"
                      "RRRRRYORRRROTORRROOYORRRROYORR"
                      "RRRRRRRRRRNRRRNRRRRORRRRRRRORO"
                      "OORRRRRRRRRRRRRRRRRRRRRRRRNNNR"
                      "RNRRRRRRRRRRRRORRRRRRRBCCCRRRR"
                      "RRRRRRRRRORRRRRRRRRRRNCKKCRRRR"
                      "ORRRRRRRRRRORRRRPRRRROCKKGRRRR"
                      "RRRRRRRRRRNNRRRRRRRRRRCKBWRRPR"
                      "RRRRRRRTWRRRRRRRRPRRRRCKNBRRRR"
                      "RNRRRNRWCNRNRORNPNRRRNCCCCRRRN"
                      "OWYOYYOWWNYTOYWOYYOYYOYYRYYOYY"
                      "YYOYTOYWWYGGGGCGYGYGGYYOYTOTYR"
                      "YTYYRWYWWTGNGWGGGGGGGYOYYOWBCB"
                      "RBBBOYOWWOGGGNGGGNGGGOOYOYYBBB"
                      "WBBBWOYYOYGCGGNGGNGYGYYOYTRYWY"
                      "YOWTOYYOYYONWYWYRYYNYYOYYTYYOY"
                      "OTWOTOOYYOYNOOOOYYOTYOYYYBBKYY"
                      "YYOYBBBYNWYOYYOWYOYNOYYOWBBBYR"
                      "YOYWBBBOYYYOYOWYOOYOYWOYYOWYYY"
                      "OYYOYWYYYOYOOYYOYWOYYYYYOYYOWY"
                      "YYOYYOYYOYYOYYOYYNYYOYYOYYOYYO"))
   (make-level :name "Death and Life" :difficulty 0.3 :slots 3 :seed 23 :par 519
               :order "2222222222222222222121111111111111112111111100000000110000100000000000000021112212100000222222"
               :rows '("WGGYCGGGCGGGCGGGCGGGCGNGCGGNCG"
                      "GGGWGGGCGGGBCGGCGGGCGGGWGGGCGG"
                      "GGBGWWWWGCCGYGCGGGCGGPCGGNCGNG"
                      "GCCCYKKWBCCGGCGGGCPRRRPPPCGGGC"
                      "CGBBWWWWBGGGBGGGYYYNPPPPPPGGCC"
                      "YGBBKBBBBGGBGGGYYYOTTPPPPPOCGN"
                      "GGBKGBKKGGCGGGRRRRRTOPPPPPPPGG"
                      "GCBKBBKBBCGGCCPOPPPCBBPPRPPPPC"
                      "WGBCBBCBBGGGCGTTTWCCPPYYKPPPPG"
                      "GGBKKGKKBGCCGGPTPPPPWPPTTTPWRG"
                      "GGBKBBKBBGCGGPRPPOONCCWPTPPPPP"
                      "GCBGBBBBBGGCGPPPPOYYYOPPPPYYYO"
                      "CGBKGBKBBNGGCCCPPYPNKKPPPPPPPP"
                      "GCBKBBKBBNGCGPPRTTTPPPPPPPRWPP"
                      "GGBBBBBBBKCGGCCCPTCCCPPYYYPPOP"
                      "GCBKKBKKBNGGGPWPPPPPPPPPPOPRPP"
                      "WGBKBBKBBNGGCPPCGCWPPWKWPOPPPP"
                      "GYBBBBBBBCNCGGPPPPRRRYTTOPPPPG"
                      "GGBGBKKKBGNYGGOPRKKWWTTOPPPPPG"
                      "CCBKBBKBBBNGCCPPGCKCBPPPPPPPRC"
                      "CNCBBBKBBGNGCGGPPPWKPRRRRPPPGG"
                      "GGBBKBKKBGNGNGGCPPTTTPPPCCCCGG"
                      "GGBKYGNGGGNGGGCGGRRTPPBBBPCGGG"
                      "GCGGYBGYGCGGGCGGGCPPPCCCWCGGGC"
                      "BGGGCGNGCGGGCGGYCGGGCPGGGGGGCG"
                      "GGGCGGGCGGGCYGGCNGGCGGGCGGNCGG"))
   (make-level :name "Parliament" :difficulty 0.9 :slots 3 :seed 10 :par 597
               :order "00000000000000222222000000000000000001111111111111111111222211122222111111122221221222222222"
               :rows '("POOPNOWORROOPOYPOOROOPOOWNOPOO"
                      "OOPOOPOOPOOPOOPROPROPOOPKOPOYP"
                      "OPOOPOOOOOWOOPORPOOPOOPYGKOOPY"
                      "POKPOOPOOPYOPOOPOOOOYOOOBKOPYO"
                      "OOBKYROOPOOPOOPOOPOYYYRRBBPOOP"
                      "OPBBOOOROOOOOPROWOTYTOYOBBYOPO"
                      "POCBOOPORGONOOOOOYYWYYYYBBOKOO"
                      "OOBBOPORPKBPOOPKOPYYYOYPKBPBKP"
                      "OPBBPOKPNBBOOPNBBROYTYPOBBOBCO"
                      "BBBBBBBBBBBBGKBBBPOPOPPBBBBBBC"
                      "KBGBBBBBBBGBBBBBGPPWRPRBBBBBGB"
                      "CBCBBBBBGBBBBBBCBCGBBBBBBBBCBB"
                      "BKBBBBBGBBBKBGBBBBBBKBBBBCBBBK"
                      "CGBBBBBKBBBBBBKBBBBBBBBBBBBBBG"
                      "BRBBPBKWBBPBBPGBPBYPYCYBBRCKPC"
                      "PBBPBBPBBRBBWBBPBYPTBYBYPGBPBB"
                      "BBPBGPBBPBKPGBWBBPYGYBYPBBPGGP"
                      "BWBBRGBPBBPKBPBBPYBYBYPYBWBBPB"
                      "PBBPBBPGBPBBPBBPBBOBYRYKPBBPBB"
                      "KBPBBPBBPBBPBBPBBPBOPOBPBBPBBP"
                      "GPBGPBKPGBRBGPKBRBOPOBOBBPKCPB"
                      "WBBRCGPBCPBBPBBPBBPNGNKGPBBPBB"
                      "BCPKBOBBWBBPBBOBBPOBOBOPCBPBBR"
                      "BPBBPBBPBGRBBPBBPCBOBOPBBPGBPB"
                      "PBBPBBPBBPCGPGBPBBWBYPGBOGBRBB"
                      "BBPKBPBBWBBPBGPBKPGOPOBPBKPBBP"))
   (make-level :name "Rouen Cathedral" :difficulty 0.9 :slots 3 :seed 9 :par 518
               :order "10001011111111111110000000000122200000000000022222222222222220220000222222112222111111111111110"
               :rows '("CCTOBYOWYOBOOYWRYYOBWWYYOBYOCG"
                      "CCYYOOYTOYOWYYOWOYWNOYYOYOOYBC"
                      "CCYCOBYBWBRKOBYBOBYBYCWBOBWYBB"
                      "CCYOOWNYYOOYWYYOYYOOYOOWOYWRCC"
                      "CCTWOOYYOWYOYYOYWOTOOYYWYOOYBC"
                      "CCYYOYWOYYOYTWYCOYTYWYOYTOYWCC"
                      "CCYYRYOYYORYOBBCKGOOYOWYWOYNCC"
                      "CCOYROYWNYORYKBCWBWOOWYOYOYYCC"
                      "CCYYOOYYYYOWBCCWCCBOWYOYOOYYCC"
                      "CCYOOYRYTONYOGWCCGOOYOWYOYYNCC"
                      "GWOWOWYYOYOOWKKCBBYYOOWOWOWYCB"
                      "CCWYYYYOYWRYOOYBWYWOOYOWOOYYGC"
                      "CCYYYYWBYOOYOWYGYOOOWRBYROYWCC"
                      "CCNNOTYNONYNWNONYNYNONBNYNOWCC"
                      "CCYYOYBGBYOYYOBBBYYYYCBBOOYYCC"
                      "WCYOOOBBBOOWOBBGBBWNYKBBOOYNWC"
                      "CGOYOOBKBYNOYBBBBROOYBBGYOOYCC"
                      "CCYYOWBCBYOTWBCGBYOOYBCBWNCYGB"
                      "CCWROYKCBWOYOBCCBYOOYBCBOOYOCC"
                      "BCOYOOCCKYOOWKCCBOYOWBCBYOOWGC"
                      "CCYWOYBCCYOYYBCWBWYOTBCCTRYYCC"
                      "CCYRRYKCBOOONKCCCYOOYBCGOOWOWG"
                      "WCOYNOBCKYOWTBCCBOCOOKCGYOOTCW"
                      "CCYYOYKCKYRYYBCCBYYRYBCBTNYYCC"
                      "CGYOOWBCBOOYWBCCBYOOYBBBOOWOCC"
                      "GCYYOOBCBWOOYBCCBOYOOBCBTOOWCC"))
   (make-level :name "Hay Wain" :difficulty 0.7 :slots 3 :seed 6 :par 552
               :order "222222222222222220222222022222000001001012011200000000000111111110010000111110111111111110"
               :rows '("CGGGCYGCCWCBCCCGWYWWCCCCCCCCGG"
                      "WGGGKGGGCCCCWWTWCWWWGCWWCKGYGG"
                      "GKGGGGGNKCWWWWWWWWTCCCBCWGGNGK"
                      "GGYYGKGGYCYWCWWCCBBCCCWBGGKGGG"
                      "CGKGGGGGGNGCGCCBCCWCCCCCYYGGCG"
                      "GGGNGCKGGGWGCCCCCCWWBCCCGGGKGG"
                      "NNGKYGNYGCCCCCCCCCCCWWCCNGGYGG"
                      "KNGGGCGKGGCCCGCCWWYWWWWGNGGNKG"
                      "GGGGKGNGGGCCCYCTWWCWWWCCGKGYGG"
                      "NNNNNNYNNYCCBWWWWWWCCCCCGGGYGK"
                      "NORRRRGGGGCBCWGCCBCGCCBCGGKGGN"
                      "RRRRRPGGYBGGGGYGYGYNTGYNGGCGGG"
                      "RRYRWRKGYYGGGYCYGOGYKYYYGGGKNG"
                      "NRORRNGGGGKGCGYGYKYGYGYGOCGGGG"
                      "OPRRRRCNGGGGCOKYGTGYGYTYYWGGKO"
                      "RRORRRGGGGGNGYYYYYOYYGTNYKYNTG"
                      "GGGGNGNGGBBCKKNKNNNNNGKCNBCBBC"
                      "GGYGGYGYNKCBBNOONNTNNBGBOCBBBC"
                      "GGCGGGNYKBKCCNNNTNONOCBBNKGCBB"
                      "BBCBBCBBCBBCBNNKONNNKBBCNBCBBC"
                      "BCBBCBCWBBGBBTKNGTBNBNCBGWBBCB"
                      "CBGCBGWCBBBBWBNWNBNBNCBBGBBWBB"
                      "BCCBBCBBBBBCGNBTBNBNCNBCKBCBBC"
                      "BCBKCBKWBBCBBCNBOBNCNBCBBCBGWB"
                      "CBBCBBCBBGBBWNGNBNWNBNBBCBBCBB"
                      "BBCBBCBBCBKGBBTBNCNBNBBCBBCBBC"))
   (make-level :name "Blue Boy" :difficulty 0.9 :slots 3 :seed 5 :par 566
               :order "222222221002200200022000000000000022022222222220000000000111111111111111111111121112221111122"
               :rows '("CNNKNOKCTKNKKNCKNKKNNCNNKTNKCN"
                      "NNCNNKNNKCNKTOKNCKNKNNOCNNKNNK"
                      "NKONBNNKNKKCNKTNNNCKNNKNNCNNKN"
                      "KNNKNNCNNKNNKCOOOOKKBKNNKNNGNN"
                      "NCKNNGNNCNNKNONROONNKNCKNTKNNC"
                      "NKNCKNNKNNCOOYYTTYNKNONNCKNNKO"
                      "KNNKNCBNNKNNCOBTKYKCNKNNKNBKON"
                      "CNKNNKNCKNNBOKTRTNNNGCNKONKNCK"
                      "NGCNKNNKNCKNNWTWWCNKNNKCNKNNKN"
                      "NOOGGNKNNKNCKNNBNTCNNKNNKCNKNN"
                      "NNKNNKNNKNNKNNKCNKNNKNNKTNNNNK"
                      "NKNNKNTBNNKNNKBBGNNKTNNNTKNNKT"
                      "KNNGNNKNNKNNKNCBBNKNNBONKNNKNN"
                      "NNKNNKTNKNNKNBBBCBNNKNKKNNNNNK"
                      "NKNNKNNKNNKTNCBBBBTKKKWKKKNTKN"
                      "KNNKNNKNNKNNBKBCBBBNKKKKKNNNNN"
                      "NNKNNKNNKNNNCBBBBCWNKKKKKNNNTN"
                      "NKNNKTNKNNKBBBCWBBBCNNKNNKKNKN"
                      "NNGNGGNGGNGWWBBBBBBBGNGGNYGNGC"
                      "GGNGGNGGNGBKBGBBBBWKWGGNGGNGGN"
                      "GNGGNGGNGCNGBBGGBWGNGGNGGOGGNG"
                      "TGGNGGNGGNGGCBGNBCNGGNCGNGGOGG"
                      "GNNGGNCGTGGNBBNGGKCGNGGNCGNGGN"
                      "GNGGNCGNYGOGCBGGCGGNGGTGGNGCNG"
                      "NGGNGGNGGTGGBCGNCBKGGNCGNGGNGG"
                      "GCOGGNGGOGGKKNNGKKKGOGGNGGNGNN"))
   (make-level :name "Primavera" :difficulty 0.3 :slots 3 :seed 13 :par 519
               :order "22222212220222222222222222222000002000000000000000002200000001011111111111111111111111111111"
               :rows '("GKGGKKGKKOOKGKKGKKGKKGKKGKKGKK"
                      "KKGBKCKGGKKGKKNKKGKKGOKGKKGKKG"
                      "KGKKGKKGKKGKKOKBGKKOKKONKGOKGK"
                      "GKOGKKGKBGKKOBBBBKGKKGKKOKKGOK"
                      "KKGKKGKKGKKGKKGBKGKKCKKYKNGKKG"
                      "KNKKGOKGKKGKBGKKGKOGKKGKKGKKGK"
                      "GGKGKKNNBGOKGKBGKKGKKGKKGTYGYT"
                      "KKGKKGKKGKKGKOGTKCKKOTNGKBBKGG"
                      "KGKKGKOOKKGKKGTTTKOGKTGKBBBGGC"
                      "GKTTKYTKTTNOGKKRKKGKWWWKGBBGGG"
                      "KKWWKWWKYWKGKBWTROKKWWWGKBKKGG"
                      "KGWWGWWOWWGOKOWWRKKGBWWKKBBKGG"
                      "GKWWKTWKWWKKGPWWRROKYCCKGBBGGG"
                      "KKWCBWWNWWKGKRCWRNKOYRRGKBCKGG"
                      "KYWWGTWGWWGKNRWWRRKOWWCKKBGKGN"
                      "GKWWKWWKWCKKGRYWRRGKWWWKGBBGNG"
                      "KBWWKWWKWWKGNRWWRRKKWWPGKBBKGG"
                      "GGWWGWYGWWGKKRWYRRKGWRYGKBBKGG"
                      "GKWWKWWKWWKKGRWWRRGKBWWKGBBGCG"
                      "KKWWNWWGWTKGKKWCRGKKWWWGKBBKGG"
                      "WKTWGWWBWWYKWWWWRPGYWWRPNBBOGG"
                      "BWKPWGRGYBKRYWWWTGGNWWWBYBBBCG"
                      "RPPWCOGBGGPRPWGWOGWYKGWRPGKGPW"
                      "RGGGGWWGGGYGYKYGGWYKWGKYYBPYGR"
                      "YWGRKYGGGKKBGGRPPKKGYKTWRGGYTK"
                      "OPRGYGWYPWWRWRRGYPWGGGGBBGGWGK"))
   (make-level :name "Sistine Madonna" :difficulty 0.15 :slots 3 :seed 54 :par 557
               :order "20001110110202201110000111000000000000000000011122222222222222111111111111111122222222222"
               :rows '("NNNTKNTNNNNNNKNKNNNTNTTNNNYNNN"
                      "GYYGWTTCWWWCWWWWWWWBWWWCWWGGCG"
                      "GGGGWWCWWWCWCWCTWWCYWWCYTWGGGG"
                      "GGGYWCWWWCWWWCBBKCWWWCWWWCYGGG"
                      "GYGGCWWWGTWWCBBBCBWWCCCWBWGNGG"
                      "GGGGWWWCWWWCWCTTYBTCWWTCYWGGYN"
                      "CGYYWWCWWWCWWBTTOBCWCTCYWYYGGN"
                      "YGGYTCWWWCTWTCTTTCWCWCWWCCGGGG"
                      "GGGGCWWWCWYWCYWRCWWWCWWWCWGGGY"
                      "GNGYWWWBWWYCWWWTYWTCTTWCWWYGGC"
                      "GGGGWWCWWWBKBTTTTOBBWWCWWWGGNG"
                      "GCGGWCTWWCBBTTTTTTTBWCWTWCGGGN"
                      "GGGYCWTWCWKBBTTTWWBGCWWYCWYGGG"
                      "GGGGCRORCCBBBRRTPKBCWWYYYWGCCN"
                      "GGGGCRNRWWKBBRRORBKBTWYWYWGGGN"
                      "CGGYWRROWCGBBNOORBBBWCYYYCOGNY"
                      "GGGCCRORCWBBBRRRRBBBGWYYYWGGGG"
                      "NGGGWRPRWWBKBRRRPGBBTYOWYTGGGG"
                      "GGGOYRRNWWBBBRRROBBBWWYYTWYGGY"
                      "GGGGTRPRWCGBBRRRPCBBRCTYYCGGGG"
                      "GNGGGRRRCOYTBORRPTYTNWYYYYGGCC"
                      "GGGYWRRRGTTOCPRRCTYTCPYOYTYGGC"
                      "NOKNNNNTNOTTNNNTNWTTNNNNNNNNKK"
                      "NNNNNNNNTNNNNNNNNTNONNNNOKTNTT"
                      "NNNNNNKNNNNNNNNNNONNNNTTTNNNNN"
                      "NTNNNTNONNTNNTONNNTNNNNNTTNTON"))
   (make-level :name "Ambassadors" :difficulty 0.3 :slots 3 :seed 58 :par 752
               :order "00000000000112010110000021101000122212222222112111111110001110111111122222222222222222000001"
               :rows '("KGCGKGGGKGGGKGGGKCGGKGGGKNGGKG"
                      "GGGKGYGKGYGKGGGKGGGKGGGKNGGKGG"
                      "YGKGNKKGGYKNYNKGGGBGGGKGGKGGGG"
                      "GGYGTTGGGNGGGKGGYKGGCKGNGTOGGG"
                      "GGGGTTGGKGGGKGGGKGGGKGGGKOYGKG"
                      "GKKWWWKKGGGKGGGKGGGKGGNKONNKKG"
                      "GPOWTWRPGGBBBYOYGWWWGYKKTNOGKG"
                      "GOPWWWPPGKKCBOTOGWWWNKNGNTKKNK"
                      "KPWKKNPPKGBGKYYTKWWWKGBNKKKKGG"
                      "GPPKKKPWGPRRRNRRRNRNNGKKKKKKKG"
                      "NPPKKKPPGORRRRRRORRRPGKGKKKBKG"
                      "GPPKGKPPGKONNNNKNNNNGGKKKKBKNK"
                      "GKKGKKBGKGGGBGGGKGGCKGKKKKKKKG"
                      "GKKKKKKKCGGKBGGOOOOKYGKKKKKBKG"
                      "GKKKKKKBGGKBBBKOROOGWGKKBBKKBG"
                      "GKKKKKKKGNNNBONKNNNNNKKKBKNKKK"
                      "GKGKKKKKKGGGKGCGKGGGKGKKKGKKKG"
                      "GKKKKKKKGGGKGGGKGGGKCGKGKKKKKG"
                      "GKKKKKGKGGKGGGKGGYKGGGKKKKKKKG"
                      "GKKKBKKKGKGGGKCGGBYGGKKKKKKBKK"
                      "KCNWTWNWNWNWNWNWNWWWNWNYNYNWNW"
                      "WKWNWKWNWNWWWWWWYWTTTNWNWNWNYN"
                      "NWNWNWNWWWWTWYOTTWNWNWNCKWNWNW"
                      "WNWNWNTTTYWNWTWOWNWOWNWNWNWKWN"
                      "NTNWNWNWNWNWNWNWNWNWNWNWNWNWNW"
                      "WNWNWNWOWNWNWNWNWNWNWNWNWNWNWN"))
   (make-level :name "Napoleon" :difficulty 0.3 :slots 3 :seed 37 :par 538
               :order "211112221211101101111102122112200001111101111110000000000000000001122222222222222222222000002"
               :rows '("BBBBKKNKBCBBBBCBBBCBBBBBBBKNKB"
                      "KKNKKKKKKBBBBBBGBBBBKKKKKKKBKK"
                      "KKKKKKKKBBBBBCBBBKNKGKKKKBKKKB"
                      "KBBKNKBBTBBBBBOBBKKKKKKKKGBBBB"
                      "KBBBBBBBBBOOKKKKKOOKKKBBBBBBBB"
                      "BBGGBBBBBOBOOYTYYTYYBBBBBBBBBB"
                      "GBGBBGBBOOBOYTTTYYYYYBCCBBBBBB"
                      "BCBKKWCBBOOBOYOYWYTYBBBKBBBBGB"
                      "BBBBWWWBBKOOGCBBYONBBBGBBCBBBB"
                      "GBCBWWWKKKKKCBBBKKBBKBBBBCBKKK"
                      "KBBBKKWKKKNKBBBKBBBBBBBBBKKKKK"
                      "KBBKKKWKNKKKBBBBBBCBBBBBCKKKKK"
                      "KKBBBBWWBBBBBCBBKBBBNBBBBBBKBB"
                      "KKKBGBBWWKBBBBBKBBKGBKBBBBBBBB"
                      "GKNNNNNNYNWWWWWWWYWKONONKNONNN"
                      "KKGKNNTNWTWWWWWYWWWWWNTKNNNNNN"
                      "KBKKKNNNWWWWWWWWWWWTNKKBNNNNON"
                      "KKKKKNNYWWWTWWWWWWWWWNKKKNNKNN"
                      "KBKKBKNNWWWWWTWWWWWWNKKNNOKNNN"
                      "KBKKKGBNWWWWWWWWWWWWCNONNKNNKN"
                      "KKKKKKGNTNWWWWCYWWWNNNTNTNTNOO"
                      "KKKGKKKNNYNNNNWNNNWNNNTNTTTNTN"
                      "KKKKKKKKKNWNNNNNNTWTNKRNRNPNRN"
                      "KKKBKKBKKNWNNNNNNNNWNTPNPNRNRN"
                      "KKBGKKKBKKWNNNNNNNNNWNNNONNNNO"
                      "KKKKBKKKKNKWNNNNNNNKWKKNNNNNNN"))
   (make-level :name "Raft of Medusa" :difficulty 0.15 :slots 3 :seed 4 :par 525
               :order "0112111111111111210111111111222222222200001111222200002000000020000000200000002222222222220011112"
               :rows '("OBKKNKNKNKBKNKKKNKKYYKTYNOYKWY"
                      "KGKNKKKNKKKNKKKNKKKYKYYNYYGOYK"
                      "NKNKKKNNKKNKKKNKKKNGWYNYYKYYKY"
                      "KNKKKNBNKTKKKNKBKKKYYNYYKYOKYY"
                      "NKKKNKKTNTKKOKKKNKGONYYKYRWYYK"
                      "KKKNKNKNOTTNKKKNKKNKYYKYNTYYKY"
                      "KGKKKKNKTTTKKKNKKKNYYKYTNYYBYY"
                      "KOKKKNKTTTTTKNKKKNKYKYNTTYBKYN"
                      "NKKKNKKTTTTTNBKNNKKGYYNTTKYYNY"
                      "KKKKKKTTTTTTTKNNKBKNYTTOTYYNYY"
                      "KKNKKKTTTTYTOKNKKKNNKTTTTYNWYK"
                      "KNKKKTTTTTYTTTKBNNOYYTYYTNYYKY"
                      "CKCGNTTTTTTTTTGGNKTTCTTTTGCKCK"
                      "NGGGYTTTTTTTNWTTWCTTGTTTTGGCKC"
                      "NGGNCKCNNGGGNGYTTKTTGYWWTKGKBG"
                      "GGKNKCKOGNGTTCKTTGTTCWTTTCBCGG"
                      "CKCNWKTTNNCTYKCTTGTTCTTTTKNKGG"
                      "NCTTKCTTKNKTTCKTTCTTKTTTTCNCKC"
                      "CKTTNNTTNNNTTNNTTTTWNTTTTNNNBK"
                      "NNNNNNNNNNNNNNTNNNNNNNNNTNNNKC"
                      "GTNKNNNNNNNNNNNNNNNNNONNNONNBK"
                      "WGGCKCWGGWGGWCNWKGYGCWGGWWGWNG"
                      "GKTKCWCGWGGWCKYKGYGGYGCWCKWGNW"
                      "GWKWWCGTKCWCKWKCYGKWKCWCKWGGWC"
                      "WKCWCKWKCWCKWKCWCKWKCWCGWKCWBK"
                      "KCWCKWKCWCGWKCWGKWKCWGKTKCWCKW"))
   (make-level :name "Falling Rocket" :difficulty 0.3 :slots 3 :seed 8 :par 465
               :order "222222222222112111122222111111111110000000000000000000011111111111110010100212222222220000000022222"
               :rows '("GKKBBBBBBKKKKTYNKYKKWGKKBWBBBB"
                      "BBBBBBBBBBGKKKKYWKRWKBBBWBBBBB"
                      "BBBBBBBBBBKGRKKOGKWBBBBWBBBBBB"
                      "KCBBBBBBBBKKKGKKKOCBBBWBBBBBBB"
                      "GGBBBBBKCKKYBKKGKKRGBWBBGBBBBC"
                      "BBBBBBBBKKOKKKKKOGBYWCBBBWKKGK"
                      "BBBKBGBKKBKOOKKRKTRWBKBBWBCKKK"
                      "BBKKKKKNKKKONNGWKKWOKBBWKKKKGK"
                      "KKGKKNKGKGKKYKWKYYKKKKTGKNKKKN"
                      "KKKKGKKKKKGGKKKKKYWKKWKKKGKKKK"
                      "GKKKKKGKKKKKGGKYWWKKGKKKKKKGKG"
                      "KGKGKKKKGKKKKKKGKKKKKKGKKKKKKG"
                      "BKKGKKKKRNGKKKKKKBBGKKKBNBKKKK"
                      "BKKKKGKKRKWKGKBBYRBBKBKKKKGKKK"
                      "KBKKKKKGWKKKCOBBBBBBBBBKKKKKGK"
                      "BBGKBYKYKOBCBCBBCBCBBBGGRRKKKK"
                      "BBKKGKKGKBOKOBGBBBBOBBBKKYKGKK"
                      "KKKKKKYOBKBBGYBYBBYBBOKKBKKYKK"
                      "KGBGOYCNGKCYBBCBRBCKKKWNKKCKKG"
                      "KCKGNCKNKCGPBCBBBCKKKCYNWCKKKC"
                      "CKKKNRKNCKKKCKKKCKOGCBKNCKGKCK"
                      "CKKCNKKNKKKCKKGGKKKCKGKNKKKCGK"
                      "KKCKNKCNKGCKKKCKGKWKNKCNGKCKKN"
                      "KCKKGCKKNCKGKCBKKCYKKCKKKCKKNG"
                      "CKKKBKYKCNKKCGKKCKGKCKKGCKKGCK"
                      "KGKCKKKGBCBCBKKCKKKCKKGCKKNCBB"))
   (make-level :name "Lily Rose" :difficulty 0 :slots 3 :seed 24 :par 427
               :order "022210211111111111111111112222222222222221222111101100010000000000011100022222200000000020022"
               :rows '("KGGKGGKGGKGGBGCKCGKGGKGGKGGKCG"
                      "GWWGGKGGKGWWGGGGGKGGKGGKGWWWGK"
                      "GYGGKYGKWWYWWKGGKCGWWGKGGYYGKN"
                      "KGWWGGKGYKGYKGOKGGKYGKGGKGGKGY"
                      "GGYGGKGWWGWTGOYOGKGYKWWKGGOCGK"
                      "GNGGKGOYGYYGGYOGKGNKGYKGGOYOCG"
                      "KGGKGOYOGBGGKGGKGWWGGKGGKGRYGG"
                      "GGKNGKOGKGGKGGKGGYGGKGOKGGKYWK"
                      "GKGGGGGKYGKGWWGGKYGKGOYOYKGYKW"
                      "NGGBGGKGGKCGYGGKGGKNGKOGKGGKGY"
                      "CGKGGKGGRGGKGGKRYKGGKGGKRPKGNY"
                      "CKGGKGGCTYWWGKOYOWWWGPKGTKGGKG"
                      "KGGKGGKYYTYGBGGOGYYTYKGGKNPWWG"
                      "GGKYGKGGTTTKGGKGGNTTWGCKGGKYGK"
                      "GKCPKGGKWWWGGKYPKGWWWRKGGKGGKG"
                      "NGGKGGKGWYWGKGGKGGWWWKGGKGGKGG"
                      "GGNGGNGGTWYBGGKNGKWWCGGKOGKGGK"
                      "GKCGKGGKCWWRGKGGKGWWWGKGGNGRGR"
                      "KGCKGGKGYWWGKGGKGGWWWKGGKGGKYG"
                      "GRKYGKGGWWWKRGKGGKWWWGGGGGKYCR"
                      "GKGGKGNWWWWTGKGGGWWWWWKGGKGGKG"
                      "NGGNGCKWWWTWKGCKGTWWWWGGKNGKGG"
                      "NGKGGKGWWWWWGPPGGWWWCWNKGGKGGP"
                      "YKGGKGGKGWKGRKGPKGGWGNKGGKGGKG"
                      "GPGKGGKGYKCGKNGKGGKGNKGGBPGKGG"
                      "GGNGRKCGKGGKGGNGGKGGGGPKGGKGYK"))
   (make-level :name "Moulin Rouge" :difficulty 0.3 :slots 3 :seed 33 :par 529
               :order "0000000000000000000000000000000111111111111111112212222222222222222222222222211111221111111120"
               :rows '("ONGNGNNGGNGGOGGNGYOCGNCGOGGNGG"
                      "GGNGYNGYNGYOCCNGGNCGNGGONCKGGN"
                      "GNGNKNYNGGOGGNNGNGGNGGOGTTGGTG"
                      "NGGNGGNGGOGGNGGNGGNGGOGKKKGNGC"
                      "GGNGGNCGOYCNGGNGGNGGNGGKKKNGGN"
                      "CNGGNGGOGGNGGNGGNGGYNNNKKKGGTK"
                      "NNONKNONNNONTNONONNNNNYKKKOKKN"
                      "NONOKKKNNKKKNOOOOOKKKRNTKOKONO"
                      "OONNRTNNONTNOOKTONNTONNNKKNNON"
                      "NNNOYTTONTTWNNTTTNTTYNNKNNNONO"
                      "NNONKBGNNKKKNNKKKKKKKKKKNNONTN"
                      "NONNGKBNOKKNNOKKKOKKKNNNNONNNO"
                      "ONNNKKKNOKKKONKKKNKKKNTNONNNOT"
                      "NNKOOOOOOOOOOOOYOOOOTTNONNNRNN"
                      "NNONROYOYNOOOOOOOOYONNOKYYYYYN"
                      "NONNOONOOOOOOOYOOONONONNOYYYYO"
                      "ONNNONNNONNNKTNNONNNONNCYYYTYN"
                      "KNNONTTONTKKNNNONNOONNCCWWWCCN"
                      "NNONNNONNKNNNNONNNYNNNCCWWWCCO"
                      "NONNNONKNNNNNONTNRONNOWWKWWWCO"
                      "ONNNONKNNNNOOONNONONONCCTWRCCN"
                      "NNNOKKNONNTONTNONTKONNCCWWWCCN"
                      "TNOKNNONNNONNTONNNONNNOCCGCCNN"
                      "NKGNNOKNNONNTONNNOTNNOONNBNNNO"
                      "KNNNONNNYNNNOKONONKNONNNONNKOT"
                      "NNOONNNONNTOONNONNNONNOONNNONN"))
   (make-level :name "Evening Snow" :difficulty 0.5 :slots 3 :seed 52 :par 411
               :order "1111111122222222222222222222222222222112100000000000111111111111000111110011000000000000000"
               :rows '("KKGKKKKKKWKKKKKKKKKKKKKKKKNTKK"
                      "KBKKCKKKKKKKKKKKKNKKKKKKKKKKKK"
                      "KKCKKKWKCKKKKNNKBWKKKGWKCKKKKK"
                      "KKKKKNWKKCKKKKKKKKKKKKKKGWBKKK"
                      "GKWKWWCWKKKKKKKNKKKKKGKKKKKKKK"
                      "KKKKWWWWCKWKKNKBKKKKKKKWKKKKKK"
                      "KKKCWWWWWWKKGKGKKKKWKKWCWKKKKK"
                      "KCWWWWCWWWWKKKKKKKKKKCWWWCKKKG"
                      "NWWWCCWWWCGWNNNOONNNWWWYWWTNWN"
                      "CWCWWWWCWWWWCWWWWBWWWWCTWBWCYW"
                      "CWWWWCCCWWWKWKWCWYWWCWWWWWWWWW"
                      "WWWCWKWWCGWWCCWWWWCWWWKCWWCWCW"
                      "CCWTWWCWWWWWKWWWCWWWKCWWWWCWWW"
                      "WWWTCWWWWKWWYWCWWWWWWKCWWWYCWC"
                      "WNKWNNNYWNWKNNWWWCWNTNNWNNWWWW"
                      "WNNNNNNWTNWNNNNWWWWNNNNNNNWKCW"
                      "WWYNNNNTCNYTNNWWCWWNOWNWNNWWWW"
                      "WNNWNNNGWWNNNWWTKWWNTNCKNNWWKK"
                      "WNNNNTNWWWNNNNTWWWWNNNNNNNTWWW"
                      "WWWWKYWWWWCWWWYWWCTWWWWWWWYWWY"
                      "WGWWWKWWYTWWWTWTYWWWWYWWWCKWWW"
                      "WWWWCKWWWKWWCKWWWWWWWKYWCWWWWW"
                      "WTWWWKWWKWKWWWKKKWWWWYCCWWKWWW"
                      "WWWWWWWWWWWWWWWWKWWWWWWKWWWWWW"
                      "YWCWBWWWWWKWWWWWWWWKWWWWWWWWWW"
                      "WWCWWWCWWBWWWWTWWWWWWWWWWWWWWW"))
   (make-level :name "Cypresses" :difficulty 0.9 :slots 3 :seed 7 :par 420
               :order "00000000000000000000000000000110011111111111111111111111111112222122222222222222222222222222"
               :rows '("WWWWWWWCCBCCBWWWWWWCCBCCBYWWCC"
                      "WYWWCBCCBCCWWWWWWBCWBCGKGCWWWB"
                      "CCCCBCCBCCBWWBCBBCCGCCNGGWCCBC"
                      "BCCBCCBCCBCCBGCBGWBCCKGCKCCBCG"
                      "CCBCCBCWBCCBGCBCCGCCBGGKGGKGGB"
                      "CBCCBCBBCCBCGBGCBCCKCGKGGBGGKC"
                      "BCCBCCWWWBCCBCCBCCWWWKGGKGGKGC"
                      "CCBCWWYWWWWBCCBCCWWWKCGNGGKGGK"
                      "CBCWWWWWWWBCCBCWWWWWGCBGGKGGNG"
                      "BWWCWWBCCBCCKYWWWWWCGKGGKGGKGG"
                      "WWBCCGCCBCWGWTBCCBCGNGGKGYKGGK"
                      "GKGGGGGNGBGGGBGGGGGGGGKGGKGGKY"
                      "BGGCGGNGBGGGBGGNGCGGGBGCKGGKGG"
                      "GGGGGGGBGGGCGGGGGGGGKGGKGGKGGK"
                      "GGGGGGBCGGGYGGBGGGBKGGKCCKGGKC"
                      "NTYOYNOYYCGGNYYOWYONGKGGBGNKGN"
                      "YYONYOYYOYNOTWOOYNYGKGGKNGNGNK"
                      "YNYYOYYOTYOYYOYNOYYOYONYYOYWON"
                      "RTYOYYNYYOYOONTOYYOYNOYYOYYNYY"
                      "OYOYNYYYYWYNYYOYYONYOYYNYNOYYO"
                      "YONYOOOOYNOYYOYWNYYOYYOKYOYYOY"
                      "NYYRYYOPYOYYOYTOYWOYYNYYOYWONY"
                      "YYOOYTYYOYYOKYOOYOYNOYYNYYOYYO"
                      "YOYNOOYOYWNYYOOYONYOYYOWNOYYNY"
                      "OTYOYYOYNOYTOYYNOYOYTOTYOYYOTN"
                      "YYOYOONYOYWOYNOYYOYWNYYOWYONYO"))
   (make-level :name "Sleeping Gypsy" :difficulty 0.3 :slots 3 :seed 8 :par 516
               :order "2021022222222000020000001000122221111111111122222222222222222111111111111111111000000000000000000"
               :rows '("KBBBBKBBBBBBBBBGBGBBKBBBBNBBBG"
                      "BGBCKGBBBNBBBBBBBBBGKBBBKYBKBK"
                      "BBBWBBBBBKCBBKBBGCKBBBBKWYWKKB"
                      "BKKBBBBKCBKBKBCCWKGCBWKWYYTWBB"
                      "BKKWGBKBBBBBBBBBGBBKBKBBYYTBBB"
                      "KKBGCKBBBBKBBBKKBBGBKBKBBYBBBB"
                      "BBGBKBBBCKGBBKKBGBBKWBBBKBBBKK"
                      "KKBKBBBBKBKBCKNONCKGWBBNBBBBKB"
                      "CBKBBCKKBBGBKNTNNNBBBBKCBBNKBG"
                      "BNBBGNNTBBBKBOKNNNOOOOOOBOKBBB"
                      "BBBTNNTKTNKBBKNNNKOOOOOOOKBBBB"
                      "WNNNNKNNNNNNCCKNTOOOOOOOOOCWCC"
                      "TNNNNNNNNNNNNTKNNNOOOOOORNNCGC"
                      "TTTTTYTTTOTTYTWTTNONOOOOOTYTTT"
                      "YTTTTKTYTTWTTTTTOOTOTYOTRTTTYW"
                      "TTYYWTTTTYTTTWYTYTTYTTTNKOOOOY"
                      "TTYTYTWTWTTTTYTYTWYTTTTWOOONYT"
                      "TTKTTTTTWTTYWOTTWOWTYTTTYOROOT"
                      "TKTKWGBNYWYBRYWGBRYWYBRYTTTTTY"
                      "TTTTGBNOWGBRWWNBPYWGBRYWYOTTTT"
                      "TTTNBRYYGGRYTYBRYWGCPYYNTTYTTO"
                      "YTTBRTWGBRYWGBRTWGBROWGGTTTTYW"
                      "TYYTTTTWYYTWTTTTWTTTTTTYTYTYTT"
                      "TTTTOTTTTYTYTTTTTTWTTTOTOYYWTT"
                      "TTTWYTYTTTTTTYTTTTWTYTWTTTTYWT"
                      "TYTTTTTTYTTTTYTYTTTYYTYWTTYTTT"))
   (make-level :name "Japanese Bridge" :difficulty 0.9 :slots 3 :seed 4 :par 390
               :order "22212222211112111111111111111111111111112222222222222222222220000200000000000000000000000000100"
               :rows '("KGGGGCGYGGGKGGYGNCGGGYKGGGGCYG"
                      "GGGGGYBGNGYGGYGYGKGCYYGCCGGYKG"
                      "YKGGGYGGGGGGKGGGGGGYGGGGNGYYCG"
                      "GGGGWGGKGGGYGGGGGGKGGNCGNYGGCK"
                      "GGKYGCGGGGYGCKGGGYGGGGGGKGYGGN"
                      "GYYGGGCGNYGWWWTWWWWWGGYYGCGGGG"
                      "GYGKGGGGWWWGWGKWNYWGWWWGGKGGCY"
                      "YGGGGGWWGTGGWGYWGGWGKWGWTGCGYG"
                      "CGGGWWTGGWGCCCCYCGCCYWGGWWWYYG"
                      "GGGWGYWGCWWGWCCWCCCCWCCGTGYWGN"
                      "YCWWYBCWCCTBBBBBBBCBWBCCWYNWYW"
                      "CBCWGWCCGBBCCBCCGCCBBBBCCWBWBW"
                      "WBCWCWGBCBCCBCCBCCBBGBCBBWCCCC"
                      "CCCBBBCCBCCBWCBCCCCWBCCCCKBWGC"
                      "WCCBBGCYCYBCCCCCCCWBGGBYWGCBCC"
                      "WBKGYCGCGBCGBCGKBCBCCBCCYCGBBB"
                      "BGBGCGGCBCCGCCBWGBCCBCBBCYBGGG"
                      "GBNGBCWCCCBCCKCCBBCBCWBCCBCCGC"
                      "BGPGCCBCGPGGBCCBCCKCCGGCBWCBCG"
                      "PGBGPGCGPGCKCCBCWBCCBCWBBCYWGB"
                      "GPGCCGYGWCKCCKBCGYGBCBBGCYCCBC"
                      "KCCBCCBCCBCCBCCBGCBGCBCGPGCBCC"
                      "CWBCCKCCBBCCWGBCCBCCBCCGPGGYGB"
                      "NPGCBGGWGCBGPGCCGBCBGWGCCKCCBC"
                      "WGPGGCCCCBGCBGCBCYYCCGYGYGCBWC"
                      "CCBGPGCCBBCBCBBCCBCCBGCBCCBGCB"))
   (make-level :name "Haystacks" :difficulty 0.15 :slots 3 :seed 28 :par 444
               :order "202001110001000000000220211222222222220000022220001111100000002111121122111111111112222112221"
               :rows '("YYCYCYWYCYCYWYWWCYWYCYCTWOCYCY"
                      "YCYCYWYCYGYWYCYCYWYCYCYWTCYGYW"
                      "WWCYWYGYWYWYCYCYWWCYCYWYCYCYWY"
                      "WCYWYCYGYTYCYCYWYCYCYWYCTCYWOC"
                      "CYWYCYCYWWCWGYWYCYCYWYCYBYWYCY"
                      "YWYWYGYWYCYCYWYCYCOWYCOWWWYCYC"
                      "WYCYBYYTCYWYWYCYCYWYCYWYWYCYBY"
                      "YCYCYTYCYCYWOCYCWWYCYCYCYCYWYW"
                      "GBBBBBBBBKBGBBBBBBBBKBBBBGBBCB"
                      "GBGBGBGBGBGBGBGBGBGBGBCBGBCGGK"
                      "GGGKGKGKOKGBGKNBGKGKGRGKGKGKGN"
                      "GNGGCOORONOOGGCGYGYOOONOGGCCGN"
                      "WYOWOOONNNNNOYYYYYOONOTNNYYWYY"
                      "YTPYOONNNNNNNYWYYOOONNNNNKPYYP"
                      "YWYOOONNNNNNNKYYWYOONNNNNWOYWY"
                      "OYYPOONNNONNNYYPOYOOKNNNNYYPYY"
                      "TYWYOONNNNNNNTWYYWOONNNKOYCYOW"
                      "YPYYOONNNNNNNPYYPYOONNNNNPYYPY"
                      "WYYWOYNOKNNNNYYWYYOONNNONYYWWY"
                      "YYPYYONNNKNNNYPYYPYOOKNNNYPOYO"
                      "OWYYNONNNTONNWYOWOWWGBBBCKBBKY"
                      "PYYPYYBBBBBBBBBBBTPYBBBKBBBGBO"
                      "YYTYOWBBBBBBKBBBBWYYWYYWYYWYYT"
                      "TPYYRYYRYOWYYPYYPYYOYYPYYPOOPY"
                      "WYYWYYYYYWWTWYYYTYYYYWYWWYYYOY"
                      "YYRYYOYYRYYPTYPWYWYYPYYPYYPYYP"))
   (make-level :name "Boating Party" :difficulty 0.9 :slots 3 :seed 22 :par 527
               :order "0222222222222220002022000000222220000000000000000000022222222211111111111111111111111111111101"
               :rows '("PWWNWWRWWPWTPWWRWWRWYRWWRYWPYW"
                      "NWWPWWRWTOWWPWTRCWRWWPCWRWWRCW"
                      "PWCRWWOWWRWWPWWRWWNWTRWWRWWNWW"
                      "RWYRWWRWWRWWPWWRCTPWYRWTRWWRWW"
                      "WWCGGCGGCGGCYGCGGCYGCGGGGGCGGC"
                      "WWGGCYGCNGKKGCGGCGGBGGCBBCGGCG"
                      "WYYCGGCYGCTTCNORGGCGGCNTTGGCNG"
                      "WTTYYWYGCGBBGYYTGWGYYCGWWGCGGC"
                      "WWCGCTWCGGBGGGWYCCGTTGCWWGWYCG"
                      "WCWCGNGGGCBBCGWWCGGKKCGWWGTTGG"
                      "NWWTNKKNONBCNNYWNNTKKNNCWNKKNN"
                      "NWWKNKKNNTCCNNWWKNNKKONWWNKBNN"
                      "KTWNNBKNNNBBNNWWNKNKKPPYWNKKTK"
                      "NWWONKKNNNBBNNWWNNNKKTTWWNNKNN"
                      "NWTNNBKNNOBBTNWCTNNKKWCYTNKKKN"
                      "OWWNNKKNNRNNNYWWONNBKWWNNNKKNN"
                      "NNNNNKKPNONTNYNNONNKKWWNONKKNN"
                      "NNNWTWYPWWWWWWWWWCWWNTWNNNKKNN"
                      "NNNWWWWWWWWWWCWWWWWWKWWTNTNNNN"
                      "NNOWWCWWWWWWWWWWWWWWNWWNKNNNNN"
                      "TKKBWWWWWYWYWWTWWWWWNWWNNKNNNN"
                      "NKNKWWWTWTTWWWWWTTWWNWWNNTNNNN"
                      "NGKKNNNNNKTNNNNNNNNNNNONNNNNNN"
                      "NNNKNNNKTNNNNNNONNKNKNNNNONNNN"
                      "NNNNNNNTNNNNNTNONNNNNNNKNONNKN"
                      "NNTTONNONNNKKNONNNONNNNNNNNONN"))
   (make-level :name "Folies Bergere" :difficulty 0.15 :slots 3 :seed 6 :par 655
               :order "22220000001100000000000120010000000110221111112211022222221122211111111222221221112122100"
               :rows '("TYYOKKNYTKKYTTOWNKTNTTBKOYOTNK"
                      "YNTTTWYYNNOTRNWYWKYKTKKTTNNGYN"
                      "TNKYWYWKTTYTBOYWTONTYKYWKWTYKN"
                      "NTOTNTONTWYTKKYYNNYTKTWYWTTNYT"
                      "TTKOYWNTTTOONTOWTYKKNWYWTOTNKY"
                      "KTOTYNYNOONOYYWTTOTTOKYOYOOYTT"
                      "TTOOONOKKNOYOTKTKTKNTNTKTNTKOY"
                      "OTYNOYTKTTTYNYTTTKOOKTOTYTYTKO"
                      "TNYOONTOOTNOKWWWWWKKKTTTGOYOTN"
                      "OYKTTWNTTTKKKWWWWWKNNTOOKTNYON"
                      "TYNYKYTNYNTYKKKPPKBOYKOYNOOYTO"
                      "KNTTTOYTKNKKKKKKKGNKOTTKKYWYTK"
                      "NKYKYYYKOBNKKGGKKGKKKKNYOYOYOO"
                      "YTBTNWRYOOYKKKKKKGKKKKYGKGYKKT"
                      "NTBORYRYTOKKKKKKKKBGGNRGNKTKTO"
                      "TOCYROROWNBKKKKKKKKKKYKGNKONWN"
                      "OOBYRTRTNOGKKKKKKNKKKYNGNKTKTT"
                      "WWWTWWWTOOOTWWWTWWWTWWYTWYWYWW"
                      "WWTWWWTCBCCCWWTWWWTWWWTWWWTWWW"
                      "CTWWWOTWCCCWWTWWYTCWWTWWTTCWWT"
                      "TWCWTWWWTWWYTWWWTWWWTWWWTYTCTW"
                      "WWTTCWWTWWWTYWWTWWWTWWWYWWWTWW"
                      "WWTCWCTWWWTWCWTWWYTWWWTWWWTWWW"
                      "WTWWWTWWWTWWWWWWWTWWCTTWWTWWCT"
                      "TWWWTCWCTWWWTWWWTWWWTWWWTWWWTW"
                      "WWWTWWWTWWYTWWWTWWWTYWWWWWWTWW"))
   (make-level :name "Third of May" :difficulty 0.9 :slots 3 :seed 5 :par 464
               :order "1122200012222222200222220000000000000010010000100000001111222221211111122222221111111111122111"
               :rows '("BKKKKBKKKGKGKKKBKKBKGKKKKBKKKB"
                      "KKKKGKKBKBKKKKBKKGKBKKGKBKKKNK"
                      "KKKTKKKKBKKGKBKKKKBKKKKBKKKKBG"
                      "KKBTKKBKKKKKBKNKKBKKKKBKKKKBGK"
                      "KBTTTKBKKNKBKKKKBBKGKBKKKBBKKK"
                      "BKWWTBKKKKBKKNKBKKKKBKKKKBKKNK"
                      "KKOTTKKTTTTTKKBKNKKBGKKKBKKKKC"
                      "KKTTWKKTTOTTWCKKKKBKKKKBKKGKBK"
                      "KKTTTWKTTYTWGKKKKGKBKGBKKKGCKB"
                      "NNNKNNYNNNWNNKNNNNKNNNNKNTONKN"
                      "KNNNNKWNNNCNNTNKNGKNKKKKBKKKNB"
                      "NNKNNONWWWNNKNNNNBBNNOKBBNNNNB"
                      "TNNNKNNWWYNNNNKNNBBKNTNBBNNNOB"
                      "KKNTNNBWWWNKTWWWYWWWWWWWWWWTWW"
                      "NTNKNNNWWWNNNKNNNBBNNNNBGNNNKB"
                      "NNTNNTNYYYKNNKNNNGBNNNNBBKNNNB"
                      "NNKNNNNYYYNNKNNNNBBNONKCCNNNNB"
                      "NNNOKNNYYYKNNNKNNCBNNNNBCKNNNB"
                      "PORRRRKYYYTKTNYNKBBNOKNBBONNNB"
                      "RRRRRRNNKNNNWWWWWBBNNONKBNNNKB"
                      "RRRNRRNNNNKYWWWCWBGNNONBBKNONB"
                      "RRNRPROBNNTYWCWYWBBNNKKBCNTNTB"
                      "NONTGKNNNKNWWWWWWBGKNNNBBNNTNB"
                      "NKNNNNKNNNNKYTYYYBBNNNNBBKNNNB"
                      "NNNKNNKNKNNONKYNNBCNNNNBBNNNNC"
                      "KNNNNKTKNNNNNNNKNBBNNKNBBKNNNB"))
   (make-level :name "Monk by the Sea" :difficulty 0.9 :slots 3 :seed 28 :par 447
               :order "11121222222220002000000000000000000000000000010111222222222222112222222122111111111111111111111"
               :rows '("KBBKBKKBBGBBKBBKKBKCBKBBKCBKBK"
                      "BBNBKKBBNBGKGBKBCKBBKKBKCKKBGK"
                      "KKBBKKCKBBKBKKBBKBGNBBKBBKBBKB"
                      "NBBKBBKBCKBBKBCKBBKBBNBBKBGKBB"
                      "CCBCCBCCBCCBGCBCCBCCBCCBCCBCCB"
                      "WBWCBCCBCGBCCBCCBCCBCCBWCCCCBG"
                      "BCBKCCBCCBCCBCBBWCBCCCBCBCCBCC"
                      "CCKGCBCCBCCBCCBCCBBCBCGBCCKCCB"
                      "CKCBWCCWCWBCCGCCBCCWCCKCCKCWBC"
                      "WCWWWCWTWBWWCCWWWCWWWWWWWCWWWC"
                      "WWCWWWGWCYGWWTCCWWCWWWCCTWCWYW"
                      "WWWCWWWGWWWBWWWCWTWCWCWGWWWCWW"
                      "CTWWCWWYCWWWCWWWCYWWCWWWCWWWWW"
                      "YCWYWCWWWCCWWWWWWCWYWCWWWCWWWC"
                      "KKBKBKBKNOBKKKBKKKCKKNBNKKBKGK"
                      "GBKNKBGKKTKGKGKNKBGGKBKNGKKGKB"
                      "BKKKBKKKBKKGBKKKBNKGBKKKKKKKBG"
                      "KKOYTTTTTKTTTTTBNKKBKKKBKKKBKK"
                      "WTTWTTWTTKTOTTOWTOWTOWTTWTTWTT"
                      "TWWTTWTTWKTWTTWTTYTTWTTWTYWTOW"
                      "TWTTWTTWTYWTTWYTWTTWOTWOYWTTWT"
                      "WTTWTTWTWWYTWTTWTTWTTYTWWTTWTO"
                      "TTWTWYTTWTTYTTYTTWTTYTTYTTYTTY"
                      "TYTTYYTYTTYTTYTTYTWTTTYTTYTWYT"
                      "YYTYTTYTTYYTYTWYTTYTTYTTYTYYTY"
                      "TOTWTYTTYTOYTTYTTYYTOTTYTTYTTY"))
   (make-level :name "Astronomer" :difficulty 0.9 :slots 3 :seed 1 :par 522
               :order "1222000000000000000000000000000222200222222112222222220222121111111111101111111111111122221"
               :rows '("CCCCCCCCKNNNNNNNKONNKNNNKNNNKN"
                      "CWWCWWGTNNNKNNNGNNNKNNNBNNNBNO"
                      "CWWCWWCWNONNNNBNNNKTNKKKKKKKKN"
                      "BCCCCCWGNKNNNKNNNKNNTKTTTTOTKK"
                      "CWTBWWCTKNNNBNNNKNNNKKTTTTTTKN"
                      "CCWCWWCTNNNKONKKNNNKNKTOTTTTKN"
                      "CCCCCCCCTNKNNYKKNNKNNKTTTTTTKN"
                      "CWWCWWCWNKNNTTTNNKNNNKOTTWTWKK"
                      "CWWGWWCWKNNBBBBBBBNNKKKKNKGKKN"
                      "CCCCCCYOYNOBBBBKBBKNNNNBNKNKNN"
                      "CCWBWYNYYYCBBBBBBBKTNNKNONKNNN"
                      "GWWCYYYNYYCCBBKBBBNKNKNTNKNNTK"
                      "CCCCCYYYYYNBBBBBBBKNKNNNKNNNKN"
                      "CWWCWWYWYNNBBBBBCBNKNNNKNNNKNN"
                      "RGBRGRBGRRGRRBBBBCKNKKKNNNKNNN"
                      "GRRGRRGBRGNBGBKBKBNNOKNOTKNNNK"
                      "CRGNBYRRGRRYBBBBBBNNKNNKKNNNKN"
                      "OGRRGGRGRBGRRKBBBGNKNNNKNNNGNN"
                      "GPBGRRGNRGBRGNBBBBKNNNKNNNKNNN"
                      "RRGBRYRBGRRGPKBBBGNNNGNNKKNNNK"
                      "BCRRGRNNBRGRBNBBBBNNKTKNKNKOKN"
                      "GGRGRBYRRGRRGNBCBBNKNNNKNNNNNN"
                      "RRNRRGBRGOBCRKBGBBKNNNKNNNBNNN"
                      "PGRBGRRGRRGBPKBKBBNNTKNNKKNNNG"
                      "GRRGBNGRBGRRGTBCBBONBNNTKNNNNN"
                      "RKGNRCRRGBRGRNBKBBNKONNBKNNKNN"))
   (make-level :name "Lady with Ermine" :difficulty 0.5 :slots 3 :seed 59 :par 539
               :order "11111111111111111111111112011000100010101200000220000002222222222222000000000001222222222222220"
               :rows '("NNKKKKKKKKKGKKKKKKKKKKKKBKKKKK"
                      "KKKKGKKKKKKNBKKNKBKGKBNKNGKKKK"
                      "NKKKKKBGKKBKKKKNNKKKKKKKKKNKKK"
                      "KGKGKKKNGKKKKNNNNNKKKGKGKKKKBK"
                      "KKKBBKKGKKKGNNNNNNNKKKKKKBKKKK"
                      "NGKKKKKGKKGBNNTTTTNKKKKKKKKGKK"
                      "KKGKKGKKKKKKNTTTTTTKKKKKKKKGKK"
                      "KKNKGKKKKKKNKYBTTKOKKKKKKKKKKK"
                      "KKKKKKKKKKKBNTTTYTTKKKKKKKKKKK"
                      "KKKNKKKKKKKKTTTTYTWKKKKKKKKKKK"
                      "KKKKKKKKKKKKNTTONOTKKKKKKKKKKB"
                      "GKKBKKKKKKKKKNTTTTBKKKKKKKKKKG"
                      "KKKKKKKKKKKKKKTTTOKKKNGKKKKKKK"
                      "KNKKKKKKKKKNKKTTTTKKKKKKKKKKKN"
                      "KKKKNGKKKKNKNKNKNKNKNKKKKKKGKG"
                      "KKKKKKKKKKRWNTTTTTTBBCKKKKKKNK"
                      "KKKKKKKKKBKWWTWWTYTBCBKKKKKKKK"
                      "KKKKKKKKKORWWWTTOTTCBCCKKNKKNK"
                      "KKKKKKGKKROYYWORBBCBBCBKKKKKKK"
                      "NKKKKKKKROYWWWWRBCBBCBCBKKKNKK"
                      "KKKKKBBKORYCWWTRCBCCBBCBKKKKKG"
                      "KKGNKKKORRWWWWWOBKCBBCBBCKKKKB"
                      "KKKKKBBRRROWWTNRBCBBBBBCBKKKKK"
                      "KKKKKKRRRORTWWTRBBBBBBCBBCKGKK"
                      "KNKKBNRRORRPORRRKBCBBCBBCCKKKK"
                      "KKKKKNPORNPYRRROBCBBCKBCBBGKKK"))
   (make-level :name "Harvesters" :difficulty 0.9 :slots 3 :seed 3 :par 548
               :order "000000000000000000000000110000011110111111111111111111111111011202222222222222222222222222222222"
               :rows '("CWWCWWCWWCWWWWGGYWCWTCWWCWWCWW"
                      "WWCWTCWWCWWCGGGNGCWWCWWCWWCWCC"
                      "WCWWCWWCWWCYCGYGYYWCYWCWWCWWCT"
                      "CWWCWWCWWBGGGGGYYYNWWCWWBWWCWW"
                      "GGNYGYYNYNCNYGGGGGGGCGCCCBCCCC"
                      "YGGGGYGGGGNGGGGGGGGCCCCCWCWCCC"
                      "NGYGCGCTGNGGCCGYGGGGGCCGGGGCCG"
                      "GGGYYGNCGWGGNGGGGCYNYCNGBNGGNC"
                      "YYGTGNYGNGGYCNGGTGGWGCTNGTBGTG"
                      "GGGTGGGTYGGGGNNGGGWGGGGCGGGGBG"
                      "TYOWYOOTWYOYYNOYYYTTOTTYYYOYYT"
                      "WOWWWOYWYOYYYNKTYOTTTTTYTYYYYO"
                      "OYOTOYYWOTYWONNYOYWTTYTTOYTYOY"
                      "YYYOOYYWYYTOYNNOYYYWTTTOYYWOYY"
                      "YYOYYYOYYOOYYNNYWYOTTTTTTYWYYY"
                      "YYYYYRYTYOYYYTNYYNYTTTTTTYWYYO"
                      "OYYOOYYTOYYYONNYOYWYTTYTTTYYOY"
                      "YTYOYOYOYYYOYNNOYYYOTTTTOTYOYY"
                      "YYNYYWOYTTOYYNNYWWOYTTTTWTTYYY"
                      "YOYYOOTOWWYTTNKTTNYTYWTTTWTYTO"
                      "OYYWTTOYWWYRRNNWWYYYYTTOWTTTOT"
                      "YYYNBBYOYWYRRNNWWWYNYTTTTTTTWY"
                      "TWOYBBOYYYOPOWOWWWOYYYTYTTTTTY"
                      "YOYYBBYOOOYYYYYYOOYYYOTTTTTTTO"
                      "OYYYOYYYOYYYOWOYOTYYRYTTTWTTTW"
                      "OYYOYOYOYOYNYYOOYYYOYYWTWTTTTT"))
   (make-level :name "Sainte-Victoire" :difficulty 0.3 :slots 3 :seed 3 :par 407
               :order "1122122211111222212211111211011011222211111112222211100222000222222000000000000000000000000"
               :rows '("NNWGWGCCGCGGWGCGGCGCGGCGYGGCNK"
                      "YCNNCGBGCCBWGCGWCCCGCNCWWNNNGC"
                      "CCWCNNNCCCWCCGWCCBWWCCWKTWWWCW"
                      "CWCCCWWNNWCCBWCCCTCCCWBCCTCCCW"
                      "WCGCYCCWWBCBWBCCCGBCRCCWCCCCWC"
                      "CCCWCCCTCCCYCCGWPBBPBBRWCCCWCC"
                      "CCWCBCWBCCWBCGBPBBPBGPBBPWCCCW"
                      "CWWCCWCCCYCCBBPBBPKBPBBPBBPCBW"
                      "WCCCWCCCTCPBBPBBPBBPBBPKBPKBPC"
                      "CCCWCCWWCCKBWBBPBBPKBPBBRBBPBB"
                      "CBYCBCWGCCBPBBPBBWBBPBBPBBPBGP"
                      "CWCCCCCCBBPBBPBBPBBWBBWBGRBBPB"
                      "NNTKGGGNNGYGYYYWOONOGGGGKNNOYG"
                      "NONTOWTTTTTWTWTTOOROGGCGNTNTGG"
                      "NONNTGTWTCTGTYTYOOROGGGGNNTNYG"
                      "NGGGGGWWWYYYOYOOGGGGRRRNYGGGGG"
                      "GNGCGGGGYYYYOYORNGGGWTWNCGGGGC"
                      "GYCGNGGGYTYOOOOOYGGGWWCOGGCGGG"
                      "YGGCYYYWRONNRORGNONNGGGGGYNGYY"
                      "GGNGYYYYOOOOWWWGNONNGGGGGGGGYY"
                      "NGGGYYYOOOOOWCYCTNKNGGGGGOROYY"
                      "YYYYOOONGNNGNNNTGGGCCNYGYWWCOY"
                      "OYYYOYOOGGCGNNNNGGGGCGGGYWWWYO"
                      "YYYYOOOOGGNGNNNNGGNGYNGNYTYYYO"
                      "OROYGGNGNONOGGGGGGGGTYYYYOOYGG"
                      "OOORYGGCNNNKGGGNGYGGYYYYOONOGN"))
   (make-level :name "Yellow Christ" :difficulty 0.9 :slots 3 :seed 26 :par 475
               :order "11111111112211112222222222222220000000002222222222220000000000000000000001111111111211111110"
               :rows '("YGYYGGYCGYCGYGNNYGYGGYGYYGGYGG"
                      "GYYGGYGGYGGYGGNNGYGGTGYYGGYGGY"
                      "GYCGYGGYCNYGGYYOYGGYGGYGGYGYYG"
                      "YGGYGGYGGYGCYCROOGYNGONGTGGRGG"
                      "GGYNGONNNNNNNNYONNNNNNNOGGRRRY"
                      "YYRRRYNNYYYYYYTYYYYYYYNNYTRRRY"
                      "YYPRRYYOYYYYOOYYYOYYWYOYYRRORY"
                      "YYRRRYYOYYYYOYYTYOYWWROYRRRNYY"
                      "YOYNYTOYYYYOYYYYYYYYRRRYRRRNYY"
                      "YOWNYYOYOYYRYYYYOYYYRRRYRPRNYY"
                      "OYGNYOGYYWGYYWWWWYGORRGYYNGYWY"
                      "OGYYYGYOYCOYYWCWWGYYOGYYYYYYYG"
                      "GYYWYYYYNOYYGWWWWYYNGNYYCNYYGO"
                      "YYYNOYYGWNYCYYYYYYYGONYGOYYGYO"
                      "YYNOYWGYOYGYYOYYYYGYYYGOYWGYYY"
                      "WYYOTYYYOYOYYOYWYWOYYYYNYYTYYY"
                      "YYOYYYWWOYYYOYYYYOYYYYOYWWYOWY"
                      "YYOYYWWWWYYYYYOYYOYWWYOWWTWYYY"
                      "YOYYYKTWKYYOYYNNOYWWWWYKTYKWYY"
                      "YOTYYKKKKTYOYYNTOYKTWKYKKNKOYY"
                      "NTYYYKBKKTOYYYNNYOKKKGYKNKKYOY"
                      "OTWYYKKKKYRTYYNNYYKKKKYKKKBYYY"
                      "YYYYRKKKKOYYYYNYYYNKKKYKKKKYYO"
                      "WYYYOKKKKNYYYOOYWYKGGKYKKKKYYO"
                      "YTYOTKKKKYYYYNYYYYKKGKYKKKBYNY"
                      "YYWOYKKBKYYWYOOYYYGKBKYKKNKYOY"))
   (make-level :name "Girls on Bridge" :difficulty 0.9 :slots 3 :seed 1 :par 519
               :order "22010000001100000010000000000000000022220221111111111111111111111111112222222222222222222222"
               :rows '("WCCWCWWWCWCCCCCWCCWCCWCCCCCWCC"
                      "CCWCCWCCWCCWCGWCCWCBTRCYCCCCCW"
                      "CWCGWCCWCCWCCWCCWCCPRRNRCWWGYC"
                      "CCCCCBWCWWCCWWCWCRRRORRRORGGGC"
                      "CCWCCWCCWCCWCCWCCWWWWTTWWGKGNK"
                      "CWCCWCCWCCWCCWCGTCWWWTTYWKGGKN"
                      "BKKBYYBKYYKNYYKKKKWTYCWWKGNGGG"
                      "KKBGWWKKRRKBGGBKKBWWCYWYWGKCGK"
                      "KBKKWWGBORBKGGKKBKWWWWWWCKYGKG"
                      "WTWWWWWWRRWWGGWWWWWOYYYOYYYBGK"
                      "YYOOWWOYRROTGGOWYWWYYYOYKKBGKB"
                      "TOYYWWYORRYTGGOYWTYYYOBKKBKKBK"
                      "OWOYWWYYRRYYCGYWTYYYKBKKBKKGKK"
                      "YTYOWWYOPRYOGGWTYYKKBKKBKKGGGB"
                      "YTNWYYOYYYOYWTTOBNBBKKBKNCGGGG"
                      "YOYOYOYYYOYYTNKBKKBKKBKKBGCGGN"
                      "OOYYOYYYNYWWKKBNKKKKBKGBKGGGGN"
                      "YYYOWYOOYWTKKCKGKGKBKKBKKBGGGK"
                      "YYOYOYOYWTKKBKKBKKCKKBKBBKKGKK"
                      "YOYYYOKWTKKBKKKKKBKNBKKBKKBKKB"
                      "RYYYBKWTKKBKKBKKBKKBKKBKKBKKBN"
                      "YONBWTTKKGKKBKKBKKBKKBNKBKKBKK"
                      "KKBWTBKBBKKBKKBGKBKGKKKBGKBKKB"
                      "KBWTKKKBBKBKKBKKBKKCBKBKKBKKBK"
                      "BTTBKKBKKBBGBKNBKKBKKBKKBKKBKK"
                      "CTBKKBBKBKKBKKBGKKNKBBNBKGBKKG"))
   (make-level :name "Red Blue Yellow" :difficulty 0.9 :slots 3 :seed 1 :par 445
               :order "112211211112222222220000022201111112222222222222220001110100000011110000000000000111111110"
               :rows '("CYWWYWYCKRRRORRRRPRRRRRRRRBROR"
                      "WWWWTWWWKRRORRRRNORRRRNRRRKRPR"
                      "TWWWCWTWKPRRNRNRRNNRRRORRRKRRP"
                      "WWWWWWWWKRRRRRRNRRRRRPRORRKRPR"
                      "WWTWWWWTKRPNRORORNORRRRRRRKRRR"
                      "WWWWCCWWBPRRRRRPRORRRRRRRRKRRR"
                      "WWCWWWWCKRRRRRORNRRRRRNRORKRRN"
                      "KKNBGGKKKROPRORNRRRRRORRORGRRR"
                      "WWCWCTWWKRRRRRRRROORRRRRRRKRRR"
                      "WWWWWCWWKRRRRPRNRRRRRORRRRGRRR"
                      "WWWWWWWCKOPRNNRRRRRRORORPRKRPR"
                      "WWWCWWWWNNORRRRRRRORRORRRPKNRR"
                      "WCYTWWWWKRNPRRRRRRNRRRRPROKRRR"
                      "WWWWCWWWKNRPRRRPRRRORRRRRNNRRR"
                      "YYWWWWTCKRRRRRRRRRRRRPRRRPBRRR"
                      "WTTWWWWTBRRRRRNRRNRRRRORRNKRRN"
                      "WWWWWWWWKRRPRRRRRRRORRRORRGRRR"
                      "CWWWWYWCBRNRRRRRRRRRRRRORNKRRR"
                      "KKKBNGKKKKKKKBNKKKKKKKKNKKKKKB"
                      "GBBBBBBKGWWWWWWWWWWWWWTWCWKWTW"
                      "BBBKBBBBKTWCWWWYTWWWWTWWWWKWYW"
                      "CBBBBBBBKWWWWWWWWYWWWWWWWWKWCW"
                      "BBCBKCBBKWWWWWWWWWWTYWWWWTKKKK"
                      "BBGBKBKBKWWWWWWWYWWYTWWCWWKYYY"
                      "GBKBCBBBKWWWWWCWWWWWYWWTWYKYYO"
                      "BGCBBBKBKWWWWWWCWCWYWWWWYWKYYY"))
   (make-level :name "The Sower" :difficulty 0.9 :slots 3 :seed 49 :par 712
               :order "00000000000000000000100000000001011111212222111111111111111111111112222222222212222222222222220"
               :rows '("GYTYNYYOGYYYGYYYCWWYYYOYGTTYGY"
                      "YYYGYYYGYYYGYOYGOYYGWYYGOYYGYY"
                      "TYGYYTGYYYGOYYGYYYGYYYGYYYNYYY"
                      "YGYTYGYYYGYYYYYYYTYYWGYTYYYYYG"
                      "GYYYGWYYGWOTYROONOYYGWOOGYOYNY"
                      "YWYGYYYCOYOTOOOTOOOYYYYGYYYGYY"
                      "YYGYYYGYYOYOOYYYYYOOYYGYYYGYWW"
                      "TGOYYGYYYGYOOYYYYYORYCYYYGYTYG"
                      "YYWYGYYYGYYOYYYYYYYOYYYOGYYYGO"
                      "OOOOOOOOYROOOONOOYOOOOOOOOOROY"
                      "GYYYGYYYGYYYYYYYYYTTYYYYGYYYGY"
                      "YBBPWBKWYCOOYOOYOOOYYBBWCBCPOC"
                      "BPOCBPOYYYOWYOOOORYYBPRCBPOBBP"
                      "OBBPOBKBBBBPYYYYYYYPOBCPOGKCOB"
                      "BCOBBOCBBBBCBPOYCPOBBWOGBPCBBP"
                      "CKBWOGBBBBCBBBWCOBBPCBBPOWBPOG"
                      "BPOBGPOBBBNBBTCCBTOCBPOKWPOBBC"
                      "YCBCOCBBBBBOOCBPOWCPOBBCOBGPCC"
                      "BPCBBPOKBBOBBPOBBCOGBWCBBOOCBP"
                      "OCBPOBCBOBGCOBBPCBBPOCBPOBCPWG"
                      "CPOBBCYBBPBBBPOCBPOKCPNBBCOBBP"
                      "OKBPCBKWRCBPOBCPOCBBWBWRCBBWOC"
                      "BPOCBPBBCPOWBBOBBPGBCPOGBWOBGY"
                      "NBCPOBBCOBBPBBBROWCPOBCPRKBCWB"
                      "BWOBBKCBBPOCBPOBGOOBBCOBWPCBBP"
                      "CKBPOGBOOBCPOBBWOYWPCBBPOCBPOB"))
   (make-level :name "Whistler's Mother" :difficulty 0.9 :slots 3 :seed 6 :par 783
               :order "00010000101212210000000000000010111111111100000011101112221111111222222222222222222222221"
               :rows '("NKKNKKKYTTWTTTTWTTTTCTTOTWTTTT"
                      "KWKYKWKTTWWOOTTTOTTTTTTTWTTTTC"
                      "KKKKKKKTWOTYOWTTTTWTWTTWTTTTWT"
                      "KKKKBKKWTTTWKKKKKKKKKTWTTWTTTT"
                      "GWNWKWKTOOTTKYWWCWWWKWTTTYWYYO"
                      "KNKKKKKTTTCTKWWWWWWWKTTTTWTTYT"
                      "KKKKNKKTTWTTKWNNNNNWKTTTWTTTTW"
                      "KYBWKYKTWTTYKWNNOKNWKTTWTTWTWT"
                      "KKGKKKKWYWTOKWWWWWWWKTWTTTTWTW"
                      "KKKNKBKOTOTWKKKKKKKYWWTYTTWTTT"
                      "KWGWKWKTWTWTTTTCWTTTWWTTTWTWTT"
                      "KKKKKBKTTYTTTTWWTTTTWWWTYTTYTW"
                      "KNKKKBKWWTYWTWTTKKKKKKKKWTTTWT"
                      "BWKYBWGWTTTTCTTOKKKKKKKKNNNWTT"
                      "NKKKKKKTTTTWTTTKKKKKKKKKNNNTTO"
                      "KKGKGNKTTTWTTTTWWWGKKKKNNNNTTT"
                      "NWKWBWKTTWWTTOWWWWKKKKKKNNNTTW"
                      "KKKKKKKWWTTTTWKKKBKKKKKGONNTWT"
                      "KNKKKKKWTTOTWKKNKKKKKGBKNNOWTT"
                      "KYGWKYKTTTTWTKKNKKKKGKGKTNNTWT"
                      "KKKKKKKNNNNNKKKKKKKKKNKKNNNTNN"
                      "KKKBKGKNNNNNKKKKKKKKKKKKKNNNON"
                      "NNNNNONNKNONTNNNNNNNNNNNNNNNNN"
                      "NNNKNNNNNNNNNKKNNNNNNNNNNNNNON"
                      "NNNNKNNNNTNKONNNNONNNNNTNOONTN"
                      "KNNNNNNNNNNNTNNNNNNNNNNNONNNNN"))
   (make-level :name "Madame X" :difficulty 0.9 :slots 3 :seed 41 :par 652
               :order "111111111111111111111111111111110222222222220000002000000000000000000000000022222222222222222222"
               :rows '("ONNONNONNONNONTONKOONONNYNNONN"
                      "NNONNONKONNONNONTONNONNOONONNO"
                      "NOONONNOTNOOOONNRTNOKNONNONNON"
                      "ONNRNNONNOOOONTNNNONNONNONNONN"
                      "NNONNONNONNOKNWWCONNONKOTNONON"
                      "NONNONNONNONNNWWPNNYNNONNONNON"
                      "ONNONNONNONKONWWWNONNONNONNONN"
                      "ONOONONNONOONWWWNNNNONNOTNONNO"
                      "NNNNONTONNONNWCWONNOOOONNNNNYN"
                      "ONNONNONNONWWWYWWWWNNONTONNOON"
                      "NNNNNONKONNWWWWWTWWWONNONNONNO"
                      "KONNOKNNNNWNNKKKKNNOWNONNONNON"
                      "ONNYNNONONWNKKKKKNONWYNTOKNONT"
                      "NNTNNYNNTWNKKKBKGKNNTWNTNNTNNT"
                      "NTNNTNNTNNTGKKKKGNNTKWTNNTNNTN"
                      "TNNTKNTNNTKKKKKBKKTNNTNNTNNNNN"
                      "ONTONTNNTKKKKKKKKKKOTNNNTNNONN"
                      "NTNNTNNTNKBKKKKKNKKTNNTNKBNNTN"
                      "TNNTNNONOKKKKBKKKKBONTNNBKNTNN"
                      "TNTTNTNNKKGKKKKKKKKNTNNWKKTNNW"
                      "NTNNTNNTKKKKGNNKKKKTNNTNKKNNTO"
                      "ONNTNNOKKKKKKKGGKKKKNTNNKKNTNN"
                      "NNTNNTNKKKKKKKKKKGKKTNNWKKTNNT"
                      "KTNNTNKKGNKKKKKKKKKKNNONKBNNTN"
                      "TNNTNNKKKKKKKKKKNKKKNTKNKKNTNO"
                      "OOTNKKKBKKKKKGKKKKKKKNNTGKTNNW"))
   (make-level :name "Angelus" :difficulty 0.3 :slots 3 :seed 4 :par 464
               :order "11111111111121111101111111101111100000000000000000000000000001022022222222222222222222222222221"
               :rows '("WWCYCCWCCWCGWGCWCCWCCWCBWCCWBW"
                      "CGYCCWCGWCCWWWWCGYCCWCCWWCWCCW"
                      "GWCCWCCCCCWCWWCCWCCWCCTCGWCCWW"
                      "WBCCGCWGCWCCWCCYCCWCCWCCWCCWWC"
                      "CCWCCWCCWCCWCCWCBWCGWCCWCBWCCW"
                      "CCCCYWCWWCWCCCCCWCCWGCWCCWCCWB"
                      "WYYWYYWKYWYYWOYWYYWTYWYOWYYWYT"
                      "YYWYYYBKNYYWTYWYYWNYWYYWYBWYYW"
                      "YWYYWYNNNTCOYWYYWNNNYYWYYBYYWY"
                      "WYYCYYNNNWYYWYYWYBKKYWYOWBYWYY"
                      "YYWYYWNNNYYWTYWOYGKKWYYYBBBYYW"
                      "YWYYWYNNNYWYOTYYWKKKYYWYBBBWWY"
                      "GGGGYGNNNGGCGGGGGKKKGGGCGGYGGG"
                      "TGTTTTKKKTTGTTTTGKKKTGTTTTGOTT"
                      "YTTNTTKKKTYWTGTTBBBBBYTGTTWTGT"
                      "NTTTTCTTNTGTTWTGBBBCBTTTTGTTTY"
                      "TTGTTTONOTOTGYOTBBBBCTGTOTWGTT"
                      "TYYTGTNNNCOTTWYTBGBBKTTKNTNNTC"
                      "TOTTTTOTNTTOTTTTBBBBBOTNNOKNTY"
                      "TTTRTTNNNTYTROOTKBBGBTTOKTTKOT"
                      "OTTWTONNNTOOOOTOBBBBBTYTTOTTTY"
                      "TWOTTTNNNTTOOOTTKBBBBTOOTTOOTT"
                      "TTOTOTNNTOTOYOOTTKKKTOTTOTTWTO"
                      "TOTTOTNTTTTYTTTTOTYWTOTOTTOTTT"
                      "TWTOTTTTOTTTWOTYTTRTTOTRTTTTNT"
                      "OTTTYOTTTTOTTTWOTTWTOTTTTOYTTT"))
   (make-level :name "Waterfall" :difficulty 0.3 :slots 3 :seed 16 :par 515
               :order "11111111111111111010000000000000000000000000000111111111111222222222222222222022222222222"
               :rows '("CWWWWWWWYYWWYWWWWWWTWWWWTWWWWW"
                      "WWTWWWWWWWWWWWWWWWWYWWWWWCWTTW"
                      "NGGGGGKNWCTWWYWWWWTWWWKNOKBNNN"
                      "NGNNNGNNNGCBBWBWCBCWNGNGGCGYNG"
                      "KTNNKNNNKOBKCWBWCBCWGNGGGKYGCN"
                      "NNNGNNNGNNCBGWBWCBCWNONGYGGGKN"
                      "NNNNNNKNNTBWCBCWBCCBNNKNONKNNO"
                      "GGYGNCNTNGBWCBCYBWCNNNONNGNNNG"
                      "GKCGCNNKKNBWCGCWBWCNKNTNKNKNKN"
                      "CGGNONNGNNOWBCCBCWBGONNGNNNGTN"
                      "NNKTTNKNKKKWBWCBCWCNNNKNNGYGGG"
                      "NGNNKYNNNGTTBWCBWWBNNGNNGGGGGG"
                      "KNNTKNNNKNNBCWGWCBCNKNNTKGYGYC"
                      "NNNGNNKGNNNBCCBWCBCGTKNGONNGKN"
                      "NNNGGGGNNNKBCWKWCBBNOTKNKNKNNN"
                      "NGGCKGGGNGNWCKCWBWNNNGNNNNNNNN"
                      "KNGGGGGNKNNWCKWCBWNNKNNNBONNKN"
                      "NNNGNNNGTNNYCBCWGWNGONNYTNNGNN"
                      "NNKNKTNNNNKTBTCBCWGNNNBNNNKNKK"
                      "OGNYNGYNNGNNBWCCCWNOOGNYNGYNKG"
                      "KTNYKNYNKNTTBWGBCCNNNNNOKNYNGN"
                      "NTNBNNRGNNNNCWBWCNTGNNNWTNBGNO"
                      "TTTBTTPTTTTTTTYTYTTTTTYWTYCOOT"
                      "TTTBTTRTTTTTTTTTTTTTTTYWTTBTTO"
                      "YTTTTTTOTTTTTTTTOTTTOYTTYTTTYT"
                      "TTTOTTYTTTTTTTTTTTTTTTTTTTTTTT"))
   (make-level :name "Bacchus" :difficulty 0 :slots 3 :seed 6 :par 460
               :order "2110020222200011111212022222222222111111111111111111122222220000000000000020000200020010121"
               :rows '("NKKKNGKKNKKKNKKKNKKKNKKKKKGKNK"
                      "KKKNKNKOKKNBKKKBKKKBKKGNKKKNKK"
                      "KKNKKKNKKKNGGBKGGBNGGKNKKKNKKK"
                      "KNKKKKKKKNBNKNGKKCGNBNKKNNKNKT"
                      "NKKKOKKBNKGGKGKTKKKKGGKKNKGKNK"
                      "KKKNKKKNKKKKKKTTTKKKKKKNKKKNKK"
                      "KKNKKNNKKKNGKTTTTTNBKKNKGKKKKK"
                      "KNKKKNKKKNKKKTKTKTKKKNGKKTGKKN"
                      "KKKKNKKKNBKKKTYTTOKGNBKKNKKKNG"
                      "BKKTKKKNKKKNKKTROKKNKKNNKGKOKK"
                      "GKNKKKNKKKOKKKGTKNNKKNNKKKNBKK"
                      "KNKKKTGKKWTTTTTWWWWWWNKGKKKKKN"
                      "TKKKNKKBTWWTTTTYWWWWWKKKTKKKNK"
                      "KKKNKNBNKWWWCWWWCWTTWKNNKKKNKB"
                      "KKNKBKNKKWWWWWWYTWYWOTOKOKNKKB"
                      "KTBKKNKKKWWWWWCWWWWWTNYRRRKKKN"
                      "OKBKNKKKNWWWWWWWWWWWWKKKRBKKNK"
                      "KKKNKKKNKWWWWWWWWYWWWKKNTKGNNK"
                      "KKTKKKNBKWWTWWWWCWWWWKNNNNNKKK"
                      "KNGBKORBBCWWWCWWWWWWWNKKKNKKKO"
                      "KKKKRRYYGGWWWWWWWWWWWKKKKKKKNK"
                      "KKNNNPPNNNNWWWCWWWCWWKBNKKKKKK"
                      "NNNNKNNOKKNNKKNKKKKKKGNKKKNKKK"
                      "NNNNKNNKNNNTNNNONNTNNNNNNNTNNN"
                      "NOONNTNNNNNNTNNNNKNNKNNNNNNONN"
                      "NNNNNNNNNNNNNNKKNNNNNNNKNNNKNN"))))

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
;;; column to deploy from next says whether some order clears the plate.
;;; It tries first the boxes that take the most at once, gives up on a
;;; position as soon as no box within reach of the free slots can take
;;; anything, and remembers each position it has been through -- how far
;;; into each column, and what the boxes out still hold -- so as not to go
;;; through it twice.  Not which cubes are left: positions that differ only
;;; in which cubes of a colour went are near enough the same, and telling
;;; them apart makes the search too slow to finish.
;;;
;;; It is not exactly the game: there, ants walk and rest, and two boxes of
;;; one colour share cubes as their ants happen to arrive.  So an order it
;;; finds can lose; with ACCEPT, the caller plays it out in the real model
;;; and the search goes on past any that does.

(defun cell-size (h w)
  "The side of a cube, in points, for a picture H by W."
  (min 26d0 (/ 370d0 (+ w 2)) (/ *plate-room* h)))

(defun slot-entry-column (slot slots w size)
  "ENTRY-COLUMN without a game: for SLOT of SLOTS, under a picture W wide of
cubes SIZE."
  (let ((x (* (- slot (/ (1- slots) 2)) *slot-step*)) ; from the middle
        (left (- (/ (* w size) 2))))
    (max 0 (min (1- w) (round (- (/ (- x left) size) 0.5d0))))))

(defun settle (grid deployed)
  "Let the DEPLOYED boxes, each a list (colour remaining entry-column slot),
take what they can from GRID, which is changed.  The boxes still not empty.
A box short of ants for every cube it can reach takes the nearest, as its
ants do: measured from where they step onto the plate, below its slot."
  (loop with h = (array-dimension grid 0)
        for progress = nil
        do (dolist (box deployed)
             (loop while (plusp (second box))
                   for cubes = (reachable-cubes grid (first box) nil h (third box))
                   while cubes
                   do (loop for (nil r c) in cubes
                            while (plusp (second box))
                            do (setf (aref grid r c) nil)
                               (decf (second box))
                               (setf progress t))))
        while progress)
  (remove-if #'zerop deployed :key #'second))

(defun reachable-colours (grid)
  "The colours with a cube some ant can reach now."
  (destructuring-bind (h w) (array-dimensions grid)
    (let ((dist (flood grid -1 -1))
          (colours '()))
      (dotimes (r h colours)
        (dotimes (c w)
          (let ((colour (aref grid r c)))
            (when (and colour
                       (not (member colour colours))
                       (loop for (dr dc) in *directions*
                             for nr = (+ r dr)
                             for nc = (+ c dc)
                             thereis (and (walkable-p grid nr nc)
                                          (>= (aref dist (cell-index w nr nc)) 0))))
              (push colour colours))))))))

(defun hopeless-p (columns pointers free reachable)
  "After SETTLE, when nothing deployed can take more: whether no box any
column could bring out within FREE more slots -- the boxes before it going
out too, and taking nothing -- has a REACHABLE colour.  Then the slots fill
and the round is lost, whatever the order."
  (loop for column across columns
        for p in pointers
        never (loop for box in (nthcdr p column)
                    repeat free
                    thereis (member (first box) reachable))))

(defun solvable-p (grid columns slots &key deployed budget accept (greedy t))
  "Whether some order of deployment from COLUMNS, each a list of
(colour . count), clears GRID with SLOTS slots; and if so, the second value,
the columns in that order.  DEPLOYED, as for SETTLE, are boxes already in
slots.  ACCEPT, given an order that clears the grid, may refuse it, and the
search goes on.  GREEDY false tries columns from the left, not those whose
boxes take most first.  With a BUDGET, it gives up after looking at that many
positions: the third value is then :GAVE-UP."
  (let* ((seen (make-hash-table :test 'equal))
         (columns (coerce columns 'vector))
         (h (array-dimension grid 0))
         (w (array-dimension grid 1))
         (size (cell-size h w)))
    (labels ((key (pointers deployed)
               (list pointers
                     (sort (mapcar (lambda (box) (list (fourth box) (first box) (second box))) deployed)
                           #'< :key #'first)))
             (search-from (grid pointers deployed prefix)
               (when (zerop (cube-count grid))
                 (let ((order (reverse prefix)))
                   (return-from search-from
                     (if (or (null accept) (funcall accept order)) (values t order) nil))))
               (let ((key (key pointers deployed))
                     (slot (loop for slot below slots
                                 unless (find slot deployed :key #'fourth)
                                   return slot)))
                 (when (or (null slot) (gethash key seen))
                   (return-from search-from nil))
                 (when (and budget (>= (hash-table-count seen) budget))
                   (return-from solvable-p (values nil nil :gave-up)))
                 (setf (gethash key seen) t)
                 (let ((reachable (reachable-colours grid)))
                   (when (hopeless-p columns pointers (- slots (length deployed)) reachable)
                     (return-from search-from nil))
                   ;; Every column's next position, those that take most first.
                   (let ((moves '()))
                     (loop for k from 0
                           for p in pointers
                           for column = (aref columns k)
                           when (< p (length column))
                             do (let* ((box (nth p column))
                                       (new (list (first box) (rest box)
                                                  (slot-entry-column slot slots w size) slot))
                                       (pointers (copy-list pointers)))
                                  (incf (nth k pointers))
                                  (push (if (member (first box) reachable)
                                            ;; Only a box that can take
                                            ;; anything changes the grid.
                                            (let* ((grid (copy-grid grid))
                                                   (left (cube-count grid))
                                                   (deployed (settle grid (cons new (mapcar #'copy-list deployed)))))
                                              (list (- left (cube-count grid)) k grid pointers deployed))
                                            (list 0 k grid pointers (cons new deployed)))
                                        moves)))
                     (loop for (nil k grid pointers deployed)
                             in (if greedy
                                    (stable-sort (nreverse moves) #'> :key #'first)
                                    (nreverse moves))
                           do (multiple-value-bind (won order)
                                  (search-from grid pointers deployed (cons k prefix))
                                (when won
                                  (return-from search-from (values t order))))))))))
      (let ((grid (copy-grid grid)))
        (search-from grid (make-list (length columns) :initial-element 0)
                     (settle grid (mapcar #'copy-list deployed))
                     '())))))

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

(defparameter *deal-tries* 6
  "Orders that lose when played out before DEAL-WINNABLE gives up on a way
of searching a deal.")

(defparameter *deal-budget* 20000
  "Positions DEAL-WINNABLE lets the solver look at, each way, for each seed.")

(defun deal-winnable (grid level &key accept)
  "A deal that SOLVABLE-P accepts, trying seeds from the level's own; the
seed used is the second value, and the winning order it found the third.
ACCEPT is SOLVABLE-P's, given the columns, the seed and an order.  For each
seed the solver searches greedily and then from the left, each until it has
had *DEAL-TRIES* orders refused or looked at *DEAL-BUDGET* positions."
  (loop for seed from (level-seed level)
        repeat 60
        for columns = (deal grid (level-columns level) seed (level-difficulty level))
        do (dolist (greedy '(t nil))
             (let ((refused 0))
               (multiple-value-bind (won order)
                   (block search
                     (solvable-p grid columns (level-slots level)
                                 :greedy greedy
                                 :budget *deal-budget*
                                 :accept (and accept
                                              (let ((columns columns) (seed seed))
                                                (lambda (order)
                                                  (or (funcall accept columns seed order)
                                                      (when (>= (incf refused) *deal-tries*)
                                                        (return-from search nil))))))))
                 (when won
                   (return-from deal-winnable (values columns seed order)))))))
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
  (time 0d0)                            ; seconds played, in the model's time
  plan                                  ; columns of a winning order still to tap
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
  (slot-entry-column slot (length (game-slots game)) (game-w game) (game-cell-size game)))

(defun make-round (level columns seed &optional plan)
  "A round of LEVEL with the queue COLUMNS, as DEAL makes them, dealt by SEED."
  (let* ((grid (parse-picture (level-rows level)))
         (h (array-dimension grid 0))
         (w (array-dimension grid 1))
         (size (cell-size h w)))
    (make-game :level level
               :plan plan
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
               :rng (make-rng (or seed (level-seed level))))))

(defun new-game (level)
  "A round of LEVEL, dealt by its seed and with its order as the plan; or,
for a level without an order, the first deal from its seed whose solver's
order wins when played out."
  (let ((grid (parse-picture (level-rows level))))
    (if (level-order level)
        ;; Worked out ahead, by src/tune.lisp, and checked by src/test.lisp.
        (make-round level
                    (deal grid (level-columns level) (level-seed level) (level-difficulty level))
                    (level-seed level)
                    (map 'list #'digit-char-p (level-order level)))
        (multiple-value-bind (columns seed plan)
            (deal-winnable grid level
                           :accept (lambda (columns seed order)
                                     (play-out-checked (make-round level columns seed) order)))
          (make-round level columns seed plan)))))

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
      ;; Off the plan, HINT has to find another.
      (if (eql column (first (game-plan game)))
          (pop (game-plan game))
          (setf (game-plan game) nil))
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

(defun rest-seconds (rng)
  "How long an ant rests, by *REST-SECONDS*."
  (destructuring-bind (shortest mean longest) *rest-seconds*
    (min longest (- shortest (* mean (log (- 1d0 (random-between rng 0d0 0.999d0))))))))

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
           (setf (ant-rest ant) (rest-seconds (game-rng game)))
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

;;; Playing a round out, headless: to check an order in the real model.

(defparameter *replay-dt* (/ 1d0 30) "Seconds a step when the model plays by itself.")

(defun fork-game (game)
  "A copy of GAME to play on without disturbing it: its own grid, boxes,
ants and random sequence, and no plan."
  (let ((boxes (make-hash-table :test 'eq))
        (copy (copy-structure game)))
    (flet ((box (box)
             (or (gethash box boxes) (setf (gethash box boxes) (copy-box box)))))
      (setf (game-grid copy) (copy-grid (game-grid game))
            (game-reserved copy) (copy-grid (game-reserved game))
            (game-columns copy) (map 'vector (lambda (column) (mapcar #'box column))
                                     (game-columns game))
            (game-slots copy) (map 'vector (lambda (box) (and box (box box))) (game-slots game))
            (game-ants copy) (mapcar (lambda (ant)
                                       (let ((new (copy-ant ant)))
                                         (setf (ant-box new) (box (ant-box ant)))
                                         new))
                                     (game-ants game))
            (game-rng copy) (copy-rng (game-rng game))
            (game-plan copy) nil
            (game-events copy) '()))
    copy))

(defun play-out (game order &key (limit 3600) (dt *replay-dt*))
  "Play a copy of GAME on, DT a step, deploying from the columns in ORDER as
fast as slots come free.  Whether it is won, and the game's time at the end."
  (let ((game (fork-game game))
        (order (copy-list order)))
    (loop while (and (eq (game-state game) :playing) (< (game-time game) limit))
          do (when (and order (free-slot game))
               (unless (deploy game (pop order))
                 (return-from play-out (values nil (game-time game)))))
             (step-game game dt)
             (setf (game-events game) '()))
    (values (eq (game-state game) :won) (game-time game))))

;;; Hints and stars.

(defparameter *check-steps* (list (/ 1d0 30) (/ 1d0 60) (/ 1d0 120))
  "Frame steps an order must win at to be trusted: a 60Hz screen's at double
speed, a 60Hz screen's, and a 120Hz screen's.  Ants are chaotic enough that
an order that wins at one step can lose at another.")

(defun play-out-checked (game order)
  "Whether ORDER wins from GAME at every one of *CHECK-STEPS*; and the
longest it takes."
  (let ((worst 0d0))
    (dolist (dt *check-steps* (values t worst))
      (multiple-value-bind (won time) (play-out game order :dt dt)
        (unless won (return (values nil time)))
        (setf worst (max worst time))))))

(defparameter *hint-budget* 400
  "Positions a hint may search before it gives up: enough for the solver to
find its way from most positions, and quick enough on a phone.")

(defparameter *hint-tries* 4
  "Orders a hint may play out, to find one that wins, before it gives up.")

(defun hint (game)
  "What to do now: (:deploy k) to tap column K, (:none) when there is nothing
left to tap, (:wait) when a winning order goes on with the boxes already
out, (:lost) when the solver can show there is none, or (:unsure) when it
looked as far as it may and found nothing.
While the player keeps to the order the deal was checked with, the next
column of that; otherwise the solver's, from where the round stands."
  (cond ((every #'null (game-columns game)) (list :none))
        ((game-plan game)
         (if (free-slot game) (list :deploy (first (game-plan game))) (list :wait)))
        (t (replan game))))

(defun replan (game)
  "HINT from where the round stands, keeping the order it finds as the plan.
Cubes ants are on their way to count as taken, and boxes as short of the
ants already out.  An order the solver finds is played out before it is
believed, and the search goes on past one that loses."
  (let ((grid (copy-grid (game-grid game)))
        (slots (length (game-slots game)))
        (tries 0))
    (dotimes (i (array-total-size grid))
      (when (row-major-aref (game-reserved game) i)
        (setf (row-major-aref grid i) nil)))
    (setf (game-plan game) nil)
    (multiple-value-bind (won order gave-up)
        (block search
          (solvable-p grid
                      (loop for column across (game-columns game)
                            collect (loop for box in column
                                          collect (cons (box-colour box) (box-count box))))
                      slots
                      :budget *hint-budget*
                      :accept (lambda (order)
                                (when (> (incf tries) *hint-tries*)
                                  (return-from search (values nil nil :gave-up)))
                                (play-out game order))
                      :deployed (loop for box across (game-slots game)
                                      for slot from 0
                                      when (and box (> (box-count box) (box-out box)))
                                        collect (list (box-colour box) (- (box-count box) (box-out box))
                                                      (entry-column game slot) slot))))
      (when won (setf (game-plan game) order))
      (cond ((or gave-up (and (not won) (plusp tries)))
             ;; An order the solver believed lost when played: it may not
             ;; have looked at the one that wins.
             (list :unsure))
            ((not won) (list :lost))
            ((and order (free-slot game)) (list :deploy (first order)))
            (t (list :wait))))))

(defparameter *star-margins* '(1.25d0 1.6d0)
  "A round within the first times its picture's par earns three stars,
within the second two, and slower one.")

(defun stars (game &optional hinted)
  "Stars for a won round: by its time against the level's par, and no more
than two with a hint."
  (let* ((par (or (level-par (game-level game)) (game-time game)))
         (stars (cond ((<= (game-time game) (* par (first *star-margins*))) 3)
                      ((<= (game-time game) (* par (second *star-margins*))) 2)
                      (t 1))))
    (if hinted (min stars 2) stars)))

(defun step-game (game dt)
  (when (eq (game-state game) :playing)
    (incf (game-time game) dt)
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
