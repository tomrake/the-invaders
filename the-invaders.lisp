;;;; the-invaders.lisp --- A remake of the glassic game Space Invaders

;; Copyright (C) 2015 Jan Tatham

;; Author: Jan Tatham <jan@sebity.com>
;; Keywords: games

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package #:the-invaders)

(defparameter *data-root*
  (asdf:system-source-directory 'the-invaders))
(defparameter *font-root* (merge-pathnames "fonts/" *data-root*))
(defparameter *audio-root* (merge-pathnames "audio/" *data-root*))
(defparameter *gfx-root* (merge-pathnames "gfx/" *data-root*))

;;;; Game Params
(defparameter *game-width* 800)
(defparameter *game-height* 600)
(defparameter *game-state* 0) ; 0:menu/intro, 1:in game, 2:game over

(defparameter *level* nil)
(defparameter *space-w* 70)
(defparameter *space-h* 45)

(defparameter *pause* nil)

(defparameter *ship* nil)
(defparameter *player-lives* 3)
(defparameter *player-level* 1)
(defparameter *player-shots* nil)
(defparameter *player-score* 0)
(defparameter *player-explosion* nil)

(defparameter *ship* nil)



(defparameter *enemy* nil)
(defparameter *enemy-count* 0)
(defparameter *enemy-shots* nil)
(defparameter *enemy-direction* 'right)
(defparameter *enemy-explosion* nil)

(defparameter *enemy-move-delay* 60)
(defparameter *enemy-move-space* 10)

(defparameter *mothership* nil)
(defparameter *mothership-explosion* nil)

(defparameter *game-ticks* 0)

;;;; Sound Params
(defparameter *mixer-opened* nil)
(defparameter *mothership-engine* nil)
(defparameter *soundfx* nil)

(defparameter *ss-ship* nil)
(defparameter *ss-enemy* nil)
(defparameter *ss-mothership* nil)
(defparameter *img-explosion-enemy* nil)
(defparameter *img-explosion-ship* nil)

;;;; GFX Params
(defparameter *gfx-ss-ship* (merge-pathnames "spritesheet_player.png" *gfx-root*))
(defparameter *gfx-ss-enemy* (merge-pathnames "spritesheet_enemy.png" *gfx-root*))
(defparameter *gfx-ss-mothership* (merge-pathnames "spritesheet_mothership.png" *gfx-root*))
(defparameter *gfx-explosion-enemy* (merge-pathnames "explosion-1.png" *gfx-root*))
(defparameter *gfx-explosion-ship* (merge-pathnames "explosion-2.png" *gfx-root*))
(defparameter *gfx-space-bg* (merge-pathnames "space-bg.jpg" *gfx-root*))
(defparameter *gfx-title-bg* (merge-pathnames "title-bg.jpg" *gfx-root*))
(defparameter *gfx-game-over-bg* (merge-pathnames "game-over-bg.jpg" *gfx-root*))

;;;; Font Params
(defparameter *terminus-ttf-12* 
  (make-instance 'SDL:ttf-font-definition
		 :size 12
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-18* 
  (make-instance 'SDL:ttf-font-definition
		 :size 18
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-24* 
  (make-instance 'SDL:ttf-font-definition
		 :size 24
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-32* 
  (make-instance 'SDL:ttf-font-definition
		 :size 32
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))

(defparameter *ttf-font-small* nil)
(defparameter *ttf-font-normal* nil)
(defparameter *ttf-font-large* nil)
(defparameter *ttf-font-huge* nil)


;;;;;;;;;;;;;;;;;;;;;;;; STRUCTS/CLASSES ;;;;;;;;;;;;;;;;;;;;;;;;

(defclass game-object ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)
   (dx :initarg :dx :initform 0 :accessor dx)
   (dy :initarg :dy :initform 0 :accessor dy)))

(defmethod draw ((obj game-object))
  (error "Nothing to draw!"))

(defmethod delta-move ((obj game-object))
  "Change the object position by one step."
  (incf (x obj) (dx obj))
  (incf (y obj) (dy obj)))


;;; The Image object class

(defclass image-object (game-object)
  ((image :initarg :image :reader image)))

(defmethod draw-image ((obj image-object))
  (img-draw (image obj) (x obj) (y ob)))

;;; The Sprite object class

(defclass sprite-object (game-object)
  ((sheet :initarg :sheet :reader sheet)))

(defmethod draw-cell ((obj sprite-object) (cell number))
  "Draw the cell of spite-object from the sprite sheet."
  (blt-draw (sheet obj) (x obj) (y obj) cell))

(defclass player-shot (game-object)
  ())

(defmethod remove-object ((obj player-shot))
  (setf *player-shots* (remove obj *player-shots*)))

;;;; The Image Sheet class

(defclass image-sheet ()
  ((image :initarg :image :reader image)
   (sheet :initform nil)))

(defmethod sheet ((ss image-sheet))
  (cond ((slot-value ss 'sheet))
	(t (load-sheet ss)
	   (slot-value ss 'sheet))))

(defmethod load-sheet ((ss image-sheet))
  (setf (slot-value ss 'sheet) (sdl-image:load-image (image ss)))
  (slot-value ss 'sheet))

(defmethod img-draw  ((ss image-sheet) (x number) (y number))
  (sdl:draw-surface-at-* (sheet ss) x y))

;;;; The Sprite Sheet class

(defclass sprite-sheet-object (image-sheet)
  ((cells :initarg :cells :reader cells)))


(defmethod load-sheet :after ((ss sprite-sheet-object))
  (when (cells ss)
    (setf (sdl:cells (sheet ss)) (cells ss))))

;; (defmethod sheet ((ss sprite-sheet-object))
;;   "Return the sdl image object for sheet object."
;;   (cond ((slot-value ss 'sheet))
;; 	(t (setf (slot-value ss 'sheet) (sdl-image:load-image (image ss))
;; 		 (sdl:cells (slot-value ss 'sheet)) (cells ss))
;; 	   (slot-value ss 'sheet))))

(defmethod blt-draw ((ss sprite-sheet-object) (x number) (y number) (cell number))
  "Draw the cell portion of the sheet object for the cell at x and y offsets."
  (sdl:draw-surface-at-* (sheet ss) x y :cell cell))

;;;; The enemy class

(defclass enemy (sprite-object)
  ((sprite :initarg :sprite :initform 0 :accessor sprite)))


(defmethod draw ((obj enemy))
  (draw-cell obj (+ (sprite obj)
		    (mod (/ (x obj) 2) 10))))

(defmethod remove-object ((obj enemy))
  (setf *enemy* (remove obj *enemy*)))

(defmethod explode ((target enemy) (projectile player-shot))
  (let ((e (make-instance 'exploding-enemy :x (x target) :y (y target)
			  :image *img-explosion-enemy*)))
    (push e *enemy-explosion*)
    (add-cleanup-hook e #'(lambda() (remove-object e)))
    ;; Remove the two objects
    (remove-object target)
    (remove-object projectile)
    (determine-enemy-speed)
    (play-sound 3)
    (incf *player-score* 10)
    (set-countdown e 6)))

(defclass ship (sprite-object)
  ())

(defmethod remove-object ((obj ship))
  (setf *ship* nil))
  
(defmethod draw-cell ((obj ship) (cell number))
  "The player ship is centered on the gun 26 pixels from the edge."
  (blt-draw (sheet obj) ( - (x obj) 26) (y obj) cell))



;;;; Countdown objects allow objects to change at the creation of a countdown
;;;; And then at the cleanup when the count is zero.

(defclass countdown-object ()
  ((countdown :initarg :countdown :initform 0 :accessor countdown)
   (trigger-hooks :initarg :trigger-hooks :initform nil)
   (cleanup-hooks :initarg :cleanup-hooks :initform nil)))

(defmethod active-countdown ((obj countdown-object))
  (countdown obj))

(defmethod add-trigger-hook ((obj countdown-object) (thunk function))
  "Add a function to perfom on count start."
  (push thunk (slot-value obj 'trigger-hooks)))


(defmethod add-cleanup-hook ((obj countdown-object) (thunk function))
  "Add a function to perform on countdown end."
  (push thunk (slot-value obj 'cleanup-hooks)))

(defmethod run-triggers ((obj countdown-object))
  (dolist (func (slot-value obj 'trigger-hooks)) (funcall func)))


(defmethod run-cleanup ((obj countdown-object))
  (dolist (func (slot-value obj 'cleanup-hooks)) (funcall func)))


(defmethod next ((obj countdown-object))
  "Do the next time step. Run cleanup if time is zero or less."
  (cond  ((null (countdown obj)))
	 ((< (countdown obj) 1)
	  (run-cleanup obj)
	  (setf (countdown obj) nil))
         (t (decf (countdown obj)))))

(defmethod set-countdown ((obj countdown-object) (count integer))
  "Cause the triggers to run and set the countdown clock to count."
  (when (and (not (null count)) (> count 0))
    (setf (countdown obj) count)
    (run-triggers obj)))


(defstruct player-explosion
  (x 0)
  (y 0)
  (time 0))


(defclass enemy-shot (game-object)
  ())

(defmethod remove-object ((obj enemy-shot))
  (setf *enemy-shots* (remove obj *enemy-shots*)))


(defclass exploding-enemy (countdown-object image-object)
  ())

(defmethod draw ((obj exploding-enemy))
  (img-draw (image obj) (x obj) (y obj)))

(defmethod remove-object ((obj exploding-enemy))
  (setf *enemy-explosion* (remove obj *enemy-explosion*)))


(defstruct enemy-explosion
  (x 0)
  (y 0)
  (time 0))

(defclass mothership (sprite-object) 
  ())


(defmethod remove-object ((obj mothership))
  (setf *mothership* nil))

(defmethod draw ((obj mothership))
  (draw-cell obj (floor (mod *game-ticks* 9) 3)))



(defstruct mothership-explosion
  (x 0)
  (y 0)
  (time 0))



;;;;;;;;;;;;;;;;;;;;;;;; UTILS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; SQUARE function

(defun square (x)
  (* x x))


;;;; RANDOM-ELEMENT function

(defun random-element (lst)
  (elt lst (random (length lst))))





;;;;;;;;;;;;;;;;;;;;;;;; PRIMITIVES ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DRAW-TEXT function

(defun draw-text (string x y r g b &optional (font *ttf-font-normal*))
  (sdl:draw-string-solid-* string
			   x y
			   :color (sdl:color :r r :g g :b b)
			   :font font))


;;;; DRAW-BOX function

(defun draw-box (x y w h r g b)
  (sdl:draw-box (sdl:rectangle-from-midpoint-* x y w h)
		:color (sdl:color :r r :g g :b b)))


;;;; DRAW-LINE function

(defun draw-line (x0 y0 x1 y1 r g b)
  (sdl:draw-line-* x0 y0 x1 y1
		   :color (sdl:color :r r :g g :b b)))


;;;; DRAW-CIRCLE function

(defun draw-circle (x y rad r g b)
  (sdl:draw-circle-* x y rad
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-CIRCLE-FILLED function

(defun draw-circle-filled (x y rad r g b)
  (sdl:draw-filled-circle-* x y rad
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-ELLIPSE-FILLED function

(defun draw-ellipse-filled (x y rx ry r g b)
  (sdl:draw-filled-ellipse-* x y rx ry
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-POLYGON function

(defun draw-polygon (vertices r g b)
  (sdl:draw-filled-polygon vertices :color (sdl:color :r r :g g :b b)))


;;;; PLAY-SOUND function

(defun play-sound (s)
  (sdl-mixer:play-sample (aref *soundfx* s)))


;;;;;;;;;;;;;;;;;;;;;;;; ENEMY ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; CREATE-ENEMY function

(defun create-enemy (dy)
  (setf *enemy* 'nil)

  (if (> dy 200)
      (setf dy 200))

  (loop for y below 5
     do (loop for x below 8
	   do (push
	       (make-instance
		'enemy
		:x (* x *space-w*)
		:y (+ (* y *space-h*) dy)
		:sheet *ss-enemy*
		:sprite y) *enemy*))))


;;;; DRAW-ENEMY function

(defun draw-enemy ()
  (loop for e in *enemy*
     do (draw e)))

;;;; UPDATE-ENEMY function

(defun update-enemy ()
  (setf *game-ticks* (incf *game-ticks*))
  (when (>= *game-ticks* *enemy-move-delay*)
      (determine-enemy-position) 
      (update-enemy-position)
      (play-sound 1)
      (setf *game-ticks* 0)))


;;;; DETERMINE-ENEMY-POSITION function

(defun determine-enemy-position ()
  (loop for e in *enemy*
     do (if (and (equalp *enemy-direction* 'right)
		 (>= (+ (x e) 50) *game-width*))
	    (setf *enemy-direction* 'down-and-left)
	    (if (and (equalp *enemy-direction* 'left)
		     (<= (x e) 0))
		(setf *enemy-direction* 'down-and-right)))))


;;; UPDATE-ENEMY-POSITION function

(defun update-enemy-position ()
  (cond ((equalp *enemy-direction* 'right)
	 (loop for e in *enemy*
	    do (setf (x e) (+ (x e) *enemy-move-space*))))

	((equalp *enemy-direction* 'left)
	 (loop for e in *enemy*
	    do (setf (x e) (+ (x e) (- *enemy-move-space*)))))

	((equalp *enemy-direction* 'down-and-right)
	 (loop for e in *enemy*
	    do (progn (setf (y e) (+ (y e) 20))
		      (setf *enemy-direction* 'right))))

	((equalp *enemy-direction* 'down-and-left)
	 (loop for e in *enemy*
	    do (progn (setf (y e) (+ (y e) 20))
		      (setf *enemy-direction* 'left)))))

  (enemy-hit-bottom)
  (fire-enemy-shot))


;;; ENEMY-HIT-BOTTOM function

(defun enemy-hit-bottom ()
  (loop for e in *enemy*
     do (if (> (+ (y e) 32) 540)
	    (setf *player-lives* 0))))


;;;; DETERMINE-ENEMY-SPEED function

(defun determine-enemy-speed ()
  (cond ((= (length *enemy*) 30) (setf *enemy-move-delay* 45))
	((= (length *enemy*) 25) (setf *enemy-move-delay* 30))
	((= (length *enemy*) 20) (setf *enemy-move-delay* 20))
	((= (length *enemy*) 15) (setf *enemy-move-delay* 15))
	((= (length *enemy*) 10) (setf *enemy-move-delay* 12))
	((= (length *enemy*) 5) (setf *enemy-move-delay* 9))
	((= (length *enemy*) 1) (setf *enemy-move-delay* 5))
	(t ())))


;;;; FIRE-ENEMY-SHOT function

(defun fire-enemy-shot ()
  (dotimes (n (ceiling (/ *player-level* 5)))
    (when (< (random 100) (+ 20 *player-level*))

      (let ((enemy (random-element *enemy*)))
	(push (make-instance 'enemy-shot :x (+ (x enemy) 24) 
			     :y (+ (y enemy) 32)
			     :dx 0
			     :dy (+ (random 3) 3)) *enemy-shots*)
	(play-sound 2)))))


;;;; DRAW-ENEMY-SHOT function

(defun draw-enemy-shot ()
  (loop for f in *enemy-shots*
     do (draw f)))


(defmethod draw ((shot enemy-shot))
  (draw-box (x shot) (y shot) 2 10 255 0 0))

;;;; UPDATE-ENEMY-SHOTS function

(defun update-enemy-shots ()
  (loop for f in *enemy-shots*
     do (progn (if (> (y f) *game-height*)
		   (remove-object f)
		   (delta-move f))
	       (enemy-shot-player f)))

  (if (<= *player-lives* 0)
      (change-game-state)))


;;;; ENEMY-SHOT-PLAYER function

(defun enemy-shot-player (s)
  (let ((p *ship*))
    (when (and (<= (- (x p) 26) (x s))
		 (>= (+ (x p) 26) (+ (x s) 2))
		 (<= (y p) (y s))
		 (>= (+ (y p) 32) (y s)))
	    (create-ship-explosion)
	    (setf *player-lives* (decf *player-lives*))
	    (play-sound 4)
	    (setf (x p) 400)
	    (remove-object s))))


;;;; CREATE-ENEMY-EXPLOSION function

(defun create-enemy-explosion (x y)
  (push (make-enemy-explosion :x x :y y :time 6) *enemy-explosion*))

(defun update-enemy-explosion ()
  (loop for e in *enemy-explosion* do (next e)))


;;;; DRAW-ENEMY-EXPLOSION function

(defun draw-enemy-explosion ()
  (loop for e in *enemy-explosion* do (draw e)))


;;;;;;;;;;;;;;;;;;;;;;;; MOTHERSHIP ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DEPLOY-MOTHERSHIP function

(defun deploy-mothership ()
  (let ((chance (random 1000)))
    (if (and (= chance 1)
	     (not *mothership*)
	     (> (length *enemy*) 4)
	     (< (length *enemy*) 38))
	(create-mothership))))


;;;; CREATE-MOTHERSHIP function

(defun create-mothership ()
  (let ((entrance (random 2))
	(x -70)
	(dx 3))

    (when (= entrance 1)
	(setf x (+ *game-width* 5))
	(setf dx -3))

    (setf *mothership* (make-instance 'mothership :x x :y 35 :dx dx :sheet *ss-mothership*)))
  (play-mothership-engine))


;;;; DRAW-MOTHERSHIP function

(defun draw-mothership ()
  (if *mothership*
      (draw *mothership*)))



;;;; UPDATE-MOTHERSHIP function

(defun update-mothership ()
  (when *mothership*
      (let ((m *mothership*))
       (setf (x m) (+ (x m) (dx m)))
       (if (or (<= (x m) -75)
	       (>= (x m) (+ *game-width* 10)))
	   (setf *mothership* nil)))))


;;;; CREATE-MOTHERSHIP-EXPLOSION function

(defun create-mothership-explosion (m)
  (setf *mothership-explosion* (make-mothership-explosion :x (x m)
							  :y (y m)
							  :time 15)))


;;;; DRAW-MOTHERSHIP-EXPLOSION function

(defun draw-mothership-explosion ()
  (when *mothership-explosion*
      (let ((m *mothership-explosion*))
	 (if (zerop (mothership-explosion-time m))
	     (setf *mothership-explosion* nil)
	     (progn (setf (mothership-explosion-time m)
		(decf (mothership-explosion-time m)))
		(sdl:draw-surface-at-* (sdl-image:load-image *gfx-explosion-ship*)
			(mothership-explosion-x m)
			(mothership-explosion-y m)))))))

;;;; PLAY-MOTHERSHIP-ENGINE function

(defun play-mothership-engine ()
  (sdl-mixer:play-music *mothership-engine*))


;;;; STOP-MOTHERSHIP-ENGINE function

(defun halt-mothership-engine ()
  (sdl-mixer:halt-music 100))


;;;;;;;;;;;;;;;;;;;;;;;; PLAYER ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; CREATE-SHIP function

(defun create-ship ()
  (setf *ship* (make-instance 'ship :x 400 :y 540 :sheet *ss-ship*)))


;;;; DRAW-SHIP function

(defun draw-ship (p)
   (draw-cell  p  (mod *game-ticks* 3)))

;;;; MOVE-SHIP function

(defun move-ship (p direction)
  (cond ((equalp direction 'left) (progn (setf (x p) (- (x p) 4))
					 (if (<= (x p) 26)
					     (setf (x p) 26))))

	((equalp direction 'right) (progn (setf (x p) (+ (x p) 4))
					  (if (>= (x p) (- *game-width* 26))
					      (setf (x p) (- *game-width* 26)))))))


;;;; FIRE-SHOT function

(defun fire-shot (p)
  (when (zerop (length *player-shots*))
      (push (make-instance 'player-shot :x (x p) :y (y p) :dy -5 :dx 0) *player-shots*)
      (play-sound 2)))


;;;; DRAW-SHOT function

(defun draw-shot ()
  (loop for f in *player-shots*
     do (draw f)))

(defmethod draw ((shot player-shot))
  (draw-box (x shot) (y shot) 2 10 255 255 255))


;;;; UPDATE-PLAYER_SHOTS function

(defun update-player-shots ()
  (loop for f in *player-shots*
     do (progn (if (<= (y f) 0)
		   (setf *player-shots* (remove f *player-shots*))
		   (delta-move f))
	       (player-shot-enemy f)
	       (player-shot-mothership f))))


;;;; PLAYER-SHOT-HIT function

(defun player-shot-enemy (s)
  (loop for e in *enemy*
     do (when (and (<= (x e) (x s))
		 (>= (+ (x e) 48) (+ (x s) 2))
		 (<= (y e) (y s))
		 (>= (+ (y e) 32) (y s)))
	  (explode e s)))

  (when (end-of-level-p)
      (calculate-score)
      (new-level)
      (play-sound 6)))


;;;; PLAYER-SHOT-MOTHERSHIP function

(defun player-shot-mothership (s)
  (if *mothership*
      (let ((m *mothership*))
	(if (and (<= (x m) (x s))
		 (>= (+ (x m) 64) (+ (x s) 2))
		 (<= (y m) (y s))
		 (>= (+ (y m) 32) (y s)))
	    (progn (create-mothership-explosion m)
		   (setf *player-score* (+ *player-score* (calculate-mothership-score m)))
		   (remove-object m)
		   (halt-mothership-engine)
		   (play-sound 5))))))


;;;; CREATE-SHIP-EXPLOSION function

(defun create-ship-explosion ()
  (push (make-player-explosion :x (x *ship*)
			       :y (y *ship*)
			       :time 20)
	*player-explosion*))


;;;; DRAW-PLAYER-EXPLOSION function

(defun draw-player-explosion ()
  (loop for p in *player-explosion*
     do	(progn (setf (player-explosion-time p) (decf (player-explosion-time p)))
	       (if (zerop (player-explosion-time p))
		   (setf *player-explosion* (remove p *player-explosion*))
		   (sdl:draw-surface-at-* (sdl-image:load-image *gfx-explosion-ship*)
					  (player-explosion-x p) (player-explosion-y p))))))


;;;;;;;;;;;;;;;;;;;;;;;; LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-LEVEL function

(defun display-level ()
  (sdl:draw-surface-at-* (sdl-image:load-image *gfx-space-bg*) 0 0))


;;;; DRAW-GAME-UI function

(defun draw-game-ui ()
  (draw-text (format nil "Score: ~a" *player-score*) 20 5 255 255 255)
  (draw-text (format nil "Level: ~a" *player-level*) 380 5 255 255 255)
  (draw-text (format nil "Lives: ~a" *player-lives*) 700 5 255 255 255)
  (if (eql *pause* t)
      (draw-text "Paused" 
	     380 280 255 255 255 *ttf-font-large*)))


;;;; END-OF-LEVEL-P function

(defun end-of-level-p ()
  (if (zerop (length *enemy*))
      t
      nil))


;;;; CALCULATE-SCORE function

(defun calculate-score ()
  (setf *player-score* (+ *player-score* (* *player-lives* 100) (* *player-level* 100))))


;;;; CALCULATE-MOTHERSHIP-SCORE function

(defun calculate-mothership-score (m)
  (let ((mid (+ (x m) 32))
	(sect (/ *game-width* 4)))
    (cond ((<= mid sect)
	   (if (< (dx m) 0)
	       50
	       300))

	  ((<= mid (* sect 2))
	   (if (< (dx m) 0)
	       100
	       150))

	  ((<= mid (* sect 3))
	   (if (< (dx m) 0)
	       150
	       100))

	  (t (if (< (dx m) 0)
		 300
		 50)))))


;;;; NEW-LEVEL function

(defun new-level ()
  (setf *player-level* (incf *player-level*))
  (reset-level))


;;;; RESET-LEVEL function

(defun reset-level ()
  (let ((level *player-level*))
    (if (> level 15)
	(setf level 15))
    (create-enemy (+ 70 (* level 10)))
    (setf *enemy-move-delay* 60)
    (setf *enemy-direction* 'right)
    (setf *mothership* nil)
    (setf *player-shots* nil)
    (setf *enemy-shots* nil)
    (setf *enemy-explosion* nil)))

;;;;;;;;;;;;;;;;;;;;;;;; SCREENS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-END-GAME function

(defun display-end-game ()
  (sdl:draw-surface-at-* (sdl-image:load-image *gfx-game-over-bg*) 0 0)

  ;(draw-text "The Invaders" 310 20 255 255 255 *ttf-font-huge*)

  (draw-text "Game Over" 330 150 255 255 255 *ttf-font-huge*)

  (draw-text (format nil "Final Score: ~a" *player-score*) 280 250 255 255 0 *ttf-font-huge*)

  (draw-text "Press SPACE to Continue..." 290 570 255 255 255))


;;;; DISPLAY-MENU function

(defun display-menu ()
  (sdl:draw-surface-at-* (sdl-image:load-image *gfx-title-bg*) 0 0)

  (draw-text "Press SPACE to Continue..." 290 570 255 255 255))


;;;;;;;;;;;;;;;;;;;;;;;; GAME STATE ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; PAUSE-GAME function

(defun pause-game ()
  (if (eql *pause* nil)
      (setf *pause* t)
      (setf *pause* nil)))


;;;; STATE-IN-PLAY function

(defun state-in-play ()
  
  (unless (eql *pause* t)
    (update-player-shots)
    (update-enemy)
    (update-mothership)
    (update-player-shots)
    (update-enemy-shots)
    (deploy-mothership)
    (update-enemy-explosion))

  (display-level)
  (draw-ship *ship*)
  (draw-enemy)
  (draw-mothership)
  (draw-shot)
  (draw-enemy-shot)
  (draw-player-explosion)
  (draw-enemy-explosion)
  (draw-mothership-explosion)
  (draw-game-ui))


;;;; CONTINUE-OPTION function

(defun continue-option ()
  (cond ((zerop *game-state*) (change-game-state))
	((= *game-state* 2) (change-game-state))
	(t ())))


;;;; CHANGE-GAME-STATE function

(defun change-game-state ()
  (cond ((zerop *game-state*) 
	 (progn (reset-game)
		(play-sound 6)
		(setf *game-state* 1)))

	((= *game-state* 1) (setf *game-state* 2))
	
	((= *game-state* 2) (setf *game-state* 0))
	
	(t ())))


;;;;;;;;;;;;;;;;;;;;;;;; THE GAME ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; RENDER function

(defun render ()
  (sdl:clear-display sdl:*black*)

  (cond ((= *game-state* 1) (state-in-play))

	((= *game-state* 2) (display-end-game))

	(t (display-menu)))

  (sdl:update-display))


;;;; RESET-GAME function

(defun reset-game ()
  (setf *random-state* (make-random-state t))
  (setf *pause* nil)
  (setf *player-level* 0)
  (setf *player-lives* 3)
  (setf *player-score* 0)
  (setf *player-shots* nil)
  (setf *player-explosion* nil)
  (setf *enemy-shots* nil)
  (setf *enemy-explosion* nil)
  (setf *mothership-explosion* nil)
  (new-level))



;;;; INITIALIZE-GAME function

(defun initialize-game ()
  (setf *game-state* 0)
  (create-ship))


(defun load-sprite-sheet ()
  ; enemy sprite sheet
  (setf *ss-enemy*
	(make-instance 'sprite-sheet-object
		       :image *gfx-ss-enemy*
		       :cells '((0 0 48 32) (48 0 48 32) (96 0 48 32) (144 0 48 32) (192 0 48 32)
		  (0 32 48 32) (48 32 48 32) (96 32 48 32) (144 32 48 32) (192 32 48 32))))
 

  ; player sprite sheet
  (setf *ss-ship*
	(make-instance 'sprite-sheet-object
		       :image *gfx-ss-ship*
		       :cells '((0 0 52 32) (0 32 52 32) (0 64 52 32))))
	
					; mothership sprite sheet
  (setf *ss-mothership*
	(make-instance 'sprite-sheet-object
		       :image *gfx-ss-mothership*
		       :cells '((0 0 64 32) (0 32 64 32) (0 64 64 32))))

  (setf *img-explosion-enemy*
	(make-instance 'image-sheet
		       :image *gfx-explosion-enemy*))
  
  (setf *img-explosion-ship*
	(make-instance 'image-sheet
		       :image *gfx-explosion-ship*))
  
  )



;;;; SETUP-AUDIO function

(defun setup-audio ()
  (setf *soundfx* (make-array 7))
  (sdl-mixer:init-mixer :mp3)
  (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 1024 :enable-callbacks nil))
  (when *mixer-opened*
    (setf (aref *soundfx* 0) (sdl-mixer:load-sample (sdl:create-path "bass-1.ogg" *audio-root*)))
    (setf (aref *soundfx* 1) (sdl-mixer:load-sample (sdl:create-path "bass-2.ogg" *audio-root*)))
    (setf (aref *soundfx* 2) (sdl-mixer:load-sample (sdl:create-path "laser-1.ogg" *audio-root*)))
    (setf (aref *soundfx* 3) (sdl-mixer:load-sample (sdl:create-path "explode-1.ogg" *audio-root*)))
    (setf (aref *soundfx* 4) (sdl-mixer:load-sample (sdl:create-path "explode-2.ogg" *audio-root*)))
    (setf (aref *soundfx* 5) (sdl-mixer:load-sample (sdl:create-path "explode-3.ogg" *audio-root*)))
    (setf (aref *soundfx* 6) (sdl-mixer:load-sample (sdl:create-path "level-up.ogg" *audio-root*)))
    (setf *mothership-engine* (sdl-mixer:load-music (sdl:create-path "mothership.ogg" *audio-root*)))
    (sample-finished-action)
    (sdl-mixer:allocate-channels 16)))


;;; SAMPLE-FINISHED-ACTION function

(defun sample-finished-action ()
  (sdl-mixer:register-sample-finished
   (lambda (channel)
     (declare (ignore channel))
     nil)))


;;;; CLEAN-UP function

(defun clean-up ()
  (when *mothership-engine*
    (when (sdl-mixer:music-playing-p)
      (sdl-mixer:Pause-Music)
      (sdl-mixer:Halt-Music))
    (sdl:Free *mothership-engine*)
    (setf *mothership-engine* nil))

  (when (sdl-mixer:sample-playing-p nil)
    (sdl-mixer:pause-sample t)
    (sdl-mixer:Halt-sample :channel t))

  (loop for s below (length *soundfx*)
     do (if (equal (aref *soundfx* s) 0)
	    t
	    (progn (sdl:free (aref *soundfx* s))
		   (setf (aref *soundfx* s) 0))))
  
  (when *mixer-opened*
    (sdl-mixer:Close-Audio t)
    (setf *mixer-opened* nil))
  (sdl-mixer:quit-mixer))


;;;; START function

(defun start ()

  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (sdl:window *game-width* *game-height* :title-caption "The Invaders")
    (setf (sdl:frame-rate) 60)

    (setup-audio)

    (load-sprite-sheet)
    (initialize-game)
    (reset-game)
    ;(sdl-mixer:play-music *music-intro* :loop t)

    (unless (sdl:initialise-default-font *terminus-ttf-18*)
      (error "FONT-EXAMPLE: Cannot initialize the default font."))

    (setf *ttf-font-small* (sdl:initialise-font *terminus-ttf-12*))
    (setf *ttf-font-normal* (sdl:initialise-font *terminus-ttf-18*))
    (setf *ttf-font-large* (sdl:initialise-font *terminus-ttf-24*))
    (setf *ttf-font-huge* (sdl:initialise-font *terminus-ttf-32*))
    
    (sdl:with-events ()
      (:quit-event ()
		   (clean-up)
		   t)
      (:key-down-event (:key key)
		       (case key
			 (:sdl-key-p (if (= *game-state* 1)
					 (pause-game)))
			 (:sdl-key-q (if (= *game-state* 1)
					 (change-game-state)))
			 (:sdl-key-z (if (= *game-state* 1)
					 (fire-shot *ship*)))
			 (:sdl-key-space (continue-option))
			 (:sdl-key-escape (sdl:push-quit-event))))
      (:key-up-event (:key key)
		     (case key))
      (:idle ()
	     (when (sdl:get-key-state :sdl-key-left) (move-ship *ship* 'left))
	     (when (sdl:get-key-state :sdl-key-right) (move-ship *ship* 'right))
	     (render)))))
