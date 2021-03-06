#!/usr/bin/env sbcl --dynamic-space-size 4Gb --script
;; #!/bin/bash # -*-Lisp-*-
;; #|
;; # http://speely.wordpress.com/2010/11/27/writing-scripts-with-common-lisp/ 
;; exec sbcl --dynamic-space-size 4Gb --script $0 "$@" # hashlife consumes much memory
;; exit
;; |#

;; for debug
;; (declaim (optimize (debug 3) (safety 3)
;;                    (speed 0) (space 0) (compilation-speed 0)))
(setf *print-circle* t)

(declaim (optimize (debug 0) (safety 0)
                   (speed 3) (space 3) (compilation-speed 0)))


;;;; cores
(defun life-rule (self count)
  (if (if self
	  (member count '(2 3))
	  (= count 3))
      1 0))

;; structs and utilities
(defstruct node
  nw ne sw se level id population board result)

(defstruct (board)
  root cache origin next-id empty-nodes zero one)

(defun hash4 (a b c d)
  (list a b c d))

(defmacro while (pred &body body)
  `(loop (unless ,pred (return))
      ,@body))

;; functions for node/board
(defun init-board ()
  (let ((b (make-board :cache (make-hash-table :test #'equalp)
		       :origin (cons 0 0))))
    (let ((zero (make-node :level 0 :id 0 :population 0 :board b))
	  (one  (make-node :level 0 :id 1 :population 1 :board b))
	  (cache (board-cache b)))
      (loop for i below 16 do
	   (setf (gethash (hash4 (if (logbitp 0 i) 1 0)
				   (if (logbitp 1 i) 1 0)
				   (if (logbitp 2 i) 1 0)
				   (if (logbitp 3 i) 1 0)) cache)
		 (make-node :level 1 :id (+ i 2)
			    :population (logcount i)
			    :board b
			    :nw (if (logbitp 0 i) one zero)
			    :ne (if (logbitp 1 i) one zero)
			    :sw (if (logbitp 2 i) one zero)
			    :se (if (logbitp 3 i) one zero)
			    :result (vector nil))))
      (setf (board-zero b)        zero
	    (board-one  b)        one
	    (board-next-id b)     18
	    (board-root b)        zero
	    (board-empty-nodes b) (list zero))
      b)))

(defun board-get-node (b nw ne sw se)
  (let ((key (hash4 (node-id nw) (node-id ne) (node-id sw) (node-id se))))
    (unless (gethash key (board-cache b))
      (setf (gethash key (board-cache b))
	    (make-node :nw nw :ne ne :sw sw :se se :level (1+ (node-level nw))
		       :id (incf (board-next-id b))
		       :population (reduce #'+ (mapcar #'node-population (list nw ne sw se)))
		       :board b
		       :result (coerce (loop for i below (1+ (node-level nw)) collect nil) 'vector))))
    (gethash key (board-cache b))))

(defun board-get-empty-node (b level)
  (declare (fixnum level))
  (if (< level (the fixnum (length (board-empty-nodes b))))
      (nth level (board-empty-nodes b))
      (let ((e (board-get-empty-node b (1- level))))
	(board-get-node b e e e e))))

;; subnode accessor shorthands

; quarter quarter cells
(defmacro defnn__ ()
  "define sub-sub-quad accessors:
nn00 nn01 nn02 nn03
nn04 nn05 nn06 nn07
nn08 nn09 nn10 nn11
nn12 nn13 nn14 nn15"
  `(progn
     ,@(let ((l '(nw ne sw se)))
	    (loop for i below 4 append
		 (loop for j below 4 collect
		      `(defun ,(intern (format nil "NN~2,'0d" (+ (* 1 (mod   i 2))
							     (* 4 (floor i 2))
							     (* 2 (mod   j 2))
							     (* 8 (floor j 2))))) (n)
			 (,(intern (format nil "NODE-~a" (nth i l))) (,(intern (format nil "NODE-~a" (nth j l))) n))))))))
(defnn__)
(defun sub-sub-quad-list (n)
  (list (nn00 n) (nn01 n) (nn02 n) (nn03 n)
	(nn04 n) (nn05 n) (nn06 n) (nn07 n)
	(nn08 n) (nn09 n) (nn10 n) (nn11 n)
	(nn12 n) (nn13 n) (nn14 n) (nn15 n)))

; quarter cells
(defun node-get-subquad (n x y)
  (let ((b (node-board n)))
    (case y
      (0 (case x
	   (0 (node-nw n))
	   (1 (board-get-node b (nn01 n) (nn02 n) (nn05 n) (nn06 n)))
	   (2 (node-ne n))))
      (1 (case x
	   (0 (board-get-node b (nn04 n) (nn05 n) (nn08 n) (nn09 n)))
	   (1 (board-get-node b (nn05 n) (nn06 n) (nn09 n) (nn10 n)))
	   (2 (board-get-node b (nn06 n) (nn07 n) (nn10 n) (nn11 n)))))
      (2 (case x
	   (0 (node-sw n))
	   (1 (board-get-node b (nn09 n) (nn10 n) (nn13 n) (nn14 n)))
	   (2 (node-se n)))))))

(defmacro defn_ ()
  "define (mutually overlapping) sub-quad accessors:
n0 n1 n2
n3 n4 n5
n6 n7 n8"
  `(progn ,@(loop for i below 3 append
		 (loop for j below 3 collect
		      `(defun ,(intern (format nil "N~d" (+ j (* 3 i)))) (n)
			 (node-get-subquad n ,j ,i))))))
(defn_)
(defun sub-quad-list (n)
  (list (n0 n) (n1 n) (n2 n)
	(n3 n) (n4 n) (n5 n)
	(n6 n) (n7 n) (n8 n)))

(defun node-children (n)
  (list (n0 n) (n2 n)
	(n6 n) (n8 n)))

;; node functions
(defun node-width (n)
  (ash 1 (node-level n)))

(defun node-step-size (n)
  (ash 1 (- (node-level n) 2)))

(defun node-get (n x y)
  (cond ((or (< x 0) (< y 0)
	     (>= x (node-width n)) (>= y (node-width n))) 0)
	((zerop (node-level n)) (node-id n))
	(t (let ((half (ash (node-width n) -1)))
	     (if (< x half)
		 (if (< y half)
		     (node-get (node-nw n) x y)
		     (node-get (node-sw n) x (- y half)))
		 (if (< y half)
		     (node-get (node-ne n) (- x half) y)
		     (node-get (node-se n) (- x half) (- y half))))))))

(defun node-get-list (n origx origy &optional rect)
  (let ((width (node-width n))
	(half (ash (node-width n) -1)))
    (when rect
      (destructuring-bind (x0 y0 x1 y1) rect
	(when (or (< x1 origx) (< y1 origy)
		  (<= (+ origx width) x0) (<= (+ origy width) y0))
	  (return-from node-get-list nil))))
    (cond
      ((zerop (node-level n))
       (if (= 1 (node-id n))
	   (list (list origx origy))
	   nil))
      (t (append
	  (node-get-list (node-nw n) origx origy rect)
	  (node-get-list (node-ne n) (+ origx half) origy rect)
	  (node-get-list (node-sw n) origx (+ origy half) rect)
	  (node-get-list (node-se n) (+ origx half) (+ origy half) rect))))))

(defun node-set (n new origx origy x y)
  (let ((width (node-width n))
	(half  (ash (node-width n) -1))
	(b     (node-board n)))
    (if (or (< x origx) (< y origy)
	    (<= (+ origx width) x) (<= (+ origy width) y))
	(error "node-set out of range: ~a" (list x y))
	(if (zerop (node-level n))
	    (if new (board-one b) (board-zero b))
	    (let ((nw (node-nw n))
		  (ne (node-ne n))
		  (sw (node-sw n))
		  (se (node-se n)))
	      (if (< y (+ origy half))
		  (if (< x (+ origx half))
		      (board-get-node b (node-set nw new origx origy x y) ne sw se)
		      (board-get-node b nw (node-set ne new (+ origx half) origy x y) sw se))
		  (if (< x (+ origx half))
		      (board-get-node b nw ne (node-set sw new origx (+ origy half) x y) se)
		      (board-get-node b nw ne sw (node-set se new (+ origx half) (+ origy half) x y)))))))))

(declaim (ftype (function (node fixnum) node) node-next-center))
(defun node-next-center% (n step)
  (cond ((zerop step) (n4 n))
	((<= step (node-step-size n))
	 (if (= (node-level n) 2)
	     (let* ((l   (sub-sub-quad-list n))
		    (b   (node-board n))
		    (one (board-one b))
		    (ids (mapcar #'life-rule
				 (loop for x in '(5 6 9 10) collect
				      (eq (nth x l) one))
				 (loop for x in '(5 6 9 10) collect
				      (count one (loop for dx in '(-5 -4 -3 -1 1 3 4 5) collect
						      (nth (+ x dx) l)))))))
	       (gethash (apply #'hash4 ids) (board-cache b)))
	     (let* ((b (node-board n))
		    (halfstep (ash (node-step-size n) -1))
		    (halfstepp (>= step halfstep))
		    (remain (if halfstepp (- step halfstep) step)))
	       (let ((nexts (mapcar
			     (if halfstepp (lambda (n) (node-next-center n halfstep)) #'n4)
			     (sub-quad-list n))))
		 (destructuring-bind (n0 n1 n2 n3 n4 n5 n6 n7 n8) nexts
		   (board-get-node b
				   (node-next-center (board-get-node b n0 n1 n3 n4) remain)
				   (node-next-center (board-get-node b n1 n2 n4 n5) remain)
				   (node-next-center (board-get-node b n3 n4 n6 n7) remain)
				   (node-next-center (board-get-node b n4 n5 n7 n8) remain)))))))
	(t (error "something went wrong"))))

(defun node-next-center (n step)
  (let ((ind (integer-length step)))	;step == 2^(ind-1), or zero
    (unless (aref (node-result n) ind)
      (setf (aref (node-result n) ind)
	    (node-next-center% n step)))
    (aref (node-result n) ind)))
      

(defun board-trim% (b)
  "trim board. return non-nil if success"
  (let ((pop (node-population (board-root b))))
    (cond
      ((zerop pop)
       (prog1
	   (not (zerop (node-level (board-root b))))
	 (setf (board-root b) (board-zero b)
	       (board-origin b) (cons 0 0))))
      ((< (node-level (board-root b)) 2) nil)
      (t
       (let* ((subquads (sub-quad-list (board-root b)))
	      (pos (position pop (mapcar #'node-population subquads))))
	 (when pos
	   (setf (board-root b) (nth pos subquads))
	   (incf (car (board-origin b)) (* (mod   pos 3) (ash (node-width (board-root b)) -1)))
	   (incf (cdr (board-origin b)) (* (floor pos 3) (ash (node-width (board-root b)) -1)))))))))

(defun board-trim (b)
  (while (board-trim% b)))

(defun board-collect (b)
  (board-trim b)
  (let ((old (board-cache b)))
    (setf (board-empty-nodes b) (list (board-zero b))
	  (board-cache b) (make-hash-table :test #'equalp))
    (labels ((canonicalize (n to)
	       (if (< (node-id n) 16)
		   n
		   (let ((key (mapcar #'node-id (node-children n))))
		     (setf (gethash key to)
			   (board-get-node b
			      (canonicalize (node-nw n) to)
			      (canonicalize (node-ne n) to)
			      (canonicalize (node-sw n) to)
			      (canonicalize (node-se n) to)))))))
      (loop for i below 16 do
	   (let ((key (hash4 (if (logbitp 0 i) 1 0)
			       (if (logbitp 1 i) 1 0)
			       (if (logbitp 2 i) 1 0)
			       (if (logbitp 3 i) 1 0))))
	     (setf (gethash key (board-cache b))
		   (gethash key old))))
      (setf (board-root b)
	    (canonicalize (board-root b) (board-cache b))))))


(defun board-double (b)
  (let ((n (board-root b)))
    (if (zerop (node-level n))
	(progn
	  (decf (car (board-origin b)) 1)
	  (decf (cdr (board-origin b)) 1)
	  (setf (board-root b)
		(if (zerop (node-population n))
		    (board-get-empty-node b 1)
		    (gethash (hash4 0 0 0 1) (board-cache b)))))
	(let ((e (board-get-empty-node b (1- (node-level (board-root b))))))
	  (decf (car (board-origin b)) (ash (node-width n) -1))
	  (decf (cdr (board-origin b)) (ash (node-width n) -1))
	  (setf (board-root b)
		(board-get-node b (board-get-node b e e e (node-nw n))
				(board-get-node b e e (node-ne n) e)
				(board-get-node b e (node-sw n) e e)
				(board-get-node b (node-se n) e e e)))))
    b))
	
    

(defun board-clear (b)
  (setf (board-root b) (board-zero b)
	(board-origin b) (cons 0 0)))

(defun board-get (b x y)
  (let ((r (board-root b)))
    (node-get r (- x (car (board-origin b))) (- y (cdr (board-origin b)) ))))

(defun board-get-all (b rect)
  (let ((r (board-root b)))
    (node-get-list r (car (board-origin b)) (cdr (board-origin b)) rect)))

(defun node-within (n x y)
  (and (<= 0 x) (< x (node-width n))
       (<= 0 y) (< y (node-width n))))

(defun board-within (b x y)
  (node-within (board-root b) (- x (car (board-origin b))) (- y (cdr (board-origin b)))))

(defun board-set (b new x y)
  (while (not (board-within b x y))
    (board-double b))
  (setf (board-root b)
	(node-set (board-root b) new
		  (car (board-origin b)) (cdr (board-origin b))
		  x y)))

(defun board-print-rect (b rect)
  (destructuring-bind (x0 y0 x1 y1) rect
  (let ((poss (board-get-all b rect)))
    (loop for y from y0 to y1
          for xs = (remove-if-not (lambda (p) (= y (cadr p))) poss) do
	 (progn (fresh-line)
		(loop for x from x0 to x1 do
		     (princ
		      (if (find x xs :key  #'car)
			  #\o #\.))))))))

(defun board-step (b step)
  (while (or (< (node-step-size (board-root b)) step)
	     (zerop (node-level (board-root b))))
    (board-double b))
  (let ((diff (node-width (board-root b))))
    (board-double b)
    (board-double b)
    (incf (car (board-origin b)) diff)
    (incf (cdr (board-origin b)) diff)
    (setf (board-root b) (node-next-center (board-root b) step))))

#|
(defparameter *b* (init-board))
(board-clear *b*) (mapcar (lambda (pos) (board-set *b* t (car pos) (cadr pos))) '((0 0) (0 -1) (-1 -2) (-1 0) (-2 0))) (board-print-rect *b* '(-2 -2 2 2))
(board-step *b* 4)(board-print-rect *b* '(-2 -2 2 2))
(board-step *b* 4)(board-print-rect *b* '(-2 -2 2 2))

|#

;;;; gui codes

(eval-when (:execute :compile-toplevel :load-toplevel)
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
					 (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
)
(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload :lispbuilder-sdl)
  (ql:quickload :split-sequence))

(defun reverse-shape-v (shape)
  (reverse shape))

(defun reverse-shape-h (shape)
  (mapcar #'reverse shape))

(defun reverse-shape-vh (shape)
  (mapcar #'reverse (reverse shape)))

(defun rotate-shape (shape)
  "rotate shape to right by 90 degrees"
  (mapcar (lambda (l) (coerce l 'string))
	  (mapcar #'nreverse
		 (apply #'mapcar #'list
			(loop for l in shape collect
			     (loop for c across l collect c))))))

(defun board-put-shape (b shape x0 y0)
  (loop for y from y0
     for l in shape
     do (loop for x from x0
	   for c across l
	   do (board-set b (char= c #\O) x y))))

(defparameter *board* nil)

(defun life (world-width world-height scale drawer)
  (setf *board* (init-board))
  (let ((scale scale)
	(w (* world-width scale))
	(h (* world-height scale))
	(origx 0)
	(origy 0)
	(mouse-state (make-hash-table))
	(prevpos nil)
	(update t)
	(stopping nil)
	(currently-fullscreen? nil)
	(video-w)
	(video-h)
	(step-size 1)
	(generation 0))

    (let ((result (funcall drawer)))
      (when result
	(destructuring-bind (x0 y0 x1 y1) result
	  (setf origx (floor (- (+ x0 x1) world-width) 2)
		origy (floor (- (+ y0 y1) world-height) 2)))))
	 
    (labels ((get-rect ()
	       (list origx origy (+ origx (floor w scale)) (+ origy (floor h scale))))
	     (board-coodinate (x y)
	       (list (+ origx (floor x scale))
		     (+ origy (floor y scale))))
	     (display-coodinate (x y)
	       (list (* scale (- x origx))
		     (* scale (- y origy))))
	     (toggle-fullscreen ()
	        (if currently-fullscreen?
		    (progn
		      (setf w (car currently-fullscreen?)
			    h (cdr currently-fullscreen?)
			    currently-fullscreen? nil
			    world-width  (floor w scale)
			    world-height (floor h scale))
		      (sdl:resize-window w h :sw t :resizable t))
		  (progn 
		      (setf currently-fullscreen? (cons w h)
			    w video-w
			    h video-h
			    world-width  (floor w scale)
			    world-height (floor h scale))
		      (sdl:resize-window w h :sw t :fullscreen t))))
	     (draw-cell (x y color)
	       (if (> scale 1)
		   (sdl:draw-box (sdl:rectangle :x (floor (* scale (- x origx)))
						:y (floor (* scale (- y origy)))
						:w (round scale)
						:h (round scale))
				 :color color)
		   (sdl:draw-pixel-* (floor (* scale (- x origx)))
				     (floor (* scale (- y origy)))
				     :color color)))
	     (draw-node (n localorigx localorigy) ; draw recursively
	       (when (and (not (zerop (node-population n)))
		          (< localorigx (+ origx world-width))
			  (< localorigy (+ origy world-height))
			  (< origx (+ localorigx (node-width n)))
			  (< origy (+ localorigy (node-width n))))
		 (if (or (> (min 1 scale) (ash 1 (node-level n)))
			 (zerop (node-level n)))
		     (draw-cell localorigx localorigy sdl:*white*)
		     (let ((half (ash (node-width n) -1)))
		       (draw-node (node-nw n) localorigx localorigy)
		       (draw-node (node-ne n) (+ localorigx half) localorigy)
		       (draw-node (node-sw n) localorigx (+ localorigy half))
		       (draw-node (node-se n) (+ localorigx half) (+ localorigy half))))))
)
      (sdl:with-init (sdl:sdl-init-video)
	(destructuring-bind (w h) (coerce (sdl:video-dimensions) 'list)
	  (setf video-w w video-h h))
	(sdl:window w h :sw t :async-blit t :title-caption "life" :resizable t)
	(setf (sdl:frame-rate) 60)
	(sdl:enable-key-repeat 500 20)

	(sdl:with-events ()
	  (:quit-event () t)
	  (:video-resize-event (:w w_ :h h_)
			       (setf w w_ h h_
				     world-width  (floor w scale)
				     world-height (floor h scale))
			       (sdl:resize-window w h)) ;'window size' is logical (pixel) size?
	  (:key-down-event (:key key :mod mod)
			   ;; (fresh-line) (princ mod) (princ key) (finish-output)
			   (case key
			     (:sdl-key-q    (sdl:push-quit-event))
			     (:sdl-key-f    (toggle-fullscreen))
			     (:sdl-key-space (setf stopping (not stopping)))
			     (:sdl-key-c    (board-clear *board*))
			     (:sdl-key-up
			      (cond
			       ((not (zerop (logand 3 mod))) ; shift
				(setf step-size (* step-size 2)))
			       ((not (zerop (logand 192 mod))) ; ctrl
				(setf scale (* scale 2)
				      origx (+ origx (floor (/ w scale) 2))
				      origy (+ origy (floor (/ h scale) 2))
				      world-width  (floor w scale)
				      world-height (floor h scale)))
			       (t
				(decf origy (floor (/ 50 scale))))))
			     (:sdl-key-down
			      (cond
			       ((not (zerop (logand 3 mod)))
				(when (not (= step-size 1))
				  (setf step-size (/ step-size 2))))
			       ((not (zerop (logand 192 mod)))
				(setf scale (/ scale 2)
				      origx (- origx (floor (/ w scale) 4))
				      origy (- origy (floor (/ h scale) 4))
				      world-width  (floor w scale)
				      world-height (floor h scale)))
			       (t
				(incf origy (floor (/ 50 scale))))))
			     (:sdl-key-right (incf origx (floor (/ 50 scale))))
			     (:sdl-key-left (decf origx (floor (/ 50 scale))))
			     ))
	  (:mouse-button-down-event (:button button :x x :y y)
				    (setf (gethash button mouse-state) t
					  prevpos (board-coodinate x y))
				    (when (= button sdl:sdl-button-left)
				      ;; (format t "~&!1") (finish-output)
				      (setf update nil)
				      (destructuring-bind (x y) (board-coodinate x y)
					(board-set *board* t x y))))
	  (:mouse-button-up-event (:button button)
				  (setf (gethash button mouse-state) nil
					prevpos nil)
				  (when (= button sdl:sdl-button-left)
				    (setf update t)))
	  (:mouse-motion-event (:x x :y y)
	    (when (gethash sdl:sdl-button-left mouse-state)
	      (destructuring-bind (x y) (board-coodinate x y)
		(if (and prevpos (not (and (= x (car  prevpos))
					   (= y (cadr prevpos)))))
		    (destructuring-bind (x0 y0) prevpos
		      (if (> (abs (- x x0)) (abs (- y y0)))
			  (loop for dx to (abs (- x x0))
			        for xx =  (abs (- x x0))
			     do (board-set
				 *board* t
				 (+ x0 (* (- x x0) (/ dx xx)))
				 (+ y0 (* (- y y0) (/ dx xx)))))
			  (loop for dy to (abs (- y y0))
			        for yy =  (abs (- y y0))
			     do (board-set
				 *board* t
				 (+ x0 (* (- x x0) (/ dy yy)))
				 (+ y0 (* (- y y0) (/ dy yy)))))))
		    (board-set *board* t x y))
		(setf prevpos (list x y)))))
	  (:idle ()
		 (sdl:clear-display sdl:*black*)
		 (when (and update (not stopping))
		     (board-step *board* step-size)
		     (if (> (node-level (board-root *board*)) 20)
		       (board-collect *board*)
		       (board-trim *board*))
		     (incf generation step-size)
		     (sdl:set-caption (format nil "life: generation ~A, step 2^~A, population: ~A" generation (1- (integer-length step-size)) (node-population (board-root *board*))) nil))
		 (draw-node (board-root *board*)
			    (car  (board-origin *board*))
			    (cdr (board-origin *board*)))
		 (sdl:update-display)))))))

(compile 'life)

(defparameter *puffer-train*
  '(".OOO...........OOO"
    "O..O..........O..O"
    "...O....OOO......O"
    "...O....O..O.....O"
    "..O....O........O."))

(defparameter gun-lu
              '(
                ".................................."
                "...............OOO..............OO"
                "...............O...O............OO"
                "...............O....O............."
                "................O...O............."
                ".................................."
                ".OO.............O...O............."
                ".OO............O....O............."
                "...............O...O............OO"
                "...............OOO..............OO"
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                "..O.....O........................."
                ".OOO...OOO........................"
                "OO.O...O.OO......................."
                ".................................."
                ".................................."
                "...O...O.........................."
                "...O...O.........................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".OO.....OO........................"
                ".OO.....OO........................"
                ))

;; 新しい銃 右下方向に発射  (発射のタイミングがちょっと違う)
(defparameter gun-rd
              '(
                "........................OO.....OO."
                "........................OO.....OO."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".......................OO.O...O.OO"
                ".......................O..O...O..O"
                "........................OOO...OOO."
                ".................................."
                ".................................."
                ".................................."
                ".................................."
                ".................O................"
                "OO...............OO..............."
                "OO................OO.............."
                ".............OO..OO............OO."
                "...............................OO."
                ".................................."
                ".................................."
                ".............OO..OO..............."
                "OO................OO.............."
                "OO...............OO..............."
                ".................O................"
                ))

;; 新しい銃 右上方向に発射 (gun-rd を上下反転したもの)
(defparameter gun-ru (reverse-shape-v gun-rd))

;; 宇宙船を食うやつ
(defparameter eater
              '(
                "OO.."
                "O.O."
                "..O."
                "..OO"
                ))

;; ブリンカー
(defparameter blinker
              '("..O.."
                "..O.."
                "..O.."
                ))

(defparameter *r-pentomino*
  '(".OO"
    "OO."
    ".O."))

(defparameter *glider*
  '("..O"
    "O.O"
    ".OO"))

(defparameter *pentadecathlon*
  '("OOOOOOOOOO"))

(defun drawer-shape (shapes)
  (lambda ()
    (loop for (x y s) in shapes do
	 (board-put-shape *board* s x y))))

(defun args ()
  #+clozure (let ((a (ccl::command-line-arguments)))
	      (nthcdr (position "--" a :test #'string-equal) a))
  #+sbcl sb-ext:*posix-argv*)

(defun run ()
    ;; ;; r-pentomino
    ;; (life 400 400 3 (list (list 200 200 *r-pentomino*)))
    ;; ;; glider
    ;; (life 100 100 3 (list (list 0 0 *glider*)))
    ;; ;; gliderguns
    ;; (life 240 160 3
    ;;       `((20 10 ,gun-rd)
    ;;         (23 101 ,gun-ru)
    ;;         (96 101 ,gun-lu)
    ;;         (220 75 ,eater)
    ;;         ))
    ;; ;; puffer-train
    ;; (life 1900 400 1 (drawer-shape
    ;; 		      (list (list 200 200
    ;; 				  (rotate-shape *puffer-train*)))))
    ;; ;; pentadecathlon
    ;; (life 20 20 3 `((5 10 ,*pentadecathlon*)))
  (life 800 600 1
	(cond ((null (cadr (args)))
	       ;; puffer train
	       (drawer-shape
		(list (list 200 200
			    (rotate-shape *puffer-train*)))))
	      ((string-equal "rle" (pathname-type (cadr (args))))
	       (lambda ()
		 (read-rle *board* (cadr (args)))))
	      ((string-equal "lif" (pathname-type (cadr (args))))
	       (lambda ()
		 (read-lif *board* (cadr (args)))))
	      ((string-equal "mc" (pathname-type (cadr (args))))
	       (lambda ()
		 (read-mc *board* (cadr (args)))))
	      (t (error "not know file type: ~a" (cadr (args))))))
    )



(defun read-a-value (s)
  (let ((c (read-char s nil :eof))
	(num 1)
	(chars nil)
	(add 0)
	(state nil))
    (case c
      (:eof nil)
      (#\Newline (read-a-value s))
      (t
       ;; number
       (while (char<= #\0 c #\9)
	 (push c chars)
	 (setf c (read-char s)))
       (when chars
	 (setf num (parse-integer (coerce (nreverse chars) 'string))
	       chars nil))
       ;; state
       (when (char<= #\p c #\y)
	 (setf add (* 24 (- (char-int c) (char-int #\o))))
	 (setf c (read-char s)))
       (setf state
	     (case c
	       ((#\b #\.) 0)
	       ((#\o #\*) 1)		; #\* is for .mc file
	       (#\$ :eol)
	       (#\! nil)
	       (t (+ 1 add (- (char-int c) (char-int #\A))))))
       (cons state num)))))

(defun read-rle (board filename)
  (let ((state 'header)
	x y curx cury maxx)
    (with-open-file (s filename)
      (loop for l = (read-line s nil nil)
	 while l do
	   (unless (or (zerop (length l))
		     (char= (char l 0) #\#))
	     (case state
	       (header
		(let* ((specs (mapcar
			       (lambda (s) (split-sequence:split-sequence #\= s))
			       (split-sequence:split-sequence
				#\, (remove #\space l)))))
		  ;; (print specs)
		  (when (not (string-equal
			      "B3/S23"
			      (cadr (assoc "rule" specs :test #'string-equal))))
		    (error "can't use rule ~A"
			   (cadr (assoc "rule" specs :test #'string-equal))))
		  (setf x (parse-integer
			   (cadr (assoc "x" specs :test #'string-equal)))
			y (parse-integer
			   (cadr (assoc "y" specs :test #'string-equal)))
			curx x cury y maxx x)
		  (setf state 'body)))
	       (body
		(with-input-from-string (s l)
		  (loop for r = (read-a-value s)
		       while r do
		       (case (car r)
			 (:eol (setf maxx (max maxx curx)
				     curx x)
			       (incf cury (cdr r)))
			 (0 (incf curx (cdr r)))
			 (1 (dotimes (i (cdr r))
			      (board-set board t curx cury)
			      (incf curx)))))))))))
    (list x y maxx cury)))

(defun read-lif (board filename)
  (let ((x 0) (y 0) (curx 0) (cury 0) minx miny maxx maxy)
    (with-open-file (s filename)
      (loop for l = (read-line s nil nil)
	 while l do
	   (unless (or (zerop (length l)))
	     (if (char= #\# (char l 0))
		 (when (char= #\P (char l 1))
		   (when (= 3 (length (split-sequence:split-sequence #\Space l)))
		     (destructuring-bind (_ x_ y_)
			 (split-sequence:split-sequence #\Space l)
		       (declare (ignore _))
		       (setf x (parse-integer x_)
			     y (parse-integer y_)
			     curx x cury y))))
		 (with-input-from-string (s l)
		   (loop named charloop for r = (read-char s nil :eol) do
			(progn
			  (when (not minx)
			    (setf minx x miny y maxx x maxy y))
			  (case r
			    (:eol (setf maxx (max maxx curx)
					curx x
					maxy (max maxy cury))
				  (incf cury)
				  (return-from charloop))
			    (#\. (incf curx))
			    (#\* (board-set board t curx cury)
				 (incf curx))
			    (t (error "strange charactor found: ~a" r))))))))))
    (list minx miny maxx maxy)))

(defun get-2x2 (board id)
  (gethash (hash4 (if (logbitp 0 (- id 2)) 1 0)
		  (if (logbitp 1 (- id 2)) 1 0)
		  (if (logbitp 2 (- id 2)) 1 0)
		  (if (logbitp 3 (- id 2)) 1 0)) (board-cache board)))

(defun safe-node-id (n?)
  (if (node-p n?)
      (node-id n?)
      n?))

(defun safe-node-level (n?)
  (if (node-p n?)
      (node-level n?)
      n?))

(defun read-mc (board filename)
  (let ((nodes (list :dummy)))
    (with-open-file (s filename)
      (loop for l = (read-line s nil nil)
	 while l do
	   (unless (or (zerop (length l)) (char= #\[ (char l 0)))
	     (if (char= #\# (char l 0))
		 nil
		 (with-input-from-string (s l)
		   ;; (print l) (print (mapcar #'safe-node-id nodes)) (finish-output)
		   (if (member (peek-char nil s) '(#\. #\* #\$))
		       (push
			(let ((a (make-array '(4 4) :initial-element 2))
			      (x 0) (y 0))
			  (loop named charloop for r = (read-a-value s)
			     while r do
			       (case (car r)
				 (:eol (setf x 0) (incf y))
				 (0 (incf x))
				 (1 (incf (aref a (floor x 2) (floor y 2))
					  (ash 1 (+ (mod x 2) (* 2 (mod y 2)))))
				    (incf x))))
			  (let ((nw (board-get-node board
						    (get-2x2 board (aref a 0 0))
						    (get-2x2 board (aref a 1 0))
						    (get-2x2 board (aref a 0 1))
						    (get-2x2 board (aref a 1 1))))
				(ne (board-get-node board
						    (get-2x2 board (aref a 2 0))
						    (get-2x2 board (aref a 3 0))
						    (get-2x2 board (aref a 2 1))
						    (get-2x2 board (aref a 3 1))))
				(sw (board-get-node board
						    (get-2x2 board (aref a 0 2))
						    (get-2x2 board (aref a 1 2))
						    (get-2x2 board (aref a 0 3))
						    (get-2x2 board (aref a 1 3))))
				(se (board-get-node board
						    (get-2x2 board (aref a 2 2))
						    (get-2x2 board (aref a 3 2))
						    (get-2x2 board (aref a 2 3))
						    (get-2x2 board (aref a 3 3)))))
			    (board-get-node board nw ne sw se)))
			nodes)
		       (let* ((tmp
			       (mapcar #'parse-integer
				       (split-sequence:split-sequence #\space l)))
			      (level (car tmp))
			      (children (mapcar
					 (lambda (x)
					   (if (zerop x)
					       (board-get-empty-node board (1- level))
					       (nth (- (length nodes) x 1) nodes)))
					 (cdr tmp))))
			 ;; (print (mapcar #'safe-node-id children))
			 ;; (princ (mapcar #'safe-node-level children))
			 (push (apply #'board-get-node board children) nodes))))))))
    (setf (board-root   board) (car nodes)
	  (board-origin board) '(0 . 1))

    (list 0 1
	  (ash 1 (node-level (board-root board)))
	  (1+ (ash 1 (node-level (board-root board)))))))

;; (print (args))
(defun main ()
  (cffi:define-foreign-library sdl
      (:darwin (:or (:framework "SDL")
		    (:default "libSDL")))
    (:windows "SDL.dll")
    (:unix (:or "libSDL-1.2.so.0.7.2"
		"libSDL-1.2.so.0"
		"libSDL-1.2.so"
		"libSDL.so"
		"libSDL")))
  
    #+sbcl(sb-int:with-float-traps-masked (:invalid) (run))
    #-sbcl (run)
    )

(main)

;; /hashlife.lisp ~/Downloads/golly-2.6-mac109/Patterns/Life/Breeders/LWSS-breeder.rle

;; sbcl --script hashlife.lisp ~/Downloads/golly-2.6-mac109/Patterns/Life/Breeders/LWSS-breeder.rle
;; ccl-single-thread -l hashlife.lisp -e '(quit)' -- ~/Downloads/golly-2.6-mac109/Patterns/Life/Breeders/LWSS-breeder.rle
