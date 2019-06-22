;baka
;;basic
(defun range (s e) 
  (let* ((n (+ (- e s) 1))
         (idx-lst nil))
    (let ((arr (make-array n)))
      (dotimes (i n)
        (setf (aref arr i) (+ s i)))
      arr)))

(defun assocdr (key alist)
  (cdr (assoc key alist)))

;; extended def of vector
(defun append-vector (vec x)
  (let* ((n (length vec))
         (vec-ret (make-array (+ n 1)
                              :element-type (send vec :element-type))))
     (dotimes (i n)
       (setf (aref vec-ret i) (aref vec i)))
     (setf (aref vec-ret n) x)
     (setq self vec-ret)))

;; math
(defun square (x) (* x x))

(defun dist (pos1 pos2)
  (let* (
         (x1 (elt pos1 0)) (y1 (elt pos1 1)) (z1 (elt pos1 1))
         (x2 (elt pos2 0)) (y2 (elt pos2 1)) (z2 (elt pos2 1))
         )
    (sqrt (+ 
            (square (- x1 x2))
            (square (- y1 y2))
            (square (- z1 z2))
            ))))

;; map 
(defun mapcar2 (fn lst1 lst2)
  (unless (= (length lst1) (length lst2)) (error "must be same length"))
  (let ((ret nil) (N (length lst1)))
    (dotimes (i N ret)
      (push (funcall fn (car lst1) (car lst2)) ret)
      (setq lst1 (cdr lst1))
      (setq lst2 (cdr lst2)))
    (nreverse ret)))

(defun mapvector (f v)(do ((i 0 (1+ i)))((>= i (length v)))(funcall f (aref v i))))
(defun vector-sum (v)(let ((i 0))(mapvector #'(lambda (x) (setq i (+ i x))) v)i)) ;; TODO remove

(defun mapcar-vector (func vec)
  (let* ((N (length vec))
         (ret (make-array N)))
    (dotimes (i N ret)
      (setf (aref ret i) (funcall func (aref vec i))))))

(defun mapc-vector (func vec)
  (let* ((N (length vec)))
    (dotimes (i N) (funcall func (aref vec i)))))

(defun mapc-idx-vector (func vec)
  (let* ((N (length vec)))
    (dotimes (i N) (funcall func i (aref vec i)))))
#|
(defun example-mapc-idx-vector ()
  (let* ((ret (make-array 10)))
    (mapc-idx-vector #'(lambda (idx x) (setf (aref ret idx) idx)) (make-array 10))
    ret))
|#

(defun vector->list (vec)
  (let ((lst-ret nil))
    (mapc-vector #'(lambda (x) (push x lst-ret)) vec)
    (nreverse lst-ret)
    lst-ret))

         
(defmethod vector 
  ;; I wish eus has type system...
  (:mapc (fn &rest args)
   (let ((nargs (length args))) 
     (case nargs
       (0 (send self :_mapc-all fn)); all
       (1 (send self :_mapc-idx-lst fn (car args))); index list
       (2 (send self :_mapc-slice fn (car args) (cadr args)))))) ; slice

  (:_mapc-all (fn)
   (send self :_mapc-slice fn 0 (- (length self) 1)))

  (:_mapc-slice (fn s e)
   (send self :_mapc-idx-lst fn (range s e)))

  (:_mapc-idx-lst (fn vec)
   (when (listp vec) (setq vec (apply #'vector vec)))
   (mapc-vector #'(lambda (idx) (setf (aref self idx) (funcall fn (aref self idx))))
                vec))
  )

(defmethod vector
  (:sum ()
   (let ((res 0))
     (dotimes (i (length self))
       (setq res (+ res [self i])))
     res))
  (:normalize ()
   (let ((vec-new (copy-object self))
         (sum (send self :sum)))
     (dotimes (i (length self))
       (setf [vec-new i] (/ [self i] sum)))
     vec-new))
  )


  

(defmethod vector 
  (:mapcar (fn &rest args)
   (let ((nargs (length args))) 
     (case nargs
       (0 (send self :_mapcar-all fn)); all
       (1 (send self :_mapcar-idx-lst fn (car args))); index list
       (2 (send self :_mapcar-slice fn (car args) (cadr args)))))) ; slice

  (:_mapcar-all (fn)
   (send self :_mapcar-slice fn 0 (- (length self) 1)))

  (:_mapcar-slice (fn s e)
   (send self :_mapcar-idx-lst fn (range s e)))

  (:_mapcar-idx-lst (fn vec-idx )
   (when (listp vec-idx) (setq vec-idx (apply #'vector vec-idx)))
   (let* ((N (length vec-idx))
          (vec-ret (make-array N)))
     (dotimes (i N)
       (setf (aref vec-ret i) (funcall fn (aref self (aref vec-idx i)))))
     vec-ret))
  )

#|
(setq a (vector 1 2 3))
(send a :mapc #'(lambda (x) -1) '(1 2))
(send a :mapc #'(lambda (x) 100) 0 1)
(send a :mapc #'(lambda (x) 1000))
(send a :mapcar #'(lambda (x) -1) '(1 2))
|#



         

