
;; Обходы графов Суслякова Майя, Палицкая Софья, Филков Роман

(defun main ()
    (let* ((g (input (ask "Введите граф")))
           (v (gensym 'w)) 
           (y (ask "Введите тип обхода 1. Ширина 2. Глубина"))
           (b (input (ask "Введите стартовую вершину"))))
           (window! 300 300 v _white)
           (rebro! g (get-coords g) v _black)
           (circle (get-coords g) v _black)

           (sleep* 3000)

       (cond ((eq y "1") (bfs g (get-coords g) (list b) (list b) v))
             ((eq y "2") (dts g (get-coords g) b v))
             (t 'ok))
       (rebro! g (get-coords g) v _black)
       (circle (get-coords g) v _black)))

(defun get-next-b (g curr chk)
   (let* ((tmp1 (remove-if-not (lambda (p) (member curr p)) g))
          (tmp2 (apply 'append tmp1))) 
     (remove-if (lambda (x) (or (eq x curr) (member x chk))) tmp2)))

;;(defun bfs (g a que chk w)
;;      (cond ((null que) 'ok)
;;             (t (let* ((curr (car (last que))) 
;;                       (nxt (get-next-b g curr chk)))
;;                 (if (null nxt) (bfs g a (butlast que) chk w)
;;                    (progn (iter (for v in nxt)(rebro a curr v w _red ) (circle a w _black)(sleep* 500)
;;                           (bfs g a (append nxt (butlast que))(append nxt chk) w))))))))

(defun bfs (g a que chk w)
      (cond ((null que) 'ok)
             (t (let* ((curr (car que)) 
                       (nxt (get-next-b g curr chk)))
                 (if (null nxt) (bfs g a (cdr que) chk w)
                    (progn (iter (for v in nxt)(rebro a curr v w _red ) (circle a w _black)(sleep* 500))
                           (bfs g a (append (cdr que) nxt)(append nxt chk) w)))))))

(defun window! (x y v color)
      (grwCreate v x y "" color)
      (grwScale v -100 100 -100 100)
      (grwSetParm v 3 1 _WHITE) 
      (grwShow v 5 5 ))

(defun rebro! (g a v color)
  (cond ((null g) 'ok)
  (t (rebro a (nth 0 (car g)) (nth 1 (car g)) v color) (rebro! (cdr g) a v color))))

(defun coord! (a n)
       (cond (( null a) nil)
       (t (if (eq (car  (car a)) n) (car a) (coord! (cdr a) n)))))

(defun circle (a v color)
      (cond ((null a) 'ok)
            (t (grwSetParm v 3 0 _white _white)
               (grwCircle v (nth 1 (car a)) (nth 2 (car a)) 10 color)
               (grwPrint v (- (car (cdr (car a))) 5) (+ 5 (car (last (car a)))) (car (car a)) color)
               (circle (cdr a) v color))))

(defun  get-verts (g)
   (setof (remove-if  'numberp (apply 'append g))))

(defun get-coords (G)
  (LET* ((VERTS (GET-VERTEX G)) 
         (NV (LENGTH VERTS))
         (FI (/ (* 2 3.14159265358979) NV))
         (X 0)
         (Y 0)
         (RES NIL))
     (ITER (FOR V IN VERTS) (FOR I UPFROM 0)
        (COLLECTING (LIST V (* 80 (COS (* I FI))) (* 80 (SIN (* I FI)))) INTO RES)))) RES))

(defun rebro (A X Y V COLOR) 
   (GRWLINE V (NTH 1 (COORD! A X)) (NTH 2 (COORD! A X)) (NTH 1 (COORD! A Y)) (NTH 2 (COORD! A Y)) COLOR))

(defun get-vertex (g)
  (setof (apply 'append g)))

(defun dts (g a curr v &optional (stk (list curr))
                             (chk (list curr)))
     (cond ((null stk) 'ok)
           (t (LET*  ((nxt (get-next g curr chk)))
              (if (null nxt) (dts g a (cadr stk) v (cdr stk) chk)
                             (progn 
                                (rebro a curr nxt v _red)
                                (sleep* 800)
                                (dts g a nxt v (cons nxt stk)
                                          (cons nxt chk))))))))

(defun get-next (G curr chk)
           (let* ((tmp1 (remove-if-not (lambda (p) (member curr p)) g))
                  (tmp2 (mapcar (lambda (p) (if (eq (car p) curr)
                                                (cadr p) (car p))) tmp1))
                  (tmp3 (remove-if (lambda (x) (member x chk)) tmp2)))
                  (car tmp3)))

(main)









 

