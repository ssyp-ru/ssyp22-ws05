
;; Визуализация графов Вера Личман

(defun av (g)
  (setof (apply 'append g)))

(defun drw ()
  (let* ((w (gensym 'w))
         (g (input (ask "Задайте граф")))
         (color (input (ask "Какого цвета?")))
         (verts (av g))
         (n (length verts))
         (f (/ (* 2 _pi) n))
         (x 0)
         (y 0)
         (x1 0)
         (y1 0)
         (n1 0)
         (n2 0)
         (pair 0))
    
    (grwCreate w 400 400 "" _WHITE)
    (grwscale w -100 100 -100 100)
    (grwSetParm w 3 1 _WHITE)
    (grwshow w 5 5)
    
    (iter (for i from 0 to (- n 1))
          (setq x (* 60 (cos (* i f))))
          (setq y (* 60 (sin (* i f))))
          (grwcircle w x y 5 color)
          (grwfill w x y color color)
          (grwprint w x (+ 12 y) (nth i verts) color))
    
    (iter (for p in g)
          (let* ((v1 (car p))
                (v2 (cadr p))
                (i1 (position v1 verts))
                (i2 (position v2 verts)))
            (setq x (* 60 (cos (* i1 f))))
            (setq y (* 60 (sin (* i1 f))))
            (setq x1 (* 60 (cos (* i2 f))))
            (setq y1 (* 60 (sin (* i2 f))))
            (grwline w x y x1 y1 color)))
      
     (loop (setq k (str2fix (ask "Задайте режим: 1-выбор вершины; 2-выбор ребра; 0-выход")))
           (cond ((= k 2) 

                 (setq pair (ask "Задайте ребро"))
                 ;;(when (= 0 (strLen pair)) (return 'ok))
           
                 (let* ((pair (input pair)) 
                        (m1 (car pair))
                        (m2 (cadr pair))
                        (j1 (position m1 verts))
                        (j2 (position m2 verts)))
        
                   (cond ((or (member (list m1 m2) g)  (member (list m2 m1) g))  

                          (setq x (* 60 (cos (* j1 f))))
                          (setq y (* 60 (sin (* j1 f))))
                          (setq x1 (* 60 (cos (* j2 f))))
                          (setq y1 (* 60 (sin (* j2 f))))

                          (grwcircle w x y 5 _black)
                          (grwfill w x y _black _black)
                          (grwcircle w x1 y1 5 _black)
                          (grwfill w x1 y1 _black _black)
                          (grwline w x y x1 y1 _black)
                          (sleep* 1000)
                          (grwcircle w x y 5 color)
                          (grwfill w x y color color)
                          (grwcircle w x1 y1 5 color)
                          (grwfill w x1 y1 color color)
                          (grwline w x y x1 y1 color))
    
                        (t (say "Вершины не связаны.")))))

                 ((= k 1)

                        (setq v (input (ask "Задайте вершину")))

                        (let ((pos (position v verts)))

                             (setq x (* 60 (cos (* pos f))))
                             (setq y (* 60 (sin (* pos f))))
                                                       
                             (grwcircle w x y 5 _black)
                             (grwfill w x y _black _black)

                             (iter (for pair in g)
                                     (cond ((member v pair) 
                                          (let* ((m1 (car pair))
                                                (m2 (cadr pair))
                                                (j1 (position m1 verts))
                                                (j2 (position m2 verts)))
                                                (setq x (* 60 (cos (* j1 f))))
                                                (setq y (* 60 (sin (* j1 f))))
                                                (setq x1 (* 60 (cos (* j2 f))))
                                                (setq y1 (* 60 (sin (* j2 f))))
                                             
                                                (grwline w x y x1 y1 _black)))))

                             (sleep* 1000)

                             ;;(grwcircle w x y 5 _GREEN)
                             ;;(grwfill w x y color _GREEN)

                             (iter (for pair in g)
                                     (cond ((member v pair) 
                                          (let* ((m1 (car pair))
                                                (m2 (cadr pair))
                                                (j1 (position m1 verts))
                                                (j2 (position m2 verts)))
                                                (setq x (* 60 (cos (* j1 f))))
                                                (setq y (* 60 (sin (* j1 f))))
                                                (setq x1 (* 60 (cos (* j2 f))))
                                                (setq y1 (* 60 (sin (* j2 f))))

                                                (grwcircle w x y 5 color)
                                                (grwfill w x y color color)
                                                (grwcircle w x1 y1 5 color)
                                                (grwfill w x1 y1 color color)
                                                (grwline w x y x1 y1 color)
                                             
                                                (grwline w x y x1 y1 color)))))))
  
                 (t (return 'ok) )))))

(drw)



                          
