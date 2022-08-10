;; 
;; Генератор графов. Иванов Михаил
;;

;; Вспомогательные функции

(defun index (key lst)
(let ((ind 0))
(dolist (i lst ind)
   (if (equal i key)
       (return ind)
       (setq ind (+ ind 1))))))

(defun vx (n total ln)
(* ln (sin (/ (* 2 n _pi) total))))

(defun vy (n total ln)
(* ln (cos (/ (* 2 n _pi) total))))

(defun c ()(grwcls 'w))

(defun disting (g ht)
(dolist (i g 'ok)
   (hashput ht (car i) (list (cadr i)(caddr i)))
   (hashput ht (cadr i) (list (car i)(caddr i)))) ht)

(defun path (g from to &optional (dists nil))
  (dolist (i g 'ok)
     (if (null dists) (disting g)())))

;;(defun quadratic (a b c)
;;(let ((d (- (^ b 2) (* 4 a c)))(x1 0)(x2 nil))
;;(cond ((> d 0) (progn (setq x1 (/ (+ -b (^ d 1/2)) 2)) (setq x2 (/ (- -b (^ d 1/2)) 2)) ()))
;;      ((= d 0) ())
;;      (t       (nil)))))

;; Генерация графов

(defun gen(size type &optional (arg 30))
(let ((g nil)) 
   (cond ((eq type "сетка")
           (setq size (* (\ size 4) 4))
           (dotimes (i (- (/ size 2) 1) 'ok)
              (setq g (append g (list (list (conv (+ i 2) size) (conv (- size (+ i 2) -2) size)))))
              (setq g (append g (list (list (+ (/ size 4) i 2) (conv (- (* size 3/2) (+ (/ size 4) i))size)))))))
         ((eq type "кольцо")
           (dotimes (i size 'ok)
              (setq g (append g (list (list i (% (+ i 1) size)))))))
         ((eq type "полный")
           (dotimes (i size 'ok)
              (dotimes (j i 'ok)
                (setq g (append g (list (list (+ i 1) (+ j 1))))))))
         ((eq type "случайный")
           (dotimes (i size 'ok)
              (dotimes (j i 'ok)
                 (cond ((< (rnd 100) arg)
                    (setq g (append g (list (list (+ i 1) (+ j 1))))))))))) g ))

(defun conv (n size)
   (cond ((= n 0) size)
         ((> n size) (% n size))
         (t n)))

(defun breadth (g chk queue)
   (if (null queue) chk
       (let* ((curr (car queue))
              (lst (remove-if (lambda (x) (member x chk)) (bounds g curr))))
          (if (null lst) (breadth g chk (cdr queue))
                         (progn
                            (dolist (i lst t) (print curr) (prints "->") (printline i))
                            (breadth g (append lst chk) (append (cdr queue) lst)))))))

(defun bounds (g v)
(let ((res nil))
   (dolist (i g res)
     (cond ((= (cadr i) v) (setq res (append res (list (car i)))))
           ((= (car i) v) (setq res (append res (list (cadr i)))))))))

;; Обход в ширину

;;(defun breadth (g from)
;;(let ((chk (list from)) (queue (list from)))
;;(dolist (i (bounds g from) 'ok)
;;(
;;))

(defun bounds (g v)
(let ((res nil))
   (dolist (i g res)
        (cond ((= (cadr i) v) (setq res (append res (list (car i)))))
              ((= (car i) v) (setq res (append res (list (cadr i)))))))))

;; Рисование графа

(defun graph (g win &optional (sorted nil))
(let ((points nil)
      (np 0)
 	  (ne (length g)) 
	  (size 0)
	  (outer 0))                                         ;; points - Cпиcoк тoчeк
  (dolist (i g 'ok)                                      ;; np - Koл-вo тoчeк
      (push (car i) points)                              ;; ne - Koл-вo peбep
      (push (cadr i) points))                            ;; size - Paзмep oбъeктoв
      (setq points (setof points))                       ;; outer - Bнутpeнний тeкcт или внeшний
      (if sorted (setq points (qsort points)) nil)
      (setq np (length points))
      (if (> np 20) (setq outer 1) nil)
      (setq size (* (^ np -0.5) 30))
      (grwFont win "Bahnschrift" (* size 2) t nil)
      (grwSetParm win (/ size 3) 0 _BLACK _BLACK)
      (grwScale win -100 100 -100 100)
      (dolist (i g 'ok)
         (grwLine win (vx (index (car i) points) np 80) (vy (index (car i) points) np 80)
                      (vx (index (cadr i) points) np 80) (vy (index (cadr i) points) np 80)
                      _BLACK))
      (dotimes (i np 'ok)
         (grwCircle win (vx i np 80) (vy i np 80) (/ size (+ outer 1)) _BLACK)
         (grwPrint win (- (vx i np (+ 80 (* outer 8))) (/ (grwTextW win (nth i points)) 2))
                       (+ (vy i np (+ 80 (* outer 8))) (* size 3/4)) (nth i points)
         (grwrgb (- 255 (* outer 255)) (- 255 (* outer 255)) (- 255 (* outer 255)))))))
 
;; Старт

(defun start nil
(let ((size 0)
      (type "")
	  (arg nil)
	  (w (gensym 'w))) ;; id очередного окна
	  
   (grwCreate w 300 300 "" _WHITE)
   (grwSetParm w 3 0 _WHITE)
   (grwShow w 5 5) 

   (setq size (str2fix (ask "Bведите paзмep гpaфa:")))
   
;;   (setq type (ask "Bведите тип гpaфa: 
;;              (Ceткa, кoльцo, ceткa+кoльцo, звездa, мyльтизвездa, эпициклoидa, cлyчaйный, пoлный)"))

   (setq type (ask "Bведите тип гpaфa: 
              (Ceткa, кoльцo, cлyчaйный, пoлный)"))
    
   (if (or (eq type "звездa") (eq type "случайный") (eq type "эпициклoидa"))
       (setq arg (str2fix (ask "Bведите пapaмeтp:"))) nil)

;;   (if (eq type "мультизвезда")
;;       (setq arg (input (ask "Bведите cпиcoк пapaмeтpoв:"))) nil)
	   
   (graph (gen size type arg) w t)))
 
;;(start) 
 
(defun start-1 nil
(let ((size   20)
	  (arg    10)
	  (w      nil)
	  (i      0)
	  (proz   0)      
	  (types '("сетка" "кольцо" "случайный" "полный")))

   (dolist  (type types 'ok)
     
	  (setq i (+ i 1)) 
	  (setq  proz (* 100.0 (/ i 4)))
	  (Pshow proz type)
	 
      (setq w (gensym 'w))	 
	  
      (grwCreate w 300 300 type _WHITE)
      (grwSetParm w 3 0 _WHITE)
      (graph (gen size type arg) w t))

   (phide) 
   (grwShowAll -1)))	  
 
(start-1) 
 
;; данные

'( 
(MS AL) (IN OH) (OR NV) (TX AR) (TX LA) (OK KS) (OR CA) (MO KY) (MO IL) (AR TN) 
(KY VA) (OR ID) (CA NV) (CA AZ) (NV AZ) (NJ PA) (NJ NY) (NY CT) (NY MA) (CO NM) 
(AR LA) (AR MS) (SC NC) (TN KY) (TN NC) (OK MO) (UT CO) (MT WY) (MT SD) (MT ND) 
(OH PA) (VA WV) (VA MD) (WV PA) (WV DC) (WV MD) (MD DC) (WI MI) (NH ME) (KY WV) 
(CT MA) (MA RI) (MA VT) (MA NH) (MN WI) (NM TX) (NM OK) (TX OK) (NY VT) (CT RI) 
(PA NJ) (PA NY) (SD MN) (SD IA) (ND MN) (DC DE) (AZ NM) (ID UT) (ID WY) (ID MT) 
(NV UT) (NV ID) (AZ UT) (KS NE) (KS MO) (NE SD) (NE IA) (NE MO) (SD ND) (WY CO) 
(AL TN) (GA SC) (GA NC) (GA TN) (IA MO) (IA IL) (IA WI) (WI IL) (FL GA) (AL GA) 
(WY NE) (WY SD) (CO NE) (CO KS) (CO OK) (MI IN) (MI OH) (MO AR) (IL KY) (MS TN) 
(UT WY) (OK AR) (MN IA) (TN VA) (NC VA) (KY OH) (IN KY) (FL AL) (DE PA) (DE NJ) 
(WA ID) (WA OR) (VT NH) (DC PA) (LA MS) (IL IN) (MO TN) (OH WV) )

'((10 1) (5 9) (5 16) (12 20) (7 1) (4 18) (5 1) (5 20)
(13 15) (11 4) (16 4) (10 17) (13 20) (6 8) (12 9) (14 6)
(6 11) (8 15) (16 3) (1 6) (19 20) (4 5) (9 13) (7 6)
(13 8) (20 3) (9 20) (12 8) (17 15) (2 3) (3 8) (2 11)
(11 12) (4 10) (7 14) (2 12) (4 13) (19 4) (17 2) (3 8))

                 