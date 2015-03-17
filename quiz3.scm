(define pi '(+ (/ (/ (/ a b) e) b)
			   (/ 1
				  (+ 3
					 (/ 4
						(+ 5
						   (/ 9
							  (+ 7
								 (/ 16
									(* (+ 9 a) b))))))))))

(define t1 '(+ 1
			   (/ 1
				  (* (* a
						(/ (/ t t)
						   (+ (+ 1
								 (/ x
									(- a
									   (- b c))))
							  (- (/ (/ 1 2)
									3)
								 (/ 1 (/ 2 3))))))
					 (+ (/ 4
						   (+ 5 (/ 9
								   (+ 7
									  (/ 168
										 (* (+ x y) a))))))
						(/ m n))))))

(define t2 '(* 1 (- (/  c (+ a b)) 2)))

(define t3 '(* 1 (- (+  c (+ a b)) 2)))

(define (build-node x w h)
  (list (cons x (cons w h))))

(define (node-val node)
  (caar node))

(define (node-w node)
  (cadar node))

(define (node-h node)
  (begin 
	(cddar node)))

(define (isleaf node)
  (null? (cdr node)))

(define (tree-h tree)
  (if (isleaf tree) 
	(node-h tree)
	(node-h (car tree))))
(define (tree-w tree)
  (if (isleaf tree)
	(node-w tree)
	(node-w (car tree))))
(define (tree-root-val tree)
  (if (isleaf tree)
	(node-val tree)
	(node-val (car tree))))

(define (node-print node)
  (begin
	(display "val:")
	(display (node-val node))
	(display " w:")
	(display (node-w node))
	(display " h:")
	(display (node-h node))
	(newline)))

(define (list-max l)
  (cond ((null? (cdr l)) (car l))
		(else (max (car l) (list-max (cdr l))))))

(define (list-size l)
  (begin 
	(cond ((null? l) 0)
		  (else (+ 1 (list-size (cdr l)))))))

(define (op? c)
  (or (eq? c '+) (eq? c '-) (eq? c '*) (eq? c '/)))

(define (count-down tree)
  (cond ((isleaf tree) 0)
		(else (if (eq? (tree-root-val tree) '/)
				(tree-h (caddr tree))
				(max (count-down (cadr tree)) (count-down (caddr tree)))))))
(define (count-up tree)
  (cond ((isleaf tree) 0)
		(else (if (eq? (tree-root-val tree) '/)
				(tree-h (cadr tree))
				(max (count-up (cadr tree)) (count-up (caddr tree)))))))

(define (build-tree x)
  (cond ((null? x) '())
		((not (pair? x)) (if (number? x) 
						   (build-node x (string-length (number->string x)) 1)
						   (build-node x 1 1)
						   ))
		(else  
		  ;(newline)
		  ;(display x)
		  (letrec* ((subtree (map build-tree (cdr x)))
					(subtree-size-sub1 (- (list-size subtree) 1))
					(h-sum (reduce-left + 0 (map tree-h subtree)))
					(h-max (list-max (map tree-h subtree)))
					(w-sum (reduce-left + 0 (map tree-w subtree)))
					(w-max (list-max (map tree-w subtree)))
					(val (car x)))
				   (if (eq? val '/)
					 (cons (build-node val (+ 2 w-max) (+ 1 h-sum)) subtree)
					 (letrec* ((lu (count-up (car subtree)))
							   (ld (count-down (car subtree)))
							   (ru (count-up (cadr subtree)))
							   (rd (count-down (cadr subtree)))
							   (h (max (+ 1 lu rd) (+ 1 ld ru) (+ 1 lu ld) (+ 1 rd ru)))
							   )
							  ;(newline)
							  ;(display h)
							  ;(newline)
							  ;(display rd)
							  (cons (build-node val (+ subtree-size-sub1 w-sum 2) h) subtree)))))))


(define (create-paint w h)
  (make-initialized-vector h (lambda (x) (make-vector w #\space))))

(define (show-paint-list p-list)
  (if (null? p-list)
	'()
	(begin
	  (newline)
	  (map display (car p-list))
	  (show-paint-list (cdr p-list)))))

(define (show-paint paint)
  (let 
	((lpaint (vector->list (vector-map vector->list paint)))
	 )
	(show-paint-list lpaint)))

(define (set-paint p r c newval)
  ;(newline)
  ;(display r)
  ;(display " ")
  ;(display c)
  ;(display " ")
  ;(display newval)
  (vector-set! (vector-ref p r) c newval)
  )

(define (line p x y len)
  (cond 
	((= len 0) '())
	((= len 1) (set-paint p y x #\-))
	(else (begin
			(set-paint p y x #\-)
			(line p (+ x 1) y (- len 1))))))

(define (draw bg tree x y)
  (begin
	(cond ((null? tree) '())
		  ((isleaf tree) (set-paint bg y x (tree-root-val tree)))
		  (else 
			(let ((opw (tree-w tree))
				  (oph (tree-h tree))
				  (opv (tree-root-val tree))
				  (p1w (tree-w (cadr tree)))
				  (p1h (tree-h (cadr tree)))
				  (p2w (tree-w (caddr tree)))
				  (p2h (tree-h (caddr tree))))
			  (if (eq? opv '/)
				(begin
				  (draw bg (cadr tree) (+ x (- (integer-floor opw 2) (integer-floor p1w 2))) y)
				  (line bg x (+ y p1h) (tree-w tree))
				  (draw bg (caddr tree) (+ x (- (integer-floor opw 2) (integer-floor p2w 2))) (+ 1 p1h y))
				  )
				(letrec* ((lu (count-up (cadr tree)))
						  (ld (count-down (cadr tree)))
						  (ru (count-up (caddr tree)))
						  (rd (count-down (caddr tree)))
						  (center (max lu ru))
						  (lefty (- center lu ))
						  (righty (- center ru ))
						  )
						 (begin
						   (draw bg (cadr tree) x (+ y lefty))
						   (set-paint bg (+ y center) (+ x 1 p1w) opv)
						   (draw bg (caddr tree) (+ x 3 p1w) (+ y righty))
						   ))))))))

(define (real-draw tree)
  (letrec*
	((w (tree-w tree))
	 (h (tree-h tree))
	 (bg (create-paint w h)))
	(begin 
	  (draw bg tree 0 0)
	  (show-paint bg))))

(define paint-tree (build-tree t1))
(real-draw paint-tree)

;(set! paint-tree (build-tree t1))
;(real-draw paint-tree)

;(real-draw paint-tree)

;(real-draw paint-tree)

