#lang racket
; Requirements:
(require graphics)
(require racket/gui)
(require "instructions.rkt")

(define side-x 800)
(define side-y 700)
(define m 5)
(define n 5)
(define p 2)
(define colors (list  "green" "silver" "yellow" "mediumspringgreen" "mediumblue"
                      "orange" "red" "turquoise" "mintcream" "magenta" "black" ))

(struct player (name nature color score wins rank) #:transparent)
(struct box (o l r u d) #:transparent)
(define default-box (box 0 0 0 0 0))
(define info '())
(define natures (list "Human" "CPU-I" "CPU-II" "CPU-III"))
(define players (make-vector 0))
(define sx 0)
(define sy 0)
(define (float-to-int x)
  (define (helper x i)
    (if (= x i)
        i
        (helper x (+ i 1))))
  (helper x 0))
(define board '())

(open-graphics)

(define (instructions)
  (let ([inst (open-viewport "INSTRUCTIONS" 800 400)])
    (clear-vp inst "black")
    ((draw-string inst) (make-posn 100 50) "INSTRUCTIONS : " "blue")
    ((draw-string inst) (make-posn 100 100) line-1 "white")
    ((draw-string inst) (make-posn 100 150) line-2 "white")
    ((draw-string inst) (make-posn 100 200) line-3 "white")
    ((draw-string inst) (make-posn 100 250) line-4 "white")
    ((draw-string inst) (make-posn 100 300) line-5 "white")
    ((draw-string inst) (make-posn 100 350) line-6 "white")
    ))

(define (draw-parallel vp p xx yy)
  (let* ([p1 (car p)]
         [p2 (cdr p)]
         [x1 (* xx (+ (car p1) 1))]
         [y1 (* yy (+ (cdr p1) 1))]
         [x2 (* xx (+ (car p2) 1))]
         [y2 (* yy (+ (cdr p2) 1))]
         [c (list-ref colors (random 10))])
    (begin
      ((draw-line vp) (make-posn (+ 2.5 x1) (+ 0.5 y1)) (make-posn (+ 2.5 x2) (+ 0.5 y2)) c)
      ((draw-line vp) (make-posn (+ 1.5 x1) (+ 1.5 y1)) (make-posn (+ 1.5 x2) (+ 1.5 y2)) c)
      ((draw-line vp) (make-posn (+ 0.5 x1) (+ 2.5 y1)) (make-posn (+ 0.5 x2) (+ 2.5 y2)) c)
      ((draw-line vp) (make-posn (+ 1.0 x1) (+ 0.0 y1)) (make-posn (+ 1.0 x2) (+ 0.0 y2)) c)
      ((draw-line vp) (make-posn (+ 0.0 x1) (+ 1.0 y1)) (make-posn (+ 0.0 x2) (+ 1.0 y2)) c))))

(define (make-title vp pts xx yy c)
  (cond [(not (null? pts))
         (let* ([p (car pts)])                
           (begin
             (draw-parallel vp p xx yy)
             (sleep (* c 0.1))
             (make-title vp (cdr pts) xx yy c)))]))


(define (initiate-board)
  (let ([box0 (make-list m (make-list n #t))]
        [box1 (make-list 4 (make-list m (make-list n #f)))])
    (set! board (cons box0 box1))))
        
(define (connect-the-dots)
  (let ([vp (open-viewport "DOT-CONNECT" side-x side-y)])
    (draw-page-1 vp 1)))
    
(define (draw-page-1 vp c)
  (begin
    (clear-vp vp "black")
    (add-border vp "white")
    ((draw-solid-rectangle vp) (make-posn 0 200) side-x 5 "white")
    ((create-grid vp) 2 48 800 200)
    (make-title vp (vector->list (randomize-vec title)) (get-side 800 48) (get-side 200 2) c) (sleep (* c 0.2))
    ((draw-string vp) (make-posn 100 280) "SETTINGS : " "cyan") (sleep (* c 0.2))
    ((draw-string vp) (make-posn 225 350) "NUMBER OF ROWS : " "yellow") (sleep (* c 0.2))
    ((draw-string vp) (make-posn 225 400) "NUMBER OF COLUMNS : " "yellow") (sleep (* c 0.2))
    ((draw-string vp) (make-posn 225 450) "NUMBER OF PLAYERS : " "yellow") (sleep (* c 0.2))
    ((draw-text vp) (make-posn 450 350) "-" "white" "black") 
    ((draw-text vp) (make-posn 500 350) "5" "black" "white")
    ((draw-text vp) (make-posn 550 350) "+" "white" "black") 
    ((draw-text vp) (make-posn 450 400) "-" "white" "black") 
    ((draw-text vp) (make-posn 500 400) "5" "black" "white")
    ((draw-text vp) (make-posn 550 400) "+" "white" "black") 
    ((draw-text vp) (make-posn 450 450) "-" "white" "black") 
    ((draw-text vp) (make-posn 500 450) "2" "black" "white")
    ((draw-text vp) (make-posn 550 450) "+" "white" "black")
    (sleep 0.2)
    ((draw-solid-rectangle vp) (make-posn 500 620) 200 60 "lawngreen")
    ((draw-solid-rectangle vp) (make-posn 100 620) 200 60 "purple")
    ((draw-string vp) (make-posn 565 655) "PROCEED" "red") 
    ((draw-string vp) (make-posn 145 655) "INSTRUCTIONS" "yellow") 
    (update-initial-info (cons (make-posn 445 334) (make-posn 465 357))
                         (cons (make-posn 545 334) (make-posn 565 357))
                         (cons (make-posn 445 384) (make-posn 465 407))
                         (cons (make-posn 545 384) (make-posn 565 407))
                         (cons (make-posn 445 434) (make-posn 465 457))
                         (cons (make-posn 545 434) (make-posn 565 457))
                         (cons (make-posn 100 620) (make-posn 300 680))
                         (cons (make-posn 500 620) (make-posn 700 680))
                         (make-posn 500 350)
                         (make-posn 500 400)
                         (make-posn 500 450)
                         vp)))

(define (check-in p box)
  (let ([p1 (car box)]
        [p2 (cdr box)])
    (and (<= (posn-x p) (posn-x p2))
         (>= (posn-x p) (posn-x p1))
         (<= (posn-y p) (posn-y p2))
         (>= (posn-y p) (posn-y p1)))))

(define (info-abstraction vp id s op lim box)
  (cond [(op id lim) (begin (set! id (+ id s))
                            ((draw-text vp)
                             box
                             (number->string id)
                             "black"
                             "white")
                            id)]
        [else id]))

(define (update-initial-info m1 p1 m2 p2 m3 p3 ins play u1 u2 u3 vp)
  (begin
    (let* ([qq (query-mouse-posn vp)])
      (cond 
        [(check-in qq play)
         ((draw-rectangle vp) (car play) (- (posn-x (cdr play)) (posn-x (car play)))
                              (- (posn-y (cdr play)) (posn-y (car play))) "white")]
        [(check-in qq ins)
         ((draw-rectangle vp) (car ins) (- (posn-x (cdr ins)) (posn-x (car ins)))
                              (- (posn-y (cdr ins)) (posn-y (car ins))) "white")]
        [else ((draw-rectangle vp) (car play) (- (posn-x (cdr play)) (posn-x (car play)))
                                   (- (posn-y (cdr play)) (posn-y (car play))) "black")
              ((draw-rectangle vp) (car ins) (- (posn-x (cdr ins)) (posn-x (car ins)))
                                   (- (posn-y (cdr ins)) (posn-y (car ins))) "black")]))
    (let* ([mc (ready-mouse-click vp)]
           [ps (if (not (eq? #f mc)) (mouse-click-posn mc) (make-posn 0 0))])
      (cond [(check-in ps play) (begin (set! players (make-vector p)) (page-2 vp))]
            [else (begin
                    (cond [(check-in ps ins) (instructions)]
                          [(check-in ps m1)
                           (set! m (info-abstraction vp m -1 > 1 u1))]
                          [(check-in ps p1)
                           (set! m (info-abstraction vp m 1 < 12 u1))]
                          [(check-in ps m2)
                           (set! n (info-abstraction vp n -1 > 1 u2))]
                          [(check-in ps p2)
                           (set! n (info-abstraction vp n 1 < 12 u2))]
                          [(check-in ps m3)
                           (set! p (info-abstraction vp p -1 > 2 u3))]
                          [(check-in ps p3)
                           (set! p (info-abstraction vp p 1 < 8 u3))]))]))
    (update-initial-info m1 p1 m2 p2 m3 p3 ins play u1 u2 u3 vp)))


(define (get-side l n)
  (/ l (+ n 2.0)))

(define (create-grid vp)
  (define (grid-creater m n a b sx sy x y)
    (if (and (= a m) (= b n))
        ((draw-rectangle vp) (make-posn x y) 3 3 "white")
        (if (> b n)
            (grid-creater m n (+ a 1) 0 sx sy sx (+ y sy))
            (begin
              ((draw-rectangle vp) (make-posn x y) 3 3 "white")
              (grid-creater m n a (+ b 1) sx sy (+ x sx) y)))))    
  (lambda (m n X Y)
    (let ([sx (get-side X n)]
          [sy (get-side Y m)])
      (grid-creater m n 0 0 sx sy sx sy))))

(define (clear-vp vp color)
  ((draw-solid-rectangle vp) (make-posn 0 0) side-x side-y color))

(define (draw-text vp)
  (lambda (p text c1 c2)
    (let ([x (posn-x p)]
          [y (posn-y p)])
      ((draw-solid-rectangle vp) (make-posn (- x 5) (- y 16)) 20 23 c1)
      ((draw-string vp) (make-posn x y) text c2))))
      

(define (add-border vp c)
  ((draw-solid-rectangle vp) (make-posn 0 0) side-x 5 c)
  ((draw-solid-rectangle vp) (make-posn 0 0) 5 side-y c)
  ((draw-solid-rectangle vp) (make-posn 0 (- side-y 5)) side-x 5 c)
  ((draw-solid-rectangle vp) (make-posn (- side-x 5) 0) 5 side-y c)
  ((draw-solid-rectangle vp) (make-posn 0 (- side-y 100)) side-x 5 c))

(define (page-2 vp)
  (clear-vp vp "black")
  (add-border vp "white")
  ((draw-solid-rectangle vp) (make-posn 500 620) 200 60 "lawngreen")
  ((draw-solid-rectangle vp) (make-posn 100 620) 200 60 "purple")
  ((draw-string vp) (make-posn 575 655) "PLAY!" "red") 
  ((draw-string vp) (make-posn 145 655) "INSTRUCTIONS" "yellow")
  ((draw-string vp) (make-posn 100 50) "PLAYER ATTRIBUTES : " "cyan")
  (draw-players vp)
  )

(define (draw-players vp)
  (define s (/ 500 (+ p 2.0)))
  (define (iter i)
    (cond [(<= i p)
           (begin
             (let ([z (+ 111 (* s i))]
                   [name (string-append "Player " (number->string i))]
                   [color (list-ref colors i)])
               ((draw-string vp) (make-posn 75 z) name "white")
               ((draw-rectangle vp) (make-posn 70 (- z 21)) 200 31 "white")
               ((draw-text vp) (make-posn 350 z) "<" "white" "black")
               ((draw-text vp) (make-posn 480 z) ">" "white" "black")
               ((draw-string vp) (make-posn 400 z) (list-ref natures 0) "white")
               ((draw-text vp) (make-posn 600 z) "<" "white" "black")
               ((draw-text vp) (make-posn 700 z) ">" "white" "black")
               ((draw-text vp) (make-posn 650 z) "" color "black")
               (vector-set! players (- i 1) (player name 0 i 0 0 0))
               (iter (+ i 1))))]
          [else (update-players vp)] 
          ))
  (iter 1))

(define (get-i y s a b)
  (define (iter i)
    (if (> i p) #f
        (let ([l1 (+ a (* i s))]
              [l2 (+ b (* i s))]
              )
          (cond [(< y l1) #f]
                [(< y l2) i]
                [else (iter (+ i 1))]))))
  (iter 1))

(define (get-key vp s pp pq)
  (let* ([q (get-key-press vp)]
         [j (key-value q)]
         )
    (cond
      [(not (char? j)) (get-key vp s pp pq)]
      [(eq? j #\backspace) (begin (cond [(> (string-length s) 0)
                                         (begin (set! s (substring s 0 (- (string-length s) 1)))
                                                ((draw-solid-rectangle vp) pq 190 20 "black")
                                                ((draw-string vp) pp s "white"))])
                                  (get-key vp s pp pq))]
      [(eq? j 'release) (get-key vp s pp pq)]
      [(eq? j #\return)
       (begin ((draw-rectangle vp) (make-posn 70 (- (posn-y pq) 6)) 200 31 "white") s)]
      [(< (string-length s) 18) (begin (set! s (string-append s (make-string 1 j)))
                                       ((draw-string vp) pp s "white")
                                       (get-key vp s pp pq))]
      [else (get-key vp s pp pq)])))

(define (update-players vp)
  (begin
    (let ([qq (query-mouse-posn vp)])
      (cond [(check-in qq (cons (make-posn 500 620) (make-posn 700 680)))
             ((draw-rectangle vp) (make-posn 500 620) 200 60 "white")]
            [(check-in qq (cons (make-posn 100 620) (make-posn 300 680)))
             ((draw-rectangle vp) (make-posn 100 620) 200 60 "white")]
            [else
             ((draw-rectangle vp) (make-posn 100 620) 200 60 "black")
             ((draw-rectangle vp) (make-posn 500 620) 200 60 "black")]))
    (let* ([mc (ready-mouse-click vp)]
           [ps (if (not (eq? #f mc)) (mouse-click-posn mc) (make-posn 0 0))]
           [x (posn-x ps)]
           [y (posn-y ps)]
           [s (/ 500 (+ p 2.0))])
      (cond [(check-in ps (cons (make-posn 500 620) (make-posn 700 680)))
             (start-game vp)]
            [else
             (begin
               (cond
                 [(check-in ps (cons (make-posn 100 620) (make-posn 300 680)))
                  (instructions)]
                 [(and (> x 70) (< x 270))
                  (let ([t (get-i y s 90 121)])
                    (cond [(not (eq? #f t))
                           (begin
                             ((draw-solid-rectangle vp) (make-posn 73 (+ 96 (* s t))) 190 20 "black")
                             (let ([plr (vector-ref players (- t 1))])
                               ((draw-rectangle vp) (make-posn 70 (+ 90 (* s t))) 200 31 "green")
                               (vector-set! players
                                            (- t 1)
                                            (player (get-key vp "" (make-posn 75 (+ 111 (* s t)))
                                                             (make-posn 73 (+ 96 (* s t))))
                                                    (player-nature plr) (player-color plr) 0
                                                    (player-wins plr) (player-rank plr)))))]))]
                 [else (let* ([t (get-i y s 95 118)])
                         (cond [(not (eq? #f t))
                                (let* ([plr (vector-ref players (- t 1))])
                                  (cond [(and (> x 345) (< x 365))
                                         (info-abstraction2 vp t - 4 plr (make-posn 380 (+ 96 (* s t))) 90 20 (make-posn 400 (+ 111 (* s t))) 0)]
                                        [(and (> x 475) (< x 495))
                                         (info-abstraction2 vp t + 4 plr (make-posn 380 (+ 96 (* s t))) 90 20 (make-posn 400 (+ 111 (* s t))) 0)]
                                        [(and (> x 595) (< x 615))
                                         (info-abstraction2 vp t - 10 plr (make-posn 645 (+ 95 (* s t))) 20 23 (make-posn 0 0) 1)]
                                        [(and (> x 695) (< x 715))
                                         (info-abstraction2 vp t + 10 plr (make-posn 645 (+ 95 (* s t))) 20 23 (make-posn 0 0) 1)]))]))])
               (update-players vp))]))))
           
(define (info-abstraction2 vp t op lim plr p1 length breadth p2 u)
  (cond [(= u 0) (let* ([z (modulo (op (player-nature plr) 1) lim)])
                   (vector-set! players (- t 1) (player (player-name plr) z (player-color plr) 0 (player-wins plr) (player-rank plr)))
                   ((draw-solid-rectangle vp) p1 length breadth "black")
                   ((draw-string vp) p2 (list-ref natures z) "white"))]
        [else    (let* ([z (modulo (op (player-color plr) 1) lim)])
                   (vector-set! players (- t 1) (player (player-name plr) (player-nature plr) z 0 (player-wins plr) (player-rank plr)))
                   ((draw-solid-rectangle vp) p1 length breadth (list-ref colors z)))]))

(define (randomize-vec vec)
  (define (randomize-vec-helper vec ans)
    (let ([l (vector-length vec)])
      (if (= 0 l)
          ans
          (let ([a (random l)])
            (randomize-vec-helper (vector-append (vector-take vec a) (vector-drop vec (+ a 1)))
                                  (vector-append ans (vector (vector-ref vec a))))))))
  (randomize-vec-helper vec (make-vector 0)))

(define (lines-abstraction vp lol)
  (cond [(not (null? lol))
         (begin (let ([p (car lol)]) ((draw-line vp) (caar p) (cdar p) (cdr p)))
                (lines-abstraction vp (cdr lol)))]))

(define (get-vertical-lines s i l c ans)
  (if (= i l)
      ans
      (get-vertical-lines s (+ i 1) l c(cons (cons (cons (make-posn (* s i) 605) (make-posn (* s i) 694))
                                                   c) ans))))
(define (show-players-scores vp i s)
  (cond [(< i p)
         (let ([plr (vector-ref players i)])
           (begin
             ((draw-text vp) (make-posn (+ 300 (* s (quotient (+ i 2) 2))) (+ 635 (* 40 (remainder i 2))))
                             "" (list-ref colors (player-color plr)) "white")
             ((draw-solid-rectangle vp) (make-posn (+ 330 (* s (quotient (+ i 2) 2))) (+ 620 (* 40 (remainder i 2))))
                                        25 25 "black")
             ((draw-string vp) (make-posn (+ 330 (* s (quotient (+ i 2) 2))) (+ 635 (* 40 (remainder i 2))))
                               (number->string (player-score plr)) (list-ref colors (player-color plr)))
             (show-players-scores vp (+ i 1) s)))]))

(define (show-current-player vp i)
  (let* ([p (vector-ref players i)]
         [c (list-ref colors (player-color p))])
    (begin
      (add-border vp (get-color i))
      ((draw-solid-rectangle vp) (make-posn 300 605) 5 90 (get-color i))
      ((draw-solid-rectangle vp) (make-posn 10 610) 285 80 c)
      ((draw-string vp) (make-posn 20 655) (player-name p) "black")
      ((draw-string vp) (make-posn 250 655) (number->string (player-score p)) "black"))
    ))
           
(define (create-info)
  (make-vector m (make-vector n default-box)))

(define (start-game vp)
  (set! players (randomize-vec players))
  (clear-vp vp "black")
  (add-border vp "white")
  ((create-grid vp) m n side-x (- side-y 100))
  (show-players-scores vp 0 (get-side 500 (/ p 2.0)))
  (show-current-player vp 0)
  (set! info (create-info))
  (set! sx (get-side 800 n))
  (set! sy (get-side 600 m))
  (initiate-board)
  (play-game vp 0)
  )

(define (edit-board b r)
  (define (iter i l)
    (cond [(= i r) (append (list (modify-list (list-ref board r) (car b) (cdr b) #f)
                                 (modify-list (list-ref board (+ r 1)) (car b) (cdr b) #t))
                           (cddr l))]
          [else (cons (car l) (iter (+ i 1) (cdr l)))]))
  (set! board (iter 0 board)))

(define (game-over?)
  (define (iter i ans)
    (if (= i p)
        ans
        (iter (+ i 1) (+ ans (player-score (vector-ref players i))))))
  (eq? (* m n) (iter 0 0)))         

(define (box-integer ps)
  (let ([x (- (floor (/ (posn-x ps) sx)) 1)]
        [y (- (floor (/ (posn-y ps) sy)) 1)]
        )
    (cons
     (cond [(< x 0) 0] [(>= x n) (- n 1)] [else (- (float-to-int x) 0)])
     (cond [(< y 0) 0] [(>= y m) (- m 1)] [else (- (float-to-int y) 0)]))))

(define (side-integer ps b)
  (let* ([x (- (/ (posn-x ps) sx) 1)]
         [y (- (/ (posn-y ps) sy) 1)]
         [c (cons (+ 0.5 (car b)) (+ 0.5 (cdr b)))]
         [slp (abs (/ (- (cdr c) y) (- (car c) x)))])
    (cond [(> slp 1) (if (> y (cdr c)) 3 2)]
          [(> x (car c)) 1]
          [else 0])))

(define (get-width)
  (- 4 (/ (max m n) 4.0)))
    
(define (play-game vp turn)
  (show-players-scores vp turn (get-side 500 (/ p 2.0)))
  (show-current-player vp turn)
  (let ([sxs (game-over?)])
    (if (not (eq? #f sxs))
        (begin (sleep 1) (final-vp vp))
        (begin (let* ([plr (vector-ref players turn)]
                      [ntr (player-nature plr)])
                 (let ([w (cond [(= ntr 0) (human-turn vp turn)]
                                [(= ntr 1) (sleep 1) (cpu-1-turn vp turn 0 (sqrt (* m n)))] ;POWER
                                [(= ntr 2) (sleep 0.5) (cpu-2-turn vp turn 0 (* m n))]
                                [(= ntr 3) (sleep 0.5) (cpu-3-turn vp turn 0 (* m n))]
                                )])
                   (if (= 0 w)
                       (play-game vp (modulo (+ turn 1) p))
                       (begin
                         (vector-set! players turn
                                      (player (player-name plr)
                                              (player-nature plr)
                                              (player-color plr)
                                              (+ w (player-score plr))
                                              (+ w (player-wins plr))
                                              (player-rank plr)))
                         (play-game vp turn)))))))))

(define (get-color i)
  (list-ref colors (player-color (vector-ref players i))))

(define (can-do? b s)
  (let ([d (get-box b)])
    (cond [(= s 0) (eq? 0 (box-l d))] [(= s 1) (eq? 0 (box-r d))]
          [(= s 2) (eq? 0 (box-u d))] [(= s 3) (eq? 0 (box-d d))])))

(define (complete? b)
  (let ([bx (get-box b)])
    (and (= 1 (box-l bx)) (= 1 (box-r bx)) (= 1 (box-u bx)) (= 1 (box-d bx)))))

(define (box-rank b)
  (let ([bx (get-box b)])
    (+ (box-l bx) (box-r bx) (box-u bx) (box-d bx))))

(define (get-box b)
  (vector-ref (vector-ref info (cdr b)) (car b)))

(define (update-info vp i b s)
  (cond [(= s 0) (if (= (car b) 0) (update-box i vp b 0)
                     (+ (update-box i vp b 0) (update-box i vp (cons (- (car b) 1) (cdr b)) 1)))]
        [(= s 1) (if (= (car b) (- n 1)) (update-box i vp b 1)
                     (+ (update-box i vp b 1) (update-box i vp (cons (+ (car b) 1) (cdr b)) 0)))]
        [(= s 2) (if (= (cdr b) 0) (update-box i vp b 2)
                     (+ (update-box i vp b 2) (update-box i vp (cons (car b) (- (cdr b) 1)) 3)))]
        [(= s 3) (if (= (cdr b) (- m 1)) (update-box i vp b 3)
                     (+ (update-box i vp b 3) (update-box i vp (cons (car b) (+ (cdr b) 1)) 2)))]))

(define (update-box i vp b s)
  (let ([bx (get-box b)]
        [r (box-rank b)]
        [w 0])
    (begin
      (cond [(= 3 r) (begin (set! w 1) (set! bx (box i 1 1 1 1)) (fill-box vp i b))]
            [(= s 0) (set! bx (box (box-o bx) 1 (box-r bx) (box-u bx) (box-d bx)))]
            [(= s 1) (set! bx (box (box-o bx) (box-l bx) 1 (box-u bx) (box-d bx)))]
            [(= s 2) (set! bx (box (box-o bx) (box-l bx) (box-r bx) 1 (box-d bx)))]
            [(= s 3) (set! bx (box (box-o bx) (box-l bx) (box-r bx) (box-u bx) 1))])
      (edit-board b r)
      (modify-info b bx) 
      w)))

(define (modify-l l x bx)
  (cond [(= x 0) (cons bx (cdr l))]
        [else (cons (car l) (modify-l (cdr l) (- x 1) bx))]))  

(define (modify-list l x y bx)
  (cond [(= y 0) (cons (modify-l (car l) x bx) (cdr l))]
        [else (cons (car l) (modify-list (cdr l) x (- y 1) bx))]))

(define (modify-info b bx)
  (let* ([l (map vector->list (vector->list info))]
         [l2 (modify-list l (car b) (cdr b) bx)])
    (set! info (list->vector (map list->vector l2)))))

(define (fill-box vp i b)
  (let ([w (get-width)])
    ((draw-solid-rectangle vp)
     (make-posn (+ 7 w (* sx (+ 1 (car b)))) (+ w 7 (* sy (+ 1 (cdr b)))))
     (- sx (+ 14 w)) (- sy (+ 14 w)) (get-color i))))

(define (join vp b s i)
  (let ([width (get-width)])
    (cond [(< s 2) ((draw-solid-rectangle vp) (make-posn (* sx (+ (car b) 1 s)) (+ 3 (* sy (+ (cdr b) 1)))) width (- sy 3) (get-color i))]
          [else ((draw-solid-rectangle vp) (make-posn (+ 3 (* sx (+ (car b) 1 ))) (* sy (+ (cdr b) s -1))) (- sx 3) width (get-color i))
                ])))

(define (human-turn vp i)
  (let* ([mc (get-mouse-click vp)]
         [ps (mouse-click-posn mc)]
         [b (box-integer ps)]
         [s (side-integer ps b)])
    (if (can-do? b s)
        (begin
          (join vp b s i)
          (update-info vp i b s))
        (human-turn vp i))))

(define (remaining-side b)
  (let ([bx (get-box b)])
    (cond [(= 0 (box-l bx)) 0] [(= 0 (box-r bx)) 1]
          [(= 0 (box-u bx)) 2] [(= 0 (box-d bx)) 3])))

(define (x-boxes x i j ans)
  (cond [(= i m) ans]
        [(= j n) (x-boxes x (+ i 1) 0 ans)]
        [else (x-boxes x i (+ j 1) (if  (list-ref (list-ref (list-ref board x) i) j) (cons (cons j i) ans) ans))]))
  
(define (safe-move? b s)
  (if (= 2 (box-rank b)) #f
      (cond [(= s 0) (if (not (= 0 (car b))) (not (= (or 2 3) (box-rank (cons (- (car b) 1) (cdr b))))) #t)]
            [(= s 1) (if (not (= (- n 1) (car b))) (not (= (or 2 3) (box-rank (cons (+ (car b) 1) (cdr b))))) #t)]
            [(= s 2) (if (not (= 0 (cdr b))) (not (= (or 2 3) (box-rank (cons (car b) (- (cdr b) 1))))) #t)]
            [(= s 3) (if (not (= (- m 1) (cdr b))) (not (= (or 2 3) (box-rank (cons (car b) (+ (cdr b) 1))))) #t)])))

(define (cpu-1-turn vp i count power)
  (let ([l (x-boxes 3 0 0 '())])
    (if (null? l)
        (let ([a (random n)]
              [b (random m)]
              [s (random 4)])
          (if (can-do? (cons a b) s)
              (cond [(and (< count power) (not (safe-move? (cons a b) s))) (cpu-1-turn vp i (+ count 1) power)]
                    [else
                     (begin 
                       (join vp (cons a b) s i)
                       (update-info vp i (cons a b) s))])
              (cpu-1-turn vp i count power)))
        (let* ([r (random (length l))]
               [b (list-ref l r)]
               [s (remaining-side b)])
          (begin
            (join vp b s i)
            (update-info vp i b s))))))

(define (cpu-2-turn vp i count power)
  (let ([l3 (x-boxes 3 0 0 '())])
    (cond [(not (null? l3))
           (let* ([r (random (length l3))]
                  [b (list-ref l3 r)]
                  [s (remaining-side b)])
             (begin
               (join vp b s i)
               (update-info vp i b s)))]
          [else (let* ([l (append (x-boxes 1 0 0 '()) (x-boxes 0 0 0 '()))])
                  (cond [(and (< count power) (not (null? l)))
                         (let* ([a (random (length l))]
                                [s (random 4)]
                                [b (list-ref l a)])
                           (if (can-do? b s)
                               (cond [(not (safe-move? b s))
                                      (cpu-2-turn vp i (+ count 1) power)]
                                     [else
                                      (begin 
                                        (join vp b s i)
                                        (update-info vp i b s))])
                               (cpu-2-turn vp i count power)))]
                        [else
                         (let* ([l2 (x-boxes 2 0 0 '())]
                                [bm (best-move vp l2)]
                                [b (car bm)]
                                [s (cdr bm)])
                           (begin
                             (join vp b s i)
                             (update-info vp i b s)))]))])))

(define (cpu-3-turn vp i count power)
  (let* ([l3 (x-boxes 3 0 0 '())]
         [all-boxes (append (x-boxes 0 0 0 '()) (x-boxes 1 0 0 '()) (x-boxes 2 0 0 '()) l3)]
         [broken-boxes (append* (map (lambda(b) (break-box b)) all-boxes))]
         [safe-moves (map (lambda (k) (safe-move? (car k) (cdr k))) broken-boxes)])
    (cond [(not (null? l3))
           (if (all-false? safe-moves)
               (cpu-3-attack vp i l3)
               (cpu-2-turn vp i 0 1))]
          [else (let* ([l (append (x-boxes 1 0 0 '()) (x-boxes 0 0 0 '()))])
                  (cond [(and (< count power) (not (null? l)))
                         (let* ([a (random (length l))]
                                [s (random 4)]
                                [b (list-ref l a)])
                           (if (can-do? b s)
                               (cond [(not (safe-move? b s))
                                      (cpu-3-turn vp i (+ count 1) power)]
                                     [else
                                      (begin 
                                        (join vp b s i)
                                        (update-info vp i b s))])
                               (cpu-3-turn vp i count power)))]
                        [else
                         (let* ([l2 (x-boxes 2 0 0 '())]
                                [bm (best-move vp l2)]
                                [b (car bm)]
                                [s (cdr bm)])
                           (begin
                             (join vp b s i)
                             (update-info vp i b s)))]))])))

(define (all-false? l)
  (cond [(null? l) #t]
        [(car l) #f]
        [else (all-false? (cdr l))]))

(define (break-box b)
  (let* ([l (get-rem-sides b)])
    (map (lambda(s) (cons b s)) l)))

(define (cpu-3-attack vp i l)
  (begin
    (let ([ch (chances-remaining vp)])
      (cond [(and (not (= p 2))(= 1 (modulo ch p))) (cpu-2-turn vp i 0 1)]
            [(and (not (= p 2)) (< ch p)) (cpu-2-turn vp i 0 1)]
            [(= 1 (length l)) (begin (2-sacrifice vp i (car l)) 0)]
            [(= 2 (length l)) (begin (4-sacrifice vp i (car l) (cadr l)) 0)]
            [else (cpu-2-turn vp i 0 1)]))))

(define (2-sacrifice vp i b)
  (let* ([plr (vector-ref players i)]
         [plr2 (player "TESTER" 1 10 0 0 0)]
         [bord board]
         [infi info])
    (begin
      (set! players (vector-append players (vector plr2)))
      (let* ([num (until-loss-of-turn vp (total-score))])
        (set! players (vector-take players p))
        (set! board bord)
        (set! info infi)
        (cond [(= num 1) (begin
                           (show-players-scores vp i (get-side 500 (/ p 2.0)))
                           (show-current-player vp i)
                           (sleep 1)
                           (let* ([w (cpu-2-turn vp i 0 1)])
                             (vector-set! players i
                                          (player (player-name plr)
                                                  (player-nature plr)
                                                  (player-color plr)
                                                  (+ w (player-score plr))
                                                  (+ w (player-wins plr))
                                                  (player-rank plr)))))]
              [else (begin
                      (play-n-times (- num 2) vp i)
                      (cond [(= (total-score) (- (* m n) 2)) (let* ([plyr (vector-ref players i)])
                                                               (begin
                                                                 (sleep 0.5)
                                                                 (cpu-2-turn vp i 0 1)
                                                                 (sleep 0.5)
                                                                 (cpu-2-turn vp i 0 1)
                                                                 (vector-set! players i (player (player-name plyr)
                                                                                                (player-nature plyr)
                                                                                                (player-color plyr)
                                                                                                (+ 2 (player-score plyr))
                                                                                                (+ 2 (player-wins plyr))
                                                                                                (player-rank plyr)))
                                                                 (play-game vp i)))]
                            [else (let* ([r (play-trick vp i)]
                                         [b (car r)]
                                         [s (cdr r)])
                                    (begin (sleep 0.5)
                                           (join vp b s i)
                                           (update-info vp i b s)))]))])))))

(define (4-sacrifice vp i b1 b2)
  (let ([w1 (sac-4-test vp b2)])
    (cond [(not (cdr w1))
           (let ([w2 (sac-4-test vp b1)])
             (cond [(> (car w1) (car w2)) (begin (play-trick-2 vp i b1) (2-sacrifice vp i b1))]
                   [else (begin (play-trick-2 vp i b2) (2-sacrifice vp i b2))]))]
          [else
           (let* ([plr (vector-ref players i)]
                  [plr2 (player "TESTER" 1 10 0 0 0)]
                  [bord board]
                  [infi info])
             (begin
               (set! players (vector-append players (vector plr2)))
               (let* ([num (until-loss-of-turn vp (total-score))])
                 (set! players (vector-take players p))
                 (set! board bord)
                 (set! info infi)
                 (play-n-times (- num 4) vp i)
                 (cond [(= (total-score) (- (* m n) 4)) (4-end-game vp i)]
                       [else (let* ([l3 (x-boxes 3 0 0 '())]
                                    [ch (give-2nd vp (car l3))]
                                    [b (car ch)]
                                    [s (cdr ch)])
                               (sleep 1)
                               (join vp b s i)
                               (update-info vp i b s))]))))])))

(define (4-end-game vp i)
  (define (helper c)
    (cond [(< c 3) (begin
                     (show-players-scores vp i (get-side 500 (/ p 2.0)))
                     (show-current-player vp i)
                     (sleep 1)
                     (let* ([w (cpu-2-turn vp i 0 1)]
                            [plr (vector-ref players i)])
                       (vector-set! players i
                                    (player (player-name plr)
                                            (player-nature plr)
                                            (player-color plr)
                                            (+ w (player-score plr))
                                            (+ w (player-wins plr))
                                            (player-rank plr))))
                     (helper (+ c 1)))]))
  (helper 0))
         
(define (sac-4-test vp b)
  (let* ([plr (player "TESTER" 1 10 0 0 0)]
         [bord board]
         [infi info])
    (define (helper l w)
      (cond [(null? l) (cons 0 #t)]
            [(null? (cdr l)) (cons w #f)]
            [else
             (let* ([b2 (car (remove b l))]
                    [s2 (car (get-rem-sides b2))])
               (begin
                 (join vp b2 s2 p)
                 (update-info vp p b2 s2)
                 (helper (x-boxes 3 0 0 '()) (+ w 1))))]))
    (begin
      (set! players (vector-append players (vector plr)))
      (let ([ans (helper (x-boxes 3 0 0 '()) 0)])
        (set! players (vector-take players p))
        (set! board bord)
        (set! info infi)
        ans))))

(define (play-trick-2 vp i b)
  (let* ([plr (vector-ref players i)])
    (define (helper l w)
      (cond [(null? (cdr l)) w]
            [else
             (let* ([b2 (car (remove b l))]
                    [s2 (car (get-rem-sides b2))]
                    [plr (vector-ref players i)])
               (begin
                 (show-players-scores vp i (get-side 500 (/ p 2.0)))
                 (show-current-player vp i)
                 (sleep 0.5)
                 (join vp b2 s2 i)
                 (update-info vp i b2 s2)
                 (vector-set! players i
                              (player (player-name plr)
                                      (player-nature plr)
                                      (player-color plr)
                                      (+ 1 (player-score plr))
                                      (+ 1 (player-wins plr))
                                      (player-rank plr)))
                 
                 (helper (x-boxes 3 0 0 '()) (+ w 1))))]))
    (begin
      (helper (x-boxes 3 0 0 '()) 0))))
        
(define (give-2nd vp b)
  (let* ([plr (player "TESTER" 1 10 0 0 0)]
         [bord board]
         [infi info])
    (define (helper l c b1 s1)
      (cond [(= c 2) (cons b1 s1)]
            [else
             (let* ([b2 (car (remove b l))]
                    [s2 (car (get-rem-sides b2))])
               (begin
                 (join vp b2 s2 p)
                 (update-info vp p b2 s2)
                 (helper (x-boxes 3 0 0 '()) (+ c 1) b2 s2)))]))
    (begin
      (set! players (vector-append players (vector plr)))
      (let ([ans (helper (x-boxes 3 0 0 '()) 0 0 0)])
        (set! players (vector-take players p))
        (set! board bord)
        (set! info infi)
        ans))))
  
                 
(define (play-trick vp total)
  (let* ([brd board]
         [infi info]
         [plr (player "TESTER" 1 10 0 0 0)])
    (begin
      (set! players (vector-append players (vector plr)))
      (cpu-1-turn vp p 0 1)
      (let* ([l3 (x-boxes 3 0 0'())]
             [b (car l3)]
             [s (car (get-rem-sides b))])
        (set! players (vector-take players p))
        (set! board brd)
        (set! info infi)
        (cons b s)))))

(define (play-n-times num vp i)
  (let ([plr (vector-ref players i)])
    (cond [(> num 0) (begin
                       (show-players-scores vp i (get-side 500 (/ p 2.0)))
                       (show-current-player vp i)
                       (sleep 0.5)
                       (let* ([w (cpu-2-turn vp i 0 1)])
                         (vector-set! players i
                                      (player (player-name plr)
                                              (player-nature plr)
                                              (player-color plr)
                                              (+ w (player-score plr))
                                              (+ w (player-wins plr))
                                              (player-rank plr))))
                       (play-n-times (- num 1) vp i))])))

(define (chances-remaining vp)
  (let* ([brd board]
         [infi info]
         [plr (player "TESTER" 1 10 0 0 0)])
    (define (helper c score)
      (cond [(= (* m n) score) c]
            ;[(= c p) p] 
            [else (begin
                    (let* ([w (until-loss-of-turn vp score)])
                      (helper (+ c 1) (+ w score))))]))
    (begin
      (set! players (vector-append players (vector plr)))
      (let* ([ans (helper 0 (total-score))])
        (set! players (vector-take players p))
        (set! board brd)
        (set! info infi)
        ans))))

(define (total-score)
  (define (helper i score)
    (if (= i p)
        score
        (helper (+ i 1) (+ score (player-score (vector-ref players i))))))
  (helper 0 0))

(define (get-rem-sides b)
  (let ([bx (get-box b)]
        [ans '()])
    (begin (cond [(= 0 (box-l bx)) (set! ans (cons 0 ans))]) (cond [(= 0 (box-r bx)) (set! ans (cons 1 ans))])
           (cond [(= 0 (box-u bx)) (set! ans (cons 2 ans))]) (cond [(= 0 (box-d bx)) (set! ans (cons 3 ans))])
           ans)))

(define (until-loss-of-turn vp total)
  (define (iter score)
    (if (= (* m n) (+ score total))
        score
        (let* ([w (cpu-1-turn vp p 0 1)])
          (if (= 0 w)
              score
              (iter (+ score w))))))
  (iter 0))

(define (f vp b total)
  (let* ([q (get-rem-sides b)]
         [s1 (car q)]
         [s2 (cadr q)]
         [plr (player "TESTER" 1 10 0 0 0)]
         [brd board]
         [infi info]
         [ans '()])
    (begin
      (set! players (vector-append players (vector plr)))
      (update-info vp p b s1)
      (let ([score (until-loss-of-turn vp total)])
        (set! ans (cons (list b s1 score) ans)))
      (set! info infi)
      (set! board brd)
      (set! players (vector-take players p))
      (set! players (vector-append players (vector plr)))
      (update-info vp p b s2)
      (let ([score (until-loss-of-turn vp total)])
        (set! ans (cons (list b s2 score) ans)))
      (set! info infi)
      (set! board brd)
      (set! players (vector-take players p))
      ans)))
       
(define (best-move vp lb)
  (define (best-score l b s sc)
    (cond [(null? l) (cons b s)]
          [else
           (let ([k (car l)])
             (if (< (list-ref k 2) sc) (best-score (cdr l) (list-ref k 0) (list-ref k 1) (list-ref k 2))
                 (best-score (cdr l) b s sc)))]))
  (let ([lisht (append* (map (lambda (b) (f vp b (total-score))) lb))])
    (best-score lisht (cons 0 0) 0 (* m n))))

(define (show-final-scores-left vp i c s)
  (cond [(< i p)
         (let* ([plr (vector-ref players i)]
                [sa (player-score plr)])
           (begin
             (cond [(not (= sa s)) (begin (set! s sa) (set! c (+ c 1)))])
             ((draw-string vp) (make-posn 80 (+ 250 (* i 30)))
                               (string-append
                                (number->string c) ". " (player-name plr) " : ")
                               (list-ref colors (player-color plr)))
             ((draw-string vp) (make-posn 290 (+ 250 (* i 30))) (number->string (player-score plr)) (list-ref colors (player-color plr)))
             ((draw-rectangle vp) (make-posn 70 (+ 179.5 50 (* i 30))) 260 30 "white")
             (let ([p (vector-ref players i)])
               (vector-set! players i (player (player-name p) (player-nature p) (player-color p)
                                              (player-score p) (player-wins p) (+ c (player-rank p)))))
             (show-final-scores-left vp (+ i 1) c s)))]))

(define (show-final-scores-right vp i c s l n)
  (cond [(< i p)
         (begin
           ((draw-string vp) (make-posn 470 195) "Sort by: " "gold")
           ((draw-rectangle vp) (make-posn 520 175) 105 30 "black")
           ((draw-string vp) (make-posn 540 195) "Ranks" (if (= n 0) "red" "gray"))           
           ((draw-rectangle vp) (make-posn 625 175) 105 30 "black")
           ((draw-string vp) (make-posn 635 195) "Boxes Won" (if (= n 0) "gray" "red"))
           ;((draw-rectangle vp) (make-posn (+ (* n 105) 520) 175) 105 30 "red")
           (let* ([plr (vector-ref (list-ref l n) i)]
                  [sa (cond [(= 0 n) (player-rank plr)]
                            [(= 1 n) (player-wins plr)])])
             (begin
               ((draw-solid-rectangle vp) (make-posn 472 (+ 179.5 52 (* i 30))) 256 26 "black")
               (cond [(not (= sa s)) (begin (set! s sa) (set! c (+ c 1)))])
               ((draw-string vp) (make-posn 480 (+ 250 (* i 30)))
                                 (string-append
                                  (number->string c) ". " (player-name plr) " : ")
                                 (list-ref colors (player-color plr)))
               (cond [(= n 0)
                      ((draw-string vp) (make-posn 690 (+ 250 (* i 30))) (number->string (player-rank plr)) (list-ref colors (player-color plr)))]
                     [else
                      ((draw-string vp) (make-posn 690 (+ 250 (* i 30))) (number->string (player-wins plr)) (list-ref colors (player-color plr)))])
               ((draw-rectangle vp) (make-posn 470 (+ 179.5 50 (* i 30))) 260 30 "white")
               (show-final-scores-right vp (+ i 1) c s l n))))]))

(define (final-vp vp)
  (clear-vp vp "black")
  (add-border vp "white")
  ((draw-string vp) (make-posn 305 100) "THE MATCH HAS ENDED!" "yellow")
  ((draw-string vp) (make-posn 100 150) "HERE ARE THE SCORES: " "green")
  ((draw-string vp) (make-posn 500 150) "OVERALL STANDINGS: " "violet")
  (set! players (vector-sort players (lambda (a b) (> (player-score a) (player-score b)))))
  (show-final-scores-left vp 0 0 0)
  (show-final-scores-right vp 0 0 0
                           (list (vector-sort players (lambda (a b) (< (player-rank a) (player-rank b))))
                                 (vector-sort players (lambda (a b) (> (player-wins a) (player-wins b))))) 0)
  ((draw-string vp) (make-posn 270 550) "WHAT DO YOU WANT TO DO NOW?" "cyan")
  ((draw-solid-rectangle vp) (make-posn 50 620) 200 60 "lawngreen")
  ((draw-solid-rectangle vp) (make-posn 300 620) 200 60 "silver")
  ((draw-solid-rectangle vp) (make-posn 550 620) 200 60 "red")
  ((draw-string vp) (make-posn 100 640) "PLAY AGAIN" "black")
  ((draw-string vp) (make-posn 350 640) "PLAY AGAIN" "black")
  ((draw-string vp) (make-posn 98 670) "(Same Settings)" "black")
  ((draw-string vp) (make-posn 340 670) "(Different Settings)" "black")
  ((draw-string vp) (make-posn 625 655) "EXIT!" "white")
  (get-final-info vp
                  (cons (make-posn 50 620) (make-posn 250 680))
                  (cons (make-posn 300 620) (make-posn 500 680))
                  (cons (make-posn 550 620) (make-posn 750 680))
                  (cons (make-posn 520 175) (make-posn 625 205))
                  (cons (make-posn 625 175) (make-posn 730 205))))

(define (restart-players i)
  (cond [(< i p)
         (let ([plr (vector-ref players i)])
           (begin
             (vector-set! players i (player (player-name plr) (player-nature plr) (player-color plr) 0 (player-wins plr) (player-rank plr)))
             (restart-players (+ i 1))))]))

(define (get-final-info vp b1 b2 b3 b4 b5)
  (begin
    (let ([qq (query-mouse-posn vp)])
      (cond [(check-in qq b1)
             ((draw-rectangle vp) (car b1) (- (posn-x (cdr b1)) (posn-x (car b1))) (- (posn-y (cdr b1)) (posn-y (car b1))) "white")]
            [(check-in qq b2)
             ((draw-rectangle vp) (car b2) (- (posn-x (cdr b2)) (posn-x (car b2))) (- (posn-y (cdr b2)) (posn-y (car b2))) "white")]
            [(check-in qq b3)
             ((draw-rectangle vp) (car b3) (- (posn-x (cdr b3)) (posn-x (car b3))) (- (posn-y (cdr b3)) (posn-y (car b3))) "white")]
            [else
             ((draw-rectangle vp) (car b1) (- (posn-x (cdr b1)) (posn-x (car b1))) (- (posn-y (cdr b1)) (posn-y (car b1))) "black")
             ((draw-rectangle vp) (car b2) (- (posn-x (cdr b2)) (posn-x (car b2))) (- (posn-y (cdr b2)) (posn-y (car b2))) "black")
             ((draw-rectangle vp) (car b3) (- (posn-x (cdr b3)) (posn-x (car b3))) (- (posn-y (cdr b3)) (posn-y (car b3))) "black")]))
    (let* ([mc (ready-mouse-click vp)]
           [ps (if (not (eq? #f mc)) (mouse-click-posn mc) (make-posn 0 0))])
      (cond [(check-in ps b1) (begin (restart-players 0) (start-game vp))]
            [(check-in ps b2) (begin (set! m 5) (set! n 5) (set! p 2) (draw-page-1 vp 0))]
            [(check-in ps b3)
             (close-viewport vp)]
            [(check-in ps b4)
             (begin(show-final-scores-right vp 0 0 0
                                            (list (vector-sort players (lambda (a b) (< (player-rank a) (player-rank b))))
                                                  (vector-sort players (lambda (a b) (> (player-wins a) (player-wins b))))) 0)
                   (get-final-info vp b1 b2 b3 b4 b5))]
            [(check-in ps b5)
             (begin (show-final-scores-right vp 0 0 0
                                             (list (vector-sort players (lambda (a b) (< (player-rank a) (player-rank b))))
                                                   (vector-sort players (lambda (a b) (> (player-wins a) (player-wins b))))) 1)
                    (get-final-info vp b1 b2 b3 b4 b5))]
            [else (get-final-info vp b1 b2 b3 b4 b5)]))))

;(connect-the-dots)

