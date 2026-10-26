;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname maze) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/universe)
(require 2htdp/image)
(require spd/tags)

(@htdw Maze)

;: to run: (main (generate-maze r c))

;; Constants:
;(define WIDTH 700)
;(define HEIGHT 700)
(define TILE-DIM 80)
(define PLAYER-DIM 30)
;(define TOP PLAYER-DIM)
;(define BOT (- HEIGHT 1 PLAYER-DIM))
;(define LEF PLAYER-DIM)
;(define RIG (- WIDTH 1 PLAYER-DIM))
(define PLAYER (square PLAYER-DIM "solid" "green"))
;(define MTS (rectangle WIDTH HEIGHT "solid" "white"))

;; ===========================================================================
;; Data definitions:

(@htdd Player)
(define-struct player (x y))
;; Player is (make-player Number Number Number Number)
;; interp. (make-player x y) is player
;; - position x, y in screen coordinates
;; - prev position prev-x, prev-y in screen coordinates
;; CONSTRAINT: x, prev-x is in [LEF, RIG]; y, prev-y is in [TOP, BOT] !!!
(define P1 (make-player PLAYER-DIM PLAYER-DIM))

(@dd-template-rules compound)
(define (fn-for-player p)
  (... (player-x p)
       (player-y p)))

(@htdd Cell)
;; cell is one of:
;; - empty
;; - "wall"
;; - "goal"
;; interp. type of cell
(define C1 empty)
(define C2 "wall")
(define C3 "goal")
;(@dd-template-rules one-of
;                     atomic-distinct
;                     atomic-distinct
;                     atomic-distinct)
(define (fn-for-cell c)
  (cond [(empty? c) (...)]
        [(string=? c "wall") (...)]
        [(string=? c "goal") (...)]))

(@htdd Maze)
(define-struct maze (grid player))
;; Maze is (make-maze (listof (listof Cell)) Player)
;; interp. a grid !!!
;; CONSTRAINT: Inner lists are all same size
;; CONSTRAINT: Only only one "goal"
;; CONSTRAINT: Maze must be solvable\
(define M0 (make-maze (list (list C3)) P1))
(define M1 (make-maze (list (list C1 C3)) P1))
(define M2 (make-maze (list (list C1) (list C1) (list C3)) P1))
(define M3 (make-maze (list (list C1 C1) (list C2 C1) (list C2 C3)) P1))
(define M4 (make-maze (list (list C1 C1 C1) (list C2 C2 C1) (list C3 C1 C1))
                      P1))
(define M5 (make-maze (list (list C1 C1 C1) (list C2 C2 C1) (list C2 C2 C3))
                      P1))
(define M6 (make-maze (list (list C1 C1 C2) (list C2 C1 C1) (list C2 C2 C3))
                      P1))
(define M7 (make-maze (list (list C1 C1) (list C2 C1) (list C1 C3)) P1))
;(@dd-template-rules compound ref)
(define (fn-for-maze m)
  (... (fn-for-grid (maze-grid m))
       (maze-player m)))
; (define (fn-for-grid m)
;   (... (fn-for-loc (first m))
;        (fn-for-grid (rest m))))

(@htdd Pos)
(define-struct pos (x y))
;; Pos is (make-pos Integer Integer)
;; interp. represents a position in the grid x, y

;; ===========================================================================
;; Functions:

(@htdf main)
(@signature Maze -> Maze)
;; start the game, call with (main M1) !!!
;; <no tests for main functions>
(@template-origin htdw-main)
(define (main m)
  (big-bang m
    (on-draw   render-maze)
    (on-key    handle-key)
    (on-mouse  handle-mouse)))

(@htdf render-maze)
(@signature Maze -> Image)
;; render maze with player in grid
;(define (render-maze m) empty-image);stub
(@template-origin Maze)
(define (render-maze m)
  (local [(define p (maze-player m))]
    (place-image PLAYER (player-x p) (player-y p) (render-grid (maze-grid m)))))

(@htdf render-grid)
(@signature (listof (listof Cell)) -> Image)
;; 
;(define (render-grid m) empty-image);stub
(@template-origin use-abstract-fn)
(define (render-grid g)
  (foldr above empty-image (map render-row g)))

(@htdf render-row)
(@signature (listof Cell) -> Image)
;; 
;(define (render-row m) empty-image);stub
(@template-origin use-abstract-fn)
(define (render-row r)
  (foldr beside empty-image (map render-cell r)))

(@htdf render-cell)
(@signature Cell -> Image)
;; Renders a cell based on its type
;(define (render-cell c) empty-image);stub
(define (render-cell c)
  (cond [(empty? c) (square TILE-DIM "solid" "white")]
        [(string=? c "wall") (square TILE-DIM "solid" "black")]
        [(string=? c "goal") (square TILE-DIM "solid" "red")]))

(@htdf handle-mouse)
(@signature Maze Integer Integer MouseEvent -> Maze)
;; 
;(define (handle-mouse m x y me) m);stub
(@template-origin MouseEvent)
(define (handle-mouse m x y me)
  (make-maze (maze-grid m) (player-mouse m x y me)))

(@htdf player-mouse)
(@signature Maze Integer Integer MouseEvent -> Player)
;; 
;(define (player-mouse m x y me) m);stub
(define (player-mouse m x y me)
  (cond [(mouse=? me "drag") (mouse-drag m x y)]
        [else (maze-player m)]))

(@htdf mouse-drag)
(@signature Maze Integer Integer -> Player)
;; 
;(define (mouse-drag m x y) m);stub
(define (mouse-drag m x y)
  (local [(define p-x (player-x (maze-player m)))
          (define p-y (player-y (maze-player m)))
          (define grid (maze-grid m))
          (define d (sqrt (+ (sqr (- x p-x)) (sqr (- y p-y)))))
          (define row (floor (/ y TILE-DIM)))
          (define col (floor (/ x TILE-DIM)))
          
          (define (cell-valid? r c)
            (and (>= r 0) (< r (length grid))
                 (>= c 0) (< c (length (first grid)))
                 (not (and (string? (list-ref (list-ref grid r) c))
                           (string=? (list-ref (list-ref grid r) c) "wall")))))
          
          (define curr-cell (if (cell-valid? row col)
                                (list-ref (list-ref grid row) col)
                                "wall"))
          
          (define (valid-postion? x y)
            (local [(define (bounds op coord)
                      (floor (/ (op coord (/ PLAYER-DIM 2)) TILE-DIM)))
                    (define corners
                      (list (list - -) (list - +) (list + -) (list + +)))]
              
              (andmap (λ (n)
                        (cell-valid? (bounds (first n) y)
                                     (bounds (second n) x)))
                      corners)))
          (define (center i) (+ (/ TILE-DIM 2) (* i TILE-DIM)))]
    (if (and (<= d (/ PLAYER-DIM 2)) (valid-postion? x y))
        (cond [(empty? curr-cell) (make-player x y)]
              [(string=? curr-cell "goal") (make-player (center col)
                                                        (center row))]
              [(string=? curr-cell "wall") (maze-player m)])
        (maze-player m))))

(@htdf handle-key)
(@signature Maze KeyEvent -> Maze)
;; 
;(define (handle-key m ke)m);stub
(@template-origin KeyEvent)
(define (handle-key m ke)
  (cond [(key=? ke "r") (make-maze
                         (maze-grid m) (make-player PLAYER-DIM PLAYER-DIM))]
        [else m]))

(@htdf generate-maze)
(@signature Integer Integer -> Maze)
;; generates maze of size given with a single goal tile placed
;(define (generate-maze rows cols) M1);stub
(define (generate-maze rows cols)
  (make-maze (set-goal (algo-maker
                        (make-pos 0 0) empty (wall-maze rows cols) empty))
             (make-player PLAYER-DIM PLAYER-DIM)))

(@htdf wall-maze)
(@signature Integer Integer -> (listof (listof Cell)))
;; creates maze of all walls of given size
;(define (wall-maze rows cols) M1);stub
(define (wall-maze rows cols)
  (build-list rows (λ (n)
                     (build-list cols (λ (m)
                                        (if (or (odd? m)
                                                (odd? n))
                                            "wall" empty))))))

(@htdf algo-maker)
(@signature Pos (listof Pos) (listof (listof Cell)) (listof Pos) ->
            (listof (listof Cell)))
;; depth-first search algorithm of maze generation
;(define (algo-maker curr-p visited g) empty);stub
(define (algo-maker curr visited g stack)
  (local [(define vis (cons curr visited))
          (define unvis-neighb
            (filter (λ (p) (and (>= (pos-x p) 0)
                                (< (pos-x p) (length g))
                                (>= (pos-y p) 0)
                                (< (pos-y p) (length (first g)))
                                (not (member? p visited))))
                    (list (make-pos (pos-x curr) (+ (pos-y curr) 2))
                          (make-pos (+ (pos-x curr) 2) (pos-y curr))
                          (make-pos (pos-x curr) (- (pos-y curr) 2))
                          (make-pos (- (pos-x curr) 2) (pos-y curr)))))
          (define shuff (shuffle unvis-neighb))
          (define s (append shuff stack))]
    (cond [(empty? s) g]
          [(empty? shuff) (algo-maker (first s)
                                      (cons (first s) vis)
                                      (remove-wall curr (first s) g)
                                      (rest s))]
          
          [else (algo-maker (first shuff)
                            (cons (first shuff) vis)
                            (remove-wall curr (first shuff) g)
                            (append (rest shuff) stack))])))

(@htdf shuffle)
;; 
(define (shuffle l)
  (if (empty? l)
      empty
      (local [(define i (random (length l)))
              (define chosen (list-ref l i))
              (define rest-l (remove chosen l))]
        (cons chosen (shuffle rest-l)))))

(@htdf remove-wall)
(@signature Pos Pos (listof (listof Cell)) -> (listof (listof Cell)))
;; 
;(define (remove-wall n g) empty);stub
(define (remove-wall curr neighb g)
  (local [(define mid-x (/ (+ (pos-x curr) (pos-x neighb)) 2))
          (define mid-y (/ (+ (pos-y curr) (pos-y neighb)) 2))
          (define (update-row row y value)
            (cond [(= y 0) (cons value (rest row))]
                  [else (cons (first row) (update-row
                                           (rest row) (- y 1) value))]))
          (define (update-grid g x y value)
            (cond [(= x 0) (cons (update-row (first g) y value) (rest g))]
                  [else (cons (first g) (update-grid
                                         (rest g) (- x 1) y value))]))]
    (update-grid g mid-x mid-y empty)))

(@htdf set-goal)
(@signature (listof (listof Cell)) -> (listof (listof Cell)))
;; Sets the "goal" in the maze based on the furthest position from (0, 0).
;(define (set-goal g) g);stub
(define (set-goal g)
  (local [(define (set-goal-helper wl visited max-d best-p)
            (cond [(empty? wl) best-p]
                  [else
                   (local [(define curr (first wl))
                           (define p (first curr))
                           (define d (second curr))
                           (define next-vis (cons p visited))
                           (define neighbs (unvis-neighb p next-vis))
                           (define next-wl (append (rest wl)
                                                   (map
                                                    (λ (n) (list n (+ d 1)))
                                                    neighbs)))]
                     (if (> d max-d)
                         (set-goal-helper next-wl next-vis d p)
                         (set-goal-helper next-wl next-vis max-d best-p)))]))
          
          (define (unvis-neighb p visited)
            (filter (λ (n) (and (>= (pos-x n) 0)
                                (< (pos-x n) (length g))
                                (>= (pos-y n) 0)
                                (< (pos-y n) (length (first g)))
                                (not (member? n visited))
                                (not
                                 (local [(define cell
                                           (list-ref (list-ref g
                                                               (pos-x n))
                                                     (pos-y n)))]
                                       (and (string? cell) 
                                            (string=? cell "wall"))))))
                    (list (make-pos (pos-x p) (+ (pos-y p) 1))
                          (make-pos (+ (pos-x p) 1) (pos-y p))
                          (make-pos (pos-x p) (- (pos-y p) 1))
                          (make-pos (- (pos-x p) 1) (pos-y p)))))

          
          (define (place-goal p grid)
            (local [(define (update-row row y)
                      (if (= y 0)
                          (cons "goal" (rest row))
                          (cons (first row) (update-row (rest row) (- y 1)))))
                    (define (update-grid grid x)
                      (if (= x 0)
                          (cons (update-row (first grid) (pos-y p))
                                (rest grid))
                          (cons (first grid)
                                (update-grid (rest grid) (- x 1)))))]
              (update-grid grid (pos-x p))))
          
          (define target (set-goal-helper
                          (list (list (make-pos 0 0) 0)) empty -1
                          (make-pos 0 0)))]
    (place-goal target g)))
