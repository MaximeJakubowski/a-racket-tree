#lang racket
(require racket/draw)
(require racket/gui/base)

(struct point (x y) #:transparent)
(struct line (a b) #:transparent)

(define [rotate-point ang origin target]
  (let ([s (sin ang)]
        [c (cos ang)]
        [px (- (point-x target) (point-x origin))]
        [py (- (point-y target) (point-y origin))])
    (point (+ (- (* px c) (* py s)) (point-x origin))
           (+ (+ (* px s) (* py c)) (point-y origin)))))

(define [simple-tree depth seed alpha len]
  (cond
    [(eq? depth 0) '()]
    [else
     (let* ([a (point (point-x seed) (+ (point-y seed) len))]
            [b (point (point-x seed) (+ (point-y seed) len))]
            [p1 (rotate-point (/ alpha 2) seed a)]
            [p2 (rotate-point (- 0 (/ alpha 2)) seed b)])
       (append
        (cons (line seed p1) (simple-tree (- depth 1) p1 alpha len))
        (cons (line seed p2) (simple-tree (- depth 1) p2 alpha len))))]))

(define [st-decay depth seed alpha len decay]
  (cond
    [(eq? depth 0) '()]
    [else
     (let* ([a (point (point-x seed) (+ (point-y seed) len))]
            [b (point (point-x seed) (+ (point-y seed) len))]
            [p1 (rotate-point (/ alpha 2) seed a)]
            [p2 (rotate-point (- 0 (/ alpha 2)) seed b)])
       (append
        (cons (line seed p1) (st-decay (- depth 1) p1 alpha (* len decay) decay))
        (cons (line seed p2) (st-decay (- depth 1) p2 alpha (* len decay) decay))))]))

(define [st-decay-spread depth seed alpha len decay spread]
   (cond
    [(eq? depth 0) '()]
    [else
     (let* ([a (point (point-x seed) (+ (point-y seed) len))]
            [b (point (point-x seed) (+ (point-y seed) len))]
            [p1 (rotate-point (/ alpha 2) seed a)]
            [p2 (rotate-point (- 0 (/ alpha 2)) seed b)])
       (append
        (cons (line seed p1) (st-decay-spread (- depth 1) p1 (* alpha spread) (* len decay) decay spread))
        (cons (line seed p2) (st-decay-spread (- depth 1) p2 (* alpha spread) (* len decay) decay spread))))]))

(define [st-decay-spread-rotate depth seed alpha len decay spread rot trot]
  (cond
    [(eq? depth 0) '()]
    [else
     (let* ([a (point (point-x seed) (- (point-y seed) len))]
            [b (point (point-x seed) (- (point-y seed) len))]
            [p1 (rotate-point (+ (/ alpha 2) trot) seed a)]
            [p2 (rotate-point (+ (- 0 (/ alpha 2)) trot) seed b)])
       (append
        (cons (line seed p1) (st-decay-spread-rotate (- depth 1) p1 (* alpha spread) (* len decay) decay spread rot (+ trot rot)))
        (cons (line seed p2) (st-decay-spread-rotate (- depth 1) p2 (* alpha spread) (* len decay) decay spread rot (- trot rot)))))]))

(define [draw-tree f filename]
  (define target (make-bitmap 1024 1024))
  (define dc (new bitmap-dc% [bitmap target]))
  (define [dc-draw s]
    (cond
      [(null? s) (send target save-file filename 'png)]
      [else
       (send dc draw-line
             (point-x (line-a (car s))) (point-y (line-a (car s)))
             (point-x (line-b (car s))) (point-y (line-b (car s))))
       (dc-draw (cdr s))]))
  (dc-draw f))



;(define t (st-decay-spread-rotate 5 (point 512 256) (/ pi 3) 120 0.8 1.0 (/ pi 4) 0))
;(draw-tree t "frac.png")

(define frame (new frame%
                   [label "Tree"]
                   [width 1024]
                   [height 512]))

(define [dc-draw dc s]
  (cond
    [(null? s) (void)]
    [else
     (send dc draw-line
           (point-x (line-a (car s))) (point-y (line-a (car s)))
           (point-x (line-b (car s))) (point-y (line-b (car s))))
     (dc-draw dc (cdr s))]))

(define hpanel (new horizontal-panel%
                       [parent frame]
                       [alignment '(center center)]))

(define controlpanel (new vertical-panel%
                          [parent hpanel]
                          [stretchable-width #f]))

(define tree-depth 0)
(define tree-alpha (/ pi 2))
(define tree-length 10)
(define tree-length-decay 1.0)
(define tree-alpha-decay 1.0)
(define tree-branch-rotation (/ pi 4))

(define tree (st-decay-spread-rotate 0 (point 0 0) (/ pi 3) 120 0.8 1.0 (/ pi 4) 0))

(define depth-slider (new slider%
                          [parent controlpanel]
                          (label "Tree depth")
                          (min-value 0)
                          (max-value 20)
                          (init-value 4)))

(define alpha-slider (new slider%
                          [parent controlpanel]
                          (label "Branch angle")
                          (min-value 0)
                          (max-value 360)
                          (init-value 90)))

(define length-slider (new slider%
                           [parent controlpanel]
                           (label "Branch length")
                           (min-value 0)
                           (max-value 100)
                           (init-value 50)))

(define alpha-decay-slider (new slider%
                                [parent controlpanel]
                                (label "Branch angle decay")
                                (min-value -100)
                                (max-value 100)
                                (init-value 0)))

(define length-decay-slider (new slider%
                                 [parent controlpanel]
                                 (label "Branch length decay")
                                 (min-value 0)
                                 (max-value 100)
                                 (init-value 0)))

(define rotation-slider (new slider%
                             [parent controlpanel]
                             (label "Tree rotation")
                             (min-value 0)
                             (max-value 360)
                             (init-value 0)))

(define gobutton (new button%
                      [parent controlpanel]
                      [label "Generate"]
                      [callback (lambda (button event)
                                  (set! tree (st-decay-spread-rotate
                                              (send depth-slider get-value)
                                              (point 768 768)
                                              (degrees->radians (send alpha-slider get-value))
                                              (send length-slider get-value)
                                              (- 1 (/ (send length-decay-slider get-value) 100))
                                              (- 1 (/ (send alpha-decay-slider get-value) 100))
                                              (degrees->radians (send rotation-slider get-value))
                                              0))
                                  (send tree-view on-paint))]))

(define tree-view (new canvas% [parent hpanel]
                       [paint-callback
                        (lambda (canvas dc)
                          (send dc clear)
                          (send dc set-smoothing 'aligned)
                          (dc-draw dc tree))]))

(send frame show #t)


