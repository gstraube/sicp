#lang sicp
(#%require sicp-pict)

(define (frame-outline-painter frame)
  ((segments->painter (list
                      (make-segment (vector-add
                                     (frame-edge1 frame)
                                     (frame-edge2 frame))
                                    (frame-edge1 frame))
                      (make-segment (vector-add
                                     (frame-edge1 frame)
                                     (frame-edge2 frame))
                                    (frame-edge2 frame))
                      (make-segment (frame-origin frame)
                                    (frame-edge1 frame))
                      (make-segment (frame-origin frame)
                                    (frame-edge2 frame))))
   frame))

(define (frame-x-painter frame)
  ((segments->painter (list
                      (make-segment (vector-add
                                     (frame-edge1 frame)
                                     (frame-edge2 frame))
                                    (frame-origin frame))
                      (make-segment (frame-edge1 frame)
                                    (frame-edge2 frame))))
   frame))

(define (frame-diamond-painter frame)
  ((segments->painter (list
                      (make-segment (vector-scale 0.5 (frame-edge1 frame))
                                    (vector-scale 0.5 (frame-edge2 frame)))
                      (make-segment (vector-scale 0.5 (frame-edge1 frame))
                                    (vector-add (frame-edge1 frame)
                                                (vector-scale 0.5 (frame-edge2 frame))))
                      (make-segment (vector-scale 0.5 (frame-edge2 frame))
                                    (vector-add (frame-edge2 frame)
                                                (vector-scale 0.5 (frame-edge1 frame))))
                      (make-segment (vector-add (frame-edge1 frame)
                                                (vector-scale 0.5 (frame-edge2 frame)))
                                    (vector-add (frame-edge2 frame)
                                                (vector-scale 0.5 (frame-edge1 frame))))))
   frame))
