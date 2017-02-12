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


