#lang scheme
(#%require r5rs/init)

(define (board)
  (vector
     (list->vector '(X O X O X O X O) )
     (list->vector '(O X O X O X O X) )
     (list->vector '(X O X O X O X O) )
     (list->vector '(O X O X O X O X) )
     (list->vector '(X O X O X O X O) )
     (list->vector '(O X O X O X O X) )
     (list->vector '(X O X O X O X O) )
     (list->vector '(O X O X O X O X) )
  )
)