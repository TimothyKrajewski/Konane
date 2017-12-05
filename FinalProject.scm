#lang scheme
(#%require r5rs/init)

;;This is the full board that we are starting with
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

;;This is a fully empty board
(define (EmptyBoard)
  (vector
     (list->vector '(- - - - - - - -) )
     (list->vector '(- - - - - - - -) )
     (list->vector '(- - - - - - - -) )
     (list->vector '(- - - - - - - -) )
     (list->vector '(- - - - - - - -) )
     (list->vector '(- - - - - - - -) )
     (list->vector '(- - - - - - - -) )
     (list->vector '(- - - - - - - -) )
  )
)