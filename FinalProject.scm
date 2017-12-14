#lang scheme
(#%require r5rs/init)
;Tim Krajewski
;Chris Strebenz 
;Kevin Ackerman
;Josh Blanks
;James Kennedy

;Global variables that hold the infomation we need to produce the end of game statistics
(define evals 0)
(define cuts 0)
(define allBranches 0)
(define branches 0)


;;------------------------------------Error handling------------------------------------
;Checks if input is not an int
(define (notInt caller)
  (display "Enter from 1 to 8.")(newline)
  (cond
    ((equal? "secondmove" caller) (secondmove))
    ((equal? "fMv" caller) (fMv))))
;Checks if input is out of bounds
(define (NotInBounds n caller)
  (display "Enter from 1 to 8.")(newline)
  (cond
    ((equal? "secondmove" caller) (secondmove))
    ((equal? "fMv" caller) (fMv))))
;Checks first move 
(define (firstMoveCheck caller)
  (display "Enter <1, 8>, <8, 1>, <4, 5>, and <5, 4>.")(newline)
  (cond
    ((eqv? "secondmove" caller) (secondmove))
    ((eqv? "fMv" caller) (fMv))))
;Checks if a move in possible
(define (cantMove currState turn human CPU)
  (display "You can't move there")(newline) (gamer currState turn human CPU))
;Also checks if input is out of bounds
(define (outOfBound currState turn human CPU)
  (display "Enter from 1 to 8.") (newline) (gamer currState turn human CPU))

;Also checks if the input is not an int
(define (NotInt currState turn human CPU)
  (display "Enter between 1 to 8.")(newline) (gamer currState turn human CPU))

;;------------------------------------Standard Functiions------------------------------------

;;Performs car on all elements of a given list of lists
(define carOnAll
  (lambda (carOnThis)
    (map car carOnThis)
    ))
;;Performs car on all elements of a given list of lists
(define cdrOnAll
  (lambda (cdrOnThis)
    (map car cdrOnThis)
    ))
;;Prints out the given input. Used primarily for debugging
(define printThisOut
  (lambda (theThing)
    (print theThing)
    ))

;;This is a flatten standard  function and is based on https://rosettacode.org/wiki/Flatten_a_list#Scheme
(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))


;Finds the maximum value in a list
(define MaxInList
  (lambda (list)
    (if (= (length list) 1)
        (car list)
        (max (car list) (MaxInList (cdr list)))
    )
  )
)

;Finds the minimum value in a list
(define MinInList
  (lambda (list)
    (if (= (length list) 1)
        (car list)
        (min (car list) (MinInList (cdr list)))
    )
  )
)
;This function is called at the end of a game when the computer has won. Shows end of game stats
(define (CPUWin)
  (display "The CPU won!")
  (newline)
  (display "Data shows:")(newline)
  (display "Number of evaluations: ")(display evals)(newline)
  (display "Number of cuts: ")(display cuts)(newline)
  (display "Average branch factor: ")(display (/ allBranches branches))(newline))
;This function is called at the end of a game when the  has won. Shows end of game stats
(define (HumanWin)
  (display "You won!")
  (newline)
  (display "Data shows:")(newline)
  (display "Number of evaluations: ")(display evals)(newline)
  (display "Number of cuts: ")(display cuts)(newline)
  (display "Average branch factor: ")(display (/ allBranches branches))(newline))
;Checks if an item is in the given list
(define (IsIn list SearchTerm)
    (cond ((null? list) #f)
        ((equal? (car list) SearchTerm) #t)
        (else (IsIn (cdr list) SearchTerm))))


;Attempts to make a move up. Move is performed and added to the list if it is possible
(define MoveUp
  (lambda (currState CI RI origCol origRow movesList)
    (if
      (and
        (< (+ RI 2) 8)
        (eqv? '- (vector-ref (vector-ref currState (+ RI 2)) CI))
        (not (eqv? '- (vector-ref (vector-ref currState (+ RI 1)) CI)))
      )
      (MoveUp currState CI (+ 2 RI) origCol origRow (append movesList (list (list (list origCol origRow) (list CI (+ RI 2))))))
      movesList
    )
  )
)
;Checks to see if the board is in a gameover state and if so, determines the winner
(define (determineWinner currState playerUp human CPU)
  (set! evals (+ evals 1))
  (cond
    ((= (TotalMovable currState CPU 0 0) 0)
     100)
    ((= (TotalMovable currState human 0 0) 0)
     0)
    (else (/ (TotalMovable currState human 0 0)
     (TotalMovable currState CPU 0 0)))))

;A helper function for running the miniMax algorithm
(define (leaf currState playerUp human CPU)
  (or
   (and (= (TotalMovable currState CPU 0 0) 0) (eqv? playerUp CPU))
   (and (= (TotalMovable currState human 0 0) 0) (eqv? playerUp human))
  )
)

;Returns a list of all possible moves that can be made by the current player in a row
(define (evalMovesInRow currState playerUp RI CI movesList)
  (cond
    ((> CI 7) movesList)
    ((eqv? playerUp (vector-ref (vector-ref currState RI) CI))
       (evalMovesInRow currState playerUp RI (+ CI 2)
                           (MoveUp currState CI RI CI RI
                                              (MoveDown currState CI RI CI RI
                                                                (MoveRight currState CI RI CI RI
                                                                                (MoveLeft currState CI RI CI RI movesList)))))
     )
    (else (evalMovesInRow currState playerUp RI (+ CI 1) movesList))))
;Finds the total number of moveable pieces given the current state
(define (TotalMovable currState playerUp RI sum)
  (cond ((= RI 8) sum)
        (else (TotalMovable currState playerUp (+ RI 1) (+ sum (TotalMovableInRow currState playerUp RI 0 0))))))

;Determines the number of movable pieces in a given row
(define (TotalMovableInRow currState playerUp RI CI sum)
  (cond
    ((> CI 7) sum)
    ((eqv? playerUp (vector-ref (vector-ref currState RI) CI))
     (cond
       ((and
         (< (+ RI 2) 8)
         (eqv? '- (vector-ref (vector-ref currState (+ RI 2)) CI))
         (not (eqv? '- (vector-ref (vector-ref currState (+ RI 1)) CI))))
        (TotalMovableInRow currState playerUp RI (+ CI 2) (+ sum 1)))
       ((and
         (> (- RI 2) -1)
         (eqv? '- (vector-ref (vector-ref currState (- RI 2)) CI))
         (not (eqv? '- (vector-ref (vector-ref currState (- RI 1)) CI))))
        (TotalMovableInRow currState playerUp RI (+ CI 2) (+ sum 1)))
       ((and
         (< (+ CI 2) 8)
         (eqv? '- (vector-ref (vector-ref currState RI) (+ CI 2)))
         (not (eqv? '- (vector-ref (vector-ref currState RI) (+ CI 1)))))
        (TotalMovableInRow currState playerUp RI (+ CI 2) (+ sum 1)))
       ((and
         (> (- CI 2) -1)
         (eqv? '- (vector-ref (vector-ref currState RI) (- CI 2)))
         (not (eqv? '- (vector-ref (vector-ref currState RI) (- CI 1)))))
        (TotalMovableInRow currState playerUp RI (+ CI 2) (+ sum 1)))
       (else (TotalMovableInRow currState playerUp RI (+ CI 2) sum))))
    (else (TotalMovableInRow currState playerUp RI (+ CI 1) sum))))

;Duplicates the board
(define (dupBoard currState)
  (list->vector (map list->vector (map vector->list (vector->list currState))))
)

;Returns a list of all possible moves that can be made by the current player
(define (evalMoves currState playerUp RI movesList)
  (cond ((= RI 8) movesList)
        (else (evalMoves currState playerUp (+ RI 1) (append movesList (evalMovesInRow currState playerUp RI 0 '()))))))

;Attempts to make a move left
(define MoveLeft
  (lambda (currState CI RI origCol origRow movesList)
    (if
      (and
        (> (- CI 2) -1)
        (eqv? '- (vector-ref (vector-ref currState RI) (- CI 2)))
        (not (eqv? '- (vector-ref (vector-ref currState RI) (- CI 1))))
      )
      (MoveLeft currState (- CI 2) RI origCol origRow
                        (append movesList (list (list (list origCol origRow) (list (- CI 2) RI)))))
      movesList
    )
  )
)

;Attempts to make a move down
(define MoveDown
  (lambda (currState CI RI origCol origRow movesList)
    (if
      (and
        (> (- RI 2) -1)
        (eqv? '- (vector-ref (vector-ref currState (- RI 2)) CI))
        (not (eqv? '- (vector-ref (vector-ref currState (- RI 1)) CI)))
      )
      (MoveDown currState CI (- RI 2) origCol origRow (append movesList (list (list (list origCol origRow) (list CI (- RI 2))))))
      movesList
    )
  )
)

;Attempts to make a move right
(define MoveRight
  (lambda (currState CI RI origCol origRow movesList)
    (if
      (and
        (< (+ CI 2) 8)
        (eqv? '- (vector-ref (vector-ref currState RI) (+ CI 2)))
        (not (eqv? '- (vector-ref (vector-ref currState RI) (+ CI 1))))
      )
      (MoveRight currState (+ CI 2) RI origCol origRow (append movesList (list (list (list origCol origRow) (list (+ CI 2) RI)))))
      movesList
    )
  )
)

;Creates a set of all possible child boards that can occur using any possible move
(define ExploredBoards
  (lambda (currState playerUp)
    (map (lambda (mv) (move (dupBoard currState) mv)) (evalMoves currState playerUp 0 '()))
  )
)


;Picks the move with the highest evaluation score
(define maxedMove
  (lambda (list)
    (if (= (length list) 1)
        (car list)
        (if (> (caar list) (car (maxedMove (cdr list))))
            (car list)
            (maxedMove (cdr list))
        )
    )
  )
)

;Finds the min score of the child boards
(define minChild
  (lambda (ExploredBoards level playerUp human CPU imin v iter)
    (if (< v imin)
        ((lambda () (set! cuts (+ cuts 1)) (set! allBranches (+ allBranches iter)) (set! branches (+ branches 1)) imin))
        (if (= (length ExploredBoards) 0)
            ((lambda () (set! allBranches (+ allBranches iter)) (set! branches (+ branches 1)) v))
            (minChild (cdr ExploredBoards) level playerUp human CPU imin (min v (pruning (car ExploredBoards) (- level 1) (if (eq? playerUp human) CPU human) human CPU imin v)) (+ iter 1))
        )
    )
  )
)

;Finds the max score from the child boards
(define maxedChild
  (lambda (ExploredBoards level playerUp human CPU v imax iter)
    (if (> v imax)
        ((lambda () (set! cuts (+ cuts 1)) (set! allBranches (+ allBranches iter)) (set! branches (+ branches 1)) imax))
        (if (= (length ExploredBoards) 0)
            ((lambda () (set! allBranches (+ allBranches iter)) (set! branches (+ branches 1)) v))
            (maxedChild (cdr ExploredBoards) level playerUp human CPU (max v (pruning (car ExploredBoards) (- level 1) (if (eq? playerUp human) CPU human) human CPU v imax)) imax (+ iter 1))
        )
    )
  )
)


;MiniMax algorithm
(define RunMiniMax
  (lambda (currState level playerUp human CPU)
      (cond
        ((leaf currState playerUp human CPU) (determineWinner currState playerUp human CPU))
        ((= level 0) (determineWinner currState playerUp human CPU))
        (else
          (let ((cboards (ExploredBoards currState playerUp)))
            (set! allBranches (+ allBranches (length cboards)))
            (set! branches (+ branches 1))
          (if (eq? playerUp human)
              (MaxInList (map (lambda (b) (RunMiniMax b (- level 1) CPU human CPU)) cboards))
              (MinInList (map (lambda (b) (RunMiniMax b (- level 1) human human CPU)) cboards))
          ))
        )
      )
  )
)

;The RunMiniMax algorithm which is defined above, but this version has
;alpha-beta pruning.
(define pruning
  (lambda (currState level playerUp human CPU imin imax)
    (cond
        ((leaf currState playerUp human CPU) (determineWinner currState playerUp human CPU))
        ((= level 0) (determineWinner currState playerUp human CPU))
        (else
          (if (eq? playerUp human)
              (maxedChild (ExploredBoards currState playerUp) level playerUp human CPU imin imax 0)
              (minChild (ExploredBoards currState playerUp) level playerUp human CPU imin imax 0)
          )
        )
      )
  )
)

;Finds the move with the best chance of winning
(define (findBestMove currState level currentTarget human CPU)
  (cadr (maxedMove (map (lambda (mv) (list (pruning (move (dupBoard currState) mv) (- level 1) CPU human CPU 0 100) mv)) (evalMoves currState currentTarget 0 '()))))
)



;Finds best possible second move 
(define (secondmovechooser currState fMv level)
  (cadr (maxedMove (map (lambda (move) (list (pruning (putpiece (dupBoard currState) (car move) (cadr move) '-) (- level 1) 'x 'o 'x 0 100) move))
                       (secondMoves fMv))))
)

;Finds all possible moves for the second move
(define (secondMoves fMv)
  (append
    (if (> (car fMv) 0)
        (list (list (- (car fMv) 1) (cadr fMv)))
        '()
    )
        (append
          (if (< (car fMv) 7)
              (list (list (+ (car fMv) 1) (cadr fMv)))
              '()
          )
              (append
                (if (> (cadr fMv) 0)
                    (list (list (car fMv) (- (cadr fMv) 1)))
                    '()
                )
                  (if (< (cadr fMv) 7)
                     (list (list (car fMv) (+ (cadr fMv) 1)))
                     '()
                  )
              )
        )
  )
)

;Creates initial game board
(define (makeboard)
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

;Updates the location of a currentTarget at an x-y coordinate
(define (putpiece currState x y p)
  (vector-set! (vector-ref currState y) x p)
  currState)

;Prints out the currentTarget removed by the computer if the computer goes 2nd.
(define (printinitmove m)
  (display (string-append "Remove currentTarget at <"
                          (number->string (+ (car m) 1))
                          ","
                          (number->string (- 8 (cadr m)))
                          ">"
                          "\n"))
  m)

;Helper function for printing currState to console
(define (PrintHelper arr r)
  (if (vector? arr)
      (let ((l (vector->list arr))) (if (if (display r) (display " ")) (PrintHelper l r)))
      (if (list? arr)
          (if (= 0 (length arr)) (display "\n") (if (PrintHelper (car arr) r) (PrintHelper (cdr arr) (- r 1))))
          (if (display arr) (display " "))))
  #t)

;Prints out the move that was just performed by the computer.
(define (printmove dp)
  (display (string-append "Move currentTarget at <"
                          (number->string (+ (caar dp) 1))
                          ","
                          (number->string (- 8 (cadar dp)))
                          "> to location <"
                          (number->string (+ (caadr dp) 1))
                          ","
                          (number->string (- 8 (cadadr dp)))
                          ">"
                          "\n"))
  dp)



;Checks that the given move only uses integers. only uses integers that are on the currState, and only uses a possible move
;If everything is valid, performs the move
(define (validMove currState turn human CPU)
  (let ((combiner (list
   (inputnum "1" "combiner" #f)
   (inputnum "2" "combiner" #f))))
    (cond
      ((IsIn (map integer? (flatten combiner)) #f)
       (NotInt currState turn human CPU))
      ((or (< (MinInList (flatten combiner)) 0) (> (MaxInList (flatten combiner)) 7))
       (outOfBound currState turn human CPU))
      ((not (IsIn (evalMoves currState CPU 0 '()) combiner))
       (cantMove currState turn human CPU))
      (else (gamer (move currState combiner) 1 human CPU)))))

;Clears the current state
(define (clear currState x1 y1 x2 y2)
  (if (and (= x1 x2) (= y1 y2))
      (if (putpiece currState x1 y1 '-)
       currState)
      (let (
            (temp1 (if (= x1 x2)
                    (if (< y1 y2) y1 y2)
                    (if (< x1 x2) x1 x2)))
            (temp2 (if (= x1 x2)
                    (if (>= y1 y2) y1 y2)
                    (if (>= x1 x2) x1 x2)))
            (temp3 (if (= x1 x2)
                    (if (< y1 y2) x1 x2)
                    (if (< x1 x2) y1 y2)))
            (temp4 (if (= x1 x2)
                    (if (>= y1 y2) x1 x2)
                    (if (>= x1 x2) y1 y2))))
        (clear (putpiece currState (if (= x1 x2) temp3 temp1) (if (= x1 x2) temp1 temp3) '-)
                   (if (= x1 x2) temp3 (+ 1 temp1))
                   (if (= x1 x2) (+ 1 temp1) temp3)
                   (if (= x1 x2) temp4 temp2)
                   (if (= x1 x2) temp2 temp4)))))

;If the computer goes first, asks the computer player for combiner, then the opponent player for combiner
(define (fMv)
  (display "You are O!") (newline)
  (let ((f (if (display (string-append "Please tell me where I should move first (I can decide for myself after this): " "\n")) (inputnum "" "fMv" #t))))
    (let ((s (if (display (string-append "Your move: " "\n")) (inputnum "" "fMv" #f))))
      (gamer (putpiece (putpiece (makeboard) (car f) (cadr f) '-) (car s) (cadr s) '-) 1 'X 'O))))

;If the computer goes second, asks for the opponent players first move, then the computer removes an adjacent currentTarget
(define (secondmove)
  (display "You are X!") (newline)
  (let ((f (if (display (string-append "Your first move: " "\n")) (inputnum "" "secondmove" #t))))
    (let ((b (putpiece (makeboard) (car f) (cadr f) '-)))
      (let ((s (printinitmove (secondmovechooser b f 4))))
        (gamer (putpiece b (car s) (cadr s) '-) 2 'O 'X)))))

;Initiates the game based on which player goes first
(define (init turn)
  (if (= turn 1) (fMv) (secondmove)))

;Performs the given move onto the currState
(define (move currState dp)
  (let ((p (vector-ref (vector-ref currState (cadar dp)) (caar dp))))
    (putpiece (clear currState (caar dp) (cadar dp) (caadr dp) (cadadr dp)) (caadr dp) (cadadr dp) p)))

;Asks the user for combiner and reads the combiner, performing a series of checks for valid combiner
(define (inputnum n caller firstMove)
  (list (cond
          ((display (string-append "X" n ": "))
           (let ((combiner (read)))
             (if firstMove
                 (if (not (integer? combiner)) (notInt caller)
                     (if (or (< combiner 1) (> combiner 8)) (NotInBounds n caller)
                         (if (not (or (= combiner 1) (= combiner 8) (= combiner 4) (= combiner 5))) (firstMoveCheck caller) (- combiner 1))))
                 (if (integer? combiner) (- combiner 1) combiner)))))
        (cond
          ((display (string-append "Y" n ": "))
           (let ((combiner (read)))
             (if firstMove
                 (if (not (integer? combiner)) (notInt caller)
                     (if (or (< combiner 1) (> combiner 8)) (NotInBounds n caller)
                         (if (not (or (= combiner 1) (= combiner 8) (= combiner 4) (= combiner 5))) (firstMoveCheck caller) (- 8 combiner))))
                 (if (integer? combiner) (- 8 combiner) combiner)))))))

;Prints the currState, then either performs the computer's move or asks for and performs the opponent's move
(define (gamer currState turn human CPU)
  (display (string-append "  1 2 3 4 5 6 7 8" "\n"))
  (PrintHelper (vector->list currState) 8) 
  (if (= turn 1)
      (if (endGame currState human) #f
          (if (display (string-append "Computer Turn: " "\n"))
              (gamer (move currState (printmove (findBestMove currState 4 human human CPU))) 2 human CPU)))
      (if (endGame currState CPU) #t
          (if (display (string-append "Your Turn: " "\n"))
              (validMove currState turn human CPU)))))

;Determines whether there are no possible moves on the currState for a player
(define (endGame currState turn)
  (if (= (length (evalMoves currState turn 0 '())) 0) #t #f))

;Runs the entire game from the start to end
(define (StartGame)
  (if
   (let ((turn (if (equal? (if (display "Does the computer start 'Yes' or 'No' ") (let ((answer (read)))
    (if (not (or (equal? answer 'yes) (equal? answer 'no)))
        ((display "Enter 'Yes' or 'No'")(newline)
  (StartGame))
        answer))) 'Yes) 1 2)))
     (init turn))
   (CPUWin)
   (HumanWin)))

(StartGame)
