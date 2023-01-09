;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname balls-in-tubes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;A Game is (make-game Nat Nat (listof (listof Sym)))
(define-struct game (tubesize maxcolours tubes))




;;**************************
;; Joshua Kobes (21025102)
;; CS135 Fall 2022
;; A10 Q1 (a)
;;**************************

;;(check-colour? occu cnum los) consumes two natrual number
;;the tubesize and the maxcolours and a list of symbols
;;then using generative function it evaluates wheter the symbol
;;in the list occured tubesize times with each colours

;Examples
(check-expect (check-colour? 5 2 '(yellow yellow yellow red red red red red yellow yellow)) true)

(check-expect (check-colour? 2 2 '(red red blue blue)) true)

(check-expect (check-colour? 2 2 '(red blue blue)) false)

;;check-colour?: Nat Nat (listof Sym) -> Bool
(define (check-colour? occu cnum los)
  (cond [(and (empty? los)
              (equal? 0 cnum)) true]
        [(equal? occu (length (filter (lambda (x) (equal? (first los) x)) los)))
         (check-colour? occu (- cnum 1) (filter (lambda (x) (not (equal? (first los) x))) los))]
        [else false]))



;;**************************
;; Joshua Kobes (21025102)
;; CS135 Fall 2022
;; A10 Q1 (b)
;;**************************
;;(valid-game? gm) consumes a game and evaluates whether
;;the consumed game is valid or not, by checking whether
;;all tubes have at most consumed tubesize
;;also if there are at most consumed maxcolours of different
;;symbols in the list and lastly it checks it there is tubesize
;;occurrences of each different symbols

;;For examples
(check-expect (valid-game? (make-game 5 2 '(()()()()()))) true)
(check-expect (valid-game? (make-game 2 2 '(()))) true)

;;valid-game?: Game -> Bool
(define (valid-game? gm)
  (local [;;(tubesizecheck gm) consuems a gm and checks whether
          ;;the symbols in each list appears atmost tubesize and return in list
          ;;tubesize: Game -> (listof (listof Sym))
          (define (tubesizecheck gm)
            (foldr (lambda (x y)
                     (cond [(>= (game-tubesize gm) (length x)) (cons x y)]
                                         [else false]))
                   '() (game-tubes gm)))
          
          ;;(appendtubes gm) consumes a gm and flattens the tubes in a list
          ;;appendtubes: Game -> (listof Sym)
          (define (appendtubes gm)
            (foldr (lambda (x y) (append x y)) '() (game-tubes gm)))
          
          ;;(maxcolourcheck c1 c2 los) consumes two nat that represent maxcolours
          ;;and a listof symbols then check how many colours are present in the list
          ;;maxcolourcheck: Nat Nat (listof (listof Sym)) -> Bool
          (define (maxcolourcheck c1 c2 los)
            (cond [(< c1 0) false]
                  [(and (empty? los)
                        (<= c1 c2)) true]
                  [(list? los)
                   (maxcolourcheck (- c1 1) c2
                                   (filter
                                    (lambda (x) (not (equal? (first los) x))) los))]
                  [else false]))
          
          ;;(coloursingame c1 c2 los) consuems two nat and a listof symbols then produces the
          ;;updated value for c1
          ;;coloursingame: Nat Nat (listof Sym) -> (anyof Nat or Bool) 
          (define (coloursingame c1 c2 los)
            (cond [(< c1 0) false]
                  [(and (empty? los)
                        (<= c1 c2)) c1]
                  [(list? los)
                   (coloursingame (- c1 1) c2
                                  (filter (lambda (x) (not (equal? (first los) x))) los))]
                  [else false]))

          ;;(check-colour? occu cnum los) consumes two natrual number
          ;;the tubesize and the maxcolours and a list of symbols
          ;;then using generative function it evaluates wheter the symbol
          ;;in the list occured tubesize times with each colours
          ;;check-colour?: Nat Nat (listof Sym) -> Bool
          (define (check-colour? occu cnum los)
            (cond [(and (empty? los)
                        (equal? 0 cnum)) true]
                  [(equal? occu (length (filter (lambda (x) (equal? (first los) x)) los)))
                   (check-colour? occu (- cnum 1)
                                  (filter (lambda (x) (not (equal? (first los) x))) los))]
                  [else false]))]
    (and (list? (tubesizecheck gm))
         (maxcolourcheck (game-maxcolours gm)
                         (game-maxcolours gm)
                         (game-tubes gm))
         (check-colour? (game-tubesize gm)
                        (- (game-maxcolours gm)
                           (coloursingame (game-maxcolours gm)
                                          (game-maxcolours gm)
                                          (game-tubes gm)))
                        (game-tubes gm))))) 
;;**************************
;; Joshua Kobes (21025102)
;; CS135 Fall 2022
;; A10 Q1 (c)
;;**************************
;;(remove-completed gm): consumes a game then
;;removes the tube that is filled with the same colour
;;then returns the remaining tubes, if the colours in the remaining
;;tubes have decrease it will update that info aswell.
;;after remove-complete will produce a game with updated maxcolours and
;;the remaining tubes

;;For examples
(check-expect (remove-completed (make-game 3 3 (list (list 'yellow 'red 'yellow)
                                                     (list 'blue 'blue 'blue)
                                                     (list 'red 'yellow 'red))))
              (make-game 3 2 (list (list 'yellow 'red 'yellow)
                                   (list 'red 'yellow 'red))))

(check-expect (remove-completed (make-game 3 3 (list (list 'yellow 'red 'yellow)
                                                     (list 'yellow 'blue 'blue)
                                                     (list 'red 'blue 'red))))
              (make-game 3 3 (list (list 'yellow 'red 'yellow)
                                   (list 'yellow 'blue 'blue)
                                   (list 'red 'blue 'red))))

(check-expect (remove-completed (make-game 3 3  (list (list 'yellow 'yellow 'yellow)
                                                      (list 'blue 'blue 'blue)
                                                      (list 'red 'red 'red))))
              (make-game 3 0 empty))

(check-expect (remove-completed (make-game 3 3 (list (list 'yellow 'yellow 'yellow)
                                                     (list 'red 'blue 'blue)
                                                     (list 'red 'blue 'red))))
              (make-game 3 2 (list (list 'red 'blue 'blue)
                                   (list 'red 'blue 'red))))
(check-expect (remove-completed (make-game 3 3 '((red red red)
                                                 (blue blue blue)
                                                 (yellow yellow yellow))))
              (make-game 3 0 empty))

;;remove-completed: Game -> Game
(define (remove-completed gm)
  (local [;;(remove-completed-pls gm) consumes a game and removes all the completed tubes
          ;; remove-completed-pls: Game -> Game
          (define (remove-completed-pls gm)
            (foldr (lambda (x r) (cond [(empty? x) (cons x r)]
                                       [(and (empty? (filter (lambda (y) (not (equal? (first x) y))) x))
                                             (equal? (length x) (game-tubesize gm))) r]
                                       [else (cons x r)])) '() (game-tubes gm)))
          
          ;;(appendtubes nelos) consumes listof listof symbols then produces a flattened list
          ;;appendtubes: (listof (listof Sym)) -> (listof Sym)
          (define (appendtubes nelos)
            (foldr (lambda (x y) (append x y)) '() nelos))

          ;;(coloursingame c1 c2 los) consuems two nat and a listof symbols then produces the
          ;;updated value for c1
          ;;coloursingame: Nat Nat (listof Sym) -> (anyof Nat or Bool)
          (define (coloursingame c1 c2 los)
            (cond [(< c1 0) false]
                  [(and (empty? los)
                        (<= c1 c2)) c1]
                  [(list? los)
                   (coloursingame (- c1 1) c2
                                  (filter (lambda (x) (not (equal? (first los) x))) los))]
                  [else false]))]
    
    (make-game (game-tubesize gm)
               (- (game-maxcolours gm)
                  (coloursingame (game-maxcolours gm)
                                 (game-maxcolours gm)
                                 (appendtubes (remove-completed-pls gm))))
               (remove-completed-pls gm))))
;;**************************
;; Joshua Kobes (21025102)
;; CS135 Fall 2022
;; A10 Q1 (d)
;;**************************

;;(finished-game? gm) will consumed a game
;;then it will check if all the tubes are empty or filled with
;;same colour of balls then produce a true/false
;;whether the game is finished or not.

;; For examples
(check-expect (finished-game? (make-game 2 2 '())) true)
(check-expect (finished-game? (make-game 2 2 '(() () () ()))) true)
(check-expect (finished-game? (make-game 2 2 '((blue blue) (red) (red)))) false)

;;finished-game?: Game -> Bool
(define (finished-game? gm)
  (cond [(list? (game-maxcolours (remove-completed gm))) false]
        [(equal? 0 (game-maxcolours (remove-completed gm))) true]
        [else false]))

;;**************************
;; Joshua Kobes (21025102)
;; CS135 Fall 2022
;; A10 Q1 (e)
;;**************************
;;(num-blocks llos) consumes a list of list of symbols
;;then it will count every consecutive sequence of symbols as
;;one block then produce the amount of blocks in the
;;consumed list of list of symbols

;;For examples
(check-expect (num-blocks (list empty '(a b a) '(a a))) 4)
              
(check-expect (num-blocks (list '(a a a a b) '(a b a))) 5)

;;num-blocks: (listof (listof Sym)) -> Nat
(define (num-blocks llos)
  (foldr (lambda (x r) (cond [(empty? x) (+ 0 r)]
                             [(list? x) (+ (length (count-blocks x)) r)])) 0 llos))


;;(count-blocks): make every consecutive sequence in the list
;;as singular.

;;helper example
(check-expect (count-blocks '(a a b b a)) '(a b a))

;;count-blocks: (listof Sym) -> (listof Sym)
(define (count-blocks los)
  (foldr (lambda (x r) (cond [(empty? r) (cons x r)]
                             [(equal? (first r) x) r]
                             [else (cons x r)])) '() los))

;;**************************
;; Joshua Kobes (21025102)
;; CS135 Fall 2022
;; A10 Q1 (f)
;;**************************
;;(equiv-game? gm1 gm2) consumes two games
;; then it will compare one on another to evaluate
;;whether two games are identical

;;For example
(check-expect (equiv-game? (make-game 3 3 (list (list 'yellow 'red 'yellow)
                                                     (list 'blue 'blue 'blue)
                                                     (list 'red 'yellow 'red)))
                           (make-game 3 3 (list (list 'yellow 'red 'yellow)
                                                     (list 'blue 'blue 'blue)
                                                     (list 'red 'yellow 'red)))) true)
;;equiv-game? : Game Game -> Bool 
(define (equiv-game? gm1 gm2)
  (local [;;(maxcolourscheck gm1 gm2) checks whether maxcolours value on both games are equivalent
          ;;maxcolourscheck: Game Game -> Bool
          (define (maxcolourscheck gm1 gm2)
            (equal? (game-maxcolours gm1) (game-maxcolours gm2)))

          ;;(tubesizecheck gm1 gm2) compares the tubesize value from two consumed games produce true
          ;;when equivalent else false
          ;;tubesizecheck: Game Game -> Bool
          (define (tubesizecheck gm1 gm2)
            (equal? (game-tubesize gm1) (game-tubesize gm2)))

          ;;(tubenumcheck gm1 gm2) compares the tubes from two consumed games produce true
          ;;when equivalent else false
          ;;tubenumcheck Game Game -> Bool
          (define (tubenumcheck gm1 gm2)
            (equal? (length (game-tubes gm1)) (length (game-tubes gm2))))

          ;;(identicaltubes gm1lst gm2lst) consumes two list of list of symbols then
          ;;identifies if all the lists are identical. 
          ;;identicaltubes: (listof (listof Sym)) (listof (listof Sym)) -> Bool
          (define (identicaltubes gm1lst gm2lst)
            (cond [(and (empty? gm1lst)
                        (empty? gm2lst)) true]
                  [(member? (first gm1lst) gm2lst)
                   (identicaltubes (remove (first gm1lst) gm1lst)(remove (first gm1lst) gm2lst))]
                  [else false]))]
    (and (maxcolourscheck gm1 gm2)
         (tubesizecheck gm1 gm2)
         (tubenumcheck gm1 gm2)
         (identicaltubes (game-tubes gm1) (game-tubes gm2)))))


;;(check-expect (equiv-game? (make-game 2 2 '((blue blue) (red blue) (red)))
;;                           (make-game 4 2 '(() () (red blue blue) (red)))) false)


;;**************************
;; Joshua Kobes (21025102)
;; CS135 Fall 2022
;; A10 Q1 (g)
;;**************************
;; (all-equiv? logm1 logm2) consumes two list of Games
;;then compare one on another to see whether the two consumed
;;list of games are equivalent

;;Example
(check-expect (all-equiv? (list (make-game 3 3 (list (list 'blue 'red 'yellow)
                                                     (list 'blue 'yellow 'blue)
                                                     (list 'red 'yellow 'red)))
                                (make-game 3 3 (list (list 'blue 'red 'blue)
                                                     (list 'yellow 'blue 'yellow)
                                                     (list 'red 'yellow 'red))))
                          (list (make-game 3 3 (list (list 'blue 'red 'yellow)
                                                     (list 'blue 'yellow 'blue)
                                                     (list 'red 'yellow 'red)))
                                (make-game 3 3 (list (list 'blue 'red 'blue)
                                                     (list 'yellow 'blue 'yellow)
                                                     (list 'red 'yellow 'red))))) true)


(check-expect (all-equiv? (list (make-game 3 3 (list (list 'blue 'red 'yellow)
                                                     (list 'blue 'yellow 'blue)
                                                     (list 'red 'yellow 'red)))
                                (make-game 3 3 (list (list 'blue 'red 'blue)
                                                     (list 'yellow 'blue 'yellow)
                                                     (list 'red 'yellow 'red))))
                          (list (make-game 3 3 (list (list 'blue 'yellow 'blue)
                                                     (list 'blue 'red 'yellow)
                                                     (list 'red 'yellow 'red)))
                                (make-game 3 3 (list (list 'red 'yellow 'red)
                                                     (list 'blue 'red 'blue)
                                                     (list 'yellow 'blue 'yellow))))) true)



;;all-equiv?: (listof Game) (listof Game) -> Bool

(define (all-equiv? logm1 logm2)
  (cond [(and (empty? logm1)
              (empty? logm2)) true]
        [(empty? logm1) false]
        [(empty? logm2) false]
        [(equiv-game-exist? (first logm1) logm2)
         (all-equiv? (my-remove (first logm1) logm1) (my-remove (first logm1) logm2))]
        [else false]))

;;(equiv-game-exist? gm1 logm2) using recursion it checks whether
;;gm1 exist in logm2 then return true it so, else false

;;helper Example
(check-expect (equiv-game-exist?
               (make-game 3 2 '((red red)
                                (blue blue blue)
                                (red blue red))) (list (make-game 3 2 '((red red)
                                                                        (blue blue blue)
                                                                        (red blue red)))
                                                       (make-game 3 2 '((red red)
                                                                        (red blue blue)
                                                                        (blue blue red))))) true)
;;equiv-game-exist?: Game (listof Game) -> Bool
(define (equiv-game-exist? gm1 logm2)
  (cond [(empty? logm2) false]
        [(equiv-game? gm1 (first logm2)) true]
        [else (equiv-game-exist? gm1 (rest logm2))]))

;;(my-remove gm1 logm) consumes a game and remove that exact game from the
;;consumed list of game

;;helper example
(check-expect (my-remove
               (make-game 3 2 '((red red)
                                (blue blue blue)
                                (red blue red))) (list (make-game 3 2 '((red red)
                                                                        (blue blue blue)
                                                                        (red blue red)))
                                                       (make-game 3 2 '((red red)
                                                                        (red blue blue)
                                                                        (blue blue red)))))
              (list (make-game 3 2 '((red red)
                                     (red blue blue)
                                     (blue blue red)))))
;;my-remove: Game (listof Game) -> (listof Game)
(define (my-remove gm1 logm)
  (foldr (lambda (x r) (cond [(not (equiv-game? gm1 x)) (cons x r)]
                             [else r])) '() logm))















