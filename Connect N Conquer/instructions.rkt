#lang racket
(provide (all-defined-out))

(define line-1 "1. Click in between two dots to connect them.")
(define line-2 "2. Your aim should be to complete a four sided box.")
(define line-3 "3. The player that completes a box gets another turn.")
(define line-4 "4. The game ends when all the boxes have been completed.")
(define line-5 "5. The more boxes a player has occupied, the better his rank will be.")
(define line-6 "6. After typing your name in the text-box, hit enter before moving ahead.")

(define title (vector (cons (cons 0 0) (cons 0 1)) (cons (cons 0 1) (cons 0 2)) (cons (cons 0 0) (cons 1 0))
                                                    (cons (cons 0 2) (cons 1 2)) (cons (cons 1 2) (cons 2 2)) (cons (cons 1 0) (cons 2 0))

                                                    (cons (cons 3 0) (cons 3 1)) (cons (cons 3 1) (cons 3 2)) (cons (cons 3 2) (cons 4 2)) (cons (cons 4 0) (cons 3 0))
                                                    (cons (cons 4 2) (cons 5 2)) (cons (cons 5 2) (cons 5 1)) (cons (cons 5 1) (cons 5 0)) (cons (cons 5 0) (cons 4 0)) 

                                                    
                                                    (cons (cons 6 2) (cons 6 1)) (cons (cons 6 1) (cons 6 0)) (cons (cons 6 0) (cons 7 1))
                                                    (cons (cons 7 1) (cons 8 2)) (cons (cons 8 2) (cons 8 1)) (cons (cons 8 1) (cons 8 0))

                                                    (cons (cons 9 2) (cons 9 1)) (cons (cons 9 1) (cons 9 0)) (cons (cons 9 0) (cons 10 1))
                                                    (cons (cons 10 1) (cons 11 2)) (cons (cons 11 2) (cons 11 1)) (cons (cons 11 1) (cons 11 0))

                                                    (cons (cons 14 0) (cons 13 0)) (cons (cons 13 0) (cons 12 0)) (cons (cons 12 0) (cons 12 1)) (cons (cons 12 1) (cons 12 2))
                                                    (cons (cons 12 2) (cons 13 2)) (cons (cons 13 2) (cons 14 2)) (cons (cons 12 1) (cons 13 1))

                                                    (cons (cons 17 0) (cons 16 0)) (cons (cons 16 0) (cons 15 0)) (cons (cons 15 0) (cons 15 1))
                                                    (cons (cons 15 1) (cons 15 2)) (cons (cons 15 2) (cons 16 2)) (cons (cons 16 2) (cons 17 2))

                                                    (cons (cons 18 0) (cons 19 0)) (cons (cons 19 0) (cons 20 0)) (cons (cons 19 0) (cons 19 1)) (cons (cons 19 1) (cons 19 2))

                                                    (cons (cons 23 2) (cons 23 1)) (cons (cons 23 1) (cons 23 0)) (cons (cons 23 0) (cons 24 1))
                                                    (cons (cons 24 1) (cons 25 2)) (cons (cons 25 2) (cons 25 1)) (cons (cons 25 1) (cons 25 0))

                                                    (cons (cons 30 0) (cons 29 0)) (cons (cons 29 0) (cons 28 0)) (cons (cons 28 0) (cons 28 1))
                                                    (cons (cons 28 1) (cons 28 2)) (cons (cons 28 2) (cons 29 2)) (cons (cons 29 2) (cons 30 2))

                                                    (cons (cons 31 0) (cons 31 1)) (cons (cons 31 1) (cons 31 2)) (cons (cons 31 2) (cons 32 2)) (cons (cons 32 2) (cons 33 2))
                                                    (cons (cons 33 2) (cons 33 1)) (cons (cons 33 1) (cons 33 0)) (cons (cons 33 0) (cons 32 0)) (cons (cons 32 0) (cons 31 0))

                                                    (cons (cons 34 2) (cons 34 1)) (cons (cons 34 1) (cons 34 0)) (cons (cons 34 0) (cons 35 1))
                                                    (cons (cons 35 1) (cons 36 2)) (cons (cons 36 2) (cons 36 1)) (cons (cons 36 1) (cons 36 0))

                                                    (cons (cons 37 0) (cons 37 1)) (cons (cons 37 1) (cons 37 2)) (cons (cons 37 2) (cons 38 2))
                                                    (cons (cons 38 2) (cons 39 2)) (cons (cons 39 2) (cons 39 1)) (cons (cons 39 1) (cons 39 0))
                                                    (cons (cons 39 0) (cons 38 0)) (cons (cons 38 0) (cons 37 0)) (cons (cons 38 1) (cons 39 2))

                                                    (cons (cons 40 0) (cons 40 1)) (cons (cons 40 1) (cons 40 2)) (cons (cons 40 2) (cons 41 2))
                                                    (cons (cons 41 2) (cons 42 2)) (cons (cons 42 2) (cons 42 1)) (cons (cons 42 1) (cons 42 0))

                                                    (cons (cons 45 0) (cons 44 0)) (cons (cons 44 0) (cons 43 0)) (cons (cons 43 0) (cons 43 1)) (cons (cons 43 1) (cons 43 2))
                                                    (cons (cons 43 2) (cons 44 2)) (cons (cons 44 2) (cons 45 2)) (cons (cons 43 1) (cons 44 1))

                                                    (cons (cons 46 2) (cons 46 1)) (cons (cons 46 1) (cons 46 0)) (cons (cons 46 0) (cons 47 0)) (cons (cons 47 0) (cons 48 0))
                                                    (cons (cons 48 0) (cons 48 1)) (cons (cons 48 1) (cons 47 1)) (cons (cons 47 1) (cons 46 1)) (cons (cons 47 1) (cons 48 2))))

(define title-2 (vector (cons (cons 0 0) (cons 0 1)) (cons (cons 0 1) (cons 0 2))
                                                    (cons (cons 0 2) (cons 1 2)) (cons (cons 1 2) (cons 2 2))

                                                    (cons (cons 3 0) (cons 4 0)) (cons (cons 4 0) (cons 5 0)) (cons (cons 4 0) (cons 4 1))
                                                    (cons (cons 4 1) (cons 4 2)) (cons (cons 3 2) (cons 4 2)) (cons (cons 4 2) (cons 5 2))

                                                    (cons (cons 6 2) (cons 6 1)) (cons (cons 6 1) (cons 6 0)) (cons (cons 6 0) (cons 7 1))
                                                    (cons (cons 7 1) (cons 8 2)) (cons (cons 8 2) (cons 8 1)) (cons (cons 8 1) (cons 8 0))

                                                    (cons (cons 9 0) (cons 9 1)) (cons (cons 9 1) (cons 9 2))
                                                    (cons (cons 9 1) (cons 11 0)) (cons (cons 9 1) (cons 11 2))

                                                    (cons (cons 14 2) (cons 14 1)) (cons (cons 14 1) (cons 14 0)) (cons (cons 14 0) (cons 15 1))
                                                    (cons (cons 15 1) (cons 16 2)) (cons (cons 16 2) (cons 16 1)) (cons (cons 16 1) (cons 16 0))

                                                    (cons (cons 19 0) (cons 19 1)) (cons (cons 19 1) (cons 19 2)) (cons (cons 19 2) (cons 20 1))
                                                    (cons (cons 20 1) (cons 21 2)) (cons (cons 21 2) (cons 21 1)) (cons (cons 21 1) (cons 21 0))

                                                    (cons (cons 22 0) (cons 23 0)) (cons (cons 23 0) (cons 24 0)) (cons (cons 23 0) (cons 23 1))
                                                    (cons (cons 23 1) (cons 23 2)) (cons (cons 22 2) (cons 23 2)) (cons (cons 23 2) (cons 24 2))

                                                    (cons (cons 25 2) (cons 25 1)) (cons (cons 25 1) (cons 25 0)) (cons (cons 25 0) (cons 26 1))
                                                    (cons (cons 26 1) (cons 27 2)) (cons (cons 27 2) (cons 27 1)) (cons (cons 27 1) (cons 27 0))))