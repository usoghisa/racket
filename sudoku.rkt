;;load to dr raket
#lang racket
(require racket/set)
(define (parts matrix row column number)
  (take (drop (car(take (drop matrix row)1))column)number))

(define mtrx '(
              (1 2 3 4 5 6 7 8 9) 
              (2 0 3 4 5 0 7 8 9)
              (3 2 3 0 5 6 7 8 9)
              (4 2 3 4 5 6 7 8 9)
              (5 2 3 4 5 6 7 8 9)
              (6 2 3 4 5 6 7 8 9)
              (7 2 3 0 0 6 7 8 9)
              (8 2 3 0 5 6 7 8 9)
              (9 2 3 0 5 6 7 8 9)))

(flatten (list (parts mtrx 0 6 3)
               (parts mtrx 1 6 3)
               (parts mtrx 2 6 3)))

(define block(flatten(list 
                    (parts mtrx 0 0 3)
                    (parts mtrx 1 0 3)
                    (parts mtrx 2 0 3))))

;return single element parameterised
(define (pivot mtrx r c n )
  (car(parts mtrx r c n)))

;return any block parameterised
(define(anyBlock mtrx r1 c1 n1 r2 c2 n2 r3 c3 n3)
               (flatten(list
                    (parts mtrx r1 c1 n1)
                    (parts mtrx r2 c2 n2)
                    (parts mtrx r3 c3 n3))))

;return any anyColum parameterised
(define(anyColum mtrx 
                     r1 c1 n1 r2 c2 n2 r3 c3 n3
                     r4 c4 n4 r5 c5 n5 r6 c6 n6
                     r7 c7 n7 r8 c8 n8 r9 c9 n9)
               (flatten(list
                    (parts mtrx r1 c1 n1)
                    (parts mtrx r2 c2 n2)
                    (parts mtrx r3 c3 n3)
                    (parts mtrx r4 c4 n4)
                    (parts mtrx r5 c5 n5)
                    (parts mtrx r6 c6 n6)
                    (parts mtrx r7 c7 n7)
                    (parts mtrx r8 c8 n8)
                    (parts mtrx r9 c9 n9))))

(print "call pivot rtn 2 " ) (pivot mtrx 0 1 1 )
(print "rtn #t #f is num  ret f " ) (number? (pivot mtrx 0 0 1 ))
(print "is 0  " )(= 0 (pivot mtrx 1 0 1 ))

(define (row a b c mtrx )
  (anyBlock mtrx 
          a 0 3 
          b 3 3 
          c 6 3 ))

(row 0 0 0 mtrx)

(range 10)

(print "row " ) (row 0 0 0 mtrx )
(print "row " ) (row 1 1 1 mtrx)
(define col1 (anyColum mtrx 
          0 0 1 
          1 0 1 
          2 0 1 
          3 0 1 
          4 0 1 
          5 0 1
          6 0 1 
          7 0 1 
          8 0 1))
#|MULTYLINE COMMENT|#
(print "col1 " )col1
(print "row " )(row 0 0 0 mtrx) 
(print "singlr ELE r2 c8 n1 ")(car(parts mtrx 2 8 1))
(print "rtn set 6 4 5 r c n ")(car(parts mtrx 6 4 5)) ;r c n

;(map (lambda (x) (= 0 x)) row1)
(define (changeZero row1)
(map (lambda (n)
        (cond ((= 0 n) '(set 1 2 3 4 5 6 7 8 9))
         ((> 0 n))
         (else n)))row1))

;; change 0 to a set
(define (changeToSet row)
(map(lambda(n)
        (cond ((= n 0) (set 1 2 3 4 5 6 7 8 9))
         ((> n 0)(set n) ))
    )row))

(define mtrxSet(list 
 (changeToSet (row 0 0 8 mtrx))
 (changeToSet (row 1 1 8 mtrx))
 (changeToSet (row 2 2 8 mtrx))
 (changeToSet (row 3 3 8 mtrx))
 (changeToSet (row 4 4 8 mtrx))
 (changeToSet (row 5 5 8 mtrx))
 (changeToSet (row 6 6 8 mtrx))
 (changeToSet (row 7 7 8 mtrx))
 (changeToSet (row 8 8 8 mtrx))
))


(display  "matrix with n substitute with set is mtrxSet \n" ) 
mtrxSet

(print "call pivot rtn set " ) (pivot mtrxSet 1 1 1 )
(print "remove n 9 from set " )(set-remove (pivot mtrxSet 1 1 1 ) 9 )
(print "remove n 4 from set2 " )(set-remove (set 1 3 4 ) (set-first (set 4)) )
(print "remove (set 2) from set " )(set-remove (pivot mtrxSet 1 1 1 ) (set-first(pivot mtrxSet 1 0 1 )) )

;(removePivFromSet mtrxSet (row 0 0 8 mtrxSet))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (replace lst x y)
  (cond
    [(empty? lst) lst]
    [(list? (first lst)) (cons (replace (first lst) x y) (replace (rest lst) x y))]
    [(equal? (first lst) x) (cons y (replace (rest lst) x y))]
    [else (cons (first lst) (replace (rest lst) x y))]))

(define (extract matrix row column number)
  (take(drop(car(take(drop matrix row)1))column)number))

(define (manipulate matrix row column number changeSet changeSetTo )
  (replace (extract matrix row column number) changeSet changeSetTo))

(define(trans matrix row column number changeSet changeSetTo)
  (let ([inner (take matrix row)]
        [remainder (drop matrix row)])  
    (cons (car inner) 
          (list 
           (flatten
            (list
             (take(first(take remainder 1))column)
             (manipulate matrix row column number changeSet changeSetTo)
             (drop(first(take remainder 1))(+ column number)) 
             ))
           (first(take(drop remainder row)(-(length matrix) (+ row 1))))
           (first(rest(take(drop remainder row)(-(length matrix) (+ row 1)))))
           (first(rest(rest(take(drop remainder row)(-(length matrix) (+ row 1))))))
           (first(rest(rest(rest(take(drop remainder row)(-(length matrix) (+ row 1)))))))
           (first(rest(rest(rest(rest(take(drop remainder row)(-(length matrix) (+ row 1)))))))) 
           (first(rest(rest(rest(rest(rest(take(drop remainder row)(-(length matrix) (+ row 1)))))))))
           (first(rest(rest(rest(rest(rest(rest(take(drop remainder row)(-(length matrix) (+ row 1))))))))))
           )
          )
    )
  )
;(define (trans matrix row column number changeSet changeSetTo)
(print "remove (set 2) from set r1 c1 " )
(trans mtrxSet 1 1 1 (pivot mtrxSet 1 1 1 )(set-remove (pivot mtrxSet 1 1 1 )(set-first(pivot mtrxSet 1 0 1 ))))

(print "remove (set 2) from set row1 col5 col1" )
(trans
 (trans mtrxSet 1 5 1 (pivot mtrxSet 1 5 1)(set-remove(pivot mtrxSet 1 5 1)(set-first(pivot mtrxSet 1 0 1 ))))
 1 1 1 (pivot mtrxSet 1 1 1 )(set-remove (pivot mtrxSet 1 1 1 )(set-first(pivot mtrxSet 1 0 1 ))))

(print "row 1, 2 output original mtrxSet" )(row 0 0 0 mtrxSet)(row 1 1 1 mtrxSet)
;; use set ! will update mtrxSet 
;(set! mtrxSet(trans mtrxSet 1 1 1 (pivot mtrxSet 1 1 1 ) (set-remove (pivot mtrxSet 1 1 1 )(set-first(pivot mtrxSet 1 0 1 )) ) )) mtrxSet

(print "rtn #t if 1 ele set")(equal?(set-rest(pivot mtrxSet 1 1 1))(set))

(define(findSigleTon row )
(map(lambda(ele) ;(equal?(set-rest n)(set))
        (cond
         ((equal? #t (equal?(set-rest ele)(set))) ele)
    ))row))

(define(findLongSet row )
(map(lambda(ele) 
        (cond
         ((equal? #f (equal?(set-rest ele)(set)))ele)
    ))row))
(define (singlTonLs r) 
  (findSigleTon (row r r r mtrxSet)))

(define (LongSetLs r) 
  (findLongSet (row r r r mtrxSet)))
(print "all Single ton of row1 ")(singlTonLs 1)
(print "all LongSetLs of row1 ")(LongSetLs 1) 



