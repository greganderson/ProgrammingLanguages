#lang plai-typed


(define-type Tree
  [leaf (val : number)]
  [node (val : number)
        (left : Tree)
        (right : Tree)])

;; Part 1 - Sum
(define (sum [t : Tree]) : number
  (type-case Tree t
    [leaf (v) v]
    [node (v l r) (+ v (+ (sum l) (sum r)))]))

(test (sum (leaf 1)) 1)
(test (sum (node 5 (leaf 5)(leaf 5))) 15)
(test (sum (node 5 (leaf 6)(leaf 7))) 18)


;; Part 2 - Negate
(define (negate [t : Tree]) : Tree
  (type-case Tree t
    [leaf (v) (leaf (* v -1))]
    [node (v l r) (node (* v -1) (negate l)(negate r))]))

(test (negate (node 5 (leaf 6)(leaf 7))) (node -5 (leaf -6)(leaf -7)))
(test (negate (node 5 (leaf -5)(leaf 5))) (node -5 (leaf 5)(leaf -5)))


;; Part 3 - Contains?
(define (contains? [t : Tree][n : number]) : boolean
  (type-case Tree t
    [leaf (v) (= v n)]
    [node (v l r) (or (contains? l n)(contains? r n))]))

(test (contains? (node 5 (leaf 6)(leaf 7)) 6) #t)
(test (contains? (node 5 (leaf 6)(leaf 7)) 3) #f)


;; Part 4 - Big Leaves?
(define (big-leaves? [t : Tree]) : boolean
  (bigger-leaves? t 0))

(define (bigger-leaves? [t : Tree] [n : number]) : boolean
  (type-case Tree t
    [leaf (v) (> v n)]
    [node (v l r) (and (bigger-leaves? l (+ n v)) (bigger-leaves? r (+ n v)))]))

(test (big-leaves? (node 5 (leaf 6) (leaf 7))) #t)
(test (big-leaves? (leaf -1)) #f)
(test (big-leaves? (leaf 1)) #t)
(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) #f)
