(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (
                 (member? a (cdr lat)))))))


(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                 (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l))
                 (firsts (cdr l)))))))


(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
        (cons old
           (cons new (cdr lat))))
      (else (cons (car lat)
                (insertR new old (cdr lat)))))))

(insertr 'c 'b '(a b d))


(define insertl
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
        (cons new lat))
      (else (cons (car lat)
                (insertL new old (cdr lat)))))))


(insertl 'c 'b '(a b d))
(a c b d)


(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
           (cons new (cdr lat)))
      (else (cons (car lat)
                (subst new old (cdr lat)))))))
subst


(subst 'c 'b '(a b d))
(a c d)



(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((or (eq? (car lat) o1) (eq? (car lat) o2))
           (cons new (cdr lat)))
      (else (cons (car lat)
                (subst2 new o1 o2 (cdr lat)))))))

(subst2 'c 'b 'z '(a b c d))
(a c c d)





(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat)
                 (multirember a (cdr lat)))))))

(multirember 'a '(a b c d a b c d a))



(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
        (cons old
           (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat)
                (multiinsertR new old (cdr lat)))))))


(multiinsertr 'z 'b '(a b c d a b c d a b c))
(a b z c d a b z c d a b z c)



(define multisubst
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((or (eq? (car lat) o1) (eq? (car lat) o2))
           (cons new (multisubst new o1 o2 (cdr lat))))
      (else (cons (car lat)
                (multisubst new o1 o2 (cdr lat)))))))


(multisubst 'z 'a 'b '(a b c d a b c d a))
(z z c d z z c d z)

(define add1
  (lambda (n)
    (+ 1 n)))

(add1 6)


(define sub1
  (lambda (n)
    (- n 1)))

(sub1 98)


(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))
addtup

(addtup '(1 2 3 4))



(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
        (cons (+ (car tup1) (car tup2))
          (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(1 2 3) '(5 6 7 8 9))
(6 8 10 8 9)


(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
pick


(pick 3 '(a b c d e r))


(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
rempick



(rempick 3 '(a b c d e r))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))


(no-nums '(a 1 b 2))
(a b)

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(all-nums '(a 1 b 2))
(1 2)

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))


(occur 'a '(a b a b a))
3


(define one?
  (lambda (n)
    (eq? n 1)))


(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? (car l) a) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))


(rember* 'a '(a b (a b) ((a b) a b) b ))
(b (b) ((b) b) b)
#t


(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
         (cond
           ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
           (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))


(insertr* 'z 'a '(a (a b (a b) a b)))
(a z (a z b (a z b) a z b))
