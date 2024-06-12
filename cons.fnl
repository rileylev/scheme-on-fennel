(local debug-assert assert)

(fn number?    [n] (= (type n) :number))
(fn boolean?   [x] (= (type x) :boolean))
(fn procedure? [f] (= (type f) :function))
;; Lua strings are immutable and interned, like scheme's symbols
;; Scheme's strings are mutable arrays.
(fn symbol?    [s] (= (type s) :string))
(fn zero? [n] (= n 0))

(fn nil? [x] (= x nil))
;; TODO: Should I match up nil with undefined?

(local eq? rawequal)
(fn equal? [x y] (= x y))
(fn exact? [n] false)
(fn inexact? [n] (number? n))
;; TODO: how should exactness fit with Lua's types?
(fn integer? [n] (= (math.type n) :integer))
(fn exact-integer? [n]
  (and (exact? n) (integer? n)))
(local inf math.huge)
(local ninf (- inf))
(fn infinite? [x]
  (or (= x inf) (= x ninf)))
(fn finite? [x]
  (and (< ninf x) (< x inf)))
;; > One reason is that it is not always possible to compute eqv? of two
;; > numbers in constant time, whereas eq? implemented as pointer
;; > comparison will always finish in constant time.
;; --r7rs p32
(fn eqv? [x y]
  (if (exact? x)
      (and (exact? y) (equal? x y))
      (eq? x y)))


;;; Tension between scheme and fennel: nil vs null
;;; The semantics are too different:
;;;     (f) = (f nil) ≠ (f '())
;;;
;;; Then there's two potential kinds of lists:
;;; lua lists which are nil terminated
;;; scheme lists which are null terminated <--
;;; I am only doing scheme lists
(local null {})
(fn null? [x]
  "Returns #t if obj is the empty list, otherwise returns #f (r7rs 42)"
  (= x null))

(local cars (setmetatable {} {:__mode "k"}))
(local cdrs (setmetatable {} {:__mode "k"}))

(fn access [tbl key val force-set?]
  "Read or write the field ~key~ in ~tbl~.
Calling access with only two arguments reads:
(access tbl key) ==> (. table key)

Calling access with an additional (non-nil) argument sets:
(access tbl key new-value) ==> (tset tbl key)

To distinguish setting nil from reading, pass true for force-set?:

(access tbl key maybe-nil true) => (tset tbl key)

which allows unsetting:
(access tbl key nil true) => (tset tbl key nil) "
  (if (or val force-set?)
      (tset tbl key val)
      (. tbl key)))
(fn zshift [n]
  "Convert from Lua-style indexing starting from 1 to Javascript-style
 indexing starting from 0. Non-numbers are left alone"
  (if (number? n)
      (+ n 1)
      n))
(fn zaccess [tbl key val force-set?]
  "Adapt ~access~ for javascript-style indexing a la zshift. When ~key~
is not a number, ~zaccess~ is equivalent to ~access~. When ~key~ is a
number, add 1.

(zaccess tbl N ...) => (access tbl (+ 1 N) ...)

(zaccess tbl x ...) => (access tbl x ...)"
  (access tbl (zshift key) val force-set?))

(local pair-mt {})
(fn pair? [x] (= (getmetatable x) pair-mt))

(fn car [cell val force-set?]
  "Returns the contents of the car field of pair . Note that it
is an error to take the car of the empty list. (r7rs 41)

'Undocumented' extension: val and force-set? can be used to set the car"
  (debug-assert (pair? cell))
  (access cars cell val force-set?))
(fn cdr [cell val force-set?]
  "Returns the contents of the cdr field of pair . Note that it
is an error to take the cdr of the empty list. (r7rs 41)

'Undocumented' extension: val and force-set? can be used to set the car"
  (debug-assert (pair? cell))
  (access cdrs cell val force-set?))
(fn set-car! [cell val] (car cell val true))
(fn set-cdr! [cell val] (cdr cell val true))
;; cdadrs (r7rs 42)
(fn caar [cell val force-set?]
  (car (car cell) val force-set?))
(fn cadr [cell val force-set?]
  (car (cdr cell) val force-set?))
(fn cdar [cell val force-set?]
  (cdr (car cell) val force-set?))
(fn cddr [cell val force-set?]
  (cdr (cdr cell) val force-set?))
;; TODO: cxr library?


(fn pair-mt.__eq [x y]
  (and (= (car x) (car y))
       (= (cdr x) (cdr y))))
;;; Roberto Ierusalimschy Programming in lua 4ed p145
(fn icons [cell]
  "Indexed iterator for a list made of cons cells."
  (var cell cell)
  (var i 1)
  (fn []
    (if (null? cell) nil
        (let [old i
              a   (car cell)]
          (set i (+ 1 i))
          (set cell (cdr cell))
          (values old a)))))
(set pair-mt.__pair icons)

(fn cons [a d]
  "Construct a pair (cons cell) with car ~a~ and cdr ~d~"
  (let [cell (setmetatable {} pair-mt)]
    (car cell a true)
    (cdr cell d true)
    cell))
(fn uncons [cell]
  "Return the car and cdr. Useful for destructuring. "
  (values (car cell) (cdr cell)))

;; TODO: what to do with polymorphic length?
(fn seq+list [seq lst]
  (faccumulate [lst lst
                i (length seq) 1 -1]
    (cons (. seq i) lst)))
(fn seq->list [seq] (seq+list seq null))
(fn list [& args] (seq->list args))
(fn cons* [& args]
  (let [last (table.remove args)]
    (seq+list args last)))
(fn make-list [k fill]
  (faccumulate [acc null
                _ 1 k]
    (cons fill acc)))
(fn reverse-append [x y]
  (if (null? x) y
      (reverse-append (cdr x)
                      (cons (car x) y))))
(fn reverse [x]
  (reverse-append x null))
(fn list-copy [x]
  (reverse (reverse x)))
;; TODO: do less work
(fn append [x ...]
  (fn append2 [x y]
    (reverse-append (reverse x) y))
  (if (nil? x) null
      (append2 x (append ...))))
(fn list-tail [lst k]
  (if (zero? k) lst
      (list-tail (cdr lst) (- k 1))))
(fn list-ref [lst n val force?]
  (car (list-tail lst (+ 1 n)) val force?))
(fn list-set! [lst n val]
  (list-ref lst n val true))

;; TODO: i don't like the name
;; r7rs 43
(fn mem-pred [lst pred?]
  (fn loop [lst]
    (if (null? lst) false
        (let [(a d) (uncons lst)]
          (if (pred? a) lst
              (loop d)))))
  (loop lst))
(fn member [obj lst compare?]
  (let [compare? (or compare? equal?)]
    (mem-pred lst #(compare? obj $))))
(local memq #(member $1 $2 eq?))
(local memv #(member $1 $2 eqv?))
(fn assoc-pred [alist pred?]
  (let [good-car? #(pred? (car $))
        memed     (mem-pred alist good-car?)]
    (and memed (car memed))))
(fn assoc [obj alist compare?]
  (let [compare (or compare? equal?)]
   (assoc-pred alist #(compare? obj $))))
(local assq #(assoc $1 $2 eq?))
(local assv #(assoc $1 $2 eqv?))

(local vector-mt {})
(fn vector? [x]
  (= (getmetatable x) vector-mt))
(fn vector-ref [vec k obj force?]
  (assert (vector? vec))
  (assert (number? k))
  (zaccess vec k obj force?))
(fn vector-set! [vec k obj]
  (vector-ref vec k obj true))
(fn make-vector [k fill]
  (assert (number? k))
  (let [acc (setmetatable {:length k} vector-mt)]
    (for [i 1 k]
      (. acc i fill))
    acc))
(fn vector-length [v] v.length)
(set vector-mt.__len vector-length)
(fn vector-mt.__eq [x y]
  (and (= (vector-length x) (vector-length y))
       (let [k (vector-length x)]
         (faccumulate [ok true
                       i 1 k
                       &until (not ok)]
           (= (. x i) (. y i))))))

(local bytevector-mt {})
(fn bytevector? [x]
  (= (getmetatable x) bytevector-mt))

{: null : null? : cons : car : cdr : pair? : list : cons*}
