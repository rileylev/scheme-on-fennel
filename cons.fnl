(local debug-assert assert)
(macro debug-do [...] `(do ,...))

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
(fn integer? [n] (= (math.type n) :integer))
(fn exact? [n] (integer?))
(fn inexact? [n] (and (number? n) (not (exact? n))))
;; TODO: how should exactness fit with Lua's types?
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

;; r7rs 6.8  p.48
(local vector-mt {})
(fn vector? [x]
  "Returns #t if obj is a vector; otherwise returns #f."
  (= (getmetatable x) vector-mt))
;; store length as field :n to match Lua's convention
(fn make-vector [k fill]
  "Returns a newly allocated vector of ~k~ elements. If a second
argument is given, then each element is initialized to ~fill~.
Otherwise the initial contents of each element is unspecified. (r7rs 48)"
  (assert (number? k))
  (let [vec (setmetatable {:n k} vector-mt)]
    (vector-fill! vec fill)
    vec))
(fn vector [...]
  "Returns a newly allocated vector whose elements contain
the given arguments. It is analogous to list. (r7rs 48)"
  (setmetatable (table.pack ...) vector-mt))
(fn vector-length [v]
  "Returns the number of elements in vector as an exact integer. (r7rs 48)"
  v.n)
(fn raw-vector-ref [vec k obj force?]
  (zaccess vec k obj force?))
(fn vector-ref [vec k obj force?]
  "The vector-ref procedure returns the contents of element
k of vector (r7rs 48).

'Undocumented' extention: behaves like accessor if you pass extra args"
  (debug-assert (vector? vec))
  (debug-assert (number? k))
  ;; It is an error if ~k~ is not a valid index of vector. (r7rs 48)
  (debug-assert (and (<= 0 k) (< k (vector-length vec))))
  (raw-vector-ref vec k obj force?))
(fn vector-set! [vec k obj]
  "The vector-set! procedure stores obj in element k of
vector (r7rs 48)."
  (vector-ref vec k obj true))
(macro xfor [[var start end] ...]
  "For but with half-open interval: [start,end)"
  `(for [,var ,start (- ,end 1)]
    ,...))
(fn vector-fill! [vec fill start end]
  "The ~vector-fill!~ procedure stores fill in the elements of
vector between start and end."
  (let [start (or start 0)
        end   (or end (length end))]
    (xfor [i start end]
      (vector-set! vec i fill))))
(set vector-mt.__len vector-length)
(fn vector-mt.__eq [x y]
  (and (= (vector-length x) (vector-length y))
       (let [k (vector-length x)]
         (faccumulate [ok true
                       i 1 k
                       &until (not ok)]
           (= (. x i) (. y i))))))
(local vector->list seq->list)
;;; TODO: list->seq + list->vector
;;; TODO: vector->string, string->vector
(fn no-overlap? [[a b] [c d]]
  (or (<= b c) (<= d a)))
(fn vector-copy-no-overlap! [to at from start end]
  (let [start (or start 0)
        end   (or end (vector-length end))
        len   (- end start)]
    (debug-assert (if (eq? to from)
                      (no-overlap? [at (+ at len)] [start end])))
    (xfor [i 0 len]
      (vector-set to (+ at i)
                  (raw-vector-set! (+ start i))))))
;; TODO: improve name. My real goal for this function is to allow growing vectors.
(fn raw-vector-copy! [to at from start end]
  ;; TODO: optimize
  ;; > This can be achieved without allocating storage by
  ;; > making sure to copy in the correct direction in
  ;; > such circumstances.  (r7rs 49)
  ;; suboptimal but correct implementation:
  (if (eq? to from)
      (let [len (- end start)
            buf (make-vector len)]
        (vector-copy-no-overlap! buf 0 from start end)
        (vector-copy-no-overlap! to at buf 0 len))
      (vector-copy-no-overlap! to at from start end)))
(fn vector-copy! [to at from start end]
  "It is an error if at is less than zero or greater than the length
of to. It is also an error if (- (vector-length to) at) is less
than (- end start).

Copies the elements of vector from between start and end
to vector to, starting at at. The order in which elements
are copied is unspecified, except that if the source and des-
tination overlap, copying takes place as if the source is first
copied into a temporary vector and then into the destina-
tion. This can be achieved without allocating storage by
making sure to copy in the correct direction in such cir-
cumstances. (r7rs 49)"
  (debug-do
   (let [start (or start 0)
         end   (or end (vector-length from))]
     (assert (not (or (< at 0) (> at (length to))))
                   "It is an error if at is less than zero or greater than the length of to (r7rs 49).")
     (assert (not (< (- (vector-length to) at)
                           (- end start)))
                   "It is also an error if (- (vector-length to) at) is less than (- end start). (r7rs 49)")))
  (raw-vector-copy! to at from start end))
(fn vector-copy [vec start end]
  (let [start (or start 0)
        end (or end (vector-length vec))
        new-vec {}]
    (tset :n new-vec (- end start))
    (raw-vector-copy! new-vec 0 vec)
    new-vec))

(local bytevector-mt {})
(fn bytevector? [x]
  (= (getmetatable x) bytevector-mt))

{: null : null? : cons : car : cdr : pair? : list : cons*}

;; Local Variables:
;; eval: (put 'xfor 'fennel-indent-function 'defun)
;; End:
