(local debug-assert assert)
(macro debug-do [...] `(do ,...))

(fn number?    [n] (= (type n) :number))
(fn boolean?   [x] (= (type x) :boolean))
(fn procedure? [f]
  "Returns #t if obj is a procedure, otherwise returns #f."
  (= (type f) :function))
;; Lua strings are immutable and interned, like scheme's symbols
;; Scheme's strings are mutable arrays.
(fn symbol?    [s] (= (type s) :string))
(fn zero?      [n] (= n 0))

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
;; > One reason is that it is not always possible to compute `eqv?' of two
;; > numbers in constant time, whereas `eq?' implemented as pointer
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
(fn null? [obj]
  "Returns #t if OBJ is the empty list, otherwise returns #f (r7rs 42)"
  (= obj null))

(local cars (setmetatable {} {:__mode "k"}))
(local cdrs (setmetatable {} {:__mode "k"}))

(fn access [tbl key val force-set?]
  "Read or write the field KEY in TBL.
Calling access with only two arguments reads:
(access TBL KEY) ==> (. table KEY)

Calling access with an additional (non-nil) argument sets:
(access TBL KEY NEW-VALUE) ==> (tset TBL KEY NEW-VALUE)

To disambiguate setting nil from reading, pass true for force-set?:

(access TBL KEY MAYBE-NIL TRUE) => (tset TBL KEY MAYBE-NIL TRUE)

which allows unsetting:
(access TBL KEY nil TRUE) => (tset TBL KEY nil) "
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
  "Adapt `access' for javascript-style indexing via `zshift'. When KEY
is not a number, `zaccess' is equivalent to `access'. When KEY is a
number, add 1.

(zaccess tbl N ...) => (access tbl (+ 1 N) ...)

(zaccess tbl x ...) => (access tbl x ...)"
  (access tbl (zshift key) val force-set?))

(local pair-mt {})
(fn pair? [x] (= (getmetatable x) pair-mt))

(fn car [pair val force-set?]
  "Returns the contents of the car field of PAIR . Note that it
is an error to take the car of the empty list. (r7rs 41)

'Undocumented' extension: VAL and FORCE-SET? can be used to set the car"
  (debug-assert (pair? pair))
  (access cars pair val force-set?))
(fn cdr [pair val force-set?]
  "Returns the contents of the cdr field of PAIR . Note that it
is an error to take the cdr of the empty list. (r7rs 41)

'Undocumented' extension: VAL and FORCE-SET? can be used to set the car"
  (debug-assert (pair? pair))
  (access cdrs pair val force-set?))
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
(set pair-mt.__pairs icons)
(set pair-mt.__ipairs icons)

(fn cons [a d]
  "Construct a pair (cons cell) with car `a' and cdr `d'"
  (let [cell (setmetatable {} pair-mt)]
    (car cell a true)
    (cdr cell d true)
    cell))
(fn car+cdr [cell]
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
        (let [(a d) (car+cdr lst)]
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
(fn vector? [obj]
  "Returns #t if OBJ is a vector; otherwise returns #f."
  (= (getmetatable obj) vector-mt))
;; store length as field :n to match Lua's convention
(fn make-vector [k fill]
  "Returns a newly allocated vector of K elements. If a second
argument is given, then each element is initialized to FILL.
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
(fn unchecked-vector-ref [vec k obj force?]
  (zaccess vec k obj force?))
(fn vector-ref [vec k obj force?]
  "The `vector-ref' procedure returns the contents of element
K of [VEC] (r7rs 48).

'Undocumented' extention: behaves like accessor if you pass extra args"
  (debug-assert (vector? vec)) ; allow byte-vectors for the minute
  (debug-assert (number? k))
  ;; It is an error if K is not a valid index of vector. (r7rs 48)
  (debug-assert (and (<= 0 k) (< k (vector-length vec))))
  (unchecked-vector-ref vec k obj force?))
(fn vector-set! [vec k obj]
  "The `vector-set!' procedure stores OBJ in element K of
[VEC] (r7rs 48)."
  (vector-ref vec k obj true))
(macro xfor [[var start end] ...]
  "For but with a half-open interval: [start,end)"
  `(for [,var ,start (- ,end 1)]
    ,...))
(fn vector-fill! [vec fill start end]
  "The `vector-fill!' procedure stores fill in the elements of
[VEC] between START and END."
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
                  (unchecked-vector-set! (+ start i))))))
;; TODO: improve name. My real goal for this function is to allow growing vectors.
(fn unchecked-vector-copy! [to at from start end]
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
  "Copies the elements of vector FROM between START and END to vector TO, starting at AT.
The order in which elements are copied is unspecified, except that if the
source and destination overlap, copying takes place as if the source is
first copied into a temporary vector and then into the destination. This
can be achieved without allocating storage by making sure to copy in the
correct direction in such circumstances.

It is an error if AT is less than zero or greater than the length
of TO. It is also an error if (- (vector-length TO) AT) is less
than (- END START). (r7rs 49)"
  (debug-do
   (let [start (or start 0)
         end   (or end (vector-length from))]
     (assert (not (or (< at 0) (> at (length to))))
             "It is an error if at is less than zero or greater than the length of to (r7rs 49).") (assert (not (< (- (vector-length to) at) (- end start)))
             "It is also an error if (- (vector-length to) at) is less than (- end start). (r7rs 49)")))
  (unchecked-vector-copy! to at from start end))
(fn vector-copy [vec start end]
  (let [start (or start 0)
        end (or end (vector-length vec))
        new (vector)]
    (tset :n new (- end start))
    (vector-copy-no-overlap! new 0 vec)
    new))

;; 6.9 bytevector
;; Naive implementation as a vector of bytes
(local bytevector-mt {:__index vector-mt})
(fn bytevector? [obj]
  "Returns #t if OBJ is a bytevector. Otherwise, #f is returned. (r7rs 49)"
  (= (getmetatable x) bytevector-mt))
(fn make-bytevector [k byte]
  "The `make-bytevector' procedure returns a newly allocated
bytevector of length K. If BYTE is given, then all elements
of the bytevector are initialized to BYTE, otherwise the
contents of each element are unspecified. (r7rs 49)"
  (setmetatable (make-vector k byte) bytevector-mt))
(fn bytevector [...]
  "Returns a newly allocated bytevector containing its arguments."
  (setmetatable (vector ...) bytevector-mt))
(fn bytevector-length [v]
  "Returns the length of [V] in bytes as an exact integer."
  (vector-length v))
(fn call-as-vector [v f]
  (setmetatable v vector-mt)
  (local ret (f v))
  (setmetatable v bytevector-mt)
  ret)
(macro as-vector [[v & vs] ...]
  (if (= v nil)
      `(do ,...)
      `(call-as-vector v
        (fn [,v] (as-vector ,vs ,...)))))
(fn bytevector-u8-ref [v idx val force?]
  "Returns the K th byte of [V]
It is an error if K is not a valid index of [V] (r7rs 50)."
  (as-vector [v]
    (vector-ref v idx val force?)))
(fn bytevector-u8-set! [v k byte force?]
  "It is an error if K is not a valid index of [V]
Stores BYTE as the K th byte of [V] (r7rs 50)."
  (bytevector-u8-ref v k byte force?))
(fn bytevector-copy [v start end]
  "Returns a newly allocated bytevector containing the bytes
in [V] between START and END (r7rs 50)."
  (setmetatable
   (as-vector [v]
     (vector-copy v start end))
   bytevector-mt))
(fn bytevector-copy! [to at from start end]
  "Copies the bytes of bytevector FROM between START and END to bytevector TO, starting at AT.
 The order in which bytes are copied is unspecified, except that
if the source and destination overlap, copying takes place as if the source
is first copied into a temporary bytevector and then into the destination.
This can be achieved without allocating storage by making sure to copy in
the correct direction in such circumstances.

It is an error if AT is less than zero or greater than the length
of TO. It is also an error if (- (bytevector-length TO) AT) is
less than (- END START). (r7rs 50)"
  (as-vector [to from]
    (vector-copy! to at from start end)))
;; TODO: bytevector-append

;; 6.10 control features
(fn append-last [args]
  (let [pop! table.remove
        last (pop! args)]
    (each [_ x (ipairs last)]
      (table.insert args x))
    args))
(fn apply [f & args]
  (f (table.unpack (append-last args))))

;; ports/io
;; I think I can make a port just a stream from lua?
;; I think it depends on parameterize, at least for current-error-port
(fn current-input-port [] (io.input))
(fn current-output-port [] (io.output))
;; os.tmpname or io.tmpfile for opening strings as ports?

{: null : null? : cons : car : cdr : pair? : list : cons*}

;; Local Variables:
;; eval: (put 'xfor 'fennel-indent-function 'defun)
;; eval: (put 'as-vector1 'fennel-indent-function 'defun)
;; eval: (put 'as-vector 'fennel-indent-function 'defun)
;; End:
