(define-module (scmutils))

(eval-when (load compile eval)
           (load-from-path "load.scm"))

;; Unary operators

(define type g:type)
(define type-predicate g:type-predicate)
(define arity g:arity)

(define inexact? g:inexact?)

(define zero-like g:zero-like)
(define one-like g:one-like)
(define identity-like g:identity-like)

(define zero? g:zero?)
(define one? g:one?)
(define identity? g:identity?)

(define negate g:negate)
(define invert g:invert)

(define square g:square)
(define cube   g:cube)

(define sqrt g:sqrt)

(define exp g:exp)
(define log g:log)

(define exp2  g:exp2)
(define exp10 g:exp10)
(define log2  g:log2)
(define log10 g:log10)

(define sin g:sin)
(define cos g:cos)
(define tan g:tan)
(define sec g:sec)
(define csc g:csc)

(define asin g:asin)
(define acos g:acos)

(define sinh g:sinh)
(define cosh g:cosh)
(define tanh g:tanh)
(define sech g:sech)
(define csch g:csch)

(define asinh g:asinh)
(define acosh g:acosh)
(define atanh g:atanh)

(define abs g:abs)

(define determinant g:determinant)
(define trace g:trace)

(define derivative g:derivative)

;; Binary (and n-ary) operators

(define = g:=)

(define + g:+)
(define - g:-)
(define * g:*)
(define / g:/)

(define dot-product g:dot-product)

(define expt g:expt)
(define gcd g:gcd)

;; Complex operators

(define make-rectangular g:make-rectangular)
(define make-polar g:make-polar)

(define real-part g:real-part)
(define imag-part g:imag-part)
(define magnitude g:magnitude)
(define angle g:angle)

(define conjugate g:conjugate)

;; Misc operators

(define atan g:atan)

(define partial-derivative g:partial-derivative)
(define partial g:partial)

(define apply g:apply)

;; Compound operators (from mathutil.scm)

(define arg-scale g:arg-scale)
(define arg-shift g:arg-shift)

(define sigma g:sigma)

(define ref   g:ref)
(define size  g:size)

(define compose g:compose)

;; Export bindings -- hack to just export everything
(module-for-each (lambda (sym var)
                   (module-replace! (current-module) (list sym)))
                 (current-module))
