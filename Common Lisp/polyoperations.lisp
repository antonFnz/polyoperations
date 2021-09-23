;;;; -*- Mode: Lisp -*-

;; functions that verifies structures used during operations
(defun is-monomial (m)
  (and (listp m)
       (eq 'm (first m))
       (numberp (monomial-coefficient m))
       (let ((mtd (monomial-degree m))
             (vps (var-powers m))
             )
         (and (integerp mtd)
              (>= mtd 0)
              (listp vps)
              (every #'is-varpower vps)))))

(defun monomial-coefficient (m)
  (second m))

(defun monomial-degree (m)
  (third m))

(defun var-powers (m)
  (fourth m))

(defun is-varpower (vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (varpower-power vp))
             (v (varpower-symbol vp))
             )
         (and (integerp p)
              (>= p 0)
              (symbolp v)))))

(defun varpower-power (vp)
  (second vp))

(defun varpower-symbol (vp)
  (third vp))

(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (p-monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))

(defun p-monomials (p)
  (second p))

;;;; get-monomial
;; returns a monomial structure obtained parsing expression
(defun get-monomial (expression)
  (let ((mono (parse-monomial expression)))
    (list 'm
          (second mono)
          (reduce '+ (fourth mono) :key 'second)
          (simplify
           (sort (copy-list (fourth mono)) 'string< :key 'third)))))

;; parses the expression given with get-monomial
(defun parse-monomial (m)
  (cond ((numberp m)
         (list 'm m 0 nil))
        ((symbolp m)
         (list 'm 1 1 (list (list 'v 1 m))))
        ((and (eq '* (first m))
              (null (second m)))
         (list 'm 0 0 nil))
        ((eq 'expt (first m))
         (list 'm 1 (third m) (list (list 'v (third m) (second m)))))
        ((and (eq '* (first m))
              (symbolp (second m)))
         (list 'm 1 'td (parse-vars (rest m))))
        ((and (eq '* (first m))
              (listp (second m))
              (eq 'expt (first (second m))))
         (list 'm 1 'td (parse-vars (rest m))))
        ((and (eq '* (first m))
              (numberp (second m)))
         (list 'm (second m) 'td (parse-vars (rest (rest m)))))
        (T (error "Syntax Error"))))

(defun parse-vars (v)
  (cond ((null v) nil)
        ((atom (first v))
         (cons (list 'v 1 (first v))
               (parse-vars (rest v))))
        ((eq 'expt (first (first v)))
         (if (= 0 (third (first v)))
             (parse-vars (rest v))
           (cons (list 'v (third (first v)) (second (first v)))
                 (parse-vars (rest v)))))))

;; simplify a list of variables
(defun simplify (v)
  (cond ((null v) nil)
        ((equal (third (first v)) (third (second v)))
         (simplify (cons (list 'v
                               (+ (second (first v)) (second (second v)))
                               (third (first v)))
                         (rest (rest v)))))
        (T (cons (first v)
                 (simplify (rest v))))))

;;;; get-polynomial
;; returns a polymonial structure obtained parsing expression
(defun get-polynomial (expression)
  (if (null expression) (list 'poly nil)
    (order-poly (list 'poly (parse-monomials expression)))))

;; parses each monomial of polynomial
(defun parse-monomials (ms)
  (if (and (listp ms)
           (eq '+ (first ms)))
      (mapcar 'get-monomial (rest ms))
    (list (get-monomial ms))))

;; orders parsed monomials
(defun order-poly (ms)
  (list (first ms)
        (remove-null-ms
         (simplify-poly
          (sort (copy-list (second ms)) 'compare-m)))))

;; simplifies monomials and removes null monomials
(defun simplify-poly (ms)
  (cond ((null ms) nil)
        ((null (second ms)) ms)
        ((equal (fourth (first ms)) (fourth (second ms)))
         (simplify-poly
          (cons (list 'm
                      (+ (second (first ms))
                         (second (second ms)))
                      (third (first ms))
                      (fourth (first ms)))
                (rest (rest ms)))))
        (T (cons (first ms)
                 (simplify-poly (rest ms))))))

;; removes null monomials from list of monomials
(defun remove-null-ms (ms)
  (cond ((null ms) nil)
        ((zerop (second (first ms)))
         (remove-null-ms (rest ms)))
        (T (cons (first ms)
                 (remove-null-ms (rest ms))))))

;; functions used by sorting function to compare elements
(defun compare-m (m1 m2)
  (cond ((< (third m1) (third m2)) T)
        ((= (third m1) (third m2))
         (compare-v (fourth m1) (fourth m2)))
        (T nil)))

(defun compare-v (v1 v2)
  (if (equal v1 v2) T
    (cond ((equal (first v1) (first v2))
           (compare-v (rest v1) (rest v2)))
          ((eq (third (first v1)) (third (first v2)))
           (< (second (first v1)) (second (first v2))))
          (T (string< (third (first v1))
                      (third (first v2)))))))

;;;; print-polynomial
;; prints in std out the polymonial given as input
(defun print-polynomial (poly)
  (if (is-polynomial poly)
      (pprint-polynomial poly)
    (pprint-polynomial (get-polynomial poly))))

(defun pprint-polynomial (poly)
  (let ((out (make-string-output-stream)))
    (pprint-ms (second poly) out)
    (format t "~a" (get-output-stream-string out))))

;; prints monomials
(defun print-ms (ms out)
  (if (null (rest ms))
      (progn (print-m (first ms) out)
             (format out "~%"))
    (progn (print-m (first ms) out)
	   (format out " + ")
	   (print-ms (rest ms) out))))

;; prints single monomial
(defun print-m (m out)
  (cond ((null (fourth m))
         (format out "~d" (second m)))
        ((= (second m) 1)
         (print-v (fourth m) out))
        (T
         (format out "~d " (second m))
         (print-v (fourth m) out))))

;; prints variables and their exponent
(defun print-v (v out)
  (cond ((and (null (rest v))
              (= 1 (second (first v))))
         (format out "~a" (third (first v))))
        ((null (rest v))
         (format out "~a^~d" (third (first v)) (second (first v))))
        ((= 1 (second (first v)))
         (format out "~a " (third (first v)))
         (print-v (rest v) out))
        (T
         (format out "~a^~d " (third (first v)) (second (first v)))
         (print-v (rest v) out))))

;;;; is-zero
;; returns true when x is any representation of null polynomial/monomial
(defun is-zero (x)
  (cond ((and (numberp x) (= 0 x)) T)
        ((and (is-monomial x)
              (= 0 (second x))) T)
        ((and (is-polynomial x)
              (null (second x))) T)
        (T nil)))

;;;; vars-of
;; returna the list of Variables of the internal representation
(defun vars-of (x)
  (cond ((is-polynomial x)
         (if (null (second x)) nil
           (append (vars-of (first (second x)))
                   (vars-of (list 'poly (rest (second x)))))))
        ((is-monomial x)
         (var-powers x))
        (T (vars-of (get-polynomial x)))))

;;;; coefficients
;; returns the list of coefficients that appear in polynomial
(defun coefficients (poly)
  (let ((ms (monomials poly)))
    (mapcar 'monomial-coefficient ms)))

;;;; variables
;; returns the list of variable symbols that appears in polynomial, without duplicates
(defun variables (poly)
  (sort (copy-list
         (remove-duplicates (mapcar (lambda (x) (third x)) (vars-of poly))))
        'string<))

;;;; monomials
;; returns the list of monomials composing the polynomial
(defun monomials (poly)
  (cond ((is-polynomial poly)
         (p-monomials poly))
        ((is-monomial poly) (list poly))
        (T (monomials (get-polynomial poly)))))

;;;; max-degree
;; returns the max degree of any monomial composing the polynomial
(defun max-degree (p)
  (if (is-polynomial p)
      (if (null (second p)) 0
        (apply 'max (mapcar (lambda (x) (third x)) (second p))))
    (max-degree (get-polynomial p))))

;;;; min-degree
;; returns the min degree of any monomial composing the polynomial
(defun min-degree (p)
  (if (is-polynomial p)
      (if (null (second p)) 0
        (apply 'min (mapcar (lambda (x) (third x)) (second p))))
    (min-degree (get-polynomial p))))

;;;; poly-add
;; returns p1 + p2, sum of 2 polynomials or monomials
(defun poly-add (p1 p2)
  (order-poly (list 'poly (append (monomials p1) (monomials p2)))))

;;;; poly-subt
;; returns p1 - p2, which is p1 + (-p2)
(defun poly-subt (p1 p2)
  (poly-add p1 (p-opposite p2)))

;; returns the opposite of a polynomial
(defun p-opposite (p)
  (list 'poly (m-opposite (monomials p))))

;; returns the opposites of a list of monomials
(defun m-opposite (ms)
  (if ms (cons (list 'm
                     (* -1 (monomial-coefficient (first ms)))
                     (monomial-degree (first ms))
                     (var-powers (first ms)))
               (m-opposite (rest ms)))))

;;;; poly-mul
;; returns p1 * p2, product of the 2 polynomials/monomials
(defun poly-mul (p1 p2)
  (order-poly (list 'poly (ms-times (monomials p1) (monomials p2)))))

(defun ms-times (ms1 ms2)
  (if ms1 (append (m-times (first ms1) ms2))
    (ms-times (rest ms1) ms2)))

(defun m-times (m1 ms2)
  (if ms2
      (let ((m2 (first ms2)))
        (cons (list 'm
                    (* (second m1) (second m2))
                    (+ (third m1) (third m2))
                    (simplify
                     (sort (copy-list (append (fourth m1) (fourth m2)))
                           'string< :key 'third)))
              (m-times m1 (rest ms2))))))

;;;; poly-eval
;; returns the value of a polynomial in a n-dimensional point in space represented by values
;; which is a list containing a numeric value of each variable symbol of the polynomial
(defun poly-eval (p values)
  (ms-eval (monomials p)
          (variables (list 'poly (monomials p)))
          values))

(defun ms-eval (ms vars values)
  (if ms (+ (* (second (first ms))
               (vs-eval (fourth (first ms)) vars values))
            (ms-eval (rest ms) vars values))
    0))

(defun vs-eval (vs vars values)
  (if vs (let* ((v (first vs))
                (value (nth (position (third v) vars) values)))
           (if value (* (expt value (second v))
                        (vs-eval (rest vs) vars values))
             (error "Not enough values.")))
    1))
