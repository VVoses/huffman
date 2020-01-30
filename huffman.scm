
;;;
;;; Data structure:
;;;

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;;;
;;; Decoding a tree:
;;;

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (decode-tail bits tree)
  (if (null? bits)
    '()
    (let ((next-branch
        (choose-branch (car bits) tree)))
      (if (leaf? next-branch)
        (cons (symbol-leaf next-branch)
          (decode (cdr bits) tree))
        (decode (cdr bits) next-branch)))))

(define (choose-branch bit branch)
  (if (= bit 0)
      (left-branch branch)
      (right-branch branch)))


;;;
;;; Sorting lists:
;;;

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;;;
;;; Test data:
;;;

(define sample-tree
  (make-code-tree
   (make-leaf 'chickens 8)
   (make-code-tree
    (make-leaf 'eat 5)
    (make-code-tree
     (make-leaf 'night 1)
     (make-leaf 'by 1)))))

(define sample-code '(0 1 0 0 1 1 1 1 1 0))

;;;
;;; encoding
;;;

(define (encode-symbol symbol tree)
  (define (exists? symbol branch)
    (member symbol (symbols branch))
  )
  (if (leaf? tree)
    '()
    (let ((left (left-branch tree))
          (right (right-branch tree)))
      (cond ((exists? symbol left) (cons 0 (encode-symbol symbol left)))
        ((exists? symbol right) (cons 1 (encode-symbol symbol right)))
        (else #f)))))

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
      (encode (cdr message) tree))))

(define (grow-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (cond ((null? set) '())
    ((null? (cdr set)) (car set))
    (else (successive-merge (adjoin-set (make-code-tree (car set) (cadr set)) (cddr set))))))

(define (huffman-leaves tree)
  (if (leaf? tree)
    (list (list (symbol-leaf tree) (weight-leaf tree)))
    (append (huffman-leaves (left-branch tree)) (huffman-leaves (right-branch tree)))))

;;; takes a tree and return expected codeword length
(define (expected-codeword-length tree)
  (let ((total (weight tree)))
    (define (codeword-help branch)
      (if (leaf? branch)
        (* (/ (weight-leaf branch) total) (length (encode-symbol (symbol-leaf branch) tree)))
        (+ (codeword-help (left-branch branch)) (codeword-help (right-branch branch)))))
    (codeword-help tree)))
