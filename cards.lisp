
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               cards.lisp
;;;;
;;;;   Started:            Sun May 31 02:36:52 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;   https://en.wikipedia.org/wiki/Playing_cards_in_Unicode
;;;;   https://codepoints.net/playing_cards
;;;;   https://eev.ee/blog/2015/09/12/dark-corners-of-unicode/
;;;;
;;;;   https://en.wikipedia.org/wiki/List_of_poker_hands
;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+ :sbcl (load "/home/slytobias/lisp/packages/core" :verbose nil)
  #- :sbcl (load "/home/slytobias/lisp/packages/core.lisp" :verbose nil))

;(load "/home/slytobias/lisp/packages/collections.lisp")

(defpackage :cards
  (:shadowing-import-from :collections :intersection :set :subsetp :union)
  (:use :common-lisp :core :collections)
  (:export :rank :suit :face-up :label :turn :turn-up :turn-down
           :emptyp :remaining :shuffle :deal :hit :add :nth :cards :count :insert
           :discard :presentp :remove :classify :pprint :first :last
           :clubs :diamonds :hearts :spades
           :card :hand :deck :ranks :suits
           :jack :queen :king :ace)
  (:shadow :shuffle :count :emptyp :remove :pprint :sort :nth :first :last))

(in-package :cards)

(declaim (optimize safety))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    CARD class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defconstant ranks #(2 3 4 5 6 7 8 9 10 jack queen king ace))
(defconstant ranks #(ace 2 3 4 5 6 7 8 9 10 jack queen king))
(defconstant suits #(clubs diamonds hearts spades))
(defconstant suit-labels '((spades ."♠")
                           (hearts . "♡")
                           (diamonds . "♢")
                           (clubs . "♣")))
;; (defconstant suit-labels '((spades ."♤")
;;                            (hearts . "♡")
;;                            (diamonds . "♢")
;;                            (clubs . "♧")))
;; (defconstant suit-labels '((spades ."♠")
;;                            (hearts . "♥")
;;                            (diamonds . "♦")
;;                            (clubs . "♣")))

(defun rank+ (card)
  "Determine the numerical rank for CARD."
  (position (rank card) ranks))

(defun value (card)
  "Determine the numerical value for CARD."
  (assert (typep card 'rank))
  (1+ (position (rank card) ranks)))

(defun rankp (r)
  (find r ranks))
(deftype rank ()
  '(satisfies rankp))

(defun suitp (s)
  (find s suits))
(deftype suit ()
  '(satisfies suitp))

(defun suit< (s1 s2)
  (< (position s1 suits)
     (position s2 suits)))

(defun rank< (r1 r2)
  (< (position r1 ranks)
     (position r2 ranks)))

(defclass card ()
  ((rank :reader rank :initarg :rank :type rank)
   (suit :reader suit :initarg :suit :type suit)
   (face-up :reader face-up :initform nil)))

;;;
;;;    Problematic even for INITIALIZE-INSTANCE!
;;;    
;; (defmethod rank :around ((c card))
;;   (if (face-up c)
;;       (call-next-method)
;;       (error "No can peek!")))

;; (defmethod suit :around ((c card))
;;   (if (face-up c)
;;       (call-next-method)
;;       (error "No can peek!")))

(defun card-equal (card1 card2)
  (and (eql (rank card1) (rank card2)) 
       (eql (suit card1) (suit card2))))

(defmethod initialize-instance :after ((c card) &rest initargs)
  (declare (ignore initargs))
  (assert (typep (rank c) 'rank))
  (assert (typep (suit c) 'suit)))

(defgeneric label (card))
(defmethod label ((c card))
  (cdr (assoc (suit c) suit-labels)))

(defgeneric turn (card))
(defmethod turn ((c card))
  (with-slots (face-up) c
    (setf face-up (not face-up))))

(defgeneric turn-up (card))
(defmethod turn-up ((c card))
  (with-slots (face-up) c
    (setf face-up t)))

(defgeneric turn-down (card))
(defmethod turn-down ((c card))
  (with-slots (face-up) c
    (setf face-up nil)))

(defmethod print-object ((c card) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~A of ~A" (rank c) (label c))))

(defun pprint (cards stream)
  (format stream "~:{~A~A~:^, ~}" (mapcar #'(lambda (card) (list (rank card) (label card))) cards)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    HAND class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass hand ()
  ((cards :reader cards :initform (make-array 10 :fill-pointer 0 :adjustable t))))

(defgeneric count (cards)
  (:documentation "How many cards are in the collection?"))
(defmethod count ((h hand))
  (length (cards h)))

(defmethod print-object ((h hand) stream)
  (print-unreadable-object (h stream :type t)
    (format stream "~A" (cards h))))

(defmethod turn-up ((h hand))
  (loop for card across (cards h)
        do (turn-up card)))

(defmethod turn-down ((h hand))
  (loop for card across (cards h)
        do (turn-down card)))

(defgeneric add (cards card)
  (:documentation "Add a CARD to a collection of cards."))
(defmethod add ((h hand) (c card))
  (vector-push-extend c (cards h))
  c)
(defmethod add ((dest hand) (src hand))
  (loop until (emptyp src)
        do (add dest (discard src 0))
        finally (return dest)))

(defgeneric insert (cards i card)
  (:documentation "Add CARD to a collection of cards at index I."))
(defmethod insert ((h hand) i (c card))
  (labels ((extend (store)
             (unless (array-in-bounds-p store (fill-pointer store))
               (adjust-array store (* 2 (length store))))
             (incf (fill-pointer store)))
           (shift-up (store i)
             (setf (subseq store (1+ i)) (subseq store i))))
    (assert (<= 0 i (count h)) () "Invalid index: ~A" i)
    (extend (cards h))
    (shift-up (cards h) i)
    (setf (aref (cards h) i) c)))

(defgeneric discard (hand card)
  (:documentation "Remove a specific CARD from the HAND"))
(defmethod discard ((h hand) (c card))
  (let ((index (position c (cards h) :test #'card-equal)))
    (unless (null index)
      (discard h index))))
(defmethod discard ((h hand) (i integer))
  (flet ((shift-down (store)
           (setf (subseq store i) (subseq store (1+ i)))) )
    (assert (<= 0 i (1- (count h))) () "Invalid index: ~A" i)
    (let ((doomed (nth h i)))
      (shift-down (cards h))
      (decf (fill-pointer (cards h)))
      doomed)))

(defgeneric emptyp (cards)
  (:documentation "Is the collection of cards empty?"))
(defmethod emptyp ((h hand))
  (zerop (count h)))

(defgeneric sort (hand)
  (:documentation "Sort the cards in a hand by suit then rank."))
(defmethod sort ((h hand))
  (with-slots (cards) h
    (setf cards (sort-by cards (list (list #'suit< #'suit) (list #'rank< #'rank)))) ))

(defgeneric nth (hand n)
  (:documentation "Return Nth card in HAND."))
(defmethod nth ((h hand) n)
  (elt (cards h) n))

(defgeneric first (hand)
  (:documentation "Return the first card in HAND."))
(defmethod first :around ((h hand))
  (if (emptyp h)
      (error "Hand is empty.")
      (call-next-method)))
(defmethod first ((h hand))
  (nth h 0))

(defgeneric last (hand)
  (:documentation "Return the last card in HAND."))
(defmethod last :around ((h hand))
  (if (emptyp h)
      (error "Hand is empty.")
      (call-next-method)))
(defmethod last ((h hand))
  (nth h (1- (count h))))

; clear

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    DECK class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Add hash slot to test for duplicates??
;;;    
(defclass deck ()
  ((cards :reader cards :initform (make-linked-queue))
   (count :reader count :documentation "The number of cards in the full deck.")
   (random-state :initform (make-random-state t))))

(defmethod initialize-instance :after ((d deck) &rest initargs)
  (declare (ignore initargs))
  (populate d)
  (shuffle d))

(defgeneric populate (deck)
  (:documentation "Fill up a DECK with cards."))
(defmethod populate :around ((d deck))
  (assert (emptyp d))
  (call-next-method))
(defmethod populate ((d deck))            
  (with-slots (cards count) d
    (loop for suit across suits
          do (loop for rank across ranks 
                   do (enqueue cards (make-instance 'card :rank rank :suit suit))))
    (setf count (size cards))))

(defmethod print-object ((d deck) stream)
  (with-slots (cards count) d
    (if (emptyp d)
        (print-unreadable-object (d stream :type t)
          (format stream "~D (~D) card~P" (remaining d) count (remaining d)))
        (print-unreadable-object (d stream :type t)
          (format stream "~D (~D) card~P ~A" (remaining d) count (remaining d) (front cards)))) ))

(defgeneric remaining (deck))
(defmethod remaining ((d deck))
  (size (cards d)))

(defmethod emptyp ((d deck))
  (zerop (remaining d)))

(defgeneric shuffle (deck))
(defmethod shuffle ((d deck))
  (with-slots (cards random-state) d
    (let ((shuffled (core:shuffle (coerce (elements cards) 'vector) random-state)))
      (make-empty cards)
      (loop for card across shuffled do (enqueue cards card)))) )

(defgeneric deal (deck count))
(defmethod deal :around ((d deck) count)
  (assert (not (emptyp d)))
  (call-next-method))
(defmethod deal ((d deck) count)
  (let ((hand (make-instance 'hand)))
    (with-slots (cards) d
      (loop repeat count
            do (add hand (dequeue cards))
            finally (return hand)))) )

(defgeneric hit (deck hand))
(defmethod hit :around ((d deck) (h hand))
  (assert (not (emptyp d)))
  (call-next-method))
(defmethod hit ((d deck) (h hand))
  (with-slots (cards) d
    (add h (dequeue cards))))

(defgeneric refill (deck)
  (:documentation "Refill an empty deck."))
(defmethod refill :around ((d deck))
  (assert (emptyp d))
  (call-next-method))
(defmethod refill ((d deck))
  (populate d)
  (shuffle d))

(defgeneric presentp (deck card))
(defmethod presentp ((d deck) (c card))
  (with-slots (cards) d
    (contains cards c :test #'card-equal)))

(defmethod add :around ((d deck) (c card))
  (assert (not (= (remaining d) (count d))) () "Deck is full.")
  (assert (not (presentp d c)) () "Card ~A already present in deck." c)
  (call-next-method))
(defmethod add ((d deck) (c card))
  (with-slots (cards) d
    (enqueue cards c)))

(defgeneric remove (deck card)
  (:documentation "Remove the given CARD from the DECK if present."))
(defmethod remove ((d deck) (target card))
  (with-slots (cards) d
    (loop repeat (remaining d)
          for card = (dequeue cards)
          if (card-equal target card) do (return target)
          else do (enqueue cards card)
          finally (return nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    POKER-HAND class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass poker-hand (hand)
  ()
  (:documentation "A hand of cards in poker."))

(defgeneric flushp (hand)
  (:documentation "Is this HAND a flush?"))
(defmethod flushp ((h hand))
  (and (= (count h) 5)
       (equalelts (cards h) :key #'suit)))

(defgeneric straightp (hand)
  (:documentation "Is this HAND a straight?"))
(defmethod straightp ((h hand))
  "Determines whether HAND is a straight and returns the low card."
  (if (= (count h) 5)
      (let ((sorted-hand (coerce (cl:sort (copy-seq h) #'< :key #'rank+) 'list)))
        (values (every #'(lambda (a b) (= 1 (- (rank+ a) (rank+ b))))
                       (rest sorted-hand)
                       sorted-hand)
                (first sorted-hand)))
      (values nil nil)))

(defgeneric straight-flush-p (hand)
  (:documentation "Is this HAND a straight flush?"))
(defmethod straight-flush-p ((h hand))
  (and (straightp h) (flushp h)))

(defgeneric royal-flush-p (hand)
  (:documentation "Is this HAND a royal flush?"))
(defmethod royal-flush-p ((h hand))
  (multiple-value-bind (straightp low-card) (straightp h)
    (and straightp
         (flushp h)
         (= (rank+ low-card) 10))))

(defgeneric bin (hand)
  (:documentation "Group cards in HAND by rank."))
(defmethod bin ((h hand))
  (loop with bins = (make-hash-table)
        for card in (cards h)
        do (push card (gethash (rank card) bins '()))
        finally (return bins)))

(defgeneric four-of-a-kind-p (hand)
  (:documentation "Does this HAND contain 4 of a kind?"))
(defmethod four-of-a-kind-p ((h hand))
  (if (= (count h) 5)
      (loop for v being the hash-values in (bin h)
            if (= (length v) 4) do (return t)
            finally (return nil))
      nil))

(defgeneric three-of-a-kind-p (hand)
  (:documentation "Does this HAND contain 3 of a kind?"))
(defmethod three-of-a-kind-p ((h hand))
  (if (= (count h) 5)
      (loop for v being the hash-values in (bin h)
            if (= (length v) 3) do (return t)
            finally (return nil))
      nil))

(defgeneric count-pairs (hand)
  (:documentation "How many pairs does this HAND contain?"))
(defmethod count-pairs ((h hand))
  (loop for v being the hash-values in (bin h)
        count (= (length v) 2) into pairs
        finally (return pairs)))

(defgeneric full-house-p (hand)
  (:documentation "Is this HAND a full house?"))
(defmethod full-house-p ((h hand))
  (and (= (count h) 5)
       (three-of-a-kind-p h)
       (= 1 (count-pairs h))))

(defgeneric two-pair-p (hand)
  (:documentation "Does this HAND contain 2 pairs?"))
(defmethod two-pair-p ((h hand))
  (= 2 (count-pairs h)))

(defgeneric one-pair-p (hand)
  (:documentation "Does this HAND contain only 1 pair?"))
(defmethod one-pair-p ((h hand))
  (= 1 (count-pairs h)))

(defgeneric classify (hand)
  (:documentation "Classify a HAND in poker."))
(defmethod classify ((h hand))
  (assert (= 5 (count h)) () "Not a poker hand.")
  (cond ((royal-flush-p h) :royal-flush)
        ((straight-flush-p h) :straight-flush)
        ((four-of-a-kind-p h) :four-of-a-kind)
        ((full-house-p h) :full-house)
        ((flushp h) :flush)
        ((straightp h) :straight)
        ((three-of-a-kind-p h) :three-of-a-kind)
        ((= 2 (count-pairs h)) :two-pair)
        ((= 1 (count-pairs h)) :one-pair)
        (t :high-card)))

