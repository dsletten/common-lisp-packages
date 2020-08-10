
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
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/collections.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :cards
  (:shadowing-import-from :collections :intersection :set :subsetp :union)
  (:use :common-lisp :lang :collections :test) 
  (:export :rank :suit :face-up :label :turn :turn-up :turn-down :deck :emptyp :remaining :shuffle :deal :add :presentp :clubs :diamonds :hearts :spades :card :deck :jack :queen :king :ace)
  (:shadow :shuffle :count))

(in-package :cards)

(defconstant ranks #(2 3 4 5 6 7 8 9 10 jack queen king ace))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    CARD class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass card ()
  ((rank :reader rank :initarg :rank)
   (suit :reader suit :initarg :suit)
   (face-up :reader face-up :initform nil)))

(defun card-equal (card1 card2)
  (and (eql (rank card1) (rank card2)) 
       (eql (suit card1) (suit card2))))

(defun rankp (r)
  (find r ranks))
(deftype rank ()
  '(satisfies rankp))

(defun suitp (s)
  (find s suits))
(deftype suit ()
  '(satisfies suitp))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    DECK class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Add hash slot to test for duplicates??
;;;    
(defclass deck ()
  ((cards :reader cards)
   (count :reader count :documentation "The number of cards in the full deck.")
   (random-state :initform (make-random-state t))))

(defmethod initialize-instance :after ((d deck) &rest initargs)
  (declare (ignore initargs))
  (with-slots (cards count) d
    (let ((deck (make-linked-queue)))
      (loop for suit across suits
            do (loop for rank across ranks do (enqueue deck (make-instance 'card :rank rank :suit suit))))
      (setf cards deck
            count (size deck))))
  (shuffle d))

(defmethod print-object ((d deck) stream)
  (with-slots (cards count) d
    (if (zerop (remaining d))
        (print-unreadable-object (d stream :type t)
          (format stream "~D (~D) card~P" (remaining d) count (remaining d)))
        (print-unreadable-object (d stream :type t)
          (format stream "~D (~D) card~P ~A" (remaining d) count (remaining d) (front cards)))) ))
  
(defgeneric remaining (deck))
(defmethod remaining ((d deck))
  (size (cards d)))

(defgeneric emptyp (deck))
(defmethod emptyp ((d deck))
  (zerop (size (cards d))))

(defgeneric shuffle (deck))
(defmethod shuffle ((d deck))
  (with-slots (cards random-state) d
    (let ((shuffled (lang:shuffle (coerce (elements cards) 'vector) random-state))
          (shuffled-deck (make-linked-queue)))
      (loop for card across shuffled do (enqueue shuffled-deck card))
      (setf cards shuffled-deck))))

(defgeneric deal (deck))
(defmethod deal ((d deck))
  (with-slots (cards) d
    (dequeue cards)))

(defmethod deal :around ((d deck))
  (assert (not (emptyp (cards d))))
  (call-next-method))

(defgeneric presentp (deck card))
(defmethod presentp ((d deck) (c card))
  (with-slots (cards) d
    (contains cards c :test #'card-equal)))

(defgeneric add (deck card))
(defmethod add ((d deck) (c card))
  (with-slots (cards) d
    (enqueue cards c)))

(defmethod add :around ((d deck) (c card))
  (assert (not (= (remaining d) (count d))) () "Deck is full.")
  (assert (not (presentp d c)) () "Card ~A already present in deck." c)
  (call-next-method))
