#lang racket
;GPS program was written by Newell Simon and Shaw in 1960
;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File gps1.lisp: First version of GPS (General Problem Solver)

; &rest - accumulates all the keyword-value pairs in keyword-args
; are specified two specific keyword pars, :test and :test-not
; if you have &key and want other keywords, you'll have to add &allow-other-keys

; The call to remove will contain 2 :test keywords, but it's ok;
; Common Lisp declares that the leftmost value is the one that counts ;)
(define (find-all item seq &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
        (if test-not
                (apply #'remove item seq
                        :test-not (complement test-not) keyword-args)
                (apply #'remove item seq
                        :test (complement test) keyword-args) ) )

(define *state* null) ; "The current state: a list of conditions."
(define *ops* null) ; "A list of available operators."

(struct op (action preconds add-list del-list )) ;"An operation"

  ;"General Problem Solver: achieve all goals using *ops*."
(define (GPS *state* goals *ops*)
  (when (andmap achieve goals) 'solved)) ; andmap replaces every

  ;"A goal is achieved if it already holds,
  ;or if there is an appropriate op for it that is applicable."
(define (achieve goal)
  (or (member goal *state*)
      (ormap apply-op ; ormap replaced some
            (find-all goal *ops* appropriate-p))))

(defun (find-all item seq &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
        (if test-not
                (apply remove item seq
                        :test-not (complement test-not) keyword-args)
                (apply remove item seq
                        :test (complement test) keyword-args) ) )

  ;"An op is appropriate to a goal if it is in its add list."
(define (appropriate-p goal op)
  (member goal (op-add-list op)))

(define (apply-op op)
  ;"Print a message and update *state* if op is applicable."
  (when (every #'achieve (op-preconds op))
    (print (list 'executing (op-action op)))
    (set! *state* (set-difference *state* (op-del-list op)))
    (set! *state* (union *state* (op-add-list op)))
    t))

;;; ==============================
;(struct op (action   preconds add-list del-list )) ;"An operation"
(define *school-ops*
  (list
    (make-op 'drive-son-to-school    '(son-at-home car-works) '(son-at-school) '(son-at-home))
    (make-op 'shop-installs-battery  '(car-needs-battery shop-knows-problem shop-has-money) '(car-works))
    (make-op 'tell-shop-problem      '(in-communication-with-shop) '(shop-knows-problem))
    (make-op 'telephone-shop         '(know-phone-number) '(in-communication-with-shop))
    (make-op 'look-up-number         '(have-phone-book) '(know-phone-number))
    (make-op 'give-shop-money        '(have-money) '(shop-has-money) '(have-money))))
