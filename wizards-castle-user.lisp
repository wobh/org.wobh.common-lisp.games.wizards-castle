;; -*- mode: lisp; coding: utf-8 -*-

(defpackage #:org.wobh.common-lisp.games.wizards-castle-user
  (:use #:common-lisp)
  (:nicknames #:wizards-castle-user #:wizard-user #:zot-user)
  (:local-nicknames (#:wizards-castle
                     #:org.wobh.common-lisp.games.wizards-castle))
  (:import-from #:wizards-castle
		;; printing
		#:*wiz-out*
		;; arrays
                #:array-index-row-major
		;; castles
                #:castle
                #:setup-castle
                #:make-castle
                #:cas-rooms
		#:entrance
                ;; adventurers
                #:adventurer
                #:make-adventurer
                #:random-sex
                #:female #:male
                #:hobbit #:elf #:human #:dwarf
                #:lethargy #:leech #:forget
                #:ruby-red #:norn-stone #:pale-pearl #:opal-eye
                #:green-gem #:blue-flame #:palantir #:silmaril
                ;; castle-position
                ;; castle-subscripts
		;; castle-scry
		#:make-message-creature-at
                ;; map-all-rooms
                #:adv-map-room
                #:get-castle-creature
                ;; setup-test
                #:join-history
                #:adv-enters-castle
                ;; play-test
                #:*forgetfulness*
                #:*curse-notify*
                #:*cas-coords*
                #:*gaze-mapper*
		#:*that-sleep-of-death*
                #:launch
                #:main
                ;; test-eval
                #:cas-history
                #:make-history
                #:record-event
                #:join-history
                #:wiz-format
                #:*wiz-out*
                #:make-wiz-form
                #:adv-finds-creature
                #:latest-event-p
                #:adv-entered-room
                #:adv-enters-room
                #:begin-turn
                #:events-since
                #:oldest-event)
  (:export #:make-test-adv
	   #:castle-position
	   #:castle-subscripts
	   #:castle-scry
           #:how-convenient
           #:map-all-rooms
           #:setup-test
           #:play-test
           #:test-eval)
  (:documentation "ORG.WOBH.COMMON-LISP.GAMES.WIZARDS-CASTLE-USER

Test Environment"))

(in-package #:org.wobh.common-lisp.games.wizards-castle-user)

(defvar *r* (make-random-state t)
  "Reusable random state for test environment.")

(defvar *a* nil
  "Test adventurer.")

(defvar *z* nil
  "Test castle (may or may not contain adventurer).")

(defparameter *adventurers*
  (list
   :basic       (lambda ()
		  (list :rc 'human
			:sx (random-sex (make-random-state t))
			;; NOTE: the randomly chosen sex for the
			;; default adventurer uses a random state
			;; independant from *R*.
			:st 11 :iq 10  :dx 11
			:wv  2 :av  2  :ah 14
			:gp  0 :lf  t  :fl  0))
   :blind-adept '(:rc human  :sx female
                  :st 18 :iq 12  :dx 18
                  :wv  3 :av  3  :ah 21
                  :gp 20 :lf nil :fl  0
                  :bl  t)
   :bookworm    '(:rc hobbit :sx male
                  :st  6 :iq 18  :dx 18
                  :gp 20 :lf  t  :fl  0
                  :bf  t)
   :valkyrie    '(:rc dwarf  :sx female
                  :st 16 :iq 14  :dx  8
                  :wv  2 :av  3  :ah 21
                  :gp 10 :lf nil :fl 10)
   :barbarian   '(:rc human  :sx male
                  :st 18 :iq  6  :dx 12
                  :wv  3 :av  1  :ah  7
                  :gp  0 :lf nil :fl 10
                  :cr (forget))
   :sorceress   '(:rc elf    :sx female
                  :st  6 :iq 18  :dx 12
                  :wv  1 :av  1  :ah  7
                  :gp  0 :lf  t  :fl 99
                  :rf  t
                  :cr (lethargy))
   :tourist     '(:rc human  :sx male
                  :st  6 :iq 10  :dx 8
                  :gp 6000
                  :cr (leech)))
  "Test adventurers")

(defun get-adventurer-params (adv-name)
  (let ((adv-params (getf *adventurers* adv-name)))
    (etypecase adv-params
      (function (funcall adv-params))
      (cons     adv-params))))

(defun make-test-adv (adv-name)
  "Make one of several pre-generated characters."
  (apply #'make-adventurer
	 (get-adventurer-params adv-name)))

(defun castle-position (item &key (castle *z*))
  "Returns first row-major-index where `item' can be found."
  (loop
    with rooms = (cas-rooms castle)
    with room-count = (array-total-size rooms)
    with match = nil
    for index from 0 below room-count
    for maybe = (row-major-aref rooms index)
    do (when (eql item maybe)
	 (setf match t)
	 (loop-finish))
    finally
       (return
	 (when match index))))

(defun castle-subscripts (item &key (castle *z*))
  "Returns first array subscripts where `item' can be found."
  (array-index-row-major castle (castle-position item :castle castle)))

(defun castle-scry (room-ref &key (castle *z*))
  "Prints the message as if adventurer were using a crystal orb to gaze into a room."
  (let ((coords (etypecase room-ref
		  (integer (array-index-row-major (cas-rooms castle) room-ref))
		  (cons room-ref))))
    (format nil
	    "You see ~@?"
	    (make-message-creature-at nil
				      (get-castle-creature castle room-ref)
				      coords))))

(defun castle-room-swap (room-ref-this room-ref-that &key (castle *z*))
  "Swap locations of two creatures in castle."
  (with-accessors ((rooms cas-rooms)) castle
    (let ((index-this (etypecase room-ref-this
			(integer room-ref-this)
			(cons (apply #'array-row-major-index rooms room-ref-this))))
	  (index-that (etypecase room-ref-that
		      (integer room-ref-that)
		      (cons (apply #'array-row-major-index rooms room-ref-that)))))
      (rotatef (row-major-aref rooms index-this)
               (row-major-aref rooms index-that))
      castle)))

(defun how-convenient (item &key (castle *z*))
  "Moves item east of the entrance."
  (castle-room-swap (castle-position item :castle castle)
		    (1+ (castle-position 'entrance :castle castle))
		    :castle castle))

(defun map-all-rooms (&key (adv *a*) (castle *z*))
  "Maps all the rooms in a castle."
  (declare (type castle castle)
           (type adventurer adv))
  (loop
     for ridx from 0 below (array-total-size (cas-rooms castle))
     do (adv-map-room adv ridx
                      (get-castle-creature castle ridx))))

(defun setup-test (&key (adv-name :basic) map-all-rooms enter-castle)
  "Set or reset test environment."
  (let ((*random-state* (make-random-state *r*))
	(*wiz-out* nil))
    (setf *z* (setup-castle t))
    (setf *a* (make-test-adv adv-name))
    (when map-all-rooms
      (map-all-rooms :adv *a* :castle *z*))
    (when (and *a* enter-castle)
      (join-history (cas-history *z*)
                    (adv-enters-castle *z* *a*)))
    (values *a* *z*)))

;;;; [wc 2013-01-31] TODO: come up with an error handler that does
;;;; something useful for reporting problems for play testers.

(defun play-test (&key (adventurer *a*) (castle *z*) (last-castle t)
               (forget-type *forgetfulness*)
               (curse-notify *curse-notify*)
               (gaze-map *gaze-mapper*)
               (cas-coords *cas-coords*)
               (sleep-of-death 1)
               ;; (play-again *play-again*)
               (random-state (make-random-state *r*)))
  "Run a test game."
  (let* ((*random-state* random-state)
         (*forgetfulness* forget-type)
         (*curse-notify* curse-notify)
         (*cas-coords* cas-coords)
         (*gaze-mapper* gaze-map)
         (*that-sleep-of-death* sleep-of-death)
         ;; (*play-again* play-again)
         ;; (*wiz-intro* intro)
         ;; (*wiz-help* help)
         )
    (launch)
    (main :castle castle :adventurer adventurer)
    (when last-castle castle)))

(defun test-eval (wiz-form &key (castle *z*) (history 'castle))
  "Evaluate wiz-form for testing."
  (loop
    with history = (if (eq history 'castle)
                       (cas-history castle)
                       (make-history))
    with turn = (make-history)
    do
       (multiple-value-bind (events message)
           (apply (first wiz-form) castle (rest wiz-form))
         (record-event turn (oldest-event events))
         (join-history history events)
         (when message
           (wiz-format *wiz-out* message)))
       (cond ((eq (first wiz-form) 'adv-enters-room)
              (setf wiz-form (make-wiz-form 'adv-finds-creature)))
             ((latest-event-p 'adv-entered-room history)
              (setf wiz-form (make-wiz-form 'adv-enters-room)))
             (t (setf wiz-form nil)))
    until (null wiz-form)
    finally (return
              (values
               (begin-turn castle)
               (events-since (oldest-event turn) history)))))
