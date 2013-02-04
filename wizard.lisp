;;;; -*- Mode:lisp;coding:utf-8 -*-
;;;; FILE: wizard.lisp

;;;; DESCRIPTION:

;;;; Wizard's Castle

;;;; Copyright (C) 1980 Joseph Power
;;;; Last revised - 4/12/80 11:10 PM"

;;;; Adapted to Common Lisp by William Clifford

;;;; AUTHORS:

;;;; William Clifford [wc] wobh@yahoo.com

;;;; NOTES:

;;;; References

;;;; Power, Joseph R.; Wizard's Castle; Recreational Computing; 1980,
;;;; July-August pgs 10-17

;;;; O'Hare, John; Wizard's Castle; Baf's guide to the Interactive
;;;; Fiction Archive; http://www.wurb.com/if/index; page:
;;;; http://www.wurb.com/if/game/678

;;;; Stetson, J.F.; Wizard's Castle; Baf's guide to the Interactive
;;;; Fiction Archive; http://www.wurb.com/if/index; page:
;;;; http://www.wurb.com/if/game/678

;;;; Licht, Derell; Wizard's Castle;
;;;; http://home.comcast.net/~derelict/winwiz.html

;;;; Interview with Joseph Power:
;;;; http://www.armchairarcade.com/neo/node/1381 

(defpackage "WIZARDS-CASTLE"
  (:nicknames "WIZARD" "ZOT")
  (:use "CL")
  (:export "MAIN" "SETUP-ADVENTURER" "SETUP-CASTLE")
  (:export "TEST" "MAKE-TEST-ADV" "SETUP-TEST")
  (:export "*R*" "*A*" "*Z*" )
  (:documentation "Joseph Power's _Wizard's Castle_"))

(in-package "WIZARDS-CASTLE")


;;;; Randomess functions

;;; Powers, 1980, pg 14, ln# 70
;;; DEFFNA(Q)=1+INT(RND(8)*Q)

(defun random-array-subscripts (array &optional (random-state *random-state*))
  "Create a list random subscripts in array."
  (mapcar (lambda (n) (random n random-state))
	  (array-dimensions array)))

(defun random-aref (array &optional (random-state *random-state*))
  "Get random element of array."
  (apply #'aref array (random-array-subscripts array random-state)))

(defun random-elt (seq &optional (random-state *random-state*))
  "Get random element from sequence."
  (elt seq (random (length seq) random-state)))

(defun random-range (limit &optional (limit-max Nil) (random-state *random-state*))
  "Get a random number in range inclusive."
  (if limit-max
      (+ limit (random (- (1+ limit-max) limit) random-state))
      (random (1+ limit) random-state)))

(defun shuffle (seq &optional (random-state *random-state*))
  "Knuth shuffle a sequence."
  (let ((len (length seq)))
    (dotimes (i len seq)
      (rotatef (elt seq i)
	       (elt seq (+ i (random (- len i) random-state)))))))

;;; Knuth shuffle
;;; https://groups.google.com/d/topic/comp.lang.lisp/1ZtO84hrAuM/discussion




;;;; Castle accessors

;;; Powers, 1980, pg 13
;;; "FNB(Q) = Q + 8 * ((Q = 9) - (Q = 0)) <- causes wraparound at borders"

;;; Powers, 1980, pg 14, ln# 70
;;; DEFFNB(Q)=Q+8((Q=9)-(Q=0))

;;; Powers describes Zot's castle as an 8x8x8 manifold, like 8 nested
;;; donuts ("torus") in which falling from the inmost means landing on
;;; the outermost (1980, pg 11). To affect this, here are some array
;;; access functions that apply a modulus to their subscripts.

(defun mrray-in-bounds-p (array &rest subscripts)
  "Check that number of subscripts match array rank."
  (= (length subscripts) (array-rank array)))

(defun mref (array &rest subscripts)
  "Access array with modulus on subscripts for manifold-like access."
  (when (apply #'mrray-in-bounds-p array subscripts)
    (apply #'aref array
	   (map-into subscripts #'mod subscripts
		     (array-dimensions array)))))

(defun set-mref (array subscripts value)
  "Set array element with modulus applied to subscripts."
  (when (apply #'mrray-in-bounds-p array subscripts)
    (setf (apply #'aref array
		 (map-into subscripts #'mod subscripts
			   (array-dimensions array))) value))
  ;; FIXME: (maybe) I'm using MAP-INTO instead of MAPCAR so that it
  ;; will give an error when given too many subscripts as well as too
  ;; few.
  )

(defsetf mref set-mref)


(defun row-major-mref (array index)
  "Access an array from an index modulated to the array's size."
  (row-major-aref array (mod index (array-total-size array))))

(defun mrray-row-major-index (array &rest subscripts)
  "Make an index for array from modulated subscripts."
  (when (apply #'mrray-in-bounds-p array subscripts)
    (apply #'array-row-major-index array
	   (map-into subscripts #'mod subscripts (array-dimensions array)))))


;;; Powers, 1980, pg 15, ln# 50: 
;;; DEFFND(Q)=Q*64+X*8+Y-585

;;; Powers, 1980, pg 13:
;;; "computes room location in memory [...] After 32767, memory
;;; locations (for POKE and PEEK commands are numbered -32768 (8000
;;; hex) to -1 (FFFF hex)."

;;; Actually we might need the opposite. David D. Smith's third post
;;; in "iteration over multidimensional array" is a function that
;;; turns an index into array subscripts.

;;; http://groups.google.com/group/comp.lang.lisp/msg/bca19f4a3d0a5e3e

(defun array-index-row-major (array index)
  "Turn a row-major-index back into subcripts for the array."
  (row-major-aref array index) ; trigger error if index out of range
  (reduce
   #'(lambda (dim x)
       (nconc
	(multiple-value-list (truncate (car x) dim))
	(cdr x)))
   (cdr (array-dimensions array)) :initial-value (list index) :from-end T))

;;; array filter

(defun filter-array-indices (predicate an-array)
  "Make a list of array indices which elements pass some test"
  (loop
     for index from 0 below (array-total-size an-array)
     when (funcall predicate (row-major-aref an-array index))
     collect index))


;;;; Directions and vectors in Castle Zot

(defconstant +directions+
  '((down  (1 0 0))
    (up    (-1 0 0))
    (south (0 1 0))
    (north (0 -1 0))
    (east  (0 0 1))
    (west  (0 0 -1))))

(defun direction-p (symbol)
  (find symbol +directions+ :key #'first))

(defun vector-of-direction (direction)
  (assert (direction-p direction))
  (second (find direction +directions+ :key #'first)))

(defun map-manifold-vectors (function manifold &rest vectors)
  "Map function over manifold vectors, modulus dimensions of the manifold."
  (mapcar #'mod (apply #'mapcar function vectors)
	  (array-dimensions manifold)))

;;; The original coordinate system reversed what I would call the X
;;; and Y axes and also counted up from 1.

;;; I judge the interest of coherence outweighs fidelity in this
;;; case. We will count from zero and use "normal" x and y
;;; internally. We'll translate into zot coords for the user interface.

;;; NOTE: 
;;; (aref array-3d x y z)
;;; (array-row-major-index array-3d z y x)

(defparameter *cas-coords* 'zot
  "What style to display castle coordinates in.")

(defun wiz-coords (coords)
  "Arrange coordinates in the order and value expected."
  (ecase *cas-coords*
    (array (reverse coords))
    (zot (map 'list #'1+
	      (list (second coords) (third coords) (first coords))))))

(defun unwiz-coords (wizd-coords)
  "Arrange coordinates from original game order and values to internal order and values."
  (ecase *cas-coords*
    (array (reverse wizd-coords))
    (zot (map 'list #'1-
	      (list (third wizd-coords)
		    (first wizd-coords)
		    (second wizd-coords))))))

     

;;;; TODO mimic display output?

;;;  - http://oldcomputers.net/ 
;;;  - http://www.old-computers.com
;;;  - http://www.computer-museum.nl

;;; | make      | model    | ch-width | ch-height | px-width | px-height |
;;; |-----------+----------+----------+-----------+----------+-----------|
;;; | Exidy     | Sorcerer |       64 |  30 (32?) |      512 |       240 |
;;; | Commodore | Pet      |       40 |        25 |          |           |
;;; | Apple     | II       |       40 |        24 |      280 |       192 |
;;; | Commodore | Vic-20   |       22 |        23 |      176 |       184 |
;;; | Commodore | 64       |       40 |        25 |          |           |
;;; | MS        | DOS      |       80 |        25 |          |           |


;; (defstruct console text-width text-height)

;; (defparameter *consoles*
;;   (list
;;    :exidy-sorcerer (make-console :text-width 64 :text-height 30)
;;    :commodore-pet (make-console :text-width 40 :text-height 25)
;;    :microsoft-dos (make-console :text-width 80 :text-height 25)))

;; Some sources say the Exidy Sorcerer had a text-height of 32.

;; (defun get-console (computer)
;;   (getf *consoles* (intern (symbol-name computer) 'keyword)))

;; (defparameter *platform* (get-console 'exidy-sorcerer))


;;;; Input and output

(defconstant +all-caps+
  "~:@(~@?~)"
  "Format string for WIZ-FORMAT when ZOT SPEAKS IN ALL CAPS")

(defconstant +mixed-case+
  "~@?"
  "Format string for WIZ-FORMAT when Zot speaks normally")

(defparameter *wiz-format-string*
  +all-caps+
  ;; +mixed-case+    ; for a less obnoxious Zot
  "Format string for WIZ-FORMAT.")

;;; FIXME: feels a bit like reinventing the wheel here

(defparameter *wiz-width* 64)
(defparameter *wiz-out* *standard-output*)
(defparameter *wiz-err* *query-io*)
(defparameter *wiz-qio* *query-io*)

(defun wiz-format (stream str &rest args)
  "Format a string for output in wizard's castle."
  (apply #'format stream *wiz-format-string* str args))

(defun wiz-write-line (string &key (stream *wiz-out*) (start 0) end)
  "Write a line in Wizard's Castle."
  (write-line (wiz-format Nil string) stream :start start :end end))

(defun wiz-write-string (string &key (stream *wiz-out*) (start 0) end)
  "Write a string in Wizard's Castle." 
  (write-string (wiz-format Nil string) stream :start start :end end))

(defun wiz-format-error (stream string &rest args)
  "Write a formatted error message to STREAM."
  (wiz-format stream "~%** ~?" string args))

(defun wiz-error (string &rest args)
  "Writes wiz-formatted error message to *WIZ-ERR*, returns Nil."
  (apply #'wiz-format-error *wiz-err* string args)
  (finish-output *wiz-err*))

(defun wiz-prompt (string &rest args)
  "Write a prompt."
  (terpri *wiz-qio*)
  (wiz-write-string (wiz-format Nil string args)))

(defun wiz-read-line (&optional (stream *wiz-qio*))
  "Read a line of player input."
  (read-line stream))
				      
(defun wiz-read-char (&optional (stream *wiz-qio*))
  "Read the upcased first character from whatever entered."
  (char-upcase (read-char stream)))

(defun wiz-read-n (&optional (stream *wiz-qio*))
  "Read a number from whatever entered."
  (parse-integer (read-line stream) :junk-allowed t))

(defun wiz-read-coord (axis)
  "Read a coordinate."
  (let ((coord (wiz-read-n)))
    (list
     (intern (symbol-name axis) 'keyword)
     (ecase *cas-coords*
       (zot   (1- coord))
       (array coord)))))

(defun make-prompt-adv-choice (&optional string)
  "Make a prompt with 'Your choice' at the end."
  (let ((prompt "Your choice "))
    (if string
	(wiz-format Nil "~A~%~&~A" string prompt)
	(wiz-format Nil "~&~A" prompt))))

(defmacro with-player-input
    ((var prompt &key
	  (readf #'wiz-read-char) (istream *wiz-qio*)
	  (writef #'wiz-prompt)   (ostream *wiz-qio*))
     &body body)
  "Read input to var and do something with it or set var to Nil if a
different input is needed."
  (let ((out (gensym "OUTCOME")))
    `(loop
	with ,var = Nil
	with ,out = Nil
	do
	  (funcall ,writef ,prompt ,ostream)
	  (finish-output ,ostream)
	  (clear-input ,istream)
	  (setf ,var (funcall ,readf ,istream))
	  (setf ,out ,@body)
	until (not (null ,var))
	finally (return ,out))))


(defun wiz-y-or-n-p (prompt &optional message)
  "Return T, Nil or requery if answer is Y, N, or something else."
  (when message
    (wiz-write-line message))
  (with-player-input
      (input prompt :readf #'wiz-read-char)
    (case input
      (#\Y T)
      (#\N Nil)
      (T (setf input (wiz-error "Answer yes or no "))))))

(defun wiz-read-direction (prompt &optional input-error-message)
  "Read a direction from the player.

INPUT-ERROR-MESSAGE provides an error message and loops, otherwise, it
returns INPUT-ERROR."
  (with-player-input (direction prompt)
    (case direction
      (#\N 'north)
      (#\E 'east)
      (#\W 'west)
      (#\S 'south)
      (T (if input-error-message
	     (setf direction (wiz-error input-error-message))
	     'input-error)))))


;;; ASCII controls used in code:

;;; | DEC | CHR | Description     | character | Format  |
;;; |-----+-----+-----------------+-----------+---------|
;;; | 007 | BEL | Bell            |           |         |
;;; | 010 | NL  | Newline         | #\Newline | ~&      |
;;; | 012 | FF  | Formfeed        | #\Page    | ~|      |
;;; | 013 | CR  | Carriage Return | #\Return  | ~%      |


;;;; Events, Turns, and History

;;; Events

(defparameter *events*
  '(adv-ate
    adv-entered-castle
    adv-entered-room adv-found
    adv-mapped adv-viewed-map
    adv-walked adv-teleported
    adv-used adv-tried adv-opened adv-drank
    adv-warped adv-fell adv-staggered 
    adv-attacked adv-wounded adv-cast-spell adv-bribed adv-retreated
    foe-attacked foe-wounded foe-bound foe-slain foe-bribed foe-unbound
    adv-strike-hit adv-strike-missed
    adv-blinded adv-bound
    adv-cured adv-unbound
    adv-bought adv-sold adv-ignored
    adv-dozed adv-forgot
    adv-gained adv-lost
    adv-armor-damaged adv-weapon-broke adv-armor-destroyed
    adv-changed-race adv-changed-sex
    chest-expoded
    adv-left-castle
    adv-slain
    player-quit-game
    player-error)
  "List of events")

(defun event-p (obj)
  "Verify that the object is an event."
  (find 
   (typecase obj
     (list   (first obj))
     (T obj))
   *events*))

(defun make-event (&rest args)
  "An event is a list describing what happened."
  (assert (find (first args) *events*))
  args)

(defun make-event* (&rest args)
  (assert (find (first args) *events*))
  (apply #'list* args))

(defun event-type-of (event)
  "Get event type info."
  (assert (event-p event))
  (first event))

(defun event-subtype-of (event &optional degree)
  "Get event subtype information."
  (assert (event-p event))
  (let ((subtype (rest event)))
    (etypecase degree
      (null subtype)
      (symbol (member degree subtype))
      ((integer 0) (subseq subtype degree))
      )))

(defun event-type-p (event check)
  "Check that an event has a particular properties."
  (assert (event-p event))
  (etypecase check
    (symbol (find check event))
    (list   (search check event))))

(defun event-property (event &optional degree)
  "Get most specific information about event"
  (assert (event-p event))
  (etypecase degree
    (null (first (last event)))
    (symbol (rest (member degree event)))
    ((integer 0) (last event degree))))


;;; History

(defun all-events-p (events-list)
  "Is every element of EVENTS-LIST an event?"
  (every #'event-p events-list))

(defun make-history (&rest events)
  "A history is push-down stack of events (returns reversed list of events)."
  (assert (or Nil (all-events-p events)))
  (reverse events))

(define-modify-macro join-histories (new-history)
  (lambda (old-history new-history)
    (setf old-history (concatenate 'list new-history old-history)))
  "Combine two histories")

(define-modify-macro record-event (event)
  (lambda (history event)
    (assert (event-p event))
    (push event history))
  "Record an event to history.")

(define-modify-macro record-events (&rest events)
  (lambda (history &rest events)
    (setf history (revappend events history)))
  "Record events to history.")

(defun count-events (history)
  "Count the number of turns in history." 
  (count-if #'event-p history :key 'first))

(defun events-since (event history)
  "History since event."
  (assert (event-p event))
  (let* ((test (etypecase event
		 (symbol #'equal)
		 (list   #'event-type-p)))
	 (last (position event history :key #'event-type-of :test test)))
    (when (typep last '(integer 0))
      (subseq history 0 (1+ last)))))

(defun find-event (event history)
  "Search history for event."
  (find-if (lambda (this-event) (event-type-p this-event event))
	 history))

(defun latest-event (history)
  "Last event in history."
  (first history))

(defun oldest-event (history)
  "First event in history."
  (first (last history)))


;;; Turns (a special kind of event)

(defparameter *turn-events*
  '(adv-drank adv-walked adv-used adv-opened adv-viewed-map adv-dozed)
  "List of events counted as turns.")

(defun turn-p (obj)
  "Is the given event a turn?"
  (assert (event-p obj))
  (find
   (etypecase obj
     (symbol obj)
     (list   (first obj)))
   *turn-events*))

(defun count-turns (history)
  "Count the number of turns in history." 
  (count-if #'turn-p history :key 'first))


;;;; Messages

(defun make-text (&rest strings)
  (apply #'concatenate 'string strings))

(define-modify-macro push-text (text)
  (lambda (message text)
    (setf message (make-text message text)))
  "Push a text onto a message.")


;;;; Show title screen

(defconstant +intro-text-dos+
  (format Nil
	  "~2&Many cycles ago, in the kingdom of N'DIC, the gnomic~%~
              wizard ZOT forged his great ORB of power. He soon vanished~%~
              utterly, leaving behind his vast subterranean castle~%~
              filled with esurient MONSTERS, fabulous TREASURES, and~%~
              the incredible ORB of ZOT. From that time hence, many~%~
              a bold youth has ventured into the WIZARD'S CASTLE. As~%~
              of yet, NONE has ever emerged victoriously! BEWARE!!~2%")
  "This intro is a slightly modified version of the article's introduction.")

(defparameter *wiz-intro* Nil
  "Original game does not print an into like some later ones.")

(defun launch (&optional intro)
  (write-line (string #\page))
  (write-line "Creating Arrays")
  (when intro
    (wiz-write-line intro)))


(defun make-message-title (&optional (width *wiz-width*))
  "Print title screen"
  (let ((stars (make-string width :initial-element #\*))
	(title "THE WIZARD'S CASTLE"))
    (with-output-to-string (message)
      (format message "~|")
      (format message "~2&~A~%~%" stars)
      (format message "~&~VT~A~%"
	      (1- (floor (- width (length title)) 2)) title)
      (format message "~2&~A~%"  stars)
      (format message "~2&~A"
	      "Copyright 1980 (C) 1980 by Joseph R Power")
      (format message "~2&~A~%~%"  "Last Revised - 04/12/80"))))

;; FIXME what should I do about form-feed (CHR$(12)? clear screen CLS?
;; Seems likely the revision date here means 1980-04-12

;;; TODO: filter list by contents of another list



;;;; Zot's creatures

;;; the documentation strings are from (Power 1980), pg 13 

;;; FIXME: perhaps some of the parameters should be constants?

;;; FIXME: the first item on these lists is Nil because the original
;;; basic source code used natural number indexes. To maintain
;;; consistency with the code, and prevent confusion when checking
;;; against it, the zeroth element has been set to Nil.


;; |  n | symbol      | icon| text                | type     |
;; |----|-------------+-----+---------------------|----------|
;; |  0 | Nil         | Nil | Nil                 | Nil      |
;; |  1 | empty-room  | #\. | "an empty room"     | empty    |
;; |  2 | entrance    | #\e | "the entrance"      | entrance |
;; |  3 | stairs-up   | #\u | "stairs going up"   | stairs   |
;; |  4 | stairs-down | #\d | "stairs going down" | stairs   |
;; |  5 | pool        | #\p | "a pool"            | room     |
;; |  6 | chest       | #\c | "a chest"           | room     |
;; |  7 | gold        | #\g | "gold pieces"       | room     |
;; |  8 | flares      | #\f | "flares"            | room     |
;; |  9 | warp        | #\w | "a warp"            | room     |
;; | 10 | sinkhole    | #\s | "a sinkhole"        | room     |
;; | 11 | crystal-orb | #\o | "a crystal orb"     | room     |
;; | 12 | book        | #\b | "a book"            | room     |
;; | 13 | kobold      | #\m | "a kobold"          | monster  |
;; | 14 | orc         | #\m | "an orc"            | monster  |
;; | 15 | wolf        | #\m | "a wolf"            | monster  |
;; | 16 | goblin      | #\m | "a goblin"          | monster  |
;; | 17 | ogre        | #\m | "an ogre"           | monster  |
;; | 18 | troll       | #\m | "a troll"           | monster  |
;; | 19 | bear        | #\m | "a bear"            | monster  |
;; | 20 | minotaur    | #\m | "a minotaur"        | monster  |
;; | 21 | gargoyle    | #\m | "a gargoyle"        | monster  |
;; | 22 | chimera     | #\m | "a chimera"         | monster  |
;; | 23 | balrog      | #\m | "a balrog"          | monster  |
;; | 24 | dragon      | #\m | "a dragon"          | monster  |
;; | 25 | vendor      | #\v | "a vendor"          | vendor   |
;; | 26 | ruby-red    | #\t | "the ruby red"      | treasure |
;; | 27 | norn-stone  | #\t | "the norn stone"    | treasure |
;; | 28 | pale-pearl  | #\t | "the pale pearl"    | treasure |
;; | 29 | opal-eye    | #\t | "the opal eye"      | treasure |
;; | 30 | green-gem   | #\t | "the green gem"     | treasure |
;; | 31 | blue-flame  | #\t | "the blue flame"    | treasure |
;; | 32 | palantir    | #\t | "the palantir"      | treasure |
;; | 33 | silmaril    | #\t | "the silmaril"      | treasure |
;; | 34 | x           | #\? | "x"                 | ?        |

(defvar *creature-data*
  '((x		 "x"                 #\?)
    (empty-room  "an empty room"     #\.)
    (entrance 	 "the entrance"      #\e)
    (stairs-up	 "stairs going up"   #\u)
    (stairs-down "stairs going down" #\d)
    (pool        "a pool"            #\p)
    (chest       "a chest"	     #\c)
    (gold-pieces "gold pieces"	     #\g)
    (flares      "flares"	     #\f)
    (warp 	 "a warp" 	     #\w)
    (sinkhole	 "a sinkhole"	     #\s)
    (crystal-orb "a crystal orb"     #\o)
    (book 	 "a book" 	     #\b)
    (kobold	 "a kobold"	     #\m)
    (orc         "an orc"	     #\m)
    (wolf	 "a wolf"	     #\m)
    (goblin	 "a goblin"	     #\m)
    (ogre 	 "an ogre" 	     #\m)
    (troll	 "a troll"	     #\m)
    (bear	 "a bear"	     #\m)
    (minotaur	 "a minotaur"	     #\m)
    (gargoyle 	 "a gargoyle" 	     #\m)
    (chimera	 "a chimera"	     #\m)
    (balrog	 "a balrog"	     #\m)
    (dragon	 "a dragon"	     #\m)
    (vendor	 "a vendor"	     #\v)
    (ruby-red	 "the Ruby Red"	     #\t) 
    (norn-stone	 "the Norn Stone"    #\t) 
    (pale-pearl  "the Pale Pearl"    #\t)
    (opal-eye	 "the Opal Eye"	     #\t)
    (green-gem	 "the Green Gem"     #\t)
    (blue-flame  "the Blue Flame"    #\t)
    (palantir	 "the Palantir"	     #\t)
    (silmaril	 "the Silmaril"	     #\t)
    (runestaff   "the Runestaff"     Nil)
    (orb-of-zot  "the Orb of Zot"    Nil)
    )
  "All the possible castle contents")

(defun creature-p (creature)
  "Is the given symbol a creature?"
  (assert (typep creature 'symbol))
  (find creature (subseq *creature-data* 1 34) :key 'first))

(defun get-creature-data (creature-ref &optional data-type)
  "Return the requested data about the creature."
  (let ((creature
	 (etypecase creature-ref
	   (symbol (find creature-ref *creature-data* :key 'first))
	   ((integer 1 33) (elt *creature-data* creature-ref))
	   (string (find creature-ref *creature-data* :key 'second
			 :test 'equal)))))
    (if data-type
	(ecase data-type
	  (list creature)
	  (number (position creature *creature-data*)) 
	  (symbol (first creature))
	  (string (second creature))
	  (character (third creature)))
	creature)))

(defun get-creature-name (creature-ref)
  "Get the creature symbol."
  (get-creature-data creature-ref 'symbol))

(defun get-creature-code (creature-ref)
  "Get the creature number."
  (get-creature-data creature-ref 'number))

(defun get-creature-text (creature-ref)
  "Get the creature text."
  (get-creature-data creature-ref 'string))

(defun get-creature-icon (creature-ref)
  "Get the creature map icon."
  (string (get-creature-data creature-ref 'character)))

(defun get-unmapped-icon ()
  "Get the icon for a unmapped room."
  (string (third (first *creature-data*))))


;;;; Locations and creatures in castle Zot.

(defconstant +zot-castle-dimensions+ '(8 8 8)
  "Dimensions of castle.")

(defun make-castle-rooms ()
  "Make an array for storing castle room data."
  (make-array +zot-castle-dimensions+
	      :element-type 'symbol :initial-element 'empty-room))

(defun castle-height (castle-rooms)
  "How tall is the castle?"
  (first (last (array-dimensions castle-rooms) 3)))

(defun castle-level-dimensions (castle-rooms)
  "What are the dimensions of the castle levels?"
  (last (array-dimensions castle-rooms) 2))

(defun calc-castle-level-offset (level castle-rooms)
  "Calculate the index offset for castle-levels"
  (reduce #'* (list* level (castle-level-dimensions castle-rooms))))

;;; FIXME [wc 2012-12-26] It seems like it would be good to prepare
;;; for n-d castles, but it's a little over-generalized and none of
;;; the code following really supposes this.

(defun make-castle-level (castle-rooms level)
  "Return a displaced array of a castle level."
  (make-array (castle-level-dimensions castle-rooms)
	      :displaced-to castle-rooms
	      :displaced-index-offset
	      (calc-castle-level-offset level castle-rooms)))

(defun make-castle-levels (castle-rooms)
  "Make a list of displaced arrays to the different castle floors."
  (loop
     for level from 0 below (castle-height castle-rooms)
     collect
       (make-castle-level castle-rooms level)))

(defun make-castle-map ()
  "Make an array for storing castle map data."
  (make-array +zot-castle-dimensions+
	      :element-type 'string
	      :initial-element (get-unmapped-icon)))

(defun add-castle-vectors (castle-rooms &rest vectors)
  "Vectors in Zot's castle must add with modulus of array-dimensions."
  (apply #'map-manifold-vectors #'+ castle-rooms vectors))

(defun subtract-castle-vectors (castle-rooms &rest vectors)
  "Vectors in Zot's castle must subtract with modulus of array-dimensions."
  (map-manifold-vectors #'- castle-rooms vectors))

;;;; Room and creature types


(defparameter *rooms*
  '(pool chest gold-pieces flares warp sinkhole crystal-orb book)
  "List of the room creature types in the castle.")

(defun room-p (creature)
  "Is this creature a room?"
  (assert (typep creature 'symbol))
  (find creature *rooms*))

(defparameter *monsters*
  '(kobold orc wolf goblin ogre troll bear
    minotaur gargoyle chimera balrog dragon)
  "List of the monster creature types in the castle.")

(defun monster-p (creature)
  "Is this creature a monster?"
  (assert (typep creature 'symbol))
  (find creature *monsters*))

(defun random-monster ()
  "Return a random monster."
  (random-elt *monsters*))

(defun enemy-p (creature)
  "Is this creature an enemy?"
  (assert (typep creature 'symbol))
  (find creature (append *monsters* '(vendor)))
  ;; FIXME: The castle can tell us if the vendor is an enemy or
  ;; not. This may just represent whether the creature is attackable.
  )

(defun get-enemy-text (enemy)
  "Return the enemy text."
  (assert (enemy-p enemy))
  (get-creature-text enemy))

;;1790 a=peek(fnd(z))-12:wc=0:if (a<13)or (vf=1)then2300
;;2300 q1=1+int(a/2):q2=a+2:q3=1

(defun calc-enemy-value (enemy)
  ;; FIXME: creature-battle-skill, creature-combat value?
  "Return the enemy value."
  (assert (enemy-p enemy))
  (- (get-creature-code enemy) 12))

(defun calc-enemy-hit-points (enemy)
  "Return the enemy hit points."
  (assert (enemy-p enemy))
  (+ 2 (calc-enemy-value enemy)))

(defun calc-enemy-damage (enemy)
  "Return the enemy hit points."
  (assert (enemy-p enemy))
  (1+ (floor (calc-enemy-value enemy) 2)))

;;; TODO: vendors can have hp and dmg too


(defparameter *treasures*
  '(ruby-red norn-stone pale-pearl opal-eye
    green-gem blue-flame palantir silmaril)
  "List of the treasure creature types in the castle.")

(defun treasure-p (creature)
  "Is the creature a treasure."
  (assert (typep creature 'symbol))
  (member creature *treasures*))

(defun random-treasure ()
  "Return a random treasure."
  (random-elt *treasures*))

(defun get-treasure-index (treasure)
  "Return treasure index number."
  (assert (treasure-p treasure))
  (position treasure *treasures*))

(defun treasure-lessp (t1 &rest ts)
  "Is the index of the first treasure argument less than the rest of
treasure arguments."
  (apply #'<
	 (loop
	    for tr in (list* t1 ts)
	    collect (get-treasure-index tr))))

(defun sort-treasure-list (treasure-list)
  "Sort the given treasure list."
  (sort treasure-list #'treasure-lessp))

(defun creature-type-p (creature type)
  "Return T if the type of creature is valid"
  (cond ((eq type creature) T)
	((eq type 'monster) (monster-p creature))
	((eq type 'treasure) (treasure-p creature))
	((eq type 'room) (room-p creature))
	(T Nil)))

(defun creature-type-of (creature)
  "Return the type of creature given."
  (cond
    ((monster-p creature)
     'monster)
    ((treasure-p creature)
     'treasure)
    (T
     creature)))

(defparameter *eats*
  '("wich" " stew" " soup" " burger"
    " roast" " munchy" " taco" " pie")
  "Names of the eight recipes (orc tacos, etc)")

(defun random-eats ()
  "Return a random eat."
  (random-elt *eats*))

(defconstant +entrance+ '(0 0 3)
  "Coordinates of the entrance")


;;;; Adventurer attributes

(defun get-attr-data (attr-ref attr-data &optional data-type)
  "Get requested data for the attr."
  (let ((attr
	 (etypecase attr-ref
	   (symbol (find attr-ref attr-data :key 'first))
	   (string (find attr-ref attr-data :key 'second))
	   ((integer 1) (elt attr-data (1- attr-ref))))))
    ;; FIXME: can't I make an adventurer object a attr-ref?
    (if data-type
	(ecase data-type
	  (list attr)
	  (symbol (first attr))
	  (string (second attr))
	  (number (position attr attr-data)))
	attr)))

(defparameter *race-data*
  '((hobbit "hobbit")
    (elf    "elf")
    (human  "human")
    (dwarf  "dwarf"))
  "The four races")

(defun get-race-text (race-ref)
  (get-attr-data race-ref *race-data* 'string))

(defparameter *races* '(hobbit elf human dwarf))

(defun random-race (&optional (random-state *random-state*))
  (random-elt *races* random-state))

(defparameter *sex-data*
  '((female "female")
    (male   "male"))
  "The sexes of adventurers.")

(defun get-sex-text (sex-ref)
  (get-attr-data sex-ref *sex-data* 'string))

(defparameter *sexes* '(female male))

(defun random-sex (&optional (random-state *random-state*))
  (random-elt *sexes* random-state))

;;;; Adventurer equipment data

(defun get-equip-data (equip-ref equip-data &optional data-type)
  (let ((equip
	 (etypecase equip-ref
	   (symbol (find equip-ref equip-data :key 'first))
	   (string (find equip-ref equip-data :key 'second))
	   ((integer 0) (elt equip-data equip-ref)))))
    (if data-type
	(ecase data-type
	  (list equip)
	  (symbol (first equip))
	  (string (second equip))
	  (number (position equip equip-data)))
	equip)))

(defparameter *armor*
  '((no-armor  "nothing")
    (leather   "leather")
    (chainmail "chainmail")
    (plate     "plate"))
  "The four armor types")

(defun get-armor-value (armor-ref)
  (get-equip-data armor-ref *armor* 'number))

(defun get-armor-text (armor-ref)
  (get-equip-data armor-ref *armor* 'string))

(defun armor-p (item)
  "Is the item armor?"
  (find item *armor* :key 'first))

(defparameter *weapons*
  '((no-weapon "nothing")
    (dagger    "dagger")
    (mace      "mace")
    (sword     "sword"))
  "The four weapons")

(defun get-weapon-value (weapon-ref)
  (get-equip-data weapon-ref *weapons* 'number))

(defun get-weapon-text (weapon-ref)
  (get-equip-data weapon-ref *weapons* 'string))

(defun weapon-p (item)
  "Is the item a weapon?"
  (find item *weapons* :key 'first))

(defparameter *rankings*
  '((adv-st "strength")
    (adv-iq "intelligence")
    (adv-dx "dexterity"))
  "The adventurer's ranking attributes.")

(defun get-ranking-data (ranking-ref ranking-data &optional data-type)
  (let ((ranking
	 (etypecase ranking-ref
	   (symbol (find ranking-ref ranking-data :key 'first))
	   (string (find ranking-ref ranking-data :key 'second))
	   ((integer 0) (elt ranking-data ranking-ref)))))
    (if data-type
	(ecase data-type
	  (list ranking)
	  (symbol (first ranking))
	  (string (second ranking))
	  (number (position ranking ranking-data)))
	ranking)))
  
(defun get-ranking-text (ranking-ref)
  (get-ranking-data ranking-ref *rankings* 'string))

(defstruct (adventurer (:conc-name adv-))
  "A bold youth"
  (rc Nil)
  (sx Nil)
  (bf Nil)
  (rf Nil)
  (of Nil)
  (bl Nil)
  (st 2)
  (iq 8)
  (dx 14)
;;(ot 8) ; FIXME: not used after character creation
  (av 0) ; FIXME: symbol
  (ah 0) ; FIXME: property?
  (wv 0) ; FIXME: symbol
  (gp 60)
  (fl 0)
  (lf Nil)
  (fd 60)
  (tr #*00000000) ; FIXME: list, vector?
  (cr ())
  (tn ())
  (mp (make-castle-map)) ; FIXME: move definition of this function above
  )


;; - adv-rc: race
;; - adv-sx: sex
;; - adv-bf: book-stuck-to-hands flag (t = book stuck)
;; - adv-rf: runestaff possession flag (t = player owns it)
;; - adv-of: orb of zot possession flag (t = player owns it)
;; - adv-bl: blindness flag (t = player is blind)
;; - adv-st: current number of strength points
;; - adv-iq: current number of intelligence points
;; - adv-dx: current number of dexterity points
;; - adv-ot: amount of other points the player gets
;; - adv-av: number of points your armor absorbs per hit
;; - adv-ah: total number of hit points your armor has left
;; - adv-wv: number of points of damage your weapon does
;; - adv-gp: total number of gold pieces you possess
;; - adv-fl: total number of flares you possess
;; - adv-lf: lamp-owned flag (t = player owns it)	       
;; - adv-fd: last turn you ate on
;; - adv-tr: list of treasures you possess
;; - adv-cr: list of curses affecting you
;; - adv-tn: list of turns

(defun limit (test value limit1 &optional limit2)
  "Return value if test against limits succeeds otherwise return limit."
  (if limit2 
      (if (funcall test limit1 value) 
	  (if (funcall test value limit2) value limit2)
	  limit1)
      (if (funcall test value limit1) value limit1)))

(defun make-limiter (test limit1 &optional limit2)
  "Return a function which constrains value between limits."
  (lambda (value)
    (limit test value limit1 limit2)))

;;; Use only on ranked attributes, adv-st adv-dx adv-iq, with limits
;;; between 0 and 18

(defconstant +adv-rank-min+ 0)
(defconstant +adv-rank-max+ 18)
(defconstant +adv-rank-limiter+
  (make-limiter #'< +adv-rank-min+ +adv-rank-max+))

(define-modify-macro incf-adv-rank (&optional (delta 1))
  (lambda (place delta)
    (setf place (funcall +adv-rank-limiter+ (+ place delta))))
  "Adventurer rankings must stay within 0 and 18")

(define-modify-macro decf-adv-rank (&optional (delta 1))
  (lambda (place delta)
    (setf place (funcall +adv-rank-limiter+ (- place delta))))
  "Adventurer rankings must stay within 0 and 18")

(defun set-adv-rank (adv ranking rank)
  "Set an adventurer ranking to rank. Forces ranking to stay within
limits."
  ;; (assert (find ranking *rankings* :key 'first))
  (funcall (fdefinition (list 'setf ranking))
	   (funcall +adv-rank-limiter+ rank) adv))

(defun set-adv-rank-max (adv ranking)
  "Set an adventurer's ranking to ranking maximum."
  (set-adv-rank adv ranking +adv-rank-max+))

(defun set-adv-rank-min (adv ranking)
  "Set an adventurer's ranking to ranking minimum."
  (set-adv-rank adv ranking +adv-rank-min+))

;;; Adventurer inventories like adv-gp, adv-fl (also adv-ah, adv-fd).

(defconstant +adv-inv-limiter+
  (make-limiter #'> 0))

(define-modify-macro incf-adv-inv (&optional (delta 1))
  (lambda (place delta)
    (setf place (funcall +adv-inv-limiter+ (+ place delta))))
  "Adventurer inventories cannot fall below zero")

(define-modify-macro decf-adv-inv (&optional (delta 1))
  (lambda (place delta)
    (setf place (funcall +adv-inv-limiter+ (- place delta))))
  "Adventurer inventories cannot fall below zero")

;;; Adventurer attributes like race and sex.

(define-modify-macro random-change-adv-attr (alternates)
  (lambda (place alternates)
    (setf place 
	  (remove-if (lambda (alt-val) (eq place alt-val)) alternates)))
  "Change attribute to random selection of alternates")

(defun adv-alive-p (adv)
  "Is the adventurer alive?"
  (some #'(lambda (n) (< 0 n))
	(list (adv-st adv) (adv-iq adv) (adv-dx adv))))

;; 410 av=-3*(o$="p")-2*(o$="c")-(o$="l"):ifav>0440
;; ...
;; 440 ah=av*7:gp=gp-av*10:printchr$(12)

(defun wear-armor (adv armor)
  "The adventure puts on armor."
  (assert (armor-p armor))
  (let ((av (get-armor-value armor)))
    (setf (adv-av adv) av
	  (adv-ah adv) (* 7 av))
    armor))

;; 480 wv=-3*(o$="s")-2*(o$="m")-(o$="d"):ifwv>0then500
;; ...
;; 500 gp=gp-wv*10:printchr$(12):ifgp<20then540

(defun wield-weapon (adv weapon)
  "The adventurer takes up a weapon."
  (assert (weapon-p weapon))
  (setf (adv-wv adv) (get-weapon-value weapon))
  weapon)

(defparameter *adv-equipment-kinds*
  '(sword mace dagger
    plate chain leather
    flares gold-pieces
    lamp runestaff orb-of-zot)
  "Kinds of things the adventurer may carry around.")

(defun equipment-p (item)
  "Is the item equipment?"
  (find item *adv-equipment-kinds*))

(defun item-kind-of (item)
  (cond ((weapon-p item)    'weapon)
	((armor-p item)     'armor)
	((equipment-p item) item)))

(defun outfit-with (item adv)
  "Outfit the adventure with the equipment."
  (assert (typep adv 'adventurer))
  (let ((error-fmt "~S not a valid object to equip adventurer with."))
    (etypecase item
      (symbol
       (cond ((find item (rest *weapons*) :key 'first)
	      (wield-weapon adv item))
	     ((find item (rest *armor*) :key 'first)
	      (wear-armor adv item))
	     ((eq item 'lamp)
	      (setf (adv-lf adv) T))
	     ((eq item 'runestaff)
	      (setf (adv-rf adv) T))
	     ((eq item 'orb-of-zot)
	      (setf (adv-of adv) T)
	      (setf (adv-rf adv) Nil))
	     (T (error error-fmt item))))
      (list
       (cond ((eq (first item) 'flares)
	      (incf-adv-inv (adv-fl adv) (second item)))
	     (T (error error-fmt item)))))))

(defun buy-equipment (equipment price adv)
  (with-accessors ((gp adv-gp)) adv
    (assert (<= price (adv-gp adv)))
    (decf-adv-inv gp price)
    (outfit-with equipment adv)))

(defun adv-armor (adv)
  (get-equip-data (adv-av adv) *armor* 'symbol))

(defun adv-weapon (adv)
  (get-equip-data (adv-wv adv) *weapons* 'symbol))

(defun armed-p (adv)
  "Is the adventurer armed?"
  (with-accessors ((wv adv-wv)) adv
    (not (or (zerop wv) (null wv)))))

(defun bound-p (adv)
  "Does the adventurer have a book stuck to his hand?"
  (adv-bf adv))

(defun blind-p (adv)
  "Is the adventurer blind?"
  (adv-bl adv))

(defun runestaff-p (adv)
  "Does the adventurer have the runestaff?"
  (adv-rf adv))

(defun cast-spells-p (adv)
  "Is the adventurer smart enough to cast spells?"
  (< (adv-iq adv) 15))

;;; Again because basic numbered started 1 here is the indexes we're using.

;; | n | treasure text    | treasure effect     |
;; |---+------------------+---------------------|
;; | 0 | "the ruby red"   | cures lethargy      |
;; | 1 | "the norn stone" |                     |
;; | 2 | "the pale pearl" | removes leech       |
;; | 3 | "the opal eye"   | cures blindness     |
;; | 4 | "the green gem"  | cures forgetfulness |
;; | 5 | "the blue flame" | burns books         |
;; | 6 | "the palantir"   |                     |
;; | 7 | "the silmaril"   |                     |

(defun gain-treasure (adv treasure)
  (assert (treasure-p treasure))
  (setf (elt (adv-tr adv) (get-treasure-index treasure)) 1))

(defun lose-treasure (adv treasure)
  (assert (treasure-p treasure))
  (setf (elt (adv-tr adv) (get-treasure-index treasure)) 0))

(defun has-treasure-p (adv treasure)
  (assert (treasure-p treasure))
  (not (zerop (elt (adv-tr adv) (get-treasure-index treasure)))))

(defun adv-treasures (adv)
  "List treasures the adventurer has."
  (remove-if (complement (lambda (tr) (has-treasure-p adv tr)))
	     *treasures*))

(defun adv-race (adv)
  "Get the text of the adventure's race."
  (get-race-text (adv-rc adv)))

(defun adv-sex (adv)
  "Get the text of the adventurer's sex."
  (get-sex-text (adv-sx adv)))


(defun get-adv-map-icon (adv room-ref)
  (with-accessors ((map adv-mp)) adv
    (etypecase room-ref
      (integer (row-major-aref map room-ref))
      (list    (apply #'aref map room-ref)))))

(defun set-adv-map-icon (adv room-ref icon)
  (with-accessors ((map adv-mp)) adv
    (etypecase room-ref
      (integer (setf (row-major-aref map room-ref) icon))
      (list    (setf (apply #'aref map room-ref) icon)))))

(defsetf get-adv-map-icon set-adv-map-icon)

(defun adv-room-mapped-p (adv room-ref &optional creature-ref)
  "Test to see if the room has been mapped."
  (if creature-ref
      (equal (get-creature-icon creature-ref)
	     (get-adv-map-icon adv room-ref))
      (equal (get-unmapped-icon)
	     (get-adv-map-icon adv room-ref))))

(defun adv-map-room (adv room-ref creature)
  "Tags a room as mapped."
  (setf (get-adv-map-icon adv room-ref) (get-creature-icon creature))
  (make-event 'adv-mapped creature room-ref))

(defun adv-unmap-room (adv room-ref)
  "Tags a room as unmapped."
  (setf (get-adv-map-icon adv room-ref) (get-unmapped-icon)))




;;; Curses

(defun adv-cursed-p (adv &optional curse-name)
  "Return Nil or a list of curses."
  (if curse-name 
      (find curse-name (adv-cr adv))
      (adv-cr adv)))

(defun curse-lethargy (adv)
  "What happens when the curse of lethargy affects the adventurer."
  (assert (adv-cursed-p adv 'lethargy))
  (unless (has-treasure-p adv 'ruby-red)
    (make-event 'adv-dozed)))

(defun curse-leech (adv)
  "What happens when the curse of the leech affects the adventurer."
  (assert (adv-cursed-p adv 'leech))
  (unless (has-treasure-p adv 'pale-pearl)
    (decf-adv-inv (adv-gp adv) (random-range 1 5))))

(defparameter *forgetfulness* 'random
  "What kind of forgetfulness curse ")

(defun curse-forget (adv &optional (forget-type *forgetfulness*))
  "What happens when the curse of forgefulness affects the adventurer."
  (assert (adv-cursed-p adv 'forget))
  (unless (has-treasure-p adv 'green-gem)
    (adv-unmap-room
     adv (ecase forget-type
	   (mapped
	    (let ((mappedx (shuffle
			    (filter-array-indices
			     (lambda (s) (eq s (get-unmapped-icon)))
			     (adv-mp adv)))))
	      (array-index-row-major (adv-mp adv) (pop mappedx))))
	   (random
	    (random-array-subscripts (adv-mp adv)))))))

    
;;; Create character

(defun choose-race (adv)
  "Choose the adventurer's race."
  (wiz-write-line "You may be an Elf, Dwarf, Man, or Hobbit")
  (with-accessors ((rc adv-rc) (st adv-st) (dx adv-dx)) adv
    (with-player-input (race (make-prompt-adv-choice))
	(case race
	   (#\H (setf st  4 ; (incf-adv-rank st (* 2 1))
		      dx 12 ; (decf-adv-rank dx (* 2 1))
	   ;;         ot 12 ; (incf ot 4)
		      rc 'hobbit))
	   (#\E (setf st  6 ; (incf-adv-rank st (* 2 2))
		      dx 10 ; (decf-adv-rank dx (* 2 2))
		      rc 'elf))
	   (#\M (setf st  8 ; (incf-adv-rank st (* 2 3))
		      dx  8 ; (decf-adv-rank dx (* 2 3))
		      rc 'human))
	   (#\D (setf st 10 ; (incf st (* 2 4))
		      dx  6 ; (decf dx (* 2 4))
		      rc 'dwarf))
	   (T   (setf race
		      (wiz-error
		       "That was incorrect. Please type E, D, M, or H.")))))))

;; Original code set st to 2 and dx to 14. This bit of math adjusted
;; the attributes to their racial "norms" where:

;; 270 forq=1to4:ifleft$(r$(q),1)=o$thenrc=q:st=st+2*q:dx=dx-2*q
;; 280 nextq:print:ot=ot+4*(rc=1):ifrc>0thenr$(3)="human"

;; I've judged this not worth the effort of reproducing. Here's the
;; table of outcomes:

;; | q | race   | st | dx | iq | ot |
;; |---+--------+----+----+----+----|
;; | 0 | Nil    |  2 | 14 |  8 |  8 |
;; | 1 | hobbit |  4 | 12 |  8 | 12 |
;; | 2 | elf    |  6 | 10 |  8 |  8 |
;; | 3 | "man"  |  8 |  8 |  8 |  8 |
;; | 4 | dwarf  | 10 |  6 |  8 |  8 |

(defun choose-sex (adv)
  "Choose the adventurer's sex."
  (with-accessors ((race adv-race) (sx adv-sx)) adv
    (with-player-input (sex "Sex ")
      (case sex
	(#\M (setf sx 'male))
	(#\F (setf sx 'female))
	(T   (setf sex (wiz-error "Cute ~A, real cute. Try M or F" race)))))))
      
(defun allocate-points (adv)
  "Distribute other points to attributes."
  (with-accessors ((race adv-race)
		   (st adv-st) (dx adv-dx) (iq adv-iq)) adv
    (let ((ot (if (eq (adv-rc adv) 'hobbit) 12 8)))
      (wiz-write-line
       (with-output-to-string (stats)
	 (wiz-format stats "~|~&Ok ~A, you have these statistics:~%" race)
	 (wiz-format stats "~&strength= ~D intelligence= ~D dexterity= ~D~%"
		     st iq dx)
	 (wiz-format stats "~&and ~D other points to allocate as you wish.~%"
		     ot)))
      (loop
	 for (ranking ranking-text) in *rankings*
	 while (< 0 ot)
	 do
	   (loop
	      with choice = Nil
	      do
		(wiz-prompt
		 (wiz-format Nil "How many points do you add to ~A " ranking-text))
		(let ((expr (wiz-read-n)))
		  (if (typep expr (list 'integer 0 ot))
		      (setf choice expr)
		      (wiz-error "")))
	      until (not (null choice))
	      finally
		(decf-adv-inv ot choice)
		(funcall (fdefinition (list 'setf ranking))
			 (incf-adv-rank choice (funcall ranking adv)) adv)))
      (values st iq dx ot))))

(defparameter *catalog-fmt* "~{~A<~A>~^ ~}"
  "Format control string used for printing catalogs.")

(defun make-prompt-catalog (stuff item-printer catalog-data)
  (make-prompt-adv-choice
   (with-output-to-string (catalog)
     (wiz-format catalog "~&Here is a list of ~A you can buy (with cost in <>)~%"
		 stuff)
     (wiz-format catalog "~&~{~{~A<~D>~^ ~}~}~%"
		 (map 'list (lambda (item)
			      (list (funcall item-printer (first item))
				    (second item)))
		      catalog-data))
     (finish-output catalog))))

(defun get-catalog-price (item catalog-data)
  (second (find item catalog-data :key 'first)))

(defun buy-armor (adv)
  "The adventurer may buy some armor."
  (with-accessors ((race adv-race) (gp adv-gp)) adv
    (let* ((catalog '((no-armor 0) (leather 10) (chainmail 20) (plate 30)))
	   (prompt (make-prompt-catalog "armor" #'get-armor-text catalog)))
      (wiz-format *wiz-out* "~|~2&Ok ~A, you have ~D gold pieces (GP's)"
		  race gp)
      (with-player-input (choice prompt)
	(case choice
	  (#\P (buy-equipment 'plate (get-catalog-price 'plate catalog) adv))
	  (#\C (buy-equipment 'chainmail (get-catalog-price 'chainmail catalog) adv))
	  (#\L (buy-equipment 'leather (get-catalog-price 'leather catalog) adv))
	  (#\N 'no-armor)
	  (T   (setf choice
		     (wiz-error "Are you ~A or a ~A ? Type P,C,L, or N"
				(get-creature-text
				 ;; NOTE: Dragons are excluded from the list
				 ;; of beasts of insult
				 (random-elt (remove 'dragon *monsters*)))
				race))))))))


(defun buy-weapon (adv)
  "The adventurer may buy a weapon."
  (with-accessors ((race adv-race) (gp adv-gp)) adv
    (let* ((catalog '((no-weapon 0) (dagger 10) (mace 20) (sword 30)))
	   (prompt (make-prompt-catalog "weapon" #'get-weapon-text catalog)))
      (wiz-format *wiz-out*  "~|~2&Ok, bold ~A, you have ~D GP's left"
		  race gp)
      (with-player-input (choice prompt)
	(case choice
	  (#\S (buy-equipment 'sword (get-catalog-price 'sword catalog) adv))
	  (#\M (buy-equipment 'mace  (get-catalog-price 'mace catalog) adv))
	  (#\D (buy-equipment 'dagger (get-catalog-price 'dagger catalog) adv))
	  (#\N 'no-weapon)
	  (T   (setf choice
		     (wiz-error  "Is your IQ really ~D? Type S, M, D, or N"
				 (adv-iq adv)))))))))

(defun buy-lamp (adv)
  "The adventurer may buy a lamp."
  (when (< 19 (adv-gp adv))
    (when (wiz-y-or-n-p "~|Do you want to buy a lamp for 20 GP's ")
      (buy-equipment 'lamp 20 adv))
    (adv-lf adv)))

(defun buy-flares (adv)
  "The adventurer may buy some flares."
  (with-accessors ((race adv-race) (gp adv-gp) (fl adv-fl)) adv
    (when (< 0 gp)
      (wiz-format *wiz-out* "~|~&Ok, ~A, you have ~D gold pieces left~%" race gp)
      (with-player-input (flares "Flares cost 1 GP each. How many do you want "
				 :readf #'wiz-read-n)
	(cond ((typep flares (list 'integer 0 gp))
	       (buy-equipment (list 'flares flares) flares adv))
	      ((typep flares (list 'integer (1+ gp)))
	       (setf flares (wiz-error "You can only afford ~D" gp)))
	      (T
	       (setf flares (wiz-error "If you don't want any just type 0 (zero)"))))))))

(defun setup-adventurer ()
  "Make pc avatar"
  (wiz-format T "~&All right, bold one~%")
  (let ((adv (make-adventurer)))
    (choose-race     adv)
    (choose-sex      adv)
    (allocate-points adv)
    (buy-armor       adv)
    (buy-weapon      adv)
    (buy-lamp        adv)
    (buy-flares      adv) adv))

;;; Adventurer event generators

(defun make-adv-stronger (adv delta)
  (incf-adv-rank (adv-st adv) delta)
  (make-event 'adv-gained 'strength delta))

(defun make-adv-weaker (adv delta)
  (decf-adv-rank (adv-st adv) delta)
  (make-event 'adv-lost 'strength delta))

(defun make-adv-smarter (adv delta)
  (incf-adv-rank (adv-iq adv) delta)
  (make-event 'adv-gained 'intelligence delta))

(defun make-adv-dumber (adv delta)
  (decf-adv-rank (adv-iq adv) delta)
  (make-event 'adv-lost 'intelligence delta))

(defun make-adv-nimbler (adv delta)
  (incf-adv-rank (adv-dx adv) delta)
  (make-event 'adv-gained 'dexterity delta))

(defun make-adv-clumsier (adv delta)
  (decf-adv-rank (adv-dx adv) delta)
  (make-event 'adv-lost 'dexterity delta))

(defun change-adv-race (adv new-race)
  (setf (adv-rc adv) new-race)
  (make-event 'adv-changed-race new-race))

(defun change-adv-sex (adv new-sex)
  (setf (adv-sx adv) new-sex)
  (make-event 'adv-changed-sex new-sex))

(defun make-adv-richer (adv delta)
  (incf-adv-inv (adv-gp adv) delta)
  (make-event 'adv-gained 'gold-pieces delta))

(defun make-adv-poorer (adv delta)
  (decf-adv-inv (adv-gp adv) delta)
  (make-event 'adv-lost 'gold-pieces delta))

(defun give-adv-flares (adv delta)
  (incf-adv-inv (adv-fl adv) delta)
  (make-event 'adv-gained 'flares delta))

(defun take-adv-flares (adv delta)
  (decf-adv-inv (adv-fl adv) delta)
  (make-event 'adv-lost 'flares delta))

(defun give-adv-treasure (adv treasure)
  (gain-treasure adv treasure)
  (make-event 'adv-gained treasure))

(defun bind-adv-hands (adv item)
  (setf (adv-bf adv) T)
  (make-event 'adv-bound item))

(defun make-adv-blind (adv blinder)
  (setf (adv-bl adv) T)
  (make-event 'adv-blinded blinder))

(defun break-adv-weapon (adv)
  (setf (adv-wv adv) 0)
  (make-event 'adv-weapon-broke (adv-weapon adv)))

(defun destroy-adv-armor (adv)
  (setf (adv-wv adv) 0)
  (make-event 'adv-armor-destroyed (adv-armor adv)))

(defun damage-adv (adv dmg)
  "What happens when the adventurer is struck."
  (with-accessors ((av adv-av) (ah adv-ah) (st adv-st)) adv
    (let ((events ())
	  (total-damage 0))
      (when (< 0 av)
	(decf dmg av)           ; reduce damage by armor-value
	(decf-adv-inv ah av)    ; reduce armor-hits by armor-value
	(incf total-damage av)
	(when (< 0 dmg)         ; when damage more-than zero
	  (decf-adv-inv ah dmg) ; reduce armor-hits by damage
	  (incf total-damage dmg)
	  (decf dmg 0))
	(record-event events
		      (make-event 'adv-armor-damaged total-damage))
	(when (zerop ah)
	  (setf av 0)
	  (record-event events (make-event 'adv-armor-destroyed (adv-armor adv)))))
      (when (< 0 dmg)
	(record-event events (make-adv-weaker adv dmg)))
      events)))

;; 2800 IF AV=0 THEN 2830
;; 2810 Q=Q-AV : AH=AH-AV : IF Q<0 THEN AH=AH-Q : Q=0
;; 2820 IF AH < 0 THEN AH=0 : AV=0 : PRINT : PRINT "YOUR ARMOR IS DESTROYED - GOOD LUCK"
;; 2830 ST=ST-Q : RETURN

;; (list :adv-armor-destroyed "Your armor is destroyed - good luck")






;;; In Sorcerer BASIC TRUE = -1, FALSE = 0.

;;;; Zot's castle - array functions


;;; FIXME: I don't know yet why this doesn't work.

;; (defun position-array (item array &key (test #'eq) (start 0) (stop Nil))
;;        "Return the first index that matches test of item."
;;        (loop
;; 	  with res = Nil
;; 	  with beg = start
;; 	  with end = (or stop (array-total-size array)))
;; 	  for idx from beg below end
;; 	  do
;; 	    (setf res (funcall test item (row-major-aref array idx)))
	    
;; 	  until res
;; 	  finally (when res idx)))


;; Carl Taylor in "Some real world examples of using maplist & mapl?"
;; http://groups.google.com/group/comp.lang.lisp/msg/a80fbc35a0494c36

;; (defun get-all-objects-indices (object in-array &key (test #'eql))
;;   "Search an array for a given object returning a list of indices for
;; every instance of the object in the array. A list of lists, or Nil, is
;; returned."
;;   (declare (optimize (speed 3) (safety 0) (debug 0)))
;;   (let ((adjusted-dimensions-partial-products-list
;; 	 (maplist (lambda (sub-list) (reduce (function *) sub-list))
;; 		  (nconc (rest (array-dimensions in-array))
;; 			 (list 1)))))
;;     (loop
;;        for i fixnum below (array-total-size in-array)
;;        when (funcall test (row-major-aref in-array i) object)
;;        collect
;; 	 (loop
;; 	    with obj-position fixnum = i
;; 	    for  dims-partial-product
;; 	    in   adjusted-dimensions-partial-products-list
;; 	    collect
;; 	      (multiple-value-bind (quotient remainder)
;; 		  (truncate obj-position dims-partial-product)
;; 		(setf obj-position remainder)
;; 		(identity quotient))))))




;;;; Zot's castle

(defparameter *curses-init*
  (list
   (list 'lethargy #'curse-lethargy ())
   (list 'leech    #'curse-leech    ())
   (list 'forget   #'curse-forget   ()))
  "Setup features of curses")

(defstruct
    (castle (:conc-name   cas-)
	    (:constructor make-castle
			  (&aux
			   (rooms (make-castle-rooms))
			   (levels (make-castle-levels rooms))
			   ;; (loc-adventurer  +entrance+)
			   (curses *curses-init*))))
  "The castle of Zot"
  (rooms           ())
  (levels          ())
  (curses          ())  ; curses data (curse-name curse-function curse-loc)
  (loc-orb         ())  ; "location of the orb of zot"
  (loc-runestaff   ())  ; "location of the runestaff"
  (vendor-fury    Nil)  ; "vendors angry"
  (adventurer     Nil)
  (adversaries     ())
  ;; (loc-adventurer  ())  ; "player coordinates"
  (history         ())
  )

(defun get-castle-creature (castle room-ref &optional data-type)
  "Return the contents of a castle room based on reference. Reference
may be an index or list of coordinates."
  (with-accessors ((rooms cas-rooms)) castle
    (let ((creature
	   (etypecase room-ref
	     (integer (row-major-aref rooms room-ref))
	     (list    (apply #'mref rooms room-ref)))))
      (get-creature-data creature (or data-type 'symbol)))))

(defun set-castle-creature (castle room-ref creature)
  "Set creature in castle room"
  (with-accessors ((rooms cas-rooms)) castle
    (etypecase room-ref
       (integer (setf (row-major-aref rooms room-ref) creature))
       (list    (setf (apply #'aref rooms room-ref) creature)))))

(defsetf get-castle-creature set-castle-creature
    "Define setf for castle rooms.")

(defun castle-creature-p (castle room-ref creature-ref)
  "Return true if the creature in room is creature expected."
  (eq creature-ref
      (get-castle-creature castle room-ref (type-of creature-ref))))

(defun cas-creature-type-p (castle room-ref creature-type)
  "Is the creature in this room of the expected type?"
  (creature-type-p (get-castle-creature castle room-ref) creature-type))

(defun get-castle-creature-text (castle room-ref)
  "Get castle creature text."
  (get-castle-creature castle room-ref 'string))

(defun get-castle-creature-icon (castle room-ref)
  "Get castle creature icon."
  (get-castle-creature castle room-ref 'character))

(defun clear-castle-room (castle room-ref)
  "Make empty room."
  (setf (get-castle-creature castle room-ref) 'empty-room))

(defun random-castle-coords (castle)
  "Get random coordinates from the castle."
  (random-array-subscripts (cas-rooms castle)))

(defun cas-adv-here (castle)
  "Return coords of room the adventurer most recently entered."
  (first (event-subtype-of (find-event 'adv-entered-room (cas-history castle)))))

(defun cas-creature-here (castle)
  "What castle creature is in this room."
  (get-castle-creature castle (cas-adv-here castle)))

(defun cas-adv-near (castle vector-ref)
  "Return coords of room near the adventurer's current room."
  (when (typep vector-ref 'symbol)
    (setf vector-ref (vector-of-direction vector-ref)))
  (add-castle-vectors (cas-rooms castle)
		      (cas-adv-here castle)
		      vector-ref))

(defun cas-adv-last-turn (castle)
  "Find the last turn in history."
  (find-if #'turn-p (cas-history castle)))

(defun cas-adv-last-went (castle)
  "Find the last direction that the adventurer walked in."
  (second (find 'adv-walked (cas-history castle)
		:key 'first :test 'equal)))

(defun cas-adv-last-used (castle)
  "Find the last item that the adventurer used."
  (second (find 'adv-used (cas-history castle)
		:key 'first :test 'equal)))

(defun cas-adv-last-ate (castle)
  "How many turns since the adventurer last ate?"
  (count-turns (events-since 'adv-ate (cas-history castle))))


(defun castle-room-adv-p (castle coords)
  "Test to see if adventurer is in room"
  (equal (cas-adv-here castle) coords))

(defun make-map-icon-room (castle coords)
  "Make a map icon of a room in castle."
  (with-accessors ((adv cas-adventurer)) castle
    (format Nil " ~A " (get-adv-map-icon adv coords))))

(defun make-map-icon-adv (castle coords)
  "Make a map icon of a castle room with an adventurer in it."
  (with-accessors ((adv cas-adventurer)) castle
    (format Nil "<~A>" (get-adv-map-icon adv coords))))

(defun cas-adv-map-room (castle room-ref &optional creature-ref)
  "Tags a room as mapped."
  (adv-map-room (cas-adventurer castle) room-ref
		(or creature-ref
		    (get-castle-creature castle room-ref))))

(defun cas-adv-map-here (castle)
  "Sets a mapped room icon for the room the adventurer is in."
  (with-accessors ((adv cas-adventurer)
		   (here cas-adv-here)
		   (creature cas-creature-here)) castle
      (cas-adv-map-room castle here creature)))

(defun cas-adv-map-near (castle direction &optional creature)
  "Sets a mapped room icon for a nearest room in a given direction."
  (let ((near (cas-adv-near castle direction)))
    (cas-adv-map-room castle near
		      (or creature (get-castle-creature castle near)))))

(defun collect (accessor collection)
  "Collect all accessor data for each item in collection."
  (loop
     for item in collection
     collect (funcall accessor item)))

(defun get-castle-curse (castle curse-ref &optional aspect)
  "Get curse."
  (with-accessors ((curses cas-curses)) castle
    (let ((curse
	   (etypecase curse-ref
	     (symbol   (find curse-ref curses :key 'first))
	     (function (find curse-ref curses :key 'second))
	     (list     (remove-if-not (lambda (coords)
					(equal coords curse-ref))
				      curses :key 'third)))))
      (if (null aspect)
	  curse
	  (if (typep curse-ref 'list)
	      (let ((curses (remove-if-not (lambda (coords)
					(equal coords curse-ref))
				      curses :key 'third)))
		(ecase aspect
		  (name     (collect #'first  curses))
		  (function (collect #'second curses))
		  (coord    (collect #'third  curses))))
	      (ecase aspect
		(name     (first  curse))
		(function (second curse))
		(coord    (third  curse))))))))

(defun cas-room-cursed-p (castle coords &optional aspect)
  "Return Nil or a list of cursed room data for room coordinates."
  (get-castle-curse castle coords aspect))

(defparameter *curse-notify* Nil
  "Later versions printed message when a curse took effect")

(defconstant +curse-notice-ohare+ "A curse!"
  "Curse notice used in O'Hare version for Commodore PET.")

(defun gain-curse (castle)
  "The adventurer gains a curse."
  (with-accessors ((adv cas-adventurer)
		   (curses cas-curses)
		   (here cas-adv-here)) castle
    (assert (cas-room-cursed-p castle here))
    (let ((curses (cas-room-cursed-p castle here 'name)))
      (when curses
	(loop
	   for curse in curses
	   do
	     (pushnew curse (adv-cr adv))
	   collect
	     (make-event 'adv-gained 'curse curse))))))




;;;; Fill castle

;;; "GOSUB3200" finds a random room on a given floor, returns if the
;;; room is empty, gets another if it's not.

;;; This seems like a silly way to do this. It would be better to make
;;; a list (deck) of empty rooms, shuffle them, then pop (deal) the
;;; first on the list.


;; (defun replace-array-index (displ-array))


(defun castle-index-from-level-index (index level castle-rooms)
  "What is the level index of the given castle index."
  (+ (reduce #'* (list* level (castle-level-dimensions castle-rooms))) index))

(defun level-index-from-castle-index (index castle-rooms)
  "What is the level index of this castle index?"
  (- (reduce #'* (last (castle-level-dimensions castle-rooms) 2)) index))

(defun level-of-coords (coords)
  "What is the level (z-axis) of these coords"
  (assert (< 2 (length coords)))
  (first (last coords 3)))

(defun level-coords (coords)
  "What are the x-y coordinates."
  (assert (< 1 (length coords)))
  (last coords 2))

(defun get-castle-level (castle level)
  "Get levels from castle."
  (elt (cas-levels castle) level))

(defun list-empty-room-indices (castle &optional level)
  "List the indexes of empty rooms in the castle or level of the
castle."
  (with-accessors ((rooms cas-rooms)) castle
    (labels ((room-empty-p (room) (eq 'empty-room room)))
      (cond
	(level
	 (mapcar
	  (lambda (n) (castle-index-from-level-index n level rooms))
	  (filter-array-indices #'room-empty-p
				(get-castle-level castle level))))
	(T
	 (filter-array-indices #'room-empty-p rooms))))))

;; FIXME: depends on castle size constant 64.
;; (reduce #'* (castle-level-dimensions (castle-rooms castle)))

(defun random-empty-room (castle &optional level)
  "Get a random empty room from a castle or castle level."
  (first (shuffle (list-empty-room-indices castle level))))

(defun castle-levels (castle)
  "Return the number of levels in the castle."
  (first (array-dimensions (cas-rooms castle))))

(defun castle-coords-index (castle coords)
  "Turn castle coordinates into a castle index."
  (apply #'array-row-major-index (cas-rooms castle) coords))

(defun castle-index-coords (castle index)
  "Turn a castle index into castle coodinates."
  (array-index-row-major (cas-rooms castle) index))

;; (defun place-stairs-in-castle (castle &key (population 2))
;;   (loop for level from 0 to (- (castle-height castle) 2)
;;      do
;;        (let ((random-empty-rooms
;; 	      (shuffle (list-empty-room-indices castle level))))
;; 	 (loop
;; 	    repeat population
;; 	    do
;; 	      (let* ((up (pop random-empty-rooms))
;; 		     (dn (add-castle-vectors castle (vector-of-direction 'down)
;; 					     (castle-index-coords castle up))))
;; 		(setf (get-castle-room-index castle up) 'stairs-up)
;; 		(setf (get-castle-room castle dn) 'stairs-down)))))
;;   castle)

;; (defun place-creatures-on-level (castle level creatures &key (population 1))
;;   (let ((random-empty-rooms (shuffle (list-empty-room-indices level))))
;;     (loop
;;        repeat population
;;        do
;; 	 (loop 
;; 	    for creature in creatures
;; 	    do
;; 	      (setf (get-castle-room-index castle
;; 					   (pop random-empty-rooms))
;; 		    creatures)))))

(defun setup-castle (&optional (silent Nil))
  "Place stuff in castle"
  (let ((castle (make-castle)))
    (unless silent (wiz-write-string "Please be patient -"))
    ;; Place entrance (2)
    (setf (get-castle-creature castle +entrance+) 'entrance)
    (unless silent (wiz-write-string "in"))
    (with-accessors ((rooms cas-rooms)
		     (orb cas-loc-orb)
		     (runestaff cas-loc-runestaff)
		     (curses cas-curses)) castle
      (let* ((height (castle-height rooms))
	     (lvl-mt (loop
		       for level from 0 below height
		       collect
			 (shuffle (list-empty-room-indices castle level)))))
	(flet ((random-lvl-room (level)
		 (pop (elt lvl-mt level)))
	       (random-cas-room ()
		 (pop (elt lvl-mt (random (length lvl-mt))))))
	  ;; Place stairs.
	  ;; 2 stairs down (4) on floors 1 - 7 (0 - 6)
	  ;; 2 stairs up (3) on floors 2 - 8 (1 - 7)
	  ;; (place-stairs-in-castle castle)
	  (loop
	     for lvl-dn from 0 below (1- height)
	     for lvl-up from 1 below height
	     do
	       (loop repeat 2
		  do
		    (let* ((dn (random-lvl-room lvl-dn))
			   (up (castle-coords-index
				castle
				(add-castle-vectors
				 rooms
				 (vector-of-direction 'down)
				 (castle-index-coords castle dn)))))
		      (setf (get-castle-creature castle dn) 'stairs-down)
		      (setf (get-castle-creature castle up) 'stairs-up)
		      (setf (elt lvl-mt lvl-up)
			    (remove up (elt lvl-mt lvl-up))))))
	  (unless silent (wiz-write-string "i"))
	  ;; Place monsters (13 - 24).
	  ;; 1 each monster on all floors
	  (loop for level from 0 below height
	     for ch across "tializin"
	     do
	     ;; (place-creatures-on-level castle level *monsters*)
	       (loop 
		  for monster in *monsters*
		  do
		    (setf (get-castle-creature castle
					       (random-lvl-room level))
			  monster))
	       (unless silent (wiz-write-string (string ch))))
	  ;; Place vendor and items.
	  ;; 3 each item on all floors (5 - 12)
	  ;; 1 vendor on all floors (25)
	  (loop for level from 0 below height
	     do
	     ;; (place-creatures-on-level castle level *rooms* :population 3)
	       (loop
		  repeat 3
		  do
		    (loop 
		       for room in *rooms*
		       do
			 (setf (get-castle-creature castle
						    (random-lvl-room level))
			       room)))
	       (setf (get-castle-creature castle
					  (random-lvl-room level))
		     'vendor)
	     finally
	       (unless silent (wiz-write-string "g")))))
      ;; Place unique things.
      (let ((cas-mt (shuffle (list-empty-room-indices castle))))
	;; Place treasures.
	;; 1 treasure in 8 random rooms
	(loop
	   for treasure in *treasures*
	   do
	     (setf (get-castle-creature castle (pop cas-mt)) treasure))
	;; Place curses. 1 curse in 3 random empty rooms.
	(loop
	   for curse in curses 
	   for s across " ca"
	   do
	     (setf (third curse)
		   (castle-index-coords castle (random-elt cas-mt)))
	   ;; Multiple curses can be in the same room.
	     (unless silent (wiz-write-string (string s)))
	   finally
	     (unless silent (wiz-write-string "s")))
	;; Place runestaff with 1 random monster (13 - 24).
	(let ((loc-rune (pop cas-mt)))
	  (setf (get-castle-creature castle loc-rune) (random-monster)
		runestaff (array-index-row-major (cas-rooms castle) loc-rune)))
	;; Place orb in room that seems like a warp (9)
	(let ((loc-orb (pop cas-mt)))
	  (setf (get-castle-creature castle loc-orb) 'warp
		orb (array-index-row-major (cas-rooms castle) loc-orb)))
	(unless silent (wiz-write-string "tle"))))
    (unless silent (wiz-format *wiz-out* "~%~%"))
    castle))





(defun latest-creature-found (castle)
  "What creature has been most recently found."
  (event-property
   (oldest-event
    (events-since 'adv-found (cas-history castle)))))


;;; Adventurer events

(defun send-adv (coords)
  "Send an adventurer to location at coords"
  ;; (record-events (cas-history castle)
  (make-event 'adv-entered-room coords))

(defun move-adv (castle vector-ref)
  "Move adventurer in direction."
  (with-accessors ((here cas-adv-here)) castle
    (send-adv (add-castle-vectors (cas-rooms castle)
				  here
				  (vector-of-direction vector-ref)))))

(defun make-adv-fall (castle)
  (make-history
   (make-event 'adv-fell)
   (move-adv castle 'down)))

(defun make-adv-warp (castle)
  (let ((coords (random-array-subscripts (cas-rooms castle))))
    (make-history
     (make-event 'adv-warped)
     (send-adv coords))))

(defun make-adv-stagger (castle turns)
  (let ((events (make-history)))
    (join-histories events
		    (loop
		       repeat turns
		       collect
			 (make-event 'adv-staggered)))
    (record-events events
		   (move-adv castle
			     (random-elt '(north east west south))))))

(defun adv-springs-gas-trap (castle)
  (make-adv-stagger castle 20))

(defun adv-springs-bomb-trap (adv)
  (damage-adv adv (random-range 1 6)))

(defun adv-springs-glue-trap (adv item)
  (make-history (bind-adv-hands adv item)))

(defun adv-springs-flash-trap (adv)
  (make-history (make-adv-blind adv 'flash)))

(defun adv-reads-strength-manual (adv)
  (make-history (make-adv-stronger adv +adv-rank-max+)))

(defun adv-reads-dexterity-manual (adv)
  (make-history (make-adv-nimbler adv +adv-rank-max+)))


;;;; Outcomes

(defun make-outcome (outcome-name outcome-effect outcome-text)
  (list outcome-name outcome-effect outcome-text))

(defun name-of-outcome (outcome)
  (first outcome))

(defun outcome-name-p (outcome name-ref)
  (eq name-ref (name-of-outcome outcome)))

;;; fixme

(defun type-of-outcome (outcome)
  (first outcome))

(defun type-p-outcome (outcome type-ref)
  (eq type-ref (type-of-outcome outcome)))

(defun get-outcome (outcome-type outcomes)
  (find outcome-type outcomes :key 'type-of-outcome))

(defun effect-of-outcome (outcome)
  (let ((effect-ref (second outcome)))
    (etypecase effect-ref
      (null     Nil)
      (symbol   (symbol-function effect-ref))
      (function effect-ref))))

(defun text-of-outcome (outcome)
  (let ((text-ref (third outcome)))
    (etypecase text-ref
      (null     Nil)
      (symbol   (symbol-function text-ref))
      ((or function string) text-ref))))


;;;; Enter room

(defun make-message-report-inv (castle inv)
  "Make message for letting to "
  (with-accessors ((adv cas-adventurer)) castle
    (format Nil "~2&You have ~D"
	    (ecase inv
	      (gold-pieces (adv-gp adv))
	      (flares      (adv-fl adv))))))

(defun adv-finds-gold-pieces (castle)
  "What happens when an adventurer finds gold."
  (with-accessors ((adv  cas-adventurer)
		   (here cas-adv-here)) castle
    (assert (eq (get-castle-creature castle here) 'gold-pieces))
    (let ((gps (random-range 1 10))
	  (events (make-history)))
      (clear-castle-room castle here)
      (record-events events
		     (make-adv-richer adv gps)
		     (cas-adv-map-here castle))
      (values
       events
       (make-message-report-inv castle 'gold-pieces)))))

(defun adv-finds-flares (castle)
  "What happens when an adventurer finds flares."
  (with-accessors ((adv  cas-adventurer)
		   (here cas-adv-here)) castle
    (assert (eq (get-castle-creature castle here) 'flares))
    (let* ((flares (random-range 1 5))
	   (events (make-history)))
      (clear-castle-room castle here)
      (record-events events
		     (give-adv-flares adv flares)
		     (cas-adv-map-here castle))
      (values
       events
       (make-message-report-inv castle 'flares)))))

(defun adv-finds-treasure (castle)
  "What happens when an adventurer finds treasure."
  (with-accessors ((adv  cas-adventurer)
		   (here cas-adv-here)) castle
    (let* ((treasure (get-castle-creature castle here))
	   (events (list (make-event 'adv-found treasure))))
      (clear-castle-room castle here)
      (record-events events
		     (give-adv-treasure adv treasure)
		     (cas-adv-map-here castle))
      (values
       events
       "~&Its now yours"))))

;;; --- FIXME [wc 2012-12-11]
;;; streamline these with a macro

(defun adv-finds-sinkhole (castle)
  "What happens when the adventurer finds a sinkhole?"
  (assert (eq (cas-creature-here castle) 'sinkhole))
  (make-adv-fall castle))

(defun adv-finds-warp (castle)
  "What happens when the adventurer finds a warp?"
  (assert (eq (cas-creature-here castle) 'warp))
  (make-adv-warp castle))
  
(defun adv-finds-orb-of-zot (castle)
  "What happens when the adventurer finds the orb-of-zot."
  (with-accessors ((adv  cas-adventurer)
		   (here cas-adv-here)
		   (went cas-adv-last-went)) castle
    (cond
      ((equal (events-since 'adv-used (cas-history castle))
	      (make-history
	       (make-event 'adv-used 'runestaff)
	       (make-event 'adv-entered-room here)))
       (outfit-with 'orb-of-zot adv)
       (setf (cas-loc-orb castle) Nil)
       (clear-castle-room castle here)
       (values
	(make-history (make-event 'adv-found 'orb-of-zot))
	(wiz-format T "Great unmitigated Zot!~
                       ~&You just found the Orb of Zot~
                       ~&The Runestaff is gone")))
      (T
       (values
	(make-history (make-event 'adv-found 'warp)))))))



;;;; Combat

(defstruct
    (adversary
      (:conc-name foe-)
      (:constructor make-adversary
		    (creature
		     &aux
		     (text (get-creature-text creature))
		     (strike-damage (calc-enemy-damage creature))
		     (hit-points (calc-enemy-hit-points creature)))))
  (creature Nil)    ; creature
  (text "")
  (strike-damage 0) ; q1
  (hit-points 0)    ; q2
  (first-turn T)    ; q3
  (enwebbed 0)     ; enemy enwebbed
  (end Nil)
  (hit-point-limiter (make-limiter #'< 0 hit-points))
  )

(define-modify-macro decf-foe-hit-points (&optional (delta 1))
  (lambda (foe-hp delta)
    (setf foe-hp (funcall (foe-hit-point-limiter foe)
				  (- foe-hp delta))))
  "Decrease foe hit points.")

(defun latest-foe (castle)
  "Who is/was the adventurer's last/current combat opponent?"
  (first (cas-adversaries castle)))

(defun foe-alive-p (adversary)
  "Is the combatant still alive?"
  (not (zerop (foe-hit-points adversary))))

(defun foe-enwebbed-p (adversary)
  (< 0 (foe-enwebbed adversary)))

(defun damage-foe (foe damage)
  (decf-foe-hit-points (foe-hit-points foe) damage)
  (make-event 'foe-wounded damage))


(defparameter *combat-turn-events*
  '(adv-attacked adv-cast-spell adv-retreated
    foe-attacked)
  "Combat events")

;; 1790 A=PEEK(FND(Z))-12:WC=0:IF(A<13)OR(VF=1)THEN2300
;; 2300 Q1=1+INT(A/2):Q2=A+2:Q3=1

;;; FIXME what's q3? first round

;; 2520 if o$<>"w" then 2540
;; 2530 st=st-1 : wc=fna(8)+1 : on 1 - (st < 1) goto 2690,2840

(defun adv-casts-spell-web (castle)
  "The adventurer casts a web spell to entangle adversary."
  (with-accessors ((adv cas-adventurer)
		   (foe latest-foe)) castle
    (decf-adv-rank (adv-st adv) 1)
    (let ((turns-enwebbed (+ 2 (random 8))))
      (record-events (cas-history castle)
		     (make-event 'adv-cast-spell 'web turns-enwebbed))
      (push (foe-enwebbed foe) turns-enwebbed))))

;; 2540 if o$<>"f" then 2580
;; 2550 q=fna(7)+fna(7):st=st-1:iq=iq-1:if(iq<1)or(st<1)then2840
;; 2560 print"  It does ";q;"points of damage.":print
;; 2570 q2=q2-q:goto 2410

(defun make-message-cast-fireball (castle event)
  (assert (event-type-p event '(adv-cast-spell fireball)))
  (when (adv-alive-p (cas-adventurer castle))
    (format Nil "  It does ~D points of damage." (third event))))

(defun adv-casts-spell-fireball (castle)
  "The adventure casts a fireball spell on the adversary."
  (with-accessors ((adv cas-adventurer)
		   (foe latest-foe)) castle
    (decf-adv-rank (adv-st adv) 1)
    (decf-adv-rank (adv-iq adv) 1)
    (let ((damage (+ 2 (random 7) (random 7))))
      (decf-adv-rank (foe-hit-points foe) damage)
      (record-events (cas-history castle)
		     (make-event 'adv-cast-spell 'fireball damage)))))

;; 2540 if o$<>"f" then ...
;; print"death - - - ";:ifiq<15+fna(4)thenprint"yours";iq=0goto2840
;; print"his":printq2=0:goto2420

(defun make-message-cast-death (castle event)
  (assert (event-type-p event '(adv-cast-spell death)))
  (format Nil  "Death - - - ~A"
	  (if (adv-alive-p (cas-adventurer castle))
	      "his"
	      "yours")))

(defun adv-casts-spell-death (castle)
  "The adventurer casts a death spell."
  (record-events (cas-history castle)
		 (make-event 'adv-cast-spell 'death))
  (with-accessors ((adv cas-adventurer)
		   (foe latest-foe)) castle
    (with-accessors ((iq adv-iq)) adv
      (cond ((< iq (+ 15 (random-range 1 4)))
	     (setf iq 0))
	    (T
	     (setf (foe-hit-points foe) 0))))))

(defun choose-spell ()
  "Adventurer chooses a spell to cast."
  (with-player-input (spell "Which spell (web, fireball, or deathspell) ")
    (case spell
      (#\W 'adv-casts-spell-web)
      (#\F 'adv-casts-spell-fireball)
      (#\D 'adv-casts-spell-death)
      (T   (setf spell (wiz-error "Choose one of the listed options"))))))

(defun adv-casts-spell (castle)
  "The adventurer casts a spell."
  (with-accessors ((adv cas-adventurer)
		   (foe latest-foe)) castle
    (if (or (cast-spells-p adv) (< 1 (foe-enwebbed foe)))
	(wiz-error "You can't cast a spell now!")
	(funcall (symbol-function (choose-spell)) castle))))

;; Powers, 1980; 2500--2600

;; (defun make-message-adv-bribed (event)
;;   "Make adv-bribed message."
;;   (case (event-property event)

(defparameter *bribe-outcomes*
  (list
   (list 'bribe-refused Nil "'All I want is your life!'")
   (list 'bribe-accepted 'foe-accepts-bribe "Okay, just don't tell anyone."))
  "Outcomes of bribing adversaries.")

(defun adv-bribes-adversary (castle)
  "What happens when an adventurer tries to bribe a creature."
  (with-accessors ((adv cas-adventurer)
		   (history cas-history)
		   (foe latest-foe)) castle
    (let ((treasure (random-elt (adv-treasures adv)))
	  (foe-name (foe-creature foe)))
      (record-events
       history
       (make-event 'adv-bribes (foe-creature foe))
       (cond ((null treasure)
	      (wiz-write-line "All I want is your life!")
	      (make-event 'bribe-refused))
	     (T
	      (when (wiz-y-or-n-p
		     (wiz-format Nil "I want ~A will you give it too me "
				 (get-creature-text treasure)))
		(lose-treasure adv treasure)
		(when (eq 'vendor foe-name)
		  (setf (cas-vendor-fury castle) Nil))
		(make-event 'bribe-accepted foe-name treasure))))))))

(defun adv-retreats (castle)
  "Adventurer retreats from a fight."
  (with-accessors ((adv cas-adventurer)) castle
    (wiz-write-line "You have escaped")
    (let ((direction (wiz-read-direction
		      "Do you go north, south, east, or west "
		      (format Nil "Don't press your luck ~A"
			      (adv-race adv)))))
      (values
       (make-history (make-event 'adv-retreated)
		     (move-adv castle direction))))))

;; (defun make-message-adv-attacks (castle event)
;;   (assert (event-type-p event 'adv-attacks))
;;   (let ((creature-text
;; 	 (get-creature-text (foe-creature (latest-foe castle)))))
;;     (ecase (event-property event)
;;       (unarmed (format Nil "Pounding on ~A won't hurt it" creature-text))
;;       (book-stuck-on-hand "You can't beat it to death with a book!")
;;       (missed  "  Drat! Missed")
;;       (strike (format Nil "  You hit the lousy ~A"
;; 		      (subseq creature-text 2))))))

;; (defun make-message-weapon-broke (castle event)
;;   (assert (event-type-p event 'adv-weapon-broke))
;;   (format Nil "Oh no! Your ~A broke"
;; 	  (get-weapon-text (adv-wv (cas-adventurer castle)))))

(defun cas-adv-hungry-p (castle)
  "True if adventurer is hungry."
  (< 60 (count-turns (events-since 'adv-ate (cas-history castle)))))


(defun adv-kills-adversary (castle)
  "What happens when the adventurer kills a creature."
  (with-accessors ((adv cas-adventurer)
		   (here cas-adv-here)
		   (foe latest-foe)) castle
    (assert (not (foe-alive-p foe)))
    (clear-castle-room castle here)
    (record-events (cas-history castle)
		   (make-event 'adv-killed (foe-creature foe))
		   (when (cas-adv-hungry-p castle)
		     (make-event 'adv-eats (foe-creature foe)))
		   (when (eq here (cas-loc-runestaff castle))
		     (make-event 'adv-found 'runestuaff))
		   (let ((hoard (random-range 1 1000)))
		     (incf (adv-gp adv) hoard)
		     (make-event 'adv-found 'gold-pieces hoard)))))

(defun adv-weapon-broke-on-foe-p (foe)
  "Did the adventurer's weapon break on the foe?"
  (assert (foe-alive-p foe)) 
  (when (and (find (foe-creature foe) '(gargoyle dragon))
	     (zerop (random 8)))
    T))

(defparameter *adv-attacks-outcomes*
  (list
   (list 'adv-strike-missed  Nil "  Drat! Missed")
   (list 'adv-strike-hit 'damage-foe
	 (lambda (creature-ref)
	   (format Nil "  You hit the lousy ~A"
		   (subseq (get-creature-text creature-ref) 2)))))
  "Possibilities when the adventurer strikes at a foe.")

(defun make-adv-strike (adv)
  (get-outcome
   (if (< (adv-dx adv) (+ (random 20) (if (adv-bl adv) 1 0)))
      'adv-strike-missed
      'adv-strike-hit)
   *adv-attacks-outcomes*))
  
(defun adv-attacks (castle)
  "What happens when the adventure attacks a creature."
  (with-accessors ((adv cas-adventurer)
		   (foe latest-foe)) castle
    (let ((events (make-history)))
      (destructuring-bind (outcome-type outcome-effect outcome-text)
	  (cond
	    ((not (armed-p adv))
	     (list 'adv-attacks-unarmed Nil
		   (format Nil  "Pounding on ~A won't hurt it"
			   (get-creature-text (foe-creature foe)))))
	    ((bound-p adv)
	     (list 'adv-bound Nil "You can't beat it to death with a book!"))
	    (T
	     (make-adv-strike adv)))
	(when (eq outcome-type 'adv-strike-hit)
	  (record-event events (funcall outcome-effect foe (adv-wv adv))))
	(when (and (eq outcome-type 'adv-strike-hit)
		   (adv-weapon-broke-on-foe-p foe))
	  (record-event events (break-adv-weapon adv))
	  (push-text outcome-text
		     (format Nil "Oh no! Your ~A broke"
			     (get-weapon-text (adv-weapon adv)))))
	(when (not (foe-alive-p foe))
	  (record-event events (make-event 'foe-slain (foe-creature foe))))
	(values
	 events
	 outcome-text)))))

(defun make-foe-struggle-with-web (foe)
  (assert (foe-enwebbed-p foe))
  (with-accessors ((bound foe-enwebbed)) foe
    (let ((events (make-history))
	  (message (make-text)))
      (decf bound)
      (cond ((< 0 bound)
	     (record-event events (make-event 'foe-unbound))
	     (push-text message 
			(format Nil "The ~A is stuck and can't attack"
				(subseq (foe-text foe) 2))))
	    (T
	     (record-event events (make-event 'foe-unbound))
	     (push-text message "The web just broke!")))
      (values
       events
       message))))

(defparameter *foe-attack-outcomes*
  (list
   (list 'foe-strike-missed Nil "  Hah! He missed you")
   (list 'foe-strike-hit 'damage-adv "  Ouch! He hit you"))
  "Outcomes when an adversary attacks.")

(defun make-foe-strike (adv)
  (get-outcome
   (if (< (+ (random-range 1 6) (random-range 1 6) (random-range 1 6)
	     (* 3 (if (adv-bl adv) 1 0)))
	  (adv-dx adv))
       'foe-strike-missed
       'foe-strike-hit)
   *foe-attack-outcomes*))

(defun adversary-attacks (castle)
  "What happens when an adversary attacks."
  (with-accessors ((adv cas-adventurer)
		   (foe latest-foe)) castle
    (let ((events (make-history))
	  (message ""))
      (when (foe-enwebbed-p foe)
	(multiple-value-bind (struggle-events struggle-text)
	    (make-foe-struggle-with-web foe)
	  (record-event events struggle-events)
	  (push-text message struggle-text)))
      (unless (foe-enwebbed-p foe)
	(destructuring-bind (outcome-name outcome-effect outcome-text)
	    (make-foe-strike adv)
	  (when (eq outcome-name 'foe-strike-hit)
	    ;; (record-event (make-event 'foe-struck-adventurer))
	    (join-histories events (funcall outcome-effect adv (foe-strike-damage foe))))
	  (push-text message outcome-text)))
      ;; FIXME [wc 2013-01-29]: maybe use TAGBODY instead of (WHEN ...) (UNLESS ...)
      (values events message))))

(defun make-message-end-game (adv end turns)
  "What does the game report when the game ends."
  (with-accessors ((race adv-race)
		   (st adv-st) (iq adv-iq) (dx adv-dx)
		   (gp adv-gp) (lf adv-lf) (fl adv-fl)
		   (wv adv-wv) (av adv-av) (tr adv-tr)
		   (rf adv-rf) (of adv-of)) adv
    (with-output-to-string (message)
      (cond ((eq end 'death)
	     (sleep 7.5) 
	     (format message "~|~A"
		     (make-string *wiz-width*
				  :initial-element #\*))
	     (format message
		     "~2&A noble effort, oh formerly living ~A" race)
	     (format message "~&You died from a lack of ~A"
		     (cond ((< st 1) "strength")
			   ((< iq 1) "intelligence")
			   ((< dx 1) "dexterity")
			   (T "life")))
	     (format message "~&When you died you had:~%"))
	    ((eq end 'exit)
	     (format message
		     "~&You left the castle with~:[out~;~] the Orb of Zot"
		     of)))
      (unless (eq end 'death)
	(if (and of (eq end 'exit))
	    (format message "~2&A glorious victory!~
                             ~&You also got out with the following:")
	    (format message "~2&A less than awe-inspiring defeat.~
                             ~&When you left the castle you had:~%")))
      (when (not (eq end 'death))
	(format message "~&Your miserable life!~%"))
      (format message "~{~@[~A~]~%~}"
	      (loop
		 for tr-n in (adv-treasures adv)
		 collect (get-creature-text tr-n)))
      (format message "~&~A" (get-weapon-text wv))
      (format message "~&~A" (get-armor-text av))
      (when lf (format message "~&a lamp"))
      (format message "~&~D flares" fl)
      (format message "~&~D GP's" gp)
      (when rf (format message "~&the Runestaff"))
      (format message "~&and it took you ~D turns!" turns))))

;; for q=1 to 750:next q:printchr$(12):gosub3270

;; (defun make-message-fight-outcome (castle events)
;;   "Make message about the fight outcome."
;;   (with-accessors ((adv cas-adventurer)
;; 		   (here cas-adv-here)) castle
;;       (ecase (event-type-of (oldest-event events))
;; 	(adv-slain
;; 	 (make-message-end-game adv 'death
;; 				(count-turns (cas-history castle))))
;; 	(adv-killed
;; 	 (with-output-to-string (message)
;; 	   (format message "~&~A lies dead at your feet"
;; 		   (foe-text (latest-foe castle)))
;; 	   (format message
;; 		   (make-message-adv-found castle
;; 					   (oldest-event
;; 					    (events-since 'adv-found events))))))
;; 	(adv-retreats Nil)
;; 	(adv-bribed
;; 	 (make-message-adv-bribed
;; 	  (events-since 'adv-bribed events))))))

(defun make-message-adv-eats (event castle)
  "Make message adventurer eating."
  (assert (event-type-p event 'adv-eats))
  (format Nil "~&You spend an hour eating ~A~A"
	  (foe-text (latest-foe castle))
	  (random-elt *eats*)))

(defun make-message-adv-killed (castle event)
  "Return message when adventurer kills a creature."
  (assert (event-type-p event 'adv-eats))
  (format Nil "~&~A lies dead at your feet"
	  (foe-text (latest-foe castle))))


;; (defun eval-fight-result (castle)
;;   "What happened about the fight."
;;   (with-accessors ((adv cas-adventurer)
;; 		   (here cas-adv-here)) castle
;;     (let ((en-t (get-creature-text (get-castle-creature castle here))))
;;       (cond ((equal end 'death)
;; 	     '(end-game adv 'death))
;; 	    ((equal end 'victory)
;; 	     (wiz-write-line 
;; 	      (with-output-to-string (out)
;; 		(when (< 60 (- (adv-fd adv) (length (adv-tn adv))))
;; 		  (setf        (adv-fd adv) (length (adv-tn adv))))
;; 		(when (eq here (cas-loc-runestaff  castle))
;; 		  (setf (adv-rf adv) T)
;; 		  (make-event 'adv-found 'runestaff))))
;; 	     (clear-castle-room castle here))
;; 	    ((equal end 'retreat) (retreat castle))
;; 	    ((equal end 'bribe)
;; 	     (when (eq (get-castle-creature castle here) 'vendor)
;; 	       (setf (cas-vendor-fury castle) Nil)))))))

;; (defun fight-eval (castle expr)
;;   "What does the adventurer do in the fight?"
;;   (with-accessors ((adv cas-adventurer)
;; 		   (foe latest-foe)) castle
;;       (cond ((equal expr "A") (attack-adversary castle))
;; 	    ((equal expr "R") (retreat castle))
;; 	    ((equal expr "B") (bribe-adversary castle))
;; 	    ((equal expr "C") (cast-spell castle))
;; 	    (T
;; 	     (wiz-error "Choose one of the listed options")))))

(defun make-prompt-fight (castle)
  "Make the fight round prompt."
  (with-accessors ((adv cas-adventurer)
		   (foe latest-foe)) castle
    (make-prompt-adv-choice
     (with-output-to-string (facing)
       (format facing "~&You're facing ~A~%" (foe-text foe))
       (format facing "~&You may attack or retreat")
       (when (null
	      (first (events-since
		      (make-event 'adv-found
				  (foe-creature foe))
		      (cas-history castle))))
	 (format facing " or bribe"))
       (when (< 14 (adv-iq adv))
	 (format facing " or cast a spell"))
       (format facing "~&Your strength is ~D and dexterity is ~D"
	       (adv-st adv) (adv-dx adv))))))

(defun get-adv-fight-action (castle)
  "The adventurer chooses a fight action."
  (let ((prompt (make-prompt-fight castle)))
    (with-player-input (choice prompt)
      (case choice
	(#\A 'adv-attacks)
	(#\R 'adv-retreats)
	(#\B 'adv-bribes)
	(#\C 'adv-casts-spell)
	(T (setf choice (wiz-error "Choose one of the listed options")))))))

(defun fight-end-p (events)
  "Return true if fight is over"
  (find (event-type-of (latest-event events))
	'(adv-killed adv-killed-creature adv-entered-room adv-bribed)))

(defun adv-meets-adversary (castle)
  "The adventurer fights an creature in the castle."
  (with-accessors ((adv cas-adventurer)
		   (here cas-adv-here)) castle
    (push (make-adversary (get-castle-creature castle here))
	  (cas-adversaries castle))
    ;; (with-accessors ((first-turn fight-first-turn)
    ;; 		     (end fight-end)) fight)
    (loop
       with fight-events = (make-history)
       do
	 (multiple-value-bind (events message)
	     (funcall (get-adv-fight-action castle) castle)
	   (join-histories fight-events events)
	   (when message
	     (wiz-write-line message)))
       until (fight-end-p fight-events)
       finally (return fight-events))))

(defun sell-treasures-to-vendor (adv)
  (with-accessors ((gp adv-gp)) adv
    (loop
       for tr in (adv-treasures adv)
       do
	 (let ((tr-t (get-creature-text tr))
	       (tr-v (random (* (1+ (get-treasure-index tr)) 1500))))
	   (when (wiz-y-or-n-p
		  (wiz-format Nil "Do you want to sell ~A for ~D" tr-t tr-v))
	     (incf gp tr-v)
	     (lose-treasure adv tr))))))

;; (begin-game
;;  (weapons 
;;    ((no-weapon 0)
;;     (dagger   10)
;;     (mace     20)
;;     (sword    30)))
;;   (armor
;;   ((no-armor   0)
;;    (leather   10)
;;    (chain     20)
;;    (plate     30)))
;;  (lamp
;;   ((lamp      20)))
;;  (flares
;;   ((flare      1))))

;; (vendor
;;   (weapons 
;;    ((no-weapon       0)
;;     (dagger       1250)
;;     (mace         1500)
;;     (sword        2000)))
;;   (armor
;;    ((no-armor        0)
;;     (leather      1250)
;;     (chain        1500)
;;     (plate        2000)))
;;   (potions
;;    ((strength     1000)
;;     (intelligence 1000)
;;     (dexterity    1000)))
;;   (lamp
;;    ((lamp         1000))))

(defun adv-budget (gp catalog)
  "Filter catalog for items you can afford."
  (remove-if (lambda (price) (< gp price)) catalog :key 'second))

(defparameter *vendor-armor-catalog*
  '((no-armor 0) (leather 1250) (chainmail 1500) (plate 2000))
  "Armor and prices available at vendors.")

(defun buy-armor-from-vendor (adv)
  "The adventure may buy armor from a castle vendor."
  (with-accessors ((av adv-av) (gp adv-gp) (race adv-race)) adv
    (when (< 1249 gp)
      (let ((catalog (adv-budget gp *vendor-armor-catalog*)))
	(wiz-format *wiz-out* "Ok ~A, you have ~D and ~A."
		    race
		    gp
		    (get-armor-text av))
	(with-player-input
	    (armor (make-prompt-catalog "armor" #'get-armor-text catalog))
	  (case armor
	    (#\P (if (find 'plate catalog :key 'first)
		     (buy-equipment 'plate (get-catalog-price 'plate catalog) adv)
		     (wiz-error "You can't afford plate")))
	    (#\C (if (find 'chainmail catalog :key 'first)
		     (buy-equipment 'chainmail (get-catalog-price 'plate catalog) adv)
		     (wiz-error "You haven't got that much cash")))
	    (#\L (buy-equipment 'leather (get-catalog-price 'plate catalog) adv))
	    (#\N 'no-armor)
	    (T   (setf armor (wiz-error "Don't be silly. Choose a selection")))))))))

(defparameter *vendor-weapons-catalog*
  '((no-weapon 0) (dagger 1250) (mace 1500) (sword 2000))
  "Weapons and prices available at vendors.")

(defun buy-weapon-from-vendor (adv)
  "The adventurer may buy a weapon from a castle vendor."
  (with-accessors ((wv adv-wv) (gp adv-gp)) adv
    (when (< 1249 gp)
      (let ((catalog (adv-budget gp *vendor-weapons-catalog*)))
	(wiz-format *wiz-out* "You have ~D GP's left and ~A in hand."
		    gp
		    (get-weapon-text wv))
	(with-player-input
	    (weapon (make-prompt-catalog "weapon" #'get-weapon-text catalog))
	  (case weapon
	    (#\S (if (find 'sword catalog :key 'first)
		     (buy-equipment 'sword (get-catalog-price 'sword catalog) adv)
		     (wiz-error "Dungeon express card - you left home without it!")))
	    (#\M (if (find 'mace catalog :key 'first)
		     (buy-equipment 'mace  (get-catalog-price 'sword catalog) adv)
		     (wiz-error "Sorry sir, I don't give credit")))
	    (#\D (buy-equipment 'dagger (get-catalog-price 'sword catalog) adv))
	    (#\N 'no-weapon)
	    (T (wiz-error "Try choosing a selection"))))))))

(defun buy-potions-from-vendor (adv)
  "The adventurer may buy potions from a castle vendor."
  (let ((price 1000)) 
    (with-accessors ((gp adv-gp) (st adv-st) (iq adv-iq) (dx adv-dx)) adv
      (when (<= price gp)
	(loop
	   for (attr name) in '((adv-iq "intelligence") (adv-st "strength")
				(adv-dx "dexterity"))
	   with delta = 0
	   do
	     (setf delta (random-range 1 6))
	     (wiz-prompt "~2&Want to buy a potion of ~A for ~D GP's "
			 name price)
	     (when (equal (wiz-read-char) #\Y)
	       (decf-adv-inv gp price)
	       (funcall (fdefinition (list 'setf attr))
			(incf-adv-rank delta (funcall attr adv)) adv)
	       (wiz-format *wiz-out* "~2&Your ~A is now ~D"
			   name (funcall attr adv)))
	   until (< gp price))))))

(defun buy-lamp-from-vendor (adv)
  (let ((price 1000))
    (with-accessors ((lf adv-lf) (gp adv-gp)) adv
      (when (<= price gp)
	(when (wiz-y-or-n-p
	       (format Nil "Want a lamp for have ~D GP's " price))
	  (buy-equipment 'lamp price adv)
	  (wiz-write-line "Its guaranteed to outlive you!"))))))

(defun trade-with-vendor (adv)
  (with-accessors ((gp adv-gp) (race adv-race)
		   (wv adv-wv) (av adv-av)
		   (fl adv-fl) (st adv-st)
		   (iq adv-iq) (dx adv-dx)) adv
    (sell-treasures-to-vendor adv)
    (if (< gp 1000)
	(wiz-format *wiz-out* "You're too poor to trade, ~A" race))
    (progn
      (buy-armor-from-vendor   adv)
      (buy-weapon-from-vendor  adv)
      (buy-potions-from-vendor adv)
      (buy-lamp-from-vendor    adv))))

(defun adv-ignored-vendor ()
  "What happens when the adventurer ignores a vendor? (Nothing.)"
  (values
   (make-history
    (make-event 'adv-ignored 'vendor))))

(defun adv-meets-vendor (castle)
  "The adventurer encounters a vendor."
  (with-accessors ((adv cas-adventurer)) castle
    (if (cas-vendor-fury castle)
	(adv-meets-adversary castle)
	(with-player-input
	    (choice
	     (make-prompt-adv-choice "You may trade with, attack, or ignore the vendor"))
	  (cond
	    ((eq choice #\T) (trade-with-vendor adv))
	    ((eq choice #\I) (adv-ignored-vendor))
	    ((eq choice #\A)
	     (wiz-write-line "You'll be sorry you did that")
	     (setf (cas-vendor-fury castle) T)
	     (adv-meets-adversary castle))
	    (T   (setf choice (wiz-error "Nice shot, ~A" (adv-race adv)))))))))

(defun adv-finds-room (castle)
  "What happens when the adventurer enters any other kind of room"
  (assert (typep castle 'castle))
  Nil
  ;; (make-history
  ;;  (make-event 'adv-found (cas-creature-here castle))))
  )

(Defun make-message-adv-left-castle (castle event)
  "What does the game report to the player when the adventurer leaves
the castle."
  (assert (event-type-p event 'adv-leaves-castle))
  (format Nil
	  "~&You left the castle with~:[out~;~] the Orb of Zot"
	  (adv-of (cas-adventurer castle))
	  ;; (event-type-p event '(adv-leaves-castle orb-of-zot))
	  ))


(defconstant +help-text-dos+
  (format Nil
	  "~&*** WIZARD'S CASTLE COMMAND AND INFORMATION SUMMARY ***~2%~
             The following commands are available :~2%~
             Help     North    South    East     West     Up~%~
             Down     DRink    Map      Flare    Lamp     Open~%~
             Gaze     Teleport Quit~2%~
             The contents of rooms are as follows :~2%~
             . = Empty Room      B = Book            C = Chest~%~
             D = Stairs Down     E = Entrance/Exit   F = Flares~%~
             G = Gold Pieces     M = Monsters        O = Crystal Orb~%~
             P = Magic Pool      S = Sinkhole        T = Treasure~%~
             U = Stairs Up       V = Vendor          W = Warp/ORB OF ZOT~2%~
             The benefits of having treasures are :~2%~
             RUBY READ  - Avoid LETHARGY     PALE PEARL - Avoid LEECH~%~
             GREEN GEM  - Avoid FORGETTING   OPAL EYE   - Cures BLINDNESS~%~
             BLUE FLAME - Dissolves BOOKS    NORN STONE - No Benefit~%~
             PALANTIR   - No Benefit         SILMARIL   - No Benefit")
  "Some help documentation")

(defparameter *wiz-help* Nil)

(defun player-help ()
  "Report help for game."
  (assert (not (null *wiz-help*)))
  (values
   (make-history (make-event 'player-views 'help))
   *wiz-help*))

(defparameter *without-item-texts*
  (list
   :without-flares "Hey bright one, you're out of flares"
   :without-lamp   (lambda (race-ref)
		     (format Nil "You don't have a lamp ~A"
			     (get-race-text race-ref)))
   :without-runestaff "You can't teleport without the runestaff")
  "Messages when the adventurer tries something without the necessary item.")

(defun get-message (message-key messages)
  (getf messages (intern (string message-key) 'keyword)))

(defun adv-without-item-p (adv item-ref)
  (ecase item-ref
    (gold-pieces (zerop (adv-gp adv)))
    (flares      (zerop (adv-fl adv)))
    (weapon      (zerop (adv-wv adv)))
    (armor       (zerop (adv-av adv)))
    (lamp        (null (adv-lf adv)))
    (runestaff   (null (adv-rf adv)))
    (orb-of-zot  (null (adv-of adv)))))

(defun adv-tried-without-item (castle item &rest args)
  (values
   (record-events (cas-history castle) (make-event 'adv-tried 'without item))
   (wiz-error
    (let ((text-ref (get-message item *without-item-texts*)))
      (etypecase text-ref
	(string text-ref)
	(function (funcall text-ref args)))))))


(defparameter *wrong-room-messages*
  (list
   :crystal-orb "No orb - no gaze"
   :open        "The only think you opened was your big mouth."
   :pool        "If you want a drink find a pool"
   :stairs      (lambda (race-ref creature-ref)
		  (format Nil "Oh ~A, no ~A in here"
			  (get-race-text race-ref)
			  (get-creature-text creature-ref))))
  "Messages when the adventure tries something in the wrong room.")

(defun wrong-room-p (castle coords creature)
  (not (castle-creature-p castle coords creature)))

(defun adv-tried-wrong-room (castle action coords &rest args)
  (values
   (record-events (cas-history castle)
		  (make-event 'adv-tried action (get-castle-creature castle coords)))
   (let ((message (get-message action *wrong-room-messages*)))
     (etypecase message
       (string message)
       (function
	(apply message args))))))

(defun adv-tried-blind (castle action)
  "Return events and message when the adventurer tries something when blind."
  (assert (find action '(crystal-orb lamp flare map)))
  (values
   (record-events (cas-history castle)
		  (make-event 'adv-tried action 'blind))
   (wiz-error "You can't see anything, dumb ~A"
	      (adv-race (cas-adventurer castle)))))


(defun you-are-at (coords &optional (stream Nil))
  "Make message 'You are at ...'"
  (apply #'format stream "~&You are at (~D,~D) Level ~D~%"
	 (wiz-coords coords)))

(defun make-level-map (castle level)
  "Make a level map."
  (with-accessors ((here cas-adv-here)) castle
    (let ((icon-map
	   (loop
	      for y from 0 to 7
	      collect
		(loop
		   for x from 0 to 7
		   collect
		     (make-map-icon-room castle (list level y x))))))
      (when (eq level (first here))
	(destructuring-bind (y x) (rest here)
	  (setf (elt (elt icon-map y) x)
		(make-map-icon-adv castle here))))
    (with-output-to-string (level-map)
      (format level-map "~2&~{~:}" "~&~{~A~}~%" icon-map)))))

;;; FIXME: castle size dependant code

(defun adv-uses-map (castle)
  "Show the player the map the adventurer has been making."
  (with-accessors ((adv cas-adventurer) 
		   (here cas-adv-here)) castle
    (let ((events (make-history))
	  (message ""))
      (cond ((blind-p adv)
	     (adv-tried-blind castle 'map))
	    (T
	     (let ((level (first here)))
	       (record-events events
			      (make-event 'adv-viewed-map level))
	       (push-text message
			  (with-output-to-string (level-map)
			    (wiz-format level-map
					(make-level-map castle level))
			    (you-are-at (cas-adv-here castle) level-map))))))
      (values events message))))

(defun get-near-coords (castle coords)
  "Return a list of all coordinates near COORD."
  (loop
     for vy from -1 to 1
     append
       (loop
	  for vx from -1 to 1
	  collect
	    (add-castle-vectors
	     (cas-rooms castle) coords (list 0 vy vx)))))

(defun adv-uses-flare (castle)
  "What happens when the adventurer uses a flare."
  (with-accessors ((adv cas-adventurer)
		   (here cas-adv-here)
		   (rooms cas-rooms)) castle
    (cond
      ((blind-p adv) (adv-tried-blind castle 'flare))
      ((adv-without-item-p adv 'flares)
       (adv-tried-without-item castle 'without-flares))
      (T
	 (decf-adv-inv (adv-fl adv))
	 (let ((near-coords (get-near-coords castle here))
	       (events (make-history))
	       (message ""))
	   (record-events events
			  (make-event 'adv-used 'flare)
			  (make-event 'adv-mapped near-coords))
	   (loop
	      for near in near-coords
	      do (cas-adv-map-room castle near))
	   (push-text message
		      (with-output-to-string (text)
			(wiz-format text "~2&~{~:}" "~&~3@{ ~A~}~%" 
				    (loop
				       for near in near-coords
				       collect
					 (get-castle-creature-icon castle near)))
			(you-are-at (cas-adv-here castle) text)))
	   (values events message))))))


;; TODO: Unlike most actions, input errors when using the lamp cycle
;; back to the main loop. This could be fixed.


(defun adv-uses-lamp (castle)
  "What happens when the adventurer tries to use the lamp."
  (with-accessors ((adv cas-adventurer)) castle
    (cond
      ((blind-p (cas-adventurer castle))
       (adv-tried-blind castle 'lamp))
      ((adv-without-item-p adv 'lamp)
       (adv-tried-without-item castle 'without-lamp (adv-rc adv)))
      (T
       (let ((direction
	      (wiz-read-direction
	       "Where do you shine the lamp (N,S,E, or W) "))
	     (events (make-history))
	     (message ""))
	 ;; No error or message if error comes from read-direction
	 (cond ((eq direction 'input-error)
		(record-events (cas-history castle)
			       (make-event 'player-error 'bad-lamp-direction))
		(wiz-error "Turkey! That's not a direction"))
	       (T
		(let* ((there (cas-adv-near castle direction))
		       (creature (get-castle-creature castle there)))
		  (cas-adv-map-near castle direction)
		  (record-events events
				 (make-event 'adv-used 'lamp there)
				 (make-event 'adv-mapped there creature))
		  (push-text message
			     (with-output-to-string (text)
			       (format text "~&The lamp shines into ~{(~D,~D) Level ~D~}"
				       (wiz-coords there))
			       (format text  "~&There you will find ~A"
				       (get-creature-text creature)))))))
	 (values events message))))))

(defparameter *drink-pool-outcomes*
  (list
   (list 'stronger    'make-adv-stronger "stronger")
   (list 'weaker      'make-adv-weaker   "weaker")
   (list 'smarter     'make-adv-smarter  "smarter")
   (list 'dumber      'make-adv-dumber   "dumber")
   (list 'nimbler     'make-adv-nimbler  "nimbler")
   (list 'clumsier    'make-adv-clumsier "clumsier")
   (list 'change-race 'change-adv-race
	 (lambda (race-ref)
	   (format Nil "become a ~A" (get-race-text race-ref))))
   (list 'change-sex   'change-adv-sex
	 (lambda (sex-ref)
	   (format Nil "turn into a ~A" (get-sex-text sex-ref)))))
  "All of the drink outcomes.")

(defun adv-drinks-pool (castle)
  "Return events and message from drinking from a magic pool."
  (with-accessors ((adv cas-adventurer)
		   (here cas-adv-here)) castle
    (destructuring-bind (outcome-type outcome-effect outcome-text)
	(if (castle-creature-p castle here 'pool)
	    (random-elt *drink-pool-outcomes*)
	    '(drink-error Nil "If you want a drink find a pool."))
      (let ((events (list (make-event 'adv-drank 'pool)))
	    (effect
	     (when outcome-effect
	       (cond
		 ((eq outcome-type 'change-race)
		  (funcall outcome-effect
			   adv (random-elt
				(remove (adv-rc adv) *races*))))
		 ((eq outcome-type 'change-sex)
		  (funcall outcome-effect
			   adv (random-elt
				(remove (adv-rc adv) *sexes*))))
		 (T (funcall outcome-effect adv (random-range 1 3)))))))
	(values
	 (if effect
	     (record-event events effect)
	     events)
	 (if (eq outcome-type 'drink-error)
	     (wiz-error outcome-text)
	     (format Nil "You take a drink and ~A"
		     (cond
		       ((eq outcome-type 'change-race)
			(funcall outcome-text (event-property effect)))
		       ((eq outcome-type 'change-sex)
			(funcall outcome-text (event-property effect)))
		       (T (format Nil "feel ~A" outcome-text))))))))))

(defparameter *gaze-mapper* 'naive
  "Crystal Orbs can tell you where stuff is or lie to you about what's
there. This info could mapped.")

(defun make-message-creature-at (creature coords)
  
  (format Nil "~A at ~{(~D,~D) Level ~D~}"
	  (get-creature-text creature)
	  (wiz-coords coords)))

(defun gaze-mapper (adv coords creature)
  "The adventurer could map what he sees in the crystal orbs."
  (assert (typep *gaze-mapper* 'symbol))
  (labels ((gaze-map-naive ()
	     (adv-map-room adv coords creature))
	   (gaze-map-ask ()
	     (when (wiz-y-or-n-p
		    (format Nil "Do you wish to map ~A "
			    (make-message-creature-at creature coords)))
	       (adv-map-room adv coords creature)))
	   (gaze-map-smart ()
	     (unless (adv-room-mapped-p adv coords)
	       (adv-map-room adv coords creature)))
	   (gaze-map-skeptic ()
	     (unless (adv-room-mapped-p adv coords)
	       (gaze-map-ask))))
    (ecase *gaze-mapper*
      (naive (gaze-map-naive))
      (ask (gaze-map-ask))
      (smart (gaze-map-smart))
      (skeptic (gaze-map-skeptic)))))

(defparameter *gaze-crystal-orb-outcomes*
  (list
   (list 'heap 'make-adv-weaker "You see yourself in a bloody heap.")
   (list 'room 'gaze-mapper (lambda (creature-ref room-coords)
			      (format Nil "You see ~A"
				      (make-message-creature-at
				       creature-ref room-coords))))
    (list 'orb-of-zot Nil (lambda (room-coords)
			    (format Nil "You see ~A"
				    (make-message-creature-at
				     'orb-of-zot room-coords))))
    (list 'drink Nil (lambda (monster-ref)
		       (format Nil
			       "yourself drinking from a pool and becoming ~A"
			       (get-creature-text monster-ref))))
    (list 'soap Nil "a soap opera rerun")
    )
  "The visions in the crystal orb.")

  
(defun adv-uses-crystal-orb (castle)
  "Return events and message of what happens when the adventurer gazes into the orb."
  (with-accessors ((adv cas-adventurer)
		   (here cas-adv-here)) castle
    (cond ((blind-p adv) (adv-tried-blind castle 'crystal-orb))
	  ((wrong-room-p castle here 'crystal-orb)
	   (adv-tried-wrong-room castle here 'crystal-orb))
	  (T
	   (destructuring-bind (outcome-type outcome-effect outcome-text)
	       (random-elt *gaze-crystal-orb-outcomes*)
	     (let ((events (list (make-event 'adv-used 'crystal-orb)))
		   (effect
		    (cond
		      ((eq outcome-type 'heap)
		       (funcall outcome-effect adv (random-range 1 2)))
		      ((eq outcome-type 'room)
		       (let* ((coords (random-array-subscripts
				       (cas-rooms castle)))
			      (creature (get-castle-creature castle coords)))
			 (when *gaze-mapper*
			   (funcall outcome-effect adv coords creature))))
		      (T Nil))))
	       (values
		(if effect
		    (record-event events effect)
		    events)
		(format Nil "You see ~A"
			(cond
			  ((eq outcome-type 'drink)
			   (funcall outcome-text (random-monster)))
			  ((eq outcome-type 'room)
			   (destructuring-bind (coords creature)
			       (event-property effect 'adv-mapped)
			     (funcall outcome-text coords creature)))
			  ((eq outcome-type 'orb-of-zot)
			   (funcall outcome-text 
				    (if (< (random-range 1 8) 4)
					(cas-loc-orb castle)
					(random-array-subscripts
					 (cas-rooms castle)))))
			  (T outcome-text))))))))))


(defparameter *open-book-outcomes*
  (list
   (list 'flash-trap 'adv-springs-flash-trap
	 (lambda (race-ref)
	   (format Nil "FLASH! Oh no! You are now a blind ~A"
		   (get-race-text race-ref))))
   (list 'poetry   Nil "its another volume of Zot's Poetry! - Yeech!")
   (list 'magazine Nil (lambda (race-ref)
		   (format Nil "its an old copy of Play~A"
			   (get-race-text race-ref))))
   (list 'dexterity-manual 'adv-reads-dexterity-manual "dexterity")
   (list 'strength-manual  'adv-reads-strength-manual "strength")
   (list 'glue-trap 'adv-springs-glue-trap
	 (format nil "the book sticks to your hands -~&~
                      Now you can't draw your weapon")))
  "All the outcomes of opening books")

(defun adv-opens-book (castle)
  "Return events and message of what happens when the adventurer opens a book."
  (with-accessors ((adv cas-adventurer)) castle
    (destructuring-bind (outcome-type outcome-effect outcome-text)
	(random-elt *open-book-outcomes*)
      (let ((events (make-history (make-event 'adv-opened 'book)))
	    (effect
	     (when outcome-effect
	       (cond
		 ((eq outcome-type 'glue-trap)
		  (funcall outcome-effect adv 'book))
		 (T
		  (funcall outcome-effect adv))))))
	(when effect
	  (join-histories events effect))
	(values
	 events
	 (format Nil "You open the book and~%~A"
		 (cond
		   ((eq outcome-type 'flash-trap)
		    (funcall outcome-text (adv-rc adv)))
		   ((find outcome-type '(dexterity-manual strength-manual))
		    (format Nil "It's a manual of ~A" outcome-text))
		   ((eq outcome-type 'magazine)
		    (funcall outcome-text (random-race)))
		   (T outcome-text))))))))

(defparameter *open-chest-outcomes*
    (list
     (list 'bomb-trap   'adv-springs-bomb-trap "KABOOM! It explodes")
     (list 'gas-trap    'adv-springs-gas-trap "Gas! You stagger from the room")
     (list 'gold-pieces 'make-adv-richer
	   (lambda (gps)
	     (format Nil "Find ~D gold pieces" gps))))
  "All the outcomes of opening chests.")

(defun adv-opens-chest (castle)
  "Return events and messages when adventurer opens a chest."
  (with-accessors ((adv cas-adventurer)
		   (here cas-adv-here)) castle
    (destructuring-bind (outcome-type outcome-effect outcome-text)
	(get-outcome (random-elt '(bomb-trap gas-trap
				   gold-pieces gold-pieces))
		     *open-chest-outcomes*)
      (let ((events (make-history (make-event 'adv-opened 'chest)))
	    (effect
	     (case outcome-type
	       (gold-pieces (make-history
			     (funcall outcome-effect
				      adv (random-range 1 1000))))
	       (bomb-trap (funcall outcome-effect
				   adv))
	       (gas-trap (funcall outcome-effect castle)))))
	(when effect
	  (join-histories events effect))
	(values
	 events
	 (format Nil "You open the chest and ~A~%"
		 (if (eq outcome-type 'gold-pieces)
		     (funcall outcome-text
			      (event-property (latest-event events)))
		     outcome-text)))))))

(defun adv-opens (castle)
  "What happens when an adventurer opens a book or chest."
  (with-accessors ((here cas-adv-here)) castle
    (let ((creature (get-castle-creature castle here)))
      (multiple-value-bind (events message)
	  (case creature
	    (chest (adv-opens-chest castle))
	    (book  (adv-opens-book castle))
	    (T
	     (values
	      (make-history (make-event 'adv-opened creature))
	      (wiz-error "The only thing you opened was your big mouth"))))
	(clear-castle-room castle here)
	(record-event events (cas-adv-map-here castle))
	(values events message)))))

;;; Adventurer moves or is moved

(defun adv-walks (castle direction)
  "What happens when the adventure walks in a direction."
  (with-accessors ((adv cas-adventurer)
		   (here cas-adv-here)
		   (history cas-history)) castle
    (let ((room-type (get-castle-creature castle here)))
      (cond ((and (equal direction 'north)
		  (equal room-type 'entrance))
	     (record-events history
			    (make-event 'adv-walked direction)
			    (make-event 'adv-left-castle
					    (if (adv-of adv)
						'orb
						'no-orb))))
	    ((or
	      (and (equal direction 'up)
		   (wrong-room-p castle here 'stairs-up))
	      (and (eq direction 'down)
		   (wrong-room-p castle here 'stairs-down)))
	     (adv-tried-wrong-room castle here 'walk
				   (adv-race adv) (get-creature-text room-type)))
	    (T
	     (values
	      (make-history (make-event 'adv-walked direction)
			    (move-adv castle direction))))))))

(defun read-castle-coordinates ()
  (let* ((min (if (eq *cas-coords* 'zot) 1 0))
	 (max (if (eq *cas-coords* 'zot) 8 7))
	 (error-text (format Nil "Try a number from ~D to ~D" min max))
	 (prompts (list (format Nil "~A-coord (~D=far west  ~D=far east ) "
				(if (eq *cas-coords* 'zot) "Y" "X") min max)
			(format Nil "~A-coord (~D=far north ~D=far south) "
				(if (eq *cas-coords* 'zot) "X" "Y") min max)
			(format Nil "Level   (~D=top       ~D=bottom   ) "
				min max))))
    (when (eq *cas-coords* 'zot)
      (rotatef (nth 0 prompts) (nth 1 prompts)))
    (let ((coords
	   (loop
	      for prompt in prompts
	      collect
		(with-player-input (coord prompt :readf #'wiz-read-n)
		  (cond ((typep coord (list 'integer min max))
			 coord)
			(T (setf coord (wiz-error error-text))))))))
      ;; (when (eq *cas-coords* 'zot)
      ;; 	(rotatef (nth 1 coords) (nth 2 coords)))
      (if (eq *cas-coords* 'zot)
	  (unwiz-coords coords)
	  (reverse coords)))))

(defun make-adv-teleport ()
  (let ((coords (read-castle-coordinates)))
    (values
     (make-event 'adv-teleported)
     (send-adv coords))))

(defun adv-uses-runestaff (castle)
  "What happens when an adventurer teleports."
    ;; FIXME: castle size dependant code, prompts really only need to
    ;; be generated once, etc
    (with-accessors ((adv cas-adventurer)) castle
      (destructuring-bind (outcome-name outcome-effect outcome-text)
	  (if (runestaff-p adv)
	      (list 'adv-teleports #'make-adv-teleport 'Nil)
	      (list 'no-runestaff Nil "You can't teleport without the Runestaff!"))
	(let ((events ())
	      (message ""))
	  (values
	   (cond ((eq outcome-name 'adv-teleports)
		  (record-events events (make-event 'adv-used 'runestaff))
		  (record-events events (funcall outcome-effect)))
		 (T
		  (record-events events (make-event 'adv-used 'no-runestaff))))
	   (if (eq outcome-name 'adv-teleports)
	       message
	       outcome-text))))))

;;; Lines 1670 - 1780 print status, room eval

(defun make-message-adv-enters-room (adv here creature)
  "What does the game report to player when the adventurer enters a room."
  (with-output-to-string (status)
    (unless (adv-bl adv)
      (you-are-at here status))
    (with-accessors ((st adv-st) (iq adv-iq) (dx adv-dx)
		     (fl adv-fl) (gp adv-gp) (lf adv-lf)
		     (wv adv-wv) (av adv-av)) adv
      (format status "~2&~{~:}~%" "~A= ~A~^ "
	      (list "ST" st "IQ" iq "DX" dx
		    "Flares" fl "GP'S" gp))
      (format status "~&~A / ~A~@[ / a lamp~]~%"
	      (get-weapon-text wv) (get-armor-text av) lf)
      (format status "~2&Here you find ~A"
	      (get-creature-text creature)))))

(defun adv-enters-room (castle)
  "What happens when the adventurer enters a room?"
  (with-accessors ((adv cas-adventurer)
		   (here cas-adv-here)
		   (creature cas-creature-here)) castle
    (let ((message (make-message-adv-enters-room adv here creature)))
      (when (cas-room-cursed-p castle here)
	(join-histories (cas-history castle) (gain-curse castle))
	(when *curse-notify*
	  (push-text *curse-notify* message)))
      message)))

(defun adv-finds-creature (castle)
  "What happens when the adventurer encounters a creature?"
  (with-accessors ((adv cas-adventurer)
		   (here cas-adv-here)
		   (history cas-history)
		   (creature cas-creature-here)) castle
    (record-events history (make-event 'adv-found creature))
    (unless (adv-room-mapped-p adv here creature)
      (record-events history (cas-adv-map-here castle)))
    (multiple-value-bind (enter-room-events enter-room-message)
	(funcall
	   (symbol-function
	    (case (creature-type-of creature)
	      (gold-pieces 'adv-finds-gold-pieces)
	      (flares      'adv-finds-flares)
	      (warp        'adv-finds-warp)
	      (sinkhole    'adv-finds-sinkhole)
	      (treasure    'adv-finds-treasure)
	      (vendor      'adv-meets-vendor)
	      (monster     'adv-meets-adversary)
	      (T           'adv-finds-room)))
	    castle)
	(values
	 enter-room-events
	 enter-room-message))))

;; is the "your choice" prompt ever used with numerical input?

(defun adv-enters-castle (castle adv)
  "An adventurer enters the castle."
  (assert (typep adv    'adventurer))
  (assert (typep castle 'castle))
  (assert (null  (cas-history castle)))
  (assert (null  (cas-adventurer castle)))
  (setf (cas-adventurer castle) adv)
  (values
    (make-history
     (make-event 'adv-ate 'last-meal)
     (make-event 'adv-entered-castle)
     (send-adv +entrance+))
    (format Nil "~|~&Ok ~A, you enter the castle and begin.~%"
	    (adv-race (cas-adventurer castle)))))

;;; Turn lines 620 - 800

(defparameter *minor-turn-events*
  (list
   :adv-stepped-on "stepped on a frog"
   :adv-heard      (lambda ()
		     (format Nil "hear ~A"
			     (random-elt '("a scream" "footsteps"
					   "a wumpus" "thunder"))))
   :adv-sneezed    "sneezed"
   :adv-saw        "see a bat fly by"
   :adv-smelled     (lambda ()
		      (format Nil "smell ~A frying"
			      (random-monster)))
   :adv-felt       "feel like you are being watched"
   :game-announced "are playing Wizard's Castle")
  "Minor events at beginning of turn")

(defun get-minor-turn-event (event-name)
  (let ((text-ref
	 (getf *minor-turn-events* (intern (string event-name) 'keyword))))
    (etypecase text-ref
      (string text-ref)
      (function (funcall text-ref)))))

(defun random-minor-turn-event ()
  (get-minor-turn-event (random-elt (remove-if-not #'keywordp *minor-turn-events*))))

;; check game states
;; increment turn counter
;; check to see if curses apply (runestaff or orb protect you)
;; apply curses
;; lethergy increments the turn counter (ruby red keeps you awakes)
;; leech steals 1 to 5 GPs (countered by pale pearl)
;; forgetfulness (lethe?) erases a random place on your map (green gem)
;; little event (1 in 5 chance of something)
;; check up on handicaps if any
;; blindness cured by opal eye
;; book-glued-to-hand cured by blue flame
;; get input

;; gosub 3400 ->

(defun apply-curse (castle curse)
  "A curse strikes the adventurer."
  (with-accessors ((adv cas-adventurer)) castle
    (let ((outcome (funcall (get-castle-curse castle curse 'function) adv)))
      (when (event-p outcome)
	(record-events (cas-history castle) outcome)))))

(defun begin-turn (castle)
  "Every turn."
  (with-accessors ((adv cas-adventurer)
		   (history cas-history)) castle
    (with-accessors ((bl adv-bl) (bf adv-bf) (cr adv-cr)) adv
      (let ((events (make-history)))
	(loop
	   for curse in cr
	   do (apply-curse castle curse))
	(with-output-to-string (message)
	  (when (zerop (random 5))
	    (format message "~&You ~A" (random-minor-turn-event)))
	  (when (and bl (has-treasure-p adv 'opal-eye))
	    (setf bl Nil)
	    (record-events
	     events
	     (make-event 'adv-cured 'sight-restored 'opal-eye))
	    (format message "~A cures your blindness"
		    (get-creature-text 'opal-eye)))
	  (when (and bf (has-treasure-p adv 'blue-flame))
	    (setf bf Nil)
	    (record-events
	     history
	     (make-event 'adv-unbound 'book-burnt 'blue-flame))
	    (format message "~A dissolves the book"
			(get-creature-text 'blue-flame))))))))

(defun quit-game (&optional castle)
  "The player quits."
  (assert (typep castle 'castle)) ;FIXME: MAIN-EVAL passes a castle
				   ;object here, but it doesn't
				   ;otherwise get used.
  (let* ((events (make-history))
	 (message ""))
    (record-event events
		  (if (wiz-y-or-n-p "Do you really want to quit ")
		      (make-event 'player-quit-game)
		      (make-event 'player-error 'quit-canceled)))
    (unless (event-type-p (latest-event events) 'player-quit-game)
      (push-text message
		 (wiz-error "Then don't say that you do")))
    (values events message)))

(defun player-error (castle error-type &rest args)
  "Return event and messages when there's an input error."
  (values
   (make-history (make-event* 'player-error error-type args))
   (wiz-error (format Nil "Stupid ~A that wasn't a valid command"
		      (adv-race (cas-adventurer castle))))))

(defun main-read (&optional (stream *wiz-qio*))
  "The reader for the main input."
  (with-input-from-string (str (wiz-read-line stream))
    (let ((i (read-char str Nil))
	  (j (peek-char Nil str Nil)))
      (string-upcase
       (cond ((null i) "")
	     ((null j) (string i))
	     (T
	      (if (and (char-equal i #\D) (char-equal j #\R))
		  (concatenate 'string (list i j))
		  (string i))))))))

(defparameter *wiz-forms*
  '(adv-drinks-pool adv-walks
    adv-uses-map adv-uses-flare adv-uses-lamp
    adv-opens adv-uses-crystal-orb adv-uses-runestaff
    quit-game player-help player-error
    adv-enters-castle)
  "The various wiz-forms")

(defun wiz-form-p (obj)
  (find (first obj) *wiz-forms*))

(defun make-wiz-form (wiz-form-name &rest args)
  (list* wiz-form-name args))

(defun adv-moved-p (castle)
  "Has the adventurer been moved this turn (by a warp or sinkhole)?"
  (event-type-p (latest-event (cas-history castle))
		(make-event 'adv-entered-room)))

(defun vendor-here-p (castle)
  (eq (cas-creature-here castle) 'vendor))

(defun monster-here-p (castle)
  (creature-type-p (cas-creature-here castle) 'monster))

(defun main-eval (castle wiz-form)
  "Main evaluator"
  (assert (wiz-form-p wiz-form))
  (with-accessors ((history cas-history)) castle
    (loop
       do
	 (multiple-value-bind (events message)
	     (apply (first wiz-form) castle (rest wiz-form))
	   (join-histories history events)
	   (when message
	     (wiz-write-line message)))
	 (when (adv-moved-p castle)
	   (wiz-write-line (adv-enters-room castle))
	   (setf wiz-form (make-wiz-form 'adv-finds-creature)))
       until (not (adv-moved-p castle)))
    (begin-turn castle)))

(defun main-input ()
  "Gets input and returns a form to be evaluated with the castle."
  (with-player-input (input "Your move " :readf #'main-read)
    (cond
      ((equal input "DR") '(adv-drinks-pool))
      ((equal input "N")  '(adv-walks north))
      ((equal input "S")  '(adv-walks south))
      ((equal input "W")  '(adv-walks west))
      ((equal input "E")  '(adv-walks east))
      ((equal input "U")  '(adv-walks up))
      ((equal input "D")  '(adv-walks down))
      ((equal input "M")  '(adv-uses-map))
      ((equal input "F")  '(adv-uses-flare))
      ((equal input "L")  '(adv-uses-lamp))
      ((equal input "O")  '(adv-opens))
      ((equal input "G")  '(adv-uses-crystal-orb))
      ((equal input "T")  '(adv-uses-runestaff))
      ((equal input "Q")  '(quit-game))
      ((and *wiz-help* (or (equal input "H")
			   (equal input "?")))
       '(player-help))
      (T (make-wiz-form 'player-error 'main-input input)))))

(defun end-game-p (castle)
  (case (event-type-of (latest-event (cas-history castle)))
    (adv-slain        'death)
    (adv-left-castle  'exit)
    (player-quit-game 'quit)
    (T                Nil)))

(defun play-again-p ()
  (if (wiz-y-or-n-p "Play-again ")
      'player-plays)
      Nil)

(defun make-message-play-again (castle choice)
  (format Nil
	  (if (eq choice 'player-plays)
	      "Some ~A never learn~%"
	      "Maybe dumb ~A not so dumb after all~%")
	  (adv-race (cas-adventurer castle))))

(defun main (&key (adventurer Nil) (castle Nil)
	       (intro Nil) (help Nil))
  "The main game loop. If an adventurer is passed in, a castle also
passed in must not also have an adventurer already in it."
  ;; (setf *random-state* (make-random-state t))
  (launch intro)
  (wiz-write-line (make-message-title))
  (when help
    (setf *wiz-help* help))
  (loop
     with play-again = Nil
     do
       (setf castle (or castle (setup-castle)))
       (wiz-write-line
	(main-eval castle 
		   (list 'adv-enters-castle
			 (or adventurer (setup-adventurer)))))
       (with-accessors ((adv cas-adventurer)) castle
	 (loop
	    with ending = Nil
	    do
	    ;; (when (cas-adv-moved-p castle)
	    ;;   (enter-room castle))
	      (wiz-write-line (main-eval castle (main-input)))
	      (setf ending (end-game-p castle))
	    until (not (null ending))
	    finally
	      (wiz-write-line
	       (make-message-end-game adv ending
				      (count-turns (cas-history castle))))))
       (setf play-again (play-again-p))
       (wiz-write-line
	(make-message-play-again castle play-again))
       (when play-again
	 (setf adventurer Nil castle Nil))
     until (null play-again)))

(defun play-ohare (&rest args &key &allow-other-keys)
  (let ((*curse-notify* T))
    (apply #'main args)))

(defun play-stetson (&rest args &key &allow-other-keys)
  (apply #'main :intro +intro-text-dos+ :help +help-text-dos+ args))
	  


;;; TODO: figure out lisp getopts.

;;;; Test Environment

(defparameter *r* (make-random-state t)
  "Reusable random state for test environment.")

(defparameter *a* Nil
  "Test adventurer.")

(defparameter *z* Nil
  "Test castle (may or may not contain adventurer).")

(defun make-test-adv (&optional adv-name)
  "Make one of several pre-generated characters."
  (apply #'make-adventurer 
	 (case adv-name
	   (blind-adept (list :rc 'human  :sx 'female
			      :st 18 :iq 18  :dx 18
			      :wv  3 :av  3  :ah 21
			      :gp 20 :lf Nil :fl  0
			      :bl  T))
	   (bookworm    (list :rc 'hobbit :sx 'male
			      :st  6 :iq 18  :dx 18
			      :gp 20 :lf Nil :fl  0
			      :bf  T))
	   (valkyrie    (list :rc 'dwarf  :sx 'female
			      :st 16 :iq 14  :dx  8
			      :wv  2 :av 3   :ah 21
			      :gp 10 :lf Nil :fl 10))
	   (barbarian   (list :rc 'human  :sx 'male
			      :st 18 :iq  6  :dx 12
			      :wv  3 :av  1  :ah  7
			      :gp  0 :lf Nil :fl 10
			      :cr '(forget)))
	   (sorceress   (list :rc 'elf    :sx 'female
			      :st  6 :iq 18  :dx 12
			      :wv  1 :av  1  :ah  7
			      :gp  0 :lf  T  :fl 99
			      :rf  T
			      :cr '(lethargy)))
	   (tourist     (list :rc 'human  :sx 'male
			      :st  6 :iq 10  :dx 8
			      :gp 6000
			      :cr '(leech)))
	   (T           (list :rc 'human
			      :sx (random-sex (make-random-state T))
			      :st 11 :iq 10  :dx 11
			      :wv  2 :av  2  :ah 14
			      :gp  0 :lf  T  :fl  0)
			;; NOTE: the randomly chosen sex for the
			;; default adventurer uses a random state
			;; independant from *R*.
			))))

(defun map-all-rooms (&key (adv *a*) (castle *z*))
  "Maps all the rooms in a castle."
  (assert (typep castle 'castle))
  (assert (typep adv 'adventurer))
  (loop
     for ridx from 0 below (array-total-size (cas-rooms castle))
     do (adv-map-room adv ridx
		      (get-castle-creature castle ridx))))

(defun setup-test (&key adv-name map-all-rooms enter-castle)
  "Set or reset test environment."
  (let ((*random-state* (make-random-state *r*)))
    (setf *z* (setup-castle T))
    (setf *a* (make-test-adv adv-name))
    (when map-all-rooms
      (map-all-rooms :adv *a* :castle *z*))
    (when enter-castle
      (adv-enters-castle *a* *z*))
    (values *a* *z*)))

(defun test (&key (adventurer *a*) (castle *z*) (last-castle T)
	       (forget-type *forgetfulness*)
	       (curse-notify *curse-notify*)
	       (gaze-map *gaze-mapper*)
	       (cas-coords *cas-coords*)
	       (intro Nil) (help Nil)
	       (random-state (make-random-state *r*)))
  "Run a test game."
  (let* ((*random-state* random-state)
	 (*forgetfulness* forget-type)
	 (*curse-notify* curse-notify)
	 (*cas-coords* cas-coords)
	 (*gaze-mapper* gaze-map)
	 ;; (*wiz-intro* intro)
	 ;; (*wiz-help* help)
	 )
    (main :castle castle :adventurer adventurer
	  :help help :intro intro)
    (when last-castle castle)))
