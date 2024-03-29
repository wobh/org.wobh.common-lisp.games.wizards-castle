;;;; -*- mode: lisp; coding: utf-8 -*-
;;;; FILE: wizard.lisp

;;;; DESCRIPTION:

;;;; Wizard's Castle

;;;; Copyright (C) 1980 Joseph Power
;;;; Last revised - 4/12/80 11:10 PM

;;;; Adapted to Common Lisp by William Clifford

;;;; AUTHORS:

;;;; William Clifford [wc] wobh@yahoo.com

;;;; NOTES:

;;;; I intended to make it easier for extending the game and adding
;;;; options and features not originally available. However, I fear
;;;; what I've done is make a fairly baroque program with several
;;;; abstractions which may not be obviously useful.

;;;; See the README.org for more information.

;;;; MAKE-WIZ-FORM creates a lisp-like form which is evaluated in one
;;;; of the MAIN-EVAL or FIGHT-EVAL. Evaluated, it should return a
;;;; HISTORY of events and, optionally a message.

;;;; MAKE-EVENT makes an event which is a lisp-like list describing
;;;; the changes in game-state.

;;;; MAKE-HISTORY makes a history of events, which currently is just a
;;;; list of the arguments in reverse order. Arguments, when present,
;;;; must be valid events by EVENT-P.

;;;; JOIN-HISTORY is a way of extending a given event-history with a
;;;; history of additional events. Any wiz-form calling on other
;;;; wiz-forms should use JOIN-HISTORY to merge it's current events
;;;; with ones from other wiz-forms.

;;;; MAKE-TEXT and PUSH-TEXT do similiar work for strings. Any
;;;; wiz-form calling on other wiz-forms should use PUSH-TEXT to
;;;; extent the current message.

;;;; with the exception of WIZ-ERROR messages, evaluators of wizforms
;;;; will output the final message and join the form history to the
;;;; castle history. See MAIN-EVAL.

;;;; To extend the game you'll want to make OUTCOMES, (not yet
;;;; documented, see examples) and create the appropriate forms.

;;;; REFERENCES:

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

(defpackage #:org.wobh.common-lisp.games.wizards-castle
  (:nicknames #:wizards-castle #:wizard #:zot)
  (:use #:common-lisp)
  (:export #:play #:play-ohare #:play-stetson)
  (:documentation "ORG.WOBH.COMMON-LISP.GAMES.WIZARDS-CASTLE

Joseph Power's _Wizard's Castle_"))

(in-package #:org.wobh.common-lisp.games.wizards-castle)


;;;; Randomess functions

;;; Powers, 1980, pg 14, ln# 70
;;; DEFFNA(Q)=1+INT(RND(8)*Q)

(defun random-array-subscripts (array &optional (random-state *random-state*))
  "Create a list random subscripts in array."
  (flet ((randomn (n)
           (random n random-state)))
    (mapcar #'randomn
            (array-dimensions array))))

(defun random-aref (array &optional (random-state *random-state*))
  "Get random element of array."
  (row-major-aref array
                  (random (array-total-size array)
                          random-state)))

(defun random-elt (seq &optional (random-state *random-state*))
  "Get random element from sequence."
  (let ((seq-len (length seq)))
    (unless (zerop seq-len)
      (elt seq (random seq-len random-state)))))

(defun random-whole (limit &optional (random-state *random-state*))
  "Return random integer n, 0 < n <= `limit'."
  (1+ (random limit random-state)))

(defun random-range (min max &optional (random-state *random-state*))
  "Return random interger n `min' <= n <= `max'."
  (+ min (random (- max min) random-state)))

(defun nshuffle (seq &optional (random-state *random-state*))
  "Destructively Knuth shuffle a sequence."
  (let ((len (length seq)))
    (dotimes (i len seq)
      (rotatef (elt seq i)
               (elt seq (+ i (random (- len i) random-state)))))))

(defun shuffle (seq &optional (random-state *random-state*))
  "Non-destructively Knuth shuffle a sequence."
  (nshuffle (copy-seq seq) random-state))

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
                           (array-dimensions array)))
          value))
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
  (flet ((trunconc (elt acc)
           (nconc
            (multiple-value-list (truncate (car acc) elt))
            (cdr acc))))
    (reduce #'trunconc
            (cdr (array-dimensions array))
            :initial-value (list index)
            :from-end t)))

;;; array filter

(defun filter-array-indices (predicate an-array)
  "Make a list of array indices which elements pass some test"
  (loop
     for index from 0 below (array-total-size an-array)
     when (funcall predicate (row-major-aref an-array index))
     collect index))


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

(defparameter *all-caps*
  "~:@(~@?~)"
  "Format string for WIZ-FORMAT when ZOT SPEAKS IN ALL CAPS")

(defparameter *mixed-case*
  "~@?"
  "Format string for WIZ-FORMAT when Zot speaks normally")

(defparameter *wiz-format-string*
  *all-caps*
  ;; +mixed-case+    ; for a less obnoxious Zot
  "Format string for WIZ-FORMAT.")

;;; FIXME: feels a bit like reinventing the wheel here

(defparameter *wiz-width* 64)
(defparameter *wiz-out* *standard-output*)
(defparameter *wiz-err* *standard-output*)

(defun wiz-format (stream str &rest args)
  "Format a string for output in wizard's castle."
  (apply #'format stream *wiz-format-string* str args))

(defun wiz-format-error (stream string &rest args)
  "Write a formatted error message to STREAM."
  (wiz-format stream "~2&** ~?" string args))

(defun wiz-read-char (&optional (stream *standard-input*))
  "Read the upcased first character from whatever entered."
  (char-upcase (read-char stream)))

(defun wiz-read-n (&optional (stream *standard-input*))
  "Read a number from whatever entered."
  (parse-integer (read-line stream) :junk-allowed t))

(defun make-prompt-adv-choice (&optional intro)
  "Make a prompt with 'Your choice' at the end."
  (let ((prompt "Your choice "))
    (if intro
        (format nil "~2&~A~2&~(~A~)" intro prompt)
        (format nil "~2&~A" prompt))))

(defmacro with-player-input
    ((var prompt &key
          (readf '#'wiz-read-char) (istream '*standard-input*)
          (writef '#'wiz-format)   (ostream '*standard-output*))
     &body body)
  "Read input to var and do something with it or set var to nil if a
different input is needed."
  (let ((out (gensym "OUTCOME")))
    `(loop
        with ,var = nil
        with ,out = nil
        do
          (funcall ,writef ,ostream ,prompt)
          (finish-output ,ostream)
          (clear-input ,istream)
          (setf ,var (funcall ,readf ,istream))
          (setf ,out ,@body)
        until (not (null ,var))
        finally (return ,out))))

(defun wiz-y-or-n-p (prompt &optional message)
  "Return t, nil or requery if answer is Y, N, or something else."
  (when message
    (wiz-format *wiz-out* message))
  (with-player-input (input prompt :readf #'wiz-read-char)
    (case input
      (#\Y t)
      (#\N nil)
      (t (setf input
               (wiz-format-error *wiz-err*
                                 "Answer yes or no "))))))

(defun wiz-y-p (prompt &optional message)
  (when message
    (wiz-format *wiz-out* message))
  (with-player-input (input prompt :readf #'wiz-read-char)
    (case input
      (#\Y t)
      (t nil))))

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
      (t (if input-error-message
             (setf direction
                   (wiz-format-error *wiz-err*
                                     input-error-message))
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
    adv-drank-potion
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
    adv-donned adv-doffed
    adv-wielded
    adv-armor-damaged adv-weapon-broke adv-armor-destroyed
    adv-changed
    trap-sprang
    chest-expoded
    adv-left-castle
    adv-slain
    player-quit-game
    player-error
    player-viewed-help)
  "List of events")

(defun event-p (obj)
  "Verify that the object is an event."
  (find
   (typecase obj
     (list   (first obj))
     (t obj))
   *events*))

(defun make-event (&rest args)
  "An event is a list describing what happened."
  (assert (find (first args) *events*))
  args)

(defun make-event* (&rest args)
  (assert (find (first args) *events*))
  (apply #'list* args))

;;(defun name-of-event (event)
(defun name-of-event (event)
  "Get event type info."
  (assert (event-p event))
  (first event))

;;(defun event-name-p (event name-ref)
(defun event-kind-p (event-check event)
  "Check that an event has a particular properties."
  (assert (event-p event))
  (etypecase event-check
    (symbol (eql event-check (first event)))
    (list   (let ((index (mismatch event event-check)))
              (or (null index)
                  (= index (length event-check)))))))

(defun data-of-event (event &optional data-ref)
  "Get data about event."
  (assert (event-p event))
  (let ((subtype (rest event)))
    (etypecase data-ref
      (null subtype)
      (symbol (member data-ref subtype))
      ((integer 0) (subseq subtype data-ref)))))

(defun value-of-event (event &optional value-ref)
  "Get most specific information about event"
  (assert (event-p event))
  (etypecase value-ref
    (null (first (last event)))
    (symbol (rest (member value-ref event)))
    ((integer 0) (last event value-ref))))


;;; History

(defun history-p (events-list)
  "Is every element of EVENTS-LIST an event?"
  (or nil (every #'event-p events-list)))

(defun make-history (&rest events)
  "A history is push-down stack of events (returns reversed list of events)."
  (assert (history-p events))
  (reverse events))

(define-modify-macro record-event (event)
  (lambda (history event)
    (assert (event-p event))
    (push event history))
  "Record an event to history.")

(define-modify-macro record-events (&rest events)
  (lambda (history &rest events)
    (setf history (revappend events history)))
  "Record events to history.")

(define-modify-macro join-history (new-history)
  (lambda (old-history new-history)
    (setf old-history (append new-history old-history)))
    ;; (dolist (history histories old-history)
    ;;   (setf old-history (revappend history old-history))))
  "Combine histories into the old history.")

(defun count-events (history)
  "Count the number of turns in history."
  (count-if #'event-p history :key 'first))

(defun events-since (event-check history)
  "History since event."
  (assert (event-p event-check))
  (let ((index (position event-check history :test #'event-kind-p)))
    (when (typep index '(integer 0))
      (butlast history (- (length history) (1+ index))))))

(defun find-event (event-check history)
  "Search history for event."
  (flet ((event-check-p (event)
           (event-kind-p event-check event)))
    (find-if #'event-check-p history)))

(defun latest-event (history)
  "Last event in history."
  (first history))

(defun latest-event-p (event-check history)
  "Is the latest event in history the kind of event expected?"
  (if (null history)
      nil
      (event-kind-p event-check (latest-event history))))

(defun oldest-event (history)
  "First event in history."
  (first (last history)))

;; FIXME: no one calls this
(defun oldest-event-p (event-check history)
  "Is the oldest event in history the kind of event expected?"
  (event-kind-p event-check (oldest-event history)))

(defun latest-creature-found (history)
  "What creature has been most recently found."
  (value-of-event
   (oldest-event
    (events-since 'adv-found history))))

(defparameter *command-events*
  '((adv-drank pool)
    adv-walked
    (adv-used map)
    (adv-used flare)
    (adv-used lamp)
    (adv-used crystal-orb)
    (adv-used runestaff)
    (adv-opened book)
    (adv-opened chest)
    player-quit)
  "List of events generated by commands")

;;; TODO: eventually this list will become the commands by threading
;;; the castle as the first argument eg (funcall adv-walked castle
;;; 'west)

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

(defparameter *wiz-title-plain*
  "THE WIZARD'S CASTLE")

(defparameter *wiz-title-fancy*
  (format nil "* * * ~A * * *" *wiz-title-plain*))

(defparameter *wiz-title*
  *wiz-title-plain*)

(defparameter *wiz-copyright-power*
  "Copyright (C) 1980 by Joseph R Power")

(defparameter *wiz-copyright*
  *wiz-copyright-power*)

(defparameter *wiz-revision-power*
  "Last Revised - 04/12/80")

(defparameter *wiz-revision-ohare*
  "Last Revised - 07/09/80  12:30 AM")

(defparameter *wiz-revision*
  *wiz-revision-power*)

(defparameter *wiz-adaptation-ohare*
  "Converted to PET by - John O'Hare")

(defparameter *wiz-adaptation*
  nil)

(defparameter *intro-text-dos*
  (format nil
          "Many cycles ago, in the kingdom of N'DIC, the gnomic~
         ~&wizard ZOT forged his great ORB of power. He soon vanished~
         ~&utterly, leaving behind his vast subterranean castle~
         ~&filled with esurient MONSTERS, fabulous TREASURES, and~
         ~&the incredible ORB of ZOT. From that time hence, many~
         ~&a bold youth has ventured into the WIZARD'S CASTLE. As~
         ~&of yet, NONE has ever emerged victoriously! BEWARE!!")
  "This intro is a slightly modified version of the article's introduction.")

(defparameter *intro-text-ohare*
  (format nil
          "codes:~
         ~@{~&~A=~A~20T~A=~A~}"
          #\. "an empty room"
          #\e "the entrance"
          #\u "stairs going up"
          #\d "stairs going down"
          #\p "a pool"
          #\c "a chest"
          #\g "gold pieces"
          #\f "flares"
          #\w "a warp"
          #\s "a sinkhole"
          #\o "a crystal orb"
          #\b "a book"
          #\v "vendor"
          #\m "any monster"
          #\t "any treasure"
          #\? "unexplored")
  )

(defparameter *wiz-intro* nil
  "Original game does not print an into like some later ones.")

(defun make-message-launch (&key
                              (width *wiz-width*)
                              (title *wiz-title*)
                              (copyright *wiz-copyright*)
                              (revision *wiz-revision*)
                              (adaptation *wiz-adaptation*)
                              (introduction *wiz-intro*))
  "Print title screen"
  (let ((stars (make-string width :initial-element #\*))
        (indent (1- (floor (- width (length title)) 2))))
    (with-output-to-string (message)
      (format message "~&~|~&")
      (format message "~&~A" stars)
      (format message "~2&~VT~A" indent title)
      (format message "~2&~A" stars)
      (format message "~@[~2&~A~]" copyright)
      (format message "~@[~2&~A~]" revision)
      (format message "~@[~2&~A~]" adaptation)
      (format message "~@[~2&~A~]" introduction)
      (format message "~%"))))

;; FIXME what should I do about form-feed (CHR$(12)? clear screen CLS?
;; Seems likely the revision date here means 1980-04-12

(defun launch ()
  (wiz-format *wiz-out* "~A" (make-message-launch)))


;;; TODO: filter list by contents of another list


;;;; Zot's creatures

;;; the documentation strings are from (Power 1980), pg 13

;;; FIXME: perhaps some of the parameters should be constants?

;;; FIXME: the first item on these lists is nil because the original
;;; basic source code used natural number indexes. To maintain
;;; consistency with the code, and prevent confusion when checking
;;; against it, the zeroth element has been set to nil.

;; |  n | symbol      | icon| text                | type     |
;; |----|-------------+-----+---------------------|----------|
;; |  0 | nil         | nil | nil                 | nil      |
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

(defparameter *creature-data*
  '((x           "x"                 #\?)
    (empty-room  "an empty room"     #\.)
    (entrance    "the entrance"      #\e)
    (stairs-up   "stairs going up"   #\u)
    (stairs-down "stairs going down" #\d)
    (pool        "a pool"            #\p)
    (chest       "a chest"           #\c)
    (gold-pieces "gold pieces"       #\g)
    (flares      "flares"            #\f)
    (warp        "a warp"            #\w)
    (sinkhole    "a sinkhole"        #\s)
    (crystal-orb "a crystal orb"     #\o)
    (book        "a book"            #\b)
    (kobold      "a kobold"          #\m)
    (orc         "an orc"            #\m)
    (wolf        "a wolf"            #\m)
    (goblin      "a goblin"          #\m)
    (ogre        "an ogre"           #\m)
    (troll       "a troll"           #\m)
    (bear        "a bear"            #\m)
    (minotaur    "a minotaur"        #\m)
    (gargoyle    "a gargoyle"        #\m)
    (chimera     "a chimera"         #\m)
    (balrog      "a balrog"          #\m)
    (dragon      "a dragon"          #\m)
    (vendor      "a vendor"          #\v)
    (ruby-red    "the Ruby Red"      #\t)
    (norn-stone  "the Norn Stone"    #\t)
    (pale-pearl  "the Pale Pearl"    #\t)
    (opal-eye    "the Opal Eye"      #\t)
    (green-gem   "the Green Gem"     #\t)
    (blue-flame  "the Blue Flame"    #\t)
    (palantir    "the Palantir"      #\t)
    (silmaril    "the Silmaril"      #\t)
    (runestaff   "the Runestaff"     nil)
    (orb-of-zot  "the Orb of Zot"    nil)
    )
  "All the possible castle contents")

(defun creature-p (creature)
  "Is the given symbol a creature?"
  (find creature (subseq *creature-data* 1 34) :key 'first))

;; (defun creature-data (creature-ref)
(defun get-creature-data (creature-ref)
  "Return the requested data about the creature."
  (let ((creature-data (find creature-ref *creature-data* :key #'first)))
    (assert (consp creature-data))
    creature-data))

;; (defun creature-value (creature-ref)
(defun value-of-creature (creature-ref)
  "Get the creature number."
  (position creature-ref *creature-data* :key #'first))

;; (creature-text (creature-ref)
(defun text-of-creature (creature-ref)
  "Get the creature text."
  (second (get-creature-data creature-ref)))

;; (creature-icon (creature-ref)
(defun icon-of-creature (creature-ref)
  "Get the creature map icon."
  (string (third (get-creature-data creature-ref))))

(defun icon-of-unmapped ()
  "Get the icon for a unmapped room."
  (icon-of-creature 'x))


;;;; Directions and vectors in Castle Zot

(defparameter *directions*
  '((down  (1 0 0))
    (up    (-1 0 0))
    (south (0 1 0))
    (north (0 -1 0))
    (east  (0 0 1))
    (west  (0 0 -1))))

(defun direction-p (symbol)
  (find symbol *directions* :key #'first))

(defun name-of-vector (vector)
  (first (find vector *directions* :key #'second)))

(defun vector-of-direction (direction)
  (assert (direction-p direction))
  (second (find direction *directions* :key #'first)))

(defun map-manifold-vectors (function manifold &rest vectors)
  "Map function over manifold vectors, modulus dimensions of the manifold."
  (mapcar #'mod
          (apply #'mapcar function vectors)
          (array-dimensions manifold)))

;;; The original coordinate system reversed what I would call the X
;;; and Y axes and also counted up from 1.

;;; I judge the interest of coherence outweighs fidelity in this
;;; case. We will count from zero and use "normal" x and y
;;; internally. We'll translate into zot coords for the user interface.

;;; NOTE:
;;; (aref array-3d x y z)
;;; (array-row-major-index array-3d z y x)

(defparameter *cas-coords* :zot
  "What style to display castle coordinates in.")

(defun wiz-coords (coords)
  "Arrange coordinates in the order and value expected."
  (ecase *cas-coords*
    (:array (reverse coords))
    (:zot (map 'list #'1+
               (list (second coords) (third coords) (first coords))))))

(defun unwiz-coords (wizd-coords)
  "Arrange coordinates from original game order and values to internal
order and values."
  (ecase *cas-coords*
    (:array (reverse wizd-coords))
    (:zot (map 'list #'1-
               (list (third wizd-coords)
                     (first wizd-coords)
                     (second wizd-coords))))))


;;;; Locations and creatures in castle Zot.

(defparameter *zot-castle-dimensions* '(8 8 8)
  "Dimensions of castle.")

(defun make-castle-rooms ()
  "Make an array for storing castle room data."
  (make-array *zot-castle-dimensions*
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
  (make-array *zot-castle-dimensions*
              :element-type 'string
              :initial-element (icon-of-unmapped)))

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
  (find creature *rooms*))

(defparameter *monsters*
  '(kobold orc wolf goblin ogre troll bear
    minotaur gargoyle chimera balrog dragon)
  "List of the monster creature types in the castle.")

(defun monster-p (maybe)
  (find maybe *monsters*))

(deftype monster ()
  `(satisfies monster-p))

(defun random-monster ()
  "Return a random monster."
  (random-elt *monsters*))

;; (defun adversary-p (creature)
;;   "Is this creature an adversary?"
;;   (assert (creature-p creature))
;;   (find creature (append *monsters* '(vendor)))
;;   ;; FIXME: The castle can tell us if the vendor is an enemy or
;;   ;; not. This may just represent whether the creature is attackable.
;;   )

;; (defun text-of-adversary (adversary)
;;   "Return the enemy text."
;;   (assert (adversary-p adversary))
;;   (text-of-creature adversary))

;;1790 a=peek(fnd(z))-12:wc=0:if (a<13)or (vf=1)then2300
;;2300 q1=1+int(a/2):q2=a+2:q3=1

(defun calc-adversary-value (adversary)
  ;; FIXME: creature-battle-skill, creature-combat value?
  "Return the enemy value."
  ;; (assert (adversary-p adversary))
  (+ -12 (value-of-creature adversary)))

(defun calc-adversary-hit-points (adversary)
  "Return the enemy hit points."
  ;; (assert (adversary-p adversary))
  (+ 2 (calc-adversary-value adversary)))

(defun calc-adversary-strike-damage (adversary)
  "Return the enemy hit points."
  ;; (assert (adversary-p adversary))
  (1+ (floor (calc-adversary-value adversary) 2)))

;;; TODO: vendors can have hp and dmg too

(defparameter *treasures*
  '(ruby-red norn-stone pale-pearl opal-eye
    green-gem blue-flame palantir silmaril)
  "List of the treasures in the castle.")

(defun treasure-p (maybe)
  (find maybe *treasures*))

(deftype treasure ()
  `(satisfies treasure-p))

(defun random-treasure ()
  "Return a random treasure."
  (random-elt *treasures*))

(defun value-of-treasure (treasure)
  "Return treasure index number."
  (declare (type treasure treasure))
  (position treasure *treasures*))

(defun sort-treasure-list (treasure-list)
  "Sort the given treasure list."
  (sort treasure-list
        #'<
        :key #'value-of-treasure))

(defun type-of-creature (creature)
  "Return the type of creature given."
  ;; FIXME: convert to typecase
  (typecase creature
    (monster   'monster)
    (treasure  'treasure)
    (otherwise creature)))

(defparameter *meals*
  '("wich" " stew" " soup" " burger"
    " roast" " munchy" " taco" " pie")
  "Names of the eight recipes (orc tacos, etc)")

(defun random-meal ()
  "Return a random eat."
  (random-elt *meals*))

(defparameter *entrance* '(0 0 3)
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

(defun text-of-race (race-ref)
  (get-attr-data race-ref *race-data* 'string))

(defparameter *races* '(hobbit elf human dwarf))

(defun random-race (&optional (random-state *random-state*))
  (random-elt *races* random-state))

(defparameter *sex-data*
  '((female "female")
    (male   "male"))
  "The sexes of adventurers.")

(defun text-of-sex (sex-ref)
  (get-attr-data sex-ref *sex-data* 'string))

(defparameter *sexes* '(female male))

(defun random-sex (&optional (random-state *random-state*))
  (random-elt *sexes* random-state))


;;;; Adventurer equipment

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
  '((no-armor  "no armor")
    (leather   "leather")
    (chainmail "chainmail")
    (plate     "plate"))
  "The four armor types")

(defun value-of-armor (armor-ref)
  (get-equip-data armor-ref *armor* 'number))

(defun text-of-armor (armor-ref)
  (if (eq 'nothing armor-ref)
      "nothing"
      (get-equip-data armor-ref *armor* 'string)))

(defun armor-p (item)
  "Is the item armor?"
  (find item *armor* :key 'first))

(defparameter *weapons*
  '((no-weapon "no weapon")
    (dagger    "dagger")
    (mace      "mace")
    (sword     "sword"))
  "The four weapons")

(defun value-of-weapon (weapon-ref)
  (get-equip-data weapon-ref *weapons* 'number))

(defun text-of-weapon (weapon-ref)
  (if (eq 'nothing weapon-ref)
      "nothing"
      (get-equip-data weapon-ref *weapons* 'string)))

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

(defun text-of-ranking (ranking-ref)
  (get-ranking-data ranking-ref *rankings* 'string))

(defstruct (adventurer (:conc-name adv-))
  "A bold youth."
  (rc nil)
  (sx nil)
  (bf nil)
  (rf nil)
  (of nil)
  (bl nil)
  (st  2)
  (iq  8)
  (dx 14)
;;(ot  8) ; FIXME: not used after character creation
  (av  0) ; FIXME: symbol
  (ah  0) ; FIXME: property?
  (wv  0) ; FIXME: symbol
  (gp 60)
  (fl  0)
  (lf nil)
  (fd 60)
  (tr ()) ; FIXME: list, vector?
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

;;; Use only on ranked attributes, adv-st adv-dx adv-iq, with limits
;;; between 0 and 18

(defconstant +adv-rank-min+  0)
(defconstant +adv-rank-max+ 18)

(defun adv-rank (new-rank)
  "Constructs new rank value."
  (min +adv-rank-max+
       (max +adv-rank-min+ new-rank)))

(define-modify-macro incf-adv-rank (&optional (delta 1))
  (lambda (place delta)
    (setf place (adv-rank (+ place delta))))
  "Adventurer rankings must stay within 0 and 18")

(define-modify-macro decf-adv-rank (&optional (delta 1))
  (lambda (place delta)
    (setf place (adv-rank (- place delta))))
  "Adventurer rankings must stay within 0 and 18")

(defun set-adv-rank (adv ranking rank)
  "Set an adventurer ranking to rank. Forces ranking to stay within
limits."
  ;; (assert (find ranking *rankings* :key 'first))
  (funcall (fdefinition (list 'setf ranking))
           (adv-rank rank) adv))

(defun set-adv-rank-max (adv ranking)
  "Set an adventurer's ranking to ranking maximum."
  (set-adv-rank adv ranking +adv-rank-max+))

(defun set-adv-rank-min (adv ranking)
  "Set an adventurer's ranking to ranking minimum."
  (set-adv-rank adv ranking +adv-rank-min+))

;;; Adventurer inventories like adv-gp, adv-fl (also adv-ah, adv-fd).

(defun new-inv (new-inv)
  "Constructs new inventory value."
  (max 0 new-inv))

(define-modify-macro incf-inv (&optional (delta 1))
  (lambda (place delta)
    (setf place (new-inv (+ place delta))))
  "Inventories cannot fall below zero")

(define-modify-macro decf-inv (&optional (delta 1))
  (lambda (place delta)
    (setf place (new-inv (- place delta))))
  "Inventories cannot fall below zero")

;;; Adventurer attributes like race and sex.

(define-modify-macro set-adv-sex (sex)
  (lambda (place sex)
    (setf place
          (if (find sex *sexes*) sex place)))
  "Change adventurer's sex.")

(define-modify-macro set-adv-race (race)
  (lambda (place race)
    (setf place
          (if (find race *races*) race place)))
  "Change adventurer's race.")

(define-modify-macro random-change-adv-attr (alternates)
  (lambda (place alternates)
    (setf place
          (if (find place alternates)
              (random-elt
               (remove-if (lambda (alt) (eq place alt))
                          alternates))
              place)))
  "Change attribute to random selection of alternates")

(defun adv-alive-p (adv)
  "Is the adventurer alive?"
  (every #'plusp
        (list (adv-st adv) (adv-iq adv) (adv-dx adv))))

;; 410 av=-3*(o$="p")-2*(o$="c")-(o$="l"):ifav>0440
;; ...
;; 440 ah=av*7:gp=gp-av*10:printchr$(12)

(defun wear-armor (adv armor)
  "The adventure puts on armor."
  (assert (armor-p armor))
  (let ((av (value-of-armor armor)))
    (setf (adv-av adv) av
          (adv-ah adv) (* 7 av))
    armor))

;; 480 wv=-3*(o$="s")-2*(o$="m")-(o$="d"):ifwv>0then500
;; ...
;; 500 gp=gp-wv*10:printchr$(12):ifgp<20then540

(defun wield-weapon (adv weapon)
  "The adventurer takes up a weapon."
  (assert (weapon-p weapon))
  (setf (adv-wv adv) (value-of-weapon weapon))
  weapon)

(defparameter *adv-equipment-kinds*
  '(sword mace dagger
    plate chain leather
    flares gold-pieces
    lamp runestaff orb-of-zot)
  "Kinds of things the adventurer may carry around.")

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

(defun artifact-p (adv)
  "Does the adventurer have either runestaff or orb of zot?"
  (or (adv-rf adv) (adv-of adv)))

;; 2500 IFIQ<15ORQ3>1THENPRINT:PRINT"** YOU CAN'T CAST A SPELL NOW!":GOTO2320

(defparameter *magic-user-iq-threshold* 14
  "Limit of minimum IQ for casting spells")

(defun cast-spells-p (adv)
  "Is the adventurer smart enough to cast spells?"
  (< *magic-user-iq-threshold* (adv-iq adv)))

(defun adv-without-item-p (adv item-ref)
  "Is the adventurer missing the item?"
  (ecase item-ref
    (gold-pieces (zerop (adv-gp adv)))
    (flares      (zerop (adv-fl adv)))
    (weapon      (zerop (adv-wv adv)))
    (armor       (zerop (adv-av adv)))
    (lamp        (null (adv-lf adv)))
    (runestaff   (null (adv-rf adv)))
    (orb-of-zot  (null (adv-of adv)))))

;;; Again because basic numbered started 1 here is the indexes we're using.

;; | n | treasure text    | treasure effect     |
;; |---+------------------+---------------------|
;; | 0 | "the ruby red"   | cures lethargy      |
;; | 1 | "the norn stone" |                     |
;; | 2 | "the pale pearl" | cures leech         |
;; | 3 | "the opal eye"   | cures blindness     |
;; | 4 | "the green gem"  | cures forgetfulness |
;; | 5 | "the blue flame" | burns books         |
;; | 6 | "the palantir"   |                     |
;; | 7 | "the silmaril"   |                     |

(defun gain-treasure (adv treasure)
  (declare (type treasure treasure))
  (pushnew treasure (adv-tr adv)))

(defun lose-treasure (adv treasure)
  (declare (type treasure treasure))
  (setf (adv-tr adv) (remove treasure (adv-tr adv))))

(defun has-treasure-p (adv treasure)
  (declare (type treasure treasure))
  (find treasure (adv-tr adv)))

(defun adv-treasures (adv)
  "List treasures the adventurer has."
  (sort-treasure-list (adv-tr adv)))

(defun adv-race (adv)
  "Get the text of the adventure's race."
  (text-of-race (adv-rc adv)))

(defun adv-sex (adv)
  "Get the text of the adventurer's sex."
  (text-of-sex (adv-sx adv)))

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
      (equal (icon-of-creature creature-ref)
             (get-adv-map-icon adv room-ref))
      (not (equal (icon-of-unmapped)
                  (get-adv-map-icon adv room-ref)))))

(defun adv-map-room (adv room-ref creature)
  "Tags a room as mapped."
  (setf (get-adv-map-icon adv room-ref) (icon-of-creature creature))
  (make-history (make-event 'adv-mapped creature :at room-ref)))

(defun adv-unmap-room (adv room-ref)
  "Tags a room as unmapped."
  (setf (get-adv-map-icon adv room-ref) (icon-of-unmapped)))


;;;; Curses

(defun adv-cursed-p (adv &optional curse-name)
  "Return nil or a list of curses."
  (if curse-name
      (find curse-name (adv-cr adv))
      (adv-cr adv)))

(defun lethargic-p (adv)
  (unless (has-treasure-p adv 'ruby-red)
    (find 'lethargy (adv-cr adv))))

(defun curse-lethargy (adv)
  "What happens when the curse of lethargy affects the adventurer."
  (assert (adv-cursed-p adv 'lethargy))
  (when (lethargic-p adv)
    (make-event 'adv-dozed)))

(defconstant +curse-leech-theft-maximum+ 5)

(defun curse-leech (adv)
  "What happens when the curse of the leech affects the adventurer."
  (assert (adv-cursed-p adv 'leech))
  (unless (has-treasure-p adv 'pale-pearl)
    (decf-inv (adv-gp adv)
              (random-whole +curse-leech-theft-maximum+))))

(defparameter *forgetfulness* :random
  "What kind of forgetfulness curse: either :random or :mapped.")

(defun curse-forget (adv &optional (forget-type *forgetfulness*))
  "What happens when the curse of forgefulness affects the adventurer."
  (assert (adv-cursed-p adv 'forget))
  (unless (has-treasure-p adv 'green-gem)
    (adv-unmap-room
     adv (ecase forget-type
           (:mapped
            (let ((mappedx (shuffle
                            (filter-array-indices
                             (lambda (s) (eq s (icon-of-unmapped)))
                             (adv-mp adv)))))
              (array-index-row-major (adv-mp adv) (pop mappedx))))
           (:random
            (random-array-subscripts (adv-mp adv)))))))

(defun equipment-p (item)
  "Is the item equipment?"
  (find item *adv-equipment-kinds*))

(defun item-kind-of (item)
  (cond ((weapon-p item)    'weapon)
        ((armor-p item)     'armor)
        ((equipment-p item) item)))


;;; Adventurer event generators

(defun make-adv-stronger (adv delta)
  (incf-adv-rank (adv-st adv) delta)
  (make-history
   (make-event 'adv-gained 'strength :by delta)))

(defun make-adv-weaker (adv delta)
  (decf-adv-rank (adv-st adv) delta)
  (let ((events (make-history
                 (make-event 'adv-lost 'strength :by delta))))
    (unless (adv-alive-p adv)
      (record-event events (make-event 'adv-slain :by 'weakness)))
    events))

(defun make-adv-smarter (adv delta)
  (incf-adv-rank (adv-iq adv) delta)
  (make-history
   (make-event 'adv-gained 'intelligence :by delta)))

(defun make-adv-dumber (adv delta)
  (decf-adv-rank (adv-iq adv) delta)
  (let ((events (make-history
                 (make-event 'adv-lost 'intelligence :by delta))))
    (unless (adv-alive-p adv)
      (record-event events (make-event 'adv-slain :by 'dumbness)))
    events))

(defun make-adv-nimbler (adv delta)
  (incf-adv-rank (adv-dx adv) delta)
  (make-history
   (make-event 'adv-gained 'dexterity :by delta)))

(defun make-adv-clumsier (adv delta)
  (decf-adv-rank (adv-dx adv) delta)
  (let ((events (make-history
                 (make-event 'adv-lost 'dexterity :by delta))))
    (unless (adv-alive-p adv)
      (record-event events (make-event 'adv-slain :by 'clumsiness)))
    events))

(defun make-adv-richer (adv delta)
  (incf-inv (adv-gp adv) delta)
  (make-history
   (make-event 'adv-gained 'gold-pieces :by delta)))

(defun make-adv-poorer (adv delta)
  (decf-inv (adv-gp adv) delta)
  (make-history
   (make-event 'adv-lost 'gold-pieces :by delta)))

(defun give-adv-flares (adv delta)
  (incf-inv (adv-fl adv) delta)
  (make-history
   (make-event 'adv-gained 'flares :by delta)))

(defun take-adv-flares (adv delta)
  (decf-inv (adv-fl adv) delta)
  (make-history
   (make-event 'adv-lost 'flares :by delta)))

(defun give-adv-treasure (adv treasure)
  (gain-treasure adv treasure)
  (make-history (make-event 'adv-gained treasure)))

(defun take-adv-treasure (adv treasure)
  (lose-treasure adv treasure)
  (make-history (make-event 'adv-lost treasure)))

(defun bind-adv-hands (adv item)
  (setf (adv-bf adv) item)
  adv)

(defun make-adv-blind (adv)
  (setf (adv-bl adv) t)
  adv)

(defun break-adv-weapon (adv)
  (setf (adv-wv adv) 0)
  (make-history (make-event 'adv-weapon-broke (adv-weapon adv))))

(defun destroy-adv-armor (adv)
  (assert (zerop (adv-ah adv)))
  (let ((armor (adv-armor adv)))
    (setf (adv-av adv) 0)
    (make-history (make-event 'adv-armor-destroyed armor))))

(defun damage-adv (adv dmg)
  "What happens when the adventurer is struck."
  (with-accessors ((av adv-av) (ah adv-ah) (st adv-st)) adv
    (let ((events ())
          (total-damage 0))
      (when (< 0 av)
        (decf dmg av)           ; reduce damage by armor-value
        (decf-inv ah av)    ; reduce armor-hits by armor-value
        (incf total-damage av)
        (when (< 0 dmg)         ; when damage more-than zero
          (decf-inv ah dmg) ; reduce armor-hits by damage
          (incf total-damage dmg)
          (decf dmg 0))
        (record-event events
                      (make-event 'adv-armor-damaged total-damage))
        (when (zerop ah)
          (join-history events (destroy-adv-armor adv))))
      (when (< 0 dmg)
        (join-history events (make-adv-weaker adv dmg)))
      events)))

;; 2800 IF AV=0 THEN 2830
;; 2810 Q=Q-AV : AH=AH-AV : IF Q<0 THEN AH=AH-Q : Q=0
;; 2820 IF AH < 0 THEN AH=0 : AV=0 : PRINT : PRINT "YOUR ARMOR IS DESTROYED - GOOD LUCK"
;; 2830 ST=ST-Q : RETURN

(defun outfit-with (item adv)
  "Outfit the adventure with the equipment."
  (assert (typep adv 'adventurer))
  (let ((error-fmt "~S not a valid object to equip adventurer with."))
    (etypecase item
      (symbol
       (cond ((find item (rest *weapons*) :key 'first)
              (wield-weapon adv item)
              (make-history (make-event 'adv-wielded item)))
             ((find item (rest *armor*) :key 'first)
              (wear-armor adv item)
              (make-history (make-event 'adv-donned item)))
             ((eq item 'lamp)
              (setf (adv-lf adv) t)
              (make-history (make-event 'adv-gained item)))
             ((eq item 'runestaff)
              (setf (adv-rf adv) t)
              (make-history (make-event 'adv-gained item)))
             ((eq item 'orb-of-zot)
              (setf (adv-of adv) t)
              (setf (adv-rf adv) nil)
              (make-history (make-event 'adv-gained item)
                            (make-event 'adv-lost 'runestaff)))
             (t (error error-fmt item))))
      (list
       (cond ((eq (first item) 'flares)
              (incf-inv (adv-fl adv) (second item))
              (make-history (make-event 'adv-gained item (second item))))
             (t (error error-fmt item)))))))

(defun buy-equipment (equipment price adv)
  (with-accessors ((gp adv-gp)) adv
    (assert (<= price (adv-gp adv)))
    (let ((events (make-history)))
      (record-event events (make-event 'adv-bought equipment price))
      (join-history events (make-adv-poorer adv price))
      (join-history events (outfit-with equipment adv)))))

(defun cure-adv-blindness (adv)
  (with-accessors ((bl adv-bl)) adv
    (when (and bl (has-treasure-p adv 'opal-eye))
      (setf bl nil)
      (make-history (make-event 'adv-cured 'sight-restored :with 'opal-eye)))))

(defun unbind-adv-hand (adv)
  (with-accessors ((bf adv-bf)) adv
    (when (and bf (has-treasure-p adv 'blue-flame))
      (setf bf nil)
      (make-history (make-event 'adv-unbound 'book-burnt :with 'blue-flame)))))




;;; Create character

(defun choose-race (adv)
  "Choose the adventurer's race."
  (wiz-format *wiz-out* "~2&You may be an Elf, Dwarf, Man, or Hobbit")
  (with-accessors ((rc adv-rc) (st adv-st) (dx adv-dx)) adv
    (with-player-input (choice (make-prompt-adv-choice))
      (case choice
        (#\H (set-adv-race rc 'hobbit))
        (#\E (set-adv-race rc 'elf))
        (#\M (set-adv-race rc 'human))
        (#\D (set-adv-race rc 'dwarf))
        (t   (setf choice
                   (wiz-format-error *wiz-err*
                                     "That was incorrect. Please type E, D, M, or H.")))))
    (let* ((*races* '(hobbit elf human dwarf))
           (adj (* 2 (1+ (position rc *races*)))))
      (incf-adv-rank st adj)
      (decf-adv-rank dx adj)
      adv)))

;; Original code set st to 2 and dx to 14. This bit of math adjusted
;; the attributes to their racial "norms" where:

;; 270 forq=1to4:ifleft$(r$(q),1)=o$thenrc=q:st=st+2*q:dx=dx-2*q
;; 280 nextq:print:ot=ot+4*(rc=1):ifrc>0thenr$(3)="human"

;; Here's a table of outcomes:

;; | q | race   | st | dx | iq | ot |
;; |---+--------+----+----+----+----|
;; | 0 | nil    |  2 | 14 |  8 |  8 |
;; | 1 | hobbit |  4 | 12 |  8 | 12 |
;; | 2 | elf    |  6 | 10 |  8 |  8 |
;; | 3 | "man"  |  8 |  8 |  8 |  8 |
;; | 4 | dwarf  | 10 |  6 |  8 |  8 |

;; I duplicated this logic for a possible feature where attributes are
;; adjusted when an adventurer's race is changed (from pool drinking).

(defun choose-sex (adv)
  "Choose the adventurer's sex."
  (with-player-input (choice "~&Sex ")
    (case choice
      (#\M (set-adv-sex (adv-sx adv) 'male))
      (#\F (set-adv-sex (adv-sx adv) 'female))
      (t   (setf choice
                 (wiz-format-error *wiz-err*
                                   "Cute ~A, real cute. Try M or F"
                                   (adv-race adv))))))
  adv)

(defun allocate-points (adv)
  "Distribute other points to attributes."
  (with-accessors ((race adv-race)
                   (st adv-st) (dx adv-dx) (iq adv-iq)) adv
    (let ((ot (if (eq (adv-rc adv) 'hobbit) 12 8)))
      (wiz-format *wiz-out*
                  "~&~|~
                   ~2&Ok ~A, you have these statistics:~
                   ~&strength= ~D intelligence= ~D dexterity= ~D~
                   ~&and ~D other points to allocate as you wish.~
                   ~&"
                  race st iq dx ot)
      (loop
         for (ranking ranking-text) in *rankings*
         for prompt = (format nil
                              "~2&How many points do you add to ~A "
                              ranking-text)
         while (< 0 ot)
         do
           (with-player-input (choice prompt :readf #'wiz-read-n)
             (if (typep choice (list 'integer 0 ot))
                 (progn
                   (decf-inv ot choice)
                   (funcall (fdefinition (list 'setf ranking))
                            (incf-adv-rank choice
                                           (funcall ranking adv))
                            adv))
                 (setf choice (wiz-format-error *wiz-err* "")))))
      (values adv ot))))

(defparameter *catalog-fmt* "~{~A<~A>~^ ~}"
  "Format control string used for printing catalogs.")

;; 380 Z$="ARMOR":GOSUB3390:...
;; 460 Z$="WEAPONS":GOSUB3390
;; 1920 ...Z$="ARMOR":GOSUB3390:...
;; 2030 ...Z$="WEAPON":GOSUB3390...
;; 3390 PRINT"HERE IS A LIST OF ";Z$;" YOU CAN BUY (WITH COST IN <>)":PRINT:RETURN

(defun make-prompt-catalog (stuff item-printer catalog-data)
  (make-prompt-adv-choice
   (with-output-to-string (catalog)
     (format catalog
             "~2&Here is a list of ~A you can buy (with cost in <>)"
             stuff)
     (format catalog
             "~&~{~{~A<~D>~^ ~}~}"
             (map 'list
                  (lambda (item)
                    (list (funcall item-printer (first item))
                          (second item)))
                  catalog-data)))))

(defun get-catalog-price (item catalog-data)
  (second (find item catalog-data :key 'first)))

(defun buy-armor (adv)
  "The adventurer may buy some armor."
  (with-accessors ((race adv-race) (gp adv-gp)) adv
    (let* ((catalog '((nothing 0) (leather 10) (chainmail 20) (plate 30)))
           (prompt (make-prompt-catalog "armor" #'text-of-armor catalog)))
      (wiz-format *wiz-out* "~&~|~2&Ok ~A, you have ~D gold pieces (GP's)"
                  race gp)
      (with-player-input (choice prompt)
        (case choice
          (#\P (buy-equipment 'plate (get-catalog-price 'plate catalog) adv))
          (#\C (buy-equipment 'chainmail
                              (get-catalog-price 'chainmail catalog) adv))
          (#\L (buy-equipment 'leather
                              (get-catalog-price 'leather catalog) adv))
          (#\N 'no-armor)
          (t   (setf choice
                     (wiz-format-error *wiz-err*
                                       "Are you ~A or a ~A ? Type P,C,L, or N"
                                       (text-of-creature
                                        ;; NOTE: Dragons are excluded from the list
                                        ;; of beasts of insult
                                        (random-elt (remove 'dragon *monsters*)))
                                       race))))))))


(defun buy-weapon (adv)
  "The adventurer may buy a weapon."
  (with-accessors ((race adv-race) (gp adv-gp)) adv
    (let* ((catalog '((nothing 0) (dagger 10) (mace 20) (sword 30)))
           (prompt (make-prompt-catalog "weapons" #'text-of-weapon catalog)))
      (wiz-format *wiz-out*  "~&~|~2&Ok, bold ~A, you have ~D GP's left"
                  race gp)
      (with-player-input (choice prompt)
        (case choice
          (#\S (buy-equipment 'sword (get-catalog-price 'sword catalog) adv))
          (#\M (buy-equipment 'mace  (get-catalog-price 'mace catalog) adv))
          (#\D (buy-equipment 'dagger (get-catalog-price 'dagger catalog) adv))
          (#\N 'no-weapon)
          (t   (setf choice
                     (wiz-format-error *wiz-out*
                                       "Is your IQ really ~D? Type S, M, D, or N"
                                       (adv-iq adv)))))))))

(defun buy-lamp (adv)
  "The adventurer may buy a lamp."
  (when (< 19 (adv-gp adv))
    (when (wiz-y-or-n-p "~&~|~2&Do you want to buy a lamp for 20 GP's ")
      (buy-equipment 'lamp 20 adv))
    (adv-lf adv)))

(defun buy-flares (adv)
  "The adventurer may buy some flares."
  (with-accessors ((race adv-race) (gp adv-gp) (fl adv-fl)) adv
    (when (< 0 gp)
      (wiz-format *wiz-out*
                  "~&~|~&Ok, ~A, you have ~D gold pieces left" race gp)
      (with-player-input (flares "~&Flares cost 1 GP each. How many do you want "
                                 :readf #'wiz-read-n)
        (cond ((typep flares (list 'integer 0 gp))
               (buy-equipment (list 'flares flares) flares adv))
              ((typep flares (list 'integer (1+ gp)))
               (setf flares (wiz-format-error *wiz-err*
                                              "You can only afford ~D"
                                              gp)))
              (t
               (setf flares
                     (wiz-format-error *wiz-err*
                                       "If you don't want any just type 0 (zero)"))))))))

(defun setup-adventurer ()
  "Make pc avatar"
  (wiz-format *wiz-out* "~&~|~2&All right, bold one")
  (let ((adv (make-adventurer)))
    (choose-race     adv)
    (choose-sex      adv)
    (allocate-points adv)
    (buy-armor       adv)
    (buy-weapon      adv)
    (buy-lamp        adv)
    (buy-flares      adv) adv))

;;; In Sorcerer BASIC TRUE = -1, FALSE = 0.


;;;; Zot's castle - array functions

;;; FIXME: I don't know yet why this doesn't work.

;; (defun position-array (item array &key (test #'eq) (start 0) (stop nil))
;;        "Return the first index that matches test of item."
;;        (loop
;;        with res = nil
;;        with beg = start
;;        with end = (or stop (array-total-size array)))
;;        for idx from beg below end
;;        do
;;          (setf res (funcall test item (row-major-aref array idx)))

;;        until res
;;        finally (when res idx)))


;; Carl Taylor in "Some real world examples of using maplist & mapl?"
;; http://groups.google.com/group/comp.lang.lisp/msg/a80fbc35a0494c36

;; (defun get-all-objects-indices (object in-array &key (test #'eql))
;;   "Search an array for a given object returning a list of indices for
;; every instance of the object in the array. A list of lists, or nil, is
;; returned."
;;   (declare (optimize (speed 3) (safety 0) (debug 0)))
;;   (let ((adjusted-dimensions-partial-products-list
;;       (maplist (lambda (sub-list) (reduce (function *) sub-list))
;;                (nconc (rest (array-dimensions in-array))
;;                       (list 1)))))
;;     (loop
;;        for i fixnum below (array-total-size in-array)
;;        when (funcall test (row-major-aref in-array i) object)
;;        collect
;;       (loop
;;          with obj-position fixnum = i
;;          for  dims-partial-product
;;          in   adjusted-dimensions-partial-products-list
;;          collect
;;            (multiple-value-bind (quotient remainder)
;;                (truncate obj-position dims-partial-product)
;;              (setf obj-position remainder)
;;              (identity quotient))))))


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
                           ;; (loc-adventurer  *entrance*)
                           (curses *curses-init*))))
  "The castle of Zot"
  (rooms           ())
  (levels          ())
  (curses          ())  ; curses data (curse-name curse-function curse-loc)
  (loc-orb         ())  ; "location of the orb of zot"
  (loc-runestaff   ())  ; "location of the runestaff"
  (vendor-fury    nil)  ; "vendors angry"
  (adventurer     nil)
  (adversaries     ())
  ;; (loc-adventurer  ())  ; "player coordinates"
  (history         ())
  )

(defun get-castle-creature (castle room-ref)
  "Return the contents of a castle room based on reference. Reference
may be an index or list of coordinates."
  (with-accessors ((rooms cas-rooms)) castle
    (etypecase room-ref
      (integer (row-major-aref rooms room-ref))
      (list    (apply #'mref rooms room-ref)))))

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
      (get-castle-creature castle room-ref)))

(defun get-castle-creature-text (castle room-ref)
  "Get castle creature text."
  (text-of-creature (get-castle-creature castle room-ref)))
;; [wc 2012-02-03] FIXME: no one actually calls this.

(defun get-castle-creature-icon (castle room-ref)
  "Get castle creature icon."
  (icon-of-creature (get-castle-creature castle room-ref)))

(defun clear-castle-room (castle room-ref)
  "Make empty room."
  (setf (get-castle-creature castle room-ref) 'empty-room))

(defun random-castle-coords (castle)
  "Get random coordinates from the castle."
  (random-array-subscripts (cas-rooms castle)))

(defun cas-adv-here (castle)
  "Return coords of room the adventurer most recently entered."
  (first (data-of-event (find-event 'adv-entered-room (cas-history castle)))))

(defun cas-creature-here (castle)
  "What castle creature is in the room the adventuer is in?"
  (get-castle-creature castle (cas-adv-here castle)))

(defun orb-of-zot-here-p (castle)
  (equal (cas-adv-here castle) (cas-loc-orb castle)))

(defun runestaff-here-p (castle)
  (equal (cas-adv-here castle) (cas-loc-runestaff castle)))

;; (defun vendor-here-p (castle)
;;   (eq (cas-creature-here castle) 'vendor))

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
    (format nil " ~A " (get-adv-map-icon adv coords))))

(defun make-map-icon-adv (castle coords)
  "Make a map icon of a castle room with an adventurer in it."
  (with-accessors ((adv cas-adventurer)) castle
    (format nil "<~A>" (get-adv-map-icon adv coords))))

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
  "Return nil or a list of cursed room data for room coordinates."
  (get-castle-curse castle coords aspect))

(defparameter *curse-notify* nil
  "Later versions printed message when a curse took effect")

(defparameter *curse-notice-ohare* "A curse!"
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
        (t
         (filter-array-indices #'room-empty-p rooms))))))

;; FIXME: depends on castle size constant 64.
;; (reduce #'* (castle-level-dimensions (castle-rooms castle)))

(defun random-empty-room (castle &optional level)
  "Get a random empty room from a castle or castle level."
  (first (shuffle (list-empty-room-indices castle level))))

(defun castle-room-random (castle)
  (random-array-subscripts (cas-rooms castle)))

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
;;            (shuffle (list-empty-room-indices castle level))))
;;       (loop
;;          repeat population
;;          do
;;            (let* ((up (pop random-empty-rooms))
;;                   (dn (add-castle-vectors castle (vector-of-direction 'down)
;;                                           (castle-index-coords castle up))))
;;              (setf (get-castle-room-index castle up) 'stairs-up)
;;              (setf (get-castle-room castle dn) 'stairs-down)))))
;;   castle)

;; (defun place-creatures-on-level (castle level creatures &key (population 1))
;;   (let ((random-empty-rooms (shuffle (list-empty-room-indices level))))
;;     (loop
;;        repeat population
;;        do
;;       (loop
;;          for creature in creatures
;;          do
;;            (setf (get-castle-room-index castle
;;                                         (pop random-empty-rooms))
;;                  creatures)))))

(defparameter *wiz-setup-verbosity* t)

(defun setup-castle (&optional (setup-verbosity *wiz-setup-verbosity*))
  "Place stuff in castle"
  (let* ((castle (make-castle))
         (height (castle-height (cas-rooms castle)))
         (silent (not setup-verbosity))
         (lvl-mt (loop
                    for level from 0 below height
                    collect (shuffle (list-empty-room-indices castle level))))
         (msgtxt (make-string-input-stream "initializing castle")))
    (flet ((random-lvl-room (level)
             (pop (elt lvl-mt level)))
           ;; unused
           ;; (random-cas-room ()
           ;;   (pop (elt lvl-mt (random (length lvl-mt)))))
           (print-progress ()
             (unless silent
               (wiz-format *wiz-out* "~C" (read-char-no-hang msgtxt)))))
      (unless silent (wiz-format *wiz-out* "~2&Please be patient - "))
      ;; Place entrance (2)
      (with-accessors ((rooms cas-rooms)
                       (orb cas-loc-orb)
                       (runestaff cas-loc-runestaff)
                       (curses cas-curses)) castle
        (setf (get-castle-creature castle *entrance*) 'entrance)
        (print-progress)
        (setf (elt lvl-mt 0)
              (remove (castle-coords-index castle *entrance*) (elt lvl-mt 0)))
        (print-progress)
        ;; Place stairs.
        ;; 2 stairs down (4) on floors 1 - 7 (0 - 6)
        ;; 2 stairs up (3) on floors 2 - 8 (1 - 7)
        ;; (place-stairs-in-castle castle)
        (loop
          for lvl-dn from 0 below (1- height)
          for lvl-up from 1 below height
          do
             (loop
               repeat 2
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
        (print-progress)
        ;; Place monsters (13 - 24).
        ;; 1 each monster on all floors
        (loop
          for level from 0 below height
          do
             (loop
               for monster in *monsters*
               do
                  (setf (get-castle-creature castle (random-lvl-room level))
                        monster))
             ;; (place-creatures-on-level castle level *monsters*)
             (print-progress))
        ;; Place vendor and items.
        ;; 3 each item on all floors (5 - 12)
        ;; 1 vendor on all floors (25)
        (loop
          for level from 0 below height
          do
             (loop
               repeat 3
               do
                  (loop
                    for room in *rooms*
                    do
                       (setf (get-castle-creature castle (random-lvl-room level))
                             room))
                  (setf (get-castle-creature castle (random-lvl-room level))
                        'vendor))
          finally
             (print-progress))
        ;; (place-creatures-on-level castle level *rooms* :population 3)
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
            do
               (setf (third curse)
                     (castle-index-coords castle (random-elt cas-mt)))
               ;; Multiple curses can be in the same room.
               (print-progress)
            finally
               (print-progress))
          ;; Place runestaff with 1 random monster (13 - 24).
          (let ((loc-rune (pop cas-mt)))
            (setf (get-castle-creature castle loc-rune) (random-monster)
                  runestaff (array-index-row-major (cas-rooms castle)
                                                   loc-rune)))
          ;; Place orb in room that seems like a warp (9)
          (print-progress)
          (let ((loc-orb (pop cas-mt)))
            (setf (get-castle-creature castle loc-orb) 'warp
                  orb (array-index-row-major (cas-rooms castle) loc-orb)))
          (print-progress)
          (print-progress))
        (unless silent (wiz-format *wiz-out* "~%"))
        castle))))


;;; Adventurer events

(defun send-adv (castle coords)
  "Send an adventurer to location at coords"
  ;; (record-events (cas-history castle)
  (record-event (cas-history castle)
                (make-event 'adv-entered-room coords))
  castle)

(defun move-adv (castle vector-ref)
  "Move adventurer in direction."
  (with-accessors ((here cas-adv-here)) castle
    (send-adv castle
              (add-castle-vectors (cas-rooms castle)
                                  here
                                  (vector-of-direction vector-ref)))))

(defun make-adv-fall (castle)
  (record-event (cas-history castle)
                (make-event 'adv-fell))
  (move-adv castle 'down))

(defun make-adv-teleport (castle coords)
  (record-event (cas-history castle)
                (make-event 'adv-teleported))
  (send-adv castle coords))

(defun make-adv-warp (castle)
  (record-event (cas-history castle)
                (make-event 'adv-warped))
  (send-adv castle
            (random-array-subscripts (cas-rooms castle))))

(defun make-adv-stagger (castle turns)
  (join-history (cas-history castle)
                (loop
                  repeat turns
                  collect (make-event 'adv-staggered)))
  (move-adv castle
            (random-elt '(north east west south))))

(defconstant +trap-gas-disorientation-time+ 20)

(defun adv-springs-gas-trap (castle)
  (record-event (cas-history castle)
                (make-event 'trap-sprang 'gas))
  (lambda (castle)
    (make-adv-stagger castle
                      +trap-gas-disorientation-time+)))

(defconstant +trap-bomb-damage-maximum+ 6)

(defun adv-springs-bomb-trap (castle)
  (record-event (cas-history castle)
                (make-event 'trap-sprang 'bomb))
  (lambda (castle)
    (damage-adv (cas-adventurer castle)
                (random-whole +trap-bomb-damage-maximum+))))

(defun message-adv-armor-destroyed (stream castle)
  (when (latest-event-p (make-event 'adv-armor-destroyed)
                        (cas-history castle))
    (format stream "~2&Your armor is destroyed - good luck")))

(defconstant +vendor-potion-efficacy-maximum+ 6)

(defun adv-drinks-potion (adv potion)
  (when (stringp potion)
    (setf potion (intern (string-upcase potion) "WIZARDS-CASTLE")))
  (let ((events (make-history))
        (delta (random-whole +vendor-potion-efficacy-maximum+)))
    (record-event events (make-event 'adv-drank-potion potion))
    (join-history events
                  (ecase potion
                    (strength (make-adv-stronger adv delta))
                    (dexterity (make-adv-nimbler adv delta))
                    (intelligence (make-adv-smarter adv delta))))))


;;;; Outcomes

(defun make-outcome (outcome-name outcome-effect outcome-text)
  (list outcome-name outcome-effect outcome-text))

(defun name-of-outcome (outcome)
  (first outcome))

(defun outcome-name-p (outcome name-ref)
  (eq name-ref (name-of-outcome outcome)))

(defun get-outcome (outcome-name outcomes)
  (find outcome-name outcomes :key 'name-of-outcome))

(defun effect-of-outcome (outcome)
  (second outcome))

(defun text-of-outcome (outcome)
  (third outcome))

(defun type-of-outcome (outcome)
  (first outcome))


;;;; Enter room

(defun make-message-report-inv (stream castle inv)
  "Make message for inventories"
  (with-accessors ((adv cas-adventurer)) castle
    (format stream
            "~2&You have ~D"
            (ecase inv
              (gold-pieces (adv-gp adv))
              (flares      (adv-fl adv))))))

(defconstant +gold-in-rooms-maximum+ 10)

(defun adv-finds-gold-pieces (castle)
  "What happens when an adventurer finds gold."
  (with-accessors ((adv  cas-adventurer)
                   (here cas-adv-here)
                   (history cas-history)) castle
    (assert (eq (get-castle-creature castle here) 'gold-pieces))
    (let ((gps (random-whole +gold-in-rooms-maximum+)))
      (clear-castle-room castle here)
      (join-history history (make-adv-richer adv gps))
      (join-history history (cas-adv-map-here castle))
      (values castle
              (make-message-report-inv nil castle 'gold-pieces)))))

(defconstant +flares-in-rooms-maximum+ 5)

(defun adv-finds-flares (castle)
  "What happens when an adventurer finds flares."
  (with-accessors ((adv  cas-adventurer)
                   (here cas-adv-here)
                   (history cas-history)) castle
    (assert (eq (get-castle-creature castle here) 'flares))
    (let* ((flares (random-whole +flares-in-rooms-maximum+)))
      (clear-castle-room castle here)
      (join-history history (give-adv-flares adv flares))
      (join-history history (cas-adv-map-here castle))
      (values castle
              (make-message-report-inv nil castle 'flares)))))

(defun adv-finds-treasure (castle)
  "What happens when an adventurer finds treasure."
  (with-accessors ((adv  cas-adventurer)
                   (here cas-adv-here)
                   (history cas-history)) castle
    (let* ((treasure (get-castle-creature castle here)))
      (clear-castle-room castle here)
      (join-history history (give-adv-treasure adv treasure))
      (join-history history (cas-adv-map-here castle))
      (values castle "~&Its now yours"))))

;;; --- FIXME [wc 2012-12-11]
;;; streamline these with a macro

(defun adv-finds-sinkhole (castle)
  "What happens when the adventurer finds a sinkhole?"
  (assert (eq (cas-creature-here castle) 'sinkhole))
  (make-adv-fall castle))

(defun adv-finds-orb-of-zot (castle)
  "What happens when the adventurer finds the orb-of-zot."
  (with-accessors ((adv  cas-adventurer)
                   (here cas-adv-here)
                   (went cas-adv-last-went)
                   (history cas-history)) castle
    (let ((message (make-text)))
      (cond
        ((oldest-event-p (make-event 'adv-used 'runestaff)
                         (events-since 'adv-used history))
         (outfit-with 'orb-of-zot adv)
         (setf (cas-loc-orb castle) nil)
         (clear-castle-room castle here)
         (record-event history (make-event 'adv-found 'orb-of-zot))
         (join-history history (cas-adv-map-here castle))
         (push-text message
                    (format nil "Great unmitigated Zot!~
                                 ~&You just found the Orb of Zot~
                                 ~&The Runestaff is gone")))
        (t
         (record-events history (make-event 'adv-warped))
         (move-adv castle went)))
      (values castle message))))

(defun adv-finds-warp (castle)
  "What happens when the adventurer finds a warp?"
  (assert (eq (cas-creature-here castle) 'warp))
  (cond ((orb-of-zot-here-p castle)
         (adv-finds-orb-of-zot castle))
        (t (make-adv-warp castle))))

;;;; Combat

(defun foe-name-chopped (foe-text)
  (subseq foe-text 2))

(defun foe-text-trimmed (foe-text)
  (string-trim "an " foe-text))

(defparameter *foe-name-finder* #'foe-name-chopped)

(defstruct
    (adversary
      (:conc-name foe-)
      (:constructor make-adversary
                    (creature
                     &aux
                     (text (text-of-creature creature))
                     (name (funcall *foe-name-finder* text))
                     (strike-damage (calc-adversary-strike-damage creature))
                     (hit-points (calc-adversary-hit-points creature)))))
  (creature nil)    ; creature
  (text "")         ; "a creature"
  (name "")         ; "creature"
  (strike-damage 0) ; q1
  (hit-points 0)    ; q2
  (first-turn t)    ; q3
  (enwebbed 0)      ; enemy enwebbed
  (end nil))

(define-modify-macro decf-foe-hit-points (&optional (delta 1))
  (lambda (foe-hp delta)
    (setf foe-hp (new-inv (- foe-hp delta))))
  "Decrease foe hit points.")

(defun text-of-foe (adversary)
  (text-of-creature (foe-creature adversary)))

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
  (make-history (make-event 'foe-wounded damage)))

(defun foe-bribable-p (foe)
  (foe-first-turn foe))

(defparameter *combat-turn-events*
  '(adv-attacked adv-cast-spell adv-retreated
    foe-attacked)
  "Combat events")

;; 1790 A=PEEK(FND(Z))-12:WC=0:IF(A<13)OR(VF=1)THEN2300
;; 2300 Q1=1+INT(A/2):Q2=A+2:Q3=1

;;; FIXME what's q3? first round

;; 2520 if o$<>"w" then 2540
;; 2530 st=st-1 : wc=fna(8)+1 : on 1 - (st < 1) goto 2690,2840

(defparameter *web-spell-turn-minimum*  2)
(defparameter *web-spell-turn-maximum* 10)

(defun adv-casts-spell-web (castle)
  "The adventurer casts a web spell to entangle adversary."
  (with-accessors ((history cas-history)
                   (adv cas-adventurer)
                   (foe latest-foe)) castle

    (let ((turns (random-range *web-spell-turn-minimum*
                               *web-spell-turn-maximum*)))
      (record-event history (make-event 'adv-cast-spell 'web))
      (join-history history (make-adv-weaker adv 1))
      (incf (foe-enwebbed foe) turns)
      (record-event history (make-event 'foe-bound 'web turns))))
  castle)

;; 2540 if o$<>"f" then 2580
;; 2550 q=fna(7)+fna(7):st=st-1:iq=iq-1:if(iq<1)or(st<1)then2840
;; 2560 print"  It does ";q;"points of damage.":print
;; 2570 q2=q2-q:goto 2410

(defparameter *fireball-spell-damage-factor* 7)

(defun adv-casts-spell-fireball (castle)
  "The adventure casts a fireball spell on the adversary."
  (with-accessors ((history cas-history)
                   (adv cas-adventurer)
                   (foe latest-foe)) castle
    (let ((damage (+ (random *fireball-spell-damage-factor*)
                     (random *fireball-spell-damage-factor*)))
          (message (make-text)))
      (record-event history
                    (make-event 'adv-cast-spell
                                'fireball
                                damage))
      (join-history history (make-adv-weaker adv 1))
      (when (adv-alive-p adv)
        (join-history history (make-adv-dumber adv 1)))
      (when (adv-alive-p adv)
        (join-history history (damage-foe foe damage))
        (push-text message
                   (format nil
                           "~&  It does ~D points of damage." damage)))
      (unless (foe-alive-p foe)
        (record-event history (make-event 'foe-slain (foe-creature foe))))
      (values castle message))))

;; 2540 if o$<>"f" then ...
;; print"death - - - ";:ifiq<15+fna(4)thenprint"yours";iq=0goto2840
;; print"his":printq2=0:goto2420

(defun make-adv-dead (castle)
  (with-accessors ((history cas-history)
                   (adv cas-adventurer)) castle
    (join-history history
                  (make-adv-dumber adv (adv-iq adv)))
    (record-event history (make-event 'adv-slain 'death-spell)))
  castle)

(defun make-foe-dead (castle)
  (with-accessors ((history cas-history)
                   (foe latest-foe)) castle
    (join-history history (damage-foe foe (foe-hit-points foe)))
    (record-event history (make-event 'foe-slain 'death-spell)))
  castle)

(defparameter *death-spell-outcomes*
  (list
   (make-outcome 'adv-death 'make-adv-dead "yours")
   (make-outcome 'foe-death 'make-foe-dead "his"))
  "Outcomes of the death spell.")

(defconstant +magic-user-iq-minimum+ 15)
(defconstant +death-spell-iq-factor-range+ 4)

(defun adv-casts-spell-death (castle)
  "The adventurer casts a death spell."
  (with-accessors ((history cas-history)
                   (adv cas-adventurer)) castle
    (let ((message (make-text))
          (outcome (if (< (adv-iq adv)
                              (+ +magic-user-iq-minimum+
                                 (random-whole +death-spell-iq-factor-range+)))
                           'adv-death
                           'foe-death)))
      (record-events history
                     (make-event 'adv-cast-spell 'death))
      (destructuring-bind (_ outcome-effect outcome-text)
          (get-outcome outcome
                       *death-spell-outcomes*)
        (declare (ignore _))
        (funcall outcome-effect castle)
        (push-text message
                   (format nil "Death - - - ~A" outcome-text)))
      (values castle message))))

(defun choose-spell ()
  "Adventurer chooses a spell to cast."
  (with-player-input (spell "~&Which spell (web, fireball, or deathspell) ")
    (case spell
      (#\W 'adv-casts-spell-web)
      (#\F 'adv-casts-spell-fireball)
      (#\D 'adv-casts-spell-death)
      (t   (setf spell
                 (wiz-format-error *wiz-err*
                                   "Choose one of the listed options"))))))

(defun adv-casts-spell (castle)
  "The adventurer casts a spell."
  (with-accessors ((adv cas-adventurer)
                   (foe latest-foe)) castle
    (if (or (cast-spells-p adv) (foe-enwebbed-p foe))
        (funcall (symbol-function (choose-spell)) castle)
        (values castle
                (wiz-format-error nil "You can't cast a spell now!")))))

;; Powers, 1980; 2500--2600

;; (defun make-message-adv-bribed (event)
;;   "Make adv-bribed message."
;;   (case (value-of-event event)

(defparameter *bribe-outcomes*
  (list
   (list 'bribe-requested nil (formatter "~&I want ~A will you give it too me "))
   (list 'bribe-refused nil "~&'All I want is your life!'")
   (list 'bribe-accepted 'foe-accepts-bribe "~&Okay, just don't tell anyone."))
  "Outcomes of bribing adversaries.")

(defun adv-bribes (castle)
  "What happens when an adventurer tries to bribe a creature."
  (with-accessors ((history cas-history)
                   (adv cas-adventurer)
                   (foe latest-foe)) castle
    (flet ((get-text (outcome)
             (third (find outcome *bribe-outcomes* :key #'first))))
      (let ((treasure (random-elt (adv-treasures adv)))
            (foe-name (foe-creature foe))
            (message (make-text)))
        (cond ((null treasure)
               (record-event history (make-event 'adv-tried 'bribe foe-name))
               (push-text message (get-text 'bribe-refused)))
              (t
               (when (wiz-y-or-n-p
                      (format nil
                              (get-text 'bribe-requested)
                              (text-of-creature treasure)))
                 (lose-treasure adv treasure)
                 (record-event history (make-event 'adv-bribed foe-name treasure))
                 (push-text message (get-text 'bribe-accepted))
                 (when (eq 'vendor foe-name)
                   (setf (cas-vendor-fury castle) nil)))))
        (values castle message)))))

;; (defun make-message-adv-attacks (castle event)
;;   (assert (event-kind-p event 'adv-attacks))
;;   (let ((creature-text
;;       (text-of-creature (foe-creature (latest-foe castle)))))
;;     (ecase (value-of-event event)
;;       (unarmed (format nil "Pounding on ~A won't hurt it" creature-text))
;;       (book-stuck-on-hand "You can't beat it to death with a book!")
;;       (missed  "  Drat! Missed")
;;       (strike (format nil "  You hit the lousy ~A"
;;                    (subseq creature-text 2))))))

;; (defun make-message-weapon-broke (castle event)
;;   (assert (event-kind-p 'adv-weapon-broke event))
;;   (format nil "Oh no! Your ~A broke"
;;        (text-of-weapon (adv-wv (cas-adventurer castle)))))

(defun adv-hungry-p (castle)
  "True if adventurer is hungry."
  (< 60 (count-turns (events-since 'adv-ate (cas-history castle)))))

(defconstant +adversary-hoard-maximum+ 1000)

(defun adv-slays-adversary (castle)
  "What happens when the adventurer kills a creature."
  (with-accessors ((history cas-history)
                   (adv cas-adventurer)
                   (here cas-adv-here)
                   (foe latest-foe)) castle
    (assert (not (foe-alive-p foe)))
    (let ((message (make-text)))
      (push-text message (format nil "~2&~A lies dead at your feet"
                                 (text-of-foe foe)))
      (when (and (adv-hungry-p castle)
                 (typep (foe-creature foe) 'monster))
        (record-event history
                      (make-event 'adv-ate (foe-creature foe)))
        (push-text message
                   (format nil "~2&You spend an hour eating ~A~A"
                           (text-of-foe foe) (random-meal))))
      (when (runestaff-here-p castle)
        (record-event history
                      (make-event 'adv-found 'runestuff))
        (outfit-with 'runestaff adv)
        (push-text message "~2&Great Zot! You've found the Runestaff"))
      (if (typep (foe-creature foe) 'monster)
          (let ((hoard (random-whole +adversary-hoard-maximum+)))
            (make-adv-richer adv hoard)
            (record-event history
                          (make-event 'adv-gained
                                      'gold-pieces
                                      :by hoard))
            (push-text message
                       (format nil "~2&You now get his hoard of ~D GP's" hoard)))
          (progn
            (join-history history
                          (make-history
                           (outfit-with 'plate adv)
                           (outfit-with 'sword adv)
                           (adv-drinks-potion adv 'strength)
                           (adv-drinks-potion adv 'intelligence)
                           (adv-drinks-potion adv 'dexterity)))
            (push-text message
                       (format nil "~2&You get all his wares:~{~&A~}"
                               '("plate armor"
                                 "a sword"
                                 "a strength potion"
                                 "an intelligence potion"
                                 "a dexterity potion")))
            (when (adv-without-item-p adv 'lamp)
              (record-event history (outfit-with 'lamp adv))
              (push-text message
                         (format nil "~&A lamp")))))
            (clear-castle-room castle here)
            (join-history history (cas-adv-map-here castle))
      (values castle message))))

(defun adv-broke-weapon-on-foe-p (events)
  "Did the adventurer's weapon break on the foe?"
  (find-event 'adv-broke-weapon events))

(defun foe-slain-p (castle)
  (and
   (zerop (foe-hit-points (latest-foe castle)))
   (latest-event-p 'foe-slain
                   (cas-history castle))))

(defun adv-strikes-foe (castle)
  (with-accessors ((history cas-history)
                   (adv cas-adventurer)
                   (foe latest-foe)) castle
      (let ((message (make-text)))
        (when (foe-alive-p foe)
          (join-history history (damage-foe foe (adv-wv adv))))
        (when (and (find (foe-creature foe) '(gargoyle dragon))
                   (zerop (random 8)))
          (join-history history (break-adv-weapon adv))
          (push-text message
                     (format nil "~2&Oh no! Your ~A broke"
                             (text-of-weapon (adv-weapon adv)))))
        (unless (foe-alive-p foe)
          (record-event history (make-event 'foe-slain (foe-creature foe))))
        (values castle message))))

(defparameter *adv-attacks-outcomes*
  (list
   (list 'adv-strike-missed  nil (format nil "~&  Drat! Missed"))
   (list 'adv-strike-hit 'adv-strikes-foe
         (lambda (stream castle)
           (format stream
                   "~&  You hit the lousy ~A"
                   (foe-name (latest-foe castle))))))
  "Possibilities when the adventurer strikes at a foe.")

(defun make-adv-strike (adv)
  (get-outcome
   (if (< (adv-dx adv)
          (+ (random 20)
             (if (adv-bl adv) 1 0)))
      'adv-strike-missed
      'adv-strike-hit)
   *adv-attacks-outcomes*))

(defun adv-attacks (castle)
  "What happens when the adventure attacks a creature."
  (with-accessors ((history cas-history)
                   (adv cas-adventurer)
                   (foe latest-foe)) castle
    (let ((message (make-text)))
      (cond
        ((not (armed-p adv))
         (record-event history (make-event 'adv-tried 'unarmed-attack))
         (push-text message
                    (wiz-format-error nil
                                      "Pounding on ~A won't hurt it"
                                      (foe-name foe))))
        ((bound-p adv)
         (record-event history (make-event 'adv-tried 'attack-with-hands-bound))
         (push-text message
                    (wiz-format-error nil
                                      "You can't beat it to death with a book!!")))
        (t
         (destructuring-bind (outcome-name outcome-effect outcome-text)
             (make-adv-strike adv)
           (cond
             ((eq outcome-name 'adv-strike-hit)
              (push-text message (format nil outcome-text castle))
              (multiple-value-bind (strike-effects strike-message)
                  (funcall outcome-effect castle)
                (unless (castle-p strike-effects)
                  (join-history history strike-effects))
                (push-text message strike-message)))
             (t
              (push-text message outcome-text))))))
      (values castle message))))

(defun make-foe-struggle-with-web (foe)
  (assert (foe-enwebbed-p foe))
  (with-accessors ((bound foe-enwebbed)) foe
    (let ((events (make-history))
          (message (make-text)))
      (decf bound)
      (cond ((< 0 bound)
             (record-event events (make-event 'foe-unbound))
             (push-text message
                        (format nil
                                "~&The ~A is stuck and can't attack"
                                (foe-name foe))))
            (t
             (record-event events (make-event 'foe-unbound))
             (push-text message "~&The web just broke!")))
      (values
       events
       message))))

(defparameter *foe-attack-outcomes*
  (list
   (list 'foe-strike-missed nil "~&  Hah! He missed you")
   (list 'foe-strike-hit 'damage-adv "~&  Ouch! He hit you"))
  "Outcomes when an adversary attacks.")

(defconstant +adversary-strike-factor+ 6)

(defun make-foe-strike (adv)
  (get-outcome
   (if (< (+ (random-whole +adversary-strike-factor+)
             (random-whole +adversary-strike-factor+)
             (random-whole +adversary-strike-factor+)
             (* 3 (if (blind-p adv) 1 0)))
          (adv-dx adv))
       'foe-strike-missed
       'foe-strike-hit)
   *foe-attack-outcomes*))

(defun foe-attacks (castle)
  "What happens when an adversary attacks."
  (with-accessors ((history cas-history)
                   (adv cas-adventurer)
                   (foe latest-foe)) castle
    (let ((message (make-text)))
      (if (foe-enwebbed-p foe)
          (multiple-value-bind (struggle-events struggle-text)
              (make-foe-struggle-with-web foe)
            (join-history history struggle-events)
            (push-text message struggle-text))
          (destructuring-bind (outcome-name outcome-effect outcome-text)
              (make-foe-strike adv)
            (push-text message
                       (format nil
                               "~2&The ~A attacks"
                               (foe-name foe)))
            (cond ((eq outcome-name 'foe-strike-hit)
                   (join-history history
                                 (funcall outcome-effect adv
                                          (foe-strike-damage foe)))
                   (push-text message outcome-text)
                   (when (latest-event-p 'adv-armor-destroyed history)
                     (push-text message
                                (format nil #'message-adv-armor-destroyed castle))))
                  (t (push-text message outcome-text)))))
      ;; FIXME [wc 2013-01-29]: maybe use TAGBODY instead of (WHEN
      ;; ...) (UNLESS ...)
      (values castle message))))

(defun adv-retreats (castle)
  "Adventurer retreats from a fight."
  (with-accessors ((history cas-history)
                   (adv cas-adventurer)) castle
    (let ((message (make-text)))
      (multiple-value-bind (_ foe-attack-message)
          (foe-attacks castle)
        (declare (ignore _))
        (wiz-format *wiz-out* foe-attack-message))
      (wiz-format *wiz-out* "~&You have escaped")
      (let ((direction (wiz-read-direction
                        "~&Do you go north, south, east, or west "
                        (format nil "~&Don't press your luck ~A"
                                (adv-race adv)))))
        (record-event history
                      (make-event 'adv-retreated direction))
        (move-adv castle direction)
        (values castle message)))))

(defun make-message-end-game (adv end turns)
  "What does the game report when the game ends."
  (with-accessors ((race adv-race)
                   (st adv-st) (iq adv-iq) (dx adv-dx)
                   (gp adv-gp) (lf adv-lf) (fl adv-fl)
                   (wv adv-wv) (av adv-av) (tr adv-tr)
                   (rf adv-rf) (of adv-of)) adv
    (with-output-to-string (message)
      (cond ((eq end 'death)
             (format message "~&~|~&~A"
                     (make-string *wiz-width*
                                  :initial-element #\*))
             (format message
                     "~2&A noble effort, oh formerly living ~A" race)
             (format message "~&You died from a lack of ~A"
                     (cond ((< st 1) "strength")
                           ((< iq 1) "intelligence")
                           ((< dx 1) "dexterity")
                           (t "life")))
             (format message "~&When you died you had:"))
            ((eq end 'exit)
             (format message
                     "~&You left the castle with~:[out~;~] the Orb of Zot"
                     of)))
      (unless (eq end 'death)
        (if (and of (eq end 'exit))
            (format message "~2&A glorious victory!~
                             ~&You also got out with the following:")
            (format message "~2&A less than awe-inspiring defeat.~
                             ~&When you left the castle you had:")))
      (format message "~@[~&Your miserable life!~]" (adv-alive-p adv))
      (format message "~{~@[~&~A~]~}" (mapcar #'text-of-creature (adv-treasures adv)))
      (format message "~&~A" (text-of-weapon wv))
      (format message "~&~A" (text-of-armor av))
      (format message "~@[~&a lamp~]" lf)
      (format message "~&~D flares" fl)
      (format message "~&~D GP's" gp)
      (format message "~@[~&the Runestaff~]" rf)
      (format message "~&and it took you ~D turns!" turns))))

(defun make-prompt-fight (adv foe)
  "Make the fight round prompt."
  (make-prompt-adv-choice
   (with-output-to-string (facing)
     (format facing "~3&You're facing ~A" (foe-text foe))
     (format facing "~&You may attack or retreat")
     (when (foe-bribable-p foe)
       (format facing " or bribe"))
     (when (cast-spells-p adv)
       (format facing " or cast a spell"))
     (format facing "~2&Your strength is ~D and dexterity is ~D"
             (adv-st adv) (adv-dx adv)))))

(defun get-adv-fight-action (adv foe)
  "The adventurer chooses a fight action."
  (let ((prompt (make-prompt-fight adv foe)))
    (with-player-input (choice prompt)
      (case choice
        (#\A 'adv-attacks)
        (#\R 'adv-retreats)
        (#\B 'adv-bribes)
        (#\C 'adv-casts-spell)
        (t   (setf choice
                   (wiz-format-error *wiz-err*
                                      "Choose one of the listed options")))))))

(defun fight-end-p (castle)
  "Return true if fight is over"
  (with-accessors ((history cas-history)) castle
    (find (name-of-event (latest-event history))
          '(adv-slain foe-slain adv-entered-room adv-bribed))))

;; 2310 if(c(1,4)>t(1))or(bl=1)or(dx<fna(9)+fna(9))then2690

(defconstant +adventurer-dx-initiative-factor+ 9)

(defun adv-initiative-p (adv)
  "Does the adversary get the first shot in a fight?"
  (not (or (lethargic-p adv)
           (blind-p adv)
           (< (adv-dx adv)
              (+ (random-whole +adventurer-dx-initiative-factor+)
                 (random-whole +adventurer-dx-initiative-factor+))))))

(defun adv-meets-adversary (castle)
  "The adventurer fights an creature in the castle."
  (with-accessors ((history cas-history)
                   (adv cas-adventurer)
                   (foe latest-foe)
                   (here cas-adv-here)) castle
    (push (make-adversary (get-castle-creature castle here))
          (cas-adversaries castle))
    (let ((message (make-text))
          (fight-form (if (adv-initiative-p adv)
                          (get-adv-fight-action adv foe)
                          'foe-attacks)))
      (setf (foe-first-turn foe) nil)
      (loop
         do
           (multiple-value-bind (action-events action-message)
               (funcall fight-form castle)
             (unless (castle-p action-events)
               (join-history history action-events))
             (when action-message
               (wiz-format *wiz-out* action-message)))
           (ecase fight-form
             (adv-attacks
              (when (foe-alive-p foe)
                (setf fight-form 'foe-attacks)))
             (adv-bribes
              (unless (latest-event-p 'adv-bribes history)
                (setf fight-form 'foe-attacks)))
             (adv-casts-spell
              (when (foe-alive-p foe)
                (setf fight-form 'foe-attacks)))
             (adv-retreats
              (setf fight-form 'nil))
             (foe-attacks
              (when (adv-alive-p adv)
                (setf fight-form (get-adv-fight-action adv foe)))))
         until (fight-end-p castle))
      (when (latest-event-p 'foe-slain history)
        (multiple-value-bind (victory-events victory-message)
            (adv-slays-adversary castle)
          (unless (castle-p victory-events)
            (join-history history victory-events))
          (push-text message victory-message)))
      (values castle message))))


;;;; vendor

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

(defun sell-treasure (adv treasure price)
  (let ((events (make-history)))
    (join-history events (take-adv-treasure adv treasure))
    (make-adv-richer adv price)
    (join-history events (make-history
                          (make-event 'adv-gained 'gold-pieces
                                      :by price)))))

(defun sell-treasures-to-vendor (adv)
  (with-accessors ((gp adv-gp)) adv
    (loop
       with events = (make-history)
       for tr in (adv-treasures adv)
       do
         (let ((tr-t (text-of-creature tr))
               (tr-v (random (* (1+ (value-of-treasure tr)) 1500))))
           (when (wiz-y-or-n-p
                  (format nil
                          "~&Do you want to sell ~A for ~D "
                          tr-t tr-v))
             (join-history events (sell-treasure adv tr tr-v)))))))

(defun adv-budget (gp catalog)
  "Filter catalog for items you can afford."
  (remove-if (lambda (price) (< gp price)) catalog :key 'second))

(defparameter *vendor-armor-catalog*
  '((nothing 0) (leather 1250) (chainmail 1500) (plate 2000))
  "Armor and prices available at vendors.")

(defun buy-armor-from-vendor (adv)
  "The adventure may buy armor from a castle vendor."
  (with-accessors ((av adv-av) (gp adv-gp) (race adv-race)) adv
    (when (< 1249 gp)
      (let ((catalog (adv-budget gp *vendor-armor-catalog*))
            (events (make-history)))
        (wiz-format *wiz-out*
                    "Ok ~A, you have ~D and ~A."
                    race
                    gp
                    (text-of-armor av))
        (join-history
         events
         (with-player-input
             (armor (make-prompt-catalog "armor" #'text-of-armor catalog))
           (case armor
             (#\P (if (find 'plate catalog :key 'first)
                      (buy-equipment
                       'plate (get-catalog-price 'plate catalog) adv)
                      (setf armor
                            (wiz-format-error *wiz-err*
                                              "You can't afford plate"))))
             (#\C (if (find 'chainmail catalog :key 'first)
                      (buy-equipment
                       'chainmail (get-catalog-price 'chainmail catalog) adv)
                      (setf armor
                            (wiz-format-error *wiz-err*
                                              "You haven't got that much cash"))))
             (#\L (buy-equipment
                   'leather (get-catalog-price 'leather catalog) adv))
             (#\N (make-history (make-event 'adv-bought 'no-armor)))
             (t   (setf armor
                        (wiz-format-error *wiz-err*
                                          "Don't be silly. Choose a selection"))))))))))

(defparameter *vendor-weapons-catalog*
  '((nothing 0) (dagger 1250) (mace 1500) (sword 2000))
  "Weapons and prices available at vendors.")

(defun buy-weapon-from-vendor (adv)
  "The adventurer may buy a weapon from a castle vendor."
  (with-accessors ((wv adv-wv) (gp adv-gp)) adv
    (when (< 1249 gp)
      (let ((catalog (adv-budget gp *vendor-weapons-catalog*))
            (events (make-history)))
        (wiz-format *wiz-out*
                    "You have ~D GP's left and ~A in hand."
                    gp
                    (text-of-weapon wv))
        (join-history
         events
         (with-player-input
	     ;; NOTE: spelling of "weapon" in vendor catalog is from
	     ;; Power (Power 1980:2030), and uncorrected in O'Hare,
	     ;; and Stetson
             (weapon (make-prompt-catalog "weapon" #'text-of-weapon catalog))
           (case weapon
             (#\S (if (find 'sword catalog :key 'first)
                      (buy-equipment 'sword
                                     (get-catalog-price 'sword catalog) adv)
                      (setf weapon
                            (wiz-format-error *wiz-err*
                                              "Dungeon express card - ~
                                               you left home without it!"))))
             (#\M (if (find 'mace catalog :key 'first)
                      (buy-equipment 'mace
                                     (get-catalog-price 'mace catalog) adv)
                      (setf weapon
                            (wiz-format-error *wiz-err*
                                              "Sorry sir, I don't give credit"))))
             (#\D (buy-equipment 'dagger
                                 (get-catalog-price 'dagger catalog) adv))
             (#\N (make-history (make-event 'adv-bought 'no-weapon)))
             (t (setf weapon
                      (wiz-format-error *wiz-err*
                                        "Try choosing a selection"))))))))))

(defun buy-potions-from-vendor (adv)
  "The adventurer may buy potions from a castle vendor."
  (let ((price 1000)
        (events (make-history)))
    (with-accessors ((gp adv-gp)) adv
      (when (<= price gp)
        (loop
           for (attr name) in *rankings*
           do
             (when (wiz-y-p
                    (format nil "~2&Want to buy a potion of ~A for ~D GP's "
                            name price))
               (record-event events (make-event 'adv-bought 'potion name price))
               (join-history events (make-adv-poorer adv price))
               (join-history events (adv-drinks-potion adv name))
               (wiz-format *wiz-out*
                           "~2&Your ~A is now ~D"
                           name
                           (funcall attr adv)))
           until (< gp price))))
    events))

(defun buy-lamp-from-vendor (adv)
  (let ((price 1000))
    (with-accessors ((lf adv-lf) (gp adv-gp)) adv
      (when (<= price gp)
        (when (wiz-y-or-n-p
               (format nil "Want a lamp for have ~D GP's " price))
          (wiz-format *wiz-out* "Its guaranteed to outlive you!")
          (buy-equipment 'lamp price adv))))))

(defun trade-with-vendor (adv)
  (with-accessors ((gp adv-gp) (race adv-race)) adv
    (let ((events (make-history)))
      (join-history events (sell-treasures-to-vendor adv))
      (cond ((< gp 1000)
             (values events
                     (wiz-format *wiz-out*
                                 "~2&You're too poor to trade, ~A"
                                 race)))
            (t
             (join-history events (buy-armor-from-vendor   adv))
             (join-history events (buy-weapon-from-vendor  adv))
             (join-history events (buy-potions-from-vendor adv))
             (join-history events (buy-lamp-from-vendor    adv)))))))

(defun adv-ignored-vendor ()
  "What happens when the adventurer ignores a vendor? (Nothing.)"
  (values (make-history (make-event 'adv-ignored 'vendor)) ""))

(defun adv-meets-vendor (castle)
  "The adventurer encounters a vendor."
  (with-accessors ((adv cas-adventurer)) castle
    (if (cas-vendor-fury castle)
        (adv-meets-adversary castle)
        (with-player-input
            (choice
             (make-prompt-adv-choice
              "~&You may trade with, attack, or ignore the vendor"))
          (cond
            ((eq choice #\T) (trade-with-vendor adv))
            ((eq choice #\I) (adv-ignored-vendor))
            ((eq choice #\A)
             (wiz-format *wiz-out* "You'll be sorry you did that")
             (setf (cas-vendor-fury castle) t)
             (adv-meets-adversary castle))
            (t   (setf choice
                       (wiz-format-error *wiz-err*
                                         "Nice shot, ~A"
                                         (adv-race adv)))))))))


;;;; Entering, exiting

(defun adv-finds-room (castle)
  "What happens when the adventurer enters any other kind of room"
  (assert (typep castle 'castle))
  nil
  ;; (make-history
  ;;  (make-event 'adv-found (cas-creature-here castle))))
  )


;;;; Help message

(defparameter *help-text-dos*
  (format nil
          "~&*** WIZARD'S CASTLE COMMAND AND INFORMATION SUMMARY ***~
          ~2&The following commands are available :~
          ~2&Help     North    South    East     West     Up~
           ~&Down     DRink    Map      Flare    Lamp     Open~
           ~&Gaze     Teleport Quit~
           ~&The contents of rooms are as follows :~
          ~2&. = Empty Room      B = Book            C = Chest~
           ~&D = Stairs Down     E = Entrance/Exit   F = Flares~
           ~&G = Gold Pieces     M = Monsters        O = Crystal Orb~
           ~&P = Magic Pool      S = Sinkhole        T = Treasure~
           ~&U = Stairs Up       V = Vendor          W = Warp/ORB OF ZOT~
          ~2&The benefits of having treasures are :~
          ~2&RUBY READ  - Avoid LETHARGY     PALE PEARL - Avoid LEECH~
           ~&GREEN GEM  - Avoid FORGETTING   OPAL EYE   - Cures BLINDNESS~
           ~&BLUE FLAME - Dissolves BOOKS    NORN STONE - No Benefit~
           ~&PALANTIR   - No Benefit         SILMARIL   - No Benefit")
  "Some help documentation")

(defparameter *wiz-help* nil)

(defun player-help (castle)
  "Report help for game."
  (declare (ignorable castle))
  (assert (not (null *wiz-help*)))
  (values
   (make-history (make-event 'player-viewed-help))
   *wiz-help*))


;;;; Outcomes

(defparameter *without-item-outcomes*
  (list
   (cons 'flares
         "Hey bright one, you're out of flares")
   (cons 'lamp
         (lambda (stream castle)
           (format stream "You don't have a lamp ~A"
                   (text-of-race (adv-rc (cas-adventurer castle))))))
   (cons 'runestaff
         "You can't teleport without the runestaff"))
  "Messages when the adventurer tries something without the necessary item.")

;; (defun get-message (message-key messages)
;;   (getf messages (intern (string message-key) 'keyword)))

(defun adv-tried-without-item (castle item)
  "What happens when the adventurer tries to use something they don't have?"
  (destructuring-bind (outcome-name . outcome-text)
      (assoc item *without-item-outcomes*)
    (values (make-history (make-event 'adv-tried :no outcome-name))
            (when outcome-text
              (wiz-format-error nil outcome-text castle)))))

(defparameter *wrong-room-outcomes*
  (list
   (cons 'gaze
         "No orb - no gaze")
   (cons 'open
         "The only thing you opened was your big mouth")
   (cons 'drink
         "If you want a drink find a pool")
   (cons 'use-stairs
         (lambda (stream castle room-needed)
           (format stream "Oh ~A, no ~A in here"
                   (text-of-race (adv-race (cas-adventurer castle)))
                   (text-of-creature room-needed)))))
  "Messages when the adventure tries something in the wrong room.")

(defun wrong-room-p (castle coords creature)
  "Is this creature in this room?"
  (not (castle-creature-p castle coords creature)))

(defun adv-tried-wrong-room (castle action coords &rest args)
  "What happens when the adventurer does something in the wrong room?"
  (destructuring-bind (outcome-name . outcome-text)
      (assoc action *wrong-room-outcomes*)
    (values (make-history
             (make-event 'adv-tried outcome-name coords
                         (get-castle-creature castle coords)))
            (when outcome-text
              (wiz-format-error nil outcome-text castle (first args))))))

(defun adv-tried-blind (castle action)
  "Return events and message when the adventurer tries something when blind."
  (assert (find action '(use-crystal-orb use-lamp use-flare view-map)))
  (values
   (record-event (cas-history castle) (make-event 'adv-tried action 'blind))
   (wiz-format-error nil "You can't see anything, dumb ~A"
                     (adv-race (cas-adventurer castle)))))


;;;; Map

(defun you-are-at (stream coords)
  "Make message 'You are at ...'"
  (format stream
          "~2&You are at ~{(~D,~D) Level ~D~}"
          (wiz-coords coords)))

(defun make-level-map (castle level)
  "Make a level map."
  (loop
     for y from 0 to 7
     collect
       (loop
          for x from 0 to 7
          collect
            (make-map-icon-room castle (list level y x)))
     into icon-map
     finally
       (return
         (with-output-to-string (level-map)
           (with-accessors ((here cas-adv-here)) castle
             (when (eq level (first here))
               (destructuring-bind (y x) (rest here)
                 (setf (elt (elt icon-map y) x)
                       (make-map-icon-adv castle here)))))
           (format level-map "~2&~{~:}" "~&~{~A~}" icon-map)))))

;;; FIXME: castle size dependant code

(defun adv-uses-map (castle)
  "Show the player the map the adventurer has been making."
  (with-accessors ((adv cas-adventurer)
                   (here cas-adv-here)
                   (history cas-history)) castle
    (let ((message (make-text)))
      (cond ((blind-p adv)
             (multiple-value-bind (blind-events blind-message)
                 (adv-tried-blind castle 'view-map)
               (join-history history blind-events)
               (push-text message blind-message)))
            (t
             (let ((level (first here)))
               (record-events history
                              (make-event 'adv-viewed-map level))
               (push-text message
                          (with-output-to-string (level-map)
                            (format level-map
                                    (make-level-map castle level))
                            (format level-map
                                    #'you-are-at (cas-adv-here castle)))))))
      (values castle message))))

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


;;;; Flare

(defun adv-uses-flare (castle)
  "What happens when the adventurer uses a flare."
  (with-accessors ((adv cas-adventurer)
                   (here cas-adv-here)
                   (rooms cas-rooms)
                   (history cas-history)) castle
    (let ((message (make-text)))
      (cond
        ((blind-p adv)
         (multiple-value-bind (blind-events blind-message)
             (adv-tried-blind castle 'use-flare)
           (join-history history blind-events)
           (push-text message blind-message)))
        ((adv-without-item-p adv 'flares)
         (multiple-value-bind (without-item-events without-item-message)
             (adv-tried-without-item castle 'flares)
           (join-history history without-item-events)
           (push-text message without-item-message)))
        (t
         (decf-inv (adv-fl adv))
         (let ((near-coords (get-near-coords castle here)))
           (record-events history
                          (make-event 'adv-used 'flare :coords near-coords))
           (loop
              for near in near-coords
              do (cas-adv-map-room castle near))
           (push-text message
                      (with-output-to-string (text)
                        (format text
                                "~2&~{~:}" "~&~3@{ ~A~}"
                                (loop
                                   for near in near-coords
                                   collect
                                     (get-castle-creature-icon castle near)))
                        (format text #'you-are-at (cas-adv-here castle)))))))
      (values castle message))))



;;;; Lamp


;; TODO: Unlike most actions, input errors when using the lamp cycle
;; back to the main loop. This could be fixed.

(defun adv-uses-lamp (castle &optional direction)
  "What happens when the adventurer tries to use the lamp."
  (with-accessors ((adv cas-adventurer)
                   (history cas-history)) castle
    (let ((message (make-text)))
      (cond
        ((blind-p (cas-adventurer castle))
         (multiple-value-bind (blind-events blind-message)
             (adv-tried-blind castle 'use-lamp)
           (join-history history blind-events)
           (push-text message blind-message)))
        ((adv-without-item-p adv 'lamp)
         (multiple-value-bind (without-item-events without-item-message)
             (adv-tried-without-item castle 'lamp)
           (join-history history without-item-events)
           (push-text message without-item-message)))
        (t
         (let ((direction
                (or direction
                    (wiz-read-direction
                     "~&Where do you shine the lamp (N,S,E, or W) "))))
               ;; No error or message if error comes from read-direction
           (cond ((eq direction 'input-error)
                  (record-events history
                                 (make-event 'player-error 'bad-lamp-direction))
                  (push-text message
                             (wiz-format-error nil
                                               "Turkey! That's not a direction")))
                 (t
                  (let* ((there (cas-adv-near castle direction))
                         (creature (get-castle-creature castle there)))
                    (cas-adv-map-near castle direction)
                    (record-events history
                                   (make-event 'adv-used 'lamp there)
                                   (make-event 'adv-mapped creature :at there))
                    (push-text message
                               (with-output-to-string (text)
                                 (format text "~2&The lamp shines into ~
                                               ~{(~D,~D) Level ~D~}"
                                         (wiz-coords there))
                                 (format text "~2&There you will find ~A"
                                         (text-of-creature creature))))))))))
      (values castle message))))


;;;; Pool

(defparameter *drink-pool-outcomes*
  (list
   (make-outcome 'stronger    'make-adv-stronger "stronger")
   (make-outcome 'weaker      'make-adv-weaker   "weaker")
   (make-outcome 'smarter     'make-adv-smarter  "smarter")
   (make-outcome 'dumber      'make-adv-dumber   "dumber")
   (make-outcome 'nimbler     'make-adv-nimbler  "nimbler")
   (make-outcome 'clumsier    'make-adv-clumsier "clumsier")
   (make-outcome 'change-race
                 (lambda (castle)
                   (let* ((adv (cas-adventurer castle))
                          (adv-race (adv-rc adv))
                          (new-race (random-elt
                                     (remove adv-race *races*))))
                     (set-adv-race (adv-rc adv) new-race)
                     (make-history (make-event 'adv-changed 'race
                                               :from adv-race
                                               :to new-race))))
                 (lambda (stream events)
                   (destructuring-bind (event-name event-kind &key from to)
                       (oldest-event events)
                     (declare (ignore from))
                     (assert (equal '(adv-changed race)
                                    (list event-name event-kind)))
                     (format stream
                            "become a ~A"
                            (text-of-race to)))))
   (make-outcome 'change-sex
                 (lambda (castle)
                   (let* ((adv (cas-adventurer castle))
                          (adv-sex (adv-sx adv))
                          (new-sex (random-elt
                                    (remove adv-sex *sexes*))))
                     (set-adv-sex (adv-sx adv) new-sex)
                     (make-history (make-event 'adv-changed 'sex
                                               :from adv-sex
                                               :to new-sex))))
                 (lambda (stream events)
                   (destructuring-bind (event-name event-kind &key from to)
                       (oldest-event events)
                     (declare (ignore from))
                     (assert (equal '(adv-changed sex)
                                    (list event-name event-kind)))
                     (format stream
                             "turn into a ~A" to)))))
  "All of the drink outcomes.")

(defconstant +pool-effect-maximum+ 3)

(defun adv-drinks-pool (castle)
  "Return events and message from drinking from a magic pool."
  (with-accessors ((adv cas-adventurer)
                   (here cas-adv-here)
                   (history cas-history)) castle
    (let ((message (make-text)))
      (cond
        ((wrong-room-p castle here 'pool)
         (multiple-value-bind (wrong-room-events wrong-room-message)
             (adv-tried-wrong-room castle 'drink here)
           (join-history history wrong-room-events)
           (push-text message wrong-room-message)))
        (t
         (destructuring-bind (outcome-name outcome-effect outcome-text)
             (random-elt *drink-pool-outcomes*)
           (when outcome-effect
             (setf outcome-effect
                   (cond ((member outcome-name '(change-race change-sex))
                          (funcall outcome-effect castle))
                         (t (funcall outcome-effect
                                     adv
                                     (random-whole +pool-effect-maximum+)))))
             (record-event history
                           (make-event 'adv-drank 'pool
                                       :and (name-of-event (latest-event outcome-effect))))
             (join-history history outcome-effect))
           (when outcome-text
             (setf outcome-text
                   (cond ((member outcome-name '(change-race change-sex))
                          (format nil outcome-text outcome-effect))
                         (t (format nil
                                    "feel ~A"
                                    outcome-text))))
             (push-text message
                        (format nil
                                "You take a drink and ~A"
                                outcome-text))))))
        (values castle message))))


;;;; Crystal Orb

(defparameter *gaze-mapper* nil
  "Crystal Orbs can tell you where stuff is or lie to you about what's
there. This info could be mapped.")

(defun make-message-creature-at (stream creature coords)
  (format stream
          "~A at ~{(~D,~D) Level ~D~}"
          (text-of-creature creature)
          (wiz-coords coords)))

(defun gaze-mapper (adv coords creature)
  "The adventurer could map what he sees in the crystal orbs."
  (labels ((gaze-map-naive ()
             (adv-map-room adv coords creature))
           (gaze-map-ask ()
             (when (wiz-y-or-n-p (format nil
                                         "Do you wish to map ~A "
                                         (make-message-creature-at nil
                                                                   creature
                                                                   coords)))
               (adv-map-room adv coords creature)))
           (gaze-map-smart ()
             (unless (adv-room-mapped-p adv coords)
               (adv-map-room adv coords creature)))
           (gaze-map-skeptic ()
             (unless (adv-room-mapped-p adv coords)
               (gaze-map-ask))))
    (let ((events (make-history)))
      (join-history events
                      (ecase *gaze-mapper*
                        (:naive (gaze-map-naive))
                        (:ask (gaze-map-ask))
                        (:smart (gaze-map-smart))
                        (:skeptic (gaze-map-skeptic)))))))

(defconstant +orb-damage-maximum+ 2)

(defparameter *gaze-crystal-orb-outcomes*
  (list
   (make-outcome 'heap
                 (lambda (castle)
                   (join-history (cas-history castle)
                                 (apply #'make-history
                                        (make-event 'adv-used 'crystal-orb
                                                    :saw 'adv-in-bloody-heap)
                                        (make-adv-weaker (cas-adventurer castle)
                                                         (random-whole +orb-damage-maximum+))))
                   castle)
                 (lambda (stream castle)
                   (when (> 4
                            (length (events-since (make-event 'adv-used 'crystal-orb
                                                              :saw 'adv-in-bloody-heap)
                                                  (cas-history castle))))
                     (format stream "yourself in a bloody heap."))))
   (make-outcome 'room
                 (lambda (castle)
                   (let* ((coords (castle-room-random castle))
                          (creature (get-castle-creature castle coords)))
                     (join-history (cas-history castle)
                                   (make-history
                                    (make-event 'adv-used 'crystal-orb
                                                :saw creature
                                                :at coords)))
                     (when *gaze-mapper*
                       (join-history (cas-history castle)
                                     (gaze-mapper (cas-adventurer castle)
                                                  coords
                                                  creature))))
                   castle)
                 (lambda (stream castle)
                   (when (latest-event-p (make-event 'adv-used 'crystal-orb :saw)
                                         (cas-history castle))
                     (destructuring-bind (&rest _ &key saw at &allow-other-keys)
                         (latest-event (cas-history castle))
                       (declare (ignore _))
                       (make-message-creature-at stream
                                                 saw
                                                 at)))))
   (make-outcome 'orb-of-zot
                 (lambda (castle)
                   (join-history (cas-history castle)
                                 (make-history
                                  (make-event 'adv-used 'crystal-orb
                                              :saw 'orb-of-zot
                                              :at (random-elt (list (cas-loc-orb castle)
                                                                    (castle-room-random castle))))))
                   castle)
                 (lambda (stream castle)
                   (when (latest-event-p (make-event 'adv-used 'crystal-orb
                                                     :saw 'orb-of-zot)
                                         (cas-history castle))
                     (make-message-creature-at stream
                                               'orb-of-zot
                                               (value-of-event
                                                (latest-event
                                                 (cas-history castle)))))))
   (make-outcome 'drink
                 (lambda (castle)
                   (join-history (cas-history castle)
                                 (make-history
                                  (make-event 'adv-used 'crystal-orb
                                              :saw 'self-drinking-from-pool
                                              :becoming (random-monster))))
                   castle)
                 (lambda (stream castle)
                   (when (latest-event-p (make-event 'adv-used 'crystal-orb
                                                     :saw 'self-drinking-from-pool)
                                         (cas-history castle))
                     (format stream
                             "yourself drinking from a pool and becoming ~A"
                             (text-of-creature
                              (value-of-event
                               (latest-event
                                (cas-history castle))))))))
   (make-outcome 'soap
                 (lambda (castle)
                   (join-history (cas-history castle)
                                 (make-history
                                  (make-event 'adv-used 'crystal-orb
                                              :saw 'soap-opera)))
                   castle)
                 "a soap opera rerun"))
  "The visions in the crystal orb.")

(defun adv-uses-crystal-orb (castle)
  "Return events and message of what happens when the adventurer gazes
into the orb."
  (with-accessors ((adv cas-adventurer)
                   (here cas-adv-here)
                   (history cas-history)) castle
    (let ((message (make-text)))
      (cond
        ((blind-p (cas-adventurer castle))
         (multiple-value-bind (blind-events blind-message)
             (adv-tried-blind castle 'use-crystal-orb)
           (join-history history blind-events)
           (push-text message blind-message)))
        ((wrong-room-p castle here 'crystal-orb)
         (multiple-value-bind (without-item-events without-item-message)
             (adv-tried-wrong-room castle 'gaze here)
           (join-history history without-item-events)
           (push-text message without-item-message)))
        (t
         (destructuring-bind (outcome-name outcome-effect outcome-text)
             (random-elt *gaze-crystal-orb-outcomes*)
           (declare (ignore outcome-name))
           (when outcome-effect
             (setf outcome-effect
                   (funcall outcome-effect castle)))
           (push-text message (format nil
                                      "You see ~@?"
                                      outcome-text
                                      castle)))))
      (values nil message))))


;;;; Book

(defparameter *open-book-outcomes*
  (list
   (make-outcome 'flash-trap
                 (lambda (castle)
                   (make-adv-blind (cas-adventurer castle))
                   (join-history (cas-history castle)
                                 (make-history
                                  (make-event 'adv-opened 'book
                                              :and '(trap-sprang flash))
                                  (make-event 'trap-sprang 'flash
                                              :and 'adv-blinded)))
                   castle)
                 (lambda (stream castle)
                   (when (latest-event-p (make-event 'trap-sprang 'flash :and 'adv-blinded)
                                         (cas-history castle))
                     (format stream
                             "~&FLASH! Oh no! You are now a blind ~A"
                             (adv-race (cas-adventurer castle))))))
   (make-outcome 'poetry
                 (lambda (castle)
                   (join-history (cas-history castle)
                                 (make-history
                                  (make-event 'adv-opened 'book
                                              :reads 'poetry))))
                 (lambda (stream castle)
                   (when (latest-event-p (make-event 'adv-opened 'book :reads 'poetry)
                                       (cas-history castle))
                       (format stream
                               "~&its another volume of Zot's Poetry! - Yeech!"))))
   (make-outcome 'magazine
                 (lambda (castle)
                   (join-history (cas-history castle)
                                 (make-history
                                  (make-event 'adv-opened 'book
                                              :reads 'magazine))))
                 (lambda (stream castle)
                   (when (latest-event-p (make-event 'adv-opened 'book :reads 'magazine)
                                         (cas-history castle))
                     (format stream
                             "~&its an old copy of Play~A"
                             (text-of-race (random-race))))))
   (make-outcome 'dexterity-manual
                 (lambda (castle)
                   (make-adv-stronger (cas-adventurer castle)
                                      +adv-rank-max+)
                   (join-history (cas-history castle)
                                 (make-history
                                  (make-event 'adv-opened 'book
                                              :reads 'dexterity-manual))))
                 (lambda (stream castle)
                   (make-adv-nimbler (cas-adventurer castle)
                                      +adv-rank-max+)
                   (when (latest-event-p (make-event 'adv-opened 'book :reads 'dexterity-manual)
                                         (cas-history castle))
                     (format stream "~&it's a manual of dexterity"))))
   (make-outcome 'strength-manual
                 (lambda (castle)
                   (make-adv-nimbler (cas-adventurer castle) +adv-rank-max+)
                   (join-history (cas-history castle)
                                 (make-history
                                  (make-event 'adv-opened 'book
                                              :reads 'strength-manual))))
                 (lambda (stream castle)
                   (when (latest-event-p (make-event 'adv-opened 'book :reads 'strength-manual)
                                         (cas-history castle))
                     (format stream "~&it's a manual of strength"))))
   (make-outcome 'glue-trap
                 (lambda (castle)
                   (bind-adv-hands (cas-adventurer castle) 'book)
                   (join-history (cas-history castle)
                                 (make-history
                                  (make-event 'adv-opened 'book
                                              :and '(trap-sprang glue))
                                  (make-event 'trap-sprang 'glue
                                              :and 'adv-bound
                                              :to 'book))))
                 (lambda (stream castle)
                   (when (latest-event-p (make-event 'trap-sprang 'glue :and 'adv-bound :to 'book)
                                         (cas-history castle))
                     (format stream
                             "~&the book sticks to your hands -~&~
                                Now you can't draw your weapon")))))
  "All the outcomes of opening books")

(defun adv-opens-book (castle)
  "Return events and message of what happens when the adventurer opens a book."
  (with-accessors ((adv cas-adventurer)
                   (here cas-adv-here)) castle
    (let ((message (make-text)))
      ;; TODO [wc 2013-01-29] Seems like BLIND-P ought to have an
      ;; effect on some of the events.
      (destructuring-bind (outcome-name outcome-effect outcome-text)
          (random-elt *open-book-outcomes*)
        (declare (ignorable outcome-name))
        (when outcome-effect
          (setf outcome-effect
                (funcall outcome-effect castle)))
        (when outcome-text
          (push-text message
                     (format nil
                             "~&You open the book and ~@?"
                             outcome-text
                             castle))))
      (values nil message))))


;;;; Chest

(defconstant +gold-in-chests-maximum+ 1000)

(defparameter *open-chest-outcomes*
    (list
     (make-outcome 'bomb-trap
                   (lambda (castle)
                     (when (latest-event-p (make-event 'adv-opened 'chest)
                                           (cas-history castle))
                       #'adv-springs-bomb-trap))
                   (lambda (stream castle)
                     (when (latest-event-p (make-event 'trap-sprang 'bomb)
                                           (cas-history castle))
                       (format stream
                               "~&KABOOM! It explodes"))))
     (make-outcome 'gas-trap
                   (lambda (castle)
                     (when (latest-event-p (make-event 'adv-opened 'chest)
                                           (cas-history castle))
                       #'adv-springs-gas-trap))
                   (lambda (stream castle)
                     (when (latest-event-p (make-event 'trap-sprang 'gas)
                                           (cas-history castle))
                       (format stream
                               "~&Gas! You stagger from the room"))))
     (make-outcome 'gold-pieces
                   (lambda (castle)
                     (let ((adv (cas-adventurer castle))
                           (gps (random-whole +gold-in-chests-maximum+)))
                       (join-history (cas-history castle)
                                     (make-adv-richer adv gps)))
                     castle)
                   (lambda (stream castle)
                     (when (latest-event-p (make-event 'adv-gained 'gold-pieces)
                                           (cas-history castle))
                       (destructuring-bind (action object &key by)
                           (latest-event (cas-history castle))
                         (declare (ignorable action object))
                         (format stream
                                 "~&Find ~D gold pieces"
                                 by))))))
  "All the outcomes of opening chests.")

(defun adv-opens-chest (castle)
  "Return events and messages when adventurer opens a chest."
  (with-accessors ((adv cas-adventurer)
                   (here cas-adv-here)
                   (history cas-history)) castle
    (record-event history (make-event 'adv-opened 'chest))
    (let ((message (make-text)))
      (destructuring-bind (outcome-name outcome-effect outcome-text)
          (get-outcome (random-elt '(bomb-trap gas-trap
                                     gold-pieces gold-pieces))
                       *open-chest-outcomes*)
        (declare (ignore outcome-name))
        (when outcome-effect
          (setf outcome-effect
                (funcall outcome-effect castle)))
        (when outcome-text
          (setf outcome-text
                (format nil
                        "You open the chest and~@?"
                        outcome-text castle))
          (push-text message outcome-text))
        (etypecase outcome-effect
          (function (funcall outcome-effect castle))
          (castle t)))
      (values nil message))))


;;;; Open something

(defun adv-opens (castle)
  "What happens when an adventurer opens a book or chest."
  (with-accessors ((here cas-adv-here)
                   (creature cas-creature-here)) castle
    (let ((events (make-history))
          (message (make-text)))
      (multiple-value-bind (open-events open-message)
          (case creature
            (chest (adv-opens-chest castle))
            (book  (adv-opens-book castle))
            (otherwise (adv-tried-wrong-room castle 'open here)))
        (clear-castle-room castle here)
         (join-history events open-events)
        (values
         (join-history events (cas-adv-map-here castle))
         (push-text message open-message))))))


;;;; Adventurer moves or is moved

(defun adv-walks (castle direction)
  "What happens when the adventure walks in a direction."
  (with-accessors ((adv cas-adventurer)
                   (here cas-adv-here)
                   (history cas-history)) castle
    (let ((room-type (cas-creature-here castle))
          (message (make-text)))
      (flet ((handle-wrong-room (room-type)
               (multiple-value-bind (wrong-room-events wrong-room-message)
                   (adv-tried-wrong-room castle
                                         'use-stairs
                                         here
                                         room-type)
                 (join-history history
                               wrong-room-events)
                 (push-text message wrong-room-message))))
        (cond ((and (equal direction 'north)
                    (equal room-type 'entrance))
               (record-events history
                              (make-event 'adv-walked direction)
                              (if (adv-of adv)
                                  (make-event 'adv-left-castle 'with-orb)
                                  (make-event 'adv-left-castle))))
              ((and (equal direction 'up)
                    (wrong-room-p castle here 'stairs-up))
               (handle-wrong-room 'stairs-up))
              ((and (eq direction 'down)
                    (wrong-room-p castle here 'stairs-down))
               (handle-wrong-room 'stairs-down))
              (t
               (record-event history (make-event 'adv-walked direction))
               (move-adv castle direction))))
      (values castle message))))

(defun read-castle-coordinates ()
  (let* ((zot-coords (eql *cas-coords* :zot))
         (min (if zot-coords 1 0))
         (max (if zot-coords 8 7))
         ;; [wc 2013-01-31] FIXME: castle size dependant code, prompts
         ;; really only need to be generated once, etc
         (error-text (format nil "Try a number from ~D to ~D" min max))
         (prompts (list (format nil "~A-coord (~D=far west  ~D=far east ) "
                                (if zot-coords "Y" "X") min max)
                        (format nil "~A-coord (~D=far north ~D=far south) "
                                (if zot-coords "X" "Y") min max)
                        (format nil "Level   (~D=top       ~D=bottom   ) "
                                min max))))
    (when zot-coords
      (rotatef (nth 0 prompts) (nth 1 prompts)))
    (let ((coords
           (loop
              for prompt in prompts
              collect
                (with-player-input (coord prompt :readf #'wiz-read-n)
                  (cond ((typep coord (list 'integer min max))
                         coord)
                        (t (setf coord
                                 (wiz-format-error *wiz-err*
                                                   error-text))))))))
      ;; (when (eq *cas-coords* 'zot)
      ;;        (rotatef (nth 1 coords) (nth 2 coords)))
      (if zot-coords
          (unwiz-coords coords)
          (reverse coords)))))

(defun adv-uses-runestaff (castle &optional coords)
  "What happens when an adventurer uses the runestaff?"
  (with-accessors ((history cas-history)
                   (adv cas-adventurer)) castle
      (let ((message (make-text)))
        (cond ((adv-without-item-p adv 'runestaff)
               (multiple-value-bind (without-item-events without-item-message)
                   (adv-tried-without-item castle 'runestaff)
                 (join-history history without-item-events)
                 (push-text message without-item-message)))
              (t
               (record-events history (make-event 'adv-used 'runestaff))
               (make-adv-teleport castle
                                  (or coords (read-castle-coordinates)))))
        (values castle message))))


;;; Lines 1670 - 1780 print status, room eval

(defun make-message-adv-enters-room (adv here creature)
  "What does the game report to player when the adventurer enters a room."
  (with-output-to-string (status)
    (unless (blind-p adv)
      (format status #'you-are-at here))
    (with-accessors ((st adv-st) (iq adv-iq) (dx adv-dx)
                     (fl adv-fl) (gp adv-gp) (lf adv-lf)
                     (wv adv-wv) (av adv-av)) adv
      (format status
              "~2&~{~:}~
               ~&~A / ~A~@[ / a lamp~]"
              "~A= ~A~^ "
              (list "ST" st "IQ" iq "DX" dx "Flares" fl "GP'S" gp)
              (text-of-weapon wv)
              (text-of-armor av)
              lf)
      (format status "~2&Here you find ~A"
              (text-of-creature creature)))))

(defun adv-enters-room (castle)
  "What happens when the adventurer enters a room?"
  (with-accessors ((adv cas-adventurer)
                   (here cas-adv-here)
                   (creature cas-creature-here)
                   (history cas-history)) castle
    (let ((message (make-message-adv-enters-room adv here creature)))
      (when (cas-room-cursed-p castle here)
        (join-history history (gain-curse castle))
        (when *curse-notify*
          (push-text message *curse-notify*)))
      (record-event history (make-event 'adv-found creature))
      (values castle message))))

(defun adv-finds-creature (castle)
  "What happens when the adventurer encounters a creature?"
  (with-accessors ((history cas-history)
                   (adv cas-adventurer)
                   (here cas-adv-here)
                   (creature cas-creature-here)) castle
    (let ((message (make-text)))
      (multiple-value-bind (find-creature-events find-creature-message)
          (funcall
           (symbol-function
            (case (type-of-creature creature)
              (gold-pieces 'adv-finds-gold-pieces)
              (flares      'adv-finds-flares)
              (warp        'adv-finds-warp)
              (sinkhole    'adv-finds-sinkhole)
              (treasure    'adv-finds-treasure)
              (vendor      'adv-meets-vendor)
              (monster     'adv-meets-adversary)
              (t           'adv-finds-room)))
           castle)
        (unless (castle-p find-creature-events)
          (join-history history find-creature-events))
        (unless (adv-room-mapped-p adv here creature)
          (join-history history (cas-adv-map-here castle)))
        (push-text message find-creature-message))
      (values castle message))))

;; is the "your choice" prompt ever used with numerical input?


;;;; Entering castle

(defun adv-enters-castle (castle adv)
  "An adventurer enters the castle."
  (declare (type castle castle)
           (type adventurer adv))
  (assert (null (cas-history castle)))
  (assert (null (cas-adventurer castle)))
  (with-accessors ((history cas-history)) castle
    (setf (cas-adventurer castle) adv)
    (let ((message (make-text)))
      (record-events history
                     (make-event 'adv-ate 'last-meal)
                     (make-event 'adv-entered-castle))
      (send-adv castle *entrance*)
      (push-text message
                 (format nil
                         "~&~|~2&Ok ~A, you enter the castle and begin."
                         (adv-race (cas-adventurer castle))))
      (values castle message))))


;;;; Atmospherics
;;; Turn lines 620 - 800

(defparameter *minor-event-outcomes*
  (list
   (make-outcome 'adv-stepped-on nil "stepped on a frog")
   (make-outcome 'adv-heard      nil
                 (lambda (stream)
                   (format stream
                           "hear ~A"
                           (random-elt '("a scream" "footsteps"
                                         "a wumpus" "thunder")))))
   (make-outcome 'adv-sneezed    nil "sneezed")
   (make-outcome 'adv-saw        nil "see a bat fly by")
   (make-outcome 'adv-smelled    nil
                 (lambda (stream)
                   (format stream
                           "smell ~A frying"
                           (text-of-creature (random-monster)))))
   (make-outcome 'adv-felt       nil "feel like you are being watched")
   (make-outcome 'game-announced nil "are playing Wizard's Castle"))
  "Minor events at beginning of turn")

;; check game states
;; increment turn counter
;; check to see if curses apply (runestaff or orb protect you)
;; apply curses
;; lethergy increments the turn counter (ruby red keeps you awakes)
;; leech steals 1 to 5 GPs (countered by pale pearl)
;; forgetfulness (lethe?) erases a random place on your map (green gem)
;; minor event (1 in 5 chance of something)
;; check up on handicaps if any
;; blindness cured by opal eye
;; book-glued-to-hand cured by blue flame
;; get input

;; gosub 3400 ->

;; 620 t=t+1 : if rf+of>0 then 690

(defun apply-curse (castle curse)
  "A curse strikes the adventurer."
  (with-accessors ((adv cas-adventurer)) castle
    (unless (artifact-p adv)
      (let ((outcome (funcall (get-castle-curse castle curse 'function) adv)))
        (when (event-p outcome)
          (record-events (cas-history castle) outcome))))))


;;;; Turn

(defun begin-turn (castle)
  "Every turn."
  (with-accessors ((adv cas-adventurer)
                   (history cas-history)) castle
    (loop
       for curse in (adv-cr adv)
       do (apply-curse castle curse))
    (let ((healed-sight (cure-adv-blindness adv))
          (hand-freed (unbind-adv-hand adv)))
      (join-history history healed-sight)
      (join-history history hand-freed)
      (with-output-to-string (message)
        (when (zerop (random 5))
          (let ((minor-events (if (blind-p adv)
                                  ;; When blind, you step on things more.
                                  (substitute
                                   (get-outcome 'adv-stepped-on *minor-event-outcomes*)
                                   (get-outcome 'adv-sees *minor-event-outcomes*)
                                   *minor-event-outcomes*)
                                  *minor-event-outcomes*)))
            (format message
                    "~2&You ~@?"
                    (text-of-outcome (random-elt minor-events)))))
        (when healed-sight
          (format message
                  "~&~A cures your blindness"
                  (text-of-creature (value-of-event (latest-event history)))))
        (when hand-freed
          (format message
                  "~&~A dissolves the book"
                  (text-of-creature (value-of-event (latest-event history)))))))))


;;;; Game

(defun quit-game (&optional castle)
  "The player quits."
  (with-accessors ((history cas-history)) castle
    (let* ((message (make-text)))
      (record-event history
                    (if (wiz-y-or-n-p "Do you really want to quit ")
                        (make-event 'player-quit-game)
                        (make-event 'player-error 'quit-canceled)))
      (unless (latest-event-p 'player-quit-game history)
        (push-text message
                   (wiz-format-error nil
                                     "Then don't say that you do")))
      (values castle message))))

(defun player-error (castle error-type &rest args)
  "Return event and messages when there's an input error."
  (values
   (make-history (make-event* 'player-error error-type args))
   (wiz-format-error *wiz-err*
                     "Stupid ~A that wasn't a valid command"
                     (adv-race (cas-adventurer castle)))))

(defun main-read (&optional (stream *standard-input*))
  "The reader for the main input."
  (with-input-from-string (str (read-line stream))
    (let ((i (read-char str nil))
          (j (peek-char nil str nil)))
      (string-upcase
       (cond ((null i) "")
             ((null j) (string i))
             (t
              (if (and (char-equal i #\D) (char-equal j #\R))
                  (concatenate 'string (list i j))
                  (string i))))))))

(defparameter *wiz-forms*
  '(adv-enters-castle adv-enters-room adv-finds-creature
    adv-drinks-pool adv-walks
    adv-uses-map adv-uses-flare adv-uses-lamp
    adv-opens adv-uses-crystal-orb adv-uses-runestaff
    quit-game player-help player-error)
  "The various wiz-forms")

(defun wiz-form-p (obj)
  (find (first obj) *wiz-forms*))

(defun make-wiz-form (wiz-form-name &rest args)
  (assert (find wiz-form-name *wiz-forms*))
  (list* wiz-form-name args))

(defun end-game-p (castle)
  (case (name-of-event (latest-event (cas-history castle)))
    (adv-slain        'death)
    (adv-left-castle  'exit)
    (player-quit-game 'quit)
    (t                nil)))

(defun main-eval (castle wiz-form)
  "Main evaluator"
  (assert (wiz-form-p wiz-form))
  (with-accessors ((history cas-history)) castle
    (loop
       do
         (multiple-value-bind (events message)
             (apply (first wiz-form) castle (rest wiz-form))
           (unless (castle-p events)
             (join-history history events))
           (when message
             (wiz-format *wiz-out* message))
           (setf wiz-form
                 (cond ((eq (first wiz-form) 'adv-enters-room)
                        (make-wiz-form 'adv-finds-creature))
                       ((latest-event-p 'adv-entered-room history)
                        (make-wiz-form 'adv-enters-room))
                       (t nil))))
       until (null wiz-form)))
  (unless (end-game-p castle)
    (begin-turn castle)))

(defparameter *main-commands*
  '(("DR" adv-drinks-pool)
    ("N"  adv-walks north)
    ("S"  adv-walks south)
    ("W"  adv-walks west)
    ("E"  adv-walks east)
    ("U"  adv-walks up)
    ("D"  adv-walks down)
    ("M"  adv-uses-map)
    ("F"  adv-uses-flare)
    ("L"  adv-uses-lamp)
    ("O"  adv-opens)
    ("G"  adv-uses-crystal-orb)
    ("T"  adv-uses-runestaff)
    ("Q"  quit-game)
    ("H"  player-help)
    ("?"  player-help))
  "The principle commands.")

(defun setup-main-input (&key (help *wiz-help*))
  (let ((commands (if help *main-commands* (butlast *main-commands* 2))))
    (lambda ()
      "Gets input and returns a form to be evaluated with the castle."
      (with-player-input (input "~2&Your move " :readf #'main-read)
        (let ((command (rest (assoc input commands :test #'string-equal))))
          (apply #'make-wiz-form
                 (or command
                     (list 'player-error 'main-input input))))))))

;; (defparameter *play-again* t)

(defun play-again-p ()
  ;; (when *play-again*
  (if (wiz-y-or-n-p "~2&Play-again ")
      'player-plays)
      nil)

(defun make-message-play-again (castle choice)
  (format nil
          (if (eq choice 'player-plays)
              "~2&Some ~A never learn~%"
              "~2&Maybe dumb ~A not so dumb after all~%")
          (adv-race (cas-adventurer castle))))

;; for q=1 to 750:next q:printchr$(12):gosub3270

(defparameter *that-sleep-of-death* 7.5
  "Pause on death.")

(defun sleep-of-death (&optional (duration *that-sleep-of-death*))
  (sleep duration))

(defun main (&key (adventurer (setup-adventurer)) (castle (setup-castle)))
  "The main game loop. If an adventurer is passed in, a castle also
passed in must not also have an adventurer already in it."
  ;; (setf *random-state* (make-random-state t))
  (loop
     with play-again = nil
     with main-input = (setup-main-input)
     for message = (main-eval castle
                              (make-wiz-form 'adv-enters-castle
                                             adventurer))
     do
       (with-accessors ((adv cas-adventurer)
                        (history cas-history)) castle
         (when message
           (wiz-format *wiz-out* message))
         (loop
            with ending = nil
            for message = (main-eval castle (funcall main-input))
            do
              (when message
                (wiz-format *wiz-out* message))
              (setf ending (end-game-p castle))
            until (not (null ending))
            finally
              (when (eq ending 'death)
                (sleep-of-death))
              (wiz-format *wiz-out*
                          (make-message-end-game adv
                                                 ending
                                                 (count-turns history)))))
       (setf play-again (play-again-p))
       (wiz-format *wiz-out*
                   (make-message-play-again castle play-again))
       (when play-again
         (setf adventurer nil castle nil))
     until (null play-again)))

(defun play (&rest args &key &allow-other-keys)
  (launch)
  (apply #'main args))

(defun play-ohare (&rest args &key &allow-other-keys)
  (let ((*curse-notify* t)
        (*wiz-title* *wiz-title-plain*)
        (*wiz-copyright* *wiz-copyright-power*)
        (*wiz-revision* *wiz-revision-ohare*)
        (*wiz-adaptation* *wiz-adaptation-ohare*)
        (*wiz-intro* *intro-text-ohare*))
    (launch)
    (apply #'main args)))

(defun play-stetson (&rest args &key &allow-other-keys)
  (let ((*wiz-title* *wiz-title-fancy*)
        (*wiz-copyright* nil)
        (*wiz-revision* nil)
        (*wiz-adaptation* nil)
        (*wiz-intro* *intro-text-dos*)
        (*wiz-setup-verbosity* nil)
        (*wiz-help* *help-text-dos*))
    (launch)
    (apply #'main args)))

;;; TODO: figure out lisp getopts.
