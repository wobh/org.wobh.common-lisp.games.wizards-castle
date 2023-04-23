;; -*- mode: lisp; coding: utf-8 -*-

(defpackage #:org.wobh.common-lisp.games.wizards-castle-test
  (:nicknames #:wizards-castle-test)
  (:use #:common-lisp)
  (:import-from #:wizards-castle
                #:adv-alive-p
                #:strength #:intelligence #:dexterity
                #:adv-st #:adv-dx #:adv-iq
                #:+adv-rank-max+
                #:set-adv-rank-max
                #:incf-adv-rank
                #:+adv-rank-min+
                #:set-adv-rank-min
                #:decf-adv-rank
                #:adv-of
                #:adv-rf
                #:text-of-creature
                #:make-adv-smarter
                #:make-adv-dumber
                #:*magic-user-iq-threshold*
                #:cast-spells-p
                #:incf-inv #:decf-inv
                #:foe-enwebbed-p
                #:adv-initiative-p
                #:lethargic-p
                #:blind-p
                #:cure-adv-blindness
                #:bound-p
                #:unbind-adv-hand
                #:cas-history
                #:cas-adventurer
                #:setup-castle
                #:make-history
                #:make-event
                #:outfit-with
                #:orb-of-zot
                #:adv-gained #:adv-lost
                #:adv-fl
                #:adv-gp
                #:make-adversary
                #:dragon
                #:foe-enwebbed
                #:lethargic-p
                #:lethargy #:leech #:forget
                #:adv-cured #:sight-restored
                #:adv-unbound #:book-burnt
                #:give-adv-treasure #:take-adv-treasure
                #:ruby-red #:norn-stone #:pale-pearl #:opal-eye
                #:green-gem #:blue-flame #:palantir #:silmaril
                #:*wiz-out*
                #:make-wiz-form
                #:main-eval
                #:adv-enters-castle
                #:adv-ate #:last-meal
                #:adv-entered-castle
                #:adv-entered-room
                #:adv-found #:*entrance*
                #:adv-mapped #:entrance)
  (:import-from #:wizards-castle-user
                #:make-test-adv)
  (:export #:test-all)
  (:export #:test-adv-alive-p-when-adv-st-is-zero
           #:test-adv-alive-p-when-adv-dx-is-zero
           #:test-adv-alive-p-when-adv-iq-is-zero
           #:test-adv-st-is-not-below-rank-minimum-when-decreased
           #:test-adv-dx-is-not-below-rank-minimum-when-decreased
           #:test-adv-iq-is-not-below-rank-minimum-when-decreased
           #:test-adv-st-is-not-above-rank-maximum-when-increased
           #:test-adv-dx-is-not-above-rank-maximum-when-increased
           #:test-adv-iq-is-not-above-rank-maximum-when-increased
           #:test-adv-fl-not-below-zero-when-decreased
           #:test-adv-gp-not-below-zero-when-decreased
           #:test-adv-artifact-exclusion
           #:test-adv-gains-spellcasting-ability
           #:test-adv-loses-spellcasting-ability
           #:test-adv-lethargy
           #:test-adv-blind
           #:test-adv-book-bound-to-hands
           #:test-foe-enwebbed-p
           #:test-setup-castle
           #:test-adv-enters-castle)
  (:documentation "ORG.WOBH.COMMON-LISP.GAMES.WIZARDS-CASTLE-TEST
"))

(in-package #:org.wobh.common-lisp.games.wizards-castle-test)

;;; Alive

(defun test-adv-alive-p-when-adv-st-is-zero
    (&optional (adv (make-test-adv :basic)))
  (assert (adv-alive-p adv)
          (adv)
          "Adventurer expected to be alive, but dead: ~W"
          adv)
  (setf (adv-st adv) 0)
  (assert (null (adv-alive-p adv))
          (adv)
          "Adventurer expected to be dead, but alive: ~W"
          adv))

(defun test-adv-alive-p-when-adv-dx-is-zero
    (&optional (adv (make-test-adv :basic)))
  (assert (adv-alive-p adv)
          (adv)
          "Adventurer expected to be alive, but dead: ~W"
          adv)
  (setf (adv-dx adv) 0)
  (assert (null (adv-alive-p adv))
          (adv)
          "Adventurer expected to be dead, but alive: ~W"
          adv))

(defun test-adv-alive-p-when-adv-iq-is-zero
    (&optional (adv (make-test-adv :basic)))
  (assert (adv-alive-p adv)
          (adv)
          "Adventurer expected to be alive, but dead: ~W"
          adv)
  (setf (adv-iq adv) 0)
  (assert (null (adv-alive-p adv))
          (adv)
          "Adventurer expected to be dead, but alive: ~W"
          adv))

;;; Rank limits respected

(defun test-adv-st-is-not-below-rank-minimum-when-decreased
    (&optional (adv (make-test-adv :basic)))
  (set-adv-rank-min adv 'adv-st)
  (decf-adv-rank (adv-st adv))
  (assert (= +adv-rank-min+ (adv-st adv))
          (adv)
          "Adventurer expected to have st minimum ~D: ~W"
          +adv-rank-min+
          adv))

(defun test-adv-dx-is-not-below-rank-minimum-when-decreased
    (&optional (adv (make-test-adv :basic)))
  (set-adv-rank-min adv 'adv-dx)
  (decf-adv-rank (adv-dx adv))
  (assert (= +adv-rank-min+ (adv-dx adv))
          (adv)
          "Adventurer expected to have dx minimum ~D: ~W"
          +adv-rank-min+
          adv))

(defun test-adv-iq-is-not-below-rank-minimum-when-decreased
    (&optional (adv (make-test-adv :basic)))
  (set-adv-rank-min adv 'adv-iq)
  (decf-adv-rank (adv-iq adv))
  (assert (= +adv-rank-min+ (adv-iq adv))
          (adv)
          "Adventurer expected to have iq ~D: ~W"
          +adv-rank-min+
          adv))

(defun test-adv-st-is-not-above-rank-maximum-when-increased
    (&optional (adv (make-test-adv :basic)))
  (set-adv-rank-max adv 'adv-st)
  (incf-adv-rank (adv-st adv))
  (assert (= +adv-rank-max+ (adv-st adv))
          (adv)
          "Adventurer expected to have st maximum ~D: ~W"
          +adv-rank-max+
          adv))

(defun test-adv-dx-is-not-above-rank-maximum-when-increased
    (&optional (adv (make-test-adv :basic)))
  (set-adv-rank-max adv 'adv-dx)
  (incf-adv-rank (adv-dx adv))
  (assert (= +adv-rank-max+ (adv-dx adv))
          (adv)
          "Adventurer expected to have dx maximum ~D: ~W"
          +adv-rank-max+
          adv))

(defun test-adv-iq-is-not-above-rank-maximum-when-increased
    (&optional (adv (make-test-adv :basic)))
  (set-adv-rank-max adv 'adv-iq)
  (incf-adv-rank (adv-iq adv))
  (assert (= +adv-rank-max+ (adv-iq adv))
          (adv)
          "Adventurer expected to have iq maximum ~D: ~W"
          +adv-rank-max+
          adv))

;;; Adventurer inventory limits

(defun test-adv-fl-not-below-zero-when-decreased
    (&optional (adv (make-test-adv :basic)))
  (with-accessors ((fl adv-fl)) adv
    (decf-inv fl (1+ fl))
    (assert (zerop fl)
            (adv)
            "This adventurer should have no flares")))

(defun test-adv-gp-not-below-zero-when-decreased
    (&optional (adv (make-test-adv :basic)))
  (with-accessors ((gp adv-gp)) adv
    (decf-inv gp (1+ gp))
    (assert (zerop gp)
            (adv)
            "This adventurer should have no gold pieces")))

;;; Acquiring the Orb of Zot

(defun test-adv-artifact-exclusion
    (&optional (adv (make-test-adv :sorceress)))
  (assert (and (null (adv-of adv)) (adv-rf adv)) ()
          "This adventurer should have ~A but not ~A: ~W"
          (text-of-creature 'runestaff)
          (text-of-creature 'orb-of-zot)
          adv)
  (outfit-with 'orb-of-zot adv)
  (assert (and (adv-of adv) (null (adv-rf adv))) ()
          "This adventurer should have ~A but not ~A: ~W"
          (text-of-creature 'orb-of-zot)
          (text-of-creature 'runestaff)
          adv))

;;; Limits to spellcasting

(defun test-adv-gains-spellcasting-ability
    (&optional (adv (make-test-adv :basic)))
  (assert (null (cast-spells-p adv))
          (adv)
          "This adventurer should not be able to cast spells: ~W"
          adv)
  (make-adv-smarter adv
                    (1+ (- *magic-user-iq-threshold* (adv-iq adv))))
  (assert (cast-spells-p adv)
          (adv)
          "This adventurer should be able to cast spells: ~W"
          adv))

(defun test-adv-loses-spellcasting-ability
    (&optional (adv  (make-test-adv :sorceress)))
  (assert (cast-spells-p adv)
          (adv)
          "This adventurer should be able to cast spells: ~W"
          adv)
  (make-adv-dumber adv
                   (1- (adv-iq adv)))
  (assert (null (cast-spells-p adv))
          (adv)
          "This adventurer should not be able to cast spells: ~W"
          adv))

;;; Curses and disabilities, effects of gaining or losing

(defun test-adv-lethargy
    (&optional (adv (make-test-adv :sorceress)))
  (assert (lethargic-p adv)
          (adv)
          "This adventurer should be lethargic: ~W"
          adv)
  (assert (null (adv-initiative-p adv))
          (adv)
          "This adventurer should never have initiative: ~W"
          adv)
  (give-adv-treasure adv 'ruby-red)
  (assert (null (lethargic-p adv))
          (adv)
          "While this adventurer has ~A, their lethargy should be cured: ~W"
          (text-of-creature 'ruby-red)
          adv)
  (take-adv-treasure adv 'ruby-red)
  (assert (lethargic-p adv)
          (adv)
          "When this adventurer loses ~A their lethargy should return: ~W"
          (text-of-creature 'ruby-red)
          adv))

(defun test-adv-blind
    (&optional (adv (make-test-adv :blind-adept)))
  (assert (blind-p adv)
          (adv)
          "This adventurer should be blind: ~W"
          adv)
  (assert (= +adv-rank-max+ (adv-dx adv))
          (adv)
          "This adventurer should have surpassing dexterity: ~W"
          adv)
  (assert (null (adv-initiative-p adv))
          (adv)
          "This adventurer should never have initiative: ~W"
          adv)
  ;; FIXME: the following doesn't work because the effect doesn't
  ;; occur until `begin-turn'.
  (assert (equal '(t
                   ((adv-gained opal-eye))
                   ((adv-cured sight-restored :with opal-eye))
                   nil
                   ((adv-lost opal-eye))
                   nil)
                 (list (blind-p adv)
                       (give-adv-treasure adv 'opal-eye)
                       (cure-adv-blindness adv)
                       (blind-p adv)
                       (take-adv-treasure adv 'opal-eye)
                       (blind-p adv)))
          () "When this adventurer gains ~A, it's blindness should be cured: ~W"
          (text-of-creature 'opal-eye) adv)
  (assert (adv-initiative-p adv)
          () "Cured of blindness, this adventurer should always have initiative: ~W"
          adv))

(defun test-adv-book-bound-to-hands
    (&optional (adv (make-test-adv :bookworm)))
  (assert (bound-p adv)
          (adv)
          "This adventurer should be bound: ~W"
          adv)
  (assert (= +adv-rank-max+ (adv-dx adv))
          (adv)
          "This adventurer should have surpassing dexterity: ~W"
          adv)
  (assert (= +adv-rank-max+ (adv-iq adv))
          (adv)
          "This adventurer should have surpassing intelligence: ~W"
          adv)
  (assert (equal '(t
                   ((adv-gained blue-flame))
                   ((adv-unbound book-burnt :with blue-flame))
                   nil
                   ((adv-lost blue-flame))
                   nil)
                 (list (bound-p adv)
                       (give-adv-treasure adv 'blue-flame)
                       (unbind-adv-hand adv)
                       (bound-p adv)
                       (take-adv-treasure adv 'blue-flame)
                       (bound-p adv)))))

;;; other tests

(defun test-foe-enwebbed-p
    (&optional (foe (make-adversary 'dragon)))
  (assert (not (foe-enwebbed-p foe))
          (foe)
          "Foe is not expected to be enwebbed yet: ~W"
          foe)
  (incf (foe-enwebbed foe))
  (assert (foe-enwebbed-p foe)
          (foe)
          "Foe is expected to be enwebbed: ~W"
          foe))

(defun test-setup-castle
    (&optional (cas (setup-castle nil)))
  (assert (null (cas-history cas))
          ((cas-history cas))
          "Castle should have empty history: ~W"
          (cas-history cas))
  (assert (null (cas-adventurer cas))
          ((cas-adventurer cas))
          "Castle should have no adventurer: ~W"
          (cas-adventurer cas)))

(defun test-adv-enters-castle
    (&optional
       (adv (make-test-adv :basic))
       (cas (setup-castle nil)))
  (let ((*wiz-out* nil))
    (main-eval cas (make-wiz-form 'adv-enters-castle adv))
    (assert (equal (cas-history cas)
                   (make-history (make-event 'adv-ate 'last-meal)
                                 (make-event 'adv-entered-castle)
                                 (make-event 'adv-entered-room *entrance*)
                                 (make-event 'adv-found 'entrance)
                                 (make-event 'adv-mapped 'entrance :at *entrance*)))
            ((cas-history cas))
            "Castle should have some history: ~W"
            (cas-history cas))
    (assert (equalp adv (cas-adventurer cas))
            ((cas-adventurer cas))
            "Castle should have an adventurer: ~W"
            (cas-adventurer cas))))

(defun test-all ()
  (loop
    with tests = '(test-adv-alive-p-when-adv-st-is-zero
                   test-adv-alive-p-when-adv-dx-is-zero
                   test-adv-alive-p-when-adv-iq-is-zero
                   test-adv-st-is-not-below-rank-minimum-when-decreased
                   test-adv-dx-is-not-below-rank-minimum-when-decreased
                   test-adv-iq-is-not-below-rank-minimum-when-decreased
                   test-adv-st-is-not-above-rank-maximum-when-increased
                   test-adv-dx-is-not-above-rank-maximum-when-increased
                   test-adv-iq-is-not-above-rank-maximum-when-increased
                   test-adv-fl-not-below-zero-when-decreased
                   test-adv-gp-not-below-zero-when-decreased
                   test-adv-artifact-exclusion
                   test-adv-gains-spellcasting-ability
                   test-adv-loses-spellcasting-ability
                   test-adv-lethargy
                   test-adv-blind
                   test-adv-book-bound-to-hands
                   test-foe-enwebbed-p
                   test-setup-castle
                   test-adv-enters-castle)
    for test in tests
    do (funcall test)
    finally (return t)))

