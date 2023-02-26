;; -*- mode: lisp; coding: utf-8 -*-

(defpackage #:org.wobh.common-lisp.games.wizards-castle-test
  (:use #:common-lisp)
  (:local-nicknames (#:wizards-castle
                     #:org.wobh.common-lisp.games.wizards-castle))
  (:import-from #:wizards-castle
                #:adv-alive-p
                #:strength #:intelligence #:dexterity
                #:adv-st #:adv-dx #:adv-iq
                #:set-adv-rank-max
                #:incf-adv-rank
                #:+adv-rank-max+
                #:adv-of
                #:adv-rf
                #:text-of-creature
                #:make-adv-smarter
                #:make-adv-dumber
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
  (:local-nicknames (#:wizards-castle-user
                     #:org.wobh.common-lisp.games.wizards-castle-user))
  (:import-from #:wizards-castle-user
                #:make-test-adv)
  (:documentation "ORG.WOBH.COMMON-LISP.GAMES.WIZARDS-CASTLE-TEST
"))

(in-package #:org.wobh.common-lisp.games.wizards-castle-test)

(let ((*a* (make-test-adv :basic)))
  (assert (adv-alive-p *a*))
  (setf (adv-st *a*) 0)
  (assert (null (adv-alive-p *a*))))

(let ((*a* (make-test-adv :basic)))
  (assert (adv-alive-p *a*))
  (setf (adv-dx *a*) 0)
  (assert (null (adv-alive-p *a*))))

(let ((*a* (make-test-adv :basic)))
  (assert (adv-alive-p *a*))
  (setf (adv-iq *a*) 0)
  (assert (null (adv-alive-p *a*))))

;; FIXME: I don't trust that this works.
;; (loop
;;    with delta = -1
;;    for ranking in '(adv-st adv-iq adv-dx)
;;    do
;;      (let ((adv (make-test-adv :basic)))
;;        (assert (adv-alive-p adv))
;;        (set-adv-rank-min adv ranking)
;;        (assert (null (adv-alive-p adv)))
;;        (funcall (fdefinition (list 'setf ranking))
;;                 (decf-adv-rank delta (funcall ranking adv))
;;                 adv)
;;        (assert (= +adv-rank-min+ (funcall ranking adv)))))

(let ((*a* (make-test-adv :basic)))
  (with-accessors ((st adv-st) (iq adv-iq) (dx adv-dx)) *a*
    (assert (equal '(11 10 11) (list st iq dx))))
  (loop
     with delta = 1
     for ranking in '(adv-st adv-iq adv-dx)
     do
       (set-adv-rank-max *a* ranking)
       (assert (= +adv-rank-max+ (funcall ranking *a*)))
       (funcall (fdefinition (list 'setf ranking))
                (incf-adv-rank delta (funcall ranking *a*))
                *a*)
       (assert (= +adv-rank-max+ (funcall ranking *a*)))))

;;; Acquiring the Orb of Zot
(let ((*a* (make-test-adv :sorceress)))
  (assert (and (null (adv-of *a*)) (adv-rf *a*)) ()
          "This adventurer should have ~A but not ~A: ~S"
          (text-of-creature 'runestaff)
          (text-of-creature 'orb-of-zot)
          *a*)
  (outfit-with 'orb-of-zot *a*)
  (assert (and (adv-of *a*) (null (adv-rf *a*))) ()
          "This adventurer should have ~A but not ~A: ~S"
          (text-of-creature 'orb-of-zot)
          (text-of-creature 'runestaff)
          *a*))

(let ((*a* (make-test-adv :basic)))
  (assert (null (cast-spells-p *a*)) ()
          "This adventurer should not be able to cast spells: ~S" *a*)
  (assert (equal '(nil
                   ((adv-gained intelligence :by 5))
                   t
                   ((adv-lost intelligence :by 2))
                   nil)
                 (list (cast-spells-p *a*)
                       (make-adv-smarter *a* 5)
                       (cast-spells-p *a*)
                       (make-adv-dumber *a* 2)
                       (cast-spells-p *a*)))))

(let ((*a* (make-test-adv :sorceress)))
  (assert (cast-spells-p *a*) ()
          "This adventurer should be able to cast spells: ~S" *a*)
  (assert (equal '(t
                   ((adv-lost intelligence :by 5))
                   nil
                   ((adv-gained intelligence :by 2))
                   t)
                 (list (cast-spells-p *a*)
                       (make-adv-dumber *a* 5)
                       (cast-spells-p *a*)
                       (make-adv-smarter *a* 2)
                       (cast-spells-p *a*)))))

(let ((*a* (make-test-adv :basic)))
  (with-accessors ((fl adv-fl)) *a*
    (assert (zerop fl))
    (incf-inv fl 4)
    (assert (= 4 fl))
    (decf-inv fl 5)
    (assert (zerop fl))))

(let ((*a* (make-test-adv :barbarian)))
  (with-accessors ((gp adv-gp)) *a*
    (assert (zerop gp)
            () "This adventurer should have no money: ~S" *a*)
    (incf-inv gp 4)
    (assert (= 4 gp))
    (decf-inv gp 5)
    (assert (zerop gp))))

(let ((foe (make-adversary 'dragon)))
  (assert (not (foe-enwebbed-p foe)))
  (incf (foe-enwebbed foe))
  (assert (foe-enwebbed-p foe)))

(let ((*a* (make-test-adv :sorceress)))
  (assert (lethargic-p *a*)
          () "This adventurer should be lethargic: ~S" *a*)
  (assert (null (adv-initiative-p *a*))
          () "This adventurer should never have initiative: ~S." *a*)
  (assert (equal '(lethargy
                   ((adv-gained ruby-red))
                   nil
                   ((adv-lost ruby-red))
                   lethargy)
                 (list (lethargic-p *a*)
                       (give-adv-treasure *a* 'ruby-red)
                       (lethargic-p *a*)
                       (take-adv-treasure *a* 'ruby-red)
                       (lethargic-p *a*)))
          () "While this adventurer has ~A, their lethargy should be cured: ~name"
          (text-of-creature 'ruby-red) *a*))

(let ((*a* (make-test-adv :blind-adept)))
  (assert (blind-p *a*)
          () "This adventurer should be blind: ~S" *a*)
  (assert (= +adv-rank-max+ (adv-dx *a*))
          () "This adventurer should have surpassing dexterity: ~S" *a*)
  (assert (null (adv-initiative-p *a*))
          () "This adventurer should never have initiative: ~S" *a*)
  ;; FIXME: the following doesn't work because the effect doesn't
  ;; occur until `begin-turn'.
  (assert (equal '(t
                   ((adv-gained opal-eye))
                   ((adv-cured sight-restored :with opal-eye))
                   nil
                   ((adv-lost opal-eye))
                   nil)
                 (list (blind-p *a*)
                       (give-adv-treasure *a* 'opal-eye)
                       (cure-adv-blindness *a*)
                       (blind-p *a*)
                       (take-adv-treasure *a* 'opal-eye)
                       (blind-p *a*)))
          () "When this adventurer gains ~A, it's blindness should be cured: ~S"
          (text-of-creature 'opal-eye) *a*)
  (assert (adv-initiative-p *a*)
          () "Cured of blindness, this adventurer should always have initiative: ~S"
          *a*))

(let ((*a* (make-test-adv :bookworm)))
  (assert (bound-p *a*)
          () "This adventurer should be bound: ~S" *a*)
  (assert (= +adv-rank-max+ (adv-dx *a*))
          () "This adventurer should have surpassing dexterity: ~S" *a*)
  (assert (= +adv-rank-max+ (adv-iq *a*))
          () "This adventurer should have surpassing intelligence: ~S" *a*)
  (assert (equal '(t
                   ((adv-gained blue-flame))
                   ((adv-unbound book-burnt :with blue-flame))
                   nil
                   ((adv-lost blue-flame))
                   nil)
                 (list (bound-p *a*)
                       (give-adv-treasure *a* 'blue-flame)
                       (unbind-adv-hand *a*)
                       (bound-p *a*)
                       (take-adv-treasure *a* 'blue-flame)
                       (bound-p *a*)))))

(let ((*z* (setup-castle nil)))
  (assert (null (cas-history *z*))
          ((cas-history *z*))
          "Castle should have empty history: ~S"
          (cas-history *z*))
  (assert (null (cas-adventurer *z*))
          ((cas-adventurer *z*))
          "Castle should have no adventurer: ~S"
          (cas-adventurer *z*)))

(let ((*a* (make-test-adv :basic))
      (*z* (setup-castle nil))
      (*wiz-out* nil))
  (main-eval *z* (make-wiz-form 'adv-enters-castle *a*))
  (assert (equal (cas-history *z*)
                 (make-history (make-event 'adv-ate 'last-meal)
                               (make-event 'adv-entered-castle)
                               (make-event 'adv-entered-room *entrance*)
                               (make-event 'adv-found 'entrance)
                               (make-event 'adv-mapped 'entrance :at *entrance*)))
          ((cas-history *z*))
          "Castle should have some history: ~S"
          (cas-history *z*))
  (assert (equalp *a* (cas-adventurer *z*))
          ((cas-adventurer *z*))
          "Castle should have an adventurer: ~S"
          (cas-adventurer *z*)))
