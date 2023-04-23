;; -*- mode: lisp -*-

(defsystem "org.wobh.common-lisp.games.wizards-castle"
  :description "Common Lisp implementation of _Wizards Castle_ (1980)"
  :version "0.0.1"
  :author "William Clifford <will@wobh.org>"
  :in-order-to ((test-op (test-op "org.wobh.common-lisp.games.wizards-castle/test")))
  :components ((:file "wizards-castle")
               (:file "wizards-castle-user"
                      :depends-on ("wizards-castle"))))

(defsystem "org.wobh.common-lisp.games.wizards-castle/test"
  :description "Tests for _Wizards Castle_"
  :depends-on ("org.wobh.common-lisp.games.wizards-castle")
  :perform (test-op (o c) (symbol-call 'wizards-castle-test
                                       'test-all))
  :components ((:file "wizards-castle-test")))
