(require 'elk-test)

(deftest "deftest"
  (let ((elk-test-list nil)
        (elk-test-map (make-hash-table :test 'equal))
        (elk-test-run-on-define nil))

    (deftest "test1" (assert-equal t t))
    (assert-equal '((assert-equal t t)) (gethash "test1" elk-test-map))
    (assert-nil (gethash "test2" elk-test-map))

    (deftest "test2" (assert-equal nil nil))
    (assert-equal '((assert-equal nil nil)) (gethash "test2" elk-test-map))

    (assert-equal '("test2" "test1") elk-test-list)

    (deftest "test1" (assert-equal t t))
    (assert-equal '("test2" "test1") elk-test-list)
    ))

(deftest "defsuite"
  (let ((elk-test-list nil)
        (elk-test-map (make-hash-table :test 'equal))
        (elk-test-run-on-define nil))

    (defsuite "suite1"
      (deftest "test" (assert-equal t t)))
    (let ((lookup (gethash "suite1" elk-test-map)))
      (assert-equal 'suite (car lookup))
      (assert-equal '("test") (caddr lookup))
      (assert-equal '((assert-equal t t)) (gethash "test" (cadr lookup)))
      (assert-nil (gethash "foo" (cadr lookup))))

    (defsuite "suite2"
      (deftest "test1" (assert-equal nil t))
      (deftest "test2" (assert-equal t nil)))
    (let ((lookup (gethash "suite2" elk-test-map)))
      (assert-equal 'suite (car lookup))
      (assert-equal '("test1" "test2") (caddr lookup))
      (assert-equal '((assert-equal nil t)) (gethash "test1" (cadr lookup)))
      (assert-equal '((assert-equal t nil)) (gethash "test2" (cadr lookup)))
      (assert-nil (gethash "test" (cadr lookup))))
    ))

(deftest "assert-equal"
  (assert-nil (elk-test-run-internal '((assert-equal t t))))
  (assert-nil (elk-test-run-internal '((assert-equal nil nil))))
  (assert-nil (elk-test-run-internal '((assert-equal 5.0 5.0))))
  (assert-nonnil (elk-test-run-internal '((assert-equal t nil))))
  (assert-nonnil (elk-test-run-internal '((assert-equal nil t))))
  (let ((foo t) (bar t))
    (assert-nonnil (elk-test-run-internal '((assert-equal 'foo 'bar))))
    (assert-nil (elk-test-run-internal '((assert-equal foo bar))))))

(deftest "assert-eq"
  (assert-nil (elk-test-run-internal '((assert-eq t t))))
  (assert-nil (elk-test-run-internal '((assert-eq nil nil))))
  (assert-nonnil (elk-test-run-internal '((assert-eq 5.0 5.0))))
  (assert-nonnil (elk-test-run-internal '((assert-eq t nil))))
  (assert-nonnil (elk-test-run-internal '((assert-eq nil t))))
    (let ((foo t) (bar t) (more nil))
    (assert-nonnil (elk-test-run-internal '((assert-eq 'foo 'bar))))
    (assert-nil (elk-test-run-internal '((assert-eq foo bar))))
    (assert-nonnil (elk-test-run-internal '((assert-eq foo more))))))

(deftest "assert-eql"
  (assert-nil (elk-test-run-internal '((assert-eql t t))))
  (assert-nil (elk-test-run-internal '((assert-eql nil nil))))
  (assert-nil (elk-test-run-internal '((assert-eql 5.0 5.0))))
  (assert-nonnil (elk-test-run-internal '((assert-eql t nil))))
  (assert-nonnil (elk-test-run-internal '((assert-eq nil t))))
  (let ((foo t) (bar t) (more nil))
    (assert-nonnil (elk-test-run-internal '((assert-eql 'foo 'bar))))
    (assert-nil (elk-test-run-internal '((assert-eql foo bar))))
    (assert-nonnil (elk-test-run-internal '((assert-eql foo more))))))

(deftest "assert-nonnil"
  (assert-nil (elk-test-run-internal '((assert-nonnil t))))
  (assert-nil (not (elk-test-run-internal '((assert-nonnil nil)))))
  (let ((foo t) (bar nil))
    (assert-nil (elk-test-run-internal '((assert-nonnil foo))))
    (assert-nil (not (elk-test-run-internal '((assert-nonnil bar)))))
    (assert-nil (elk-test-run-internal '((assert-nonnil 'foo))))))

(deftest "assert-t"
  (assert-nil (elk-test-run-internal '((assert-t t))))
  (assert-nonnil (elk-test-run-internal '((assert-t nil))))
  (assert-nonnil (elk-test-run-internal '((assert-t 'nonnil))))
  (let ((foo t) (bar nil))
    (assert-nil (elk-test-run-internal '((assert-t foo))))
    (assert-nonnil (elk-test-run-internal '((assert-t bar))))
    (assert-nonnil (elk-test-run-internal '((assert-t 'foo))))))

(deftest "assert-nil"
  (assert-nonnil (elk-test-run-internal '((assert-nil t))))
  (assert-nonnil (not (elk-test-run-internal '((assert-nil nil)))))
  (assert-nonnil (elk-test-run-internal '((assert-nil 'nonnil))))
  (let ((foo t) (bar nil))
    (assert-nonnil (elk-test-run-internal '((assert-nil foo))))
    (assert-nonnil (not (elk-test-run-internal '((assert-nil bar)))))
    (assert-nonnil (elk-test-run-internal '((assert-nil 'foo))))))

(deftest "assert-error"
  (assert-nil (elk-test-run-internal '((assert-error "x" (error "x")))))
  (assert-nil (elk-test-run-internal '((assert-error "x" t (error "x")))))
  (assert-nonnil (elk-test-run-internal '((assert-error "y" (error "x")))))
  (assert-nonnil (elk-test-run-internal '((assert-error "y" t (error "x")))))
  (assert-nonnil (elk-test-run-internal '((assert-error "y" nil))))
  (assert-nonnil (elk-test-run-internal '((assert-error "y" t nil)))))

(deftest "run"
  (let ((elk-test-run-on-define nil))
    (defsuite "suite1"
      (deftest "test" (assert-equal t t)))
    (assert-nil (run-elk-test "suite1"))

    (defsuite "suite1"
      (deftest "test" (assert-equal t nil)))
    (assert-nonnil (run-elk-test "suite1"))

    (deftest "test" (assert-equal t nil))
    (assert-nonnil (run-elk-test "test"))

    (deftest "test" (assert-equal t t))
    (assert-nil (run-elk-test "test"))

    (deftest "test2" (assert-equal t t))
    (build-suite "suite1" "test" "test2")
    (assert-nil (run-elk-test "suite1"))

    (deftest "test2" (assert-equal t nil))
    (build-suite "suite2" "test" "test2")
    (assert-nonnil (run-elk-test "suite2"))
    (assert-nil (run-elk-test "suite1"))
    ))
