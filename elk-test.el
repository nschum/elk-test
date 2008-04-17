;;; elk-test.el --- Emacs Lisp testing framework
;;
;; Copyright (C) 2006,2008 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 0.1
;; Keywords: lisp
;; URL: http://nschum.de/src/emacs/guess-style/
;; Compatibility: GNU Emacs 22.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Use `deftest' to define a test and `elk-test-run' to run it.
;; Create test bundles with `defsuite' or `build-suite'.
;; Verify your code with  `assert-equal', `assert-eq', `assert-eql',
;; `assert-nonnil', `assert-t', `assert-nil' and `assert-error'
;; to verify your code.
;;
;; (deftest "test1"
;;   (assert-equal t t)
;;   (assert-eq t 'foo))
;;
;; (defsuite "suite1"
;;   (deftest "test1" (assert-equal t t)))
;;
;; (deftest "test2"
;;   (assert-equal t t))
;; (build-suite "combined-suite" "test1" "test2")
;;
;; (elk-test-run "combined-suite")
;; (elk-test-run)
;;
;; To enable font-locking, add the following to your .emacs:
;;
;; (add-hook 'emacs-lisp-mode-hook 'elk-test-enable-font-lock)
;;
;;; Change Log:
;;
;; ????-??-?? (0.2)
;;    Renamed `run-elk-test' and `run-elk-tests-buffer'.
;;    Replaced `elk-test-error' with regular `error'.
;;    Added font-lock support.
;;
;; 2006-11-04 (0.1)
;;    Initial release.
;;
;;; Code:

(require 'cl)

(defgroup elk-test nil
  "Emacs Lisp testing framework"
  :group 'lisp)

(defface elk-test-deftest-face
  '((default (:inherit font-lock-keyword-face)))
  "*Face used for `deftest' and `defsuite' keywords."
  :group 'elk-test)

(defface elk-test-assertion-face
  '((default (:inherit font-lock-warning-face)))
  "*Face used for assertions in elk tests."
  :group 'elk-test)

(defvar elk-test-run-on-define nil
  "If non-nil, run elk-test tests/suites immediately when defining them.")

(defvar elk-test-map (make-hash-table :test 'equal)
  "A map of elk-test test/suite names to their implementation.")

(defvar elk-test-list nil
  "A list of all defined elk-test tests/suites.")

(defun elk-test-clear ()
  "Remove all tests from memory."
  (setq elk-test-map (make-hash-table :test 'equal)
        elk-test-list nil))

(defun elk-test-run (name &optional string-result)
  "Run the test case defined as NAME.
The result is a list of errors strings, unless STRING-RESULT is set, in which
case a message describing the errors or success is displayed and returned."
  (interactive
   (list (completing-read "Test name: " elk-test-list nil t)))
  (let ((name name))
  (let ((error-list nil)
        (test-or-suite (gethash name elk-test-map)))
    (if (not test-or-suite)
        (error "Undefined test <%s>" name)
      (if (equal (car test-or-suite) 'suite)
          ;; is test suite
          (let ((map (cadr test-or-suite)))
            (dolist (test (caddr test-or-suite))
              (setq error-list
                    (append error-list
                            (elk-test-run-internal (gethash test map))))))
        ;; is simple test
        (setq error-list (elk-test-run-internal test-or-suite)))
      (if (or string-result (interactive-p))
          (message (if error-list
                       (mapconcat 'identity error-list "\n")
                     "Test run was successful."))
        error-list)))))

(defun elk-test-run-buffer (&optional buffer)
  "Execute BUFFER as lisp code and run all tests therein."
  (interactive)
  (let* ((elk-test-list)
         (elk-test-map (make-hash-table :test 'equal))
         (elk-test-run-on-define nil)
         (inhibit-read-only t)
         (buffer-name (buffer-name buffer))
         (success t)
         (parse-res (condition-case err (eval-buffer buffer) (error err))))
    (if parse-res
        (message "Parsing buffer <%s> failed:\n%s"
                 buffer-name parse-res)
      (let ((out-buffer (get-buffer-create
                         (concat "*elk-test run " buffer-name "*")))
            failure)
        (with-current-buffer out-buffer
          (erase-buffer)
          (dolist (test elk-test-list)
            (message "running <%s>" test)
            (let ((results (elk-test-run test)))
              (when results
                (setq failure t)
                (insert "test <" test "> failed:\n")
                (dolist (result results)
                  (insert "* " result "\n"))))))
        (if failure
            (display-buffer out-buffer)
          (kill-buffer out-buffer)
          (message "Test run was successful."))))))

(defun elk-test-run-internal (test)
  (let (error-list)
    (dolist (sexp test)
      (condition-case err
          (eval sexp)
        (error (push (error-message-string err) error-list))))
    error-list))

(defmacro assert-equal (expected actual)
  "Assert that ACTUAL equals EXPECTED, or signal a warning."
  `(unless (equal ,expected ,actual)
    (error "assert-equal for <%s> failed: expected <%s>, was <%s>"
                    ',actual ,expected ,actual)))

(defmacro assert-eq (expected actual)
  "Assert that ACTUAL equals EXPECTED, or signal a warning."
  `(unless (eq ,expected ,actual)
    (error "assert-eq for <%s> failed: expected <%s>, was <%s>"
                    ',actual ,expected ,actual)))

(defmacro assert-eql (expected actual)
  "Assert that ACTUAL equals EXPECTED, or signal a warning."
  `(unless (eql ,expected ,actual)
    (error "assert-eql for <%s> failed: expected <%s>, was <%s>"
                    ',actual ,expected ,actual)))

(defmacro assert-nonnil (value)
  "Assert that VALUE is not nil, or signal a warning."
  `(unless ,value
     (error "assert-nonnil for <%s> failed: was <%s>"
                     ',value ,value)))

(defmacro assert-t (value)
  "Assert that VALUE is t, or signal a warning."
  `(unless (eq ,value t)
     (error "assert-t for <%s> failed: was <%s>"
                     ',value ,value)))

(defmacro assert-nil (value)
  "Assert that VALUE is nil, or signal a warning."
  `(when ,value
     (error "assert-nil for <%s> failed: was <%s>"
                     ',value ,value)))

(defmacro assert-error (error-message &rest body)
  "Assert that BODY raises an `error', or signal a warning.
ERROR-MESSAGE is the expected error string, use nil to accept any error.  Use
nil with caution, as errors like 'wrong-number-of-arguments' (likely caused by
typos) will also be caught!"
  `(condition-case err
       (progn ,@body
              ;; should not be reached, if body throws an error
              (error "assert-error for <%s> failed: did not raise an error"
                     '(progn . ,body)))
     (error
      (let ((actual (error-message-string err)))
        (unless (equal ,error-message actual)
          (error "assert-error for <%s> failed: expected <%s>, raised <%s>"
                 '(progn . ,body) ,error-message actual))))))

(defmacro deftest (name &rest body)
  "Define a test case.
Use `assert-equal', `assert-eq', `assert-eql', `assert-nonnil', `assert-t',
`assert-nil' and `assert-error' to verify the code."
  `(progn (unless (gethash ,name elk-test-map)
            (push ,name elk-test-list))
          (puthash ,name ',body elk-test-map)
          ,(if elk-test-run-on-define
               `(elk-test-run ',name ,t)
             name)))

(defmacro defsuite (name &rest body)
  "Define a test suite using a collection of `deftest' forms.
The resulting suite can be called with `elk-test-run' and parameter NAME."
  `(let ((suite
          (let ((elk-test-map (make-hash-table :test 'equal))
                (elk-test-list nil))
            ,@body
            (list 'suite elk-test-map (reverse elk-test-list)))))
     (unless (gethash ,name elk-test-map)
       (push ,name elk-test-list))
     (puthash ,name suite elk-test-map)
     ,(if elk-test-run-on-define
          `(elk-test-run ,name t)
        name)))

(defun build-suite (name &rest tests)
  "Define a test suite using a collection of test names.
The resulting suite can be run by calling `elk-test-run' with parameter NAME."
  (unless (gethash name elk-test-map)
    (push name elk-test-list))
  (puthash name
           (let ((map (make-hash-table :test 'equal))
                 (list nil))
             (dolist (test-name tests)
               (push test-name list)
               (when (gethash test-name map)
                 (error "Test used twice"))
               (let ((test (gethash test-name elk-test-map)))
                 (unless test
                   (error "Undefined test <%s>" test-name))
                 (puthash test-name test map)))
             (list 'suite map (reverse list)))
           elk-test-map)
  (if elk-test-run-on-define
      (elk-test-run "sample suite" t)
    name))

(defconst elk-test-font-lock-keywords
  `(("(\\_<\\(deftest\\|defsuite\\)\\_>" 1 'font-lock-keyword-face)
    (,(concat "(\\_<" (regexp-opt '("assert-equal" "assert-eq" "assert-eql"
                                    "assert-nonnil" "assert-t" "assert-nil"
                                    "assert-error") t)
              "\\_>") 1 'elk-test-assertion-face)))

(defun elk-test-enable-font-lock (&optional fontify)
  (interactive "p")
  (font-lock-add-keywords nil elk-test-font-lock-keywords)
  (when fontify
    (font-lock-fontify-buffer)))

(provide 'elk-test)
;;; elk-test.el ends here
