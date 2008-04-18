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
;; Use `deftest' to define a test and `elk-test-group' to define test groups.
;; `elk-test-run' can run tests by name, and `elk-test-run-buffer' runs them by
;; buffer.
;;
;; Tests can be defined anywhere, but dedicated (.elk) test files are
;; encouraged.  A major mode for these can be enabled like this:
;;
;; (add-to-list 'auto-mode-alist '("\\.elk\\'" . elk-test-mode))
;;
;; Verify your code with  `assert-equal', `assert-eq', `assert-eql',
;; `assert-nonnil', `assert-t', `assert-nil' and `assert-error'
;; to verify your code.
;;
;; (deftest "test1"
;;   (assert-equal t t)
;;   (assert-eq t 'foo))
;;
;; (deftest "test2"
;;   (assert-equal t t))
;; (elk-test-group "test-group" "test1" "test2")
;;
;; (elk-test-run "test-group")
;; (elk-test-run)
;;
;;; Change Log:
;;
;; ????-??-?? (0.2)
;;    Renamed `run-elk-test' and `run-elk-tests-buffer'.
;;    Replaced `elk-test-error' with regular `error'.
;;    Added major made.
;;    `assert-error' now takes a regular expression as argument.
;;    Removed defsuite functionality (Use .elk files instead).
;;    `elk-test-run-buffer' no longer evaluates the entire buffer.
;;    Test results are now clickable links.
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
  "*Face used for `deftest' keyword."
  :group 'elk-test)

(defface elk-test-assertion-face
  '((default (:inherit font-lock-warning-face)))
  "*Face used for assertions in elk tests."
  :group 'elk-test)

(defface elk-test-success-face
  '((t (:inherit mode-line-buffer-id
        :background "dark olive green"
        :foreground "black")))
  "Face used for displaying a successful test result."
  :group 'elk-test)

(defface elk-test-success-modified-face
  '((t (:inherit elk-test-success-face
        :foreground "orange")))
  "Face used for displaying a successful test result in a modified buffer."
  :group 'elk-test)

(defface elk-test-failure-face
  '((t (:inherit mode-line-buffer-id
        :background "firebrick"
        :foreground "wheat")))
  "Face used for displaying a failed test result."
  :group 'elk-test)

(defvar elk-test-run-on-define nil
  "If non-nil, run elk-test tests/groups immediately when defining them.")

(defvar elk-test-map (make-hash-table :test 'equal)
  "A map of elk-test test/groups names to their implementation.")

(defvar elk-test-list nil
  "A list of all defined elk-test tests/groups.")

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
        (test-or-group (gethash name elk-test-map)))
    (if (not test-or-group)
        (error "Undefined test <%s>" name)
      (if (equal (car test-or-group) 'group)
          ;; is test group
          (let ((map (cadr test-or-group)))
            (dolist (test (caddr test-or-group))
              (setq error-list
                    (append error-list
                            (elk-test-run-internal (gethash test map))))))
        ;; is simple test
        (setq error-list (elk-test-run-internal test-or-group)))
      (if (or string-result (interactive-p))
          (message (if error-list
                       (mapconcat 'identity error-list "\n")
                     "Test run was successful."))
        error-list)))))

(defun elk-test-prepare-error-buffer ()
  "Create and prepare a buffer for displaying errors."
  (with-current-buffer (get-buffer-create "*elk-test*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq buffer-read-only t)
      (current-buffer))))

(defun elk-test-run-buffer (&optional buffer show-results)
  "Run tests defined with `deftest' in BUFFER.
Unless SHOW-RESULTS is nil, a buffer is created that lists all errors."
  (interactive (list nil t))
  (save-excursion
    (when buffer
      (set-buffer buffer))
    (goto-char (point-min))
    (let ((inhibit-read-only t)
          (num 0)
          sexp err errors)
      (condition-case err
          (while (not (looking-at "[ \f\t\n\r\v]*\\'"))
            (push (cons (point) (read (current-buffer))) sexp))
        (error
         ;; parse error
         (setq errors (list 0
                            (format "Parsing buffer <%s> failed:\n%s"
                                    (buffer-name) (error-message-string err))))
         (setq sexp nil)))
      (dolist (p sexp)
        (let ((form (cdr p))
              (line (car p)))
          ;; only evaluate deftest and require
          (if (and (cdr form) (equal (car form) 'require))
              (eval form)
            (when (and (cddr form) (equal (car form) 'deftest))
              (incf num)
              (setq err (elk-test-run-internal (cddr form)))
              (when err
                (push (list line err (cadr form)) errors))))))
      (when (eq (derived-mode-p 'elk-test-mode) 'elk-test-mode)
        (elk-test-set-buffer-state (if errors 'failure 'success)))
      (when show-results
        (message "%i tests run (%s errors)" num
                 (if errors (length errors) "No"))
        (when errors
          (elk-test-print-errors buffer errors)))
      errors)))

(defun elk-test-print-errors (original-buffer errors &optional error-buffer)
  (with-current-buffer (or error-buffer (elk-test-prepare-error-buffer))
    (let ((inhibit-read-only t)
          (keymap (make-sparse-keymap)))
      (define-key keymap [mouse-2] 'elk-test-click)
      (define-key keymap (kbd "<return>") 'elk-test-follow-link)
      (dolist (err errors)
        (insert "test <")
        (let ((beg (point)))
          (insert (car (cddr err)))
          (set-text-properties
           beg (point)
           `(mouse-face highlight
                        help-echo "mouse-1: Jump to this test"
                        face '(:underline t)
                        elk-test-buffer ,original-buffer
                        elk-test-point ,(car err)
                        keymap ,keymap
                        follow-link t)))
        (insert "> failed:\n")
        (dolist (result (cadr err))
          (insert "* " result "\n\n")))
      (setq buffer-read-only t))))

(defun elk-test-follow-link (pos)
  "Follow the link at POS in an error buffer."
  (interactive "d")
  (let ((pos (get-text-property pos 'elk-test-point))
        (buffer (get-text-property pos 'elk-test-buffer)))
    (push-mark)
    (switch-to-buffer buffer)
    (goto-char pos)
    (ignore-errors
      (forward-sexp)
      (backward-sexp))))

(defun elk-test-click (event)
  "Follow the link selected in an error buffer."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (elk-test-follow-link (posn-point (event-end event)))))

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

(defmacro assert-error (error-message-regexp &rest body)
  "Assert that BODY raises an `error', or signal a warning.
ERROR-MESSAGE-REGEXP is a regular expression describing the expected error
message.  nil accepts any error.  Use nil with caution, as errors like
'wrong-number-of-arguments' (likely caused by typos) will also be caught!"
  `(let (actual-error)
     (condition-case err
         (progn
           ,@body
           ;; should not be reached, if body throws an error
           (setq actual-error
                 (format "assert-error for <%s> failed: did not raise an error"
                         '(progn ,@body)))
           ;; jump out
           (error ""))
       (error
        (if actual-error
            (error actual-error)
          (when ,error-message-regexp
            (setq actual-error (error-message-string err))
            (unless (string-match ,error-message-regexp actual-error)
              (error "assert-error for <%s> failed: expected <%s>, raised <%s>"
                     '(progn . ,body) ,error-message-regexp actual-error))))))))

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

(defun elk-test-group (name &rest tests)
  "Define a test group using a collection of test names.
The resulting group can be run by calling `elk-test-run' with parameter NAME."
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
             (list 'group map (reverse list)))
           elk-test-map)
  (if elk-test-run-on-define
      (elk-test-run name t)
    name))

(defconst elk-test-font-lock-keywords
  `(("(\\_<\\(deftest\\)\\_>" 1 'font-lock-keyword-face)
    (,(concat "(\\_<" (regexp-opt '("assert-equal" "assert-eq" "assert-eql"
                                    "assert-nonnil" "assert-t" "assert-nil"
                                    "assert-error") t)
              "\\_>") 1 'elk-test-assertion-face)))

(defun elk-test-enable-font-lock (&optional fontify)
  (interactive "p")
  (font-lock-add-keywords nil elk-test-font-lock-keywords)
  (when fontify
    (font-lock-fontify-buffer)))

(defun elk-test-buffer-changed-hook (a b)
  "Hook used by `elk-test-set-buffer-state' to recognize modifications."
  (elk-test-set-buffer-state 'success-modified))

(defun elk-test-set-buffer-state (state &optional buffer)
  "Set BUFFER's success state to STATE.
STATE may be either 'success, 'success-modified or 'failure.
If the state is set to 'success, a hook will be installed to switch to
'success-modified on a buffer change automatically."
  (with-current-buffer (or buffer (current-buffer))
    (set (make-local-variable 'mode-name)
         (propertize mode-name 'face
                     (case state
                       ('success 'elk-test-success-face)
                       ('success-modified 'elk-test-success-modified-face)
                       ('failure 'elk-test-failure-face)))))
  (if (eq state 'success)
      (add-hook 'before-change-functions 'elk-test-buffer-changed-hook nil t)
    (remove-hook 'before-change-functions 'elk-test-buffer-changed-hook t)))

;;;###autoload
(define-derived-mode elk-test-mode emacs-lisp-mode
  "elk-test"
  "Minor mode used for elk tests."
  (elk-test-enable-font-lock))

(defun elk-test-buffer-list ()
  "List all buffers in `elk-test-mode'."
  (mapcan (lambda (b) (when (with-current-buffer b
                              (eq major-mode 'elk-test-mode))
                        (cons b nil)))
          (buffer-list)))

(defun elk-test-run-all-buffers (&optional show-results)
  "Run all buffers in `elk-test-mode'."
  (interactive "p")
  (let ((num-buffers 0)
        (num-errors 0)
        all-errors errors)
    (dolist (buffer (elk-test-buffer-list))
      (setq errors (elk-test-run-buffer buffer))
      (incf num-errors (length errors))
      (incf num-buffers)
      (push (cons buffer errors) all-errors))
    (when show-results
      (message "%i test buffers run (%s errors)" num-buffers
               (if errors num-errors "No"))
      (when errors
        (let ((error-buffer (elk-test-prepare-error-buffer)))
          (dolist (err all-errors)
            (elk-test-print-errors (car err) (cdr err) error-buffer)))))))

(provide 'elk-test)
;;; elk-test.el ends here
