;;; flymake-statix.el --- Statix backend for flymake  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; URL: https://github.com/xFA25E/flymake-statix
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience files languages maint processes tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a flymake backend that lets the user analyze nix code
;; using statix.

;;;; Installation

;;;;; Package manager

;; If you've installed it with your package manager, you're done.
;; `flymake-statix-setup' is autoloaded, so you can call it right away.

;;;;; Manual

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'flymake-statix)

;;;; Usage

;; Add `flymake-statix-setup' to `nix-mode-hook':

;; (add-hook 'nix-mode-hook #'flymake-statix-setup)

;;;; Tips

;; + You can customize `flymake-statix-command' to your liking.

;; + If you enable `flymake-statix-enable-fix' option, then some warnings can be
;;   fixed by calling `flymake-statix-fix'.

;;;; Credits

;; This package would not have been possible without the statix[1] program.

;;  [1] https://github.com/nerdypepper/statix

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'flymake)
(require 'subr-x)

;;;; Customization

(defgroup flymake-statix nil
  "Settings for `flymake-statix'."
  :link '(url-link "https://github.com/xFA25E/flymake-statix")
  :group 'flymake)

(defcustom flymake-statix-command
  '("statix" "check" "--format" "json" "--stdin")
  "Statix command.
Make sure that it accepts json on stdin."
  :type '(repeat :tag "Command" (string :tag "Part"))
  :group 'flymake-statix)

(defcustom flymake-statix-enable-fix nil
  "Enable automatic fixing with `flymake-statix-fix'.
After changing this option, run `flymake-start'."
  :type '(boolean :tag "State")
  :group 'flymake-statix)

;;;; Commands

(defun flymake-statix-fix ()
  "Fix region at point using statix suggestion."
  (interactive)
  (when-let ((diag (cl-find-if-not #'null (flymake-diagnostics (point))
                                   :key #'flymake-diagnostic-data)))
    (save-restriction
      (widen)
      (delete-region (flymake-diagnostic-beg diag)
                     (flymake-diagnostic-end diag))
      (insert (flymake-diagnostic-data diag)))))

;;;; Functions

;;;;; Public

;;;###autoload
(defun flymake-statix-setup ()
  "Setup function for statix flymake backend.
Supposed to be added to `nix-mode-hook'."
  (add-hook 'flymake-diagnostic-functions #'flymake-statix nil t))

(defun flymake-statix (report-fn &rest _)
  "Flymake statix backend.
See Info node `(flymake)Backend functions' for REPORT-FN."
  (if (not (executable-find (car flymake-statix-command)))
      (let ((msg (concat "Cannot find a statix executable.  "
                         "See `flymake-statix-command' variable")))
        (funcall report-fn :panic :explanation msg))
    (save-restriction
      (widen)
      (dolist (report (flymake-statix--get-reports))
        (let-alist report
          (let ((type (flymake-statix--type-from-severity .severity)))
            (dolist (diagnostic .diagnostics)
              (thread-last
                (flymake-statix--make-diagnostics .note type diagnostic)
                (funcall report-fn)))))))
    (funcall report-fn nil)))

;;;;; Private

(defun flymake-statix--type-from-severity (severity)
  "Map SEVERITY to flymake type."
  (pcase-exhaustive severity
    ("Warn" :warning)
    ("Error" :error)
    ("Hint" :note)))

(defun flymake-statix--make-diagnostics (note type diagnostic)
  "Make diagnostics for the current buffer from DIAGNOSTIC.
NOTE is a string used in :note diagnostic.  TYPE is a flymake
type."
  (let-alist diagnostic
    (let* ((buf (current-buffer))
           (beg (car (flymake-diag-region buf .at.from.line .at.from.column)))
           (end (car (flymake-diag-region buf .at.to.line .at.to.column))))
      (cl-list* (flymake-make-diagnostic buf beg end type .message)
                (flymake-make-diagnostic buf beg end :note note)
                (when (and flymake-statix-enable-fix .suggestion)
                  (list (flymake-statix--make-suggestion .suggestion)))))))

(defun flymake-statix--make-suggestion (suggestion)
  "Make a fix SUGGESTION diagnostic for current buffer."
  (let-alist suggestion
    (let* ((buf (current-buffer))
           (beg (car (flymake-diag-region buf .at.from.line .at.from.column)))
           (end (car (flymake-diag-region buf .at.to.line .at.to.column)))
           (msg "Can be fixed by calling `flymake-statix-fix'"))
      (flymake-make-diagnostic buf beg end :note msg .fix))))

(defun flymake-statix--get-reports ()
  "Get statix reports for current buffer."
  (let ((buf (generate-new-buffer " *flymake statix*" t)))
    (unwind-protect
        (progn
          (pcase-let ((`(,command . ,args) flymake-statix-command))
            (apply #'call-process-region nil nil command nil buf nil args))
          (with-current-buffer buf
            (goto-char (point-min))
            (condition-case data
                (json-parse-buffer :object-type 'alist :array-type 'list
                                   :null-object nil :false-object nil)
              (:success (cdr (assq 'report data)))
              (json-end-of-file nil))))
      (kill-buffer buf))))

;;;; Footer

(provide 'flymake-statix)

;;; flymake-statix.el ends here
