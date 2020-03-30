;;; dbc.el --- Control how to open buffers -*- lexical-binding: t -*-

;; Author: Matsievskiy S.V.
;; Maintainer: Matsievskiy S.V.
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (ht "2.3"))
;; Homepage: https://gitlab.com/matsievskiysv/display-buffer-control
;; Keywords: convenience


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Control how to open buffers

;;; Code:

(require 'cl-lib)
(require 'ht)

(require 'dbc-actions)

;; {{{ Vars & custom

(defgroup dbc nil
  "Control how to open buffers"
  :group  'text
  :tag    "Display buffer control"
  :prefix "dbc-"
  :link   '(url-link :tag "GitLab" "https://gitlab.com/matsievskiysv/dbc"))

(defcustom dbc-verbose nil
  "Print matching function arguments in echo area.
This may be useful when defining new rules"
  :tag  "Print matching function arguments in echo area"
  :type 'boolean)

(defvar dbc-rules-list (ht-create) "List contains rules for dbc matching function.")

(defvar dbc-inhibit nil "Inhibit dbc.")

(defvar dbc-switch-function-basename "dbc-switch-function-" "Basename for switch functions.")

(defvar dbc-switch-function-default-priority 500 "Switch function default priority.")

;; }}}

;; {{{ Interactive functions
(defun dbc-toggle-inhibit (arg)
   "Toggle inhibit dbc.

When given prefix `ARG', 0 turns inhibit off, 1 turns inhibit on"
   (interactive "P")
   (if arg
       (if (> arg 0)
           (setq dbc-inhibit t)
         (setq dbc-inhibit 0))
     (setq dbc-inhibit (not dbc-inhibit))
     )
   (message "Display-buffer-control inhibit %s" (if dbc-inhibit "on" "off"))
   )

;; }}}

;; {{{ Ruleset functions

(defun dbc-gen-switch-function-name (ruleset priority)
  "Generate function name from given RULESET with PRIORITY."
  (concat dbc-switch-function-basename ruleset "-" (number-to-string priority)))

(defun dbc-switch-function-get-priority (func)
  "Get priority of the matching function FUNC."
  (let ((funcname (symbol-name func)))
    (if (string-match-p dbc-switch-function-basename funcname)
	(let* ((funcname (substring funcname (length dbc-switch-function-basename)))
	       (pr-pos (string-match-p "[[:digit:]]" funcname)))
	  (if pr-pos
	      (string-to-number (substring funcname pr-pos))
	    dbc-switch-function-default-priority))
      dbc-switch-function-default-priority)))

(defmacro dbc-gen-switch-function (ruleset priority)
  "Generate switch function for `display-buffer'.

RULESET will be appended to function name and used as a hash table key.
PRIORITY will be appended to function name."
  (let ((funname (intern (dbc-gen-switch-function-name ruleset priority))))
    `(defun ,funname (buffer &rest alist)
       "Pop up frames switch to BUFFER command.

Passed ALIST argument is ignored."
       (let ((newname buffer)
             (oldname (buffer-name))
             (oldmajor (symbol-name major-mode))
             (oldminor (cl-mapcar 'symbol-name minor-mode-list))
             (ruleset (ht-get dbc-rules-list ,ruleset)))
         (with-current-buffer buffer
           (setq newmajor (symbol-name major-mode)
                 newminor (cl-mapcar 'symbol-name minor-mode-list)))
         (when dbc-verbose
           (message "dbc: ruleset %s; newname %s; newmajor %s; oldname %s; oldmajor %s"
                    ,ruleset newname newmajor oldname oldmajor))
         (unless dbc-inhibit
           (cl-some (lambda (rule) (dbc-match-rule rule newname newmajor newminor
                                                        oldname oldmajor oldminor))
                    (ht-values ruleset)))))))

(defun dbc-add-ruleset (ruleset action &optional priority)
  "Add RULESET with ACTION to `display-buffer-alist`.

This function adds new RULESET `display-buffer-alist`.
RULESET contains a set of rules that are tested against buffer about
 to be shown.
If buffer match, it will be opened accorging to the ACTION.
ACTION is an argument to `display-buffer` ACTION argument.
Optional argument PRIORITY determines the order in which ruleset
matching functions get evaluated.
Default PRIORITY is defined by `dbc-switch-function-default-priority`"
  (when (string-match-p "[[:digit:]]" ruleset)
    (error "Ruleset name cannot contain digits"))
  (ht-set! dbc-rules-list ruleset (ht-create))
  (setq priority (if priority priority dbc-switch-function-default-priority))
  (let ((funcname (eval `(dbc-gen-switch-function ,ruleset ,priority))))
    (setq display-buffer-alist
          (cl-sort (append display-buffer-alist
                           (list (cons funcname action)))
                   #'<
                   :key (lambda (x) (dbc-switch-function-get-priority (car x)))))))

(defun dbc-remove-ruleset (ruleset)
  "Remove RULESET from `display-buffer-alist` and remove all rules in it."
  (setq display-buffer-alist
        (cl-sort
         (cl-loop for x in display-buffer-alist
                  unless (string-match-p
                          (concat "^"
                                  dbc-switch-function-basename
                                  ruleset
                                  "-[[:digit:]]+$")
                          (symbol-name (car x)))
                  collect x)
         #'<
         :key (lambda (x) (dbc-switch-function-get-priority (car x)))))
  (ht-remove! dbc-rules-list ruleset))

;; }}}

;; {{{ Rule functions

(cl-defun dbc-add-rule (ruleset rulename &key newname newmajor newminor oldname oldmajor oldminor)
  "Add rule RULENAME to RULESET controlling how to open a new buffer.

OLDNAME, OLDMAJOR and OLDMINOR refer to the `buffer-name`,
 `major-mode` and `minor-mode-list` of the buffer, the call was made from.
NEWNAME, NEWMAJOR and NEWMINOR refer to the `buffer-name`,
 `major-mode` and `minor-mode-list` of the buffer about to be shown.

OLDNAME, OLDMAJOR, NEWNAME and NEWMAJOR are regexp strings.
All arguments optional.
Empty argument always match."
  (let ((rulesetht (ht-get dbc-rules-list ruleset)))
    (unless rulesetht
      (error "Ruleset %s not found" ruleset))
    (ht-set! rulesetht
             rulename (ht ('newname newname)
                          ('newmajor newmajor)
                          ('newminor (if (listp newminor)
                                         newminor
                                       (list newminor)))
                          ('oldname oldname)
                          ('oldmajor oldmajor)
                          ('oldminor (if (listp oldminor)
                                         oldminor
                                       (list oldminor)))))))

(defun dbc-remove-rule (ruleset rulename)
  "Remove RULENAME from RULESET."
  (let ((rulesetht (ht-get dbc-rules-list ruleset)))
    (unless rulesetht
      (error "Ruleset %s not found" ruleset))
    (ht-remove! rulesetht rulename)))

(defun dbc-clear-rules (ruleset)
  "Remove all rules from RULESET."
  (let ((rulesetht (ht-get dbc-rules-list ruleset)))
    (unless rulesetht
      (error "Ruleset %s not found" ruleset))
    (ht-clear! rulesetht)))

(defun dbc-match-rule (rule newname newmajor newminor oldname oldmajor oldminor)
  "Match RULE.

OLDNAME, OLDMAJOR and OLDMINOR refer to the `buffer-name`,
 `major-mode` and `minor-mode-list` of the buffer, the call was made from.
NEWNAME, NEWMAJOR and NEWMINOR refer to the `buffer-name`,
 `major-mode` and `minor-mode-list` of the buffer about to be shown.

OLDNAME, OLDMAJOR, NEWNAME and NEWMAJOR are regexp strings.
All arguments optional.
Empty argument always match."
  (and rule
       (let ((regex (ht-get rule 'newname)))
         (or (not regex)
             (string-match-p regex newname)))
       (let ((regex (ht-get rule 'newmajor)))
         (or (not regex)
             (string-match-p regex newmajor)))
       (let ((mn (ht-get rule 'newminor)))
         (or (not mn)
             (cl-subsetp mn newminor :test #'string=)))
       (let ((regex (ht-get rule 'oldname)))
         (or (not regex)
             (string-match-p regex oldname)))
       (let ((regex (ht-get rule 'oldmajor)))
         (or (not regex)
             (string-match-p regex oldmajor)))
       (let ((mn (ht-get rule 'oldminor)))
         (or (not mn)
             (cl-subsetp mn oldminor :test #'string=)))))

;; }}}

(provide 'dbc)

;;; dbc.el ends here