;;; pop-up-frames.el --- Spawn new buffers in new frames according to rules -*- lexical-binding: t -*-

;; Author: Matsievskiy S.V.
;; Maintainer: Matsievskiy S.V.
;; Version: 20.03.28
;; Package-Requires: (cl-lib ht)
;; Homepage: https://gitlab.com/matsievskiysv/pop-up-frames
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

;; Spawn new buffers in new frames according to rules

;;; Code:

(require 'cl-lib)
(require 'ht)

;; {{{ Vars & custom

(defgroup pop-up-frames nil
  "Pop up buffers in new frames according to rules"
  :group  'text
  :tag    "Pop Up Frames"
  :prefix "pop-up-frames-"
  :link   '(url-link :tag "GitLab" "https://gitlab.com/matsievskiysv/pop-up-frames"))

(defcustom pop-up-frames/verbose nil
  "Print matching function arguments in echo area.
This may be useful when defining new rules"
  :tag  "Print matching function arguments in echo area"
  :type 'boolean)

;; (defcustom pop-up-frames/action '((display-buffer-reuse-window display-buffer-pop-up-frame) . ((reusable-frames . 0)))
;;   "Pop up frames `display-buffer' action.
;; See `display-buffer' help page for details."
;;   :tag  "Pop up frames `display-buffer' action"
;;   :type '(cons (repeat function) (alist :key-type symbol :value-type sexp)))

(defvar pop-up-frames/rules-list (ht-create) "List contains rules for `pop-up-frames' matching function.")

(defvar pop-up-frames/inhibit nil "Inhibit `pop-up-frames'.")

;; (defvar pop-up-frames/always-match nil "Always spawn new buffers in new frames.")

;; }}}

;; {{{ Interactive functions
(defun pop-up-frames/toggle-inhibit (arg)
   "Toggle inhibit `pop-up-frames'.

When given prefix `ARG', 0 turns inhibit off, 1 turns inhibit on"
   (interactive "P")
   (if arg
       (if (> arg 0)
           (setq pop-up-frames/inhibit t)
         (setq pop-up-frames/inhibit 0))
     (setq pop-up-frames/inhibit (not pop-up-frames/inhibit))
     )
   (message "pop-up-frames inhibit %s" (if pop-up-frames/inhibit "on" "off"))
   )

;; (defun pop-up-frames/toggle-always-match (arg)
;;    "Toggle `pop-up-frames' always match.

;; When given prefix `ARG', 0 turns always match off, 1 turns always match on"
;;    (interactive "P")
;;    (if arg
;;        (if (> arg 0)
;;            (setq pop-up-frames/always-match t)
;;          (setq pop-up-frames/always-match 0))
;;      (setq pop-up-frames/always-match (not pop-up-frames/always-match))
;;      )
;;    (message "pop-up-frames always match %s" (if pop-up-frames/always-match "on" "off"))
;;    )
;; }}}

;; {{{ Ruleset functions

(defun pop-up-frames/gen-switch-function-name (ruleset)
  "Generate function name from given RULESET."
  (concat "pop-up-frames/switch-function-" ruleset))

(defmacro pop-up-frames/gen-switch-function (ruleset)
  "Generate switch function for `display-buffer'.

RULESET will be appended to function name and used as a hash table key."
  (let ((funname (intern (pop-up-frames/gen-switch-function-name ruleset))))
    `(defun ,funname (buffer &rest alist)
       "Pop up frames switch to BUFFER command.

Passed ALIST argument is ignored."
       (let ((newname buffer)
             (oldname (buffer-name))
             (oldmajor (symbol-name major-mode))
             (oldminor (cl-mapcar 'symbol-name minor-mode-list))
             (ruleset (ht-get pop-up-frames/rules-list ,ruleset)))
         (with-current-buffer buffer
           (setq newmajor (symbol-name major-mode)
                 newminor (cl-mapcar 'symbol-name minor-mode-list)))
         (when pop-up-frames/verbose
           (message "Display-buffer-control: ruleset %s; newname %s; newmajor %s; oldname %s; oldmajor %s"
                    ,ruleset newname newmajor oldname oldmajor))
         (unless pop-up-frames/inhibit
           (cl-some (lambda (rule) (pop-up-frames/match-rule rule newname newmajor newminor
                                                        oldname oldmajor oldminor))
                    (ht-values ruleset)))))))

(defun pop-up-frames/add-ruleset (ruleset action)
  "Add RULESET with ACTION to `display-buffer-alist`.

This function adds new RULESET `display-buffer-alist`.
RULESET contains a set of rules that are tested against buffer about
 to be shown.
If buffer match, it will be opened accorging to the ACTION.
ACTION is an argument to `display-buffer` ACTION argument."
  (ht-set! pop-up-frames/rules-list ruleset (ht-create))
  (let ((funcname (eval `(pop-up-frames/gen-switch-function ,ruleset))))
    (setq display-buffer-alist
          (append display-buffer-alist
                  (list (cons funcname action))))))

(defun pop-up-frames/remove-ruleset (ruleset)
  "Remove RULESET from `display-buffer-alist` and remove all rules in it."
  (setq display-buffer-alist (assq-delete-all (intern (pop-up-frames/gen-switch-function-name ruleset))
                                              display-buffer-alist))
  (ht-remove! pop-up-frames/rules-list ruleset))

;; }}}

;; {{{ Rule functions

(cl-defun pop-up-frames/add-rule (ruleset rulename &key newname newmajor newminor oldname oldmajor oldminor)
  "Add rule RULENAME to RULESET controlling how to open a new buffer.

OLDNAME, OLDMAJOR and OLDMINOR refer to the `buffer-name`,
 `major-mode` and `minor-mode-list` of the buffer, the call was made from.
NEWNAME, NEWMAJOR and NEWMINOR refer to the `buffer-name`,
 `major-mode` and `minor-mode-list` of the buffer about to be shown.

Both OLDNAME and NEWNAME are regexp strings.
All arguments optional.
Empty argument always match."
  (let ((rulesetht (ht-get pop-up-frames/rules-list ruleset)))
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

(defun pop-up-frames/remove-rule (ruleset rulename)
  "Remove RULENAME from RULESET."
  (let ((rulesetht (ht-get pop-up-frames/rules-list ruleset)))
    (unless rulesetht
      (error "Ruleset %s not found" ruleset))
    (ht-remove! rulesetht rulename)))

(defun pop-up-frames/clear-rules (ruleset)
  "Remove all rules from RULESET."
  (let ((rulesetht (ht-get pop-up-frames/rules-list ruleset)))
    (unless rulesetht
      (error "Ruleset %s not found" ruleset))
    (ht-clear! rulesetht)))

(defun pop-up-frames/match-rule (rule newname newmajor newminor oldname oldmajor oldminor)
  "Match RULE.

OLDNAME, OLDMAJOR and OLDMINOR refer to the `buffer-name`,
 `major-mode` and `minor-mode-list` of the buffer, the call was made from.
NEWNAME, NEWMAJOR and NEWMINOR refer to the `buffer-name`,
 `major-mode` and `minor-mode-list` of the buffer about to be shown.

Both OLDNAME and NEWNAME are regexp strings.
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
             (cl-subsetp mn newminor :test 'string=)))
       (let ((regex (ht-get rule 'oldname)))
         (or (not regex)
             (string-match-p regex oldname)))
       (let ((regex (ht-get rule 'oldmajor)))
         (or (not regex)
             (string-match-p regex oldmajor)))
       (let ((mn (ht-get rule 'oldminor)))
         (or (not mn)
             (cl-subsetp mn oldminor :test 'string=)))))

;; }}}

(provide 'pop-up-frames)

;;; pop-up-frames.el ends here
