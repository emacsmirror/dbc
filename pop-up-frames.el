;;; pop-up-frames.el --- Spawn new buffers in new frames according to rules -*- lexical-binding: t -*-

;; Author: Matsievskiy S.V.
;; Maintainer: Matsievskiy S.V.
;; Version: 20.03.28
;; Package-Requires: (cl-lib ht)
;; Homepage: gitlab.com
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
  :link   '(url-link :tag "GitLab" "https://gitlab.com/"))

(defcustom pop-up-frames/verbose nil
  "Print matching function arguments in echo area.
This may be useful when defining new rules"
  :tag  "Print matching function arguments in echo area"
  :type 'boolean)

(defcustom pop-up-frames/action '((display-buffer-reuse-window display-buffer-pop-up-frame) . ((reusable-frames . 0)))
  "Pop up frames `display-buffer' action.
See `display-buffer' help page for details."
  :tag  "Pop up frames `display-buffer' action"
  :type '(cons (repeat function) (alist :key-type symbol :value-type sexp)))

(defvar pop-up-frames/rules-list (ht-create) "List contains rules for `pop-up-frames' matching function.")

(defvar pop-up-frames/inhibit nil "Inhibit `pop-up-frames'.")

(defvar pop-up-frames/always-match nil "Always spawn new buffers in new frames.")

;; }}}

;; {{{ Interactive functions
(defun pop-up-frames/toggle-inhibit (arg)
   "Toggle inhibit pop-up-frames.

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

(defun pop-up-frames/toggle-always-match (arg)
   "Toggle pop-up-frames always match.

When given prefix `ARG', 0 turns always match off, 1 turns always match on"
   (interactive "P")
   (if arg
       (if (> arg 0)
           (setq pop-up-frames/always-match t)
         (setq pop-up-frames/always-match 0))
     (setq pop-up-frames/always-match (not pop-up-frames/always-match))
     )
   (message "pop-up-frames always match %s" (if pop-up-frames/always-match "on" "off"))
   )
;; }}}

;; {{{ Rule functions

(cl-defun pop-up-frames/add-rule (rulename &key newname oldname major minor)
  "Add rule `RULENAME' to open buffer in new frame when arguments match.

`OLDNAME' is a name of a buffer from which new buffer with name `NEWNAME' was opened.
Both `OLDNAME' and `NEWNAME' are regexp. `MAJOR' is a major mode name of the `OLDNAME' buffer.
`MINOR' is a minor mode name or a list of minor mode names of buffer `OLDNAME'.
All arguments optional. Empty argument always match."
  (ht-set! pop-up-frames/rules-list rulename (ht ('oldname oldname)
                                                 ('newname newname)
                                                 ('major major)
                                                 ('minor (if (listp minor)
                                                             minor
                                                           (list minor))))))

(defun pop-up-frames/remove-rule (rulename)
  "Remove `RULENAME' from rule list."
  (ht-remove! pop-up-frames/rules-list rulename))

(defun pop-up-frames/clear-rules ()
  "Remove all rules."
  (ht-clear! pop-up-frames/rules-list))

(defun pop-up-frames/match-rule (rule newname oldname major minor)
  "Match `RULE'.

`OLDNAME' is a name of a buffer from which new buffer with name `NEWNAME' was opened.
Both `OLDNAME' and `NEWNAME' are regexp. `MAJOR' is a major mode name of the `OLDNAME' buffer.
`MINOR' is a minor mode name or a list of minor mode names of buffer `OLDNAME'."
  (and rule
       (let ((regex (ht-get rule 'oldname)))
         (or (not regex)
             (string-match-p regex oldname)))
       (let ((regex (ht-get rule 'newname)))
         (or (not regex)
             (string-match-p regex newname)))
       (let ((mj (ht-get rule 'major)))
         (or (not mj)
             (string= mj major)))
       (let ((mn (ht-get rule 'minor)))
         (or (not mn)
             (cl-subsetp mn minor :test 'string=)))))

;; }}}

;; {{{ `display-buffer-alist' related

(defun pop-up-frames-switch-to-buffer (buffer alist)
  "Pop up frames switch to buffer command."
  (let ((newname buffer)
        (oldname (buffer-name))
        (major major-mode)
        (minor (cl-mapcar 'symbol-name minor-mode-list)))
    (when pop-up-frames/verbose
      (message "PUF: newname %s; oldname %s; major %s" newname oldname major))
    (unless pop-up-frames/inhibit
      (or pop-up-frames/always-match
          (cl-some (lambda (rule) (pop-up-frames/match-rule rule newname oldname major minor))
                   (ht-values pop-up-frames/rules-list))))))

(setq display-buffer-alist
      (append display-buffer-alist
              (list (cons 'pop-up-frames-switch-to-buffer pop-up-frames/action))
              ))

;; }}}

(provide 'pop-up-frames)

;;; pop-up-frames.el ends here
