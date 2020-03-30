;;; dbc-actions.el --- Additional functions for display-buffer -*- lexical-binding: t -*-

;; Author: Matsievskiy S.V.
;; Maintainer: Matsievskiy S.V.
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
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

;; Additional functions for display-buffer

;;; Code:


(defun dbc-actions-right (buffer alist)
  "Try displaying BUFFER in a window to the right of the selected window.
Arguments are passed in ALIST.
This function is basically a copy/paste of `display-buffer-below-selected'."
  (let ((direction (if (assq 'side alist) (cdr (assq 'side alist)) 'below))
	window)
    (or (and (setq window (window-in-direction direction))
	     (eq buffer (window-buffer window))
	     (window--display-buffer buffer window 'reuse alist))
	(and (not (frame-parameter nil 'unsplittable))
	     (let ((split-width-threshold 0)
		   split-height-threshold)
	       (setq window (window--try-to-split-window
                             (selected-window) alist)))
	     (window--display-buffer
	      buffer window 'window alist display-buffer-mark-dedicated))
	(and (setq window (window-in-direction direction))
	     (not (window-dedicated-p window))
	     (window--display-buffer
	      buffer window 'reuse alist display-buffer-mark-dedicated)))))

(provide 'dbc-actions)

;;; dbc-actions.el ends here
