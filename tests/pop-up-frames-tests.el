;;; pop-up-frames-tests.el --- Tests for pop-up-frames -*- lexical-binding: t -*-

;; Author: Matsievskiy S.V.
;; Maintainer: Matsievskiy S.V.
;; Version: 20.03.28
;; Package-Requires: (cl-lib ert)
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

;; Tests for pop-up-frames

;;; Code:

;; (require 'cl-lib)
(require 'ht)

;; newname oldname major minor
(ert-deftest pop-up-frames/test-match ()
  "Test `pop-up-frames/match-rule' function."
  (let ((newname "newname")
        (oldname "oldname")
        (major "major")
        (minor '("minor")))
    (pop-up-frames/add-rule "test" :newname newname :oldname oldname :major major :minor minor)
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major minor))
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") "othername" oldname major minor))
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname "othername" major minor))
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname "othermajor" minor))
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major '("otherminor")))

    (pop-up-frames/remove-rule "test")
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major minor))

    (pop-up-frames/add-rule "test" :newname newname)
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major minor))
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") "othername" oldname major minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname "othername" major minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname "othermajor" minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major '("otherminor")))

    (pop-up-frames/add-rule "test" :oldname oldname)
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") "othername" oldname major minor))
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname "othername" major minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname "othermajor" minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major '("otherminor")))

    (pop-up-frames/add-rule "test" :major major)
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") "othername" oldname major minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname "othername" major minor))
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname "othermajor" minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major '("otherminor")))

    (pop-up-frames/add-rule "test" :minor minor)
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") "othername" oldname major minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname "othername" major minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname "othermajor" minor))
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major '("otherminor")))
    (pop-up-frames/clear-rules)
    (pop-up-frames/add-rule "1" :oldname "1")
    (pop-up-frames/add-rule "2" :oldname "2")
    (pop-up-frames/add-rule "3" :oldname "3")
    (pop-up-frames/add-rule "4" :oldname "4")
    (pop-up-frames/add-rule "5" :oldname "5")
    (let ((oldname "1"))
      (should (cl-some (lambda (rule) (pop-up-frames/match-rule rule newname oldname major minor))
		       (ht-values pop-up-frames/rules-list)))
      )
    (let ((oldname "3"))
      (should (cl-some (lambda (rule) (pop-up-frames/match-rule rule newname oldname major minor))
		       (ht-values pop-up-frames/rules-list)))
      )
    (let ((oldname "5"))
      (should (cl-some (lambda (rule) (pop-up-frames/match-rule rule newname oldname major minor))
		       (ht-values pop-up-frames/rules-list)))
      )
    (let ((oldname "7"))
      (should-not (cl-some (lambda (rule) (pop-up-frames/match-rule rule newname oldname major minor))
			   (ht-values pop-up-frames/rules-list)))
      )
    (pop-up-frames/clear-rules)
    (pop-up-frames/add-rule "test" :oldname oldname :minor "minor1")
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major '("minor1")))
    (pop-up-frames/add-rule "test" :oldname oldname :minor '("minor1" "minor2"))
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major '("minor1")))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major '("minor1" "minor2")))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") newname oldname major '("minor1" "minor2" "minor3")))

    (pop-up-frames/clear-rules)
    (pop-up-frames/add-rule "test" :newname "\\*.+\\*")
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") "*R*" oldname major minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") "*python*" oldname major minor))
    (should (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") "*shell*" oldname major minor))
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") "shell*" oldname major minor))
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") "*shell" oldname major minor))
    (should-not (pop-up-frames/match-rule (ht-get pop-up-frames/rules-list "test") "**" oldname major minor))
    ))


(provide 'pop-up-frames-tests)

;;; pop-up-frames-tests.el ends here
