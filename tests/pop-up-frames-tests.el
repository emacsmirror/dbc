;;; pop-up-frames-tests.el --- Tests for pop-up-frames -*- lexical-binding: t -*-

;; Author: Matsievskiy S.V.
;; Maintainer: Matsievskiy S.V.
;; Version: 20.03.28
;; Package-Requires: (cl-lib ert)
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

;; Tests for pop-up-frames

;;; Code:

;; (require 'cl-lib)
(require 'ht)

;; newname oldname major minor
(ert-deftest pop-up-frames/test-match ()
  "Test `pop-up-frames/match-rule' function."
  (let ((newname "newname")
        (newmajor "newmajor")
        (newminor '("newminor"))
        (oldname "oldname")
        (oldmajor "oldmajor")
        (oldminor '("oldminor")))
    (pop-up-frames/add-ruleset "test" "test")
    (pop-up-frames/add-rule "test" "test"
                            :newname newname :newmajor newmajor :newminor newminor
                            :oldname oldname :oldmajor oldmajor :oldminor oldminor)
    (should (pop-up-frames/match-rule
             (ht-get (ht-get pop-up-frames/rules-list "test") "test")
             newname newmajor newminor oldname oldmajor oldminor))
    (should-not (pop-up-frames/match-rule
                 (ht-get (ht-get pop-up-frames/rules-list "test") "test")
                 "othername" newmajor newminor oldname oldmajor oldminor))
    (should-not (pop-up-frames/match-rule
                 (ht-get (ht-get pop-up-frames/rules-list "test") "test")
                 newname "othermajor" newminor oldname oldmajor oldminor))
    (should-not (pop-up-frames/match-rule
                 (ht-get (ht-get pop-up-frames/rules-list "test") "test")
                 newname newmajor '("otherminor") oldname oldmajor oldminor))
    (should-not (pop-up-frames/match-rule
                 (ht-get (ht-get pop-up-frames/rules-list "test") "test")
                 newname newmajor newminor "othername" oldmajor oldminor))
    (should-not (pop-up-frames/match-rule
                 (ht-get (ht-get pop-up-frames/rules-list "test") "test")
                 newname newmajor newminor oldname "othermajor" oldminor))
    (should-not (pop-up-frames/match-rule
                 (ht-get (ht-get pop-up-frames/rules-list "test") "test")
                 newname newmajor newminor oldname oldmajor '("otherminor")))

    (pop-up-frames/remove-rule "test" "test")
    (should-not (pop-up-frames/match-rule
                 (ht-get (ht-get pop-up-frames/rules-list "test") "test")
                 newname newmajor newminor oldname oldmajor oldminor))

    (pop-up-frames/add-rule "test" "test" :newname newname)
    (should (pop-up-frames/match-rule
             (ht-get (ht-get pop-up-frames/rules-list "test") "test")
             newname newmajor newminor oldname oldmajor oldminor))
    (should-not (pop-up-frames/match-rule
                 (ht-get (ht-get pop-up-frames/rules-list "test") "test")
                 "other" newmajor newminor oldname oldmajor oldminor))
    (should (pop-up-frames/match-rule
             (ht-get (ht-get pop-up-frames/rules-list "test") "test")
             newname "othermajor" newminor oldname oldmajor oldminor))
    (should (pop-up-frames/match-rule
             (ht-get (ht-get pop-up-frames/rules-list "test") "test")
             newname newmajor '("otherminor") oldname oldmajor oldminor))
    (should (pop-up-frames/match-rule
             (ht-get (ht-get pop-up-frames/rules-list "test") "test")
             newname newmajor newminor "othername" oldmajor oldminor))
    (should (pop-up-frames/match-rule
             (ht-get (ht-get pop-up-frames/rules-list "test") "test")
             newname newmajor newminor oldname "othermajor" oldminor))
    (should (pop-up-frames/match-rule
             (ht-get (ht-get pop-up-frames/rules-list "test") "test")
             newname newmajor newminor oldname oldmajor '("otherminor")))

    (pop-up-frames/clear-rules "test")
    (pop-up-frames/add-rule "test" "1" :oldname "1")
    (pop-up-frames/add-rule "test" "2" :oldname "2")
    (pop-up-frames/add-rule "test" "3" :oldname "3")
    (pop-up-frames/add-rule "test" "4" :oldname "4")
    (pop-up-frames/add-rule "test" "5" :oldname "5")
    (let ((oldname "1"))
      (should (cl-some
               (lambda (rule)
                 (pop-up-frames/match-rule
                  rule newname newmajor newminor
                  oldname oldmajor oldminor))
               (ht-values (ht-get pop-up-frames/rules-list "test"))))
      )
    (let ((oldname "3"))
      (should (cl-some
               (lambda (rule)
                 (pop-up-frames/match-rule
                  rule newname newmajor newminor
                  oldname oldmajor oldminor))
               (ht-values (ht-get pop-up-frames/rules-list "test"))))
      )
    (let ((oldname "5"))
      (should (cl-some
               (lambda (rule)
                 (pop-up-frames/match-rule
                  rule newname newmajor newminor
                  oldname oldmajor oldminor))
               (ht-values (ht-get pop-up-frames/rules-list "test"))))
      )
    (let ((oldname "7"))
      (should-not (cl-some
                   (lambda (rule)
                     (pop-up-frames/match-rule
                      rule newname newmajor newminor
                      oldname oldmajor oldminor))
                   (ht-values (ht-get pop-up-frames/rules-list "test"))))
      )

    (pop-up-frames/clear-rules "test")
    (pop-up-frames/add-rule "test" "test" :oldname oldname :oldminor "minor1")
    (should-not (pop-up-frames/match-rule
                 (ht-get (ht-get pop-up-frames/rules-list "test") "test")
                 newname newmajor newminor oldname oldmajor oldminor))
    (should (pop-up-frames/match-rule
             (ht-get (ht-get pop-up-frames/rules-list "test") "test")
             newname newmajor newminor oldname oldmajor '("minor1")))
    (pop-up-frames/add-rule "test" "test" :oldname oldname :oldminor '("minor1" "minor2"))
    (should-not (pop-up-frames/match-rule
                 (ht-get (ht-get pop-up-frames/rules-list "test") "test")
                 newname newmajor newminor oldname oldmajor '("minor1")))
    (should (pop-up-frames/match-rule
             (ht-get (ht-get pop-up-frames/rules-list "test") "test")
             newname newmajor newminor oldname oldmajor '("minor1" "minor2")))
    (should (pop-up-frames/match-rule
             (ht-get (ht-get pop-up-frames/rules-list "test") "test")
             newname newmajor newminor oldname oldmajor '("minor1" "minor2" "minor3")))

    (pop-up-frames/clear-rules "test")
    (pop-up-frames/add-rule "test" "test" :newname "\\*.+\\*")
    (should (pop-up-frames/match-rule
             (ht-get (ht-get pop-up-frames/rules-list "test") "test")
             "*R*" newmajor newminor oldname oldmajor oldminor))
    (should (pop-up-frames/match-rule
             (ht-get (ht-get pop-up-frames/rules-list "test") "test")
             "*python*" newmajor newminor oldname oldmajor oldminor))
    (should (pop-up-frames/match-rule
             (ht-get (ht-get pop-up-frames/rules-list "test") "test")
             "*shell*" newmajor newminor oldname oldmajor oldminor))
    (should-not (pop-up-frames/match-rule
                 (ht-get (ht-get pop-up-frames/rules-list "test") "test")
                 "shell*" newmajor newminor oldname oldmajor oldminor))
    (should-not (pop-up-frames/match-rule
                 (ht-get (ht-get pop-up-frames/rules-list "test") "test")
                 "*shell" newmajor newminor oldname oldmajor oldminor))
    (should-not (pop-up-frames/match-rule
                 (ht-get (ht-get pop-up-frames/rules-list "test") "test")
                 "**" newmajor newminor oldname oldmajor oldminor))
    ))


(provide 'pop-up-frames-tests)

;;; pop-up-frames-tests.el ends here
