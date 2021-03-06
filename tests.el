;;; tests.el --- Tests for display-buffer-control -*- lexical-binding: t -*-

;; Author: Matsievskiy S.V.
;; Maintainer: Matsievskiy S.V.
;; Version: 20.03.28
;; Package-Requires: ((emacs "24.4"))
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

;; Tests for display-buffer-control

;;; Code:

(require 'ht)

(ert-deftest dbc-tests-helpers ()
  "Test helper functions."
  (should (string= (dbc-gen-switch-function-name "other" 100)
                   (concat dbc-switch-function-basename "other-100")))
  (let ((dbc-switch-function-basename "test-"))
      (should (string= (dbc-gen-switch-function-name "test" 100) "test-test-100"))
      (should (string= (dbc-gen-switch-function-name "tt" 5) "test-tt-5"))
      (should (= (dbc-switch-function-get-priority 'test-some-100) 100))
      (should (= (dbc-switch-function-get-priority 'test-some-1) 1))
      (should (= (dbc-switch-function-get-priority 'test-some-5.5) 5.5)))
  (should (= (dbc-compare-minor "mode-test" "mode-test")))
  (should (= (dbc-compare-minor "Mode-Test" "MoDe-tEsT"))))

(ert-deftest dbc-tests-match ()
  "Test `dbc-match-rule' function."
  (let ((newfile "newfile")
        (newname "newname")
        (newmajor "newmajor")
        (newminor '("newminor"))
        (oldfile "oldfile")
        (oldname "oldname")
        (oldmajor "oldmajor")
        (oldminor '("oldminor")))
    (dbc-add-ruleset "test" "test")
    (dbc-add-rule "test" "test"
                            :newfile newfile :newname newname :newmajor newmajor :newminor newminor
                            :oldfile oldfile :oldname oldname :oldmajor oldmajor :oldminor oldminor)
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile newname newmajor newminor oldfile oldname oldmajor oldminor))
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 "otherfile" newname newmajor newminor oldfile oldname oldmajor oldminor))
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile "othername" newmajor newminor oldfile oldname oldmajor oldminor))
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile newname "othermajor" newminor oldfile oldname oldmajor oldminor))
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile newname newmajor '("otherminor") oldfile oldname oldmajor oldminor))
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile newname newmajor newminor "otherfile" oldname oldmajor oldminor))
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile newname newmajor newminor oldfile "othername" oldmajor oldminor))
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile newname newmajor newminor oldfile oldname "othermajor" oldminor))
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile newname newmajor newminor oldfile oldname oldmajor '("otherminor")))

    (dbc-remove-rule "test" "test")
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile newname newmajor newminor oldfile oldname oldmajor oldminor))

    (dbc-add-rule "test" "test" :newname newname)
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile newname newmajor newminor oldfile oldname oldmajor oldminor))
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             "otherfile" newname newmajor newminor oldfile oldname oldmajor oldminor))
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile "other" newmajor newminor oldfile oldname oldmajor oldminor))
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile newname "othermajor" newminor oldfile oldname oldmajor oldminor))
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile newname newmajor '("otherminor") oldfile oldname oldmajor oldminor))
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile newname newmajor newminor "otherfile" oldname oldmajor oldminor))
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile newname newmajor newminor oldfile "othername" oldmajor oldminor))
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile newname newmajor newminor oldfile oldname "othermajor" oldminor))
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile newname newmajor newminor oldfile oldname oldmajor '("otherminor")))

    (dbc-clear-rules "test")
    (dbc-add-rule "test" "1" :oldname "1")
    (dbc-add-rule "test" "2" :oldname "2")
    (dbc-add-rule "test" "3" :oldname "3")
    (dbc-add-rule "test" "4" :oldname "4")
    (dbc-add-rule "test" "5" :oldname "5")
    (let ((oldname "1"))
      (should (cl-some
               (lambda (rule)
                 (dbc-match-rule rule
                                 newfile newname newmajor newminor
                                 oldfile oldname oldmajor oldminor))
               (ht-values (ht-get dbc-rules-list "test")))))
    (let ((oldname "3"))
      (should (cl-some
               (lambda (rule)
                 (dbc-match-rule rule
                                 newfile newname newmajor newminor
                                 oldfile oldname oldmajor oldminor))
               (ht-values (ht-get dbc-rules-list "test")))))
    (let ((oldname "5"))
      (should (cl-some
               (lambda (rule)
                 (dbc-match-rule rule
                                 newfile newname newmajor newminor
                                 oldfile oldname oldmajor oldminor))
               (ht-values (ht-get dbc-rules-list "test")))))
    (let ((oldname "7"))
      (should-not (cl-some
                   (lambda (rule)
                     (dbc-match-rule rule
                                     newfile newname newmajor newminor
                                     oldfile oldname oldmajor oldminor))
                   (ht-values (ht-get dbc-rules-list "test")))))

    (dbc-clear-rules "test")
    (dbc-add-rule "test" "test" :oldname oldname :oldminor "minor1")
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile newname newmajor newminor oldfile oldname oldmajor oldminor))
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile newname newmajor newminor oldfile oldname oldmajor '("minor1")))
    (dbc-add-rule "test" "test" :oldname oldname :oldminor '("minor1" "minor2"))
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile newname newmajor newminor oldfile oldname oldmajor '("minor1")))
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile newname newmajor newminor oldfile oldname oldmajor '("minor1" "minor2")))
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile newname newmajor newminor oldfile oldname oldmajor '("minor1" "minor2" "minor3")))

    (dbc-clear-rules "test")
    (dbc-add-rule "test" "test" :newname "\\*.+\\*")
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile "*R*" newmajor newminor oldfile oldname oldmajor oldminor))
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile "*python*" newmajor newminor oldfile oldname oldmajor oldminor))
    (should (dbc-match-rule
             (ht-get (ht-get dbc-rules-list "test") "test")
             newfile "*shell*" newmajor newminor oldfile oldname oldmajor oldminor))
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile "shell*" newmajor newminor oldfile oldname oldmajor oldminor))
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile "*shell" newmajor newminor oldfile oldname oldmajor oldminor))
    (should-not (dbc-match-rule
                 (ht-get (ht-get dbc-rules-list "test") "test")
                 newfile "**" newmajor newminor oldfile oldname oldmajor oldminor))))

;;; tests.el ends here
