;;; dictcc-test --- Tests for `dictcc'

;; Copyright (C) 2015 Marten Lienen

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; We are testing various aspects of how `dictcc' parses the translations from
;; the HTML results.

;;; Code:

(require 'dictcc)
(require 'dash)
(require 'f)

(defun dictcc-test--parse-html-string (string)
  "Parse STRING as an HTML document."
  (let ((doc (with-temp-buffer
              (insert string)
              (libxml-parse-html-region (point-min) (point-max)))))
    (caddr (caddr doc))))

(ert-deftest dictcc-parses-cell-to-translation ()
  (let* ((cell (dictcc-test--parse-html-string (f-read-text "test/cell.html")))
         (translation (dictcc--translation-from-cell cell)))
    (should (equal (dictcc--translation-text translation)
                   "math"))
    (should (-same-items-p (dictcc--translation-tags translation)
                           (list "Am." "coll." "math." "educ.")))))

(ert-deftest dictcc-parsing-ignores-spaces ()
  "This should not error."
  (let ((cell (dictcc-test--parse-html-string "<td> </td>")))
    (dictcc--translation-from-cell cell)))

(ert-deftest dictcc-multiword-tags-should-be-parsed-as-one ()
  (let* ((cell (dictcc-test--parse-html-string
                (f-read-text "test/split-tags.html")))
         (translation (dictcc--translation-from-cell cell)))
    (should (equal (dictcc--translation-tags translation)
                   (list "also ironic")))))

(ert-deftest dictcc-tags-are-parsed-correctly ()
  (should (equal (dictcc--tags-from-string "educ. [Am.] {var} [also ironic]")
                 (list "educ." "Am." "var" "also ironic"))))

(provide 'dictcc-test)
;;; dictcc-test.el ends here
