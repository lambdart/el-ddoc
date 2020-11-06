;;; dash-docs-test.el --- dash-docs tests
;; Copyright (C) 2013-2014  Raimon Grau
;; Copyright (C) 2013-2014  Toni Reina

;; Author: Raimon Grau <raimonster@gmail.com>
;;         Toni Reina  <areina0@gmail.com>
;; Version: 0.1
;; Keywords: docs

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
;;
;;; Commentary:

;;; Code:

;; dash-docs-sql-query

(ert-deftest dash-docs-sql-query/DASH-docset-type ()
  "Test DASH sql query (TYPE) composition."
  (should (equal
           "SELECT t.type, t.name, t.path FROM searchIndex t WHERE t.name like '%blpop%' ORDER BY LENGTH(t.name), LOWER(t.name) LIMIT 1000"
           (dash-docs-compose-sql-query "DASH" "blpop"))))

(ert-deftest dash-docs-sql-query/ZDASH-docset-type ()
  "Test ZDASH sql query (TYPE) composition."
  (should (equal "SELECT ty.ZTYPENAME, t.ZTOKENNAME, f.ZPATH, m.ZANCHOR FROM ZTOKEN t, ZTOKENTYPE ty, ZFILEPATH f, ZTOKENMETAINFORMATION m WHERE ty.Z_PK = t.ZTOKENTYPE AND f.Z_PK = m.ZFILE AND m.ZTOKEN = t.Z_PK AND t.ZTOKENNAME like '%blpop%' ORDER BY LENGTH(t.ZTOKENNAME), LOWER(t.ZTOKENNAME) LIMIT 1000"
                 (dash-docs-compose-sql-query "ZDASH" "blpop"))))

(ert-deftest dash-docs-sql-query/UNKNOWN-docset-type ()
  "Test SQL query unknown DOCSET type.."
  (should (equal nil (dash-docs-compose-sql-query "FOO" "blpop"))))

;;;; dash-docs-sub-docset-name-in-pattern

(ert-deftest dash-docs-sub-docset-name-in-pattern-test/docset-pattern ()
  "Test pattern string replace."
  (let ((pattern "Redis BLPOP")
        (docset "Redis"))
    (should (equal (dash-docs-sub-docset-name-in-pattern pattern docset) "BLPOP"))))

(ert-deftest dash-docs-sub-docset-name-in-pattern-test/docset-name ()
  "Test pattern string replace."
  (let ((pattern "BLPOP")
        (docset "Redis"))
    (should (equal (dash-docs-sub-docset-name-in-pattern pattern docset) pattern))))

(ert-deftest dash-docs-sub-docset-name-in-pattern-test/docset-special-name ()
  "Test pattern string replace."
  (let ((pattern "C++ printf")
        (docset "C++"))
    (should (equal (dash-docs-sub-docset-name-in-pattern pattern docset) "printf"))))

;;;; dash-docs-split-docset

(ert-deftest dash-docs-split-docset-test/filename-dash-entry ()
  (let ((docset
         '("C++" ("Struct" "std::timespec" "<dash_entry_name=timespec><dash_entry_originalName=std%3A%3Atimespec><dash_entry_menuDescription=std%3A%3Atimespec>en.cppreference.com/w/cpp/chrono/c/timespec.html"))))
    (should (equal '("C++" "en.cppreference.com/w/cpp/chrono/c/timespec.html" nil)
                   (dash-docs-split-docset docset)))))

;;;; dash-docs-compose-url

(ert-deftest dash-docs-compose-url-test/docset-urls ()
  "Test docset URL composition."
  ;; first variation
  (should (not (eq (string-match-p "Documents/three#anchor$"
                                   (dash-docs-compose-url "Python 3" "three" "anchor"))
                   nil)))

  ;; second
  (should (not (eq (string-match-p "Documents/three#anchor$"
                                   (dash-docs-compose-url "Css" "three#anchor" nil))
                   nil)))
  ;; last on
  (should (not (eq (string-match-p "Documents/three#anchor$"
                                   (dash-docs-compose-url "Redis" "three#anchor"))
                   nil))))

;;;; dash-docs-official-docsets

(ert-deftest dash-docs-official-docsets-test/docset-member ()
  "Should verify the list of available docsets."
  (let ((docsets (dash-docs-official-docsets)))
    (should (member "Ruby" docsets))))

(ert-deftest dash-docs-official-docsets-test/docset-no-member ()
  "Should verify the element is not a official docset member."
  (let ((docsets (dash-docs-official-docsets)))
    (should-not (member "FOO" docsets))))

;;;; dash-docs-activate-docset

(ert-deftest dash-docs-activate-docset-test/common-docsets ()
  (let ((dash-docs-common-docsets '("Redis" "Go" "CSS"))
        (dash-docs--connections
         '(("Redis" "/tmp/.docsets/Redis.docset/Contents/Resources/docSet.dsidx" "DASH")
           ("Go" "/tmp/.docsets/Go.docset/Contents/Resources/docSet.dsidx" "DASH")
           ("CSS" "/tmp/.docsets/CSS.docset/Contents/Resources/docSet.dsidx" "ZDASH"))))
    (dash-docs-activate-docset "Clojure")
    (should (equal '("Clojure" "Redis" "Go" "CSS") dash-docs-common-docsets))))

(provide 'dash-docs-test)

;;; dash-docs-test ends here
