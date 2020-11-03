;;; dash-docs.el --- Offline documentation browser using Dash docsets.  -*- lexical-binding: t; -*-
;; Copyright (C) 2013-2014  Raimon Grau
;; Copyright (C) 2013-2014  Toni Reina
;; Copyright (C) 2020       esac

;; Author: Raimon Grau <raimonster@gmail.com>
;;         Toni Reina  <areina0@gmail.com>
;;         Bryan Gilbert <bryan@bryan.sh>
;;         esac <esac-io@tutanota.com>
;;
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
;;
;; A library that provides ways to interact with dash-docs.
;;
;;; Code:

(require 'xml)
(require 'url)
(require 'url-http)
(require 'mm-decode)
(require 'json)

(eval-when-compile
  (require 'cl-macs))

(defgroup dash-docs nil
  "Search Dash docsets."
  :prefix "dash-docs-"
  :group 'applications)

(defcustom dash-docs-docsets-dir
  (expand-file-name "docsets" user-emacs-directory)
  "Default docsets directory.
If you're setting this option manually, set it to an absolute
path. You can use `expand-file-name' function for that."
  :set (lambda (opt val) (set opt (expand-file-name val)))
  :type 'string
  :group 'dash-docs)

(defcustom dash-docs-unofficial-url
  "https://dashes-to-dashes.herokuapp.com/docsets/contrib"
  "Unofficial (user) docsets URL."
  :type 'string
  :group 'dash-docs)

(defcustom dash-docs-min-length 3
  "Minimum length to start searching in docsets.
0 facilitates discoverability, but may be a bit heavy when lots
of docsets are active.  Between 0 and 3 is sane."
  :type 'integer
  :group 'dash-docs)

(defcustom dash-docs-retrieve-url-timeout 5
  "It should be a number that says (in seconds)
how long to wait for a response before giving up."
  :type 'integer
  :group 'dash-docs)

(defcustom dash-docs-candidate-format "%d %n (%t)"
  "Format of the displayed candidates.
Available formats are
   %d - docset name
   %n - name of the token
   %t - type of the token
   %f - file name"
  :type 'string
  :group 'dash-docs)

(defcustom dash-docs-debug-buffer "dash-docs-errors"
  "Debugging buffer name."
  :type 'string
  :group 'dash-docs)

(defcustom dash-docs-sql-debug-flag nil
  "Non-nil display sql query stderr in a buffer.
Setting this to nil may speed up sql query operations."
  :type 'boolean
  :group 'dash-docs)

(defcustom dash-docs-browser-func 'browse-url
  "Default function to browse Dash's docsets.
Suggested values are:
 * `browse-url'
 * `eww'"
  :type 'function
  :group 'dash-docs)

(defcustom dash-docs-ignored-docsets '("Man_Pages")
  "Return a list of ignored docsets.
These docsets are not available to install."
  :type 'list
  :group 'dash-docs)

(defcustom dash-docs-index-file
  (expand-file-name "cache/dash-docs.index" user-emacs-directory)
  "Official (cache) index file."
  :type 'string
  :group 'dash-docs)

(defcustom dash-docs-user-index-file
  (expand-file-name "cache/dash-docs-user.index" user-emacs-directory)
  "Unofficial (user cache) index file."
  :type 'string
  :group 'dash-docs)

(defvar dash-docs-docsets-url
  "https://api.github.com/repos/Kapeli/feeds/contents"
  "Official docsets URL.")

(defvar dash-docs-docsets-feed-url
  "https://raw.github.com/Kapeli/feeds/master"
  "Feeds URL for dash docsets.")

(defvar dash-docs-dash-sql-query
  "SELECT t.type, t.name, t.path FROM searchIndex t WHERE %s ORDER BY LENGTH(t.name), LOWER(t.name) LIMIT 1000"
  "DASH default sql query.")

(defvar dash-docs-zdash-sql-query
  "SELECT ty.ZTYPENAME, t.ZTOKENNAME, f.ZPATH, m.ZANCHOR FROM ZTOKEN t, ZTOKENTYPE ty, ZFILEPATH f, ZTOKENMETAINFORMATION m WHERE ty.Z_PK = t.ZTOKENTYPE AND f.Z_PK = m.ZFILE AND m.ZTOKEN = t.Z_PK AND %s ORDER BY LENGTH(t.ZTOKENNAME), LOWER(t.ZTOKENNAME) LIMIT 1000"
  "ZDASH default sql query.")

(defvar dash-docs-type-sql-query
  "SELECT name FROM sqlite_master WHERE type = 'table' LIMIT 1"
  "Default type sqlite3 query.")

(defvar dash-docs-common-docsets '()
  "List of Docsets to search active by default.")

(defvar dash-docs--connections nil
  "List of conses like (\"Go\" . connection).")

(defvar dash-docs--internal-vars
  '(dash-docs--connections
    dash-docs-common-docsets)
  "List of dash-docs internal variables.")

(defvar dash-docs-message-prefix "[dash-docs]: "
  "Internal dash-docs message prefix.")

(defvar dash-docs-mode nil
  "Non-nil means that `dash-docs-mode' is enabled.
Note: set this variable directly has no effect, use
`turn-on-dash-doc-mode' stead.")

(defvar dash-docs-json-cache-file
  (expand-file-name "cache/dash-docs-official.json" user-emacs-directory)
  "Official json cache file.")

(defmacro dash-docs--message (fmt &rest args)
  "Just an internal `message' helper."
  `(message (concat dash-docs-message-prefix ,fmt) ,@args))

(defvar dash-docs--sql-queries
  ;; DASH lambda function
  '((DASH . (lambda (pattern)
              (let ((like (dash-docs-sql-compose-like "t.name" pattern))
                    (query dash-docs-dash-sql-query))
                (format query like))))
    ;; ZDASH lambda function
    (ZDASH . (lambda (pattern)
               (let ((like (dash-docs-sql-compose-like "t.ZTOKENNAME" pattern))
                     (query dash-docs-zdash-sql-query))
                 (format query like)))))
  "SQL queries associative (NAME LAMBDA) list.")

(defun dash-docs--clean-internal-vars ()
  "Clean internal lists."
  ;; clean internal variables
  (dolist (var dash-docs--internal-vars)
    (set var nil)))

(defun dash-docs--created-dir ()
  "Create docsets default directory: `dash-docs-docsets-dir'."
  (let* ((dir dash-docs-docsets-dir)
         (prompt (format "Directory %s does not exist. Want to create it?"
                         dir)))
    (or (file-directory-p dir)
        (and (y-or-n-p prompt))
        (mkdir dir t))))

(defun dash-docs-docset-path (docset)
  "Return DOCSET (full) path."
  (let* (;; set directory
         (dir (expand-file-name dash-docs-docsets-dir))
         ;; format docset path
         (path (format "%s/%s.docset" dir docset)))
    ;; verify if the path exists
    (if (file-directory-p path) path nil)))

(defun dash-docs-docset-db-path (docset)
  "Return database DOCSET path."
  (let ((path (dash-docs-docset-path docset)))
    ;; if not found, error
    (if (not path)
        (error "missing docset '%s', please install it first" docset)
      ;; else return expanded path
      (expand-file-name "Contents/Resources/docSet.dsidx" path))))

(defun dash-docs-parse-sql-results (sql-result-string)
  "Parse SQL-RESULT-STRING splitting it by newline and '|' chars."
  (mapcar (lambda (string) (split-string string "|" t))
          (split-string sql-result-string "\n" t)))

(defun dash-docs-exec-query (db-path query)
  "Execute QUERY in the db located at DB-PATH and parse the results.
If there are errors, print them in `dash-docs--debug-buffer'."
  (dash-docs-parse-sql-results
   (with-output-to-string
     (let ((error-file (when dash-docs-sql-debug-flag
                         (make-temp-file "dash-docs-errors-file"))))
       (call-process "sqlite3" nil (list standard-output error-file) nil
                     ;; args for sqlite3:
                     "-list" "-init" "''" db-path query)
       ;; display errors, stolen from emacs' `shell-command` function
       (when (and error-file (file-exists-p error-file))
         (if (< 0 (nth 7 (file-attributes error-file)))
             (with-current-buffer (dash-docs--debug-buffer)
               (let ((pos-from-end (- (point-max) (point))))
                 (or (bobp) (insert "\f\n"))
                 ;; Do no formatting while reading error file,
                 ;; because that can run a shell command, and we
                 ;; don't want that to cause an infinite recursion.
                 (format-insert-file error-file nil)
                 ;; Put point after the inserted errors.
                 (goto-char (- (point-max) pos-from-end)))
               (display-buffer (current-buffer))))
         (delete-file error-file))))))

(defun dash-docs-docset-type (db-path)
  "Return the type of the docset based in db schema.
Possible values are \"DASH\" and \"ZDASH\".
The Argument DB-PATH should be a string with the sqlite db path."
  (let ((query dash-docs-type-sql-query))
    (if (member "searchIndex"
                (car (dash-docs-exec-query db-path query)))
        "DASH"
      "ZDASH")))

(defun dash-docs-sql-compose-like (column pattern)
  "Return a query fragment for a sql where clause.
Search in column COLUMN by multiple terms splitting the PATTERN
by whitespace and using like sql operator."
  (let ((conditions (mapcar
                     (lambda (word)
                       (format "%s like '%%%s%%'" column word))
                     (split-string pattern " "))))
    (format "%s" (mapconcat 'identity conditions " AND "))))

(defun dash-docs-compose-sql-query (docset-type pattern)
  "Return a SQL query to search documentation in dash docsets.
A different query is returned depending on DOCSET-TYPE.
PATTERN is used to compose the SQL WHERE clause."
  (let ((compose-select-query-func
         (cdr (assoc (intern docset-type)
                     dash-docs--sql-queries))))
    (when compose-select-query-func
      (funcall compose-select-query-func pattern))))

(defun dash-docs-sql-search (docset pattern)
  "Search for and PATTERN patter using the select DOCSET.

Return a list of db results.

Ex:

'((\"func\" \"BLPOP\" \"commands/blpop.html\")
 (\"func\" \"PUBLISH\" \"commands/publish.html\")
 (\"func\" \"problems\" \"topics/problems.html\"))"

  (let* ((docset-type (cl-caddr docset))
         ;; set docset database path
         (db-path (cadr docset))
         ;; set the sql query to be executed
         (query (dash-docs-compose-sql-query docset-type pattern)))
    ;; execute the query
    (dash-docs-exec-query db-path query)))

(defun dash-docs--format-row (connection row)
  "Format candidate (ROW) using its CONNECTION."
  (cons (format-spec dash-docs-candidate-format
                     (list (cons ?d (cl-first connection))
                           (cons ?n (cl-second row))
                           (cons ?t (cl-first row))
                           (cons ?f (replace-regexp-in-string
                                     "^.*/\\([^/]*\\)\\.html?#?.*"
                                     "\\1"
                                     (cl-third row)))))
        (list (car connection) row)))

(defun dash-docs-search-docset-entry (connection pattern)
  "Search PATTERN in CONNECTION, return a list of formatted rows."
  (cl-loop for row in (dash-docs-sql-search connection pattern)
           collect (dash-docs--format-row connection row)))

(defun dash-docs-search-docset-entries (pattern)
  "Search a PATTERN in all available connected docsets."
  (cl-loop for connection in (dash-docs-search-connections pattern)
           appending (dash-docs-search-docset-entry connection pattern)))

(defun dash-docs-buffer-local-docsets ()
  "Get the docsets configured for the current buffer."
  (or (and (boundp 'dash-docs-docsets) dash-docs-docsets) '()))

(defun dash-docs-add-connection (docset)
  "Add DOCSET connection to `dash-docs--connections'."
  ;; verify if docset is already present
  (when (not (assoc docset dash-docs--connections))
    ;; connection parameters
    (let* ((db-path (dash-docs-docset-db-path docset))
           (type (dash-docs-docset-type db-path))
           (connection (list docset db-path type)))
      ;; add connection to dash-docs--connections
      (push connection dash-docs--connections))))

(defun dash-docs-add-buffer-local-connections ()
  "Add dash-docs buffer local connections."
  (dolist (docset (dash-docs-buffer-local-docsets))
    (dash-docs-add-connection docset)))

(defun dash-docs-connections ()
  "Return available connections."
  (let ((docsets (dash-docs-buffer-local-docsets)))
    ;; append local docsets with common ones
    (setq docsets (append docsets dash-docs-common-docsets))
    ;; get unique 'connections' associated with the docsets
    (delq nil
          (mapcar (lambda (docset)
                    (assoc docset dash-docs--connections))
                  docsets))))

(defun dash-docs-search-connections (pattern)
  "Search PATTERN in the available connections.
If PATTERN starts with the name of a docset,
narrow the used connections to just that one."
  (let ((connections (dash-docs-connections)))
    ;; if no pattern just return all available connections
    (if (equal pattern "") connections
      ;; return connection filter by pattern:
      ;; docset name
      (cl-loop for connection in connections
               if (string-prefix-p (car connection)
                                   pattern t)
               return (list connection)))))

(defun dash-docs-del-connection (docset)
  "Remove DOCSET connection from `dash-docs--connections'."
  (let*  ((connections (dash-docs-connections))
          (connection (assoc docset connections)))
    (when (member connection connections)
      (setq dash-docs--connections
            (delq connection connections)))))

(defun dash-docs--write-file (contents file)
  "Write CONTENTS in the target FILE."
  (with-temp-file file
    ;; insert file contents in the file buffer (implicit)
    (prin1 contents (current-buffer))))

(defun dash-docs--read-file (file)
  "Return FILE contents."
  (when (file-exists-p file)
    (let ((contents (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-substring-no-properties (point-min)
                                                      (point-max)))))
      (read contents))))

(defun dash-docs-fetch-json (url file)
  "Copy JSON contents to FILE from the target URL."
  (let ((json-contents nil))
    ;; copy url json to cache file
    (url-copy-file url file t nil)
    ;; switch/case equivalent
    (cond
     ;; verify if file exists
     ((not (file-exists-p file))
      ;; debug message
      (dash-docs--message "was not possible to retrieve json file")
      ;; return nil
      nil)
     ;; default, read json contents
     (t
      (setq json-contents
            (with-temp-buffer
              (insert-file-contents-literally file)
              (json-read)))))
    ;; return json content
    json-contents))

(defun dash-docs--parse-index (json)
  "Parse index JSON elements."
  (delq nil
        (mapcar (lambda (element)
                  (let* ((name (assoc-default 'name element))
                         (ext (file-name-extension name)))
                    (when (equal ext "xml")
                      (list name))))
                json)))

(defun dash-docs-setup-index ()
  "Setup the official docset's index."
  ;; verify if file already exists
  (when (not (file-exists-p dash-docs-json-cache-file))
    ;; set json file and parse the index
    (let* ((json (dash-docs-fetch-json dash-docs-docsets-url
                                       dash-docs-json-cache-file))
           (index (dash-docs--parse-index json)))
      ;; write the file unless we don't have any parsed index
      (unless (not index)
        ;; write json to the index file
        (dash-docs--write-file index dash-docs-index-file)))))

(defun dash-docs-unofficial-docsets ()
  "Return a list of lists with docsets contributed by users.
The first element is the docset's name second the docset's archive url."
  (let ((docsets (dash-docs--read-file dash-docs-user-index-file)))
    ;; parse docsets list
    (mapcar (lambda (docset)
              (list (assoc 'name docset)
                    (assoc 'archive docset)))
            docsets)))

(defun dash-docs-official-docsets ()
  "Return a list of official docsets."
  (let ((index (dash-docs--read-file dash-docs-index-file))
        (docsets nil))
    ;; for each docset in index clean the ".xml" substring
    (dolist (docset index)
      (push (string-replace ".xml" "" (car docset)) docsets))
    ;; return docsets
    docsets))

(defun dash-docs-extract-archive (docset-temp-file)
  "Extract DOCSET-TEMP-FILE to `dash-docs-docsets-dir'.
Return the folder that was newly extracted."
  (with-temp-buffer
    (let* ((program (list "tar" nil t nil))
           (args (list "xfv" docset-temp-file "-C" dash-docs-docsets-dir))
           (result (apply #'call-process (append program args nil)))
           (folder nil))
      (cond
       ;; too long?
       ((and (not (equal result 0))
             ;; TODO: Adjust to proper text. Also requires correct locale.
             (search-backward "too long" nil t))
        ;; signals an error message
        (error "Failed extract %s to %s."
               docset-temp-file
               dash-docs-docsets-dir))
       ;; verify call-process result
       ((not (equal result 0))
        ;; signals an error message
        (error "Error %s, failed to extract %s to %s."
               result
               docset-temp-file
               dash-docs-docsets-dir)))
      ;; got the point
      (goto-char (point-max))
      ;; set path string
      (setq folder (car (split-string (thing-at-point 'line) "\\." t)))
      ;; format the folder and return it (string)
      (replace-regexp-in-string "^x " "" folder))))

(defun dash-docs--fetch-extract-archive (url temp-file)
  "Fetch URL and extract the retrieved docset TEMP-FILE archive."
  ;; http request (async version of url-copy-file)
  (url-http url
            (lambda (&rest args)
              ;; download the file
              (let* ((temp-file (car args))
                     (buffer (current-buffer))
                     (handle (with-current-buffer buffer
                               (mm-dissect-buffer t))))
                ;; return the default file protection for created files
                (let ((mm-attachment-file-modes (default-file-modes)))
                  (mm-save-part-to-file handle temp-file))
                ;; necessary?
                (kill-buffer buffer)
                ;; destroy MIME parts
                (mm-destroy-parts handle)
                ;; extract docset
                (dash-docs-extract-archive temp-file)))
            ;; temporary file archive
            (list temp-file)))

;; (defun dash-docs-fetch-xlm (&rest args))

(defun dash-docs--install-docset (url docset-name)
  "Download a docset from URL and install with name DOCSET-NAME."
  ;; set docset temporary path
  (let ((temp-file
         (format "%s%s-docset.tgz"
                 temporary-file-directory
                 docset-name))
        ;;  update (parse) generic url
        (url (url-generic-parse-url url)))
    ;; fetch and extract the temporary archive
    (dash-docs--fetch-extract-archive url temp-file)))

(defun dash-docs-installed-docsets ()
  "Return a list of installed docsets."
  ;; auxiliary variables
  (let (docsets docset)
    ;; get directories (docsets)
    (dolist (dir (directory-files dash-docs-docsets-dir nil "^[^.]"))
      ;; set docset formatted string
      (setq docset (replace-regexp-in-string "\\.docset\\'" "" dir))
      ;; if string was formatted add to docsets collection
      (when (not (equal docset dir))
        (push docset docsets)))
    ;; return docsets
    docsets))

(defun dash-docs-docset-installed-p (docset)
  "Return non-nil if DOCSET is installed."
  (member (replace-regexp-in-string "_" " " docset)
          (dash-docs-installed-docsets)))

(defun dash-docs-ensure-docset-installed (docset)
  "Install DOCSET if it is not currently installed."
  (unless (dash-docs-docset-installed-p docset)
    (dash-docs-install-docset docset)))

(defun dash-docs-parse-archive-url (xlm-file)
  "Parse the XML-FILE feed and return the first url."
  (let* ((xml (xml-parse-file xlm-file))
         (urls (car xml))
         (url (xml-get-children urls 'url)))
    ;; return first url
    (cl-caddr (cl-first url))))

(defun dash-docs-sub-docset-name-in-pattern (pattern docset-name)
  "Remove from PATTERN the DOCSET-NAME if this includes it.
If the search starts with the name of the docset, ignore it."
  (let ((regexp (format "^%s " (regexp-quote (downcase docset-name)))))
    ;; remove string in pattern
    (replace-regexp-in-string regexp "" pattern)))

(defun dash-docs-parse-url (docset filename &optional anchor)
  "Return absolute documentation FILE/URL.
Either a file:/// URL joining DOCSET, FILENAME & ANCHOR
or a http(s):// URL formed as-is if FILENAME is equal to
HTTP(S) URL."
  (let ((filename (format "%s%s" filename
                          (if anchor (format "#%s" anchor) "")))
        (regexps '("<dash_entry_.*>"
                   "//apple_ref/Function/"
                   "//apple_ref/func/"))
        (fileurl nil))
    ;; clean filename
    (dolist (regexp regexps)
      (setq filename (replace-regexp-in-string regexp
                                               ""
                                               filename)))
    ;; verify url type (file:// or http://)
    (if (string-match-p "^https?://" filename)
        filename
      ;; set file url: file:///URL
      (setq fileurl (concat "file:///"
                            (expand-file-name "Contents/Resources/Documents/"
                                              (dash-docs-docset-path docset))
                            filename))
      ;; return parsed query file url
      (prin1 (caar (url-parse-query-string fileurl))))))

(defun dash-docs-browse-url (candidate)
  "Call to `browse-url' after parse the chosen CANDIDATE."
  ;; parse chosen candidate
  (let ((docset (car candidate))
        (filename (nth 2 (cadr candidate)))
        (anchor (nth 3 (cadr candidate))))
    ;; open url (or file) using the chosen browser
    (funcall dash-docs-browser-func
             (dash-docs-parse-url docset filename anchor))))

(defun dash-docs--debug-buffer ()
  "Return the `dash-docs' debug buffer."
  (get-buffer-create dash-docs-debug-buffer))

(defun dash-docs-create-debug-buffer ()
  "Open debugging buffer and insert a header message."
  (with-current-buffer (dash-docs--debug-buffer)
    (erase-buffer)
    (insert ";; dash-docs error logging:\n\n")))

(defun dash-docs-docsets-choices ()
  "Return all available connected docsets entries."
  (dash-docs-search-docset-entries ""))

(defun dash-docs-minibuffer-read (prompt choices)
  "Read from the `minibuffer' using PROMPT and CHOICES as candidates.
Report an error unless a valid docset is selected."
  (let ((completion-ignore-case t))
    (completing-read (format "%s (%s): " prompt (car choices))
                     choices nil t nil nil choices)))

(defun dash-docs-del-common-docset (docset)
  "Delete DOCSET from `dash-docs-common-docsets'."
  ;; delete docset from common docsets
  (when (member docset dash-docs-common-docsets)
    (setq dash-docs-common-docsets
          (delete docset dash-docs-common-docsets))))

;;;###autoload
(defun dash-docs-clean-all-connections ()
  "Clean all connections interactively."
  (interactive)
  ;; clean connection
  (setq dash-docs--connections '())
  ;; clean activated docsets
  (setq dash-docs-common-docsets '()))

;;;###autoload
(defun dash-docs-install-unofficial-docset (docset-name)
  "Download an unofficial docset with specified DOCSET-NAME."
  ;; maps docset name parameter
  (interactive
   (list (dash-docs-minibuffer-read
          "Install docset"
          (mapcar 'car (dash-docs-unofficial-docsets)))))
  ;; install docset
  (let ((url (car (assoc-default docset-name
                                 (dash-docs-unofficial-docsets)))))
    (dash-docs--install-docset url docset-name)))

;;;###autoload
(defun dash-docs-install-docset-from-file (docset-temp-file)
  "Extract the content of DOCSET-TEMP-FILE.
Move it to `dash-docs-docsets-dir' and activate the docset."
  ;; maps docset temporary path parameter
  (interactive
   (list (car (find-file-read-args "Docset Archive: " t))))
  ;; extract the docset
  (let ((folder (dash-docs-extract-archive docset-temp-file)))
    ;; debug message
    (dash-docs--message "docset installed at %s" folder)))

;;;###autoload
(defun dash-docs-install-docset (docset-name)
  "Download an official docset with specified DOCSET-NAME.
Move its stuff to docsets-path."
  ;; maps docset name parameter
  (interactive
   (list (dash-docs-minibuffer-read
          "Install docset"
          (dash-docs-official-docsets))))
  ;; format url, download docset file (url-copy-file)
  ;; and move it to the right location (docset paths)
  (let ((feed-url (format "%s/%s.xml"
                          dash-docs-docsets-feed-url
                          docset-name))
        (feed-tmp-path (format "%s%s-feed.xml"
                               temporary-file-directory
                               docset-name))
        (url nil))
    ;; copy url file
    (url-copy-file feed-url feed-tmp-path t)
    ;; update url
    (setq url (dash-docs-parse-archive-url feed-tmp-path))
    ;; install docset
    (dash-docs--install-docset url docset-name)))

;;;###autoload
(defun dash-docs-activate-docset (docset)
  "Activate a DOCSET, i.e, make a connection to its database.
If called interactively prompts for the docset name."
  ;; maps docset parameter
  (interactive
   (list
    (dash-docs-minibuffer-read "Activate docset"
                               (dash-docs-installed-docsets))))
  ;; add docset to docsets list
  (push docset dash-docs-common-docsets)
  ;; start connection
  (dash-docs-add-connection docset))

;;;###autoload
(defun dash-docs-deactivate-docset (docset)
  "Deactivate DOCSET, i.e, update common docsets.
If called interactively prompts for the docset name."
  ;; maps docset parameter
  (interactive
   (list
    (dash-docs-minibuffer-read "Deactivate docset"
                               dash-docs-common-docsets)))
  ;; delete its connection
  (dash-docs-del-connection docset)
  ;; remove docset from common docsets
  (dash-docs-del-common-docset docset))

;;;###autoload
(defun dash-docs-find-file ()
  "Find dash documentation file."
  (interactive)
  ;; verify if common docsets were set
  (when (not (> (length dash-docs-common-docsets) 0))
    (call-interactively 'dash-docs-activate-docset))
  ;; map candidates
  (let* ((candidates (dash-docs-docsets-choices))
         (candidate (completing-read "Docs: " candidates nil t)))
    (if (equal candidate "")
        (dash-docs--message "error, please provide a search string"))
    ;; update the candidate
    (setq candidate (cdr (assoc candidate candidates)))
    ;; browse url a.k.a find file
    (dash-docs-browse-url candidate)))

;;;###autoload
(defun dash-docs-show-mode-state ()
  "Show dash-docs minor mode state: on/off."
  (interactive)
  ;; show mode state in echo area
  (message "[Dash-docs]: mode %s"
           (if dash-docs-mode "on" "off")))

;;;###autoload
(define-minor-mode dash-docs-mode
  "Define a new minor mode `dash-docs'.

This defines the toggle command `dash-docs' and (by default)
a control variable `dash-docs'.

Interactively with no prefix argument, it toggles the mode.
A prefix argument enables the mode if the argument is positive,
and disables it otherwise."

  :group dash-docs
  ;; :lighter dash-docs-minor-mode-string
  (cond
   (dash-docs-mode
    ;; create docsets default directory (if necessary)
    (dash-docs--created-dir)
    ;; setup docsets official index file (if necessary)
    (dash-docs-setup-index)
    ;; setup docsets unofficial (user) index file (if necessary)
    ;; (dash-docs-setup-user-index)
    ;; set dash docs mode indicator to true
    (setq dash-docs-mode t))
   (t
    ;; clean all connections
    (dash-docs-clean-all-connections)
    ;; clean internal variables
    (dash-docs--clean-internal-vars)
    ;; set mode indicator to false (nil)
    (setq dash-docs-mode nil))))

;;;###autoload
(defun turn-on-dash-docs-mode ()
  "Enable dash-docs minor mode."
  (interactive)
  ;; turn on dash-docs mode
  (dash-docs-mode 1)
  ;; show dash-docs mode state: on/off
  (dash-docs-show-mode-state))

;;;###autoload
(defun turn-off-dash-docs-mode ()
  "Disable dash-docs minor mode."
  (interactive)
  ;; turn off dash-docs mode
  (dash-docs-mode 0)
  ;; show dash-docs mode state
  (dash-docs-show-mode-state))

(provide 'dash-docs)

;;; dash-docs.el ends here
