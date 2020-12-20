;;; ddoc.el --- Offline documentation browser using Dash docsets.  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020  lambdart
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Version: Alpha 0.0.1
;; Keywords: dash, docs, documentation
;; URL: https://github.com/lambdart/ddoc-el
;;
;;; MIT License
;;
;; Copyright (c) 2020 lambdart
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; This library provides ways to interact with Dash docs, i.e,
;; install docsets archives from the internet, search entries in the
;; local docsets database, provide candidates for the *Completions*
;; `minibuffer' and open documentation (usually html files) using
;; the chosen browser (eww, firefox and others).
;;
;;; Code:

(require 'xml)
(require 'url)
(require 'url-http)
(require 'mm-decode)
(require 'json)

(eval-when-compile
  (require 'cl-macs))

(defgroup ddoc nil
  "Dash documentation sets management."
  :prefix "ddoc-"
  :group 'applications)

(defcustom ddoc-docsets-dir
  (expand-file-name "docsets" user-emacs-directory)
  "Default docsets directory.
If you're setting this option manually, set it to an absolute
path. You can use `expand-file-name' function for that."
  :set (lambda (opt val) (set opt (expand-file-name val)))
  :type 'string
  :group 'ddoc)

(defcustom ddoc-unofficial-url
  "https://dashes-to-dashes.herokuapp.com/docsets/contrib"
  "Unofficial (user) docsets URL."
  :type 'string
  :group 'ddoc)

(defcustom ddoc-min-length 3
  "Minimum length to start searching in docsets.
0 facilitates discoverability, but may be a bit heavy when lots
of docsets are active.  Between 0 and 3 is sane."
  :type 'integer
  :group 'ddoc)

(defcustom ddoc-retrieve-url-timeout 5
  "It should be a number that says (in seconds)
how long to wait for a response before giving up."
  :type 'integer
  :group 'ddoc)

(defcustom ddoc-candidate-format "%d %n (%t)"
  "Format of the displayed candidates.
Available formats are
   %d - docset name
   %n - name of the token
   %t - type of the token
   %f - file name"
  :type 'string
  :group 'ddoc)

(defcustom ddoc-debug-buffer "ddoc-errors"
  "Debugging buffer name."
  :type 'string
  :group 'ddoc)

(defcustom ddoc-sql-debug-flag nil
  "Non-nil display sql query stderr in a buffer.
Setting this to nil may speed up sql query operations."
  :type 'boolean
  :group 'ddoc)

(defcustom ddoc-browser-func 'browse-url
  "Default function to browse Dash's docsets.
Suggested values are:
 * `browse-url'
 * `eww'"
  :type 'function
  :group 'ddoc)

(defcustom ddoc-ignored-docsets '("Man_Pages")
  "Return a list of ignored docsets.
These docsets are not available to install."
  :type 'list
  :group 'ddoc)

(defcustom ddoc-index-file
  (expand-file-name "cache/ddoc.index" user-emacs-directory)
  "Official (cache) index file."
  :type 'string
  :group 'ddoc)

(defcustom ddoc-user-index-file
  (expand-file-name "cache/ddoc-user.index" user-emacs-directory)
  "Unofficial (user cache) index file."
  :type 'string
  :group 'ddoc)

(defvar ddoc-docsets-url
  "https://api.github.com/repos/Kapeli/feeds/contents"
  "Official docsets URL.")

(defvar ddoc-docsets-feed-url
  "https://raw.github.com/Kapeli/feeds/master"
  "Feeds URL for dash docsets.")

(defvar ddoc-dash-sql-query
  "SELECT t.type, t.name, t.path FROM searchIndex t WHERE %s ORDER BY LENGTH(t.name), LOWER(t.name) LIMIT 1000"
  "DASH default sql query.")

(defvar ddoc-zdash-sql-query
  "SELECT ty.ZTYPENAME, t.ZTOKENNAME, f.ZPATH, m.ZANCHOR FROM ZTOKEN t, ZTOKENTYPE ty, ZFILEPATH f, ZTOKENMETAINFORMATION m WHERE ty.Z_PK = t.ZTOKENTYPE AND f.Z_PK = m.ZFILE AND m.ZTOKEN = t.Z_PK AND %s ORDER BY LENGTH(t.ZTOKENNAME), LOWER(t.ZTOKENNAME) LIMIT 1000"
  "ZDASH default sql query.")

(defvar ddoc-type-sql-query
  "SELECT name FROM sqlite_master WHERE type = 'table' LIMIT 1"
  "Default type sqlite3 query.")

(defvar ddoc-common-docsets '()
  "List of Docsets to search active by default.")

(defvar ddoc--connections nil
  "List of conses like (\"Go\" . connection).")

(defvar ddoc--internal-vars
  '(ddoc--connections
    ddoc-common-docsets)
  "List of ddoc internal variables.")

(defvar ddoc-message-prefix "[Dash-docs]: "
  "Internal ddoc message prefix.")

(defvar ddoc-mode nil
  "Non-nil means that `ddoc-mode' is enabled.
Note: set this variable directly has no effect, use
`turn-on-dash-doc-mode' stead.")

(defvar ddoc-json-cache-file
  (expand-file-name "cache/ddoc-official.json"
                    user-emacs-directory)
  "Official json cache file.")

(defvar ddoc-json-user-cache-file
  (expand-file-name "cache/ddoc-unofficial.json"
                    user-emacs-directory)
  "Unofficial json cache file.")

(defmacro ddoc--message (fmt &rest args)
  "Just an internal `message' helper."
  `(message (concat ddoc-message-prefix ,fmt) ,@args))

(defvar ddoc--sql-queries
  ;; DASH lambda function
  '((DASH . (lambda (pattern)
              (let ((like (ddoc-sql-compose-like "t.name" pattern))
                    (query ddoc-dash-sql-query))
                (format query like))))
    ;; ZDASH lambda function
    (ZDASH . (lambda (pattern)
               (let ((like (ddoc-sql-compose-like "t.ZTOKENNAME" pattern))
                     (query ddoc-zdash-sql-query))
                 (format query like)))))
  "SQL queries associative (NAME LAMBDA) list.")

(defun ddoc--clean-internal-vars ()
  "Clean internal lists."
  ;; clean internal variables
  (dolist (var ddoc--internal-vars)
    (set var nil)))

(defun ddoc--created-dir ()
  "Create docsets default directory: `ddoc-docsets-dir'."
  (let* ((dir ddoc-docsets-dir)
         (prompt (format "Directory %s does not exist. Want to create it?"
                         dir)))
    (or (file-directory-p dir)
        (and (y-or-n-p prompt))
        (mkdir dir t))))

(defun ddoc-docset-path (docset-name)
  "Return DOCSET-NAME full path."
  (let* (;; set directory
         (docset-dir (expand-file-name ddoc-docsets-dir))
         ;; format docset path
         (docset-path (format "%s/%s.docset" docset-dir docset-name)))
    ;; verify if the path exists
    (if (file-directory-p docset-path) docset-path nil)))

(defun ddoc-docset-db-path (docset-name)
  "Return database DOCSET path."
  (let ((docset-path (ddoc-docset-path docset-name)))
    ;; verify docset path
    (if docset-path
        (expand-file-name "Contents/Resources/docSet.dsidx" docset-path)
      ;; debug message
      (ddoc--message "missing docset '%s'" docset-name)
      ;; return nil
      nil)))

(defun ddoc-parse-sql-results (sql-result-string)
  "Parse SQL-RESULT-STRING splitting it by newline and '|' chars."
  (mapcar (lambda (string) (split-string string "|" t))
          (split-string sql-result-string "\n" t)))

(defun ddoc-exec-query (db-path query)
  "Execute QUERY in the db located at DB-PATH and parse the results.
If there are errors, print them in `ddoc--debug-buffer'."
  (ddoc-parse-sql-results
   (with-output-to-string
     (let ((error-file (when ddoc-sql-debug-flag
                         (make-temp-file "ddoc-errors-file"))))
       (call-process "sqlite3" nil (list standard-output error-file) nil
                     ;; args for sqlite3:
                     "-list" "-init" "''" db-path query)
       ;; display errors, stolen from emacs' `shell-command` function
       (when (and error-file (file-exists-p error-file))
         (if (< 0 (nth 7 (file-attributes error-file)))
             (with-current-buffer (ddoc--debug-buffer)
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

(defun ddoc-docset-type (db-path)
  "Return the type of the docset based in db schema.
Possible values are \"DASH\" and \"ZDASH\".
The Argument DB-PATH should be a string with the sqlite db path."
  (let ((query ddoc-type-sql-query))
    (if (member "searchIndex"
                (car (ddoc-exec-query db-path query)))
        "DASH"
      "ZDASH")))

(defun ddoc-sql-compose-like (column pattern)
  "Return a query fragment for a sql where clause.
Search in column COLUMN by multiple terms splitting the PATTERN
by whitespace and using like sql operator."
  (let ((conditions (mapcar
                     (lambda (word)
                       (format "%s like '%%%s%%'" column word))
                     (split-string pattern " "))))
    (format "%s" (mapconcat 'identity conditions " AND "))))

(defun ddoc-compose-sql-query (docset-type pattern)
  "Return a SQL query to search documentation in dash docsets.
A different query is returned depending on DOCSET-TYPE.
PATTERN is used to compose the SQL WHERE clause."
  (let ((compose-select-query-func
         (cdr (assoc (intern docset-type)
                     ddoc--sql-queries))))
    (when compose-select-query-func
      (funcall compose-select-query-func pattern))))

(defun ddoc-sql-search (docset pattern)
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
         (query (ddoc-compose-sql-query docset-type pattern)))
    ;; execute the query
    (ddoc-exec-query db-path query)))

(defun ddoc--format-row (connection row)
  "Format candidate (ROW) using its CONNECTION."
  (cons (format-spec ddoc-candidate-format
                     (list (cons ?d (cl-first connection))
                           (cons ?n (cl-second row))
                           (cons ?t (cl-first row))
                           (cons ?f (replace-regexp-in-string
                                     "^.*/\\([^/]*\\)\\.html?#?.*"
                                     "\\1"
                                     (cl-third row)))))
        (list (car connection) row)))

(defun ddoc-search-docset-entry (connection pattern)
  "Search PATTERN in CONNECTION, return a list of formatted rows."
  (cl-loop for row in (ddoc-sql-search connection pattern)
           collect (ddoc--format-row connection row)))

(defun ddoc-search-docset-entries (pattern)
  "Search a PATTERN in all available connected docsets."
  (cl-loop for connection in (ddoc-search-connections pattern)
           appending (ddoc-search-docset-entry connection pattern)))

(defun ddoc-buffer-local-docsets ()
  "Get the docsets configured for the current buffer."
  (or (and (boundp 'ddoc-docsets) ddoc-docsets) '()))

(defun ddoc-add-connection (docset)
  "Add DOCSET connection to `ddoc--connections'."
  ;; verify if docset is already present
  (when (not (assoc docset ddoc--connections))
    ;; connection parameters
    (let ((db-path (ddoc-docset-db-path docset)))
      (when db-path
        (let* ((type (ddoc-docset-type db-path))
               (connection (list docset db-path type)))
          ;; add connection to ddoc--connections
          (push connection ddoc--connections))))))

(defun ddoc-add-buffer-local-connections ()
  "Add ddoc buffer local connections."
  (dolist (docset (ddoc-buffer-local-docsets))
    (ddoc-add-connection docset)))

(defun ddoc-connections ()
  "Return available connections."
  (let ((docsets (ddoc-buffer-local-docsets)))
    ;; append local docsets with common ones
    (setq docsets (append docsets ddoc-common-docsets))
    ;; get unique 'connections' associated with the docsets
    (delq nil
          (mapcar (lambda (docset)
                    (assoc docset ddoc--connections))
                  docsets))))

(defun ddoc-search-connections (pattern)
  "Search PATTERN in the available connections.
If PATTERN starts with the name of a docset,
narrow the used connections to just that one."
  (let ((connections (ddoc-connections)))
    ;; if no pattern just return all available connections
    (if (equal pattern "") connections
      ;; return connection filter by pattern: docset name
      (cl-loop for connection in connections
               if (string-prefix-p (car connection)
                                   pattern t)
               return (list connection)))))

(defun ddoc-del-connection (docset)
  "Remove DOCSET connection from `ddoc--connections'."
  (let*  ((connections (ddoc-connections))
          (connection (assoc docset connections)))
    (when (member connection connections)
      (setq ddoc--connections
            (delq connection connections)))))

(defun ddoc--write-file (contents file)
  "Write CONTENTS in the target FILE."
  (with-temp-file file
    ;; insert file contents in the file buffer (implicit)
    (prin1 contents (current-buffer)))
  ;; return nothing
  nil)

(defun ddoc--read-file (file)
  "Return FILE contents."
  (when (file-exists-p file)
    (let ((contents (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-substring-no-properties (point-min)
                                                      (point-max)))))
      (read contents))))

(defun ddoc-fetch-json (url file)
  "Copy JSON contents to FILE from the target URL."
  (let ((json-contents nil))
    ;; copy url json to cache file
    (url-copy-file url file t nil)
    ;; switch/case equivalent
    (cond
     ;; verify if file exists
     ((not (file-exists-p file))
      ;; debug message
      (ddoc--message "was not possible to retrieve json file")
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

(defun ddoc--parse-contrib-index (json)
  "Parse index JSON elements."
  (delq nil
        (mapcar (lambda (element)
                  (let* ((name (assoc-default 'name element))
                         (archive (assoc-default 'archive element)))
                    (list name archive)))
                json)))

(defun ddoc--parse-index (json)
  "Parse index JSON elements."
  (delq nil
        (mapcar (lambda (element)
                  (let* ((name (assoc-default 'name element))
                         (ext (file-name-extension name)))
                    (when (equal ext "xml")
                      (list name))))
                json)))

(defun ddoc-setup-index ()
  "Setup the official docset's index."
  ;; verify if file already exists
  (when (not (file-exists-p ddoc-json-cache-file))
    ;; set json file and parse the index
    (let* ((json (ddoc-fetch-json ddoc-docsets-url
                                       ddoc-json-cache-file))
           (index (ddoc--parse-index json)))
      ;; write the file unless we don't have any parsed index
      (unless (not index)
        ;; write json to the index file
        (ddoc--write-file index ddoc-index-file)))))

(defun ddoc-setup-unofficial-index ()
  "Setup unofficial contrib index."
  ;; verify if file already exists
  (when (not (file-exists-p ddoc-json-user-cache-file))
    ;; set json file and parse the index
    (let* ((json (ddoc-fetch-json ddoc-unofficial-url
                                       ddoc-json-user-cache-file))
           (index (ddoc--parse-contrib-index json)))
      ;; write json to the index file
      (ddoc--write-file index ddoc-user-index-file))))

(defun ddoc-unofficial-docsets ()
  "Return a list of lists with docsets contributed by users.
The first element is the docset's name second the docset's archive url."
  (let ((docsets (ddoc--read-file ddoc-user-index-file)))
    ;; parse docsets list
    (mapcar (lambda (docset)
              (car docset))
            docsets)))

(defun ddoc-official-docsets ()
  "Return a list of official docsets."
  (let ((index (ddoc--read-file ddoc-index-file))
        (docsets nil))
    ;; for each docset in index clean the ".xml" substring
    (dolist (docset index)
      (push (replace-regexp-in-string ".xml" "" (car docset)) docsets))
    ;; return docsets
    docsets))

(defun ddoc-extract-archive (docset-temp-file)
  "Extract DOCSET-TEMP-FILE to `ddoc-docsets-dir'.
Return the folder that was newly extracted."
  (with-temp-buffer
    (let* ((program (list "tar" nil t nil))
           (args (list "xfv" docset-temp-file "-C" ddoc-docsets-dir))
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
               ddoc-docsets-dir))
       ;; verify call-process result
       ((not (equal result 0))
        ;; signals an error message
        (error "Error %s, failed to extract %s to %s."
               result
               docset-temp-file
               ddoc-docsets-dir)))
      ;; got the point
      (goto-char (point-max))
      ;; set path string
      (setq folder (car (split-string (thing-at-point 'line) "\\." t)))
      ;; format the folder and return it (string)
      (replace-regexp-in-string "^x " "" folder))))

(defun ddoc--fetch-extract-archive (url archive)
  "Fetch URL and extract the retrieved docset TEMP-FILE archive."
  ;; http request (async version of url-copy-file)
  (url-http url
            (lambda (&rest args)
              (princ args)
              ;; download the file
              (let* ((file (cadr args))
                     (buffer (current-buffer))
                     (handle (with-current-buffer buffer
                               (mm-dissect-buffer t))))
                ;; return the default file protection for created files
                (let ((mm-attachment-file-modes (default-file-modes)))
                  (mm-save-part-to-file handle file))
                ;; necessary?
                (kill-buffer buffer)
                ;; destroy MIME parts
                (mm-destroy-parts handle)
                ;; extract docset
                (ddoc-extract-archive file)))
            ;; temporary archive file
            `(nil ,archive)))

(defun ddoc--install-docset (url docset-name)
  "Download a docset from URL and install with name DOCSET-NAME."
  ;; set docset temporary archive
  (let ((archive
         (format "%s%s.tgz"
                 temporary-file-directory
                 docset-name))
        ;;  update (parse) generic url
        (url (url-generic-parse-url url)))
    ;; fetch and extract the temporary archive
    (ddoc--fetch-extract-archive url archive)))

(defun ddoc-installed-docsets ()
  "Return a list of installed docsets."
  ;; auxiliary variables
  (let (docsets docset)
    ;; get directories (docsets)
    (dolist (dir (directory-files ddoc-docsets-dir nil "^[^.]"))
      ;; set docset formatted string
      (setq docset (replace-regexp-in-string "\\.docset\\'" "" dir))
      ;; if string was formatted add to docsets collection
      (when (not (equal docset dir))
        (push docset docsets)))
    ;; return docsets
    docsets))

(defun ddoc-docset-installed-p (docset)
  "Return non-nil if DOCSET is installed."
  (member (replace-regexp-in-string "_" " " docset)
          (ddoc-installed-docsets)))

(defun ddoc-ensure-docset-installed (docset)
  "Install DOCSET if it is not currently installed."
  (unless (ddoc-docset-installed-p docset)
    (ddoc-install-docset docset)))

(defun ddoc-parse-archive-url (xlm-file)
  "Parse the XML-FILE feed and return the first url."
  (let* ((xml (xml-parse-file xlm-file))
         (urls (car xml))
         (url (xml-get-children urls 'url)))
    ;; return first url
    (cl-caddr (cl-first url))))

(defun ddoc-sub-docset-name-in-pattern (pattern docset-name)
  "Remove from PATTERN the DOCSET-NAME if this includes it.
If the search starts with the name of the docset, ignore it."
  (let ((regexp (format "^%s " (regexp-quote (downcase docset-name)))))
    ;; remove string in pattern
    (replace-regexp-in-string regexp "" pattern)))

(defun ddoc-compose-url (name filename &optional anchor)
  "Compose the final URL.
Either a file:/// URL joining docset NAME, FILENAME & ANCHOR
or a http(s):// URL formed as-is if FILENAME is equal to HTTP(S)."
  (let* ((anchor (if anchor (format "#%s" anchor) ""))
         (filename (format "%s%s" filename anchor)))
    ;; verify url type
    (if (string-match-p "^https?://" filename)
        filename
      ;; return file:///URL
      (concat "file:///"
              (expand-file-name "Contents/Resources/Documents/"
                                (ddoc-docset-path name))
              filename))))

(defun ddoc-split-docset (docset)
  "Split DOCSET elements, return (NAME FILENAME ANCHOR)."
  (let ((name (car docset))
        (filename (nth 2 (cadr docset)))
        (anchor (nth 3 (cadr docset)))
        ;; auxiliary
        (strings nil))
    ;; verify anchor
    (unless anchor
      (setq strings (split-string filename "#")
            filename (car strings)
            anchor (cadr strings)))
    ;; clean filename
    (setq filename (replace-regexp-in-string "<dash_entry_.*>"
                                             ""
                                             filename))
    ;; return the elements list
    (list name filename anchor)))

(defun ddoc-browse-url (docset)
  "Call to `browse-url' after parse the chosen DOCSET."
  ;; split elements and compose final url
  (let* ((elts (ddoc-split-docset docset))
         (url (apply 'ddoc-compose-url elts)))
    (prin1 elts)
    ;; finally invoke browser function (implicit: open the file)
    ;; in the chosen browser
    (funcall ddoc-browser-func url)))

(defun ddoc--debug-buffer ()
  "Return the `ddoc' debug buffer."
  (get-buffer-create ddoc-debug-buffer))

(defun ddoc-create-debug-buffer ()
  "Open debugging buffer and insert a header message."
  (with-current-buffer (ddoc--debug-buffer)
    (erase-buffer)
    (insert ";; ddoc error logging:\n\n")))

(defun ddoc-docsets-collection ()
  "Return all available connected docsets entries."
  (ddoc-search-docset-entries ""))

(defun ddoc-minibuffer-read (prompt choices)
  "Read from the `minibuffer' using PROMPT and CHOICES as candidates.
Report an error unless a valid docset is selected."
  (let ((completion-ignore-case t))
    (completing-read (format "%s (%s): " prompt (car choices))
                     choices nil t nil nil choices)))

(defun ddoc-del-common-docset (docset)
  "Delete DOCSET from `ddoc-common-docsets'."
  ;; delete docset from common docsets
  (when (member docset ddoc-common-docsets)
    (setq ddoc-common-docsets
          (delete docset ddoc-common-docsets))))

(defun ddoc-clean-all-connections ()
  "Clean all connections interactively."
  (interactive)
  ;; clean connection
  (setq ddoc--connections '())
  ;; clean activated docsets
  (setq ddoc-common-docsets '()))

(defun ddoc-install-docset-from-file (docset-temp-file)
  "Extract the content of DOCSET-TEMP-FILE.
Move it to `ddoc-docsets-dir' and
activate the docset."
  ;; maps docset temporary path parameter
  (interactive
   (list (car (find-file-read-args "Docset Archive: " t))))
  ;; extract the docset
  (let ((folder (ddoc-extract-archive docset-temp-file)))
    ;; debug message
    (ddoc--message "docset installed at %s" folder)))

(defun ddoc-install-unofficial-docset (docset-name)
  "Download an unofficial docset with specified DOCSET-NAME."
  ;; maps docset name parameter
  (interactive
   (list (ddoc-minibuffer-read "Install docset"
                                    (ddoc-unofficial-docsets))))
  ;; set url
  (let ((url (car (assoc-default docset-name
                                 (ddoc--read-file
                                  ddoc-user-index-file)))))
    ;; install docset (fetch and extract) asynchronous
    (ddoc--install-docset url docset-name)))

(defun ddoc-install-docset (docset-name)
  "Download an official docset with specified DOCSET-NAME.
Move its stuff to docsets-path."
  ;; maps docset name parameter
  (interactive
   (list (ddoc-minibuffer-read
          "Install docset"
          (ddoc-official-docsets))))
  ;; format url, download docset file (url-copy-file)
  ;; and move it to the right location (docset paths)
  (let ((feed-url (format "%s/%s.xml"
                          ddoc-docsets-feed-url
                          docset-name))
        (feed-tmp-path (format "%s%s-feed.xml"
                               temporary-file-directory
                               docset-name))
        (url nil))
    ;; copy url file
    (url-copy-file feed-url feed-tmp-path t)
    ;; update url
    (setq url (ddoc-parse-archive-url feed-tmp-path))
    ;; install docset
    (ddoc--install-docset url docset-name)))

(defun ddoc-activate-docset (docset)
  "Activate a DOCSET, i.e, make a connection to its database.
If called interactively prompts for the docset name."
  ;; maps docset parameter
  (interactive
   (list
    (ddoc-minibuffer-read "Activate docset"
                               (ddoc-installed-docsets))))
  ;; add docset to docsets list
  (push docset ddoc-common-docsets)
  ;; start connection
  (ddoc-add-connection docset))

(defun ddoc-deactivate-docset (docset)
  "Deactivate DOCSET, i.e, update common docsets.
If called interactively prompts for the docset name."
  ;; maps docset parameter
  (interactive
   (list
    (ddoc-minibuffer-read "Deactivate docset"
                               ddoc-common-docsets)))
  ;; delete its connection
  (ddoc-del-connection docset)
  ;; remove docset from common docsets
  (ddoc-del-common-docset docset))

(defun ddoc-find-file ()
  "Find dash documentation file."
  (interactive)
  ;; verify if common docsets were set
  (when (not (> (length ddoc-common-docsets) 0))
    (call-interactively 'ddoc-activate-docset))
  ;; map collection
  (let* ((collection (ddoc-docsets-collection))
         (docset (completing-read "Docset: " collection nil t)))
    (if (equal docset "")
        (ddoc--message "error, please provide a search string"))
    ;; update the docset
    (setq docset (cdr (assoc docset collection)))
    ;; browse url a.k.a find file
    (ddoc-browse-url docset)))

(defun ddoc-echo-mode-state ()
  "Show ddoc minor mode state: on/off."
  (interactive)
  ;; show mode state in echo area
  (message "[Dash-docs]: mode %s"
           (if ddoc-mode "on" "off")))

;;;###autoload
(define-minor-mode ddoc-mode
  "Define a new minor mode `ddoc'.

This defines the toggle command `ddoc' and (by default)
a control variable `ddoc'.

Interactively with no prefix argument, it toggles the mode.
A prefix argument enables the mode if the argument is positive,
and disables it otherwise."

  :group ddoc
  ;; :lighter ddoc-minor-mode-string
  (cond
   (ddoc-mode
    ;; create docsets default directory (if necessary)
    (ddoc--created-dir)
    ;; setup docsets official index file (if necessary)
    (ddoc-setup-index)
    ;; setup docsets unofficial (user) index file (if necessary)
    (ddoc-setup-unofficial-index)
    ;; set dash docs mode indicator to true
    (setq ddoc-mode t))
   (t
    ;; clean all connections
    (ddoc-clean-all-connections)
    ;; clean internal variables
    (ddoc--clean-internal-vars)
    ;; set mode indicator to false (nil)
    (setq ddoc-mode nil))))

;;;###autoload
(defun turn-on-ddoc-mode ()
  "Enable ddoc minor mode."
  (interactive)
  ;; turn on ddoc mode
  (ddoc-mode 1)
  ;; show ddoc mode state: on/off
  (ddoc-echo-mode-state))

(defun turn-off-ddoc-mode ()
  "Disable ddoc minor mode."
  (interactive)
  ;; turn off ddoc mode
  (ddoc-mode 0)
  ;; show ddoc mode state
  (ddoc-echo-mode-state))

(provide 'ddoc)

;;; ddoc.el ends here
