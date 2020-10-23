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

(require 'url)
(require 'xml)
(require 'json)
(require 'cl-lib)
(require 'thingatpt)
(require 'format-spec)

(eval-when-compile
  (require 'cl-macs))

(defgroup dash-docs nil
  "Search Dash docsets."
  :prefix "dash-docs-"
  :group 'applications)

(defcustom dash-docs-docsets-path
  (expand-file-name "docsets" user-emacs-directory)
  "Default path for docsets.
If you're setting this option manually, set it to an absolute
path. You can use `expand-file-name' function for that."
  :set (lambda (opt val) (set opt (expand-file-name val)))
  :type 'string
  :group 'dash-docs)

(defcustom dash-docs-docsets-feed-url
  "https://raw.github.com/Kapeli/feeds/master"
  "Feeds URL for dash docsets."
  :type 'string
  :group 'dash-docs)

(defcustom dash-docs-docsets-url
  "https://api.github.com/repos/Kapeli/feeds/contents"
  "Official docsets URL."
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

(defcustom dash-docs-json-cache-file
  (expand-file-name "cache/kapeli.json" user-emacs-directory)
  "Kapeli json cache file."
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

(defvar dash-docs-dash-sql-query
  "SELECT t.type, t.name, t.path FROM searchIndex t WHERE %s ORDER BY LENGTH(t.name), LOWER(t.name) LIMIT 1000"
  "DASH default sql query.")

(defvar dash-docs-zdash-sql-query
  "SELECT ty.ZTYPENAME, t.ZTOKENNAME, f.ZPATH, m.ZANCHOR FROM ZTOKEN t, ZTOKENTYPE ty, ZFILEPATH f, ZTOKENMETAINFORMATION m WHERE ty.Z_PK = t.ZTOKENTYPE AND f.Z_PK = m.ZFILE AND m.ZTOKEN = t.Z_PK AND %s ORDER BY LENGTH(t.ZTOKENNAME), LOWER(t.ZTOKENNAME) LIMIT 1000"
  "ZDASH default sql query.")

(defvar dash-docs-type-sql-query
  "SELECT name FROM sqlite_master WHERE type = 'table' LIMIT 1"
  "Default type sqlite3 query.")

(defvar dash-docs--common-docsets '()
  "List of Docsets to search active by default.")

(defvar dash-docs--connections nil
  "List of conses like (\"Go\" . connection).")

(defvar dash-docs--internal-vars
  '(dash-docs--connections
    dash-docs--common-docsets)
  "List of dash-docs internal variables.")

(defvar dash-docs-message-prefix "[dash-docs]: "
  "Internal dash-docs message prefix.")

(defvar dash-docs-mode nil
  "Non-nil means that `dash-docs-mode' is enabled.
Note: set this variable directly has no effect, use
`turn-on-dash-doc-mode' stead.")

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

(defun dash-docs--created-docsets-path (docset-path)
  "Check if DOCSET-PATH directory exists."
  (or (file-directory-p docset-path)
      (and (y-or-n-p
            (format "Directory %s does not exist. Want to create it?"
                    docset-path))
           (mkdir docset-path t))))

(defun dash-docs-docsets-path ()
  "Return the path where Dash's docsets are stored."
  (expand-file-name dash-docs-docsets-path))

(defun dash-docs-docset-path (docset)
  "Return DOCSET directory path (full)."
  (let* ((base (dash-docs-docsets-path))
         (docdir (expand-file-name docset base)))
    (cl-loop for dir in (list (format "%s/%s.docset" base docset)
                              (format "%s/%s.docset" docdir docset)
                              (when (file-directory-p docdir)
                                (cl-first (directory-files
                                           docdir t "\\.docset\\'"))))
             when (and dir (file-directory-p dir))
             return dir)))

(defun dash-docs-docset-db-path (docset)
  "Return database DOCSET path."
  (let ((path (dash-docs-docset-path docset)))
    ;; if not found, error
    (if (not path)
        (error "Cannot find docset '%s'" docset)
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
         (cdr (assoc (intern docset-type) dash-docs--sql-queries))))
    (when compose-select-query-func
      (funcall compose-select-query-func pattern))))

(defun dash-docs-buffer-local-docsets ()
  "Get the docsets configured for the current buffer."
  (or (and (boundp 'dash-docs-docsets) dash-docs-docsets) '()))

(defun dash-docs-get-connections ()
  "Return available connections."
  (let ((docsets (dash-docs-buffer-local-docsets)))
    ;; append local docsets with common ones
    (setq docsets (append docsets dash-docs--common-docsets))
    ;; get unique 'connections' associated with the docsets
    (delq nil
          (mapcar (lambda (docset)
                    (assoc docset dash-docs--connections))
                  docsets))))

(defun dash-docs-add-connection (docset)
  "Add DOCSET to `dash-docs--connections'."
  ;; verify if docset is already present
  (when (not (assoc docset dash-docs--connections))
    ;; connection parameters
    (let* ((db-path (dash-docs-docset-db-path docset))
           (type (dash-docs-docset-type db-path))
           (connection (list docset db-path type)))
      ;; add connection to dash-docs--connections
      (push connection dash-docs--connections))))

;;;###autoload
(defun dash-docs-initialize-connections ()
  "Initialize `dash-docs--connections'."
  (interactive)
  (dolist (docset dash-docs--common-docsets)
    (dash-docs-add-connection docset)))

(defun dash-docs-initialize-buffer-connections ()
  "Create connections to sqlite docsets for buffer-local docsets."
  (dolist (docset (dash-docs-buffer-local-docsets))
    (dash-docs-add-connection docset)))

(defun dash-docs-connections (pattern)
  "Return a list of dash-docs connections.

If PATTERN starts with the name of a docset followed by a space, narrow the
used connections to just that one.

We're looping on all connections, but it shouldn't
be a problem as there won't be many."

  (let ((connections (dash-docs-get-connections)))
    (or (cl-loop for conn in connections
                 if (string-prefix-p
                     (concat (downcase (car conn)) " ")
                     (downcase pattern))
                 return (list conn))
        connections)))

(defun dash-docs-write-json-file (json-contents)
  "Write JSON-CONTENTS in the `dash-docs-kapeli-cache-file'."
  ;; set file path
  (let ((file-path dash-docs-json-cache-file))
    ;; write buffer to file
    (with-temp-file file-path
      ;; insert json contents in the current buffer
      (prin1 json-contents (current-buffer)))))

(defun dash-docs-read-json-from-url (url)
  "Read and return a JSON object from URL."
  (let ((buffer (url-retrieve-synchronously
                 url t t
                 dash-docs-retrieve-url-timeout))
        (json-content nil))
    (cond
     ;; verify if buffer is non-nil
     ((not buffer) (error "Was not possible to retrieve url."))
     ;; default, read json contents
     (t
      (setq json-content
            (with-current-buffer buffer
              (json-read)))))
    ;; return json content
    json-content))

(defun dash-docs-unofficial-docsets ()
  "Return a list of lists with docsets contributed by users.
The first element is the docset's name second the docset's archive url."
  (let ((docsets (dash-docs-read-json-from-url dash-docs-unofficial-url)))
    (mapcar (lambda (docset)
              (list (assoc-default 'name docset)
                    (assoc-default 'archive docset)))
            docsets)))

(defun dash-docs-official-docsets ()
  "Return a list of official docsets."
  (let ((docsets (dash-docs-read-json-from-url dash-docs-docsets-url)))
    (delq nil (mapcar
               (lambda (docset)
                 (let ((name (assoc-default 'name docset)))
                   (if (and (equal (file-name-extension name) "xml")
                            (not (member (file-name-sans-extension name)
                                         dash-docs-ignored-docsets)))
                       (file-name-sans-extension name))))
               docsets))))

(defun dash-docs--install-docset (url docset-name)
  "Download a docset from URL and install with name DOCSET-NAME."
  ;; set docset temporary path
  (let ((docset-tmp-path
         (format "%s%s-docset.tgz"
                 temporary-file-directory
                 docset-name)))
    ;; copy file to the right location
    (url-copy-file url docset-tmp-path t)
    ;; install docset from file
    (dash-docs-install-docset-from-file docset-tmp-path)))

(defun dash-docs-installed-docsets ()
  "Return a list of installed docsets."
  (let ((docset-path (dash-docs-docsets-path)))
    (cl-loop for dir in (directory-files docset-path nil "^[^.]")
             for full-path = (expand-file-name dir docset-path)
             for subdir = (and (file-directory-p full-path)
                               (cl-first (directory-files full-path
                                                          t "\\.docset\\'")))
             when (or (string-match-p "\\.docset\\'" dir)
                      (file-directory-p (expand-file-name
                                         (format "%s.docset" dir) full-path))
                      (and subdir (file-directory-p subdir)))
             collecting (replace-regexp-in-string "\\.docset\\'" "" dir))))

(defun dash-docs-extract-and-get-folder (docset-temp-path)
  "Extract DOCSET-TEMP-PATH to DASH-DOCS-DOCSETS-PATH,
and return the folder that was newly extracted."
  (with-temp-buffer
    (let* ((call-process-args (list "tar" nil t nil))
           (process-args (list
                          "xfv" docset-temp-path
                          "-C" (dash-docs-docsets-path)))
           (result (apply #'call-process
                          (append call-process-args process-args nil)))
           (path-string nil))
      (goto-char (point-max))
      (cond
       ;; too long?
       ((and (not (equal result 0))
             ;; TODO: Adjust to proper text. Also requires correct locale.
             (search-backward "too long" nil t))
        (error "Failed extract %s to %s."
               docset-temp-path (dash-docs-docsets-path)))
       ;; verify call process error
       ((not (equal result 0))
        (error "Error %s, failed to extract %s to %s."
               result docset-temp-path (dash-docs-docsets-path))))
      (goto-char (point-max))
      ;; set path string
      (setq path-string (car (split-string (thing-at-point 'line) "\\." t)))
      ;; format and return the folder
      (replace-regexp-in-string "^x " "" path-string))))

(defun dash-docs-docset-installed-p (docset)
  "Return non-nil if DOCSET is installed."
  (member (replace-regexp-in-string "_" " " docset)
          (dash-docs-installed-docsets)))

(defun dash-docs-ensure-docset-installed (docset)
  "Install DOCSET if it is not currently installed."
  (unless (dash-docs-docset-installed-p docset)
    (dash-docs-install-docset docset)))

(defun dash-docs-get-docset-url (feed-path)
  "Parse a xml feed with docset urls and return the first url.
The Argument FEED-PATH should be a string with the path of the xml file."
  (let* ((xml (xml-parse-file feed-path))
         (urls (car xml))
         (url (xml-get-children urls 'url)))
    (cl-caddr (cl-first url))))

(defun dash-docs-sub-docset-name-in-pattern (pattern docset-name)
  "Remove from PATTERN the DOCSET-NAME if this includes it.
If the search starts with the name of the docset, ignore it."
  (let ((regexp (format "^%s " (regexp-quote (downcase docset-name)))))
    ;; remove string in pattern
    (replace-regexp-in-string regexp "" pattern)))

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

(defun dash-docs-parse-url (docset-name filename &optional anchor)
  "Return the full, absolute URL to documentation.
Either a file:/// URL joining DOCSET-NAME, FILENAME & ANCHOR
or a http(s):// URL formed as-is if FILENAME is a full HTTP(S) URL."
  (let* ((filename (replace-regexp-in-string "<dash_entry_.*>" "" filename))
         (path (format "%s%s" filename
                       (if anchor (format "#%s" anchor) ""))))
    (if (string-match-p "^https?://" path) path
      (replace-regexp-in-string
       " "
       "%20"
       (concat "file:///"
               (expand-file-name "Contents/Resources/Documents/"
                                 (dash-docs-docset-path docset-name))
               path)))))

(defun dash-docs-browse-url (candidate)
  "Call to `browse-url' after parse the chosen CANDIDATE."
  ;; parse chosen candidate
  (let ((name (car candidate))
        (file (nth 2 (cadr candidate)))
        (anchor (nth 3 (cadr candidate))))
    (funcall dash-docs-browser-func
             (dash-docs-parse-url name file anchor))))

(defun dash-docs--debug-buffer ()
  "Return the `dash-docs' debug buffer."
  (get-buffer-create dash-docs-debug-buffer))

(defun dash-docs-create-debug-buffer ()
  "Open debugging buffer and insert a header message."
  (with-current-buffer (dash-docs--debug-buffer)
    (erase-buffer)
    (insert ";; Dash-docs sqlite3 error logging.\n")))

(defun dash-docs-search-docset (connection pattern)
  "Search PATTERN in CONNECTION, return a list of formatted rows."
  (cl-loop for row in (dash-docs-sql-search connection pattern)
           collect (dash-docs--format-row connection row)))

(defun dash-docs-search-all-docsets (pattern)
  "Search a PATTERN in all available connected docsets."
  (cl-loop for connection in (dash-docs-connections pattern)
           appending (dash-docs-search-docset connection pattern)))

(defun dash-docs-pattern-candidates ()
  "Provide docsets candidates."
  (dash-docs-search-all-docsets ""))

(defun dash-docs-minibuffer-read (prompt choices)
  "Read from the `minibuffer' using PROMPT and CHOICES as candidates.
Report an error unless a valid docset is selected."
  (let ((completion-ignore-case t))
    (completing-read (format "%s (%s): " prompt (car choices))
                     choices nil t nil nil choices)))

;;;###autoload
(defun dash-docs-find-file ()
  "Find dash documentation file."
  (interactive)
  ;; verify if common docsets were set
  (when (not (> (length dash-docs--common-docsets) 0))
    (call-interactively 'dash-docs-activate-docset))
  ;; initialize connections (if necessary)
  (dash-docs-initialize-connections)
  ;; map candidates
  (let* ((candidates (dash-docs-pattern-candidates))
         (candidate (completing-read "Docs: " candidates nil t)))
    (if (equal candidate "")
        (dash-docs--message "error, please provide a search string"))
    ;; update the candidate
    (setq candidate (cdr (assoc candidate candidates)))
    ;; browse url a.k.a find file
    (dash-docs-browse-url candidate)))

;;;###autoload
(defun dash-docs-clean-all-connections ()
  "Clean `dash-docs--connections' interactively."
  (interactive)
  ;; set internal var to nil
  (setq dash-docs--connections nil))

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
  (push docset dash-docs--common-docsets))

;;;###autoload
(defun dash-docs-deactivate-docset (docset)
  "Deactivate DOCSET, i.e, update common docsets.
If called interactively prompts for the docset name."
  ;; maps docset parameter
  (interactive
   (list
    (dash-docs-minibuffer-read "Deactivate docset"
                               dash-docs--common-docsets)))
  ;; delete docset from common docsets
  (setq dash-docs--common-docsets
        (delete docset dash-docs--common-docsets)))

;;;###autoload
(defun dash-docs-install-user-docset (docset-name)
  "Download an unofficial docset with specified DOCSET-NAME."
  ;; maps docset name parameter
  (interactive
   (list
    (dash-docs-minibuffer-read
     "Install docset"
     (mapcar 'car
             (dash-docs-unofficial-docsets)))))
  ;; create docsets path
  (when (dash-docs--created-docsets-path
         (dash-docs-docsets-path)))
  ;; install docset
  (let ((url (car (assoc-default docset-name
                                 (dash-docs-unofficial-docsets)))))
    (dash-docs--install-docset url docset-name)))

;;;###autoload
(defun dash-docs-install-docset-from-file (docset-tmp-path)
  "Extract the content of DOCSET-TMP-PATH.
Move it to `dash-docs-docsets-path' and activate the docset."
  ;; maps docset temporary path parameter
  (interactive
   (list (car (find-file-read-args "Docset Tarball: " t))))
  ;; get docset folder
  (let ((docset-folder
         (dash-docs-extract-and-get-folder docset-tmp-path)))
    ;; activate docset folder
    (dash-docs-activate-docset docset-folder)
    ;; debug message
    (dash-docs--message "docset installed.")))

;;;###autoload
(defun dash-docs-install-docset (docset-name)
  "Download an official docset with specified DOCSET-NAME.
Move its stuff to docsets-path."
  ;; maps docset name parameter
  (interactive
   (list (dash-docs-minibuffer-read
          "Install docset"
          (dash-docs-official-docsets))))
  ;; create docsets if necessary
  (when (dash-docs--created-docsets-path (dash-docs-docsets-path)))
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
    (setq url (dash-docs-get-docset-url feed-tmp-path))
    ;; install docset
    (dash-docs--install-docset url docset-name)))

;;;###autoload
(defun dash-docs-async-install-docset-from-file (docset-tmp-path)
  "Asynchronously extract the content of DOCSET-TMP-PATH.
Move it to `dash-docs-docsets-path` and activate the docset."
  ;; maps docset temporary path parameter
  (interactive
   (list (car (find-file-read-args "Docset Tarball: " t))))
  ;; extract the archive contents
  (let ((docset-folder (dash-docs-extract-and-get-folder docset-tmp-path)))
    (dash-docs-activate-docset docset-folder)
    (message (format
              "Docset installed. Add \"%s\" to dash-docs-common-docsets."
              docset-folder))))

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
    ;; initialize common connections list
    (dash-docs-initialize-connections)
    ;; set dash docs mode indicator to true
    (setq dash-docs-mode t))
   (t
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
