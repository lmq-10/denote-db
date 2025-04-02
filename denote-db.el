;;; denote-db.el --- SQLite cache for Denote metadata -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Lucas Quintana

;; Author: Lucas Quintana <lmq10@protonmail.com>
;; Maintainer: Lucas Quintana <lmq10@protonmail.com>
;; URL: https://github.com/lmq-10/denote-db
;; Created: 2025-01-17
;; Version: 0.0.3-dev
;; Package-Requires: ((emacs "29.1") (denote "3.0"))

;; This program is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows the user to maintain a SQLite database for the
;; purpose of caching Denote metadata.  This is similar to how other
;; note-taking programs, notably Org Roam, deal with hundreds or
;; thousands of notes while still providing fast response.
;;
;; This package requires your Emacs to be compiled with SQLite
;; support, which is the default since version 29.1.
;;
;; This is just a fun experiment right now.  You can test it and
;; suggest improvements!
;;
;; Currently, the following things are cached:
;; * id
;; * title
;; * keywords (separated by spaces)
;; * date (ISO 8601 format)
;; * type (org, md, etc.)
;; * file (absolute path)
;; * last_modified (date of last modification, ISO 8601 format)
;; * signature
;; * links (linked IDs, separated by spaces)
;;
;; To update the cache automatically, use `denote-db-update-mode'.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'denote)
(require 'seq)
(require 'sqlite)

;;;; User options:

(defgroup denote-db ()
  "Use a SQLite database for Denote metadata."
  :group 'files
  :link '(info-link "(denote-db) Top"))

(defcustom denote-db-file-name ".cache.db"
  "Name for the database file used to cache Denote metadata."
  :group 'denote-search
  :type 'string)

(defcustom denote-db-watch-changes nil
  "If non-nil, `denote-db-update-mode' will watch for changes outside Emacs.

This is done by marking the database as outdated every
`denote-db-watch-time' seconds.  Then, when `denote-db-query' is called
or a note is saved, the database is updated (using `denote-db-update'),
integrating any changes made to the notes.

This variable should be set before enabling `denote-db-update-mode'."
  :type 'boolean)

(defcustom denote-db-watch-time 3600
  "Time (in seconds) to wait before scheduling an update to the database.

Only used when `denote-db-update-mode' is enabled and
`denote-db-watch-changes' is non-nil."
  :type 'natnum)

;;; Main variables

(defvar denote-db-sqlite-asocciations '((~ . " LIKE "))
  "Map of symbols and their corresponding SQLite translation.")

(defvar denote-db--watcher nil
  "Timer used by `denote-db-update-mode'.

Only used when `denote-db-watch-changes' is non-nil.")

(defvar denote-db--should-update nil
  "Non-nil if the database should be updated.

Set by `denote-db--watcher'.

Used only by `denote-db-maybe-update'.")

(defvar denote-db--cached-alist nil
  "Alist of cached database objects.

Every element should be a cons in the form (DIRECTORY . OBJECT).

This allows us to have different databases for different silos.")

;;; Main functions:

(defun denote-db-file ()
  "Return absolute path to the database file."
  (expand-file-name denote-db-file-name (denote-directory)))

(defun denote-db ()
  "Return the database object to use as cache.
It is generated from `denote-db-file'.

The database object is normally referenced in `denote-db--cached-alist'."
  (let* ((db-file (denote-db-file))
         (dir (denote-directory))
         (db (cdr-safe (assoc dir denote-db--cached-alist)))
         (db-ok (sqlitep db)))
    (cond ((and db-ok (file-exists-p db-file))
           ;; Database is cached and file exists
           db)
          (db-ok
           ;; Database is cached, but file is missing
           (sqlite-close db)
           (setq db (sqlite-open db-file))
           (setq denote-db--cached-alist
                 (assoc-delete-all dir denote-db--cached-alist))
           (push (cons dir db) denote-db--cached-alist)
           db)
          (:else
           ;; Database is not cached
           (setq db (sqlite-open db-file))
           (push (cons dir db) denote-db--cached-alist)
           db))))

(defun denote-db-create ()
  "Create the database.

Specifically, create a table with name `denote_db' in the SQLite object
returned by `denote-db'.

The database will be empty.  To populate it, use
`denote-db-regenerate-database' instead."
  (let ((db (denote-db)))
    (with-sqlite-transaction db
      (sqlite-execute
       db
       (concat
        "CREATE TABLE"
        " denote_db"
        " (id text(15), title text, keywords text, date datetime(19),"
        " type text(10), file text, last_modified datetime(19), signature text,"
        " links text)")))))

(defun denote-db-table-exists-p ()
  "Return non-nil if there's a table in the database."
  ;; NOTE: sqlite_master is the old name, but it remains compatible
  ;; with newer versions
  (sqlite-select
   (denote-db)
   "SELECT name FROM sqlite_master WHERE type='table' AND name='denote_db'"))

(defun denote-db-drop ()
  "Drop the main table from the database.

This effectively clears the database."
  (when (denote-db-table-exists-p)
    (let ((db (denote-db)))
      (with-sqlite-transaction db
        (sqlite-execute db "DROP TABLE denote_db")))))

(defun denote-db-clean ()
  "Recreate the main table from the database."
  (denote-db-drop)
  (denote-db-create))

(defun denote-db-query--format-select (query)
  "Format a SELECT statement based on QUERY.

QUERY can be a string, a symbol or a list."
  (cond ((or (stringp query) (symbolp query))
         ;; Strings or symbols are returned verbatim
         query)
        ((listp query)
         ;; Lists are arranged
         (let (statement)
           (dolist (this-request query statement)
             (setq statement
                   (if statement
                       (concat statement "," (symbol-name this-request))
                     (symbol-name this-request))))))))

(defun denote-db-query--format-where-1 (query)
  "Subroutine of `denote-db-query--format-where'.

Used to format queries based on lists."
  (let* ((op1 (elt query 1))
         (fun (elt query 0))
         (op2 (elt query 2))
         (translated (assq fun denote-db-sqlite-asocciations)))
    (format "%s%s%s"
            op1
            (if translated
                (cdr translated)
              fun)
            (pcase op2
              ((pred stringp)
               (format "'%s'" op2))
              (_ op2)))))

(defun denote-db-query--format-where (query)
  "Format a WHERE statement based on QUERY.

QUERY can be a string or a list."
  (cond ((stringp query)
         ;; A string is returned verbatim
         query)
        ((symbolp (car-safe query))
         ;; There's a single list, which is the query
         (denote-db-query--format-where-1 query))
        ((listp query)
         ;; There's a list of lists, so we have several conditions
         (string-join
          (mapcar #'denote-db-query--format-where-1 query)
          " and "))))

(defun denote-db-query-1 (select where)
  "Main subroutine of `denote-db-query'."
  (denote-db-check-database)
  (sqlite-select
   (denote-db)
   (format
    "select %s from denote_db%s"
    (denote-db-query--format-select select)
    (if (not where)
        ""
      (concat " where " (denote-db-query--format-where where))))))

;; This is the main thing I have to document.  I haven't done it
;; because I'll probably change things in the future.
;;
;; Basically, right now we have two main keys, :select and :where.
;; They format the corresponding SELECT and WHERE statements for
;; querying the database.
;;
;; For :select, we accept a symbol or a list of symbols; it's also
;; possible to use a string which is passed verbatim.  If a list of
;; symbols is passed, it is arranged; so '(id file) becomes "id,file".
;;
;; For :where, we accept a list or a list of lists; as before, a
;; single string is passed verbatim.  The list(s) should have three
;; elements.  The first is passed in between the second and third when
;; arranged, that's why '(= id "20250103T184239") becomes
;; "id='20250103T184239'".  Some elements are transformed when they
;; are in the first position; for instance, ~ becomes " LIKE ".
;; Transformations are defined in `denote-db-sqlite-asocciations'.
;;
;; The special :no-dup key deletes duplicate items from output.
;;
;; If it proves useful, I'll also implement a :group key.  I haven't
;; found a use case yet.
;;
;;;###autoload
(cl-defun denote-db-query (&key (select "*") (where nil) (no-dup nil))
  "Query the Denote database."
  (denote-db-maybe-update)
  (let ((output (denote-db-query-1 select where)))
    ;; We assume the user wants a flattened output if there's only
    ;; one kind of item requested.  I think that's reasonable.
    (when (= (length (car-safe output)) 1)
      (setq output (flatten-list output)))
    ;; We also assume the user wants keywords and links as lists, and
    ;; not as strings separated by whitespaces (which is how they are
    ;; stored in database)
    (when (or (eq 'keywords select) (eq 'links select))
      (setq output (flatten-list (mapcar #'split-string output))))
    ;; When more than one item is selected, it is a little trickier to
    ;; convert the strings mentioned above to lists.  But the
    ;; following code achieve it.
    (when (and
           (listp select)
           (length> select 1)
           (or (memq 'keywords select) (memq 'links select)))
      (let ((key-pos (seq-position select 'keywords #'eq))
            (links-pos (seq-position select 'links #'eq)))
        (setq
         output
         (mapcar (lambda (x)
                   (seq-map-indexed
                    ;; “I heard you like lambdas, so I put a lambda in
                    ;; your lambda”
                    (lambda (y z)
                      (if (or (eq z key-pos) (eq z links-pos))
                          (and (stringp y) (split-string y))
                        y))
                    x))
                 output))))
    ;; Delete duplicate entries when the :no-dup key is passed
    (when no-dup (setq output (delete-dups output)))
    ;; Return output
    output))

(defalias 'denote-db-api #'denote-db-query)

(defun denote-db-id-exits-p (id)
  "Return non-nil if ID is already in the database."
  (denote-db-query :where `(= id ,id)))

(defsubst denote-db--containing (string)
  "Enclose STRING between percentage signs ('%').

This allows to find occurences contaning it when using the LIKE SQLite
operator."
  (concat "%" string "%"))

(defun denote-db--collect-linked-ids (file)
  "Return a list with all IDs linked in FILE."
  ;; Based on `denote-link-return-links'
  (when-let* ((file-type (denote-filetype-heuristics file))
              (regexp (denote--link-in-context-regexp file-type)))
    (with-temp-buffer
      (insert-file-contents file)
      (denote-link--collect-identifiers regexp))))

(defun denote-db-insert-file (file &optional update nocommit)
  "Insert FILE into database.

If it is already there (that is, if its ID is already in the database),
don't do anything, unless UPDATE is non-nil, in which case its data is
recreated.

The changes are committed to the database at the end of the transaction,
unless NOCOMMIT is non-nil, in which case they aren't.  This is useful
if a lot of files are inserted at once, but ensure to commit yourself if
you use that option."
  (when (and (denote-file-has-identifier-p file)
             ;; We don't store data for encrypted files in the
             ;; database, because
             ;; a) it would ask for password every time, and
             ;; b) it probably contains sensitive info anyway
             (seq-some (lambda (e) (string-suffix-p e file))
                       (denote-file-type-extensions)))
    (let* ((denote-id  (denote-retrieve-filename-identifier file))
           (type (denote-filetype-heuristics file))
           (title
            (or (denote-retrieve-front-matter-title-value file type)
                (denote-retrieve-filename-title file)
                (file-name-base file)))
           (keywords-list
            (or (denote-retrieve-front-matter-keywords-value file type)
                (denote-extract-keywords-from-path file)))
           (signature
            (or (denote-retrieve-front-matter-signature-value file type)
                (denote-retrieve-filename-signature file)))
           (mod-time (file-attribute-modification-time (file-attributes file)))
           (parsed-date
            (when denote-id
              (condition-case nil
                  ;; Only parse it as a date if it is a date
                  (denote-valid-date-p denote-id)
                (error nil))))
           (links-list (denote-db--collect-linked-ids file))
           (links-string (and links-list (string-join links-list " ")))
           (used-id (denote-db-id-exits-p denote-id))
           columns)
      (catch 'exit
        (when used-id
          (if (not update)
              ;; Don't do anything if ID already exists and we can't
              ;; update the table
              (throw 'exit nil)
            (let ((db (denote-db)))
              (sqlite-execute
               db
               (format "DELETE FROM denote_db WHERE id='%s'" denote-id))
              (unless nocommit (sqlite-commit db)))))
        ;; Determine columns to use
        (setq columns
              (delq nil
                    (list
                     (and denote-id "id")
                     (and title "title")
                     (and keywords-list "keywords")
                     (and parsed-date "date")
                     (and type "type")
                     (and file "file")
                     (and mod-time "last_modified")
                     (and signature "signature")
                     (and links-string "links"))))
        ;; Actually update the database
        (let ((db (denote-db)))
         (sqlite-execute
          db
          (format
           "INSERT INTO denote_db(%s) VALUES(%s)"
           (string-join columns ", ")
           (string-join
            (delq nil
                  (list
                   (and denote-id (format "'%s'" denote-id))
                   (and title (format "'%s'" title))
                   (and keywords-list
                        (format "'%s'" (string-join keywords-list " ")))
                   (and parsed-date
                        (format-time-string "'%FT%T'" parsed-date))
                   (and type (format "'%s'" type))
                   (and file (format "'%s'" file))
                   (and mod-time (format-time-string "'%FT%T'" mod-time))
                   (and signature (format "'%s'" signature))
                   (and links-string (format "'%s'" links-string))))
            ", ")))
         (unless nocommit (sqlite-commit db)))))))

(defun denote-db-update-file (file)
  "Update data for FILE in the database."
  (denote-db-insert-file file :update))

(defun denote-db-update ()
  "Try to update the database, without recreating it.

This function first checks for files which are not in the database, or
which are present in it but don't exist anymore.  It then proceeds to
fix these inconsistencies.  `denote-db-check-deleted-created' does this
job.

After that, it checks the modification time of every file in
`denote-directory', and compares it with the time cached in the
database (the `last_modified' column).  If the cached time is behind the
real time, that file is updated then."
  (interactive)
  (message "Updating database...")
  (unless (denote-db-check-database)
    (denote-db-check-deleted-created)
    (let ((cached-times (denote-db-query :select '(file last_modified)))
          real-times)
      (dolist (file (denote-directory-files))
        (push
         (cons file (file-attribute-modification-time (file-attributes file)))
         real-times))
      (dolist (ctime cached-times)
        (when-let* ((rtime (assoc (car ctime) real-times)))
          (when (string-lessp (cadr ctime) (format-time-string "%FT%T" (cdr rtime)))
            (denote-db-insert-file (car rtime) :update :no-commit))))
      (sqlite-commit (denote-db))
      (message "Updating database...done"))))

(defun denote-db-maybe-update ()
  "Update the database if an update is scheduled.

Only useful when `denote-db-update-mode' is enabled and
`denote-db-watch-changes' is non-nil."
  (when (and denote-db-watch-changes denote-db--should-update)
    (setq denote-db--should-update nil) ; If we don't unset it first, infinite loop :(
    (denote-db-update)
    (and denote-db--watcher (cancel-timer denote-db--watcher))
    (setq denote-db--watcher
          (run-with-timer
           denote-db-watch-time
           nil
           #'denote-db-mark-as-outdated))))

(defun denote-db-mark-as-outdated ()
  "Schedule an update for the database.

The next call to `denote-db-query' will update the database.  It will
also happen when the next note is saved if `denote-db-update-mode' is
enabled.  Actually, any call to `denote-db-maybe-update' will do it."
  (setq denote-db--should-update t))

(defun denote-db-delete-file (file)
  "Delete FILE from database."
  (let ((db (denote-db)))
    (with-sqlite-transaction db
      (sqlite-execute
       db
       (format "DELETE FROM denote_db WHERE file='%s'" file)))))

(defun denote-db-regenerate-database ()
  "Recreate the database, updating it with the metadata of all notes."
  (interactive)
  (message "Generating database...")
  (denote-db-clean)
  (mapc (lambda (file) (denote-db-insert-file file nil :nocommit))
        (denote-directory-files nil nil :text-only))
  (sqlite-commit (denote-db))
  (message "Generating database...done"))

(defun denote-db-check-database ()
  "If database is not found or is invalid, repopulate it."
  (unless (and (file-exists-p (denote-db-file))
               (denote-db-table-exists-p))
    (denote-db-regenerate-database)))

(defun denote-db-check-deleted-created (&rest _)
  "Delete removed files from database, add created ones.

This works by checking differences between cached files and existing
files."
  (interactive)
  (let* ((current-files (denote-directory-files nil nil :text-only))
         (cached-files (denote-db-query :select "file"))
         (old-files (seq-difference cached-files current-files))
         (new-files (seq-difference current-files cached-files)))
    (when old-files
      (let ((db (denote-db)))
        (with-sqlite-transaction db
          (dolist (file old-files)
            (sqlite-execute
             db
             (format "DELETE FROM denote_db WHERE file='%s'" file))))))
    (when new-files
      (dolist (file new-files)
        (denote-db-insert-file file)))))

(defun denote-db-update-this-file ()
  "Update the data of current file in the database."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (denote-db-update-file buffer-file-name)))

;; This is similar to how Org Roam does it
(defun denote-db--setup-file ()
  "Setup the current buffer to update the DB after saving the current file."
  (when (and buffer-file-name (denote-filename-is-note-p buffer-file-name))
    (add-hook 'after-save-hook #'denote-db-update-this-file nil t)))

(defun denote-db--handle-delete (file &optional _trash)
  "Remove FILE from database, if it should be removed."
  (let ((absolute-file (expand-file-name file)))
    (when (denote-filename-is-note-p absolute-file)
      (denote-db-delete-file absolute-file))))

(defun denote-db--handle-rename (file newfile &rest _)
  "If FILE or NEWFILE should be in the database, update accordingly.
Intended as an after advice for `rename-file'."
  (setq file (expand-file-name file))
  (setq newfile (expand-file-name newfile))
  (let ((denote-dir (expand-file-name (denote-directory))))
    (when (or (string-prefix-p denote-dir file)
              (string-prefix-p denote-dir newfile))
      ;; A note has been renamed, so update the database
      (unless (eq major-mode 'wdired-mode)
        ;; We don't update in WDired because a lot of renamings could
        ;; be happening at once (we do it at the end though)
        (denote-db-check-deleted-created)))))

(declare-function wdired-do-renames "wdired" (renames))

(define-minor-mode denote-db-update-mode
  "Toggle automatic update of Denote database.

Every time a note is saved, deleted or moved, the entry corresponding to
that note in the database will be properly updated.

Additionally, if `denote-db-watch-changes' is non-nil, a wide update to
the database will be scheduled every `denote-db-watch-time' seconds.
Then, saving a note or calling `denote-db-query' will update the
database.  This is useful if you also edit your notes outside Emacs."
  :global t
  (if denote-db-update-mode
      (progn
        (denote-db-update)
        (when denote-db-watch-changes
          (setq denote-db--watcher
                (run-with-timer
                 denote-db-watch-time
                 nil
                 #'denote-db-mark-as-outdated)))
        (advice-add #'rename-file :after #'denote-db--handle-rename)
        (advice-add #'delete-file :before #'denote-db--handle-delete)
        (advice-add #'wdired-do-renames :after #'denote-db-check-deleted-created)
        (add-hook 'find-file-hook #'denote-db--setup-file))
    (progn
      (and denote-db--watcher (cancel-timer denote-db--watcher))
      (remove-hook 'find-file-hook #'denote-db--setup-file)
      (advice-remove #'wdired-do-renames #'denote-db-check-deleted-created)
      (advice-remove #'rename-file #'denote-db--handle-rename)
      (advice-remove #'delete-file #'denote-db--handle-delete))))

(provide 'denote-db)
;;; denote-db.el ends here
