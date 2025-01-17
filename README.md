# denote-db: SQLite cache for Denote metadata

This package allows the user to maintain a SQLite database for the
purpose of caching Denote metadata.  This is similar to how other
note-taking programs, notably [Org Roam](https://github.com/org-roam/org-roam), deal with hundreds or
thousands of notes while still providing fast response.

SQLite is built into Emacs since version 29.1 (released in 2023), so
we don't need external dependencies to achieve this.

Note that this is merely an EXPERIMENT.  Several things need to be
polished before it is production-ready.  I could change the layout or
the format of the SQLite database at any moment, because I haven't
settled yet on what should and should not be included, and in what
form or shape.  Suggestions are welcome!

Note that I haven't documented everything yet.  I'll write a manual
once the package is finished (if it is even finished :D).

Currently, the following things are cached:

* id
* title
* keywords (separated by spaces)
* date (date of creation, ISO 8601 format)
* type (org, md, etc.)
* file (absolute path)
* last_modified (date of last modification, ISO 8601 format)
* signature

Generating the database with 500 notes takes 1.5 seconds on my laptop
and roughly 16 seconds on my smartphone (most things are slower on
Emacs for Android).  Ideally you would only generate the database
once, because after that it will be updated automagically if you
enable `denote-db-update-mode`.  Of course, if you edit the files
outside Emacs, you should run `denote-db-regenerate-database` manually
(although `denote-db-check-deleted-created` should also work for most
scenarios).

Note that this is merely a library.  It is not useful by itself.  But
look at all the fun things we can do (remember that the API is not
stable yet!):

## Call examples

### Return a list with the titles of all notes

```elisp
(denote-db-query :select 'title)
```

### Return a list with the titles of all notes with `emacs` in the title, created after 2024-01-01 but before 2025-01-01

```elisp
(denote-db-query
  :select 'title
  :where  '((~ title "%emacs%")
            (> date "2024-01-01T00:00:00")
            (< date "2025-01-01T00:00:00")))
```

Note how the `where` statement is formatted.  `~` is equivalent to the `LIKE` operator.  Familiarity with SQLite queries is naturally recommended, but you get used to them quickly.

### Return a list with all the keywords used in notes

```elisp
(delete-dups
 (flatten-list
  (mapcar #'split-string (denote-db-query :select 'keywords))))
```

### Return a list with all the keywords used in notes with the `philosophy` keyword

```elisp
(delete-dups
 (flatten-list
  (mapcar #'split-string (denote-db-query
                          :select 'keywords
                          :where  '(~ keywords "%philosophy%")))))
```

### Return a list with the absolute path to all notes modified in the last 7 days

```elisp
(denote-db-query
 :select 'file
 :where (list '> 'last_modified
              (format-time-string
               "%FT%R"
               (encode-time
                (decoded-time-add
                 (decode-time)
                 (make-decoded-time :day -7))))))
```

## Actually useful examples

### Prompt for a note completing with title, and open it

```elisp
(defun denote-db-open-note ()
  (interactive)
  (let ((data (denote-db-query :select '(title file))))
    (find-file
     (cadr
      (assoc (completing-read "Select note: " data nil t) data)))))
```

### Complete a link based on word at point

```elisp
(defun denote-db-complete-link ()
  (interactive)
  (when-let* ((bounds (bounds-of-thing-at-point 'word))
              (word (buffer-substring (car bounds) (cdr bounds))))
    (if-let* ((data (denote-db-query
                     :select '(title file)
                     :where  (list '~ 'title (concat "%" word "%"))))
              (selected (assoc
                         (completing-read
                          "Link to note: "
                          data nil t word)
                         data)))
        (progn
          (delete-region (car bounds) (cdr bounds))
          (denote-link (elt selected 1)                                ; file
                       (denote-filetype-heuristics buffer-file-name)   ; type
                       (elt selected 0)))                              ; title
      (user-error "No matching notes"))))
```

This could easily be converted into a capf, too, though I'm not
familiar with the interface.

### Prompt for a keyword with completion, then open a note with that keyword (prompting with title)

```elisp
(defun denote-db-open-file-with-keyword ()
  (interactive)
  (let ((keywords (delete-dups
                   (flatten-list
                    (mapcar
                     #'split-string
                     (denote-db-query :select 'keywords))))))
    (when-let* ((keyword (completing-read "Keyword: " keywords nil t))
                (data (denote-db-query
                       :select '(title file)
                       :where (list '~ 'keywords (concat "%" keyword "%")))))
      (find-file
       (cadr
        (assoc (completing-read "Select note: " data nil t) data))))))
```

## Pending things

* Should we store the links in the database, too?  This could prove
  very useful for things like backlinks, but it will also increase the
  odds of the cache becoming out-of-sync.  We'll probably need to
  store hash sums of the files and check them, just like Org Roam
  does.  Also, it will be hard to implement.  Is it worth it?
