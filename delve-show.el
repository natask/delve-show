;;; delve-show.el --- Search boolean lists of tags using delve -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Natnael Kahssay
;;
;; Author: Natnael Kahssay <https://github.com/natask>
;; Maintainer: Natnael Kahssay <thisnkk@gmail.com>
;; Created: June 08, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/savnkk/delve-show
;; Package-Requires: ((emacs "24.3") (delve "0.7"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; -----------------------------------------------------------
;; * Dependencies

(require 'cl-lib)
(require 'delve)
(declare-function copy-list "cl-lib")

;; -----------------------------------------------------------
;; * variables
(defcustom delve-show-tag-data-types '((fuzzy . ((format . "%%%s%%")
                                             ))
                                       (exact . ((format . "%%\"%s\"%%")
                                             )))
                                     "Types on how to treat tag search. fuzzy or exact or any other custom type."
:type '(alist (group (alist (group sexp))))
:group 'delve-show)

(defcustom delve-show-tag-data-type 'fuzzy
  "Default tag search type.
Defaults,
  - fuzzy.
  - exact."
  :type 'symbol
  :group 'delve-show)
(defcustom delve-show-fuzzy-title 'fuzzy
"Default title search type.
Defaults,
  - fuzzy.
  - exact."
:type 'symbol
:group 'delve-show)

(defcustom delve-show--parser-type 'tags-only
  "Parser type for delve-show.
Can be
   'tags-only
   'titles-and-tags"
  :type 'symbol
  :group 'delve-show)

(defcustom delve-postprocess-sort-pred (delve-db-zettel-sorting-pred #'time-less-p 'mtime)
  "Function to type for delve-show.
Can be
   'tags-only
   'titles-and-tags"
  :type 'function
  :group 'delve-show)

;; -----------------------------------------------------------
;; * code
(defun delve-show--last-char (word)
  "Return last char of WORD."
  (let* ((len (length word)))
    (substring word (- len 1))))

(defun delve-show--sans-last-char (word)
  "Return WORD without the last char."
  (let* ((len (length word)))
    (substring word 0 (- len 1))))

(defun delve-show--present-participle (word)
  "Return the present-participle (-ing) form of WORD."
  (concat
   (let ((last-str (delve-show--last-char word)))
     (cond
      ((equal last-str "e") (delve-show--sans-last-char word))
      (t word)))
   "ing"))

(defun delve-show--plural (word)
  "Return the Plural form of WORD."
  (let ((beg (let ((last-str (delve-show--last-char word)))
               (cond
                ((equal last-str "y") (delve-show--sans-last-char word))
                (t word))))
        (end (let ((last-str (delve-show--last-char word)))
               (cond
                ((equal last-str "s") "es")
                ((equal last-str "y") "ies")
                (t "s")))))
    (concat  beg end)))

(defun delve-show--country-to-people (word)
  "Return the people or country given the other as WORD.
Only works on `ethiopia' and `america'."
  (cond
   ((equal word "ethiopia")  "ethiopian")
   ((equal word "ethiopian") "ethiopia")
   ((equal word "american") "america")
   ((equal word "america") "american")
   (t nil)))

(defun delve-show--variations (word)
"Generate variations on WORD."
  (if (> (length word) 2)
      (remove 'nil
              (list word
                    (delve-show--country-to-people word)
                    (delve-show--present-participle word)
                    (delve-show--plural word)))
    word))

(defun delve-show--get-new-queries (vals)
  (pcase delve-show--parser-type
    ('tags-only
     `((like tags:tags ,(intern (concat "$r" (number-to-string (+ (length vals) 1)))))))
    ('titles-and-tags
     (pcase delve-show-fuzzy-title
       ('fuzzy
        `((or
           (like tags:tags ,(intern (concat "$r" (number-to-string (+ (length vals) 1)))))
           (like titles:title ,(intern (concat "$r" (number-to-string (+ (length vals) 2))))))))
       ('exact
     `((or
        (like tags:tags ,(intern (concat "$r" (number-to-string (+ (length vals) 1)))))
        (like titles:title ,(intern (concat "$r" (number-to-string (+ (length vals) 2)))))
        (like titles:title ,(intern (concat "$r" (number-to-string (+ (length vals) 3)))))
        (like titles:title ,(intern (concat "$r" (number-to-string (+ (length vals) 4)))))
        (like titles:title ,(intern (concat "$r" (number-to-string (+ (length vals) 5))))))))))))

(defun delve-show--get-new-vals (val)
  (pcase delve-show--parser-type
    ('tags-only
     (list val))
    ('titles-and-tags
     (pcase delve-show-fuzzy-title
       ('fuzzy
        (list val val))
        ('exact
     (list val val val val val))))))

(defun delve-show--parse-helper (stuct vals)
  (let ((valk vals) queries)
    (pcase stuct
      (`(or . ,args) (progn
                       (setq queries '(or))
                       (dolist (arg args)
                         (let ((comp  (delve-show--parse-helper arg valk)))
                           (setq valk (plist-get comp :vals))
                           (setq queries (nconc (copy-list queries) (plist-get comp :queries)))
                           ))
                       (setq queries (list queries))
                       `(:queries ,queries :vals ,valk)))
      (`(and . ,args) (progn
                        (setq queries '(and))
                        (dolist (arg args)
                          (let ((comp  (delve-show--parse-helper arg valk)))
                            (setq valk (plist-get comp :vals))
                            (setq queries (nconc (copy-list queries) (plist-get comp :queries)))
                            ))
                        (setq queries (list queries))
                        `(:queries ,queries :vals ,valk)))
      ('nil    (list :queries 'nil :vals 'nil))
      (x      `(:queries ,(delve-show--get-new-queries vals)
                :vals ,(nconc vals (delve-show--get-new-vals x))))
      )))

(defun delve-show--join (init args)
  (dolist (arg args)
    (let ((valk (delve-show--add-parse arg)))
      (if valk
          (nconc init `(,valk)))))
  init)

(defun delve-show--add-parse (stuct)
  (pcase stuct
    (`(,op . ,args) (progn
                      (cond
                       ((member op '(and or)) (delve-show--join `(,op) args))
                       (t 'nil)
                       )))
    ('nil   'nil)
    (x      (-as-> x it
                   (cond
                    ((stringp it) it)
                    (t (symbol-name it)))
                   (delve-show--variations it)
                   (if (listp it)
                       (progn
                         (-as-> it it
                                (mapcar 'intern it)
                                (cons 'or it)))
                     it)))
    ))

(defun delve-show--parse (stuct)
  (let ((ret (delve-show--parse-helper stuct '())))
    (plist-put ret :queries (car (plist-get ret :queries)))))

(defun delve-show--get-vals (vals)
  (cl-loop for fval in vals
           with i = 1
           with acc = nil
           do
           (pcase delve-show--parser-type
             ('tags-only
              ;;(setq acc (cons (format "%%%s%%" fval) acc))
                 (setq acc (cons (format (delve-show--get-format-string) fval) acc)))
             ('titles-and-tags
              (pcase delve-show-fuzzy-title
                ('exact
              (setq acc (cons (pcase (% i 5)
                               (0 (format "%% %s %%" fval))
                               (4 (format "%% %s\"" fval))
                               (3 (format "\"%s %%" fval))
                               (2 (format "%s" fval))
                               (1 (format (delve-show--get-format-string) fval))) acc)))
                ('fuzzy
                 (setq acc (cons (pcase (% i 2)
                                   (0 (format "%%%s%%" fval))
                                   (1 (format (delve-show--get-format-string) fval))) acc))))
              ))
           (setq i (+ 1 i))
           finally (return (reverse acc))))

(defun delve-show--get-format-string ()
  "Get the format string of current tag data type."
  (condition-case err
      (cdr (assoc 'format (cdr (assoc delve-show-tag-data-type delve-show-tag-data-types))))
    (error (message "make sure format is defined in %s in delve-show-tag-data-types.\n%s" delve-show-tag-data-type err) (signal (car err) (cdr err))
           ))
  )

;;;###autoload
(cl-defun delve-show--delve-get-page (&optional (tag-list 'nil) (include-titles 'nil) (search-fuzzy 'nil) (title-fuzzy 'nil))
  (let* ((delve-show--parser-type (if include-titles 'titles-and-tags 'tags-only))
         (delve-show-tag-data-type (if search-fuzzy 'fuzzy 'exact))
         (delve-show-fuzzy-title (if title-fuzzy 'fuzzy 'exact))
         (val (-as-> tag-list it
                     (delve-show--add-parse it)
                     (delve-show--parse it)))
         (query (plist-get val :queries))
         (vals (plist-get val :vals))
         (fvals (delve-show--get-vals vals))
         (constraint (if query (list :constraint `[:where ,query]) 'nil))
         (args (if vals (list :args fvals) 'nil)))
    (print (list (append (list :name "do" :postprocess (lambda (zettel) (cl-sort zettel delve-postprocess-sort-pred))) constraint args)))
         (list (append (list :name "do" :postprocess (lambda (zettel) (cl-sort zettel delve-postprocess-sort-pred))) constraint args))))

;;;###autoload
(cl-defun delve-show (&optional (tag-list 'nil) (include-titles 'nil) (search-fuzzy 'nil) (title-fuzzy 'nil))
  (let* ((test-page  (delve-show--delve-get-page tag-list include-titles search-fuzzy title-fuzzy)))
    (switch-to-buffer (delve-new-collection-buffer (delve-create-searches test-page)
                                                   (delve--pretty-main-buffer-header)
                                                   "test Buffer"))))

;;;###autoload
(cl-defun delve-results (&optional (tag-list 'nil) (include-titles 'nil) (search-fuzzy 'nil) (title-fuzzy 'nil))
  (let* ((test-page  (delve-show--delve-get-page tag-list include-titles search-fuzzy title-fuzzy)))
    (delve-operate-search (car (delve-create-searches test-page)))))

(provide 'delve-show)
;;; delve-show.el ends here
