;;; delve-show.el --- Search boolean lists of tags using delve -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Natnael Kahssay
;;
;; Author: Natnael Kahssay <https://github.com/natask>
;; Maintainer: Natnael Kahssay <thisnkk@gmail.com>
;; Created: June 08, 2021
;; Version: 0.0.1
;; Keywords: matching delve org-roam
;; Homepage: https://github.com/savnkk/delve-show
;; Package-Requires: ((emacs "26.1") (delve "0.7") (sexp-string "0.0.1"))
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

(defcustom delve-show-postprocess-sort-pred (delve-db-zettel-sorting-pred #'time-less-p 'mtime)
  "Sort function for delve."
  :type 'function
  :group 'delve-show)

;; sexp
(require 'sexp-string)

;;; Vars:
(defvar delve-show-predicates
  '((or  :name or  :transform
         ((`(or . ,clauses) `(or ,@(mapcar #' rec clauses))))
         :stringify
         ((`(or . ,clauses) (cl-reduce (lambda (acc elem)
                                         (let ((res (rec elem)))
                                           ;; HACK: works because plist-get grabs the first it can find.
                                           ;; Here it grabs the most recently computed value.
                                           ;; This means need to reverse
                                           ;; while presevering order of subsequent Even and Odd
                                           ;; index elements
                                           (-concat res acc)))
                                       clauses :initial-value accum))))
    (not :name not :transform
         ((`(not . ,clauses) `(not ,@(mapcar #' rec clauses)))))
    (and :name and
         :transform
         ((`(and . ,clauses) `(and ,@(mapcar #' rec clauses))))
         :stringify
         ((`(and . ,clauses) (cl-reduce (lambda (acc elem)
                                          (let ((res (rec elem)))
                                            (list :title (-concat (plist-get acc :title)
                                                                  (plist-get res :title))
                                                  :tags (-concat (plist-get acc :tags)
                                                                 (plist-get res :tags)))))
                                        clauses :initial-value accum))))
    (titles :name titles :aliases '(title)
            :transform
            ((`(,(or 'titles 'title) . ,rest)
              `(and ,@(mapcar (lambda (elem)
                                (cons 'or (mapcan (lambda (elem)
                                                    (mapcar (lambda (elem)
                                                        `(like titles:title ',elem))
                                                    (delve-show--get-vals-title elem)))
                                                  (rec elem)))) rest))))
            :stringify
            ((`(,(or 'titles 'title) . ,rest)
              (plist-put accum :title (-concat (plist-get accum :title) rest)))))
    (tags :name tags :aliases '(tag)
          :transform
          ((`(,(or 'tags 'tag) . ,rest)
            `(and ,@(mapcar (lambda (elem)
                              (cons 'or (mapcan (lambda (elem)
                                                  (mapcar (lambda (elem)
                                                            `(like tags:tags ',elem))
                                                          (delve-show--get-vals-tag elem)))
                              (rec elem)))) rest))))
          :stringify
          ((`(,(or 'tags 'tag) . ,rest)
            (plist-put accum :tags (-concat (plist-get accum :tags) rest)))))
    (both :name both
          :transform
          ((`(both . ,rest)
            (rec `(or (tags ,@rest)
                      (titles ,@rest)))))
          :search
          ((`(both . ,rest)
            (list :title (plist-get (rec `(titles ,@rest) accum) :title)
                  :tags (plist-get (rec `(tags ,@rest) accum) :tags))))
          :stringify
          ((`(both . ,rest)
            (rec `(titles ,@rest) accum))))
    (query :name query
           :transform
           (((pred stringp) (delve-show--variations element))
            ((pred symbolp) (delve-show--variations (symbol-name element))))
           :search
           (((pred stringp) element)
            ((pred symbolp) (symbol-name element)))
           :stringify
           (((pred stringp) element)
           ((pred symbolp) (symbol-name element))))))

(defvar delve-show-default-predicate-boolean 'and)

(defcustom delve-show-default-predicate 'tag
  "Parser type for delve-show.
Can be
   'tags
   'both"
  :type 'symbol
  :group 'delve-show)

;;; Code:
(declare-function delve-show--query-string-to-sexp "ext:delve-show" (query) t)
(declare-function delve-show--transform-query "ext:delve-show" (query) t)
(declare-function delve-show--stringify-query "ext:delve-show" (query) t)
(fset 'delve-show--query-string-to-sexp
      (sexp-string--define-query-string-to-sexp-fn  "delve-show"))
(fset 'delve-show--transform-query (sexp-string--define-transform-query-fn "delve-show" :transform))
(fset 'delve-show--stringify-query (sexp-string--define-transform-query-fn "delve-show" :stringify))

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
    (list word)))
;; -----------------------------------------------------------
;;;
(defun delve-show--get-vals-tag (val)
        (list (format (delve-show--get-format-string) val)))

(defun delve-show--get-vals-title (val)
        (pcase delve-show-fuzzy-title
                ('exact
                  (list
                       (format "%% %s %%" val)
                       (format "%% %s\"" val)
                       (format "\"%s %%" val)
                       (format "%s" val)))
                ('fuzzy
                 (list
                      (format "%%%s%%" val)))))

(defun delve-show--get-format-string ()
  "Get the format string of current tag data type."
  (condition-case err
      (cdr (assoc 'format (cdr (assoc delve-show-tag-data-type delve-show-tag-data-types))))
    (error (message "make sure format is defined in %s in delve-show-tag-data-types.\n%s" delve-show-tag-data-type err) (signal (car err) (cdr err))
           )))

(defun delve-show--wrap-list (lt)
  (if (listp lt)
  (mapcar (lambda (elem)
            (if (listp elem)
                (delve-show--wrap-list elem)
              (if (member elem '(or and))
                  elem
              (list delve-show-default-predicate elem))
              )) lt)
   (list delve-show-default-predicate lt)))

;;;###autoload
(cl-defun delve-show--delve-get-page (tag-list &key (include-titles 'nil) (tag-fuzzy 'nil) (title-fuzzy 'nil) (sexp 'nil))
  (when-let* ((delve-show-default-predicate (if include-titles 'both 'tags))
         (delve-show-tag-data-type (if tag-fuzzy 'fuzzy 'exact))
         (delve-show-fuzzy-title (if title-fuzzy 'fuzzy 'exact))
         (query (-as-> tag-list it
                     (if sexp
                         it
                     (delve-show--wrap-list it))
                     (delve-show--transform-query it)))
         (constraint (if query (list :constraint `[:where ,query]))))
         (list (append (list :name "do" :postprocess (lambda (zettel) (cl-sort zettel delve-show-postprocess-sort-pred))) constraint))))

;;;###autoload
(cl-defun delve-show (tag-list &key (include-titles 'nil) (tag-fuzzy 'nil) (title-fuzzy 'nil) (sexp 'nil))
  (let* ((test-page  (delve-show--delve-get-page tag-list :include-titles include-titles :tag-fuzzy tag-fuzzy :title-fuzzy title-fuzzy :sexp sexp)))
    (switch-to-buffer (delve-new-collection-buffer (delve-create-searches test-page)
                                                   (delve--pretty-main-buffer-header)
                                                   "test Buffer"))))

;;;###autoload
(cl-defun delve-results (tag-list &key (include-titles 'nil) (tag-fuzzy 'nil) (title-fuzzy 'nil) (sexp 'nil))
  (let* ((test-page  (delve-show--delve-get-page tag-list :include-titles include-titles :tag-fuzzy tag-fuzzy :title-fuzzy title-fuzzy :sexp sexp)))
    (delve-operate-search (car (delve-create-searches test-page)))))

(provide 'delve-show)
;;; delve-show.el ends here
