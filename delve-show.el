;;; delve-show.el --- Search boolean lists of tags using delve -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Natnael Kahssay
;;
;; Author: Natnael Kahssay <https://github.com/natask>
;; Maintainer: Natnael Kahssay <thisnkk@gmail.com>
;; Created: June 08, 2021
;; Version: 0.0.2
;; Keywords: matching delve org-roam
;; Homepage: https://github.com/savnkk/delve-show
;; Package-Requires: ((emacs "26.1") (delve "0.9.3") (sexp-string "0.0.1"))
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
(defcustom delve-show-data-types '(:title (:fuzzy (:formats ("%%%s%%"))
                                           :exact (:formats ("%% %s %%"
                                                             "%% %s\""
                                                             "\"%s %%"
                                                             "%s"))
                                           :solo  (:formats ("%s")))
                                   :tags  (:fuzzy (:formats ("%%%s%%"))
                                           :exact (:formats ("%%\"%s\"%%"))
                                           :solo  (:formats ("%s"))))
  "Types on how to treat tag search. fuzzy or exact or solo or any other custom type."
  :type '(plist :key-type (symbol :tag "For which") :value-type (plist :key-type (symbol :tag "type") :value-type (plist :key-type (symbol :tag "keys") (group :tag "data"))))
  :group 'delve-show)

(defcustom delve-show-data-type '(:title :fuzzy
                                  :tags :fuzzy)
  "Default search type.
Defaults,
  - fuzzy.
  - exact."
  :type '(plist :key-type (symbol :tag "For which") :value-type (symbol :tag "type"))
  :group 'delve-show)

(defcustom delve-show-max-results 'nil
  "Max number of results in delve buffer."
  :type 'number
  :group 'delve-show)

;; (defcustom delve-show-postprocess-sort-pred (delve-db-zettel-sorting-pred #'time-less-p 'mtime)
;;   "Sort function for delve."
;;   :type 'function
;;   :group 'delve-show)

;; sexp
(require 'sexp-string)

;;; Vars:

(defvar delve-show-predicates
  '((or  :name or  :transform
         ((`(or . ,clauses) `(or ,@(mapcar #' rec clauses)))))
    (not :name not :transform
         ((`(not . ,clauses) `(not ,@(mapcar #' rec clauses)))))
    (and :name and
         :transform
         ((`(and . ,clauses) `(and ,@(mapcar #' rec clauses)))))
    (titles :name titles :aliases '(title aliases alias)
            :transform
            ((`(,(or 'titles 'title 'aliases 'alias) . ,rest)
              `(or
                ,(-tree-map
                  (lambda (elem)
                    (if (member elem '(or and))
                        elem `(or
                               ,@(mapcan (lambda (elem)
                                           (mapcar (lambda (elem)
                                                     `(like title ,elem))
                                                   (delve-show--format-vals :title elem)))
                                         (rec elem))))) (cons 'and rest))
                ,(-tree-map
                  (lambda (elem)
                    (if (member elem '(or and))
                        elem `(or
                               ,@(mapcan (lambda (elem)
                                           (mapcar (lambda (elem)
                                                     `(like aliases ',elem))
                                                   (delve-show--format-vals :title elem)))
                                         (rec elem))))) (cons 'and rest))))))
    (tags :name tags :aliases '(tag)
          :transform
          ((`(,(or 'tags 'tag) . ,rest)
            (-tree-map
             (lambda (elem)
               (if (member elem '(or and))
                   elem `(or
                          ,@(mapcan (lambda (elem)
                                      (mapcar (lambda (elem)
                                                `(like tags ',elem))
                                              (delve-show--format-vals :tags elem)))
                                    (rec elem))))) (cons 'and rest)))))
    (olp :name olp
         :transform
         ((`(olp . ,rest)
           `(or
             ,(-tree-map
               (lambda (elem)
                 (if (member elem '(or and))
                     elem `(or
                            ,@(mapcan (lambda (elem)
                                        (mapcar (lambda (elem)
                                                  `(like filetitle ',elem))
                                                (delve-show--format-vals :title elem)))
                                      (rec elem))))) (cons 'and rest))

             ,(-tree-map
               (lambda (elem)
                 (if (member elem '(or and))
                     elem `(or
                            ,@(mapcan (lambda (elem)
                                        (mapcar (lambda (elem)
                                                  `(like olp ',elem))
                                                (delve-show--format-vals :tags elem)))
                                      (rec elem))))) (cons 'and rest))))))
    (context :name context
             :transform
             ((`(context . ,rest)
               (rec `(or (tags ,@rest)
                         (olp ,@rest))))))
    (level :name level :aliases '(l d depth)
           :transform
           ((`(,(or 'level 'l 'd 'depth) . ,rest)
             (-tree-map (lambda (elem) (if (member elem '(or and)) elem `(= level ,(string-to-number elem)))) (cons 'and rest)))))
    (all :name all
         :transform
         ((`(all . ,rest)
           (rec `(or (context ,@rest)
                     (titles ,@rest))))))
    (query :name query
           :transform
           (((pred stringp) (delve-show--variations element))
            ('nil 'nil)
            ((pred symbolp) (delve-show--variations (symbol-name element)))))))

(defvar delve-show-default-predicate-boolean 'and)
(defcustom delve-show-default-predicate 'tags
  "Parser type for delve-show.
Can be any predicates listed in for `delve-show-predicates'.
E.g.
  'tags
  'all"
  :type 'symbol
  :group 'delve-show)

;;; Code:
(declare-function delve-show--query-string-to-sexp "ext:delve-show" (query) t)
(declare-function delve-show--transform-query "ext:delve-show" (query) t)
(declare-function delve-show--stringify-query "ext:delve-show" (query) t)
(fset 'delve-show--query-string-to-sexp
      (sexp-string--define-query-string-to-sexp-fn  "delve-show"))
(fset 'delve-show--transform-query (sexp-string--define-transform-query-fn "delve-show" :transform))

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

(defun delve-show--format-vals (type val)
  "Format expand VAL as a TYPE."
  (mapcar
   (lambda (ft)
    (format ft val))
  (condition-case err
      (--> (plist-get delve-show-data-type type)
           (plist-get (plist-get delve-show-data-types type) it)
           (plist-get it :formats))
    (error (message "make sure format is defined in %s in delve-show-data-types.\n%s" delve-show-data-type err) (signal (car err) (cdr err))))))

(defun delve-show--wrap-list (lt)
  "Wrap list LT to match output of `delve-show--string-to-sexp'."
  (if (listp lt)
      (mapcar (lambda (elem)
                (if (listp elem)
                    (delve-show--wrap-list elem)
                  (if (member elem '(or and))
                      elem
                    (list delve-show-default-predicate elem)))) lt)
    (list delve-show-default-predicate lt)))

(defun delve-show--join-clauses (&rest clauses)
  "Join sql CLAUSES appropriately. Taken from `org-roam-search--join-clauses'."
  (cl-reduce
   (lambda (joined-clauses clause)
     (if (and clause (not (string-empty-p clause)))
         (if (and joined-clauses (not (string-empty-p joined-clauses)))
             (string-join
              (list "(" joined-clauses ")"
                    "AND"
                    "(" clause ")")
              " ")
           clause)
       joined-clauses))
   clauses
   :initial-value nil))

(cl-defun delve-show--query-nodes (&key conditions filter-clause sort-clause limit)
  "Create LIMIT or `delve-show-max-results' `delve-zettel' nodes stored in the database matching CONDITIONS and FILTER-CLAUSE sorted by SORT-CLAUSE."
  (let* ((conditions-clause (if conditions
                                (car (emacsql-prepare `[,conditions]))))
         (constraint-clause (delve-show--join-clauses conditions-clause filter-clause))
         (where-clause (if constraint-clause
                           (concat "WHERE " constraint-clause)))
         (order-by-clause (pcase sort-clause  ;;[(desc love) (asc this)]]
                            ((pred vectorp)
                             (car (emacsql-prepare `[:order-by ,sort-clause])))
                            ((pred stringp)
                             (concat "ORDER BY " sort-clause))
                            (_ sort-clause)))
         (limit-clause (if (or limit delve-show-max-results)
                           (format "limit %d" (or limit delve-show-max-results))))
         (query (string-join
                 (list
                  "SELECT id, file, filetitle, level, todo, pos, priority,
           scheduled, deadline, title, properties, olp, atime,
           mtime, tags, aliases, refs FROM
           -- from clause
             (
             SELECT  nodes.id as id,  nodes.file as file,  nodes.level as level,
               nodes.todo as todo,   nodes.pos as pos,  nodes.priority as priority,
               nodes.scheduled as scheduled,  nodes.deadline as deadline,  nodes.title as title,
               nodes.properties as properties,  nodes.olp as olp,  files.atime as atime,
               files.title as filetitle,
               files.mtime as mtime,  '(' || group_concat(tags.tag, ' ') || ')' as tags, '(' || group_concat(aliases.alias, ' ') || ')' as aliases,
               '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
             FROM nodes
             LEFT JOIN files ON files.file = nodes.file
             LEFT JOIN tags ON tags.node_id = nodes.id
             LEFT JOIN aliases ON aliases.node_id = nodes.id
             LEFT JOIN refs ON refs.node_id = nodes.id
             GROUP BY nodes.id)
             -- end from clause"
                  where-clause
                  order-by-clause
                  limit-clause) "\n")))
    (delve-query-do-super-query query)))

;;;###autoload
(cl-defun delve-show-create-query (&optional (input 'nil) &key (include-titles 'nil) (tag-fuzzy 'nil) (title-fuzzy 'nil) filter-clause sort-clause limit)
  "Create delve query based on INPUT.
If INCLUDE-TITLES, search for titles as well.
If TAG-FUZZY, search tags fuzzyly.
If TITLE-FUZZY, search titles fuzzyly.
If SEXP, treat argument as input of `delve-show--string-to-sexp'.
FILTER-CLAUSE, SORT-CLAUSE, and LIMIT are arguments to `delve-show--query-nodes."
  (let* ((delve-show-default-predicate (if include-titles 'all 'tags))
         (delve-show-data-type (list :tags (if tag-fuzzy :fuzzy :exact) :title (if title-fuzzy :fuzzy :exact)))
         (search-terms (if (listp input)
                         (delve-show--wrap-list input)
                         (delve-show--query-string-to-sexp input)))
         (query (delve-show--transform-query search-terms)))
    (delve--query-create
     :info (format "Query for nodes matching %s"
                   (prin1-to-string (or search-terms 'All)))
     :fn (lambda ()
           (delve-show--query-nodes :conditions query :filter-clause filter-clause :sort-clause sort-clause :limit limit)))))

;;;###autoload
(cl-defun delve-show (&optional (input 'nil) &key (include-titles 'nil) (tag-fuzzy 'nil) (title-fuzzy 'nil) filter-clause sort-clause limit)
  "Open delve buffer based on INPUT.
If INCLUDE-TITLES, search for titles as well.
If TAG-FUZZY, search tags fuzzyly.
If TITLE-FUZZY, search titles fuzzyly.
FILTER-CLAUSE, SORT-CLAUSE, and LIMIT are arguments to `delve-show--query-nodes."
  (interactive)
  (let* ((query (delve-show-create-query input :include-titles include-titles :tag-fuzzy tag-fuzzy :title-fuzzy title-fuzzy :filter-clause filter-clause :sort-clause sort-clause :limit limit)))
    (switch-to-buffer (delve--new-buffer "Result Buffer" (list query)))))

;;;###autoload
(cl-defun delve-show-results (&optional (input 'nil) &key (include-titles 'nil) (tag-fuzzy 'nil) (title-fuzzy 'nil) filter-clause sort-clause limit)
  "Return query results based on INPUT.
If INCLUDE-TITLES, search for titles as well.
If TAG-FUZZY, search tags fuzzyly.
If TITLE-FUZZY, search titles fuzzyly.
FILTER-CLAUSE, SORT-CLAUSE, and LIMIT are arguments to `delve-show--query-nodes."
  (let* ((query (delve-show-create-query input :include-titles include-titles :tag-fuzzy tag-fuzzy :title-fuzzy title-fuzzy :filter-clause :sort-clause sort-clause :limit limit)))
    (-map #'delve--zettel-create (funcall (delve--query-fn query)))))

(provide 'delve-show)
;;; delve-show.el ends here
