;;; delve-show-test.el --- Tests for delve-show.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Natnael Kahssay
;;
;; Author: Natnael Kahssay <https://github.com/natask>
;; Maintainer: Natnael Kahssay <thisnkk@gmail.com>
;; Created: December 06, 2021
;; Modified: December 06, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/natask/delve-show-test
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for delve-show.el
;;
;;; Code:

(require 'buttercup)
(require 'delve-show)

(describe "delve-show--transform-query"
  (it "works"
    (expect
     (delve-show--transform-query '(tags "tag"))
     :to-equal
     '(and (or (like tags '"%tag%") (like tags '"%taging%") (like tags '"%tags%"))))))

(provide 'delve-show-test)
;;; delve-show-test.el ends here
