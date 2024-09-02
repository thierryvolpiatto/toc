;;; toc.el --- Create TOC for org or markdown -*- lexical-binding: t -*-

;; Author: Thierry Volpiatto <thievol@posteo.net>
;; Copyright (C) 2024 Thierry Volpiatto, all rights reserved.
;; URL: https://github.com/thierryvolpiatto/toc

;; Compatibility: GNU Emacs 24.4+"
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.1
;; Keywords: convenience, files, editing, org, markdown

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

;;; Commentary:
;;
;; Create TOC in org or markdown buffers.  The TOC is created between
;; two specific headers, nothing else than TOC should be in this area.
;; NOTE: in org buffers the first org header serves as end header for
;; the TOC, so be sure to not have text between these two, it will be
;; erased when creating or updating TOC.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'imenu)

(defvar org-imenu-depth)

(defvar toc-header-org-start-regexp "^\\*.*:TOC:$")
(defvar toc-header-org-end-regexp   "^\\*")
(defvar toc-header-md-start-regexp  "^<!-- markdown-toc start -")
(defvar toc-header-md-end-regexp    "^<!-- markdown-toc end -->")
(defvar toc-unwanted-chars-regexp   "[^[:alnum:]_-]")

(defun toc--headers-regexp ()
  "Choose the TOC header regexps according to mode."
  (cond ((eq major-mode 'markdown-mode)
         (list toc-header-md-start-regexp
               toc-header-md-end-regexp))
        ((eq major-mode 'org-mode)
         (list toc-header-org-start-regexp
               toc-header-org-end-regexp))))

(defun toc--insert-elm (spcs key url)
  "Insert TOC element according to mode."
  (if (eq major-mode 'org-mode)
      (insert (format "%s- [[#%s][%s]]\n" spcs url key))
    (unless (string= key ".") ; md 1st elm is ("." . pos) in subalists.
      (insert (format "%s- [%s](#%s)\n" spcs key url)))))

(cl-defun toc-insert-toc (alist &optional (spaces 0))
  "Insert TOC elements and indent them as needed."
  (cl-loop with indent = (if (eq major-mode 'markdown-mode) 4 2)
           for (key . pos) in alist
           for spcs =  (make-string spaces ? )
           for url = (mapconcat
                      (lambda (x)
                        (downcase (replace-regexp-in-string
                                   toc-unwanted-chars-regexp "" x)))
                      (split-string key) "-")
           do (toc--insert-elm spcs key url)
           when (imenu--subalist-p (cons key pos))
           do (toc-insert-toc (if (eq major-mode 'markdown-mode)
                                  (cdr pos) pos)
                              (+ spaces indent))))

;;;###autoload
(defun toc-insert-headers-at-point ()
  "Insert TOC headers at point."
  (interactive)
  (if (eq major-mode 'org-mode)
      (insert "* Table of Contents :TOC:\n")
    (insert "<!-- markdown-toc start -->\n\n")
    (insert "<!-- markdown-toc end -->\n")))

;;;###autoload
(defun toc-toc ()
  "Create TOC in org or markdown buffers."
  (interactive)
  (let* ((org-imenu-depth 6)
         (alist (funcall imenu-create-index-function))
         (headers (toc--headers-regexp))
         (markdownp (eq major-mode 'markdown-mode))
         (initspaces 0)
         beg end)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (car headers))
        (forward-line 1)
        (setq beg (pos-bol)))
      (when (re-search-forward (cadr headers))
        (setq end (1- (pos-bol))))
      (delete-region beg end)
      (goto-char beg)
      (when markdownp
        (insert "**Table of Contents**\n\n"))
      (toc-insert-toc (cond (markdownp alist)
                            ((null (cdr alist))
                             (setq initspaces 2)
                             (cdar alist))
                            (t (cdr alist)))
                      initspaces))))

(provide 'toc)

;;; toc.el ends here
