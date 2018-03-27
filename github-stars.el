;;; github-stars.el --- Browse your Github Stars     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/github-stars.el
;; Package-Requires: ((emacs "25.1") (ghub "2.0.0"))
;; Keywords: tools
;; Created: Tue, 27 Mar 2018 20:59:43 +0800

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

;; Browse your Github Stars.

;;; Code:

(require 'ghub)
(require 'let-alist)
(require 'map)
(require 'seq)

(defvar github-stars-github-token-scopes '()
  "The Github API scopes needed by github-stars.")

(defvar github-stars nil)

(defun github-stars ()
  "Return hash table listing github stars."
  (unless github-stars
    (setq github-stars (make-hash-table :test #'equal))
    (dolist (alist (ghub-get "/user/starred" nil
                             :headers '(("Accept" .
                                         "application/vnd.github.v3.star+json"))
                             :unpaginate t
                             :auth 'github-stars))
      (puthash (let-alist alist .repo.full_name) alist github-stars )))
  github-stars)

(defun github-stars-find-duplicates (list)
  (let ((table (make-hash-table :test #'equal))
        result)
    (dolist (x list)
      (cl-incf (gethash x table 0)))
    (maphash (lambda (key value)
               (when (> value 1)
                 (push key result)))
             table)
    result))

(defun github-stars-repos-uniquify ()
  (let* ((names (map-apply
                 (lambda (_key value)
                   (let-alist value .repo.name))
                 (github-stars)))
         (dups (github-stars-find-duplicates names)))
    (map-apply
     (lambda (_key value)
       (seq-let (owner/name owner name) (let-alist value
                                          (list .repo.full_name
                                                .repo.owner.login
                                                .repo.name))
         (if (member name dups)
             (cons (concat name "\\" owner) owner/name)
           (cons name owner/name))))
     (github-stars))))

(defun github-stars-repos-read ()
  (let* ((alist (github-stars-repos-uniquify))
         (uniquified (completing-read "Browse Github Star: " alist nil t)))
    (cdr (assoc uniquified alist))))

;;;###autoload
(defun github-stars-browse-url (repo)
  (interactive (list (github-stars-repos-read)))
  (browse-url (concat "https://github.com/" repo)))

(provide 'github-stars)
;;; github-stars.el ends here
