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

(defvar github-stars-github-token-scopes '()
  "The Github API scopes needed by github-stars.")

(defvar github-stars nil)

(defun github-stars--read-response (status)
  (let ((list (ghub--read-json-response status)))
    (mapcar (lambda (alist)
              (let-alist alist
                (list (cons 'starred-at  .starred_at)
                      (cons 'owner/name  .repo.full_name)
                      (cons 'owner       .repo.owner.login)
                      (cons 'name        .repo.name)
                      (cons 'url         .repo.html_url)
                      (cons 'description .repo.description)
                      (cons 'language    .repo.language))))
            list)))

(defun github-stars ()
  "Return hash table listing github stars."
  (unless github-stars
    (setq github-stars (make-hash-table :test #'equal))
    (dolist (alist (ghub-get "/user/starred" nil
                             :query '((per_page . "100"))
                             :headers '(("Accept" .
                                         "application/vnd.github.v3.star+json"))
                             :unpaginate t
                             :reader #'github-stars--read-response
                             :auth 'github-stars))
      (puthash (let-alist alist .owner/name) alist github-stars)))
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
                 (lambda (_ alist)
                   (let-alist alist .name))
                 (github-stars)))
         (dups (github-stars-find-duplicates names)))
    (map-apply
     (lambda (key alist)
       (let-alist alist
         (if (member .name dups)
             (cons (concat .name "\\" .owner) key)
           (cons .name key))))
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
