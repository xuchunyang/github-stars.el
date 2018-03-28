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

(defun github-stars--read-link-header ()
  (let* ((link (cdr (assoc "Link" ghub-response-headers)))
         (links (and link
                     (mapcar (lambda (elt) (split-string elt "; "))
                             (split-string link ","))))
         next last first prev
         match)
    (when links
      (pcase-dolist (`(,target ,rel) links)
        (when (string-match "[?&]page=\\([^&>]+\\)" target)
          (setq match (match-string 1 target))
          (pcase rel
            ("rel=\"next\""  (setq next match))
            ("rel=\"last\""  (setq last match))
            ("rel=\"first\"" (setq first match))
            ("rel=\"prev\""  (setq prev match)))))
      `((next  . ,next)
        (last  . ,last)
        (first . ,first)
        (prev  . ,prev)))))

(defun github-stars--report-progress ()
  (let-alist (github-stars--read-link-header)
    (if .next
        (let ((message-log-max nil))
          (message "github-stars: (%s) Fetching your github stars [%s/%s]..."
                   (format-time-string "%H:%M:%S")
                   .next .last))
      (message nil))))

(defun github-stars--read-response (status)
  (let ((list (ghub--read-json-payload status)))
    (prog1 (mapcar (lambda (alist)
                     (let-alist alist
                       (list (cons 'starred-at  .starred_at)
                             (cons 'owner/name  .repo.full_name)
                             (cons 'owner       .repo.owner.login)
                             (cons 'name        .repo.name)
                             (cons 'url         .repo.html_url)
                             (cons 'description .repo.description)
                             (cons 'language    .repo.language))))
                   list)
      (github-stars--report-progress))))

(defvar github-stars nil)

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

;; https://emacs.stackexchange.com/questions/31448/report-duplicates-in-a-list
(defun github-stars--find-duplicates (list)
  (let ((table (make-hash-table :test #'equal))
        result)
    (dolist (x list)
      (cl-incf (gethash x table 0)))
    (maphash (lambda (key value)
               (when (> value 1)
                 (push key result)))
             table)
    result))

(defun github-stars--names-uniquify ()
  (let* ((names (map-apply
                 (lambda (_ alist)
                   (let-alist alist .name))
                 (github-stars)))
         (dups (github-stars--find-duplicates names)))
    (map-apply
     (lambda (key alist)
       (let-alist alist
         (if (member .name dups)
             (cons (concat .name "\\" .owner) key)
           (cons .name key))))
     (github-stars))))

(defun github-stars--completing-read ()
  (let* ((alist (github-stars-repos-uniquify))
         (uniquified (completing-read "Browse Github Star: " alist nil t)))
    (cdr (assoc uniquified alist))))

;;;###autoload
(defun github-stars-browse-url (owner/name)
  "Prompt you for one of your github stars and open it in the web browser."
  (interactive (list (github-stars--completing-read)))
  (browse-url (concat "https://github.com/" owner/name)))

(provide 'github-stars)
;;; github-stars.el ends here
