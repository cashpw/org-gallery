;;; org-gallery.el --- Image gallery for org-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Created: August 11, 2024
;; Modified: August 11, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/cashweaver/org-gallery
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Image gallery for org-mode.
;;
;;  Example of a gallery:
;;
;;  * Gallery title :gallery:
;;  Gallery description.
;;  ** [[file:foo.jpg][Image title]]
;;  :PROPERTIES:
;;  :END:
;;  Image description.
;;
;;; Code:

(require 'org-extras)

(defgroup org-gallery nil
  "Options related to `org-gallery'."
  :group 'org
  :tag "Options in org-gallery"
  :link '(url-link :tag "Github" "https://github.com/cashpw/org-gallery"))

(defcustom org-gallery--tag "gallery"
  "Tag to designate a gallery."
  :type 'string
  :group 'org-gallery)

(defcustom org-gallery--property-prefix "GALLERY"
  "Prefix for all image properties."
  :type 'string
  :group 'org-gallery)

(defun org-gallery--prop-artist ()
  "Return image artist property."
  (concat org-gallery--property-prefix "_" "ARTIST"))

(defun org-gallery--get-prop (property &optional default pom)
  "Return value for PROPERTY at POM or (optional) DEFAULT if value is nil."
  (let ((property-value (org-entry-get pom property)))
    (if default
        (or property-value default)
      property-value)))

(defun org-gallery--get-description ()
  "Get description of gallery or image at point."
  (string-trim (or (org-extras-get-content-under-heading) "")))

(cl-defstruct org-gallery-gallery
  "A gallery."
  title
  description
  images)

(defcustom org-gallery--image-property-prefix "IMAGE"
  "Prefix for all image properties."
  :type 'string
  :group 'org-gallery)

(defun org-gallery--image-prop-artist ()
  "Return image artist property."
  (concat org-gallery--image-property-prefix "_" "ARTIST"))

(defun org-gallery--image-prop-source ()
  "Return image artist property."
  (concat org-gallery--image-property-prefix "_" "SOURCE"))

(cl-defstruct org-gallery-image
  "An image in the gallery."
  title
  file
  artist
  source
  description)

(defun org-gallery--decompose-link (link-string)
  "Return plist with link and description of LINK-STRING."
  (cond
   ((string-match-p "\\[\\[\\(.*\\)\\]\\[\\(.*\\)\\]\\]" link-string)
    (progn
      (string-match "\\[\\[\\(.*\\)\\]\\[\\(.*\\)\\]\\]" link-string)
      `(:link
        ,(match-string 1 link-string)
        :description ,(match-string 2 link-string))))
   ((string-match-p "\\[\\[\\(.*\\)\\]\\]" link-string)
    (progn
      (string-match "\\[\\[\\(.*\\)\\]\\]" link-string)
      `(:link ,(match-string 1 link-string))))
   (t
    '(:link nil :description nil))))

(defun org-gallery--get-link-link (link-string)
  "Extract link from LINK-STRING."
  (plist-get (org-gallery--decompose-link link-string) :link))

(defun org-gallery--get-link-description (link-string)
  "Extract description from LINK-STRING."
  (plist-get (org-gallery--decompose-link link-string) :description))

(cl-defun org-gallery--parse-image (&key artist)
  "Parse the org-mode image gallery at point.

Galleries are defined as a heading tagged with `org-gallery--tag'."
  (let* ((link (org-entry-get nil "ITEM"))
         (file (org-gallery--get-link-link link))
         (title (org-gallery--get-link-description link))
         (artist
          (or (org-gallery--get-prop (org-gallery--image-prop-artist)) artist))
         (source (org-gallery--get-prop (org-gallery--image-prop-source)))
         (description (org-gallery--get-description)))
    (make-org-gallery-image
     :title title
     :file file
     :artist artist
     :source source
     :description description)))

(defun org-gallery--parse ()
  "Parse gallery at EPOM."
  (let* ((sub-heading-level (1+ (org-outline-level)))
         (match (format "LEVEL=%d" sub-heading-level))
         ;; Speed up `org-entry-properties' (see `org-map-entries')
         (org-trust-scanner-tags t)
         (artist (org-gallery--get-prop (org-gallery--prop-artist))))
    (save-restriction
      (unless (org-at-heading-p)
        (org-previous-visible-heading 1))
      (org-narrow-to-subtree)
      (make-org-gallery-gallery
       :title (org-entry-get nil "ITEM")
       :description (org-gallery--get-description)
       :images
       (org-map-entries
        (lambda () (org-gallery--parse-image :artist artist)) match 'tree)))))

(provide 'org-gallery)
;;; org-gallery.el ends here
