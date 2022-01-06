;;; geiser-capf.el --- arx cape geiser configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Trey Peacock
;;
;; Author: Trey Peacock <http://github/tpeacock19>
;; Maintainer: Trey Peacock <git@treypeacock.com>
;; Created: December 23, 2021
;; Modified: December 23, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/tpeacock19/geiser-capf
;; Package-Requires: ((emacs 29.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  arx cape geiser configuration
;;
;;; Code:

(require 'geiser-autodoc)
(require 'geiser-completion)
(require 'geiser-edit)
(require 'geiser-base)
(require 'geiser-doc)

(eval-when-compile (require 'cl-lib))

(geiser-custom--defcustom geiser-mode-cape-p t
  "Whether to use company-mode for completion, if available."
  :group 'geiser-mode
  :type 'boolean)

(geiser-custom--defcustom geiser-repl-cape-p t
  "Whether to use company-mode for completion, if available."
  :group 'geiser
  :type 'boolean)
;;; Helpers:

(defvar geiser-capf--properties
  (list :annotation-function (lambda (_) " Geiser")
        :company-kind (lambda (_) 'text))
  "Completion extra properties for `geiser-capf'.")

(defvar-local geiser-capf--enabled-flag nil)

(defvar-local geiser-capf--autodoc-flag nil)

(defvar-local geiser-capf--completions nil)

(defun geiser-capf--bounds (thing)
  "Return bounds of THING."
  (or (bounds-of-thing-at-point thing) (cons (point) (point))))

(defun geiser-capf--interactive (capf)
  "Complete with CAPF."
  (let ((completion-at-point-functions (list capf)))
    (or (completion-at-point) (user-error "%s: No completions" capf))))

(cl-defun geiser-capf--table-with-properties (table &key category (sort t) &allow-other-keys)
  "Create completion TABLE with properties.
CATEGORY is the optional completion category.
SORT should be nil to disable sorting."
  (if (or (not table) (and (not category) sort))
      table
    (let ((metadata `(metadata
                      ,@(and category `((category . ,category)))
                      ,@(and (not sort) '((display-sort-function . identity)
                                          (cycle-sort-function . identity))))))
      (lambda (str pred action)
        (if (eq action 'metadata)
            metadata
          (complete-with-action action table str pred))))))

(defun geiser-capf--input-valid-p (old-input new-input cmp)
  "Return non-nil if the NEW-INPUT is valid in comparison to OLD-INPUT.
The CMP argument determines how the new input is compared to the old input.
- never: Never treat the input as valid.
- prefix/nil: The old input is a prefix of the new input.
- equal: The old input is equal to the new input.
- substring: The old input is a substring of the new input."
  ;; Treat input as not changed if it contains space to allow
  ;; Orderless completion style filtering.
  (or (string-match-p "\\s-" new-input)
      (pcase-exhaustive cmp
        ('never nil)
        ((or 'prefix 'nil) (string-prefix-p old-input new-input))
        ('equal (equal old-input new-input))
        ('substring (string-match-p (regexp-quote old-input) new-input)))))

(defun geiser-capf--cached-table (beg end fun valid)
  "Create caching completion table.
BEG and END are the input bounds.
FUN is the function which computes the candidates.
VALID is the input comparator, see `geiser-capf--input-valid-p'."
  (let ((input 'init)
        (beg (copy-marker beg))
        (end (copy-marker end t))
        (table nil))
    (lambda (str pred action)
      (let ((new-input (buffer-substring-no-properties beg end)))
        (when (or (eq input 'init) (not (geiser-capf--input-valid-p input new-input valid)))
          ;; NOTE: We have to make sure that the completion table is interruptible.
          ;; An interruption should not happen between the setqs.
          (setq table (funcall fun new-input)
                input new-input)))
      (complete-with-action action table str pred))))

(defun geiser-capf--candidates (prefix)
  (and (equal prefix (car geiser-capf--completions))
       (cdr geiser-capf--completions)))

(defun geiser-capf--doc (id)
  (ignore-errors
    (when (not (geiser-autodoc--inhibit))
      (let ((help (geiser-autodoc--autodoc `((,id 0)))))
        (and help (substring-no-properties help))))))

(defun geiser-capf--doc-buffer (id)
  (let* ((impl geiser-impl--implementation)
         (module (geiser-eval--get-module))
         (symbol (make-symbol id))
         (ds (geiser-doc--get-docstring symbol module)))
    (if (or (not ds) (not (listp ds)))
        (progn
          (message "No documentation available for '%s'" symbol)
          nil)
      (with-current-buffer (get-buffer-create "*company-documentation*")
        (geiser-doc--render-docstring ds symbol module impl)
        (current-buffer)))))

(defun geiser-capf--docstring (id)
  (let* ((module (geiser-eval--get-module))
         (symbol (make-symbol id))
         (ds (geiser-doc--get-docstring symbol module)))
    (and ds
         (listp ds)
         (concat (geiser-autodoc--str* (cdr (assoc "signature" ds)))
                 "\n\n"
                 (cdr (assoc "docstring" ds))))))

(defun geiser-capf--location (id)
  (ignore-errors
    (when (not (geiser-autodoc--inhibit))
      (let ((id (make-symbol id)))
        (condition-case nil
            (geiser-edit-module id 'noselect)
          (error (geiser-edit-symbol id 'noselect)))))))

(defun geiser-capf--prefix-at-point ()
  (ignore-errors
    (when (and (not (geiser-autodoc--inhibit)) geiser-capf--enabled-flag)
      (if (nth 8 (syntax-ppss)) 'stop
        (let* ((prefix (and (looking-at-p "\\_>")
                            (geiser-completion--prefix nil)))
               (cmps1 (and prefix
                           (geiser-completion--complete prefix nil)))
               (cmps2 (and prefix
                           (geiser-completion--complete prefix t)))
               (mprefix (and (not cmps1) (not cmps2)
                             (geiser-completion--prefix t)))
               (cmps3 (and mprefix (geiser-completion--complete mprefix t)))
               (cmps (or cmps3 (append cmps1 cmps2)))
               (prefix (or mprefix prefix)))
          (setq geiser-capf--completions (cons prefix cmps))
          prefix)))))

;;; Activation

(defun geiser-capf--setup ()
  (pcase major-mode
    ('geiser-repl-mode
     (setq geiser-capf--enabled-flag geiser-repl-cape-p))
    ('geiser-mode
     (setq geiser-capf--enabled-flag geiser-mode-cape-p))))

(add-hook 'geiser-repl-mode-hook #'geiser-capf--setup)
(add-hook 'geiser-mode-hook #'geiser-capf--setup)

(defun geiser-capf--inhibit-autodoc ()
  (when (setq geiser-capf--autodoc-flag geiser-autodoc-mode)
    (geiser-autodoc-mode -1)))

(defun geiser-capf--restore-autodoc (&optional ignored)
  (when geiser-capf--autodoc-flag
    (geiser-autodoc-mode 1)))

(defun geiser-capf--lookup (&optional str)
  (let* ((prefix (geiser-capf--prefix-at-point))
         (candidates (geiser-capf--candidates (or prefix str))))
    candidates))

;;;###autoload
(defun geiser-capf (&optional interactive)
  "Complete with Geiser at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (geiser-capf--interactive #'geiser-capf)
    (let ((bounds (geiser-capf--bounds 'word)))
      `(,(car bounds) ,(cdr bounds)
        ,(geiser-capf--table-with-properties
          (geiser-capf--cached-table (car bounds) (cdr bounds) #'geiser-capf--lookup 'substring)
          :category 'geiser-capf
          :sort t)
        :exclusive no
        :company-prefix-length (cdr-safe (geiser-capf--prefix-at-point))
        :company-docsig #'geiser-capf--doc
        :company-doc-buffer #'geiser-capf--doc-buffer
        :company-location #'geiser-capf--location
        :exit-function (lambda (x _status)
                         (geiser-capf--restore-autodoc))
        ,@geiser-capf--properties))))


(provide 'geiser-capf)
;;; geiser-capf.el ends here
