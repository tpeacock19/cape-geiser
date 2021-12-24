;;; cape-geiser.el --- arx cape geiser configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Trey Peacock
;;
;; Author: Trey Peacock <http://github/tpeacock19>
;; Maintainer: Trey Peacock <git@treypeacock.com>
;; Created: December 23, 2021
;; Modified: December 23, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/tpeacock19/cape-geiser
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

(defvar cape-geiser--properties
  (list :annotation-function (lambda (_) " Geiser")
        :company-kind (lambda (_) 'text))
  "Completion extra properties for `cape-geiser'.")

(defvar-local cape-geiser--enabled-flag nil)

(defvar-local cape-geiser--autodoc-flag nil)

(defvar-local cape-geiser--completions nil)

(defun cape-geiser--candidates (prefix)
  (and (equal prefix (car cape-geiser--completions))
       (cdr cape-geiser--completions)))

(defun cape-geiser--doc (id)
  (ignore-errors
    (when (not (geiser-autodoc--inhibit))
      (let ((help (geiser-autodoc--autodoc `((,id 0)))))
        (and help (substring-no-properties help))))))

(defun cape-geiser--doc-buffer (id)
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

(defun cape-geiser--docstring (id)
  (let* ((module (geiser-eval--get-module))
         (symbol (make-symbol id))
         (ds (geiser-doc--get-docstring symbol module)))
    (and ds
         (listp ds)
         (concat (geiser-autodoc--str* (cdr (assoc "signature" ds)))
                 "\n\n"
                 (cdr (assoc "docstring" ds))))))

(defun cape-geiser--location (id)
  (ignore-errors
    (when (not (geiser-autodoc--inhibit))
      (let ((id (make-symbol id)))
        (condition-case nil
            (geiser-edit-module id 'noselect)
          (error (geiser-edit-symbol id 'noselect)))))))

(defun cape-geiser--prefix-at-point ()
  (ignore-errors
    (when (and (not (geiser-autodoc--inhibit)) cape-geiser--enabled-flag)
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
          (setq cape-geiser--completions (cons prefix cmps))
          prefix)))))

;;; Activation


(defun cape-geiser--setup ()
  (pcase major-mode
    ('geiser-repl-mode
     (setq cape-geiser--enabled-flag geiser-repl-cape-p))
    ('geiser-mode
     (setq cape-geiser--enabled-flag geiser-mode-cape-p))))

(add-hook 'geiser-repl-mode-hook #'cape-geiser--setup)
(add-hook 'geiser-mode-hook #'cape-geiser--setup)

(defun cape-geiser--inhibit-autodoc ()
  (when (setq cape-geiser--autodoc-flag geiser-autodoc-mode)
    (geiser-autodoc-mode -1)))

(defun cape-geiser--restore-autodoc (&optional ignored)
  (when cape-geiser--autodoc-flag
    (geiser-autodoc-mode 1)))

(defun cape-geiser--lookup (&optional str)
  (let* ((prefix (cape-geiser--prefix-at-point))
         (candidates (cape-geiser--candidates (or prefix str))))
    candidates))

;;;###autoload
(defun cape-geiser (&optional interactive)
  "Complete with Geiser at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (cape--interactive #'cape-geiser)
    (let ((bounds (cape--bounds 'word)))
      `(,(car bounds) ,(cdr bounds)
        ,(cape--table-with-properties
          (cape--cached-table (car bounds) (cdr bounds) #'cape-geiser--lookup 'substring)
          :category 'cape-geiser
          :sort t)
        :exclusive no
        :company-prefix-length (cdr-safe (cape-geiser--prefix-at-point))
        :company-docsig #'cape-geiser--doc
        :company-doc-buffer #'cape-geiser--doc-buffer
        :company-location #'cape-geiser--location
        :exit-function (lambda (x _status)
                         (cape-geiser--restore-autodoc))
        ,@cape-geiser--properties))))


(provide 'cape-geiser)
;;; cape-geiser.el ends here
