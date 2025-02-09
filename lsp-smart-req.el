;;; lsp-smart-req.el --- Require only the necessary modules  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/lsp-smart-req
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: internal lsp

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Require only the necessary modules.
;;

;;; Code:

(defgroup lsp-smart-req nil
  "Require only the necessary modules."
  :prefix "lsp-smart-req-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/lsp-smart-req"))

(defcustom lsp-smart-req-rules
  `((actionscript-mode . lsp-actionscript)
    (cc-mode . ccls)
    ((gpr-mode gpr-ts-mode) . lsp-ada))
  "Rules to require necessary packages.

See the variable `lsp-client-packages' for all available packages."
  :type '(list symbol)
  :group 'lsp-smart-req)

;;
;;; Externals

(declare-function lsp--require-packages "ext:lsp-mode.el")

;;
;;; Core

(defun lsp-smart-req--load-client-packages (pkgs)
  "Load the client packages."
  (cond ((listp pkgs)
         (dolist (pkg pkgs)
           (lsp-smart-req--load-client-packages pkg)))
        (t
         (unless (featurep package)
           (ignore-errors (require pkgs nil t))))))

(defun lsp-smart-req--packages (&rest _)
  "Override the function `lsp--require-packages'."
  )

(defun lsp-smart-req--enable ()
  "Enable `lsp-smart-req-mode'."
  (advice-add 'lsp--require-packages :override #'lsp-smart-req--packages))

(defun lsp-smart-req--disable ()
  "Disable `lsp-smart-req-mode'."
  (advice-remove 'lsp--require-packages #'lsp-smart-req--packages))

;;;###autoload
(define-minor-mode lsp-smart-req-mode
  "Minor mode `lsp-smart-req-mode'."
  :global t
  :require 'lsp-smart-req-mode
  :group 'lsp-smart-req
  (if lsp-smart-req-mode (lsp-smart-req--enable) (lsp-smart-req--disable)))

(provide 'lsp-smart-req)
;;; lsp-smart-req.el ends here
