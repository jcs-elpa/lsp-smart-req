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
  `((cc-mode                          . ( ccls
                                          lsp-clangd))
    (actionscript-mode                . lsp-actionscript)
    ((asm-mode fasm-mode masm-mode nasm-mode gas-mode)
     . lsp-asm)
    ((astro-mode astro-ts-mode)       . lsp-astro)
    ((awk-mode awk-ts-mode)           . lsp-awk)
    ((sh-script                       . lsp-bash))
    (beancount-mode                   . lsp-beancount)
    (protobuf-mode                    . lsp-bufls)
    (clojure-mode                     . lsp-clojure)
    (cmake-mode                       . lsp-cmake)
    (cobol-mode                       . lsp-cobol)
    (elixir-mode                      . lsp-credo)
    (crystal-mode                     . lsp-crystal)
    (csharp-mode                      . lsp-csharp)
    ((c3-mode c3-ts-mode)             . lsp-c3)
    (feature-mode                     . lsp-cucumber)
    (cypher-mode                      . lsp-cypher)
    (d-mode                           . lsp-d)
    (text-mode                        . ( lsp-grammarly
                                          lsp-ltex
                                          lsp-ltex-plus))
    (( autoconf-mode
       makefile-mode
       makefile-automake-mode
       makefile-gmake-mode
       makefile-makepp-mode
       makefile-bsdmake-mode
       makefile-imake-mode)           . lsp-autotools)
    ((gpr-mode gpr-ts-mode)           . lsp-ada)
    ((html-mode web-mode)             . (lsp-angular
                                         lsp-css))
    (ruby-mode                        . ( lsp-rubocop
                                          lsp-ruby-lsp
                                          lsp-ruby-syntax-tree
                                          lsp-typeprof))
    ((typespec-mode typespec-ts-mode) . lsp-typespec)
    (v-mode                           . lsp-v)
    (vala-mode                        . lsp-vala)
    (verilog-mode                     . lsp-verilog)
    ;;(. lsp-vetur)
    ((vhdl-mode vhdl-ts-mode)         . lsp-vhdl)
    ((vimrc-mode vimscript-ts-mode)   . lsp-vimscript)
    ;;(. lsp-volar)
    (wgsl-mode                        . lsp-wgsl)
    ((xml-mode nxml-mode)             . lsp-xml)
    (yaml-mode                        . (lsp-ansible lsp-yaml))
    (yang-mode                        . lsp-yang)
    (zig-mode                         . lsp-zig)
    ;; XXX: Must load.
    (lsp-mode                         . ( lsp-copilot)))
  "Rules to require necessary packages.

See the variable `lsp-client-packages' for all available packages."
  :type '(list symbol)
  :group 'lsp-smart-req)

;;
;;; Externals

(declare-function lsp--require-packages "ext:lsp-mode.el")

;;
;;; Core

(defun lsp-smart-req--featurep (features)
  "Check one of the FEATURES is loaded."
  (cond ((listp features)
         (cl-some (lambda (feat &rest _)
                    (lsp-smart-req--featurep feat))
                  features))
        (t
         (featurep features))))

(defun lsp-smart-req--require (pkgs)
  "Load the client packages PKGS."
  (cond ((listp pkgs)
         (dolist (pkg pkgs)
           (lsp-smart-req--require pkg)))
        (t
         (unless (featurep pkgs)
           (ignore-errors (require pkgs nil t))))))

(defun lsp-smart-req-load-all ()
  "Load all LSP modules."
  (interactive)
  (lsp-smart-req-load t))

(defun lsp-smart-req-load (&optional force)
  "Load the necessary modules.

If optional argument FORCE is non-nil, force to load the module."
  (dolist (rule lsp-smart-req-rules)
    (let ((features (car rule))
          (requires (cdr rule)))
      (when (or force
                (lsp-smart-req--featurep features))
        (lsp-smart-req--require requires)))))

(defun lsp-smart-req--packages (&rest _)
  "Override the function `lsp--require-packages'."
  (lsp-smart-req-load))

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
