;;; lsp-smart-req.el --- Lazy load LSP packages  -*- lexical-binding: t; -*-

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
;; Lazy load LSP packages.
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
  `((cc-mode                             . ( ccls
                                             lsp-clangd))
    (actionscript-mode                   . lsp-actionscript)
    ((asm-mode fasm-mode masm-mode nasm-mode gas-mode)
     . lsp-asm)
    ((astro-mode astro-ts-mode)          . lsp-astro)
    ((awk-mode awk-ts-mode)              . lsp-awk)
    (sh-script                           . lsp-bash)
    (beancount-mode                      . lsp-beancount)
    (protobuf-mode                       . lsp-bufls)
    (clojure-mode                        . lsp-clojure)
    (cmake-mode                          . lsp-cmake)
    (cobol-mode                          . lsp-cobol)
    (elixir-mode                         . ( lsp-credo
                                             lsp-elixir))
    (crystal-mode                        . lsp-crystal)
    (csharp-mode                         . ( lsp-csharp
                                             lsp-roslyn))
    ((c3-mode c3-ts-mode)                . lsp-c3)
    (feature-mode                        . lsp-cucumber)
    (cypher-mode                         . lsp-cypher)
    (d-mode                              . lsp-d)
    (dart-mode                           . lsp-dart)
    (dhall-mode                          . lsp-dhall)
    (docker                              . ( lsp-docker
                                             lsp-dockerfile))
    (earthfile-mode                      . lsp-earthly)
    (elm-mode                            . lsp-elm)
    (erlang                              . lsp-erlang)
    (fennel-mode                         . lsp-fennel)
    ((f90-mode fortran-mode)             . lsp-fortran)
    (futhark-mode                        . lsp-futhark)
    (fsharp-mode                         . lsp-fsharp)
    (gdscript-mode                       . lsp-gdscript)
    (gleam-mode                          . lsp-gleam)
    ((shader-mode glsl-mode hlsl-mode)   . ( lsp-shader
                                             lsp-glsl))
    (go-mode                             . ( lsp-go
                                             lsp-golangci-lint))
    (graphql-mode                        . lsp-graphql)
    (groovy-mode                         . lsp-groovy)
    (hack-mode                           . lsp-hack)
    (haskell-mode                        . lsp-haskell)
    (haxe-mode                           . lsp-haxe)
    ((idris-mode idris2-mode)            . lsp-idris)
    (java-mode                           . lsp-java)
    ((jq-mode jq-ts-mode)                . lsp-jq)
    (json-mode                           . lsp-json)
    (kotlin-mode                         . lsp-kotlin)
    ((tex-mode latex)                    . ( lsp-latex
                                             lsp-tex))
    (lisp-mode                           . lsp-lisp)
    (text-mode                           . ( lsp-grammarly
                                             lsp-ltex
                                             lsp-ltex-plus))
    (lua-mode                            . lsp-lua)
    (magik-mode                          . lsp-magik)
    (markdown-mode                       . ( lsp-markdown
                                             lsp-marksman
                                             lsp-mdx
                                             lsp-remark))
    (matlab-mode                         . lsp-matlab)
    (meson-mode                          . lsp-meson)
    ((scala-mode scala-ts-mode)          . lsp-metals)
    (mint-mode                           . lsp-mint)
    (mojo                                . lsp-mojo)
    (move-mode                           . lsp-move)
    (sql                                 . ( lsp-sql
                                             lsp-sqls
                                             lsp-mssql))
    (nextflow-mode                       . lsp-nextflow)
    (nginx-mode                          . lsp-nginx)
    (nim-mode                            . lsp-nim)
    (nix-mode                            . lsp-nix)
    (nushell-mode                        . lsp-nushell)
    ((reason-mode caml-mode tuareg-mode) . lsp-ocaml)
    (scad-mode                           . lsp-openscad)
    ((opascal-mode pascal-mode)          . lsp-pascal)
    ((perl-mode cperl-mode)              . ( lsp-perl
                                             lsp-perlnavigator
                                             lsp-pls))
    (php-mode                            . lsp-php)
    (purescript-mode                     . lsp-purescript)
    (powershell                          . lsp-pwsh)
    (python                              . ( lsp-pyls
                                             lsp-pylsp
                                             lsp-pyright
                                             lsp-python-ms
                                             lsp-pylyzer
                                             lsp-ruff))
    (qml-mode                            . lsp-qml)
    (ess                                 . lsp-r)
    (racket-mode                         . lsp-racket)
    (robot-mode                          . lsp-rf)
    ((roc-mode roc-ts-mode)              . lsp-roc)
    (rust-mode                           . lsp-rust)
    (( autoconf-mode
       makefile-mode
       makefile-automake-mode
       makefile-gmake-mode
       makefile-makepp-mode
       makefile-bsdmake-mode
       makefile-imake-mode)              . lsp-autotools)
    ((gpr-mode gpr-ts-mode)              . lsp-ada)
    (ruby-mode                           . ( lsp-rubocop
                                             lsp-ruby-lsp
                                             lsp-ruby-syntax-tree
                                             lsp-solargraph
                                             lsp-sorbet
                                             lsp-steep
                                             lsp-typeprof))
    (solidity-mode                       . lsp-solidity)
    ((swift-mode swift-ts-mode)          . lsp-sourcekit)
    (terraform-mode                      . lsp-terraform)
    (tilt-mode                           . lsp-tilt)
    ((conf-toml-mode toml-ts-mode)       . lsp-toml)
    ((typespec-mode typespec-ts-mode)    . lsp-typespec)
    (scheme                              . lsp-ts-query)
    (ttcn3                               . lsp-ttcn3)
    (v-mode                              . lsp-v)
    (vala-mode                           . lsp-vala)
    (verilog-mode                        . lsp-verilog)
    ((vhdl-mode vhdl-ts-mode)            . lsp-vhdl)
    ((vimrc-mode vimscript-ts-mode)      . lsp-vimscript)
    (wgsl-mode                           . lsp-wgsl)
    ((xml-mode nxml-mode)                . lsp-xml)
    (yaml-mode                           . ( lsp-yaml
                                             lsp-ansible
                                             lsp-kubernetes-helm
                                             lsp-trunk))
    (yang-mode                           . lsp-yang)
    (zig-mode                            . lsp-zig)
    ;; Web development
    (( html-mode css-mode js-mode
       web-mode
       javascript-mode
       typescript-mode
       svelte-mode)
     . ( lsp-angular lsp-css lsp-emmet lsp-eslint lsp-javascript
         lsp-svelte lsp-tailwindcss lsp-vetur lsp-volar))
    ;; Multi-purpose
    (lsp-mode                            . ( lsp-copilot
                                             lsp-semgrep
                                             lsp-sonarlint)))
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
