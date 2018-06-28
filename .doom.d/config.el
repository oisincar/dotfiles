;;; private/default/config.el -*- lexical-binding: t; -*-

(load! "bindings")
;(load! bindings)
(setq doom-font (font-spec :family "Source Code Pro" :size 18))

;; Make search work using silver-searcher on windows.
;; (Not updating value for some reason??)
(setq counsel-ag-base-command "ag -zS --vimgrep --nocolor --nogroup %s")

;; (setq visual-line-mode t)
