;; -*- lexical-binding: t; -*-
;; Smoke test: `M-x eval-buffer' this file to load the package and run
;; its test suite in the normal ERT results buffer.

(let ((dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
  (add-to-list 'load-path dir))
(require 'skk-style-completion-framework)
(ert-run-tests-interactively "idiig/completion")
