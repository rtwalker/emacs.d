;;; pcmpl-cargo.el --- Pcomplete command completion for cargo -*- lexical-binding: t; -*-

;;; Commentary:
;; Uses `pcmpl-args' to add completion support for cargo.

;;; Code:

(require 'pcmpl-args)

(defun pcmpl-args-cargo-commands ()
  "Completion table of installed cargo commands."
  (pcmpl-args-cached 'cargo-commands t
    (with-temp-buffer
      (pcmpl-args-process-file "cargo" "--list")
      (goto-char (point-min))
      (let (cmds)
        (while (re-search-forward "^    \\([^ \n]+\\) *\\(.*\\)$" nil t)
          (push (list (match-string 1) (match-string 2))
                cmds))
        (when cmds
          (pcmpl-args-completion-table-with-annotations
           (nreverse cmds)
           `(metadata (category . cargo-command))))))))

(defun pcmpl-args-cargo-extract-argspecs (shell-command)
  "Like `pcmpl-args-extract-argspecs-from-shell-command'.
Strips the \"...\" markers that cargo's help output appends to
repeatable options (e.g. \"-v, --verbose...\") so that they do not end
up in the completed option names."
  (mapcar (lambda (spec)
            (if (and (eq (car spec) 'option)
                     (stringp (cadr spec)))
                (cons 'option
                      (cons (replace-regexp-in-string "\\.\\.\\." "" (cadr spec))
                            (cddr spec)))
              spec))
          (pcmpl-args-extract-argspecs-from-shell-command shell-command)))

(defun pcmpl-args-cargo-subparser (arguments argspecs seen)
  "Subparser for completing a cargo subcommand and its arguments."
  (let ((stub (pop arguments)))
    (push (list :name 0
                :stub stub
                :values (plist-get (car seen) :values)
                :action `("CARGO-COMMAND" (:eval (pcmpl-args-cargo-commands))))
          seen)
    (if (null arguments)
        (list arguments argspecs seen)
      (setq argspecs
            (when (string-match "\\`[-_[:alnum:]]+\\'" stub)
              (ignore-errors
                (pcmpl-args-cargo-extract-argspecs
                 (concat "cargo " (shell-quote-argument stub) " --help")))))
      (setq argspecs
            (append argspecs
                    (cond ((equal stub "help")
                           `((argument * (("CARGO-COMMAND"
                                           (:eval (pcmpl-args-cargo-commands))))))
                           (t `((argument * (("FILE" t)))))))))
      (list arguments (pcmpl-args-make-argspecs argspecs) seen))))

(defun pcomplete/cargo ()
  (pcmpl-args-pcomplete
   (pcmpl-args-cached 'cargo t
     (pcmpl-args-make-argspecs
      (append
       (pcmpl-args-cargo-extract-argspecs "cargo --help")
       `((argument 0 (("CARGO-COMMAND" nil))
                   :subparser pcmpl-args-cargo-subparser)))))))

(provide 'pcmpl-cargo)
;;; pcmpl-cargo.el ends here
