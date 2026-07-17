;;; pcmpl-jj.el --- Pcomplete command completion for Jujutsu -*- lexical-binding: t; -*-

;;; Commentary:
;; Uses `pcmpl-args' to add completion support for Jujutsu (jj)
;; version control.

;;; Code:

(require 'pcmpl-args)

(defun pcmpl-args-jj-commands (&optional path)
  "Completion table of jj subcommands.
PATH is a list of parent command names, e.g. (\"bookmark\"); if
nil, the top-level commands are listed.  Returns nil if the
command named by PATH has no subcommands."
  ;; The result is stored in a list so that the absence of
  ;; subcommands is cached as well.
  (car (pcmpl-args-cached (cons 'jj-commands path) t
         (with-temp-buffer
           (apply #'pcmpl-args-process-file "jj" "help" path)
           (goto-char (point-min))
           (let (cmds)
             (when (re-search-forward "^Commands:" nil t)
               (while (re-search-forward
                       "^  \\([a-z][a-z-]*\\) +\\(.*\\)$"
                       (save-excursion
                         (re-search-forward "^[ \t]*$" nil t))
                       t)
                 (push (list (match-string 1) (match-string 2))
                       cmds)))
             (list
              (when cmds
                (pcmpl-args-completion-table-with-annotations
                 (nreverse cmds)
                 `(metadata (category . jj-command))))))))))

(defun pcmpl-args-jj-bookmarks ()
  "Return a list of jj bookmark names.
Includes local bookmarks and remote bookmarks in NAME@REMOTE
form, excluding the special \"git\" tracking remote."
  (pcmpl-args-cached 'jj-bookmarks t
    (ignore-errors
      (pcmpl-args-process-lines
       "jj" "bookmark" "list" "-T"
       "if(remote != \"git\", name ++ if(remote, \"@\" ++ remote) ++ \"\\n\")"))))

(defvar pcmpl-args-jj-hints
  '((".*=\\(REVSETS?\\|REVISIONS?\\|BOOKMARK\\(@REMOTE\\)?\\|NAMES\\)\\'"
     (:eval (pcmpl-args-jj-bookmarks))))
  "Hints for jj option arguments that accept revsets or bookmarks.
See `pcmpl-args-guess-completions-hints'.")

(defun pcmpl-args-jj-positional-argspecs (path)
  "Return argspecs for the positional arguments of the jj command PATH.
PATH is a list of command names, e.g. (\"bookmark\" \"delete\")."
  (cond ((equal path '("help"))
         `((argument * (("JJ-COMMAND" (:eval (pcmpl-args-jj-commands)))))))
        ;; Command groups, e.g. `jj bookmark' or `jj git remote'.
        ((pcmpl-args-jj-commands path)
         `((argument 0 (("JJ-COMMAND" nil))
                     :subparser ,(pcmpl-args-jj-subparser path))))
        ((and (member (car path) '("bookmark" "b"))
              (not (equal (cadr path) "create")))
         `((argument * (("BOOKMARK" (:eval (pcmpl-args-jj-bookmarks)))))))
        ((member (car path)
                 '("abandon" "duplicate" "edit" "new" "parallelize" "show"))
         `((argument * (("REVSET" (:eval (pcmpl-args-jj-bookmarks)))))))
        (t
         `((argument * (("FILE" t)))))))

(defun pcmpl-args-jj-subparser (path)
  "Return a subparser for the jj subcommand following PATH.
PATH is a list of parent command names; nil for the top level."
  (lambda (arguments argspecs seen)
    (let ((stub (pop arguments)))
      (push (list :name 0
                  :stub stub
                  :values (plist-get (car seen) :values)
                  :action `("JJ-COMMAND" (:eval (pcmpl-args-jj-commands ',path))))
            seen)
      (if (null arguments)
          (list arguments argspecs seen)
        (let ((subpath (append path (list stub))))
          (setq argspecs
                (when (string-match "\\`[-a-z]+\\'" stub)
                  (ignore-errors
                    (pcmpl-args-extract-argspecs-from-shell-command
                     (concat "jj help "
                             (mapconcat #'shell-quote-argument subpath " "))))))
          (setq argspecs
                (append argspecs
                        (pcmpl-args-jj-positional-argspecs subpath)))
          (list arguments
                (pcmpl-args-make-argspecs
                 argspecs :hints pcmpl-args-jj-hints)
                seen))))))

(defun pcomplete/jj ()
  (pcmpl-args-pcomplete
   (pcmpl-args-cached 'jj t
     (pcmpl-args-make-argspecs
      (append
       (pcmpl-args-extract-argspecs-from-shell-command "jj --help")
       `((argument 0 (("JJ-COMMAND" nil))
                   :subparser ,(pcmpl-args-jj-subparser nil))))
      :hints pcmpl-args-jj-hints))))

(provide 'pcmpl-jj)
;;; pcmpl-jj.el ends here
