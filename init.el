;;; init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(c/start-section "init" "Start")

(defvar c/init-el-start-time (current-time) "Time when init.el was started")

(c/start-section "init" "elpaca")

;; Install Elpaca
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

(c/start-section "init" "elpaca-let")
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
       (c/start-section "init" "elpaca-repo")
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (c/start-section "init" "elpaca-build-repo")
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (c/start-section "init" "elpaca-autoloads")
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(c/start-section "init" "elpaca-use-package")

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(setq use-package-always-ensure t)

(c/start-section "init" "Install Literate Config")

(elpaca (literate-config :wait t :host github :repo "aaronjensen/emacs-literate-config" :protocol ssh))

(add-hook 'literate-config-before-section-hook
          (lambda (section)
            (c/start-section "config" section)))

(unless noninteractive
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              (let ((inhibit-message t))
                (message "early-init.el: %d ms, init.el: %d ms, config.el: %d ms, post: %d ms\nInitial: %d ms, Total: %d ms"
                         (* 1000 (float-time (time-subtract c/init-el-start-time c/early-init-el-start-time)))
                         (* 1000 (float-time (time-subtract c/config-el-start-time c/init-el-start-time)))
                         (* 1000 (float-time (time-subtract c/config-el-end-time c/config-el-start-time)))
                         (* 1000 (float-time (time-subtract (current-time) c/config-el-end-time)))
                         (* 1000 (float-time (time-subtract c/config-el-end-time c/early-init-el-start-time)))
                         (* 1000 (float-time (time-subtract (current-time) c/early-init-el-start-time)))))) -90))

(defvar c/config-el-start-time (current-time))

;;; Hooks
(define-error 'c/hook-error "Error in a startup hook" 'c/error)

(defmacro c/add-transient-hook (name hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        ;; Avoid `make-symbol' and `gensym' here because an interned symbol is
        ;; easier to debug in backtraces (and is visible to `describe-function')
        (fn name))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (c/unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

(defvar c/time-hooks nil)

(defun c/run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (condition-case-unless-debug e
      (if c/time-hooks
          (let ((time (current-time)))
	         (funcall hook)
            (let ((inhibit-message t)
                  (elapsed-time (* 1000 (float-time (time-subtract (current-time) time)))))
              (when (> elapsed-time 2)
                (message "%S • %4dms • %S" c/time-hooks elapsed-time hook))))
        (funcall hook))
    (error
     (signal 'c/hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)

(defun c/run-hooks (&rest hooks)
  "Run HOOKS (a list of hook variable symbols) with better error handling.
Is used as advice to replace `run-hooks'."
  (dolist (hook hooks)
    (condition-case-unless-debug e
        (run-hook-wrapped hook #'c/run-hook)
      (c/hook-error
       (unless debug-on-error
         (lwarn hook :error "Error running hook %S because: %s"
                (if (symbolp (cadr e))
                    (symbol-name (cadr e))
                  (cadr e))
                (caddr e)))
       (signal 'c/hook-error (cons hook (cdr e)))))))

(defun c/run-hook-on (hook-var trigger-hooks)
  "Configure HOOK-VAR to be invoked exactly once when any of the TRIGGER-HOOKS
are invoked *after* Emacs has initialized (to reduce false positives). Once
HOOK-VAR is triggered, it is reset to nil.

HOOK-VAR is a quoted hook.
TRIGGER-HOOK is a list of quoted hooks and/or sharp-quoted functions."
  (dolist (hook trigger-hooks)
    (let ((fn (intern (format "%s-init-on-%s-h" hook-var hook))))
      (fset
       fn (lambda (&rest _)
            ;; Only trigger this after Emacs has initialized.
            (when (and after-init-time
                       (or (daemonp)
                           ;; In some cases, hooks may be lexically unset to
                           ;; inhibit them during expensive batch operations on
                           ;; buffers (such as when processing buffers
                           ;; internally). In these cases we should assume this
                           ;; hook wasn't invoked interactively.
                           (and (boundp hook)
                                (symbol-value hook))))
              (let ((c/time-hooks hook-var))
                (c/run-hooks hook-var))
              (set hook-var nil))))
      (cond ((daemonp)
             ;; In a daemon session we don't need all these lazy loading
             ;; shenanigans. Just load everything immediately.
             (add-hook 'elpaca-after-init-hook fn 'append))
            ((eq hook 'find-file-hook)
             ;; Advise `after-find-file' instead of using `find-file-hook'
             ;; because the latter is triggered too late (after the file has
             ;; opened and modes are all set up).
             (advice-add 'after-find-file :before fn '((depth . -101))))
            ((add-hook hook fn -101)))
      fn)))

;; Produce more helpful (and visible) error messages from errors emitted from
;; hooks (particularly mode hooks, that usually go unnoticed otherwise.
(advice-add #'run-hooks :override #'c/run-hooks)

(defvar c/first-input-hook nil
  "Transient hooks run before the first user input.")
(put 'c/first-input-hook 'permanent-local t)

(defvar c/first-file-hook nil
  "Transient hooks run before the first interactively opened file.")
(put 'c/first-file-hook 'permanent-local t)

(defvar c/first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")
(put 'c/first-buffer-hook 'permanent-local t)

(defvar c/switch-buffer-hook nil
  "A list of hooks run after changing the current buffer.")

(defun c/run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'c/switch-buffer-hook)))

(defun c/init-switch-buffer-hook (&optional _)
  "Initialize user interface by applying its hooks.
These should be done as late as possible, as to avoid/minimize prematurely
triggering hooks during startup."

  (c/run-hook-on 'c/first-buffer-hook '(find-file-hook c/switch-buffer-hook))

  ;; Only run this once
  (remove-hook 'window-buffer-change-functions #'c/init-switch-buffer-hook))

;; Initialize `c/switch-buffer-hook'
(add-hook 'window-buffer-change-functions #'c/run-switch-buffer-hooks-h)
;; `window-buffer-change-functions' doesn't trigger for files visited via the server.
(add-hook 'server-visit-hook #'c/run-switch-buffer-hooks-h)

(unless noninteractive
  ;; Initialize UI as late as possible. `window-buffer-change-functions' runs
  ;; once, when the scratch/dashboard buffer is first displayed.
  (add-hook 'window-buffer-change-functions #'c/init-switch-buffer-hook 99)
  (c/run-hook-on 'c/first-input-hook '(pre-command-hook))
  (c/run-hook-on 'c/first-file-hook '(find-file-hook dired-initial-position-hook)))

(c/start-section "init" "Load Config")

(literate-config-init)

(defvar c/config-el-end-time (current-time))
