;;; init.el -*- lexical-binding: t; -*-

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

(c/start-section "init" "Load Config")

(literate-config-init)

(defvar c/config-el-end-time (current-time))
