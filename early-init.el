;;; early-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

(defvar c/early-init-el-start-time (current-time) "Time when early-init.el was started")
(defvar c/section-split-time c/early-init-el-start-time)

(defun c/get-section-split-time ()
  "Return the time since the init or since the last time this was called."
  (let ((now (current-time)))
    (prog1
        (float-time (time-subtract now c/section-split-time))
      (setq c/section-split-time now))))

(defvar c/previous-section nil)

(defun c/start-section (file heading)
  "Print the split time for the previous heading."
  (when (and c/previous-section
             (not noninteractive))
    (let ((inhibit-message t)
          (time (* 1000 (c/get-section-split-time))))
      (when (> time 3)
        (message "%6s • %4dms • %s" (car c/previous-section) time (cdr c/previous-section)))))
  (setq c/previous-section (and file heading (cons file heading))))

(c/start-section "early" "Start")

;;; Misc
(set-language-environment "UTF-8")
;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;; Without this, Emacs will try to resize itself to a specific column size
(setq frame-inhibit-implied-resize t)

;; Remove "For information about GNU Emacs..." message at startup
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Shave seconds off startup time by starting the scratch buffer in
;; `fundamental-mode'
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(defvar c/frame-title-format "%b – Emacs"
  "Template for displaying the title bar of visible and iconified frame.")

(setq frame-title-format c/frame-title-format
      icon-title-format c/frame-title-format)

(c/start-section "early" "UI Mode Customization")

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

;; Disabled initial modes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)

(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Enable Tab Bars
(tab-bar-mode 1)

(setq c/monospace-font "Roboto Mono"
      c/monospace-font-size 15
      c/variable-pitch-font "Fira Sans"
      c/variable-pitch-font-size 15)

;; Configure default frame settings before the first frame is shown
(setq default-frame-alist
      (append
       (list
        `(font . ,(concat c/monospace-font "-" (number-to-string c/monospace-font-size)))
        `(font . ,(concat c/variable-pitch-font "-" (number-to-string c/variable-pitch-font-size)))
        '(menu-bar-lines . 0)
        '(tool-bar-lines . 0)
        '(internal-border-width . 0)
        '(undecorated-round . t)
        '(left-fringe . 16)
        '(right-fringe . 16)
        '(vertical-scroll-bars)
        '(horizontal-scroll-bars)
        '(fullscreen . maximized))
       default-frame-alist))

;; Disable the built-in package manager because Elpaca is used instead
(setq package-enable-at-startup nil)
;; (setq use-package-verbose t)
;; (setq use-package-minimum-reported-time 0.0000001)
;; (setq use-package-compute-statistics t)

(c/start-section "void" "Between early-init and init")
