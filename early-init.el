;;; early-init.el -*- lexical-binding: t; -*-
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

;; Disable the built-in package manager because Elpaca is used instead
(setq package-enable-at-startup nil)

(c/start-section "early" "UI Mode Customization")

(setq c/monospace-font "Roboto Mono"
      c/monospace-font-size 15)

;; Configure default frame settings before the first frame is shown
(setq default-frame-alist
      (append
       (list
        `(font . ,(concat c/monospace-font "-" (number-to-string c/monospace-font-size)))
        '(internal-border-width . 0)
        '(undecorated-round . t)
        '(left-fringe . 16)
        '(right-fringe . 16)
        '(menu-bar-lines . 0)
        '(tool-bar-lines . 0)
        '(vertical-scroll-bars)
        '(fullscreen . maximized))
       default-frame-alist))

(setq frame-inhibit-implied-resize t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tab-bar-mode 1)

(c/start-section "void" "Between early-init and init")
