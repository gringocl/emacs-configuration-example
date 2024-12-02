;;; early-init.el -*- lexical-binding: t; -*-

;; Disable the built-in package manager because Elpaca is used instead
(setq package-enable-at-startup nil)

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
        '(vertical-scroll-bars))
       default-frame-alist))

  (push '(menu-bar-lines . 0)   default-frame-alist)
  (push '(tool-bar-lines . 0)   default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tab-bar-mode 1)

;; Start in fullscreen
(push '(fullscreen . maximized) default-frame-alist)


