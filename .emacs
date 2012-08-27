;;=====Basic Setting, no plugins are needed=======;;
;;Self information
(setq user-mail-address "harrifeng@gmail.com")
(setq user-full-name    "harrifeng")

;;Remove Start-up Display
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;;Remove scroll bar
(customize-set-variable 'scroll-bar-mode nil)

;;Remove tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;;No bell and flash
(setq ring-bell-function 'ignore)
(blink-cursor-mode -1)

;;When on one parentheses, move to the other one
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;;use y and n instead of yes and no
(fset 'yes-or-no-p 'y-or-n-p)

;;use minibuffer recusively
(setq enable-recursive-minibuffers t)

;;Save the cursor's previous position
(require 'saveplace)
(setq-default save-place t)

;;Delete the CR and the end of the line when Ctrl+k at beginning
(setq-default kill-whole-line t)

;;Big king ring
(setq kill-ring-max 200)

;; show column number
(setq column-number-mode t)

;;80 is the column limit
(setq default-fill-column 80)
(setq-default auto-fill-function 'do-auto-fill)

;;if kill content are the same, ignore them.
(setq kill-do-not-save-duplicates t)

;;Stop scroll-bar
(setq scroll-step 0
      scroll-margin 0
      scroll-conservatively 10000)

;;Cmd is used for meta for MAC
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;;=====Key Binding=======;;
(global-set-key "\C-w"          'backward-kill-word)
(global-set-key "\C-x\C-k"      'kill-region)
(global-set-key "\C-o"          'other-window)
(global-set-key [C-return]      'delete-other-windows)
(global-set-key [C-f11]         'toggle-tool-bar-mode-from-frame)
(global-set-key [(meta ?/)]     'hippie-expand)
(global-set-key "\C-x\C-m"      'execute-extended-command)
(global-set-key "\C-c\C-m"      'execute-extended-command)

;;Same behavior with BASH
(global-set-key "\C-h"          'backward-delete-char-untabify)
(defun backward-kill-line (arg) (interactive "p") (kill-line 0) )
;; if you have to use Ctrl+U, you have to use ESC instead
(global-set-key (kbd "C-u") 'backward-kill-line)


;;ctrl space is for sogou input method
(global-set-key [(control space)] nil)
(global-set-key [(control \,)] 'set-mark-command)

;;mac also use this mapping
(define-key global-map (kbd "C-,") 'set-mark-command)

;;eshell
(defun eshell/cls ()
"Clears the shell buffer ala Unix's clear or DOS' cls"
(interactive)
;; the shell prompts are read-only, so clear that for the duration
(let ((inhibit-read-only t))
;; simply delete the region
(delete-region (point-min) (point-max))))

;; Alt+; as comment advanced;
(defun qiang-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))(global-set-key "\M-;" 'qiang-comment-dwim-line)

;;set transparent and use f4 to control it
(global-set-key [(f4)] 'loop-alpha)
(setq alpha-list '((100 100) (95 65) (70 55)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))                ;; head value will set to
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))


    (setq alpha-list (cdr (append alpha-list (list h))))
    )
)

(when (string-equal system-type "windows-nt")
  (set-face-attribute
   'default nil :font "Consolas 11")
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
              charset
              (font-spec :family "Microsoft Yahei" :size 14)))
            ;;(font-spec :family "SimSun" :size 14)))))

  (defconst my-emacs-path "c:/gitbox/Emacs/")
  (defconst my-python-path "c:/python26/")
  (defconst my-git-path "c:/Program Files/Git/bin/")

  (when (string-match system-name "sh-rd-hfeng")
       (defconst my-git-path "c:/Program Files (x86)/Git/bin/")
       (set-face-attribute 'default nil :font "Consolas 12"))

  (defconst my-emacs-unix-command
    (concat
     my-emacs-path
     "unixTool"))

  (defconst my-emacs-unix-util
    (concat
     my-emacs-path
     "unixTool/UnxUtils/usr/local/wbin"))

  (defconst my-python-script-path
    (concat
     my-python-path
     "Scripts"))

  ;; example of setting env var named “path”,
  ;; by appending a new path to existing path
  (setenv "PATH"
      (concat
       my-emacs-unix-command ";"
       my-emacs-unix-util ";"
       my-git-path ";"
       my-python-path ";"
       my-python-script-path ";"
       (getenv "PATH")
       ))
  (setenv "HOME" my-emacs-path)

  (setq exec-path
    '(
      "C:/python26/"
      "C:/python27/"
      ))
  ;Maximum Windows
    (run-with-idle-timer 1 nil 'w32-send-sys-command 61488)
)

(when (string-equal system-type "darwin")
  (defconst my-emacs-path "~/")

  (set-face-attribute 'default nil :height 150)
  ;; Use consolas for latin-3 charset.
  (set-fontset-font "fontset-default" 'iso-8859-3 "-apple-Monaco-medium-normal-normal-*-14-*-*-*-*-m-0-iso10646-1")
  (set-fontset-font "fontset-default" 'unicode "-outline-微软雅黑-normal-normal-normal-sans-16-*-*-*-p-*-iso8859-1")
  (set-fontset-font "fontset-default" 'han "-apple-STSong-medium-normal-normal-*-16-*-*-*-*-p-0-iso10646-1")

  (menu-bar-mode -1)
  (setenv "PATH"
	  (concat
	   ;;/opt/local/bin is for mac port
	   "/opt/local/bin" ":"
	   "/usr/bin:/bin"  ":"
	   "/usr/sbin:/sbin" ":"
	   (getenv "PATH")
	   ))

  (setq default-frame-alist '((height . 40)
			      (width . 150) (menu-bar-lines . 20) 
			      (tool-bar-lines . 0)))

  (global-set-key [(f5)] 'ns-toggle-fullscreen)
  )

;;UTF-8 Setting
(setq current-language-environment "UTF-8")
(setq default-input-method "chinese-py")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;packages server:marmalade
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

;;=====Advanced part, need additional plugins=======;;

;use only for emacs24
;; (load-theme 'tango-dark)
(load-theme 'sanityinc-solarized-dark t)

;;Open Recent File History
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;;python-mode
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(require 'tramp)
;; tramp setting password keep time
(setq password-cache-expiry 200)

;; python-pep8 also need tramp setting 
(require 'python-pep8)


(defconst my-emacs-yasnippet-path
  (concat
   my-emacs-path
   ".emacs.d/elpa/yasnippet-0.6.1/snippets/"))

(require 'yasnippet)
(yas/initialize)
(yas/load-directory my-emacs-yasnippet-path)

(setq mmm-global-mode 't)
(setq yas/prompt-functions '(yas/dropdown-prompt yas/x-prompt))


(desktop-save-mode 1)

;; Unused setting, maybe useful in future
 ;'(custom-safe-themes (quote ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 ;'(scroll-bar-mode nil))
