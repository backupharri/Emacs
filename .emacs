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
(global-set-key "\C-q"          'cua-mode)
(global-set-key "\C-w"          'backward-kill-word)
(global-set-key "\C-x\C-k"      'kill-region)
(global-set-key "\C-o"          'other-window)
(global-set-key [C-return]      'delete-other-windows)
(global-set-key [C-f11]         'toggle-tool-bar-mode-from-frame)
(global-set-key [(meta ?/)]     'hippie-expand)
(global-set-key "\C-x\C-m"      'execute-extended-command)
(global-set-key "\C-c\C-m"      'execute-extended-command)
(global-set-key (kbd "C-x C-y") 'kill-ring-save)



(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode js-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))
 
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end))
                 (message "Copied line")
                 (list (line-beginning-position)
                       (line-beginning-position 2)))))
 
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
 
;; Copy line from point to the end, exclude the line break
(defun qiang-copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (point)
                  (line-end-position))
                  ;; (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
 


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

  "if you want to use Song, you have to use SimSun instead of Yahei"
  (set-face-attribute
   'default nil :font "Consolas 12") 
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
              charset
              (font-spec :family "Microsoft Yahei" :size 14)))

  (defconst my-emacs-path "c:/gitbox/Emacs/")
  (defconst my-python-path "c:/python26/")
  (defconst my-git-path "c:/Program Files/Git/bin/")

  (when (string-match system-name "sh-rd-hfeng")
    (defconst my-git-path "c:/Program Files (x86)/Git/bin/")
    "CJK language also have to set for a second time, otherwise cjk words
     can not show correctly"
    (set-face-attribute 'default nil :font "Anonymous Pro 13")
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
			charset
			(font-spec :family "Microsoft Yahei" :size 15))))

  (when (string-match system-name "hfeng-desktop")
    (defconst my-git-path "c:/Program Files (x86)/Git/bin/")
    "CJK language also have to set for a second time, otherwise cjk words
     can not show correctly"
    (set-face-attribute 'default nil :font "Anonymous Pro 13")
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
			charset
			(font-spec :family "Microsoft Yahei" :size 15))))
  
  (when (string-match system-name "hfeng-t60p")
    (defconst my-git-path "c:/Program Files/Git/bin/")
    "CJK language also have to set for a second time, otherwise cjk words
     can not show correctly"
    (set-face-attribute 'default nil :font "Consolas 12")
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
			charset
			(font-spec :family "Microsoft Yahei" :size 15))))

  (defconst my-emacs-unix-command
    (concat
     my-emacs-path
     "unixTool/Misc"))

  (defconst my-emacs-unix-gnuwin32
    (concat
     my-emacs-path
     "unixTool/GnuWin32/bin"))
  
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
       my-emacs-unix-gnuwin32 ";"
       my-emacs-unix-util ";"
       my-emacs-unix-command ";"
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
  ;; Be ware that the following emacs font setting can
  ;; only be used in brew build emacs with
  ;; > brew install emacs --cocoa --srge
  (set-face-attribute
   'default nil :font "Courier New 18")
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
              charset
              (font-spec :family "华文宋体" :size 16)))

  ;; (setenv "PATH"
  ;; 	  (concat
  ;; 	   "/usr/local/bin" ":" ;; /usr/local/bin is for homebrew
  ;; 	   "/usr/bin:/bin"  ":"
  ;; 	   "/usr/sbin:/sbin" ":"
  ;; 	   (getenv "PATH")
  ;; 	   ))
  ;; (setq exec-path
  ;;   '(
  ;;     "/usr/local/bin"
  ;;     "/usr/bin"
  ;;     ))
  
  (let (
        (mypaths
         '(
	   "/bin"
	   "/sbin"
	   "/usr"
	   "/usr/bin"
	   "/usr/local/bin"
	   "/usr/sbin"
           ) )
        )

    (setenv "PATH" (mapconcat 'identity mypaths ":") )
    (setq exec-path (append mypaths (list "." exec-directory)) )
    ) 

  (setq default-frame-alist '((height . 40)
			      (width . 120) (menu-bar-lines . 20) 
			      (tool-bar-lines . 0)))

  (global-set-key [(f5)] 'ns-toggle-fullscreen)
  )

(when (string-equal system-type "gnu/linux")
  (defconst my-emacs-path "~/")

  (set-face-attribute
   'default nil :font "Courier 10 pitch 12")
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
              charset
              (font-spec :family "Yahei Consolas Hybrid" :size 15)))

  (setenv "PATH"
	  (concat
	   "/usr/bin:"  ":"
	   "/bin:"      ":"
	   "/usr/sbin:" ":"
	   "/sbin:"     ":"
	   (getenv "PATH")
	   ))

  (setq default-frame-alist '((height . 40)
			      (width . 150) (menu-bar-lines . 20) 
			      (tool-bar-lines . 0)))
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
(load-theme 'sanityinc-tomorrow-night t)

;;Open Recent File History
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;;python-mode
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(require 'tramp)
;; tramp setting password keep time
(setq password-cache-expiry 6000)

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


(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun wcy-shell-mode-auto-rename-buffer (text)
  (if (eq major-mode 'shell-mode)
      (rename-buffer  (concat "shell: " default-directory) t)))
(add-hook 'comint-output-filter-functions 'wcy-shell-mode-auto-rename-buffer)

(require 'session)
(add-hook 'after-init-hook
	  'session-initialize)

;; I don't want to use org-mode's auto type subscript.
;; only setting this is not enough, you also
;; have to set '#+OPTIONS:^:{}' at the beginning
;; of the org file, With this setting, 'a_b' will
;; not be interpreted as a subscript, but 'a_{b}' will.
(setq org-export-with-sub-superscripts nil)

;; When you have made some personal keyboard shortcuts in
;; emacs using global-set-key, both major modes and minor
;; modes will override those if it uses the same keys.
;; This is because major mode and minor mode's keymaps
;; have priority over global keymaps.
(add-hook 'org-mode-hook
	  (lambda()
	    (define-key org-mode-map (kbd "C-,") 'set-mark-command)
	    ))

; for mysql
;; show output on windows in buffer
(setq sql-mysql-options '("-C" "-t" "-f" "-n"))

;; truncate lines for long tables
(add-hook 'sql-interactive-mode-hook
(function (lambda ()
(setq truncate-lines t))))

(setq auto-mode-alist 
(append 
(list 
;; insert entries for other modes here if needed. 
(cons "\\.sq$" 'sql-mode)) 
auto-mode-alist))
(add-hook 'sql-mode-hook 'font-lock-mode) 

;;clean all the buffer content 
(add-hook 'shell-mode-hook 'my-shell-mode-hook) 
(defun my-shell-mode-hook () 
  (local-set-key (kbd "C-x C-l") (lambda nil (interactive) (erase-buffer) (comint-send-input))) 
  ) 

;;set our default style for cc mode
(setq c-default-style "stroustrup"
      c-basic-offset 4)




;;-----------Unused setting-------------------
;(desktop-save-mode 1)

;;80 is the column limit
;; (setq default-fill-column 80)
;; (setq-default auto-fill-function 'do-auto-fill)


;; If you don't want to restart emacs to make the setting work, you can
;; M-x load-file ~/.emacs



;; C-c C-y to copy current line 
;; (global-set-key (kbd "C-x C-y") 'copy-lines)

;; (defun copy-lines(&optional arg)
;;   (interactive "p")
;;   (save-excursion
;;     (beginning-of-line)
;;     (set-mark (point))
;;     (next-line arg)
;;     (kill-ring-save (mark) (point))
;;     )
;;   )

;; If you want to list all the font available you can
;; M-x set-default-font
;; TAB to list fonts
;; END OF THE CONFIGURATION FILE
