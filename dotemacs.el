; Shared Emacs Config

;; Define package repositories
;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("marmalade" . "http://marmalade-repo.org/packages/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Add all subdirectories to load-path
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir shared-config-dir)
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;; no more backup files!
(setq make-backup-files nil)

(setq inhibit-startup-screen t)

;; tab stuff - default to 4 spaces as a tab
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; go mode
(require 'go-mode-autoloads)

;; nxml mode for XML/HTML editing
(add-to-list 'auto-mode-alist '("\\.xml$"      . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.html$"     . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.htm$"      . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.shtml$"    . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"      . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.asp$"      . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.tpl.php$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"      . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.mak$"      . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.mustache$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.launch$"   . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xacro$"    . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.gazebo$"   . nxml-mode))

;; terraform mode
(require 'terraform-mode)
(add-to-list 'auto-mode-alist '("\\.tf$" . terraform-mode))
(add-to-list 'auto-mode-alist '("\\.tfvars$" . terraform-mode))
(add-to-list 'auto-mode-alist '("\\.terragrunt$" . terraform-mode))

;; Google Stylesheets should use CSS mode
(add-to-list 'auto-mode-alist '("\\.gss$" . css-mode))

;; php-mode
;; not doing require b/c of this error:
;; http://stackoverflow.com/questions/898063/making-php-mode-compatible-with-emacs-23
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$"    . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$"    . php-mode))
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(defun wicked/php-mode-init ()
  "Set some buffer-local variables."
  (setq case-fold-search t)
  (setq indent-tabs-mode nil)
  ;; (setq tab-width 4)
  (setq fill-column 78)
  (setq c-basic-offset 4)
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'case-label 2)
  (c-set-offset 'arglist-close 0))
(add-hook 'php-mode-hook 'wicked/php-mode-init)

(defun go-to-shell ()
  (set-buffer "*eshell*"))

;; some useful (I think) key mappings
(global-set-key "\C-l" 'goto-line) ; [Ctrl]-[L]
(global-set-key "\C-c#" 'comment-region)
(global-set-key "\C-c3" 'uncomment-region)
(global-set-key "\C-cf" 'align-regexp)
(global-set-key "\C-cr" 'rename-buffer)
(global-set-key "\C-c\C-f" 'revert-buffer)
(global-set-key "\C-cr" 'rename-buffer)
(global-set-key "\C-ce" 'erase-buffer)
(global-set-key "\C-cs" '(set-buffer "*eshell*"))

;; try to make working in shells nicer
(setenv "PAGER" "/bin/cat")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; make buffer names unique
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

;; smooth scrolling!
(require 'smooth-scrolling)

;; load eshell if there weren't any command line args
(if (equal 1 (length command-line-args))
    (eshell))

;; use utf-8 by default
(prefer-coding-system 'utf-8)

;; use css mode for google stylesheets
(add-to-list 'auto-mode-alist '("\\.gss$" . css-mode))

;; ruby stuff
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.knife$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.watchr$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (custom-set-variables
			  '(ruby-deep-arglist 0)
              '(ruby-deep-indent-paren-style nil))
             ))

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook '(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;; (require 'cc-mode)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; suggestion from:
;; http://emacs-fu.blogspot.com/2010/01/rectangles-and-cua.html
(setq cua-enable-cua-keys nil) ;; only for rectangles
(cua-mode t)

;; cucumber
(load "cucumber-mode.el")

;; scroll bar on the right
(if (require 'sml-modeline nil 'noerror)    ;; use sml-modeline if available
  (progn 
    (sml-modeline-mode 1)                   ;; show buffer pos in the mode line
    (scroll-bar-mode -1))                   ;; turn off the scrollbar
  (scroll-bar-mode 1)                       ;; otherwise, show a scrollbar...
  (set-scroll-bar-mode 'right))             ;; ... on the right


(setq search-highlight t                 ; highlight when searching... 
  query-replace-highlight t)             ; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)            ; enable y/n answers to yes/no 

;; javascript mode
(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(setq espresso-indent-level 2)

;; json mode
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.json.erb$" . json-mode))
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; http://emacs-fu.blogspot.com/2010/02/interactive-replacement.html
(require 'iedit)
(define-key global-map (kbd "C-;") 'iedit-mode)

;; http://emacs-fu.blogspot.com/2009/07/stepping-through-your-window.html
(require 'winner)
(setq winner-dont-bind-my-keys t) ;; default bindings conflict with org-mode

(global-set-key (kbd "<C-s-left>") 'winner-undo)
(global-set-key (kbd "<C-s-right>") 'winner-redo)
(winner-mode t) ;; turn on the global minor mode

(defun open-as-root ()
  "Open the file in the current buffer as root using sudo and tramp"
  (interactive)
  (let (fname)
    (setq fname (buffer-file-name))
    (find-file (concat "/sudo::" fname))
    (message (concat "Now editing " fname " as root!"))
    )
  )

(unless (package-installed-p 'magit)
  (package-install 'magit))

(require 'magit)
(global-set-key "\C-cg" 'magit-status)
(setq magit-process-popup-time 0)

(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

(global-set-key "\C-cb" 'mo-git-blame-current)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(require 'dim-switch-window)

(require 'lorem-ipsum)

;; scala  mode
(require 'scala-mode-auto)

;; actionscript mode
(require 'actionscript-mode)
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

;; haml mode
(require 'haml-mode)

;; (add-hook 'eshell-mode-hook
;;    '(lambda nil
;;    (let ((path))
;;       (setq path "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/ruby/bin:/home/dylan/.rvm/bin:.")
;;     (setenv "PATH" path))
;;    (local-set-key "\C-u" 'eshell-kill-input))
;;  )
         
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; (require 'rvm)
;; (rvm-use-default) ;; use rvm’s default ruby for the current Emacs session
(if (file-exists-p "~/.rbenv/version")
    (progn
      (require 'rbenv)
      (global-rbenv-mode))
  nil)

(load "soy-mode")

;; save org-mode time tracking between sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; jinja2 mode
(require 'jinja2-mode)

;; https://gist.github.com/dgutov/1274520
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; Edit With Emacs Chrome extension
;; https://chrome.google.com/webstore/detail/ljobjlafonikaiipfkggjbhkghgicgoh

(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start))

;; protocol buffer support
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; use ibuffer
(defalias 'list-buffers 'ibuffer)
(setq ibuffer-expert t)

(defun touch ()
  "updates mtime on the file for the current buffer"
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
  (clear-visited-file-modtime))
(global-set-key "\C-ct" 'touch)

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))


;; java annotations indentation
(require 'java-mode-indent-annotations)
(defun my-java-mode-hook ()
  (java-mode-indent-annotations-setup))
(add-hook 'java-mode-hook' my-java-mode-hook)

;; https://github.com/purcell/exec-path-from-shell
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; arduino-mode
(require 'arduino-mode)

;; pretty-print json
(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
    )
  )
;; clojure-mode
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(unless (package-installed-p 'cider)
  (package-install 'cider))

(unless (package-installed-p 'company)
  (package-install 'company))
(add-hook 'after-init-hook 'global-company-mode)

(show-paren-mode 1)

;; robe ruby helper
;; (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
;;   (rvm-activate-corresponding-ruby))
(unless (package-installed-p 'robe)
  (progn
    (package-refresh-contents)
    (package-install 'robe)))
(add-hook 'ruby-mode-hook 'robe-mode)

;; http://stackoverflow.com/a/3669681
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;; less mode
(require 'less-css-mode)
;; (add-hook 'less-css-mode-hook

;; dockerfile mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; elixir
(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))

;; ensime
(use-package ensime
  :ensure t
  :pin melpa-stable)

(defun eshell-clear-buffer ()
  "Clear eshell"
  (interactive)
  (let ((inhibit-read-only t))
    (erase buffer)
    (eshell-send-input)))

;; powershell mode
(require 'powershell)
