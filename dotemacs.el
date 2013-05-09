; Shared Emacs Config

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

;; nxml mode for XML/HTML editing
(add-to-list 'auto-mode-alist '("\\.xml$"     . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.html$"    . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.htm$"     . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.shtml$"   . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"     . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.asp$"     . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.tpl.php$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"     . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.mak$"     . nxml-mode))

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
  (setq fill-column 78)
  (setq c-basic-offset 2)
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
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.knife$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.watchr$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

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

;; json mode
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

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

(require 'rvm)
(rvm-use-default) ;; use rvm’s default ruby for the current Emacs session

(load "soy-mode")

;; save org-mode time tracking between sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; snippets
(add-to-list 'load-path "~/apps/emacs/lib/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)

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
