;;; init.el ---  configuration entry point doing two things:
;;;				1) Sets various directories and file variables
;;;				2) Loads configuration files

(message "[Config] Initializing Emacs %s... " emacs-version)

(defvar config-root         (file-name-directory load-file-name)
  "The root dir of the Emacs configuration.")

(defvar config-file         (expand-file-name "config.yaml" config-root)
  "The configuration YAML file")


;; ------------------------------------------------------------------------- ;;
;; Functionality to retrieve info from CONFIG YAML file
;; ------------------------------------------------------------------------- ;;
(let* ;; Find and add YAML to load path
    ((elpa-dir
      (with-temp-buffer
        (insert-file-contents config-file)
        (keep-lines "elpa-modules" (point-min) (point-max)) 
        (when (string-match "elpa-modules:[[:space:]]+\"?\\([[:alnum:]/]*\\)"
                            (buffer-string))
          (match-string 1 (buffer-string)))))
     (yaml-dir
      (car (directory-files (expand-file-name elpa-dir config-root)
                            t "yaml-"))))
  (add-to-list 'load-path yaml-dir))

(require 'yaml)
(defun yaml-parse-file (file &rest args)
  (setq args
        (or args
            (list :object-type 'plist :sequence-type 'array)))
  (with-temp-buffer
    (insert-file-contents file)
    (apply #'yaml-parse-string (buffer-string) args)))

(defvar config-plist (yaml-parse-file config-file))

(defun config-get (&rest keys)
  (let (
        (value (seq-reduce #'plist-get keys config-plist)))
    (if (eq :config-paths (car keys))
        (expand-file-name value config-root)
      value)))

(defun config-add-to-load-path (dir &optional subfolders)
  "Add DIR and optionally sub-directories to the `load-path'.

By default, only DIR is added to the `load-path' however setting
SUBFOLDERS non-nil will recursively add sub-folders. Setting it
to 'only add sub-folders while excluding DIR."
  (unless (eq subfolders 'only)
    (add-to-list 'load-path dir))
  (when subfolders
    (dolist (f (directory-files dir t "^[^\.]"))
      (when (file-directory-p f)                   
        (config-add-to-load-path f t)))))


;; ------------------------------------------------------------------------- ;;
;; User Settings
;; ------------------------------------------------------------------------- ;;
(setq user-full-name               (config-get :user-info :full-name))
(setq user-mail-address            (config-get :user-info :mail-address))
(defvar config-user
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER"))
  "The system user name")
(defvar config-machine
  (system-name)
  "The machine name")


;; ------------------------------------------------------------------------- ;;
;; Directory Structure
;; ------------------------------------------------------------------------- ;;
(setq-default default-directory    (config-get :config-paths :default))

;; Core Functionality
(config-add-to-load-path           (config-get :config-paths :core))

;; Mode Configurations
(config-add-to-load-path           (config-get :config-paths :mode))

;; ELPA packages
(setq package-user-dir             (config-get :config-paths :elpa-modules))

;; User packages that are manually installed
(config-add-to-load-path           (config-get :config-paths :user-modules) t)


;; ------------------------------------------------------------------------- ;;
;; Settings to set prior to loading
;; ------------------------------------------------------------------------- ;;
;; Always load newest byte code
(setq load-prefer-newer             t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold             50000000)

;; warn when opening files bigger than 10MB
(setq large-file-warning-threshold  10000000)


;; ------------------------------------------------------------------------- ;;
;; Configuration Loading
;; ------------------------------------------------------------------------- ;;
;;	1) config-functions		--> additional functions useful for configuring
(message "[Config] Functions")
(require 'config-functions)

;;  2) config-packages		--> sets and loads ELPA packages
(message "[Config] Packages")
(require 'config-packages)

;;  3) config-custom		--> define custom variables

;;	4) config-frames		--> sets up frame parameters and functions 
(message "[Config] Frames")
(require 'config-frames)				

;;	5) config-ui			--> configures UI
(message "[Config] UI")
(require 'config-ui)

;;	6) config-editor		--> General editor parameters
(message "[Config] Editor")
(require 'config-editor)

;;	7) config-keybindings
(message "[Config] Keybindings")
(require 'config-keybindings)

;;	8) config-modes
(message "[Config] Modes")
(require 'config-modes)


;; The file where emacs stores configuration from customize UI
(setq custom-file
      (config-get :config-paths :custom-file))

(message "[Config] Initialization Complete")
;;; init.el ends here
