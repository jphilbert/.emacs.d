;;; init.el ---  configuration entry point doing two things:
;;;				1) Sets various directories and file variables
;;;				2) Loads configuration files



(message "[Config] Initializing Emacs %s... " emacs-version)

(setq-default default-directory    "~/")
(setq user-full-name               "John P. Hilbert")
(setq user-mail-address            "jphilbert@gmail.com")

(defvar config-app-spell           "C:/bin/hunspell/bin/hunspell.exe")
(defvar config-app-spell-dict      "C:/bin/hunspell/share/hunspell/en_US.aff")
(defvar config-app-ripgrep         "C:/bin/ripgrep/rg.exe")
(defvar config-app-r               "r.exe")
(defvar config-app-python          "python.exe")
(defvar config-app-sql-oracle      "sqlplus.exe")
(defvar config-app-sql-ms          "sql-commandline.bat")

(defvar config-user
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER"))
  "The system user name")

(defvar config-machine
  (system-name)
  "The machine name")


;; ------------------------------------------------------------------------- ;;
;; Directory Structure
;; ------------------------------------------------------------------------- ;;
;; ./					--> config-dir
;;   init.el
;;	config/				
;;		core/			--> config-dir-core
;;		modes/			--> config-dir-mode
;;		custom.el			--> config-file-customization
;;	modules/
;;		elpa/			--> config-dir-elpa-modules
;;		user/			--> config-dir-user-modules
;;	save/			--> config-dir-save
;;		backups/			--> config-dir-backup
;;		autosaves/		--> config-dir-autosaves


(defvar config-dir
  (file-name-directory load-file-name)
  "The root dir of the Emacs configuration.")

(defvar config-dir-core
  (expand-file-name "config/core" config-dir)
  "The directory for core functionality.")

(defvar config-dir-mode
  (expand-file-name  "config/modes" config-dir)
  "The directory for specific mode configurations.")

(defvar config-file-customization
  (expand-file-name "config/custom.el" config-dir)
  "The file where emacs stores configuration from customize UI.")



(defvar config-dir-elpa-modules
  (expand-file-name "modules/elpa" config-dir)
  "The directory for packages pulled from an archive (ELPA, MELPA, etc.)")

(defvar config-dir-user-modules
  (expand-file-name "modules/user" config-dir)
  "The directory for packages that are manually installed.")



;; could use temporary-file-directory
(defvar config-dir-save
  (expand-file-name "save" config-dir))

(defvar config-dir-backup
  (expand-file-name "backups" config-dir-save))

;; could use temporary-file-directory
(defvar config-dir-autosave
  (expand-file-name "autosaves" config-dir-save))






;; ------------------------------------------------------------------------- ;;
;; Configuration Loading
;; ------------------------------------------------------------------------- ;;

;;	1) config-functions		--> additional functions useful for configuring
;;   2) config-packages		--> sets and loads ELPA packages
;;   3) config-custom		--> define custom variables
;;	4) config-frames		--> sets up frame parameters and fuctions 
;;	5) config-ui			--> configures UI
;;	6) config-editor		--> General editor parameters
;;	7) config-keybindings
;;	8) config-modes

(setq
 ;; Always load newest byte code
 load-prefer-newer			t
 
 ;; reduce the frequency of garbage collection by making it happen on
 ;; each 50MB of allocated data (the default is on every 0.76MB)
 gc-cons-threshold			50000000

 ;; warn when opening files bigger than 10MB
 large-file-warning-threshold	10000000)


;; add directories to Emacs's `load-path'
(add-to-list 'load-path config-dir-core)
(add-to-list 'load-path config-dir-mode)
(add-to-list 'load-path config-dir-user-modules)

(require 'config-functions)			; Additional Functions
(config-add-subfolders-to-load-path config-dir-user-modules)




;; ---------- Load Core Modules ---------- ;;
(message "[Config] Packages")
(require 'config-packages)			; Load ELPA Packages
(message "[Config] Frames")
(require 'config-frames)				; Default Frame Parameters
(message "[Config] UI")
(require 'config-ui)				; UI Configuration
(message "[Config] Editor")
(require 'config-editor)
(message "[Config] Keybindings")
(require 'config-keybindings)
(message "[Config] Modes")
(require 'config-modes)

(setq custom-file config-file-customization)

(message "[Config] Initialization Complete")
;;; init.el ends here
