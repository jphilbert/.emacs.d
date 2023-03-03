;;; init.el ---  configuration entry point doing two things:
;;;				1) Sets various directories and file variables
;;;				2) Loads configuration files

(message "[Config] Initializing Emacs %s... " emacs-version)

;; ------------------------------------------------------------------------- ;;
;; Functionality to retrieve info from CONFIG YAML file
;; ------------------------------------------------------------------------- ;;
(defvar config-root         nil
  "The root dir of the Emacs configuration.")
(defvar config-file         nil
  "The configuration YAML file")
(defvar config-settings     nil
  "plist of general settings loaded from `config-file'.
See `config-load-settings'.")

;; Load and parse config yaml
(defun config-load-settings (&optional file)
  "Loads YAML FILE into plist `config-settings'.

If FILE is not given, `config-file' is used."
  (setq file (or file config-file))
  (with-temp-buffer
    (insert-file-contents file)
    (setq config-settings
          (apply #'yaml-parse-string
                 (buffer-string)
                 (list :object-type 'plist :sequence-type 'array)))))

;; Get data from config file
(defun config-get-setting (&rest keys)
  "Retrieve the value from the path of KEYS in `config-settings'.

Has the added effect of expanding the file name if the key is in
:config-paths. Also returns nil if not found."
  (let (
        (value (seq-reduce #'plist-get keys config-settings)))
    (if (and value (eq :config-paths (car keys)))
        (expand-file-name value config-root)
      value)))

(defalias 'config-get 'config-get-setting)

;; Get data from config file AND set variable
(defmacro setq-from-config (sym &rest keys)
  "Sets SYM from the path of KEYS if found in `config-settings'."
  `(setq ,sym (or (apply 'config-get-setting ',keys) ,sym)))

(defmacro setq-default-from-config (sym &rest keys)
  "Sets the default of SYM from the path of KEYS if found in `config-settings'."
  `(setq-default ,sym (or (apply 'config-get-setting ',keys) ,sym)))

;; Function to easily add directory to load-path
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

(defun add-to-load-path-from-config (&rest keys)
  "Adds the directory from the path of KEYS if found in `config-settings'."
  (let*
      ((subfolders (car (last keys)))
       (subfolders (and (not (keywordp subfolders)) subfolders))
       (keys (if subfolders (butlast keys) keys))
       (dir (apply 'config-get-setting keys)))
     (when dir
       (config-add-to-load-path dir subfolders))))


;; ------------------------------------------------------------------------- ;;
;; Fundamental Settings
;; ------------------------------------------------------------------------- ;;
(setq config-root         (file-name-directory load-file-name))
(setq config-file         (expand-file-name "config.yaml" config-root))

(let* 
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
(config-load-settings)

(setq-from-config user-full-name               :user-info :full-name)
(setq-from-config user-mail-address            :user-info :mail-address)
(defvar config-user
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER"))
  "The system user name")
(defvar config-machine
  (system-name)
  "The machine name")


;; ------------------------------------------------------------------------- ;;
;; Directory Structure
;; ------------------------------------------------------------------------- ;;
(setq-default-from-config default-directory    :config-paths :default)

;; Core Functionality
(add-to-load-path-from-config                   :config-paths :core)

;; Mode Configurations
(add-to-load-path-from-config                   :config-paths :mode)

;; ELPA packages
(setq-from-config       package-user-dir        :config-paths :elpa-modules)

;; User packages that are manually installed
(add-to-load-path-from-config                   :config-paths :user-modules t)


;; ------------------------------------------------------------------------- ;;
;; Settings to set prior to loading
;; ------------------------------------------------------------------------- ;;

(setq
;; Always load newest byte code
 load-prefer-newer             t

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
 gc-cons-threshold             50000000

;; warn when opening files bigger than 10MB
 large-file-warning-threshold  10000000)


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
(setq-from-config       custom-file         :config-paths :custom-file)

(message "[Config] Initialization Complete")
;;; init.el ends here
