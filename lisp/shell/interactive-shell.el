;; -----------------------------------------------------------------------------
;; INTERACTIVE-SHELL.EL --- Interactive sending commands to shell
;; -----------------------------------------------------------------------------
;; Filename:		INTERACTIVE-SHELL.EL
;; Description:		Interactive sending commands to shell
;; Author:		John P. Hilbert <jphilbert@gmail.com>
;; Maintainer:		John P. Hilbert <jphilbert@gmail.com>
;; Copyright (C)	2012, John P. Hilbert, all rights reserved.
;; Created:		2012-03-10 20:25:12
;; Version:		0.1
;; Last-Updated:	2012-03-10 20:25:12
;;           By:	John P. Hilbert
;; URL:			
;; Keywords:		shell
;; Compatibility:	GNU Emacs 23.2.1
;;
;; Features that might be required by this library:
;; <NONE>

;; !!!This file is NOT part of GNU Emacs!!!

;; -----------------------------------------------------------------------------
;; License
;; -----------------------------------------------------------------------------
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; -----------------------------------------------------------------------------
;; Commentary:
;; -----------------------------------------------------------------------------
;; Interactive sending commands to shell

;; This package is a collection of methods for sending shell commands from a
;;script fil to a shell.  It has the power to use multiple shell processes and
;;executables (i.e. MS Shell, PowerShell, etc.)
;;

;; -----------------------------------------------------------------------------
;; Installation:
;; -----------------------------------------------------------------------------
;; Put INTERACTIVE-SHELL.EL to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;;	(add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;	(require 'interactive-shell)
;;	

;; -----------------------------------------------------------------------------
;; Customize:
;; -----------------------------------------------------------------------------
;; <NONE>
;;
;; All of the above can customize by:
;;      M-x customize-group RET interactive-shell RET
;;

;; -----------------------------------------------------------------------------
;; Change log:
;; -----------------------------------------------------------------------------
;; 2012/03/10
;;      * First released.
;; 

;; -----------------------------------------------------------------------------
;; Acknowledgments:
;; -----------------------------------------------------------------------------
;; This was mainly a modification of essh.el (Author currently unknown) which
;;  supposedly was borrowed from ESS.

;; -----------------------------------------------------------------------------
;; TODO:
;; -----------------------------------------------------------------------------
;; 

;; -----------------------------------------------------------------------------
;; Require
;; -----------------------------------------------------------------------------
;; <NONE>

;; -----------------------------------------------------------------------------
;; Code:
;; -----------------------------------------------------------------------------


(defvar current-shell-buffer nil
  "This buffer's associated shell.  Used only for shell scripting modes such that the mode may pipe (evaluate) code in the shell")
(make-variable-buffer-local 'current-shell-buffer)

(defvar shell-buffer-search-string "*shell"
  "Search string for shells.  This is useful particularly when using multiple shell programs and the user wishes to limit the list of options.")
(make-variable-buffer-local 'shell-buffer-search-string)

;; --------------------------------------------------------------------------
;; Shell Buffer Setting / Getting Methods
;; --------------------------------------------------------------------------
(defun process-shell ()
  "Returns a list with existing shell process."
  (interactive)
  (mapcar 'process-name 
          (remove-if-not '(lambda (arg)
                            (string-match "shell"
                                          (prin1-to-string arg)))
                         (process-list))))

(defun shell-buffer-choose ()
  "Sets and returns which process buffer to use."
  (interactive)
  (let (outpr
        (cbuf (current-buffer))
        (shellist (process-shell))
        (shell-buffer-list
         (remove-if-not '(lambda (arg)
                           (string-match shell-buffer-search-string arg))
                        (mapcar '(lambda (arg)
                                   (buffer-name
                                    (process-buffer
                                     (get-process arg))))
                                (process-shell)))))
    
    (cond ((eq (length shell-buffer-list) 0)
           ;; I don't like this chunk
           ;; maybe start shell and run this function again
           (shell)
           (switch-to-buffer cbuf)
           (setq outpr (buffer-name
                        (process-buffer
                         (get-process "shell"))))
           (sleep-for 0.5))
          
          ((eq (length shell-buffer-list) 1)
           (setq outpr (car shell-buffer-list)))
          
          ((> (length shell-buffer-list) 1)
           (setq outpr (completing-read "Send code to: "
                                        shell-buffer-list nil t))))
    
    (setq current-shell-buffer outpr)
    outpr))

(defun get-shell-buffer ()
  "Either gets the current-shell-buffer or ask for it."
  (interactive)
  (unless current-shell-buffer
    (shell-buffer-choose))
  current-shell-buffer)

(defun shell-new ()
  "Creates a new shell buffer"
  (interactive)
  (shell (generate-new-buffer-name "*shell*")))

;; --------------------------------------------------------------------------
;; Evaluation Methods
;; --------------------------------------------------------------------------
(defun shell-eval-command (sbuffer command)
  "Evaluates a single command into the shell process.  This is the root function for all shell-eval- methods."
  (let ((sprocess (get-buffer-process sbuffer)))
    (setq command (concat command "\n"))
    (accept-process-output sprocess 0 10)
    (with-current-buffer sbuffer
      (end-of-buffer) ;point is not seen being moved (unless sbuffer is focused)
      (insert command)                  ;pastes the command to shell
      (set-marker (process-mark sprocess) (point-max))
      (process-send-string sprocess command)
      ;; (accept-process-output sprocess 0 10)
      )))

(defun shell-eval-line (&optional step)
  "Evaluates the current line to the shell."
  (interactive ())
  (let ((com (buffer-substring (point-at-bol) (point-at-eol))))
    (if (> (length com) 0)
        (progn
          (shell-eval-command (get-shell-buffer) com)
          (when step (essh-next-code-line)))
      (message "No command in this line"))))

(defun shell-eval-line-and-step ()
  "Evaluates the current line to the shell and goes to next line."
  (interactive)
  (shell-eval-line t))

(defun shell-eval-region (start end)
  "Sends a region to the shell."
  (interactive "r")
  (let* ((com (buffer-substring start end))        ;reads command
         (lcom (length com))                       ;count chars
         (lastchar (substring com (1- lcom) lcom)) ;get last char
         pos)
    
    (unless (string-match "\n" lastchar) ;if last char is not "\n", then...
      (setq com (concat com "\n")))          ;...add it!
    
    (while (> (length com) 0)
      (setq pos (string-match "\n" com))
      (setq scom (substring com 0 pos))
      (setq com (substring com (min (length com) (1+ pos))))
      (shell-eval-command (get-shell-buffer) scom)
      ;; (accept-process-output sprocess 0 10) ; Why?
      ))) 

(defun shell-eval-buffer ()
  "Evaluate whole buffer to the shell."
  (interactive)
  (shell-eval-region (point-min) (point-max)))

(defun shell-eval-function ()
  "Evaluate function to the shell."
  (interactive)
  (let ((beg-end (essh-beg-end-of-function)))
    
    (if beg-end
        (save-excursion
          (goto-line (nth 0 beg-end))
          (setq origin (point-at-bol))
          (goto-line (nth 1 beg-end))
          (setq terminal (point-at-eol))
          (shell-eval-region origin terminal))
      (message "No function at current point."))))

(defun shell-eval-paragraph ()
  "Sends paragraph to shell"
  (interactive)
  (save-frame-excursion
   (mark-paragraph)
   (call-interactively 'shell-eval-region))
  (forward-paragraph))

;; --------------------------------------------------------------------------
;; Misc
;; --------------------------------------------------------------------------
(defun shell-cd-current-directory ()
  "Changes the shell working directory to the current buffer's one."
  (interactive)
  (let ((com (format "cd %s" (file-name-directory (buffer-file-name)))))
    (shell-eval-command (get-shell-buffer) com)))

;; function taken from ess package
(defun essh-next-code-line (&optional arg)
  "Move ARG lines of code forward (backward if ARG is negative).
Skips past all empty and comment lines.  Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((n 0)
        (inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc)); n=0 is success
      (while (and (= n 0)
                  (looking-at "\\s-*\\($\\|\\s<\\)"))
        (setq n (forward-line inc)))
      (setq arg (- arg inc)))
    n))

(defun essh-beg-end-of-function ()
  "Returns the lines where the function starts and ends. If there is no function at current line, it returns nil."
  (interactive)
  (let* ((curline (line-number-at-pos)) ;current line
         (curcom (buffer-substring (point-at-bol) (point-at-eol)))
         (pos (string-match "function" curcom))
         beg
         end)
    
    (save-excursion 
      (if pos 
          (progn
            (setq beg curline))
        (progn
          (while (not pos)
            (setq curline (1- curline)) ;current line
            (previous-line)                     ;go to previous line
            (setq curcom (buffer-substring (point-at-bol) (point-at-eol)))
            (setq pos (string-match "function" curcom)))
          (setq beg curline)))
      
      (beginning-of-line)
      (forward-list)                    ; move pointer to first matching brace
      (setq end (line-number-at-pos)))
    (if (and (<= (line-number-at-pos) end) (>= (line-number-at-pos) beg))
        (list beg end)
      nil)))

(provide 'interactive-shell)

;;; INTERACTIVE-SHELL.EL ends here
