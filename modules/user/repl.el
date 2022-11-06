(defvar-local repl-interactive-mode nil
  "The interactive mode used for REPL")

(defvar-local repl-buffer nil
  "Assigned interactive buffer for current script.")



;; ---------- Function Variables ---------- ;;
(defvar-local repl-function-create nil
  "Function for creating a REPL buffer

The function should create and display a REPL buffer however the
calling buffer should still have focus and be the topmost frame. If
successful it should return the REPL BUFFER otherwise nil.")

(defvar-local repl-function-set nil
  "Function for setting a REPL buffer

The function should set and return an existing REPL buffer or
return nil. Regardless the calling buffer should still retain focus.")

(defvar-local repl-function-eval nil
  "Function for evaluating depending on context.

It is expected that this automatically prints this to the
`repl-buffer'")

(defvar-local repl-function-eval-insert nil
  "Function for evaluating depending on context.

Unlike `repl-function-eval', this function prints its output
after the context executed. It may still utilize and print
`repl-buffer'")


;; ---------- Functions ---------- ;;
(defun repl-buffer-get ()
  (setq
   repl-buffer
   (cond
    ;; REPL Mode not active -> nil
    ((not repl-mode) nil)
    
    ;; Current buffer is a REPL -> pass through
    ((or (not repl-interactive-mode)
         (eq repl-interactive-mode major-mode))
     repl-buffer)
    
    ;; REPL buffer is set and alive -> pass through
    ((and repl-buffer
          (buffer-live-p (get-buffer repl-buffer)))
     repl-buffer)
    
    ;; Set a REPL -> return selection
    ((repl-buffer-set))
    
    ;; Create a REPL -> return new
    ((repl-buffer-create))

    ;; -> nil
    )))

(defun repl-buffer-show ()
  "Command for showing (i.e. raising) the `repl-buffer'.

This does not create the buffer if it does not exist."
  (when repl-buffer
    (buffer-goto-end repl-buffer)
    (frame-raise (frame-get repl-buffer))))

(defun repl-eval-post ()
  (repl-buffer-show)

  (let ((script-buffer (current-buffer)))
    (with-current-buffer repl-buffer
      (setq
       repl-buffer            script-buffer
       repl-interactive-mode  major-mode))))


;; ---------- Commands ---------- ;;
(defun repl-buffer-create ()
  "Create and sets a `repl-buffer' for the buffer"
  (interactive)
  (setq
   repl-buffer   
   (funcall repl-function-create)))

(defun repl-buffer-set ()
  "Set the `repl-buffer' for the buffer"
  (interactive)
  (setq
   repl-buffer
   (funcall repl-function-set)))

(defun repl-buffer-switch ()
  "Command for switching to (i.e. displaying) the `repl-buffer'.

This functions different depending on the mode and state of the
current buffer. If the current buffer is a REPL buffer:
    1) If `repl-buffer' is set and alive, switch to it.
    2) Otherwise, switch to the last selected buffer.

On the other hand, if the current buffer is a script file:
    1) Verify a REPL buffer exists, otherwise create one.
    2) Verify `repl-buffer' is set to an active REPL buffer. If only
       one exists, set it otherwise ask.
    3) Switch to `repl-buffer'. 
"
  (interactive)
  (if (repl-buffer-get)
      ;; (buffer-goto-end repl-buffer)
      (display-buffer repl-buffer)
    (frame-display-last)))

(defun repl-eval ()
  "Evaluate code and insert results into `repl-buffer'"
  (interactive)
  (when (repl-buffer-get)
    (funcall repl-function-eval)
    (repl-eval-post)))

(defun repl-eval-insert ()
  "Evaluate code and insert results into current buffer."
  (interactive)
  (when (repl-buffer-get)
    (funcall repl-function-eval-insert)
    (repl-eval-post)))

(define-minor-mode repl-mode
  "Toggles REPL-mode"  
  :keymap
  `(
    ;; ---------- Evaluation ----------
    ([(shift return)] .     repl-eval)
    ([(M-return)] .         repl-eval-insert)

    ;; ---------- Frame Switching ----------
    ;; SWITCH if assigned OR
    ;; SET and SWITCH OR
    ;; CREATE and SET and SWITCH
    ([f12] .                repl-buffer-switch)
    
    ;; CREATE and SET and SWITCH
    ([S-f12] .              repl-buffer-create)

    ;; SET REPL buffer
    ([C-f12] .              repl-buffer-set)

    ))


(provide 'repl)

