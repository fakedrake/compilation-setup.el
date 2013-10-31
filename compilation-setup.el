;;; compilation-setup.el
;; More often than not, you are working on more than one projects
;; simultaneously, and even if you aren't usually you don't want emacs
;; to do something different when told to recompile depending on the
;; current buffer. This problem is being solved by compilation setups.
;;
;; Run a compilation then do `cs-save'(bound to 'C-c
;; c s') in the buffer where the compilation makes sense and the same
;; compilation will be run each time you run `cs-recompile-wrapper'
;; (bound to 'C-c r').
;;
;; If you want more than one files to automatically share compilations
;; you can use the local variable `cs-key-set' which will prompt for a
;; key that can be any string (it can be something meaningful like
;; 'kernel' or previously saved key which is a path). `cs-key' is a
;; buffer local variable and can be set though .dir-locals.el to have
;; a common key for compiling a directory. If the compilation setup
;; changes (via `cs-save' for example) the setup changes for all
;; buffers that share the same key.

(require 's)

(eval-when-compile (require 'cl))

(defvar compilation-setups nil
  "alist with compilation setups. The format of each item is (KEY
  . (COMMAND . DIRECTORY)) where KEY is the first non-nil of the
  following: `compilation-setup-key' local variable,
  `buffer-file-name', `buffer-name'")

(defvar-local cs-key nil
  "This is the key used to find the compilation setup if
  non-nil. You may want to put this in .local-dirs.el so all
  files in a project share the same compilation key.")

(defun cs-key-set (key keep-setup)
  "Set the compilation key for current buffer. Complete with
  existing keys. If keep-setup is non-nil bind the current
  setup (default or saved) to the local key."
  (interactive
   (list (completing-read "Key for current setup: " compilation-setups)
	 (yes-or-no-p (format
		       "Bind the current setup (cmd:'%s' dir:'%s) to the key?"
		       (car (cs-current-setup))
		       (cdr (cs-current-setup))))))
  (let* ((old-key (cs-current-key))
	 (old-setup (cs-current-setup)))
    (cs-delete-setup old-key)
    (setq-local cs-key key)
    (when keep-setup (cs-save key (car old-setup) (cdr old-setup)))))

(defun cs-delete-setup (&optional key)
  "Delete the current cs setup."
  (interactive)

  (let ((setup (cs-current-setup))
	(dk (or key (cs-current-key))))
    (if (null (assoc dk compilation-setups))	; On global setup
	(message "No saved setup.")
      (message "Unpinning setup cmd: %s dir: %s" (car setup) (cdr setup))
      (setq compilation-setups
	    (delete-if (lambda (x) (string= dk (car x))) compilation-setups)))))

(defun cs-current-key ()
  "The key to be used if a lookup is performed."
  (or cs-key (buffer-file-name) (buffer-name)))

(defun cs-current-setup (&optional key nil-on-fail)
  "Lookup the compilation setup for current buffer or the general
  defaults. If key is provided just do the same but with
  key. Non-nil NIL-ON-FAIL gets you nil instead of the general
  default."
  (let ((lu-name (or key (cs-current-key))))
    (or (when lu-name (cdr (assoc lu-name compilation-setups)))
	(unless nil-on-fail (cons compile-command compilation-directory)))))

(defun cs-compile ()
  "Compile with shell and everything"
  (universal-argument)
  (call-interactively 'compile))

(defun cs-compile-wrapper (&optional local dir)
  "Run compile but be smart about the context. Non-nil LOCAL will
  update the local commands for this compilation. Compile in DIR
  in any case."
  (interactive
   (list (y-or-n-p "Compile updating local setup? ")
	 (ido-read-directory-name "Compilation directory: ")))
  (let ((default-dir dir))
    (if local
	(let* ((setup (cs-current-setup))
	       ;; Use local setup
	       (compile-command (car setup))
	       (compilation-directory (cdr setup)))
	  (cs-compile)
	  (cs-save))
      (cs-compile))))


(defun cs-recompile ()
  "Run `recompile' and message with what you did."
  (call-interactively 'recompile)
  (message "Compiling with: cmd: '%s', dir: '%s'"
	   compile-command compilation-directory))

(defun cs-current-setup-p (setup)
  "nil if setup is not the current."
  (and (string= compile-command (car setup))
       (string= compilation-directory (cdr setup))))

(defun cs-recompile-wrapper ()
  "Run `recompile' but switch to a local compilation setup from
`compilation-setups' if you find one."
  (interactive)
  (let* ((setup (cs-current-setup))
	 (global-recompile (cs-current-setup-p setup)) ;setup is
					;current
					;without
					;setting
	 (compile-command (car setup))
	 (compilation-direcory (cdr setup)))
    (when (or (null global-recompile)
	      (or (string-prefix-p (file-truename compilation-direcory)
				   (file-truename buffer-file-name))
		  (y-or-n-p
		   (format
		    "Global recompile in not-current dir (compilation dir: %s)? "
		    compilation-direcory))))
      (cs-recompile))))

(defun cs-save (&optional setup-key setup-command setup-dir)
  "Save the compilation setup and bind it to current buffer
`cs-recompile-wrapper' will use this setup to compile when called
from this buffer. The buffer is identified with
`buffer-file-name' or if that fails with `buffer-name'. You can
override what is being saved with non-nil valies to SETUP-KEY
SETUP-COMMAND and SETUP-DIR."
  (interactive)
  (let ((key (or setup-key (cs-current-key)))
	(val (cons (or setup-command compile-command)
		   (or setup-dir compilation-directory))))
    (if (assoc key compilation-setups)
	(setf (cdr (assoc key compilation-setups)) val)
      (add-to-list 'compilation-setups (cons key val)))
    (message "Pinned compilation: cmd: '%s', dir: '%s' to '%s'"
	     compile-command compilation-directory key)))

(defun cs-globalize (&optional setup)
  "Make SETUP compilation setup global. If nil use the one that
would be run on recompile."
  (interactive)
  (let ((setup (or setup (cs-current-setup))))
    (setq compile-command (car setup))
    (setq compilation-directory (cdr setup))))

(provide 'compilation-setup)
