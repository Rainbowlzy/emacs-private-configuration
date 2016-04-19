;; csde-shell.el
;; $Revision: 1.4 $ 

;; Adapted from the JDE by Matt Bruce <matt.bruce@morganstanley.com>
;; Maintainer:  Matt Bruce

;; JDE Author: Paul Kinnucan <paulk@mathworks.com>
;; JDE Maintainer: Paul Kinnucan

;; Keywords: csharp, tools

;; Copyright (C) 2001 Matt Bruce

;; The JDE is Copyright (C) 1997, 1998, 1999, 2000, 2001 Paul Kinnucan.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; The latest version of the CSDE is available at
;; <URL:http://www.sourceforge.com/>.

;; Please send any comments, bugs, or upgrade requests to
;; Matt Bruce (matt.bruce@morganstanley.com)

;; Need csde-run only to get the definition for 
;; save-w32-show-window macro.


(require 'powershell)

(defcustom csde-shell-startup-timeout 10
  "*Length of time the CSDE waits for the Beanshell to startup.
Increase the value of this variable if you get Lisp errors
on BeanShell startup on Unix."
  :group 'csdeshell
  :type 'integer)

(defcustom csde-shell-eval-timeout 10
  "*Length of time in seconds the CSDE waits for the CsdeShell to evaluate
an expression before giving up and signaling an error."
  :group 'csdeshell
  :type 'integer)

(defcustom csde-shell-buffer-name "*csdeshell*"
  "Name of the Powershell buffer for CSDE support."
  :group 'csdeshell
  :type 'string
  )

(defcustom csde-shell-prompt-string "csdeshell %"
  "The prompt string used for csde-shell."
  :group 'csdeshell
  :type 'string
  )


(defun csde-shell-set-window-width-string ()
  (concat  "$a = (Get-Host).UI.RawUI\n" 
	   "$b = $a.WindowSize\n"
	   "$b.Width = 19999\n"
	   "$a.BufferSize = $b")
  )
  



(defun csde-shell()
  "*Starts csdeshell, which is a Powershell dedicated to supporting CSDE, for code completion and etc."
  (interactive)
  (csde-shell-internal nil)
  )


(eval-when-compile
  (if (not (fboundp 'save-w32-show-window))
      (defmacro save-w32-show-window (&rest body)
	"Saves the value of the w32-start-process-show-window variable
before evaluating body and restores the value afterwards."
	`(if (and (eq system-type 'windows-nt)
		  (not (string-match "XEmacs" (emacs-version))))
	     (if (boundp 'win32-start-process-show-window)
		 (let ((save win32-start-process-show-window))
		   (setq win32-start-process-show-window t)
		   ,@body
		   (setq win32-start-process-show-window save))
	       (let ((save w32-start-process-show-window))
		 (setq w32-start-process-show-window t)
		 ,@body
		 (setq w32-start-process-show-window save)))	 
	   ,@body))))


(defun csde-shell-internal (&optional display-buffer) 
  (if (not (comint-check-proc csde-shell-buffer-name))
      ;; then
      (and
       (message "Starting the CsdeShell...")
       (csde-shell-start csde-shell-buffer-name)
       (message "The CsdeShell is now running...")
       )
    ;; else
    (when display-buffer
      (message "CsdeShell is already running.")
      ;;(pop-to-buffer csde-shell-buffer-name))
      )
    )
  )



(setq csde-shell-tq-reply nil)

(defun csde-shell-eval-filter (process result)
					;(message "got reply: '%s'" result)
  (let ((end-of-result (string-match (concat ".*" csde-shell-prompt-string) result)))
    ;; Check for case
    ;;   %csdeshell\n...eval output...%csdeshell\n
    ;; This can happen because the beanshell outputs two or more
    ;; prompts after evaluating some expressions.
    ;; Thanks to Stephane Nicolas.
    ;; (if (eq end-of-result 0)
    ;; (accept-process-output process 0 5))
    (if end-of-result
	(and
					;(message "end of result: '%s'" (substring (substring result 0 end-of-result) 0 -1))
	 (setq csde-shell-tq-reply (concat csde-shell-tq-reply (substring (substring result 0 end-of-result) 0 -1)))
	 )

					;else
      (and 
       (message "NOT end of result?")
       (setq csde-shell-tq-reply (concat csde-shell-tq-reply result))
       (accept-process-output process csde-shell-eval-timeout 5)
       )
      )
    )
  )



(defun csde-shell-eval (expr &optional eval-return)
  "Uses the CSDEshell interpreter to evaluate a C# statement.
If the interpreter is not already running, this function starts
the interpreter. This function returns any text output by the
C# interpreter's standard out or standard error pipes.
If the optional argument eval-return is non-nil, this function
returns the result of evaluating the Csharp output as a Lisp
expression."
  (let* ((proc
	  (if (get-buffer-process csde-shell-buffer-name)
	      (get-buffer-process csde-shell-buffer-name)
	    (let (proc2)
	      (csde-shell-internal)
	      (setq proc2 (get-buffer-process csde-shell-buffer-name))
	      (if (eq system-type 'windows-nt)
		  (accept-process-output proc2)
		(while (accept-process-output proc csde-shell-startup-timeout 0)))
	      proc2)))
	 (original-filter (if proc (process-filter proc)))
	 tmp
	 )
    
    (when proc
      (setq csde-shell-tq-reply nil)
      (set-process-filter proc 'csde-shell-eval-filter)
      ;; debug stmt
      (message "Sending: %s" expr)
      (process-send-string proc (concat expr "\nprompt\n"))
      (if (not (accept-process-output proc csde-shell-eval-timeout 100))
	  (and
	   (message "no reply...")
	   (error "No reply from CsdeShell"))
	)
      ; restore filter
      (set-process-filter proc original-filter)
      (if (string-match "// Error:" csde-shell-tq-reply)
	  (progn
	    (message 
	     "CsdeShell expression evaluation error.\n  Expression: %s\n  Error: %s"
	     expr csde-shell-tq-reply)
	    (error "CsdeShell eval error. See messages buffer for details.")))
      ;; (if eval-return (message "Evaluating reply: %s" csdeshell-tq-reply))
      
      (if eval-return
	  
	  (if csde-shell-tq-reply

	      (progn
		(message "evaluating reply: '%s'" csde-shell-tq-reply)

		(if (> (length csde-shell-tq-reply) 4)

		    (progn
		      (setq tmp (eval (read csde-shell-tq-reply)))
		      tmp
		      )

		  ;; 		     (condition-case eval-error
		  ;; 		       (progn
		  ;; 			(message "Error evaluating Lisp result of C# expression evaluation.")
		  ;; 			(message "  Csharp expression: %s." expr)
		  ;; 			(message "  Csharp evaluation result: %s." csde-shell-tq-reply)
		  ;; 			(error "Error evaluating Csharp expresson. See *Messages* buffer.")))

		     
		  ;; else, too short to eval
		  nil)
		)
	    
	    ;; else 
	    (progn
	      (message "csde-shell-eval-r: result is nil. Cannot evaluate.")
	      nil)
	    )
		
					; else (no eval)
	(progn
	  (message "no eval, reply: '%s'" csde-shell-tq-reply)
	  csde-shell-tq-reply)
	
	)
      
      )
    )
  )


					;(debug-on-entry 'csde-shell-eval)

(defun csde-shell-eval-r (psh-statement) 
  "Convenience function for evaluating Powershell statements
that return Lisp expressions as output. This function 
invokes csde0shell-eval with the evaluate-return option set to
t."
  (csde-shell-eval psh-statement t))



;; This function trims the newline from the prompt that we
;; get back from powershell.  It is set into the preoutput
;; filters, so the newline is trimmed before being put into
;; the output buffer.
(defun csde-preoutput-filter-for-prompt (string)
  (if
					; not sure why, but I have not succeeded in using a variable here???  
					;(string-match  powershell-prompt-pattern  string)

      (string-match  csde-shell-prompt-string string)
      (substring string 0 -1)
     
    string

    )
  )



(defun shell (&optional buffer)
  "Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or (if that is nil) from `shell-file-name'.
If a file `~/.emacs_SHELLNAME' exists, or `~/.emacs.d/init_SHELLNAME.sh',
it is given as initial input (but this may be lost, due to a timing
error, if the shell discards input when it starts up).
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
	 (read-buffer "Shell buffer: "
		      (generate-new-buffer-name "*shell*")))))
  (setq buffer (get-buffer-create (or buffer "*shell*")))
  ;; Pop to buffer, so that the buffer's window will be correctly set
  ;; when we call comint (so that comint sets the COLUMNS env var properly).
  (pop-to-buffer buffer)
  (unless (comint-check-proc buffer)
    (let* ((prog (or explicit-shell-file-name
		     (getenv "ESHELL") shell-file-name))
	   (name (file-name-nondirectory prog))
	   (startfile (concat "~/.emacs_" name))
	   (xargs-name (intern-soft (concat "explicit-" name "-args"))))

      (unless (file-exists-p startfile)
	(setq startfile (concat "~/.emacs.d/init_" name ".sh")))
      (apply 'make-comint-in-buffer "shell" buffer prog
	     (if (file-exists-p startfile) startfile)
	     (if (and xargs-name (boundp xargs-name))
		 (symbol-value xargs-name)
	       '("-i")))
      (shell-mode)))
  buffer)



(defun csde-shell-start (buffer)
  "Run Powershell in support of CSDE, by invoking the shell function. See the help for shell for more details.
\(Type \\[describe-mode] in the shell buffer for a list of commands.)"

  ;; get a name for the buffer
  (setq buffer (get-buffer-create buffer))

  (let ((tmp-shellfile explicit-shell-file-name)
	)
    ;; set arguments for the powershell exe.
    ;; This needs to be tunable.
    (setq explicit-shell-file-name "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")  
    (setq explicit-powershell.exe-args '("-Command" "-" )) ; interactive, but no command prompt
  
    ;; launch the shell
    (shell buffer)

    ;; restore the original shell file name
    (if explicit-shell-file-name
        (setq explicit-shell-file-name tmp-shellfile)
      )
    )
  
  (let ((proc (get-buffer-process buffer)) )    
    
    ;; This sets up the powershell RawUI screen width. By default,
    ;; the powershell v1.0 assumes terminal width of 80 chars.
    ;; This means input gets wrapped at the 80th column.  We reset the
    ;; width of the PS terminal to the window width. 
    (add-hook 'window-size-change-functions 'powershell-window-size-changed)

    (powershell-window-size-changed)

    ;; This sets up a prompt for the csdeshell.
    ;; The prompt is important because later, after sending a command to the shell,
    ;; the scanning logic that grabs the output looks for the
    ;; prompt string to determine that the output is complete.
    (comint-simple-send 
     proc
     (concat "function prompt { '" csde-shell-prompt-string "' }"))

    ;; This loads the DLL that knows how to interrogate .NET Types and
    ;; send back string representations of list s-expressions, which
    ;; are then evaluated by csde-complete.el
    (comint-simple-send 
     proc
     "[System.Reflection.Assembly]::LoadFrom('c:\\dinoch\\dev\\dotnet\\CsdeUtilities.dll');")

    ;; need the buffer width to be really wide, otherwise powershell
    ;; wraps lines.  Which is dumb.
    (comint-simple-send proc (csde-shell-set-window-width-string))

    ;; if the user exits, we won't ask whether he wants to kill the csdeshell. 
    (process-kill-without-query proc)

    ; ask for initial prompt - just for aesthetics
    (comint-simple-send proc "prompt")
    )

  ;; hook the kill-buffer action so we can kill the inferior process?
  ;; (add-hook 'kill-buffer-hook 'powershell-delete-process)

  ;; wrap the comint-input-sender with a PS version
  ;; must do this after launching the shell! 
  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender 'powershell-simple-send)

  ;; set a preoutput filter for powershell.  This will trim newlines after the prompt.
  (add-hook 'comint-preoutput-filter-functions 'csde-preoutput-filter-for-prompt)

  ;;(run-hooks 'powershell-launch-hook)

  ;; remove the window for the shell.
  ;; shell.el automatically pops to the shell buffer, but we
  ;; don't want that in this case.  We just want the buffer to run
  ;; in the background, as it were. 
  (delete-window)
  
  ;; return the buffer created
  buffer
  )


(provide 'csde-shell)			;

;; End of csde-shell.el
