;;; $Id$

(defun dino-csde-complete-build-completion-list (typeinfo)
  "Build a completion list from the TYPEINFO list, as returned by the
csde.util.Completion.getTypeinfo function."
  
  (let (result tname props methods leftover modifiers)
    
    ;; get the variable fields
    (setq tname (car typeinfo))
    
    (message "tname is '%s'" tname)

    (setq leftover (cddr typeinfo))
    (setq props (car leftover))

    (while props
      (let ((one-prop (car props) ) )
	;(message "this one prop: '%s'" (princ one-prop))

	(setq modifiers
	      	  (mapconcat 'identity (cdr (nth 3 one-prop)) " "))
	;(message "modifiers!: '%s'"  modifiers)
	
	(setq result
	      (append (list
		       (list
			(car one-prop)   ;; the name of the property
			
			;; additional information about the propery
			(concat "(Property) "
				modifiers  ;; modifiers on this prop
				" "
				(nth 2 one-prop)  ;; type of the prop
				)
			))
		      result))

	;(message "interim result is:")
	;(print result (get-buffer "*Messages*"))

;; 	  (concat 
;; 	   (nth 1 oneprop) 
;; 	   " " 
;; 	   (car oneprop)))) 
      )
      (setq props (cdr props))
      )

    (setq methods (cadr leftover))
    ;(message "methods is: ")
    ;(print methods (get-buffer "*Messages*"))

    ;; get the methods 
    ;(setq tmp (nth 2 typeinfo))
    
    (while methods
      (let ((one-method (car methods))
	    params)
	
	;(message "this one method is:" )
	;(print one-method (get-buffer "*Messages*"))
	
	(setq modifiers
	      (mapconcat 'identity (cdr (nth 4 one-method)) " "))

	(setq params
	      (if (nth 3 one-method)
		  (concat "("
			  (mapconcat 'identity (nth 3 one-method)  " ")
			  ")")
		;; else
		"()"
		)
	      )
	      
	;(message "params: '%s'" params )
	
;; 	(setq modifiers
;; 	      (mapconcat 'identity (cdr (nth 3 one-method)) " "))
;; 	(message "modifiers!: '%s'"  modifiers)


	(setq result
	      (append (list
		       (list
			(car one-method)   ;; the name of the method
			
			;; additional information about the method (in a string)
			(concat "(Method) "
				modifiers           ;; modifiers on this prop
				"  "
				params
				"  returns "
				(nth 2 one-method)  ;; return type of the method
				)
			))
		      result))
	
;	(setq result (append (list one-method) result))
	
;; 	(setq result 
;; 	      (append (list (list (car one-method) (nth 2 one-method)))
;; 		      result))
	
;	      (append (list (list (concat (car one-method) "(") (csde-complete-build-information-for-completion one-method))) 
	)
      (setq methods (cdr methods)))

    (message "build-completion-list result is: ")
    (print result (get-buffer "*Messages*"))
    result))

;; (defun dino-csde-complete-build-completion-list (typeinfo)
;;   "Build a completion list from the TYPEINFO list, as returned by the
;; csde.util.Completion.getTypeinfo function."
  
;;   (let (result tname props methods leftover tmp)
    
;;     ;; get the variable fields
;;     (setq tname (car typeinfo))
    
;;     (message "tname is '%s'" tname)

;;     (setq leftover (cddr typeinfo))

;;     ;(message "leftover is: ")
;;     ;(print leftover (get-buffer "*Messages*"))
    
;;     (setq props (car leftover))

;;     ;(dino-csde-tellme props (quote props))

;;     (while props
;;       (let ((one-prop (car props)))
;; 	;(message "this one prop: ")
;; 	;(print one-prop (get-buffer "*Messages*"))

;;       (setq result 
;;        (append (list (list (car one-prop) (nth 2 one-prop)))
;; 	result))

;; ;; 	  (concat 
;; ;; 	   (nth 1 oneprop) 
;; ;; 	   " " 
;; ;; 	   (car oneprop)))) 
;;       )
;;       (setq props (cdr props))
;;       )

;;     (setq methods (cadr leftover))
;;     ;(message "methods is: ")
;;     ;(print methods (get-buffer "*Messages*"))

;;     ;; get the methods 
;;     ;(setq tmp (nth 2 typeinfo))
    
;;     (while methods
;;       (let ((one-method (car methods)))
	
;; 	;(message "this one method is: ")
;; 	;(print one-method (get-buffer "*Messages*"))

;; 	(setq result 
;; 	      (append (list (list (car one-method) (nth 2 one-method)))
;; 		      result))
	
;; ;	      (append (list (list (concat (car one-method) "(") (csde-complete-build-information-for-completion one-method))) 
;; 	)
;;       (setq methods (cdr methods)))

    
;;     (message "build-completion-list result is: ")
;;     (print result (get-buffer "*Messages*"))
;;     result))



(defun dino-fixup-typeinfo()
  (interactive)
  (save-excursion
    (let ((mark1 (point)))
      
    (goto-char mark1)
    (while (search-forward ")(" nil t)
      (replace-match ")\n(" nil t))
    
    (goto-char mark1)
    (while (search-forward ") (" nil t)
      (replace-match ")\n(" nil t))
    
    (goto-char mark1)
    (while (search-forward "(list (list" nil t)
      (replace-match "(list\n(list" nil t))
    
    (goto-char mark1)
    (while (search-forward "'type (list" nil t)
      (replace-match "'type\n (list " nil t))
    
    (goto-char mark1)
    (forward-sexp)
    (indent-region mark1 (point))
   )
  )
  )








(setq typeinfo
      (list "System.IO.DriveInfo" 'type
	    (list
	     (list "Name" 'property "System.String" (cons 'typemodifiers (list "readonly" "public" )))
	     (list "DriveType" 'property "System.IO.DriveType" (cons 'typemodifiers (list "readonly" "public" )))
	     (list "DriveFormat" 'property "System.String" (cons 'typemodifiers (list "readonly" "public" )))
	     (list "IsReady" 'property "System.Boolean" (cons 'typemodifiers (list "readonly" "public" )))
	     (list "AvailableFreeSpace" 'property "System.Int64" (cons 'typemodifiers (list "readonly" "public" )))
	     (list "TotalFreeSpace" 'property "System.Int64" (cons 'typemodifiers (list "readonly" "public" )))
	     (list "TotalSize" 'property "System.Int64" (cons 'typemodifiers (list "readonly" "public" )))
	     (list "RootDirectory" 'property "System.IO.DirectoryInfo" (cons 'typemodifiers (list "readonly" "public" )))
	     (list "VolumeLabel" 'property "System.String" (cons 'typemodifiers (list "public" ))))
	    (list
	     (list "Equals" 'function "System.Boolean" (list "System.Object" )
		   (cons 'typemodifiers (list "public" )))
	     (list "GetDrives" 'function "System.IO.DriveInfo[]" nil (cons 'typemodifiers (list "public" "static" )))
	     (list "GetHashCode" 'function "System.Int32" nil (cons 'typemodifiers (list "public" )))
	     (list "GetType" 'function "System.Type" nil (cons 'typemodifiers (list "public" )))
	     (list "ToString" 'function "System.String" nil (cons 'typemodifiers (list "public" )))))
      )

;(debug-on-entry 'dino-csde-complete-build-completion-list)

(dino-csde-complete-build-completion-list typeinfo)
