;;;; 1st interpreter from Steele and Sussman "Art of the Interpreter"
;;;; Syntax adapted to CL conventions

;; (defun driver ()
;;   (driver-loop <the-primitive-procedures>
;; 	       (print '|LITHP ITH LITHTENING|)))

(externtc driver-loop1 (procedures form))

(defuntc driver-loop (procedures hunoz)
  (driver-loop1 procedures (read)))

;; (defun driver-loop1 (procedures form)
;;   (cond ((atom form) (driver-loop procedures (print (eval form '() procedures))))
;; 	((eq 'defun (car form))
;; 	 (driver-loop (bind (list (caadr form))
;; 			    (list (list (cdadr form) (caddr form)))
;; 			    procedures)
;; 		      (print (caadr form))))
;; 	(t (driver-loop procedures (print (eval form '() procedures))))))


;; (defun eval (exp env procedures)
;;   (cond ((atom exp) (cond ((eq 'nil exp) 'nil)
;; 			  ((eq 't exp) 't)
;; 			  ((numberp exp) exp)
;; 			  (t (value exp env))))
;; 	((eq 'quote (car exp)) (cadr exp))
;; 	((eq 'cond (car exp)) (evcond (cdr exp) env procedures))
;; 	(t (apply (value (car exp) procedures)
;; 		  (evlis (cdr exp) env procedures)
;; 		  procedures))))

;; (defun apply (fun args procedures)
;;   (cond ((primop fun) (primop-apply fun args))
;; 	(t (eval (cadr fun)
;; 		 (bind (car fun) args '())
;; 		 procedures))))

;; (defun evcond (clauses env procedures)
;;   (cond ((null clauses) (error))
;; 	((eval (caar clauses) env procedures)
;; 	 (eval (cadar clauses) env procedures))
;; 	(t (evcond (cdr clauses) env procedures))))

;; (defun evlis (arglist env procedures)
;;   (cond ((null arglist) '())
;; 	(t (cons (eval (car arglist) env procedures)
;; 		 (evlis (cdr arglist) env procedures)))))

;; (extern error ())
;; (extern value1 (name slot))
;; (extern lookup (name env))
;; (extern lookup1 (name vars vals env))
;; (extern value (name env))

;; (defun bind (vars args env)
;;   (cond ((= (length vars) (length args))
;; 	 (cons (cons vars args) env))
;; 	(t (error))))

;; (defun error ()
;;   ) ; for now do nothing

;; (defun value1 (name slot)
;;   (cond ((eq '&unbound slot) (error))
;; 	(t (car slot))))

;; (defun value (name env)
;;   (value1 name (lookup name env)))

;; (defun lookup1 (name vars vals env)
;;   (cond ((null vars) (lookup name (cdr env)))
;; 	((eq (car vars) name) vals)
;; 	(t (lookup1 name (cdr vars) (cdr vals) env))))

;; (defun lookup (name env)
;;   (cond ((null env) '&unbound)
;; 	(t (lookup1 name (caar env) (cdar env) env))))


