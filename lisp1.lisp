;;;; 1st interpreter from Steele and Sussman "Art of the Interpreter"
;;;; Syntax adapted to CL conventions

(extern value1 (name slot))
(extern lookup (name env))
(extern lookup1 (name vars vals env))
(extern value (name env))

(defun bind (vars args env)
  (cond ((= (length vars) (length args))
	 (cons (cons vars args) env))
	(t (error "Vars and Args have different lengths when binding."))))

(defun value1 (name slot)
  (cond ((eq '&unbound slot) (error "Variable is unbound"))
	(t (car slot))))

(defun value (name env)
  (value1 name (lookup name env)))

(defun lookup1 (name vars vals env)
  (cond ((null vars) (lookup name (cdr env)))
	((eq (car vars) name) vals)
	(t (lookup1 name (cdr vars) (cdr vals) env))))

(defun lookup (name env)
  (cond ((null env) '&unbound)
	(t (lookup1 name (caar env) (cdar env) env))))

(externtc driver-loop1 (env form))
(externtc driver-loop (env hunoz))
(extern eval (exp env))
(extern apply (fun args env))
(extern evcond (clauses env))
(extern evlis (arglist env))
(extern primop (fun))
(extern primop-apply (fun args))

(defun driver ()
  (driver-loop '(((car cdr eq atom cons)
		  &car &cdr &eq &atom &cons))
	       (repr 'lithp-ith-lithtening)))

(defuntc driver-loop (procedures hunoz)
  (driver-loop1 procedures (prog1_read)))

(defuntc driver-loop1 (env form)
  (cond ((atom form) (driver-loop env (print (eval form env))))
	((eq 'quit (car form)) nil)
	((eq 'defun (car form))
	 (driver-loop (bind (list (cadr form))
			    (list (list '&procedure
					(caddr form)
					(cadddr form)))
			    env)
		      (print (cadr form))))
	((eq 'print-procs (car form))
	 (driver-loop env (print env)))
	(t (driver-loop env (print (eval form env))))))


(defun eval (exp env)
  (cond ((atom exp) (cond ((eq 'nil exp) 'nil)
			  ((eq 't exp) 't)
			  ;; ((numberp exp) exp)
			  (t (value exp env))))
	((eq 'quote (car exp)) (cadr exp))
	((eq 'cond (car exp)) (evcond (cdr exp) env))
	((eq 'if (car exp)) (eval (list 'cond
					(list (cadr exp) (caddr exp))
					(list 't (cadddr exp)))
				  env))
	(t ;; (print exp)
	   (apply (value (car exp) env)
		  (evlis (cdr exp) env)
		  env))))

(defun apply (fun args env)
  ;; (print fun)
  ;; (print args)
  (cond ((primop fun) (primop-apply fun args))
	((eq (car fun) '&procedure)
	 (eval (caddr fun)
	       (bind (cadr fun) args env)))
	(t (error "Attempt to call non-function object."))))

(defun evcond (clauses env)
  (cond ((null clauses) (error "At least one clause of cond should match!"))
	((eval (caar clauses) env)
	 (eval (cadar clauses) env))
	(t (evcond (cdr clauses) env))))

(defun evlis (arglist env)
  ;; (print arglist)
  (cond ((null arglist) '())
	(t (cons (eval (car arglist) env)
		 (evlis (cdr arglist) env)))))

(defun primop (fun)
  (cond ((eq fun '&car) t)
	((eq fun '&cdr) t)
	((eq fun '&eq) t)
	((eq fun '&atom) t)
	((eq fun '&cons) t)
	(t nil)))

(defun primop-apply (fun args)
  (cond ((eq fun '&car) (car (car args)))
	((eq fun '&cdr) (cdr (car args)))
	((eq fun '&eq) (eq (car args) (cadr args)))
	((eq fun '&atom) (atom (car args)))
	((eq fun '&cons) (cons (car args) (cadr args)))
	(t (error "Got unknown primary operation"))))

