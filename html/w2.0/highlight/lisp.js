CodeHighlighter.addStyle("lisp",{
	comment : {
		exp  : /(;{1,4}[^\n]*\n)|(#\|[^*]*\*+([^\/][^*]*\*+)#\|)/
	},
	brackets : {
		exp  : /\(|\)/
	},
	string : {
		exp  : /'[^']*'|"[^"]*"/
	},
	keywords : {
		exp  : /\b(defun|defmethod|defmacro|defclass|return|return-from|with-slots|nil|let|let|lambda|if|loop|defparameter|in-package)\b/
	},
	global : {
		exp  : /\b(car|cdr|length|first|second|last|list|setf|getf|push|funcall)\b/
	}
});