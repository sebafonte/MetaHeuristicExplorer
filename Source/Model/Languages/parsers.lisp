
(defvar *parser-input*)
(defvar *current-grammar*)


(defun search-on-symbol-table (table word)
  "Answer lexer value on <table> for <word> or :unknown if not present."
  (or (cadr (assoc word table)) :unknown))

