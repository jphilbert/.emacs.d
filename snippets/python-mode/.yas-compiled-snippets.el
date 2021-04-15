;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
				 '(("setwd" "os.chdir(\"${1:`(abbreviate-file-name (file-name-directory (buffer-file-name)))`}\")\n$0" "setwd" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/python-mode/setwd" nil nil)
				   ("reload" "import importlib\nimportlib.reload($0)" "reload" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/python-mode/reload" nil nil)
				   ("getwd" "os.getcwd()\n$0" "getwd" nil nil nil "c:/Users/hilbertjp2/.emacs.d/snippets/python-mode/getwd" nil nil)
				   ("docMethod" "\"\"\"${1:The summary of the method.}\n\nArgs:\n    _arg$0\n\nReturns:\n    ${2:info on the return value}\n\"\"\"" "docMethod" nil nil
				    ((yas-indent-line 'fixed)
					(yas-wrap-around-region 'nil)
					(yas-triggers-in-field t))
				    "c:/Users/hilbertjp2/.emacs.d/snippets/python-mode/docMethod" nil nil)
				   ("docClass" "\"\"\"${1:The summary line for a class docstring should fit on one line.}\n\n${2:If the class has public attributes, they may be documented here\n    in an 'Attributes' section and follow the same formatting as a\n    function's 'Args' section. Alternatively, attributes may be documented\n    inline with the attribute's declaration (see __init__ method below).}\n\nAttributes:\n    ${3:_arg}$0\n\"\"\"" "docClass" nil nil
				    ((yas-indent-line 'fixed)
					(yas-wrap-around-region 'nil)
					(yas-triggers-in-field t))
				    "c:/Users/hilbertjp2/.emacs.d/snippets/python-mode/docClass" nil nil)
				   ("_arg" "${1:$$(upcase yas-text)} (${2:type}):\n  ${3:description}\n${4:_arg}$0" "_arg" nil nil
				    ((yas-indent-line 'fixed)
					(yas-wrap-around-region 'nil)
					(yas-triggers-in-field t))
				    "c:/Users/hilbertjp2/.emacs.d/snippets/python-mode/_arg" nil nil)))


;;; Do not edit! File generated at Wed Apr 14 16:24:50 2021
