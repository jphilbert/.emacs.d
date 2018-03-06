;; ----------------------------------------------------------------------------
;; General Settings
;; ----------------------------------------------------------------------------
(provide 'user)

(setq user-full-name	"John P. Hilbert"
      user-mail-address "jphilbert@gmail.com")

(setq default-directory "~/")

(setq directory-abbrev-alist
	 (append directory-abbrev-alist
		    '(("~/personal/".
			  "M:/")
			 ("~/dohe/".
			  "X:/Data Analysis/Data Analysis(DeptUsers)")
			 ("~/dev/".
			  "X:/Data Analysis/Data Analysis(DeptUsers)/Development Team")
			 ("~/sci/".
			  "X:/Data Analysis/Data Analysis(DeptUsers)/Science_Team"))))
