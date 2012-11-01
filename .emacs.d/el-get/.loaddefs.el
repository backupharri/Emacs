;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pylookup-lookup-at-point pylookup-update-all pylookup-update
;;;;;;  pylookup-set-search-option pylookup-lookup) "pylookup/pylookup"
;;;;;;  "pylookup/pylookup.el" (20625 58029))
;;; Generated autoloads from pylookup/pylookup.el

(autoload 'pylookup-lookup "pylookup/pylookup" "\
Lookup SEARCH-TERM in the Python HTML indexes.

\(fn SEARCH-TERM)" t nil)

(autoload 'pylookup-set-search-option "pylookup/pylookup" "\
Set search option interactively

\(fn OPTION-STRING)" t nil)

(autoload 'pylookup-update "pylookup/pylookup" "\
Run pylookup-update and create the database at `pylookup-db-file'.

\(fn SRC &optional APPEND)" t nil)

(autoload 'pylookup-update-all "pylookup/pylookup" "\
Run pylookup-update for all sources and create the database at `pylookup-db-file'.

\(fn)" t nil)

(autoload 'pylookup-lookup-at-point "pylookup/pylookup" "\
Query the for string with help of word read at point and call `pylookup-lookup'

\(fn)" t nil)

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
