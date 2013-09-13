(defpackage staat
  (:use :cl :cl-who))

(defpackage zabbix
  (:use :cl :cl-json)
  (:export :set-url!
		   :login
		   :is-error?
		   :get-hosts
		   :get-triggers))
