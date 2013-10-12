(asdf:defsystem #:staat
  :version "0.1"
  :author "Sanel Zukan"
  :license "MIT"
  :depends-on (:hunchentoot :cl-who :cl-json :drakma :bordeaux-threads)
  :description "Service status page"
  :components ((:module src
				:serial t
                :components ((:file "package")
							 (:file "zabbix")
							 (:file "template")
							 (:file "main")))))
