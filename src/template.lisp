(in-package :staat)

(defmacro with-standard-page (&body body)
  "Page template. Adjust as needed."
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html 
	 (:head 
	  (:title "Status page")
	  (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	  (:link :href "/media/css/bootstrap.min.css" :rel "stylesheet" :media "screen"))
	 (:body
	  (:br)
	  (:div :class "container"
       (:div :class "masthead"
		(:h3 :class "muted" "Status page")
	   ;; content
	   (:table :class "table"
	     (:thead
           (:tr (:th "Server name") (:th "Status") (:th "Details")))
		 (:tbody ,@body)))
	  (:div :class "footer"
	   (:div :class "container"
	     (:p :class "muted"
		   (:small "&copy; your-company.com")))))
	  (:script :src "/media/js/jquery.min.js")
	  (:script :src "/media/js/jquery.peity.min.js")
	  (:script :src "/media/js/bootstrap.min.js")
	  (:script "$(\".bar\").peity(\"bar\");") ))))
