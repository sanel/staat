(in-package :staat)

(defvar *app-config* "staat.conf")
(defvar *app-server* nil)
(defvar *app-lock* nil)
(defvar *app-thread-stop* nil)
(defvar *app-zabbix-key* nil)
(defvar *app-db* nil)
(defvar *app-last-fetch* 0)

(defmacro conf-get (var default)
  "Tries to get value of variable or fallback to default if not found. This macro
is for reading values from configuration, as configuration variables are nothing more
than global variables loaded in current package."
  `(if (boundp ',var)
     ,var
     ,default))

(defmacro let1 (var val &body body)
  "I always found 'let' way to verbose for single binding form."
  `(let ((,var ,val))
     ,@body))

(defun rename-server (name)
  "Rename server based on setup matches from configuration."
  (let1 matches (conf-get *server-name-matcher* nil)
    (loop :for i :in matches
          :do (if (string-equal (first i) name)
                (return-from rename-server (second i))))
    name))

(defun draw-status (type message)
  "Depending on type, display status message with different background color."
  (format nil "<span class=\"label ~A\">~A</span>"
          (case type
            ((:red)   "label-warning")
            ((:green) "label-success")
            (otherwise ""))
          message))

(defun update-servers (hosts triggers)
  "Rename server list received from zabbix:get-hosts, sort it and update details received
from server triggers and fill details if given."
  (let1 lst (mapcar #'(lambda (h)
                        (list (rename-server (first h))
                              (if (string-equal (third h) "1")
                                (draw-status :green "Online")
                                (draw-status :gray "Offline"))))
                    hosts)
    (sort lst #'(lambda (x y)
                  (string< (first x) (first y))))))

(defun collect ()
  "Fetch data from server and return list with processed values."
  (unless (zabbix:have-url?)
    (zabbix:set-url! (conf-get *zabbix-url* nil)))

  (unless *app-zabbix-key*
    (let* ((user (conf-get *zabbix-user* nil))
           (pass (conf-get *zabbix-password* nil))
           (key  (zabbix:login user pass)))
      ;; received key, no error here
      (if (stringp key)
        (setq *app-zabbix-key* key)
        (print "Unable to fetch key, will try latter..."))))

  (when *app-zabbix-key*
	(update-servers (zabbix:get-hosts *app-zabbix-key*)
					(zabbix:get-triggers *app-zabbix-key* 6))))

(defun collect-cached ()
  "Same as 'collect' but cache value at least *zabbix-fetch-interval* seconds."
  (let* ((now  (get-universal-time))
		 (diff (- now *app-last-fetch*)))
    (when (>= diff *zabbix-fetch-interval*)
	  (setq *app-db* (collect))
	  ;; make sure time is updated only when correct value was retrieved
	  (if *app-db* (setq *app-last-fetch* now)))
	*app-db*))
	  
(defun fetch-and-fill ()
  "Fetch and fill server statuses inside page."
  (zabbix:set-url! (conf-get *zabbix-url* nil))
  (let* ((user     (conf-get *zabbix-user* nil))
         (pass     (conf-get *zabbix-password* nil))
         (key      (zabbix:login user pass))
         ;; last 5 triggers will be enough
         (hosts    (update-servers (zabbix:get-hosts key) (zabbix:get-triggers key 5))))
    (with-html-output-to-string (*standard-output* nil :indent t)
      (loop :for h :in hosts
            :do (htm
                 (:tr
                  (:td (fmt (first h)))
                  (:td (fmt (draw-status :green "Online")))))))))

;; declare handlers; for now only one
(hunchentoot:define-easy-handler (index :uri "/") ()
  (with-standard-page
    (fmt (fetch-and-fill))))

(defun start-reader-thread ()
  (bordeaux-threads:make-thread
   (lambda ()
     (loop :until *app-thread-stop*
           :do (progn
                 (sleep 2)
                 (format t "--> ~A~%" "tick"))))))

(defun start-server (config)
  (load config)
  (setq *app-lock* (bordeaux-threads:make-lock))
  (let1 port (conf-get *staat-port* 4141)
    (format t "Staring web server on: ~A port...~%" port)
    ;; global var for REPL access
    (setq *app-server*
          (hunchentoot:start
           (make-instance 'hunchentoot:easy-acceptor
                          :port port
                          :document-root #p".")))))

(defun main ()
  "Application entry point."
  (let1 path (probe-file *app-config*)
    (if path
      (start-server path)
      (error (format nil "Unable to find '~A' in current directory. Aborting...~%" *app-config*)))))
