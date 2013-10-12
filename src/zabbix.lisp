(in-package :zabbix)

(defvar *url* nil)
(defvar *debug-jsonrpc* nil)
(defvar *agent* "staat 0.1")

;(setf drakma:*header-stream* *standard-output*)

(defun aget (key list)
  "Get item from associative list based on key and apply cdr on it."
  (cdr (assoc key list)))

(defun items->alist (&rest items)
  "Convert items to associative list. For example, calling
it as: (items->alist :foo 3 :baz 4) => '((:foo . 3) (:baz . 4))."
  (loop :for (a b) :on items :by #'cddr
        :collect (cons a b)))

(defun as-jsonrpc-str (&rest lst)
  "Convert a property list of lisp objects to JSON-RPC string. Appropriate header is added."
  (let ((ret (json:encode-json-plist-to-string
              (append '(:jsonrpc "2.0") lst))))
    (when *debug-jsonrpc*
      (print "****")
      (print ret)
      (print "----"))
    ret))

(defun validate-url ()
  "Check if global *url* was set."
  (unless *url*
    (error "Zabbix URL address wasn't set. Use (set-url! 'url') before you make any further calls.")))

(defun jsonrpc-call (url content)
  "Call given url with POST method and content, where content is JSON string.
To generate JSON string in lisp way, use 'as-jsonrpc-str'. Returns assoc list or nil something gets wrong.
When Zabbix returns assoc list, on success ':result' key will contain content. If fails, ':error' key
will be set with appropriate content list."
  (let ((stream 
         (drakma:http-request url
                              :method :post
                              :accept "application/json"
                              :content-type "application/json"
                              :user-agent *agent*
                              :external-format-out :utf-8
                              :external-format-in :utf-8
                              :redirect 100
                              :content content
                              :want-stream t)))
    (json:decode-json stream)))

(defun set-url! (url)
  "Set internal Zabbix URL. Make sure to use the same URL you type in browser."
  (if url
	(setq *url* (concatenate 'string url "/api_jsonrpc.php"))))

(defun have-url? ()
  "Check if internal Zabbix URL was set."
  *url*)

(defun is-error? (ret)
  "Check if returned content is requested content or error."
  (assoc :error ret))

(defun login (user pass)
  "Perform login on Zabbix service. If fails, returns list containing error code and message, otherwise
returns auth key as string."
  (validate-url)
  (let* ((ret (jsonrpc-call *url*
                            (as-jsonrpc-str :id 1
                                            :method "user.authenticate"
                                            :params (items->alist :user user :password pass))))
         (result (assoc :result ret)))
    (if result
      (cdr result)
      ret)))

(defun get-hosts (auth-key)
  "Return available hosts. Returned value will be in form: '((host-name host-id available) ...)."
  (validate-url)
  (let* ((ret (jsonrpc-call *url*
                            (as-jsonrpc-str :id 2
                                            :auth auth-key
                                            :method "host.get"
                                            :params (items->alist :output "extend"
                                                                  :monitored_hosts 1))))
         (result (assoc :result ret)))
    (if result
      (loop :for a :in (cdr result)
            :collect (list
                      (aget :host a)
                      (aget :hostid a)
                      (aget :available a)))
      ret)))

(defun get-triggers (auth-key n)
  "Get n triggers switched to PROBLEM in last 30 min; triggers will be returned in descending order.
If there are no triggers, nil will be returned."
  ;; 'expandDescription' must be sent as is, or Zabbix will not recognize it as valid key
  (let* ((ret (jsonrpc-call *url*
                            (as-jsonrpc-str :id 2
                                            :auth auth-key
                                            :method "trigger.get"
                                            :params (items->alist :output "extend"
                                                                  :limit n
                                                                  :only_true 1
                                                                  :monitored 1
                                                                  :sortfield "triggerid"
                                                                  :sortorder "DESC"
                                                                  :selectFunctions "extend"
                                                                  "selectHosts" "refer"
                                                                  "expandDescription" 1))))
         (result (assoc :result ret)))
    (if result
      (cdr result)
      ret)))

