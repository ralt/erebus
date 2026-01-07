(in-package #:erebus)

(defvar *cli-options* (list (cli:make-option
                             :string
                             :description "configuration file"
                             :long-name "config"
                             :short-name #\c
                             :env-vars '("EREBUS_CONFIG")
                             :key :config)))

(defun main ()
  (setf *random-state* (make-random-state t)) ;; randomness is important for us.
  (cli:run *cli-command*))

(defun cli-handler (command)
  (unless (cli:getopt command :config)
    (return-from cli-handler
      (cli:print-usage command t)))
  (let ((config (ini:parse-ini (cli:getopt command :config))))
    (cond ((ini:ini-value config :host :section :openvpn-server)
           (let ((secret (ini:ini-value config :secret :section :openvpn-server)))
             (unless secret
               (return-from cli-handler
                 (format t "only static key mode is supported at the moment")))
             (let ((client
                     (make-instance
                      'openvpn-client-static-key
                      :host (ini:ini-value config :host :section :openvpn-server)
                      :port (ini:ini-value config :port :section :openvpn-server)
                      :client-ip (ini:ini-value config :client-ip :section :erebus)
                      :secret (ini:ini-value config :secret :section :openvpn-server)
                      :key-direction (ini:ini-value config :key-direction :section :openvpn-server)
                      :cipher (ini:ini-value config :cipher :section :openvpn-server)
                      :auth (ini:ini-value config :auth :section :openvpn-server))))
               (connect client)
               (unwind-protect
                    (sleep #xffff)
                 (disconnect client))))))))

;; we want to print usage whenever an option is wrong
(defmethod cli:parse-command-line :around (command arguments)
  (handler-bind ((cli:unknown-option (lambda (c)
                                       (declare (ignore c))
                                       (invoke-restart 'cli:discard-option))))
    (call-next-method)))

(defvar *cli-command* (cli:make-command :name "erebus"
                                        :description "Rootless VPN userspace proxy"
                                        :options *cli-options*
                                        :handler #'cli-handler))
