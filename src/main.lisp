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
  (let* ((config (ini:parse-ini (cli:getopt command :config)))
         ;; [proxy-out]: the local outbound HTTP proxy (optional).
         (proxy-out (cdr (find :proxy-out config :key #'car)))
         (address (or (ini:ini-value config :address :section :proxy-out) "127.0.0.1"))
         (port (or (ini:ini-value config :port :section :proxy-out) 11023))
         ;; [proxy-in]: inbound port-forwards (label = <vpn-port> <host>:<port>).
         (proxy-in (cdr (find :proxy-in config :key #'car))))
    (cond ((ini:ini-value config :host :section :openvpn-server)
           (let ((secret (ini:ini-value config :secret :section :openvpn-server)))
             (unless secret
               (return-from cli-handler
                 (format t "only static key mode is supported at the moment")))
             (let ((client
                     (make-instance
                      'openvpn-client-static-key
                      :protocol (let ((proto (ini:ini-value config :proto :section :openvpn-server)))
                                  (cond ((string= proto "udp") :datagram)
                                        ((string= proto "tcp") :stream)))
                      :host (ini:ini-value config :host :section :openvpn-server)
                      :port (ini:ini-value config :port :section :openvpn-server)
                      :client-ip (ini:ini-value config :client-ip :section :erebus)
                      :secret (ini:ini-value config :secret :section :openvpn-server)
                      :key-direction (ini:ini-value config :key-direction :section :openvpn-server)
                      :cipher (ini:ini-value config :cipher :section :openvpn-server)
                      :auth (ini:ini-value config :auth :section :openvpn-server))))
               (connect client)
               (unwind-protect
                    (progn
                      ;; bring up inbound port-forwards
                      (dolist (entry proxy-in)
                        (multiple-value-bind (vpn-port host local-port)
                            (%parse-forward (cdr entry))
                          (expose client :vpn-port vpn-port :host host :port local-port)))
                      ;; bring up the outbound proxy if configured
                      (when proxy-out
                        (h:start (make-instance 'acceptor
                                                :address address
                                                :port port
                                                :client client)))
                      ;; stay up until interrupted; the threads above do the work.
                      (loop (sleep 3600)))
                 (disconnect client))))))))

(defun %parse-forward (spec)
  "Parse a [proxy-in] value \"<vpn-port> <local-host>:<local-port>\" into
\(values vpn-port host local-port)."
  (destructuring-bind (vpn-port target) (uiop:split-string spec :separator " ")
    (destructuring-bind (host local-port) (uiop:split-string target :separator ":")
      (values (parse-integer vpn-port) host (parse-integer local-port)))))

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
