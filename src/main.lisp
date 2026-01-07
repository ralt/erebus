(in-package #:erebus)

(defvar *cli-options* (list (cli:make-option
                             :string
                             :description "configuration file"
                             :long-name "config"
                             :short-name #\c
                             :env-vars '("EREBUS_CONFIG")
                             :key :config)))
(defvar *cli-command* (cli:make-command :name "erebus"
                                        :description "Rootless VPN userspace proxy"
                                        :options *cli-options*
                                        :handler #'cli-handler))

(defun main ()
  (setf *random-state* (make-random-state t)) ;; randomness is important for us.
  (cli:run *cli-command*))

(defun cli-handler (command)
  (let ((config (ini:parse-ini (cli:getopt command :config))))
    (cond ((ini:ini-value config :host :section :openvpn-server)
           (let ((client
                   (make-instance
                    'openvpn-client-static-key
                    :host (ini:ini-value config :host :section :openvpn-server)
                    :port (ini:ini-value config :port :section :openvpn-server)
                    :client-ip (ini:ini-value config :client-ip :section :openvpn-server)
                    :static-key (pathname
                                 (ini:ini-value config :static-key :section :openvpn-server)))))
             (connect client)
             (unwind-protect
                  (sleep #xffff)
               (disconnect client)))))))
