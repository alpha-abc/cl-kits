(defsystem "cl-proxy"
  :version "0.1.0"
  :author "alpha-abc"
  :license ""
  :depends-on ("usocket"
               "usocket-server"
               "flexi-streams"
               "cl+ssl"
               "bordeaux-threads"
               "log4cl")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "kit")
                 (:file "socks5")
                 (:file "local-server")
                 (:file "proxy-server")
                 )))
  :description ""
  :in-order-to ((test-op (test-op "cl-proxy/tests"))))

(defsystem "cl-proxy/tests"
  :author "alpha-abc"
  :license ""
  :depends-on ("cl-proxy"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-proxy"
  :perform (test-op (op c) (symbol-call :rove :run c)))
