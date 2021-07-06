(defsystem "cl-proxy"
  :version "0.1.0"
  :author "alpha-abc"
  :license ""
  :depends-on ("usocket"
               "usocket-server"
               "flexi-streams"
               "cl+ssl"
               "bordeaux-threads")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "kit" :depends-on ("package"))
                 (:file "socks5" :depends-on ("package"))
                 (:file "local-server" :depends-on ("package"))
                 (:file "proxy-server" :depends-on ("package"))
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
