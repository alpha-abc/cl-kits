;; [代理服务器]服务端
(in-package :cl-proxy)

(defvar *proxy-server-thread* nil)

(defun start-proxy-server (&optional (host #(0 0 0 0)) (port 10020))
  "启动本地TCP服务"

  (multiple-value-bind (thread)
      (usocket:socket-server host
                             port
                             #'proxy-tcp-handler
                             nil
                             :in-new-thread :t
                             :protocol :stream
                             :reuse-address t
                             :multi-threading t
                             :element-type '(unsigned-byte 8))
    (setf *proxy-server-thread* thread)))

(defun stop-proxy-server ()
  "停止本机TCP服务"

  (block nil
      (when (or (not *proxy-server-thread*)
                (not (bt:thread-alive-p *proxy-server-thread*)))
        (return))
    (bt:destroy-thread *proxy-server-thread*)
    (setf *proxy-server-thread* nil)))

(defun restart-proxy-server()
  (stop-proxy-server)
  (start-proxy-server))


(defun proxy-tcp-handler (socket)
  "处理每一个tcp连接"

  (let (
        (ssl-socket (cl+ssl:make-ssl-server-stream
                     socket
                     :certificate "/Users/tanshuai/devel/ssl/server.crt"
                     :key "/Users/tanshuai/devel/ssl/server.key"))
        )

    (let ((bs (make-array 10 :element-type '(unsigned-byte 8))))
      (read-sequence bs ssl-socket)
      (force-format t "curl request:~% ~A~%" (flexi-streams:octets-to-string bs)))
    ))
