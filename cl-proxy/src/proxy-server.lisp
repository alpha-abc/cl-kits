;; [代理服务器]服务端
(in-package :cl-proxy)

(defvar *proxy-server-thread* nil)

(defun start-proxy-server (&optional (host #(0 0 0 0)) (port 10020))
  "启动本地TCP服务"

  (multiple-value-bind (thread)
      (usocket:socket-server host
                             port
                             'proxy-tcp-handler
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


(defun proxy-tcp-handler (stream)
  "处理每一个tcp连接"

  (handler-case
  (let ((ssl-stream (cl+ssl:make-ssl-server-stream
                     stream
                     :certificate "/Users/tanshuai/devel/ssl/server.crt"
                     :key "/Users/tanshuai/devel/ssl/server.key")))

    (multiple-value-bind (target-host target-port) (proxy-auth ssl-stream)
      (usocket:with-client-socket (socket r-stream target-host target-port
                                          :element-type '(unsigned-byte 8))

        (let ((t1 (bt:make-thread
                   #'(lambda ()
                       (io-byte-copy ssl-stream r-stream))
                   :name "PROXY->TARGET"))
              (t2 (bt:make-thread
                   #'(lambda ()
                       (io-byte-copy r-stream ssl-stream))
                   :name "TARGET->PROXY")))

          (bt:join-thread t1)
          (bt:join-thread t2)))))
    (condition (c) nil)))


(defun proxy-auth (ssl-stream)
  "参考local-auth解析

return
  (values host port)"

  (let ((buffer (make-array 8 :element-type '(unsigned-byte 8)))
        (size 0))

    (setf size (read-sequence buffer ssl-stream))
    (when (/= 8 size) (error "read auth header info error when auth"))
    (let* ((ulen (bits-arr->unumber buffer :start 0 :end 2))
           (plen (bits-arr->unumber buffer :start 2 :end 4))
           (host-len (bits-arr->unumber buffer :start 4 :end 6))
           (port-len (bits-arr->unumber buffer :start 6 :end 8))
           (total-len (+ ulen plen host-len port-len))

           (body-buffer (make-array total-len :element-type '(unsigned-byte 8)))
           (body-size 0))

      (setf body-size (read-sequence body-buffer ssl-stream))
      (when (/= total-len body-size) (error "auth info error"))

      (let ((username (flexi-streams:octets-to-string (subseq body-buffer
                                                              0
                                                              ulen)))
            (password (flexi-streams:octets-to-string (subseq body-buffer
                                                              (+ ulen)
                                                              (+ ulen plen))))
            (host (flexi-streams:octets-to-string (subseq body-buffer
                                                          (+ ulen plen)
                                                          (+ ulen plen host-len))))
            (port (bits-arr->unumber (subseq body-buffer
                                             (+ ulen plen host-len)
                                             (+ ulen plen host-len port-len)))))

        ;; TODO 账户密码认证
        (if (and (string= username "abc")
                 (string= password "def"))
            
            (progn
              ;; 认证成功
              (write-sequence (make-array 1
                                          :element-type '(unsigned-byte 8)
                                          :initial-contents #(#B10))
                              ssl-stream)
              (force-output ssl-stream)
              (values host port))
            (progn
              ;; 认证失败
              (write-sequence (make-array 1
                                          :element-type '(unsigned-byte 8)
                                          :initial-contents #(#B00))
                              ssl-stream)
              (force-output ssl-stream)
              (error "auth failure")))))))
