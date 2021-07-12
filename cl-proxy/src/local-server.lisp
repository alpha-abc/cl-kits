;[本机/用户端]服务
(in-package :cl-proxy)


(defvar *local-server-thread* nil)


(defun start-local-server (&optional (host #(0 0 0 0)) (port 1082))
  "启动[local]TCP服务"

  ;;(log:config :daily "~/cl-local.log" :backup nil)
  (log:config :sane2)
  (log:info "start local server")
  
  (multiple-value-bind (thread)
      (usocket:socket-server host port
                             'local-tcp-handler
                             nil
                             :in-new-thread t
                             :protocol :stream
                             :reuse-address t
                             :multi-threading t
                             :element-type '(unsigned-byte 8))
    (setf *local-server-thread* thread)))


(defun stop-local-server ()
  "停止[local]TCP服务"
  (block nil
      (when (or (not *local-server-thread*)
                (not (bt:thread-alive-p *local-server-thread*)))
        (return))
    (bt:destroy-thread *local-server-thread*)
    (setf *local-server-thread* nil)))


(defun restart-local-server()
  "重启[local]TCP服务"
  (stop-local-server)
  (start-local-server))


(defun local-tcp-handler (stream)
  "处理每一个tcp连接"

  (handler-case
      (multiple-value-bind (uname-bs passwd-bs target-host-bs target-port-bs)
          ;; socks5协议解析
          (socks5 stream)

        ;; https://dataswamp.org/~solene/2016-09-26-21.html
        (usocket:with-client-socket (socket r-stream
                                            #(127 0 0 1) 10020 ;; 代理服务器地址以及端口信息
                                            :element-type '(unsigned-byte 8)
                                            :protocol :stream)

          (let ((ssl-stream (cl+ssl:make-ssl-client-stream
                             r-stream
                             :verify nil
                             :unwrap-stream-p t)))
            ;; 与代理服务器协商认证
            (local-auth ssl-stream uname-bs passwd-bs target-host-bs target-port-bs)
            (log:info "socks5 username/password auth success")

            (let ((t1 (bt:make-thread
                       #'(lambda ()
                           (io-byte-copy stream ssl-stream))
                       :name (make-thread-name "LOCAL->PROXY")))
                  (t2 (bt:make-thread
                       #'(lambda ()
                           (io-byte-copy ssl-stream stream))
                       :name (make-thread-name "PROXY->LOCAL"))))

              (bt:join-thread t1)
              (bt:join-thread t2)))))
    (condition (c)
      nil)))


(defun local-auth (ssl-stream uname-bs passwd-bs host-bs port-bs)
  "[local]与[proxy]协议格式
uname-bs: 用2字节表示长度
passwd-bs: 用2字节表示长度
host-bs: 用2字节表示长度
port-bs: 用2字节表示长度
|2bytes|2bytes|2bytes|2bytes|data|"

  (let ((data (concatenate 'vector
                           (unumber->bits-array (length uname-bs) :array-length 2)
                           (unumber->bits-array (length passwd-bs) :array-length 2)
                           (unumber->bits-array (length host-bs) :array-length 2)
                           (unumber->bits-array (length port-bs) :array-length 2)
                           uname-bs passwd-bs host-bs port-bs)))
    (write-sequence data ssl-stream)
    (force-output ssl-stream))

  ;; 认证结果用1字节表示, #B00 - 失败(对端会关闭连接), 其他 - 成功
  (let ((buffer (make-array 1 :element-type '(unsigned-byte 8)))
        (size 0))
    (setf size (read-sequence buffer ssl-stream))
    (when (/= 1 size) (error "read auth resp error"))
    (when (= #B00 (aref buffer 0)) (error "auth failure"))

    (log:info "认证结果[0/失败, 其他/成功] ~A" (aref buffer 0))
    ;; 认证结束, 继续后续copy流程
    ))



#+TEST
(let ((arr (make-array 10 :fill-pointer #b0 :adjustable t )))
  (vector-push (make-array 2 :initial-contents #(#B1 #B1)) arr)
  arr)


#+TEST
(append (make-array 2 :initial-contents '(1 2))
        (make-array 2 :initial-contents '(3 4))
        '()
        )


