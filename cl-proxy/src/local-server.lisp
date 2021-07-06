;[本机/用户端]服务
(in-package :cl-proxy)


(defvar *local-server-thread* nil)


(defun start-local-server (&optional (host #(0 0 0 0)) (port 1082))
  "启动[local]TCP服务"
  (multiple-value-bind (thread)
      (usocket:socket-server host
                             port
                             #'local-tcp-handler
                             nil
                             :in-new-thread :t
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
  (local-stop-server)
  (local-start-server))


(defun local-tcp-handler (stream)
  "处理每一个tcp连接"
  (multiple-value-bind (uname-bs passwd-bs host-bs port-bs)
      (socks5 stream)

    ;; https://dataswamp.org/~solene/2016-09-26-21.html
    (usocket:with-client-socket (socket stream '(127 0 0 1) 10020)
      (let ((ssl-stream (cl+ssl:make-ssl-client-stream stream :unwrap-stream-p t)))
        (local-auth ssl-stream uname-bs passwd-bs host-bs port-bs)))
    ))


(defun local-auth (ssl-stream uname-bs passwd-bs host-bs port-bs)
  "[local]与[proxy]协议格式
uname-bs: 用2字节表示长度
passwd-bs: 用2字节表示长度
host-bs: 用2字节表示长度
port-bs: 用2字节表示长度
|2bytes|2bytes|2bytes|2bytes|data|"
  (let* ((ulen (length uname-bs))
         (plen (length passwd-bs))
         (host-len (length host-bs))
         (port-len (length port-bs))

         (data (make-array (+ 8 ulen plen host-len port-len) :element-type '(unsigned-byte 8))))

    (vector-push )
    
    )
  )


#+TEST
(let ((arr (make-array 10 :fill-pointer #b0 :adjustable t )))
  (vector-push (make-array 2 :initial-contents #(#B1 #B1)) arr)
  arr)


#+TEST
(append (make-array 2 :initial-contents '(1 2))
        (make-array 2 :initial-contents '(3 4))
        '()
        )

