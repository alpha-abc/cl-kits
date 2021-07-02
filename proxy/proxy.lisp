; 参考文档
; https://sinax.be/blog/lisp/programming-network-sockets-in-lisp.html
; https://stackoverflow.com/questions/31273674/pop3-over-ssl-tls-in-common-lisp

; Common LISP: How to open an SSL / TLS stream
; https://dataswamp.org/~solene/2016-09-26-21.html

(in-package :cl-user)

(defvar *stop* nil)

(defun force-print (destination control-string &rest format-arguments)
  (apply #'format destination control-string format-arguments)
  (finish-output nil))


(defun make-listen-socket (&optional (addr '(127 0 0 1)) (port 1082))
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :protocol :tcp
                               :type :stream)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket addr port)
    (sb-bsd-sockets:socket-listen socket 1)
    socket))


(defun tcp-server ()
  (let ((s (make-listen-socket)))
    (unwind-protect
         (loop
           (when *stop* (return))
           (let ((conn (sb-bsd-sockets:socket-accept s)))
             (sb-thread:make-thread #'(lambda () (do-connection conn)))))
      (sb-bsd-sockets:socket-close s))))


(defun do-connection (conn)
  (unwind-protect
       (let ((stream (sb-bsd-sockets:socket-make-stream
                      conn
                      :output t)))
         (force-print t "socks5 result ~A~%" (socks5-method conn))
         (force-print t "socks5 get user ~A~%" (socks5-get-user conn))
         (force-print t
                      "socks5 request ~A ~A~%"
                      (socks5-request conn)
                      ;(cadr (socks5-request conn))
                      "aaa"
                      )
;         (sb-bsd-sockets:socket-send conn
;                                     (make-array 2
;                                                 :initial-element 97
;                                                 :element-type '(unsigned-byte 8))
                                        ;                                     nil)
         )
    (sb-bsd-sockets:socket-close conn)))


(defun run-server ()
  (setf *stop* nil)
  (sb-thread:make-thread #'(lambda () (tcp-server))))


(defun stop-server ()
  (setf *stop* t))


(defun socks5 (conn)
  "获取
"

  )

(defun socks5-method (conn)
  "认证方法协商, 目前只支持 [USERNAME/PASSWORD]"
  (block ~
    (multiple-value-bind (byte-buff len)
        (sb-bsd-sockets:socket-receive conn
                                       (make-array 2 :element-type '(unsigned-byte 8))
                                       nil)

      (when (or (/= len 2)
                (/= (aref byte-buff 0) #X05))
        (error "socks version error"))
        
      (multiple-value-bind (byte-buff1 len1)
          (sb-bsd-sockets:socket-receive conn
                                         (make-array (aref byte-buff 1)
                                                     :element-type '(unsigned-byte 8))
                                         nil)
 
        (when (/= (aref byte-buff 1) len1)
          (error "nmethods data length error"))

        (loop
          for e across byte-buff1
          do
             (when (= e #X02)
               (sb-bsd-sockets:socket-send
                conn
                (make-array 2 :element-type '(unsigned-byte 8) :initial-contents `(#X05 ,e))
                nil)
               (return-from ~ nil)))

        (sb-bsd-sockets:socket-send conn
                                    (make-array 2 :element-type '(unsigned-byte 8)
                                                  :initial-contents '(#X05 #XFF))
                                    nil)
        (error "no acceptable methods")))))


(defun socks5-get-user (conn)
  "提取账户密码信息
return
  #(uname-bytes passwd-bytes)
"
  (let ((ulen nil)
        (plen nil)
        (uname-bytes nil)
        (passwd-bytes nil))
    (multiple-value-bind (buff len)
        (sb-bsd-sockets:socket-receive conn
                                       (make-array 2 :element-type '(unsigned-byte 8))
                                       nil)
      (when (or (/= len 2)
                (/= (aref buff 0) #X01))
        (error "subversion[username/password] error"))
      (setf ulen (aref buff 1)))
    
    (multiple-value-bind (buff len)
        (sb-bsd-sockets:socket-receive conn
                                       (make-array (+ 1 ulen)
                                                   :element-type '(unsigned-byte 8))
                                       nil)
      (when (or (/= len (+ ulen 1))) (error "read error"))
      (setf uname-bytes (subseq buff 0 ulen))
      (setf plen (aref buff ulen)))

    (multiple-value-bind (buff len)
        (sb-bsd-sockets:socket-receive conn
                                       (make-array plen :element-type '(unsigned-byte 8))
                                       nil)
      (when (or (/= len plen)) (error "read error"))
      (setf passwd-bytes buff))

    (sb-bsd-sockets:socket-send conn
                                (make-array 2
                                            :element-type '(unsigned-byte 8)
                                            :initial-contents '(#X01 #X00))
                                nil)
    (vector uname-bytes passwd-bytes)))


(defun socks5-request (conn)
  "获取目标地址信息
return:
  #(host-bytes port-bytes)"
  (let ((host-bytes nil)
        (port-bytes nil)
        (atyp nil))
    (multiple-value-bind (buff len)
        (sb-bsd-sockets:socket-receive conn
                                       ;; 4 = 1 VER + 1 CMD + 1 RSV + 1 ATYP
                                       (make-array 4 :element-type '(unsigned-byte 8))
                                       nil)
      (when (/= 4 len) (error "read (ver, cmd, rsv, atyp) error"))
      (setf atyp (let ((ver (aref buff 0))
                       (cmd (aref buff 1))
                       (atyp (aref buff 3)))
                   (when (/= ver #X05) (error "ver error"))
                   (when (/= cmd #X01) (error "cmd not support"))
                   atyp)))

    (cond ((= atyp #X01) ; ipv4
           (multiple-value-bind (buff len)
               (sb-bsd-sockets:socket-receive conn
                                              (make-array (+ 4 2);4 - ipv4 len, 2 - port len
                                                          :element-type '(unsigned-byte 8))
                                              nil)
             (when (/= len 6) (error "read ipv4 info error"))
             (setf host-bytes (subseq buff 0 4))
             (setf port-bytes (subseq buff 4 6))
             `(,host-bytes ,port-bytes)))

          ((= atyp #X04) ; ipv6
           (multiple-value-bind (buff len)
               (sb-bsd-sockets:socket-receive conn
                                              (make-array (+ 16 2);16 - ipv6 len, 2 - port len
                                                          :element-type '(unsigned-byte 8))
                nil)
             (when (/= len 18) (error "read ipv6 info error"))
             (setf host-bytes (subseq buff 0 16))
             (setf port-bytes (subseq buff 16 18))
             `(,host-bytes ,port-bytes)))

          ((= atyp #X03) ; DOMAINNAME
           (let ((l (multiple-value-bind (buff len)
                          (sb-bsd-sockets:socket-receive
                           conn
                           (make-array 1
                                       :element-type '(unsigned-byte 8))
                           nil)
                        (when (/= len 1) (error "read domainname len error"))
                        (aref buff 0))))
             (multiple-value-bind (buff len)
                 (sb-bsd-sockets:socket-receive
                  conn
                  (make-array (+ l 2);len - domainname len, 2 - port
                              :element-type '(unsigned-byte 8))
                  nil)
               (when (/= len (+ l 2)) (error "read domainname info error"))
               (setf host-bytes (subseq buff 0 l))
               (setf port-bytes (subseq buff l (+ l 2)))
               `(,host-bytes port-bytes))))
          
          (t (error "unsupport atyp")))

    (sb-bsd-sockets:socket-send conn
                                (make-array 10
                                            :element-type '(unsigned-byte 8)
                                            :initial-contents
                                            '(#X05 #X00 #X00 #X01 #X00
                                              #X00 #X00 #X00 #X00 #X00))
                                nil)
    (vector host-bytes port-bytes)))


(when nil

  (cl-project:make-project #P"/Users/tanshuai/devel/lisp/cl-kits/cl-proxy"
                           :author "alpha-abc"
                           :email "alpha-abc@outlook.com"
                           :license nil
                           :depends-on '(:usocket :cl+ssl))


  
  )
