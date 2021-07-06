(in-package :cl-proxy)

(defconstant +SOCKS5-VER+ #X05)


(defun socks5 (stream)
  "接入socks5协议
1 协商认证方法, 目前只支持[USERNAME/PASSWORD]
2 获取username / password
3 获取目的地址

return
  #(username password dist-host dist-port)"
  (let ((uname-bs nil)
        (passwd-bs nil)
        (host-bs nil)
        (port-bs nil))
    (socks5-method stream)

    (force-format t "~A~%" "方法协商完成1")

    (multiple-value-bind (u p) (socks5-get-user stream)
      (setf uname-bs u)
      (setf passwd-bs p))

    (multiple-value-bind (h p) (socks5-get-addr stream)
      (setf host-bs h)
      (setf port-bs p))

    (values uname-bs passwd-bs host-bs port-bs)))


(defun socks5-method (stream)
  "认证方法协商, 目前只支持 [USERNAME/PASSWORD]"
  (let ((buffer (make-array 2 :element-type '(unsigned-byte 8)))
        (size 0)
        (nmethods 0))
    (setf size (read-sequence buffer stream))
    (when (/= 2 size) (error "read VER, NMETHODS size error"))
    (when (/= +SOCKS5-VER+ (aref buffer 0)) (error "socks version error"))

    (setf nmethods (aref buffer 1))

    (let ((methods-buffer (make-array nmethods :element-type '(unsigned-byte 8)))
          (method nil))
      (read-sequence methods-buffer stream)
      (setf method (loop for method across methods-buffer
                         do (when (= method #X02) (return method))))

      (write-sequence (make-array 2
                                  :element-type '(unsigned-byte 8)
                                  :initial-contents `(#X05 ,(if (not method) #XFF method)))
                      stream)
      (force-output stream)
      ;; 中断流程
      (when (not method) (error "no acceptable methods")))))


(defun socks5-get-user (stream)
  "获取账户与密码信息
return
  values (username-bytes password-bytes)"

  (let ((buffer (make-array 2 :element-type '(unsigned-byte 8)))
        (size 0)
        (ulen 0))
    (setf size (read-sequence buffer stream))
    (force-format t "size info ~A~%" size)
    (when (/= 2 size) (error "read VER, ULEN size error"))
    (when (/= #X01 (aref buffer 0)) (error "socks subversion[username/password] error"))
    (setf ulen (aref buffer 1))

    (let ((buffer1 (make-array (+ ulen 1) :element-type '(unsigned-byte 8)))
          (size1 0)
          (uname-bs nil)
          (plen 0))
      (setf size1 (read-sequence buffer1 stream))
      (when (/= (+ ulen 1) size1) (error "read UNAME, PLEN size error"))
      ;; get username here
      (setf uname-bs (subseq buffer1 0 ulen))
      (setf plen (aref buffer1 ulen))

      (let ((buffer2 (make-array plen :element-type '(unsigned-byte 8)))
            (passwd-bs nil)
            (size2 0))
        (setf size2 (read-sequence buffer2 stream))
        (when (/= plen size2) (error "read PASSWOED error"))
        ;; get password here
        (setf passwd-bs buffer2)

        (write-sequence (make-array 2
                                    :element-type '(unsigned-byte 8)
                                    :initial-contents '(#X01 #X00))
                        stream)
        (force-output stream)
        
        (values uname-bs passwd-bs)))))


(defun socks5-get-addr (stream)
  "获取需要代理的目的地址
return
  value (host-byte port-byte)

其中port-byte一共由2字节表示(2 byte / 16 bit)(大端)"

  (let ((buffer nil)
        (size 0)
        (atyp nil)
        (host-bs nil)
        (port-bs nil))

    (setf buffer (make-array 4 :element-type '(unsigned-byte 8)))
    (setf size (read-sequence buffer stream))
    (when (/= 4 size) (error "read VER, CMD, RSV, ATYP size error"))

    (when (/= +SOCKS5-VER+ (aref buffer 0)) (error "socks version error"))
    (when (/= #X01 (aref buffer 1)) (error "unsupport cmd"))

    (setf atyp (aref buffer 3))

    (case atyp
      (#X01 ; ipv4
       (block nil
         (setf buffer (make-array (+ 4 2) ; 4 - IPv4-LEN, 2 - PORT-LEN
                                  :element-type '(unsigned-byte 8)))
         (setf size (read-sequence buffer stream))

         (when (/= size (+ 4 2)) (error "read ipv4 info error"))

         (setf host-bs (subseq buffer 0 4))
         (setf port-bs (subseq buffer 4 6))))
      (#X04 ; ipv6
       (block nil
         (setf buffer (make-array (+ 16 2) ; 16 - IPv6-LEN, 2 - PORT-LEN
                                  :element-type '(unsigned-byte 8)))
         (setf size (read-sequence buffer stream))

         (when (/= size (+ 16 2)) (error "read ipv6 info error"))

         (values (subseq buffer 0 16) (subseq buffer 16 18))

         (setf host-bs (subseq buffer 0 16))
         (setf port-bs (subseq buffer 16 18))))
      (#X03 ; domainname
       (block nil
         (setf buffer (make-array 1 :element-type '(unsigned-byte 8)))
         (setf size (read-sequence buffer stream))
         (when (/= 1 size) (error "read domain name size error"))

         (let ((host-len (aref buffer 0)))
           (setf buffer (make-array (+ host-len 2) :element-type '(unsigned-byte 8)))
           (setf size (read-sequence buffer stream))
           (when (/= size (+ host-len 2)) (error "read domain name error"))


           (setf host-bs (subseq buffer 0 host-len))
           (setf port-bs (subseq buffer host-len (+ host-len 2))))))
      (t (error "invalid atype")))

    ;; 成功响应
    (write-sequence (make-array 10
                                :element-type '(unsigned-byte 8)
                                :initial-contents
                                '(#X05 #X00 #X00 #X01 #X00
                                  #X00 #X00 #X00 #X00 #X00))
                    stream)
    (force-output stream)
    (values host-bs port-bs)))
