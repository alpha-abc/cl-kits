(in-package :cl-proxy)


(defun force-format (destination control-string &rest format-arguments)
  "用于测试线程内日志输出"
  (apply #'format destination control-string format-arguments)
  (finish-output nil))


(defun bits-arr->unumber (arr &key (bit-length 8) (start 0) (end (length arr)))
  "将array转换成无符号整数(大端模式/big endian)
params:
  bit-len: 每一个元素由<bit-len>位表示
  start: 左闭
  end: 右开

return:
  无符号数字"
  (let ((n 0))
    (loop for byte-idx from (1- end) downto start
          do
             (setf (ldb (byte bit-length (* bit-length (- (1- end) byte-idx))) n)
                   (aref arr byte-idx)))
    n))


(defun unumber->bits-array(unumber &key (bit-length 8) (array-length -1))
  "将无符号整数转换成字节数组
params:
  bit-length: 每一个元素由<bit-len>位表示
  array-length: 指定结果数组大小, 但不能低于正常结果集大小

return
  array
"
  (multiple-value-bind (n1 n2) (floor (+ 1 (truncate (log unumber 2))) bit-length)
    (let ((size (+ n1 (if (> n2 0) 1 0))))
      (when (/= -1 array-length)
        (if (< array-length size)
            (error "array-length error, min length ~A but ~A~%" size array-length)
            (setf size array-length)))
      (let ((arr (make-array size :element-type `(unsigned-byte ,bit-length))))
        (loop for idx from (1- size) downto 0
              do
                 (setf (aref arr idx)
                       (ldb (byte bit-length (* bit-length (- (1- size) idx))) unumber)))
        arr))))


(defun io-copy-alpha (source target &key info)
  ""
  (force-format t "COPY BEGIN ~A~%" info)
  (let ((buffer (make-array 512 :element-type (stream-element-type source))))
    (loop for pos = (read-sequence buffer source)
          while (plusp pos)
          do
             (force-format t "COPYING ~A ~A~%" info pos)
             (progn
               (write-sequence buffer target :end pos)
               (force-output target))))
  (force-format t "COPY END ~A~%" info)
  nil)

(defun io-byte-copy (source target)
  "无缓冲, 按每个字节copy"
  (handler-case
      (loop for b = (read-byte source nil 'eof)
            while (not (eq b 'eof))
            do
               (progn
                 (write-byte b target)
                 (force-output target)))
    (condition (c)
      nil)))


#+test
(let ((k 1))
  (case k
    (1 "A")
    (2 "B")
    (t "C")
    ))

#+TEST
(let ((i 0))
  (setf (ldb (byte 8 0) i) 187)
  (setf (ldb (byte 8 8) i) 1)
  i)

#+TEST
(ldb (byte 1 19) 6)

#+TEST
(let ((arr (unumber->bits-array 65535 :bit-length 1 :array-length 9)))
  (format t "ARR: ~A~%" arr)
  (format t "NUM: ~A~%" (bits-arr->unumber arr :bit-length 1)))

