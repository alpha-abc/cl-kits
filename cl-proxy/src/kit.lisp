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


(defun unumber->bits-array(unumber &key (bit-length 8))
  "将无符号整数转换成字节数组
params:
  bit-len: 每一个元素由<bit-len>位表示

return
  array
"
  (multiple-value-bind (n1 n2) (floor (+ 1 (truncate (log unumber 2))) bit-length)
    (let* ((size (+ n1 (if (> n2 0) 1 0)))
           (arr (make-array size :element-type `(unsigned-byte ,bit-length))))
      (loop for idx from (1- size) downto 0
            do
               (setf (aref arr idx)
                     (ldb (byte bit-length (* bit-length (- (1- size) idx))) unumber)))
      arr)))


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
(let ((arr (unumber->bits-array 65535 :bit-length 1)))
  (format t "ARR: ~A~%" arr)
  (format t "NUM: ~A~%" (bits-arr->unumber arr :bit-length 1)))
