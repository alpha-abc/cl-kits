#!/usr/bin/env sbcl --script


(defpackage :cl-script
  (:use :cl :cl-user)
  (:export
   :timestamp-to-date
   :date-to-timestamp
   :md5
   :json-pretty
   :base64-to-string
   :string-to-base64
   :ipv4-info
   :test))


(in-package :cl-script)

;; 脚本模式下需要手动加载quicklisp
(load "~/quicklisp/setup.lisp")

;; 依赖的第三方库
(ql:quickload '(:cl-ppcre :yason :cl-base64) :silent :prompt)

(defun main(argv)
  "脚本入口函数"
  (cond ((= 0 (length argv)) (return-from main (help nil)))
        (t (let ((sym (intern (string-upcase (nth 0 argv)))))
             (handler-case
                 (apply sym (cdr argv))
               ;; 优先匹配函数未定义错误
               (undefined-function (uf)
                 (format nil "~A:~%~A~%~%~A~%" (color-text 31 "未知命令") (color-text 31 (nth 0 argv)) (help nil)))
               (error (err)
                 (format nil "~A:~%~A~%~%~A~%" (color-text 31 "ERROR") (color-text 31 err) (help (list sym))))
               (condition (c)
                 (format nil "~A:~%~A~%~%~%~A~%" (color-text 31 "CONDITION") (color-text 31 c) (help (list sym)))))))))


(defun color-text (n text)
  "
30:黑
31:红
32:绿
33:黄
34:蓝色
35:紫色
36:深绿
37:白色
"
  (format nil "~c[~dm~A~c[0m" #\ESC n text #\ESC))


(defun help (cmd-sym-lst)
  (format nil "~A~%
cl.lisp 工具箱, 依赖sbcl以及quicklisp

用法:
~A ~A ~A

命令列表:
~{~%~A~%~}
"
          (color-text 34 "-=-=-=-=-=-=-=-=-")
          (color-text 31 "cl.lisp")
          (color-text 33 "<command>")
          (color-text 32 "[arguments]")
          (help-doc-lst cmd-sym-lst)))


(defun help-doc-lst (syms)
  "过滤掉为空的文档"
  (mapcan #'(lambda (p)
              (let ((doc (documentation p 'function)))
                (if (equalp nil doc)
                    nil
                    (list (format nil "~A:~%~A~%"
                                  (color-text 33 (string-downcase (symbol-name p)))
                                  doc)))))
          (if (= 0 (length syms))
              (find-all-cmd-symbols)
              syms)))


(defun find-all-cmd-symbols ()
  "查找所有命令符号, 约定只有命令符号才能被导出"
  (let ((resp-lst ()))
    (do-external-symbols (sym :cl-script resp-lst)
      (when (fboundp sym)
        (push sym resp-lst))
      resp-lst)))


(defun timestamp-to-date (&rest args)
  "将时间戳(单位:秒, 默认epoch: 1970)转换成易读时间格式

usage:
timestamp-to-date $timestamp [$epoch]

example:
timestamp-to-date 1607746332 1970"
  (multiple-value-bind (second minute hour date month year day-of-week dst-p timezone)
      (decode-universal-time
       (+
        (encode-universal-time 0
                               0
                               0
                               1
                               1
                               (if (nth 1 args) (parse-integer (nth 1 args)) 1970)
                               0)
        (parse-integer (nth 0 args))))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d ~a (GMT~@d)~%"
            year month date
            hour minute second
            (nth day-of-week
                 '("星期一/Monday"
                   "星期二/Tuesday"
                   "星期三/Wednesday"
                   "星期四/Thursday"
                   "星期五/Friday"
                   "星期六/Saturday"
                   "星期日/Sunday"))
            (- timezone))))


(defun date-to-timestamp (&rest args)
  "将日期(YYYY-MM-DD hh24:mm:ss)转换成时间戳(单位:秒, 默认epoch: 1900)

useage:
date-to-timestamp $date [$epoch]

example:
date-to-timestamp '2020-12-12 12:12:12' 1970"
  (let ((resp (cl-ppcre:split "(-| +|:)" (nth 0 args))))
    (format nil
            "~a~%"
            (-
             (encode-universal-time
              (parse-integer (nth 5 resp))
              (parse-integer (nth 4 resp))
              (parse-integer (nth 3 resp))
              (parse-integer (nth 2 resp))
              (parse-integer (nth 1 resp))
              (parse-integer (nth 0 resp)))
             (encode-universal-time 0
                                    0
                                    0
                                    1
                                    1
                                    (if (nth 1 args) (parse-integer (nth 1 args)) 1970)
                                    0)))))

;;;; 需要sb-md5模块, sbcl内置
(require :sb-md5)
(defun md5 (&rest args)
  "对输入信息(字符串)生成信息摘要, (其他计算途径比如shell命令: echo -n 'abc' | md5)

usage:
md5 $str

example:
md5 'abc'"
  (format nil
          ;; 末尾加入换行符, 并不是md5计算结果, 只是为了更好的结果展示
          "~{~A~}~%" 
          (reduce #'append
                  (map 'list
                       #'(lambda (p)
                           (let ((str "0123456789abcdef"))
                             (list (aref str (ldb '(8 . 4) p))
                                   (aref str (ldb '(4 . 0) p)))))
                       (sb-md5:md5sum-string (nth 0 args))))))


(defun json-pretty (&rest args)
  "对输入的json字符串格式化, 美化输出

usage:
json-pretty $json

example:
json-pretty '[1,2]'"
  (format nil "~A~%"
          (with-output-to-string (s)
            (yason:encode
             (yason:parse (nth 0 args))
             (yason:make-json-output-stream s :indent 4)))))


(defun base64-to-string(&rest args)
  "对输入的base64字符串解码

usage:
base64-to-string $str

example:
base64-to-string 'MTI='"
  (format nil "~A~%"
          (cl-base64:base64-string-to-string (nth 0 args))))


(defun string-to-base64(&rest args)
  "对输入的字符串base64化

usage:
string-to-base64 $base64

example:
string-to-base64 '12'"
  (format nil "~A~%"
          (cl-base64:string-to-base64-string (nth 0 args))))


(defun ipv4-info (&rest args)
  "对提供ipv4地址以及cidr详细的解释

useage:
ipv4-info $ipv4 $cidr

example:
ipv4-info '128.14.35.7' '20'"

  ;; 例子
  ;; 10000000.00001110.00100011.00000111 ip地址
  ;; 11111111.11111111.11110000.00000000 地址掩码
  ;; 10000000.00001110.00100000.00000000 最小地址
  ;; 10000000.00001110.00101111.11111111 最大地址

  (when (/= 2 (length args)) (error "参数错误"))
  
  (let ((ipv4 (cl-ppcre:all-matches-as-strings
               "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])"
               (nth 0 args)))
        (cidr (parse-integer (nth 1 args))))

    ;; 参数校验
    (when (< (length ipv4) 4) (error "非法IP"))
    (when (or (< cidr 0) (> cidr 32)) (error "cidr非法"))

    (let* (
           (n0-bits (unumber->bits-array (parse-integer (nth 0 ipv4)) :bit-length 1 :array-length 8))
           (n1-bits (unumber->bits-array (parse-integer (nth 1 ipv4)) :bit-length 1 :array-length 8))
           (n2-bits (unumber->bits-array (parse-integer (nth 2 ipv4)) :bit-length 1 :array-length 8))
           (n3-bits (unumber->bits-array (parse-integer (nth 3 ipv4)) :bit-length 1 :array-length 8))

           (mask-bits (create-mask cidr))
           (r-mask-bits (create-mask cidr :reverse t)))
      (format nil "IP(v4)总数
~A

IP(v4)地址
~8,'0B.~8,'0B.~8,'0B.~8,'0B - ~A.~A.~A.~A

地址掩码/子网掩码
~8,'0B.~8,'0B.~8,'0B.~8,'0B - ~A.~A.~A.~A (~A)

最小地址/网络地址
~8,'0B.~8,'0B.~8,'0B.~8,'0B - ~A.~A.~A.~A

最大地址/广播地址
~8,'0B.~8,'0B.~8,'0B.~8,'0B - ~A.~A.~A.~A
"
              ;;
              (expt 2 (- 32 cidr))
              
              ;;
              (bits-arr->unumber n0-bits :bit-length 1)
              (bits-arr->unumber n1-bits :bit-length 1)
              (bits-arr->unumber n2-bits :bit-length 1)
              (bits-arr->unumber n3-bits :bit-length 1)

              (bits-arr->unumber n0-bits :bit-length 1)
              (bits-arr->unumber n1-bits :bit-length 1)
              (bits-arr->unumber n2-bits :bit-length 1)
              (bits-arr->unumber n3-bits :bit-length 1)

              ;;
              (bits-arr->unumber (subseq mask-bits 0 8) :bit-length 1)
              (bits-arr->unumber (subseq mask-bits 8 16) :bit-length 1)
              (bits-arr->unumber (subseq mask-bits 16 24) :bit-length 1)
              (bits-arr->unumber (subseq mask-bits 24 32) :bit-length 1)
              
              (bits-arr->unumber (subseq mask-bits 0 8) :bit-length 1)
              (bits-arr->unumber (subseq mask-bits 8 16) :bit-length 1)
              (bits-arr->unumber (subseq mask-bits 16 24) :bit-length 1)
              (bits-arr->unumber (subseq mask-bits 24 32) :bit-length 1)

              cidr

              ;;
              (bits-arr->unumber (bit-and n0-bits (subseq mask-bits 0 8)) :bit-length 1)
              (bits-arr->unumber (bit-and n1-bits (subseq mask-bits 8 16)) :bit-length 1)
              (bits-arr->unumber (bit-and n2-bits (subseq mask-bits 16 24)) :bit-length 1)
              (bits-arr->unumber (bit-and n3-bits (subseq mask-bits 24 32)) :bit-length 1)

              (bits-arr->unumber (bit-and n0-bits (subseq mask-bits 0 8)) :bit-length 1)
              (bits-arr->unumber (bit-and n1-bits (subseq mask-bits 8 16)) :bit-length 1)
              (bits-arr->unumber (bit-and n2-bits (subseq mask-bits 16 24)) :bit-length 1)
              (bits-arr->unumber (bit-and n3-bits (subseq mask-bits 24 32)) :bit-length 1)

              ;;
              (bits-arr->unumber (bit-ior n0-bits (subseq r-mask-bits 0 8)) :bit-length 1)
              (bits-arr->unumber (bit-ior n1-bits (subseq r-mask-bits 8 16)) :bit-length 1)
              (bits-arr->unumber (bit-ior n2-bits (subseq r-mask-bits 16 24)) :bit-length 1)
              (bits-arr->unumber (bit-ior n3-bits (subseq r-mask-bits 24 32)) :bit-length 1)

              (bits-arr->unumber (bit-ior n0-bits (subseq r-mask-bits 0 8)) :bit-length 1)
              (bits-arr->unumber (bit-ior n1-bits (subseq r-mask-bits 8 16)) :bit-length 1)
              (bits-arr->unumber (bit-ior n2-bits (subseq r-mask-bits 16 24)) :bit-length 1)
              (bits-arr->unumber (bit-ior n3-bits (subseq r-mask-bits 24 32)) :bit-length 1)))))


(defun test (&rest args)
  (format nil "~a ~a~%" args (nth 3 args)))


;;;; 工具区域

(defun create-mask (net-len &key (reverse nil))
  "创建地址掩码信息
params:
  net-len: 网络地址长度
return:
  地址掩码数组信息"
  (let ((arr (make-array 32 :element-type '(unsigned-byte 1))))
    (loop
      for i from 0 to (1- (length arr))
      do
         (if reverse 
             (when (>= i net-len) (setf (aref arr i) 1))
             (when (< i net-len) (setf (aref arr i) 1))))
    arr))

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
  (when (< unumber 0) (error "unumber 小于0"))
  
  (multiple-value-bind (n1 n2) (floor (+ 1 (truncate (if (> unumber 0) (log unumber 2) (log 0 0)))) bit-length)
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


;;;;

;;;;

;;;; 运行脚本, 必须位于文件最后一行
(format t "~A" (main (cdr sb-ext:*posix-argv*)))




;;;; 测试区域, 所有代码最后需要删除







(cl-ppcre:all-matches-as-strings
 "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])"
 "255.1.1.255"
)

  

  (format nil "~B"
(ldb (byte 32 0) (ash (ldb (byte 32 0) -1) (- 32 24)))
)
