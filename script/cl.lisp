#!/usr/bin/env sbcl --script


(defpackage :cl-script
  (:use :cl :cl-user)
  (:export
   :timestamp2date
   :date2timestamp
   :md5
   :json-pretty
   :test))


(in-package :cl-script)

;; 脚本模式下需要手动加载quicklisp
(load "~/quicklisp/setup.lisp")

;; 依赖的第三方库
(ql:quickload :cl-ppcre :silent :prompt)
(ql:quickload :yason :silent :prompt)

(defun main(argv)
  "脚本入口函数"
  (cond ((= 0 (length argv)) (return-from main (help nil)))
        (t (let ((sym (intern (string-upcase (nth 0 argv)))))
             (handler-case
                 (apply sym (cdr argv))
               ;; 优先匹配函数未定义错误
               (undefined-function (uf)
                 (format nil "未知命令: ~A~%~%~A~%" (nth 0 argv) (help nil)))
               (error (err)
                 (format nil "ERROR: ~A~%~%~A~%" err (help (list sym))))
               (condition (c)
                 (format nil "CONDITION: ~A~%~%~%~A~%" c (help (list sym)))))))))


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
  (format nil "-=-=-=-=-
cl.lisp 工具箱, 依赖sbcl以及quicklisp

用法:
~A ~A ~A

命令列表:
~{~%~A~%~}
"
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


(defun timestamp2date (&rest args)
  "将时间戳(单位:秒, 默认epoch: 1970)转换成易读时间格式

usage:
timestamp2date $timestamp [$epoch]

example:
timestamp2date 1607746332 1970"
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


(defun date2timestamp (&rest args)
  "将日期(YYYY-MM-DD hh24:mm:ss)转换成时间戳(单位:秒, 默认epoch: 1900)

useage:
date2timestamp $date [$epoch]

example:
date2timestamp '2020-12-12 12:12:12' 1970"
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
  (format nil "~a~%"
          (with-output-to-string (s)
            (yason:encode
             (yason:parse (nth 0 args))
             (yason:make-json-output-stream s :indent 4)))))


(defun test (&rest args)
  (format nil "~a ~a~%" args (nth 3 args)))

;;;; 运行脚本, 必须位于文件最后一行
(format t "~A" (main (cdr sb-ext:*posix-argv*)))




;;;; 测试区域, 所有代码最后需要删除







