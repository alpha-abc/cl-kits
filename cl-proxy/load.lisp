(in-package :cl-user)

;; 快速加载项目
(when t
  (pushnew #P"~/devel/lisp/cl-kits/cl-proxy" ql:*local-project-directories*)
  (ql:register-local-projects)
  (ql:quickload :cl-proxy))
