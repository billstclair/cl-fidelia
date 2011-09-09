(in-package :cl-user)

(unless (find-package "QUICKLISP")
  (load "~/quicklisp/setup"))

(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "cl-fidelia" ))

(in-package :cl-fidelia)

(format-fidelia-song (get-fidelia-song))

(ccl:quit)
