; -*- mode: lisp -*-

(in-package :cl-fidelia)

(defvar *fidelia-port* 8383)
(defvar *fidelia-host* "127.0.0.1")
(defvar *fidelia-ignore-count* 5)

(defun sample-fidelia (file)
  (let ((s (ccl:make-socket :remote-host *fidelia-host* :remote-port *fidelia-port*))
        (f (open file :direction :output :if-exists :supersede))
        (cnt 0))
    (sleep 0.25)
    (unwind-protect
         (loop for ch = (read-char-no-hang s)
            do
              (unless ch (return))
              (when (>= (incf cnt) *fidelia-ignore-count*)
                (write-char ch f)))
      (close f)
      (close s))))

(defstruct fidelia-song
  name
  artist
  is-playing-p
  length
  position)

(defun format-fidelia-song (song &optional (stream t))
  (unless song (return-from format-fidelia-song))
  (check-type song fidelia-song)
  (format stream "\"~a\" by ~a (~a/~a~a)"
          (fidelia-song-name song)
          (fidelia-song-artist song)
          (fidelia-song-position song)
          (fidelia-song-length song)
          (if (fidelia-song-is-playing-p song) "" ", paused")))

(defun parse-fidelia-xml (xml)
  (let* ((dict (cdr (assoc :|dict| xml)))
         (array (cdr (assoc :|array| dict)))
         (values (cdr (member '(:|string| "hostname") array :test #'equal)))
         (name (second (first values)))
         (artist (second (third values)))
         (length (read-from-string (second (fourth values))))
         (is-playing-p (fifth values))
         (position (sixth values)))
    (cond ((keywordp is-playing-p)
           (setf is-playing-p (eq is-playing-p :|true|)))
          (t (setf position is-playing-p
                   is-playing-p nil)))
    (setf position (read-from-string (second position)))
    (flet ((time-string (x)
             (format nil "~d:~2,'0d"
                     (floor x 60)
                     (floor (mod x 60)))))
      (and name artist
           (values (make-fidelia-song
                    :name name
                    :artist artist
                    :is-playing-p is-playing-p
                    :length (time-string length)
                    :position (time-string (* position length)))
                   ;;xml
                   )))))

(defun get-fidelia-song (&optional (file "temp.plist"))
  (ignore-errors
    (sample-fidelia file)
    (let* ((path (probe-file file))
           (name (namestring path))
           (process (ccl:run-program
                     "/usr/bin/plutil"
                     `("-convert"
                       "xml1"
                       ,name))))
      (multiple-value-bind (status exit-code)
          (ccl:external-process-status process)
        (assert (and (eq status :exited)
                     (eql exit-code 0))))
      (with-open-file (s path)
        (parse-fidelia-xml (s-xml:parse-xml-dom s :lxml))))))
