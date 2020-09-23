;;; hack to collect all file names from ASDF system for cross-compiling

(load "../utils/utils")

(pushnew :android *features*)

(require :asdf)
;;(require :sockets)
(require :sb-bsd-sockets)
(require :ecl-quicklisp)

(defvar *target*)
(defvar *type*)

(let* ((cmd-args (ext:command-args))
       (type-pos (position "--type" cmd-args :test #'string=))
       (type (if type-pos (nth (+ type-pos 1) cmd-args) "undefined"))
       (target-pos (position "--target" cmd-args :test #'string=)))
  (if target-pos
      (setf *target* (nth (+ target-pos 1) cmd-args))
      (error "need to specify --target on command-line"))
  (setf *type* (makekw type)))

(format t "*target* = ~s~%" *target*)
(format t "*type* = ~s~%" *type*)

(defparameter *pwd* (si:getenv "PWD"))
(defparameter *files-location* (format nil "~a.flist" *target*))
(defparameter *dependencies-location* "dependencies.lst")

(defun save-dependencies (system-designator system)
  (let ((depends-on (map 'list #'makekw (asdf:system-depends-on system))))
    (with-open-file (d *dependencies-location* :direction :output :if-exists :append :if-does-not-exist :create)
      (format d "~s (~{~s~^ ~})~%" system-designator depends-on))))

(ext:package-lock :common-lisp nil)

(defvar *load-orig* (symbol-function 'load))
(defvar *files*     nil)

;; redefine LOAD
(defun load (&rest args)
  (let* ((str (namestring (first args)))
         (name (subseq str 0 (position #\. str :from-end t))))
    (when (search ".cache/" name)
      (push (print name) *files*)))
  (apply *load-orig* args))

(ext:package-lock :common-lisp t)

;; find out where asdf caches files, so we can strip it off the front later
(defvar *cache-location-index*)
(let* ((current-file (namestring *load-truename*))
       (cache-file-location (namestring (asdf:apply-output-translations current-file)))
       (index (search current-file cache-file-location :from-end t))
       (cache-location (subseq cache-file-location 0 index)))
  (setf *cache-location-index* (length cache-location)))

;; load here (not earlier)
(when (probe-file (format nil "~a/~a.deps" *pwd* *target*))
  (format t "~&loading ~a.deps file~%" *target*)
  (load (format nil "~a/~a.deps" *pwd* *target*)))

(push "./" asdf:*central-registry*)
(push (format nil "~a/" *pwd*) asdf:*central-registry*)

(let ((system-designator (makekw *target*)))

  ;; dummy load to collect all file names
  (asdf:load-system system-designator)

  (let* ((target-base (namestring (asdf:system-source-directory system-designator)))
         (system (asdf:find-system system-designator)))

    (save-dependencies system-designator system)

    ;; extract file name from compile cache name and save file list
    (format t "files: ~s" *files-location*)
    (with-open-file (s *files-location* :direction :output :if-exists :supersede)
      (when (eq *type* :separate)
        (write-line target-base s))
      (setf *files* (nreverse *files*))
      (dolist (file *files*)
        (write-line (subseq file *cache-location-index*) s)))))

(format t "~%~D files~%~%" (length *files*))
