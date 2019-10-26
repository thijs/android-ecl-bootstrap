(load "../utils/utils")
(load "../utils/topo-sort")


(defparameter *dependencies-location* "dependencies.lst")

(let* ((cmd-args (ext:command-args))
       (type-pos (position "--type" cmd-args :test #'string=))
       (type (makekw (if type-pos (nth (+ type-pos 1) cmd-args) "undefined")))
       (modules (map 'list (lambda (module)
                             (let ((filename (file-namestring module)))
                               (subseq filename 0 (- (length filename) 4))))
                     (directory "fas/*.fas")))
       (mod-keys (map 'list #'makekw modules)))

  (when (eq type :separate)
    (let* ((dependencies (read-from-string (format nil "(~a)" (slurp-file *dependencies-location*))))
           (ordered (topo-sort dependencies))
           (count (length modules))
           (sorted-modules))

      (dolist (module ordered)
        (let ((module-name (string-downcase (format nil "~a" module))))
          (when (member module-name modules :test #'string=)
            (setf modules (remove module-name modules :test #'string=))
            (push module-name sorted-modules))))

      (setf modules sorted-modules)

      (when (not (= count (length modules)))
        (error "We lost or gained some modules along the way..."))))

  (with-open-file (o "../fas/load.lisp" :direction :output :if-exists :supersede)
    (format o "(let ((modules (list ")
    (format o "~{\"~a\"~^ ~}" modules)
    (format o "))~%")
    (format o "      (dir (si:getenv \"ECLDIR\")))~%")
    (format o "  (dolist (module modules)~%")
    (format o "    (android-log (format nil \"found ~~a: ~~a\" module (truename (format nil \"~~a~~a.fas\" dir module))))~%")
    (format o "    (android-log (format nil \"loading ~~a: ~~a\" module (load (format nil \"~~a~~a.fas\" dir module))))))~%")))
