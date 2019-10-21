;;;
;;; This file needs to be loaded in Android host ECL, in order to cross-compile
;;; EQL5 code.
;;;
;;; It defines all symbols and macros of EQL5, defining also dummy functions
;;; (stubs) for all wrapper function symbols and functions defined in
;;; "ecl_fun.cpp".
;;;
;;; Of course there are limitations to this approach: you can't have any EQL5
;;; function call in a top-level form; this would give you strange compile-time
;;; error messages; so just wrap any EQL5 code in a function, and make sure to
;;; call it only at run-time.
;;;
;;; Example:
;;;
;;;   (defvar *pix* (qnew "QPixmap"))  ; WRONG!
;;;
;;;   (defvar *pix* nil)               ; OK
;;;
;;;   (defun ini ()
;;;     (setf *pix* (qnew "QPixamp"))) ; OK
;;;

(defpackage :x
  (:use :common-lisp)
  (:export
   #:cc
   #:check-recompile
   #:bytes-to-string
   #:d
   #:do-string
   #:do-with
   #:empty-string
   #:ensure-list
   #:ends-with
   #:it
   #:it*
   #:if-it
   #:if-it*
   #:join
   #:let-it
   #:path
   #:split
   #:starts-with
   #:string-split
   #:string-substitute
   #:string-to-bytes
   #:when-it
   #:when-it*
   #:while
   #:while-it
   #:with-gensyms))

(provide :x)

(in-package :x)

(defmacro if-it (exp then &optional else)
  `(let ((it ,exp))
    (if it ,then ,else)))

(defmacro if-it* (exp then &optional else)
  `(let ((it* ,exp))
    (if it* ,then ,else)))

(defmacro let-it (exp &body body)
  `(let ((it ,exp))
     ,@body
     it))

(defmacro when-it (exp &body body)
  `(let ((it ,exp))
    (when it ,@body)))

(defmacro when-it* (exp &body body)
  `(let ((it* ,exp))
    (when it* ,@body)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (s)
                   `(,s (gensym)))
                 syms)
     ,@body))

(defmacro while (exp &body body)
  `(do ()
       ((not ,exp))
     ,@body))

(defmacro while-it (exp &body body)
  `(do ((it))
     ((not (setf it ,exp)))
     ,@body))

(defmacro do-string ((var str) &body body)
  `(map nil (lambda (,var)
              ,@body)
        ,str))

(defmacro do-with (with &body body)
  `(progn
     ,@(mapcar (lambda (line)
                 (append with (if (or (atom line)
                                      (eql 'quote (first line)))
                                  (list line)
                                  line)))
               body)))

(defun d (&rest args)
  "A simple debug print."
  (print (cons :debug args)))

(defun cc (&rest args)
  "Convenient string concatenation."
  (apply 'concatenate 'string args))

(defun empty-string (s)
  (zerop (length s)))

(defun %str-with (sub str starts)
  (let ((l1 (length str))
        (l2 (length sub)))
    (when (>= l1 l2)
      (string= sub (subseq str (if starts 0 (- l1 l2)) (when starts l2))))))

(defun starts-with (sub str)
  (%str-with sub str t))

(defun ends-with (sub str)
  (%str-with sub str nil))

(defun string-split (string separator)
  (let ((len (length separator))
        list)
    (do ((e (search separator string) (search separator string :start2 (+ e len)))
         (b 0 (+ e len)))
        ((not e) (push (subseq string b) list))
      (push (subseq string b e) list))
    (nreverse list)))

(defun string-substitute (new old string)
  (let ((len (length old)))
    (with-output-to-string (s)
      (do ((e (search old string) (search old string :start2 (+ e len)))
           (b 0 (+ e len)))
          ((not e) (write-string (subseq string b) s))
        (write-string (subseq string b e) s)
        (write-string new s)))))

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defun split (str &optional (sep #\Space))
  (unless (zerop (length str))
    (let (list)
      (do ((e (position sep str) (position sep str :start (1+ e)))
           (b 0 (1+ e)))
          ((not e) (push (subseq str b) list))
        (push (subseq str b e) list))
      (nreverse list))))

(defun join (list &optional (sep #\Space))
  (format nil (concatenate 'string "~{~A~^" (string sep) "~}")
          list))

(defun bytes-to-string (b)
  (map 'string 'code-char b))

(defun string-to-bytes (s)
  (map 'vector 'char-code s))

(defun path (name)
  "Needed because ECL uses base strings (not Unicode) for pathnames internally."
  #+(or darwin linux)
  (funcall (intern "QUTF8" :eql) name)
  #+win32
  (if (< (funcall (intern "%WINDOWS-VERSION" :eql)) #xa0)
      (funcall (intern "QLOCAL8BIT" :eql) name)           ; Windows 7 and lower
      name))                                              ; Windows 8 and higher

(defun check-recompile (file-name)
  "Given a global file name without file ending, ensures re-compiling on every ECL/Qt5/EQL5 version change."
  (labels ((ver-name ()
             (format nil "~A.ver" file-name))
           (version ()
             (multiple-value-bind (eql5 qt5)
                 (funcall (find-symbol "QVERSION" :eql))
               (format nil "EQL5 ~A (ECL ~A, Qt ~A)" eql5 (lisp-implementation-version) qt5)))
           (write-version ()
             (with-open-file (s (ver-name) :direction :output :if-exists :supersede)
               (princ (version) s)))
           (read-version ()
             (x:when-it (probe-file (ver-name))
               (with-open-file (s x:it :direction :input)
                 (read-line s)))))
    (unless (equal (version) (read-version))
      (compile-file file-name)
      (write-version)))
  file-name)


;;; The following are modified/simplified functions taken from "src/lsp/top.lsp" (see ECL sources)

(in-package :si)

(defun feed-top-level (form)
  (catch *quit-tag*
    (let ((*debugger-hook* nil)
          (*tpl-level* -1))
      (%tpl form))))

(defun %read-lines ()
  ;; allow multi-line expressions (command line option "-qtpl")
  (let (lines)
    (loop
      (let ((line (read-line)))
        (setf lines (if lines (format nil "~A~%~A" lines line) line))
        ;; test for balanced parenthesis; if yes, we have a READ-able expression
        ;; (see READ-FROM-STRING in EVAL-TOP-LEVEL)
        (multiple-value-bind (_ x)
            (ignore-errors
              (read-from-string (format nil "(~A)" (let ((lines* (copy-seq lines)))
                                                     (x:while-it (position #\\ lines*)
                                                       (setf lines* (replace lines* "  " :start1 x:it)))
                                                     (remove-if-not (lambda (ch)
                                                                      (find ch '(#\Space #\Newline #\( #\) #\" #\;)))
                                                                    lines*)))))
          (when (numberp x)
            (return (if (find (string-upcase lines) '("NIL" "()") :test 'string=) ; avoid strange BREAK on NIL values
                        "'()"
                        lines))))))))

(defun %tpl-read (&aux (*read-suppress* nil))
  (finish-output)
  (loop
    (case (peek-char nil *standard-input* nil :EOF)
      ((#\))
       (warn "Ignoring an unmatched right parenthesis.")
       (read-char))
      ((#\space #\tab)
       (read-char))
      ((#\newline #\return)
       (read-char)
       ;; avoid repeating prompt on successive empty lines:
       (let ((command (tpl-make-command :newline "")))
         (when command (return command))))
      (:EOF
       (terpri)
       (return (tpl-make-command :EOF "")))
      (#\:
       (let ((exp (read-preserving-whitespace)))
         (return (cond ((find exp '(:qq :exit))
                        "(eql:qquit)")
                       ((find exp '(:qa :abort))
                        "(eql:qquit -1)")
                       (t
                        tpl-make-command exp (read-line))))))
      (#\?
       (read-char)
       (case (peek-char nil *standard-input* nil :EOF)
         ((#\space #\tab #\newline #\return :EOF)
          (return (tpl-make-command :HELP (read-line))))
         (t
          (unread-char #\?)
          (return (read-preserving-whitespace)))))
      ;; We use READ-PRESERVING-WHITESPACE because with READ, if an
      ;; error happens within the reader, and we perform a ":C" or
      ;; (CONTINUE), the reader will wait for an inexistent #\Newline.
      (t
       (return (%read-lines))))))

(defun %break-where ()
  (when (> *tpl-level* 0)
    (tpl-print-current)))

(defun %tpl (form &key ((:commands *tpl-commands*) tpl-commands)
                       ((:prompt-hook *tpl-prompt-hook*) *tpl-prompt-hook*)
                       (broken-at nil)
                       (quiet nil))
  #-ecl-min
  (declare (c::policy-debug-ihs-frame))
  (let* ((*ihs-base* *ihs-top*)
         (*ihs-top* (if broken-at (ihs-search t broken-at) (ihs-top)))
         (*ihs-current* (if broken-at (ihs-prev *ihs-top*) *ihs-top*))
         (*frs-base* (or (sch-frs-base *frs-top* *ihs-base*) (1+ (frs-top))))
         (*frs-top* (frs-top))
         (*quit-tags* (cons *quit-tag* *quit-tags*))
         (*quit-tag* *quit-tags*)       ; any unique new value
         (*tpl-level* (1+ *tpl-level*))
         (break-level *break-level*)
         values -)
    (set-break-env)
    (set-current-ihs)
    (flet ((rep ()
             ;; We let warnings pass by this way "warn" does the
             ;; work.  It is conventional not to trap anything
             ;; that is not a SERIOUS-CONDITION. Otherwise we
             ;; would be interferring the behavior of code that relies
             ;; on conditions for communication (for instance our compiler)
             ;; and which expect nothing to happen by default.
             (handler-bind
                 ((serious-condition
                   (lambda (condition)
                     (cond ((< break-level 1)
                            ;; Toplevel should enter the debugger on any condition.
                            )
                           (*allow-recursive-debug*
                            ;; We are told to let the debugger handle this.
                            )
                           (t
                            (format t "~&Debugger received error of type: ~A~%~A~%~
                                         Error flushed.~%"
                                    (type-of condition) condition)
                            (clear-input)
                            (return-from rep t) ;; go back into the debugger loop.
                            )
                           )
                     )))

               (with-grabbed-console
                   (unless quiet
                     (%break-where)
                     (setf quiet t))
                 (if form
                     (setq - form
                           form nil)
                     (setq - (locally (declare (notinline tpl-read))
                               (tpl-prompt)
                               (tpl-read))))
                 (setq values (multiple-value-list
                               (eval-with-env - *break-env*))
                       /// // // / / values *** ** ** * * (car /) +++ ++ ++ + + -)
                 (tpl-print values)))))
      (when
          (catch *quit-tag*
            (if (zerop break-level)
                (with-simple-restart
                    (restart-toplevel "Go back to Top-Level REPL.")
                  (rep))
                (with-simple-restart
                    (restart-debugger "Go back to debugger level ~D." break-level)
                  (rep)))
            nil)
        (setf quiet nil)))))
