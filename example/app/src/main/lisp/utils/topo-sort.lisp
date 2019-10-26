
;; L ← Empty list that will contain the sorted nodes
;; while exists nodes without a permanent mark do
;;     select an unmarked node n
;;     visit(n)

(defvar *sorted*)

(defun get-keys (plist)
  (loop for (key rest) on plist by #'cddr
     collect key))

(defun topo-sort (graph)
  (setf *sorted* ())

  (let ((keys (get-keys graph)))
    (dolist (node keys)
      (when (not (get node :pmark))
        (visit node graph))))

  *sorted*)

;; function visit(node n)
;;     if n has a permanent mark then return
;;     if n has a temporary mark then stop   (not a DAG)
;;     mark n with a temporary mark
;;     for each node m with an edge from n to m do
;;         visit(m)
;;     remove temporary mark from n
;;     mark n with a permanent mark
;;     add n to head of L

(defun visit (node graph)
  (when (get node :pmark)
    (return-from visit))
  (when (get node :mark)
    (error "not a DAG"))
  (setf (get node :mark) t)
  (dolist (m (getf graph node))
    (visit m graph))
  (setf (get node :mark) nil)
  (setf (get node :pmark) t)
  (push node *sorted*))
