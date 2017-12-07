(defstruct object (x-location (random *level-width*)) (y-location (random *level-height*)) group type tile effect)

(defun make_objects (n)
    (setf *objects* (mapcar (lambda (x) (funcall (nth (random (length *item-builders*)) *item-builders*)))
      (make-list n))))
