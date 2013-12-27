;;;; cl-pid.lisp

(in-package #:cl-pid)

;;; "cl-pid" goes here. Hacks and glory await!

(defvar *test* nil)

(defclass pid ()
  ((kp :initarg :kp :initform 2 :accessor kp)
   (ki :initarg :ki :initform 0 :accessor ki)
   (kd :initarg :kd :initform 1 :accessor kd)
   (derivator :initarg :derivator :initform 0 :accessor derivator)
   (integrator :initarg :integrator :initform 0 :accessor integrator)
   (integrator-max :initarg :integrator-max :initform 500 :accessor integrator-max)
   (integrator-min :initarg :integrator-min :initform -500 :accessor integrator-min)
   (set-point :initarg :set-point :initform 0 :accessor set-point)
   (fault :initarg :fault :initform 0 :accessor fault)))

(defgeneric update (pid current-value)
  (:documentation "Update funktion fuer PID-Controller Object"))

(defmethod update ((pid pid) current-value)
  (let* ((fault (- (set-point pid) current-value))
	 (p-value (* (kp pid) fault))
	 (d-value (* (kd pid) (- fault (derivator pid))))
	 (i-value nil))

    (setf (fault pid) fault)
    (setf (derivator pid) fault)

    (setf (integrator pid) (+ (integrator pid) fault))

    (when (> (integrator pid) (integrator-max pid))
      (setf (integrator pid) (integrator-max pid)))

    (when (< (integrator pid) (integrator-min pid))
      (setf (integrator pid) (integrator-min pid)))

    (setf i-value (* (integrator pid) (ki pid)))

    (+ p-value i-value d-value)))
    
  
(defun pid-init ()
  (setf *test* (make-instance 'pid :set-point 60)))

(defun pid-update (current-value)
  (print (update *test* current-value)))

  
