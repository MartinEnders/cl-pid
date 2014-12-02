;;;; cl-pid.lisp

(in-package #:cl-pid)

;;; "cl-pid" goes here. Hacks and glory await!


;;;
;;; Class definition
;;; Class pid is the central PID Controller class
;;; pid is subclassed for each implementation of pid-controller
;;; 
;;; Usage: 
;;;         (in-package :cl-pid)
;;;         (defparameter *easy-pid* (make-instance 'pid :set-point 60)) ; set-point is the target value
;;;         (update *easy-pid* [current-value]) ; update returns the output of the PID-Controller
;;;

(defclass pid ()
  ((kp :initarg :kp :initform 2 :accessor kp :documentation "Proportional gain, tuning parameter")
   (ki :initarg :ki :initform 1 :accessor ki :documentation "Integral gain, tuning parameter")
   (kd :initarg :kd :initform 1 :accessor kd :documentation "Derivative gain, tuning parameter")
   (derivator :initarg :derivator :initform 0 :accessor derivator :documentation "Previous fault")
   (integrator :initarg :integrator :initform 0 :accessor integrator)
   (integrator-max :initarg :integrator-max :initform 500 :accessor integrator-max)
   (integrator-min :initarg :integrator-min :initform -500 :accessor integrator-min)
   (set-point :initarg :set-point :initform 0 :accessor set-point)
   (fault :initarg :fault :initform 0 :accessor fault :documentation "Error -> Difference between current value and set-point")))

(defclass pid-b (pid) nil)
(defclass pid-c (pid) nil)

(defmethod print-object ((object pid) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (kp ki kd derivator integrator integrator-max integrator-min set-point fault) object
      (format stream "kp: ~s
ki: ~s
kd: ~s
derivator: ~s
integrator: ~s
integrator-max: ~s
integrator-min: ~s
set-point: ~s
fault: ~s
" kp ki kd derivator integrator integrator-max integrator-min set-point fault))))

(defgeneric update (pid current-value)
  (:documentation "Update function for PID-Controller Object"))



;;;
;;; Easy PID-Controller
;;;


;;;
;;; Type A PID-Controller (reimplementation of the code on http://www.vandelogt.nl/htm/regelen_pid_uk.htm)
;;;
(defmethod update ((pid pid) current-value)
  "Current Version ignores integrator-max and integrator-min"
  (let ((p-value nil)
	(d-value nil)
	(i-value nil))
    (with-slots (kp ki kd derivator integrator integrator-max integrator-min set-point fault) pid
      ;; Prepare PID-Object for current update
      (setf fault      (- set-point current-value) ;; set error in PID-Object
	    integrator (+ integrator fault))       ;; set integrator-value in PID-Object
       
      ;; Calculate the PID Terms
      (setf p-value (* kp fault)
	    i-value (* integrator ki )
	    d-value (* (- fault derivator) kd))


      ;; Prepare PID-Object for next update
      (setf derivator fault)

      ;;(format t "p: ~A~%i: ~A~%d: ~A~%" p-value i-value d-value)
      ;;(print pid)

      (+ p-value i-value d-value))))


;;;
;;; Type B PID-Controller (reimplementation of the code on http://www.vandelogt.nl/htm/regelen_pid_uk.htm)
;;;
(defmethod update ((pid pid-b) current-value)
  "Current Version ignores integrator-max and integrator-min"
  (let ((p-value nil)
	(d-value nil)
	(i-value nil))
    (with-slots (kp ki kd derivator integrator integrator-max integrator-min set-point fault) pid
      ;; Prepare PID-Object for current update
      (setf fault      (- set-point current-value) ;; set error in PID-Object
	    integrator (+ integrator fault))       ;; set integrator-value in PID-Object
       
      ;; Calculate the PID Terms
      (setf p-value (* kp fault)
	    i-value (* integrator ki )
	    d-value (* (- current-value derivator) kd))


      ;; Prepare PID-Object for next update
      (setf derivator current-value)

      ;;(format t "p: ~A~%i: ~A~%d: ~A~%" p-value i-value d-value)
      ;;(print pid)

      (+ p-value i-value d-value))))

;;;
;;; Type C PID-Controller (reimplementation of the code on http://www.vandelogt.nl/htm/regelen_pid_uk.htm)
;;;
(defmethod update ((pid pid-c) current-value)
  "Current Version ignores integrator-max and integrator-min"
  (let ((p-value nil)
	(d-value nil)
	(i-value nil))
    (with-slots (kp ki kd derivator integrator integrator-max integrator-min set-point fault) pid
      ;; Prepare PID-Object for current update
      (setf fault      (- set-point current-value) ;; set error in PID-Object
	    integrator (+ integrator fault))       ;; set integrator-value in PID-Object
       
      ;; Calculate the PID Terms
      (setf p-value (* kp current-value)
	    i-value (* integrator ki )
	    d-value (* (- current-value derivator) kd))


      ;; Prepare PID-Object for next update
      (setf derivator current-value)

      ;;(format t "p: ~A~%i: ~A~%d: ~A~%" p-value i-value d-value)
      ;;(print pid)

      (+ p-value i-value d-value))))



;;;
;;; Here is some code to play with a PID-Controller
;;;    

;; Code for Testpurposes:

(defvar *pid-controller* nil)
(defvar *current-temperature* 0)

(defun water-simulation (energy-supplied)
  "Easy simulation of a bowl of water."
  (let ((ct *current-temperature*)
	(efficiency-factor 0.05)
	(energy-loss 0.01))
    (when (<= energy-supplied 0)
      (setf energy-supplied 0))
    (setf *current-temperature* (+ ct 
				   (* energy-supplied efficiency-factor)
				   (* (- ct) energy-loss)))))

(defun run-simulation (&optional (simulation-loops 10))
  "Easy simulation of heating a bowl of water."
  (setf *current-temperature* 20)
  (pid-init)
  (let ((pid-output nil)
	(results nil))
    (dotimes (n simulation-loops)
      (setf pid-output (pid-update *current-temperature*))
      (dotimes (n 10)
	(water-simulation pid-output))
      ;(print *current-temperature*)
      (push (list pid-output *current-temperature*) results))
    (format t "~%~%~{~{~20A~}~%~}~%" (reverse results))))
      
(defun pid-init ()
  (setf *pid-controller* (make-instance 'pid :set-point 60 :kd 0.1 :kp 1 :ki 0.02)))
;;              proportional    integral        derivative
;; Temperature 	2 to 100 	0.02 to 5 	0.1 to 20


(defun pid-update (current-value)
  (update *pid-controller* current-value))

  
