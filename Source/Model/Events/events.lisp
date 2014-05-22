;;
;; #WORKING: References:
;;
;;   - http://uglylispcode.wordpress.com/2008/12/04/event-driven-architecture-programming-in-lisp-part-2/
;;   - Dolphin Smalltalk
;;   - http://www.object-arts.com/downloads/papers/ExtendingSUnitToTestSASEEvents.pdf
;;
;; Although not included in the ANSI Smalltalk standard, SASE event frameworks have become a defacto
;; standard feature of modern Smalltalk systems. SASE is a form of the Observer pattern [Gamma95]
;; in which a subscriber requests of a specific publisher that it should notify that subscriber by sending it
;; a specific message when an individual event occurs.
;;
;; #when:send:to:
;;   self model when: #itemUpdated: send: #onItemUpdated: to: self
;;
;; #trigger:with:
;;   self trigger: #itemUpdated: with: anObject
;;
;;   If there are any subscribers to the event, then they will each be notified of the event, in no particular
;;   order, by receiving the callback message they previously registered with #when:send:to:.
;;
;; #when:perform:
;;   This method effectively adds the message send event object to the object register associated with
;;   the receiver.
;; 
;; #get-events:
;;   Answer events table for the receiver.
;;

;; Default event handler
(defvar *events-registry* nil)


(defun initialize-event-handlers ()
  (setf *events-registry* (make-instance 'events-handler)))


;; Event class (similar to EventMessageSend in Dolphin Smalltalk, within Message hierarchy).
(defclass event ()
  ((id :initarg :id :accessor id)
   (receiver :initarg :receiver :accessor receiver)
   (selector :initarg :selector :accessor selector)
   (arguments :initarg :arguments :initform nil :accessor arguments)))


(defmethod event-value ((e event) &optional args)
  (apply (selector e) (append (list (receiver e)) args)))


;; Event handler
(defclass events-handler ()
  ((events :initform (make-hash-table :test 'equals) :accessor events)))


;; Internal methods
(defmethod events-count ((handler events-handler))
  (length (events handler)))

(defmethod suscribe ((handler events-handler) object id event)
  (appendf (gethash (list object (id event)) (events handler))
           (list event)))

(defmethod unsuscribe ((handler events-handler) object event)
  (removef (gethash (list object (id event)) (events handler))
           (list event)))

;; Development methods
(defmethod when-send-to (object event-id selector subject &rest args)
  (let ((event (make-instance 'event :id event-id :receiver subject :selector selector :arguments args)))
    (suscribe *events-registry* object event-id event)))

(defmethod trigger (object event-id &rest args)
  "Trigger <object> events associated with <event-id> with <args>."
  (let ((events (gethash (list object event-id) (events *events-registry*))))
    (dolist (event events)
      (event-value event (append (arguments event) args)))
    (length events)))

(defmethod clear-event-for (object event-id)
  "Clear <event-id> event from <object>."
  (setf (gethash (list object event-id) (events *events-registry*)) nil))

(defmethod clear-events-for (object)
  "Clear <object> events."
  (let ((list))
    ;; Collect candidate events
    (maphash 
     (lambda (key value) 
       (declare (ignore value))
       (when (equals (first key) object)
         (appendf list (list (second key)))))
     (events *events-registry*))
    ;; Unregister events
    (dolist (id list)
      (clear-event-for object id))))

;; #TODO: Add and unify with #'events-for
(defmethod listeners-for (object &optional event-id)
  nil)

(defmethod events-for (object event-id)
  "Answer events for <event-id> and <object>."
  (gethash (list object event-id) (events *events-registry*)))
