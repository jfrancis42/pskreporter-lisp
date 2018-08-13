;;;; pskreporter.lisp

(in-package #:pskreporter)

(defparameter *verbose* nil)

(defmacro smart-parse-integer (int-thing)
  "Given an expression, int-thing, returns either the parsed integer,
or 0 (if null)."
  `(if ,int-thing (parse-integer ,int-thing) 0))

;; Defines an Active Receiver.
(defclass active-receiver ()
  ((mode :accessor mode :initarg :mode :initform nil)
   (antenna-information :accessor antenna-information :initarg :antenna-information :initform nil)
   (decoder-software :accessor decoder-software :initarg :decoder-software :initform nil)
   (dxcc :accessor dxcc :initarg :dxcc :initform nil)
   (region :accessor region :initarg :region :initform nil)
   (frequency :accessor frequency :initarg :frequency :initform nil)
   (locator :accessor locator :initarg :locator :initform nil)
   (callsign :accessor callsign :initarg :callsign :initform nil)))

(defun make-active-receiver (thing)
  "Make an active-receiver object from the data returned from
pskreporter.info."
  (make-instance 'active-receiver
		 :mode (first (jeff:cdr-assoc "mode" thing))
		 :antenna-information (first (jeff:cdr-assoc "antennaInformation" thing))
		 :decoder-software (first (jeff:cdr-assoc "decoderSoftware" thing))
		 :dxcc (first (jeff:cdr-assoc "DXCC" thing))
		 :region (first (jeff:cdr-assoc "region" thing))
		 :frequency (smart-parse-integer (first (jeff:cdr-assoc "frequency" thing)))
		 :locator (first (jeff:cdr-assoc "locator" thing))
		 :callsign (first (jeff:cdr-assoc "callsign" thing))))

;; Defines an Active Callsign.
(defclass active-callsign ()
  ((frequency :accessor frequency :initarg :frequency :initform nil)
   (dxcc-code :accessor dxcc-code :initarg :dxcc-code :initform nil)
   (dxcc :accessor dxcc :initarg :dxcc :initform nil)
   (reports :accessor reports :initarg :reports :initform nil)
   (callsign :accessor callsign :initarg :callsign :initform nil)))

(defun make-active-callsign (thing)
  "Make an active-callsign object from the data returned from
pskreporter.info."
  (make-instance 'active-callsign
		 :frequency (smart-parse-integer (first (jeff:cdr-assoc "frequency" thing)))
		 :dxcc-code (first (jeff:cdr-assoc "DXCCcode" thing))
		 :dxcc (first (jeff:cdr-assoc "DXCC" thing))
		 :reports (first (jeff:cdr-assoc "reports" thing))
		 :callsign (first (jeff:cdr-assoc "callsign" thing))))

;; Defines a Reception Report.
(defclass reception-report ()
  ((snr :accessor snr :initarg :snr :initform nil)
   (sender0dxcc-locator :accessor sender-dxcc-locator :initarg :sender-dxcc-locator :initform nil)
   (sender-dxcc-code :accessor sender-dxcc-code :initarg :sender-dxcc-code :initform nil)
   (sender-dxcc :accessor sender-dxcc :initarg :sender-dxcc :initform nil)
   (is-sender :accessor is-sender :initarg :is-sender :initform nil)
   (mode :accessor mode :initarg :mode :initform nil)
   (flow-start-seconds :accessor flow-start-seconds :initarg :flow-start-seconds :initform nil)
   (timestamp :accessor timestamp :initarg :timestamp :initform nil)
   (sender-locator :accessor sender-locator :initarg :sender-locator :initform nil)
   (sender-callsign :accessor sender-callsign :initarg :sender-callsign :initform nil)
   (receiver-locator :accessor receiver-locator :initarg :receiver-locator :initform nil)
   (receiver-callsign :accessor receiver-callsign :initarg :receiver-callsign :initform nil)))

(defun make-reception-report (thing)
  "Make an active-callsign object from the data returned from
pskreporter.info."
  (make-instance 'reception-report
		 :snr (smart-parse-integer (first (jeff:cdr-assoc "sNR" thing)))
		 :sender-dxcc-locator (first (jeff:cdr-assoc "senderDXCCLocator" thing))
		 :sender-dxcc-code (first (jeff:cdr-assoc "senderDXCCcode" thing))
		 :sender-dxcc (first (jeff:cdr-assoc "senderDXCC" thing))
		 :is-sender (smart-parse-integer (first (jeff:cdr-assoc "isSender" thing)))
		 :mode (first (jeff:cdr-assoc "mode" thing))
		 :flow-start-seconds (smart-parse-integer (first (jeff:cdr-assoc "flowStartSeconds" thing)))
		 :timestamp (local-time:unix-to-timestamp
			     (smart-parse-integer (first (jeff:cdr-assoc "flowStartSeconds" thing))))
		 :sender-locator (first (jeff:cdr-assoc "senderLocator" thing))
		 :sender-callsign (first (jeff:cdr-assoc "senderCallsign" thing))
		 :receiver-locator (first (jeff:cdr-assoc "receiverLocator" thing))
		 :receiver-callsign (first (jeff:cdr-assoc "receiverCallsign" thing))))

(defmethod age ((rr reception-report))
  "Returns the age in seconds for a Reception Report."
  (- (local-time:timestamp-to-unix (local-time:now)) (flow-start-seconds rr)))

(defun psk-fetch (cmd method &optional content)
  "Return the XML data as specified by cmd and method from
pskreporter.info."
  (when *verbose* (format t "~A~%" cmd))
  (xmls:parse-to-list 
   (nth-value 0 
	      (drakma:http-request
	       (concatenate
		'string
		"http://retrieve.pskreporter.info/query?" cmd)
	       :method method
	       :content content))
   :compress-whitespace t))

(defun output-fixer (returned)
  "Clean up the output (parse it into objects, etc) from psk-fetch."
  (let ((tmp (if (equal "receptionReports" (first returned))
		 (rest (rest returned))
		 returned)))
    (list
     (cons :active-receiver
	   (mapcar
	    (lambda (b)
	      (make-active-receiver (first (rest b))))
	    (remove-if-not
	     (lambda (a)
	       (equal "activeReceiver" (first a)))
	     tmp)))
     (cons :active-callsign
	   (mapcar
	    (lambda (b)
	      (make-active-callsign (first (rest b))))
	    (remove-if-not
	     (lambda (a)
	       (equal "activeCallsign" (first a)))
	     tmp)))
     (cons :reception-report
	   (mapcar
	    (lambda (b)
	      (make-reception-report (first (rest b))))
	    (remove-if-not
	     (lambda (a)
	       (equal "receptionReport" (first a)))
	     tmp)))
     (cons :last-sequence-number
	   (parse-integer
	    (first
	     (jeff:cdr-assoc "value"
			     (first (jeff:cdr-assoc "lastSequenceNumber" tmp))))))
     (cons :max-flow-start-seconds
	   (parse-integer
	    (first
	     (jeff:cdr-assoc "value"
			     (first (jeff:cdr-assoc "maxFlowStartSeconds" tmp)))))))))

(defun fetch (&key tx-call rx-call call fs-time mode rpt-limit rr-only no-active
		freq-low freq-high no-locator stats modify last-seq-no)
  "User-callable function to fetch data from pskreporter.info. See
pskreporter.info docs for an explanation of various options. The data returned from this function will likely need to be processed by output-fixer for sane use."
  (if
   (or
    (and tx-call rx-call)
    (and tx-call call)
    (and rx-call call)
    (not (or tx-call rx-call call))
    (and freq-low (not freq-high))
    (and freq-high (not freq-low)))
   nil
   (let ((cmd
	  (jeff:join
	   (remove nil (list
			(when tx-call (concatenate 'string "senderCallsign=" tx-call))
			(when rx-call (concatenate 'string "receiverCallsign=" rx-call))
			(when call (concatenate 'string "callsign=" call))
			(when fs-time (concatenate 'string "flowStartSeconds=" (format nil "~A" fs-time)))
			(when mode (concatenate 'string "mode=" mode))
			(when rpt-limit (concatenate 'string "rptlimit=" (format nil "~A" rpt-limit)))
			(when rr-only "rronly")
			(when no-active "noactive")
			(when (or freq-low freq-high)
			  (concatenate 'string "frange=" (format nil "~A" freq-low) "-" (format nil "~A" freq-high)))
			(when no-locator "nolocator")
			(when stats "statistics")
			(when modify "modify=grid")
			(when last-seq-no (concatenate 'string "lastseqno=" (format nil "~A" last-seq-no)))))
	   #\&)))
     (psk-fetch cmd :get))))

(defun best-freq (grid)
  "Return a list of the 'best' frequencies for a given grid
square (four or six digits)."
  (let ((result nil)
	(tmp nil)
	(cmd (concatenate 'string "https://pskreporter.info/cgi-bin/psk-freq.pl?grid=" grid)))
    (when *verbose* (format t "~A~%" cmd))
    (with-input-from-string (s (nth-value 0 (drakma:http-request cmd :method :get)))
      (do ((line (read-line s nil)
		 (read-line s nil)))
	  ((null line) nil)
	(setf tmp (split-sequence:split-sequence #\Space line))
	(unless (equal "#" (first tmp))
	  (setf result (cons (list (cons :freq (parse-integer (first tmp)))
				   (cons :score (parse-integer (second tmp)))
				   (cons :spots (parse-integer (third tmp)))
				   (cons :tx (parse-integer (fourth tmp)))
				   (cons :rx (parse-integer (fifth tmp))))
			     result)))))
    (sort result (lambda (a b) (> (jeff:cdr-assoc :score a) (jeff:cdr-assoc :score b))))))
