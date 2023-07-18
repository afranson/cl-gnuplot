;;;; cl-gnuplot.lisp

(in-package #:cl-gnuplot)

;; For debugging, use (setf plt::*debug-print-all-commands* nil) to print all strings sent to gnuplot
;; Go to (defun plot ...) and (defun send-strings ...) for the main entry points of this code base

;;; utilities
(defun linspace (start end &key (len 50) (step nil) (type 'float))
  "Provides a list from 'start' to 'end' (inclusive) that is 'len' in length. Or provides a list of number from 'start' until 'end' by 'step'. 'end' may not be reached if 'step' kwarg is provided.
Supported types for 'type' arg are 'float, 'double-float, 'rational, 'integer, 'bignum."
  (let ((step (if step step (/ (rational (- end start)) (1- len))))
	(len (if step (+ 1 (floor (- end start) step)) len))
	(return-list nil))
    (let ((rational-numbers
	    (do ((n 0 (1+ n))
		 (curr (rational start) (progn (push curr return-list) (+ curr step))))
		((>= n len) (values (reverse return-list) step)))))
      (case type
	((or float double-float rational) (mapcar (lambda (x) (coerce x type)) rational-numbers))
	(t (mapcar #'round rational-numbers))))))

(defun range (start-or-end &optional end)
  "Provides a list of numbers from 'start' to 'end' incrementing by 1. Always integers. Rounds if necessary.
(range 5.2) => '(0 1 2 3 4)
(range 2 5) => '(2 3 4)
(range 5 2) => '(5 4 3)"
  (let ((start (round (if end start-or-end 0)))
	(end (round (if end end start-or-end))))
    (let ((return-list nil) (sign (signum (- end start))))
      (do ((curr start (progn (push curr return-list) (+ sign curr))))
	  ((= curr end) (reverse return-list))))))

(defun partition (n sequence)
  "Groups 'sequence' into chuncks of size 'n'.
I.e. partition by 2 transforms (1 2 3 4 5 6 ...) -> ((1 2) (3 4) (5 6) ...)"
  (do ((return-sequence nil))
      ((null sequence) (reverse return-sequence))
    (if (> (length sequence) n)
	(progn (setf return-sequence (append (list (subseq sequence 0 n)) return-sequence))
	       (setf sequence (subseq sequence n)))
	(progn (setf return-sequence (append (list (subseq sequence 0 (length sequence))) return-sequence))
	       (setf sequence nil)))))

(defun transpose (xy-list &optional y-list)
  "Takes 'xy-list' of form ((x1 y1 z1 ...) (x2 y2 z2 ...) ...) and turns it into ((x1 x2 ...) (y1 y2 ...) (z1 z2 ...) ...). Also known as a transpose. It is its own inverse. Works for combinations of lists and vectors."
  (when xy-list
    (when y-list (setf xy-list (list xy-list y-list)))
    (apply #'map 'list #'list xy-list)))

(defun 3d-data-to-x-list-y-list-z-list (xyz-list)
  "Transform data in the form (((x y z) (x y z) ... ) ((x y z) (x y z) ... ) ... ) into ((x x x ...) (y y y ...) (z z z ...))."
  (transpose (apply #'append xyz-list)))

(defun x-list-y-list-z-list-to-3d-data (x-list-y-list-z-list page-length)
  "Transform data in the form ((x x x ... ) (y y y ... ) (z z z ...)) into (((x y z) (x y z) ... ) ((x y z) (x y z) ... ) ... )."
  (partition page-length (transpose x-list-y-list-z-list)))

(defun map-nest-2 (function list-inner list-outer &optional (order (or :12 :21)))
  "mapcars over 2 lists in an outer product fashion. 'function' must accept 2 arguments. The length of the return list matches the length of 'list-outer', the length of the first element in the return list is of length 'list-innner'. List inner is applied as the left argument to 'function' for 'order' :12 or as the right argument for 'order' :21.
>>> (map-nest-2 (lambda (x y) (list x y (+ x y))) '(1 2) '(10 20 30) :12)
(((1 10 11) (2 10 12)) ((1 20 21) (2 20 22)) ((1 30 31) (2 30 32)))"
  (labels ((symbol-order (number-symbol numbers)
	     (let ((symbol-name (symbol-name number-symbol)))
	       (map 'list (lambda (x) (elt numbers (1- (digit-char-p x)))) symbol-name))))
    (mapcar (lambda (y) (mapcar (lambda (x) (apply function (symbol-order order (list x y)))) list-inner)) list-outer)))

(defun x-list-y-list-z-matrix-to-3d-data (x-list y-list z-matrix)
  "Turns a 1D x-list, a 1D y-list, and a 2D z-matrix (list of lists) and returns a 3D matrix (list of lists of lists) with every xyz pairing."
  (labels ((x-list-y-list-permute (x-list y-list)
	     "Gives all permutations of the elements from 'x-list' and 'y-list'.
I.e. (x-list-y-list-permute '(1 2) '(4 5)) => '((1 4) (1 5) (2 4) (2 5))"
	     (let ((return-list nil))
	       (dolist (x-element x-list)
		 (dolist (y-element y-list)
		   (push (list x-element y-element) return-list)))
	       (reverse return-list))))
    (when y-list
      (mapcar (lambda (xys zs) (mapcar (lambda (xy z) (append xy (list z))) xys zs)) (partition (length y-list) (x-list-y-list-permute x-list y-list)) z-matrix))))

(defun split-string (splitter string)
  "Split string by splitter, which is a char."
  (declare (optimize speed (safety 1))
	   (character splitter))
  (labels ((inner-split (string &optional acc)
	     (declare (optimize speed safety)
		      (simple-string string)
		      (type (or null cons) acc))
	     (if (string/= string "")
		 (let ((pos (position splitter string :test #'char=)))
		   (if pos
		       (inner-split (subseq string (+ pos 1)) (cons (subseq string 0 pos) acc))
		       (inner-split "" (cons string acc))))
		 (reverse acc))))
    (inner-split string)))

(defun basic-read-file (file &key (comma-is-delim? t) manual-delim)
  "Ignore lines that don't lead with a number (0-9 or .+-). When a lead number is found, scans the line until a non-number char is found (not 0-9 or .+-). Takes that character to be a delimiter (usually space, tab, or comma). Set 'comma-is-delim?' to nil to allow for commas in numbers and not as the delimiter. Set 'manual-delim' to allow for first char to be that delim and skip delim checking."
  (with-open-file (in file :direction :input)
    (let ((return-lines nil))
      (do ((i 0 (1+ i))
	   (line (read-line in nil nil) (read-line in nil nil)))
	  ((not line) (reverse return-lines))
	(if (or (digit-char-p (char line 0))
		(member (char line 0) (append '(#\- #\+ #\.) (if manual-delim (list manual-delim)))))
	    (let ((delim (if manual-delim manual-delim (find-if (lambda (x) (not (member x (append (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\- #\+) (if comma-is-delim? nil '(#\,)))))) line))))
	      (push (mapcar (lambda (x) (read-from-string x nil nil)) (split-string delim line)) return-lines)))))))

;;; Data structure
;; plot-spec = a plist ... (list :key value :key value)
;; keys are the command name, i.e. terminal, size, xrange, ylabel, cbrange, palette, view, pm3d, isosamples, key (as in legend), border, grid, xtics, polar, log, etc.
;; values are the string that would follow the command, i.e. for "set terminal qt size 1080,1080 linewidth 2 font \"Arial,25\"", the key would be :terminal and the value would be "qt size 1080,1080 linewidth 2 font \"Arial,25\"". Default is for set to go before the command
;; value should be "unset" to unset a command
;; plot, splot, data, 3d-data are special commands in the plot-spec

;;; Bedrock tests
;; (format *gnuplot-stream* "plot sin(x)~&")
;; (format *gnuplot-stream* "plot '-' w lp title \"plot 1\",'-' w lp title \"plot 2\"~&1 2~&4 20~&9 10~&e~&1 10~&4 0~&9 1~&e~&")
;; (format *gnuplot-stream* "set xrange [0:6]~&replot~&")
;; (finish-output *gnuplot-stream*)


;;; Fundamental commands
;; (plot ..stuff.. and ..options..)
;; (plot-add ..stuff.. and ..options..)
;; (send-plot-options ..options..)
;; (reset)
;; (replot)
;; (save-last-plot)
;; (save-plot ..plot..) *macro*
;; (switch-save-all)
;; (restart-gnuplot)
;; (send-command ..commands..)
;; (send-string ..any valid gnuplot string command..)

;; TODO allow for generic binary location and inform if not found with what to do
(defstruct gnuplot
  "Structure for holding all the gnuplot process streams."
  (two-way-stream nil)
  (error-stream nil)
  (process nil))

(defstruct gnuplot-plot-command
  "Structure for holding the type (:plot or :plot-3d), format (all the \"'-' w lp\" stuff concatenated together), and data (a list of each dataset) for a full (multi-)plot command."
  (type nil :type symbol)
  (format nil :type list)
  (data nil :type list))

(export '(make-gnuplot gnuplot-two-way-stream gnuplot-error-stream gnuplot-process gnuplot-p))

(defvar *gnuplot* (make-gnuplot) "global gnuplot-process")
(defvar *current-plots* (make-gnuplot-plot-command) "currently plotted objects")
(defvar *save-all* nil "save all plots to images or not")
(defvar *auto-label* 0 "Iterates to give some basic identity to anonymous plots")
(defvar *debug-print-all-commands* nil)

(defvar example-2d-data '((1 2) (3 5) (5 7) (6 2) (7 5) (10 7) (14 8)))
(defvar example-3d-data '(((0 0 5) (1 0 10) (2 0 0))
			  ((0 1 0) (1 1 5) (2 1 0))
			  ((0 2 5) (1 2 0) (2 2 0))))

(defun init-gnuplot (&optional (init-global-stream t))
  "Initialize the gnuplot process and return the process streams, process class object from uiop, and the error stream. Default is to use gnuplot-qt. Can use 2>&1 to merge error and stdout streams." 
  (let* ((gnuplot-process (uiop:launch-program "/bin/gnuplot-qt"
					       :output :stream
					       :input :stream
					       :error-output :stream))
	 (gnuplot-stream (make-two-way-stream (uiop:process-info-output gnuplot-process)
					      (uiop:process-info-input gnuplot-process)))
	 (gnuplot-error-stream (uiop:process-info-error-output gnuplot-process))
	 (gnuplot-object (make-gnuplot :two-way-stream gnuplot-stream
				      :error-stream gnuplot-error-stream
				      :process gnuplot-process)))
    (if init-global-stream
	(setf *gnuplot* gnuplot-object)
	gnuplot-object)))

(defun quit-gnuplot (&optional (gnuplot-instance *gnuplot*))
  "Close the gnuplot streams and indirectly close the process."
  (uiop:close-streams (gnuplot-process gnuplot-instance))
  (uiop/launch-program:terminate-process (gnuplot-process gnuplot-instance)))

(defun get-gnuplot-error-output (&optional (gnuplot-instance *gnuplot*))
  "Read error output from 'gnuplot-instance' until it receives a nil from read-char-no-hang function."
  (let ((error-output nil))
    (do ()
	(nil)
      (push (read-char-no-hang (gnuplot-error-stream gnuplot-instance)) error-output)
      (unless (car error-output)
	(return (coerce (reverse (cdr error-output)) 'string))))))

(defun send-strings (strings &optional (err-info (or :show :get :no)) (gnuplot-instance *gnuplot*))
  "Sends the provided 'strings' (single or list of) directly to 'gnuplot-instance'. 'err-info' determines if error information is retrieved after the strings are sent. :no doesn't get error info, :get returns the error info, :show returns and prints the error info. 'strings' is a single string or list of strings"
  (let ((gnuplot-stream (gnuplot-two-way-stream gnuplot-instance)))
    (dolist (string (if (consp strings) strings (list strings)))
      (let ((sent-string (if (string= "" string) " " string)))
	(when *debug-print-all-commands* (print sent-string))
	(princ sent-string gnuplot-stream)
	(fresh-line gnuplot-stream)))
    (finish-output gnuplot-stream)
    (case err-info
      (:no nil)
      (:get (get-all-gnuplot-error-output nil gnuplot-instance))
      (:show (get-all-gnuplot-error-output t gnuplot-instance) nil))))

(defun send-strings-and-replot (strings &optional (err-info (or :no :get :show)) (gnuplot-instance *gnuplot*))
  "Sends the provided 'string' directly to 'gnuplot-instance' followed by a replot command."
  (send-strings (append strings (list "replot")) err-info gnuplot-instance))

(defun get-all-gnuplot-error-output (&optional (show nil) (gnuplot-instance *gnuplot*))
  "Retrieves all gnuplot error output from 'gnuplot-instance' (default *gnuplot*) after a command and gives it to the user in a list of strings. Also prints the error information is 'show' is t. Gnuplot output waits for return before giving another screen of output. This simply skips the returns and gives the user all the output at once."
  (sleep 0.025)
  (do ((c (get-gnuplot-error-output gnuplot-instance) (progn (send-strings "" :no gnuplot-instance)
							     (finish-output (gnuplot-two-way-stream gnuplot-instance))
							     (sleep 0.025)
							     (get-gnuplot-error-output gnuplot-instance)))
       (output nil (append (list c) output)))
      ((string= c "") (apply #'concatenate 'string output))
    (when show (print c))
    (finish-output)))

(defun replot (&optional (gnuplot-instance *gnuplot*))
  "Send the replot command to 'gnuplot-instance'"
  (send-strings "replot" :show gnuplot-instance))

(defun reset (&optional (gnuplot-instance *gnuplot*))
  "Send the reset command to 'gnuplot-instance'"
  (send-strings "reset session" :show gnuplot-instance)
  (setf *current-plots* (make-gnuplot-plot-command))
  (setf *auto-label* 0))

(defun send-command (&rest keys &key &allow-other-keys)
  "Sends a command to gnuplot instance, 'gp'. Arguments should follow the format:
(send-command :xrange \"[0:10] noreverse\" :title \"'My Title'\" :yrange \"unset\")
Use keyword ':gp' to specify a gnuplot instance to send the command to. If not, send to global, *gnuplot*, instance. Does not replot, only send to gnuplot. Useful for multiplots or other complex multi-set scripts."
  (send-strings (format-commands keys) :show))

(defun send-command-and-replot (&rest keys &key (gp *gnuplot*) &allow-other-keys)
  "Sends a command to gnuplot instance, 'gp'. Arguments should follow the format:
(send-command :xrange \"[0:10] noreverse\" :title \"'My Title'\" :yrange \"unset\")
Use keyword ':gp' to specify a gnuplot instance to send the command to. If not, send to global, *gnuplot*, instance."
  (send-strings (concatenate 'string (format-commands keys) "replot") :show gp))

(defmacro defun-create-plot-options-function (&rest keys-and-defaults)
  "Creates functions that modify plot options. Used so that IDE can provide helpful parameters and defaults WITHOUT actually setting the defaults upon execution. Only user supplied arguments will be send to the gnuplot process."
  (labels ((make-arg-list (args)
	     (mapcar
	      (lambda (x)
		(destructuring-bind (key default) x
		  `(,key ,default ,(intern (concatenate 'string (symbol-name key) "-P")))))
	      args))
	   (make-command-list (args)
	     (mapcar
	      (lambda (x)
		(destructuring-bind (key default) x
		  (declare (ignore default))
		  `(when ,(intern (concatenate 'string (symbol-name key) "-P"))
		     (list ,(intern (symbol-name key) :keyword)
			   ,key))))
	      args)))
    `(progn
       (defun send-plot-options (&key ,@(make-arg-list keys-and-defaults) &allow-other-keys)
	 "Send plot options to gnuplot process WITHOUT replotting. See 'send-plot-options-and-replot' for a version that does replot with every execution."
	 (apply #'send-command (nconc ,@(make-command-list keys-and-defaults))))
       (defun send-plot-options-and-replot (&key ,@(make-arg-list keys-and-defaults) &allow-other-keys)
	 "Send plot options to gnuplot process and replot. See 'send-plot-options' for a version that does not replot with every execution."
	 (apply #'send-command-and-replot (nconc ,@(make-command-list keys-and-defaults)))))))

(defun-create-plot-options-function
    (terminal "qt size 1080,1080 linewidth 3 font 'Arial,30'")
    (output "'myfile.png'")
  (title "'My Title' font ',40' tc rgb 'red'")
  (size "ratio 1 1.0,1.0")
  (origin "0.0, 0.0")
  (lmargin "4")
  (bmargin "-1")
  (rmargin "at screen 0.95")
  (tmargin "-1")
  (margin "0.05, 0.05, 0, 0")
  (xrange "[*:*] noreverse nowriteback noextend")
  (yrange "restore")
  (zrange "[*:*] noreverse nowriteback")
  (cbrange "[*:*]")
  (xlabel "'xlabel' offset 0,0,0 font ',12' enhanced")
  (ylabel "'ylabel' offset 0,0,0 rotate parallel")
  (zlabel "'zlabel' rotate by 13")
  (xtics "auto add ('custom tic' 1.5)")
  (ytics "14")
  (ztics "0,10,40 rotate 90 offset 0,graph 0.05")
  (mxtics "5")
  (mytics "defualt")
  (mztics "10")
  (cblabel "'cblabel' rotate 180")
  (label "'example' at graph 26, 0.23 left rotate by 34 font ',10' boxed")
  (palette "-- Just look it up --")
  (view "map 56,103")
  (pm3d "at sb interpolate 0,0 depthorder lighting map")
  (samples "100")
  (isosamples "50")
  (key "top left title 'keys' font ',16' font ',10' box maxcols 3 maxrows 2 offset screen 0.1,0")
  (border "31 ls 3 lc rgb 'purple'")
  (grid "noxtics nomztics polar novertical")
  (polar "")
  (log "xy")
  (parametric "")
  (timefmt "")
  (multiplot ""))

(defun save-last-plot (&optional (terminal (or "pngcairo" "pdfcairo" "epscairo" "wxt" "epslatex" "pslatex" "cairolatex" "jpg" "png" "gif")) (filename "mytest.png"))
  "Saves what can be recalled with 'replot' function (i.e. multiplot figures will not work - see 'save-plot' macro to save multiplots).
For various reasons, terminals have slight differences. Always find tune the plot and terminal options to suite your needs.
Some common issues:
Default terminal assume relative small image sizes and various terminals have different defualt size units. If it looks like your fonts or points are now too large or small, it's likly just the size of your output image is different.
E.g.
pngcairo size works in pixels: default is 640x480 pixels @ 72 dpi.
pdfcairo size works in inches: default is 5x3 inches"
  (send-plot-options :terminal "push")
  (send-plot-options :terminal terminal)
  (send-plot-options :output (format nil "~s" filename))
  (replot)
  (sleep .5) ;; has to wait for the plot to finish first
  (send-plot-options :output "")
  (send-plot-options :terminal "pop"))

(defmacro save-plot ((&optional (terminal (or "pngcairo" "pdfcairo" "epscairo" "wxt" "epslatex" "pslatex" "pstricks" "cairolatex" "jpg" "png" "gif")) (filename "mytest.png")) &body body)
  "Saves whatever plot is created in 'body' via the 'terminal' into 'filename'. Useful for multiplots. See 'save-last-plot' function for more details on saving."
  `(progn
     (send-plot-options :terminal "push")
     (send-plot-options :terminal ,terminal)
     (send-plot-options :output (format nil "~s" ,filename))
     ,@body
     (sleep .5) ;; has to wait for the plot to finish first
     (send-plot-options :output "")
     (send-plot-options :terminal "pop")))

(defun save-gnuplot-script (&optional (filename "tmp.gnu"))
  "Save the gnuplot session."
  (send-strings (format nil "save ~a" filename)))

(defun load-gnuplot-script (filename)
  "Load a gnuplot session from a .gnu file."
  (send-strings (format nil "load ~a" filename)))

(defun add-dash-to-string (string)
  (concatenate 'string "'-' " string))

(defun auto-label (&key (dim (or :2d :3d)))
  "Generates a default label for 2d and 3d plots with basic numbering."
  (case dim
    (:2d (format nil "'-' w lp title \"line ~d\"" (incf *auto-label*)))
    (:3d (format nil "'-' w pm3d title \"surface ~d\"" (incf *auto-label*)))))

(defun format-commands (commands)
  "Expects a command in the form (list :command options :command2 options2 ...) or (list (list :command options) (list :command2 options2) ...) and returns strings formatted it into proper gnuplot input. :plot, :slot, :data and :3d-data are treated special. If the options field is \"unset\", then the format is \"unset command\"."
  (let* ((commands (if (and (consp commands) (consp (first commands))) commands (partition 2 commands)))
	 (format-strings
	   (mapcar
	    (lambda (command)
	      (cond ((null (cdr command))
		     "")
		    ((member (car command) (list :plot :splot))
		     "~a ~{~a~^, ~}~&")
		    ((eq (car command) :data) ;; expects (((x y z ...) (x y z ...) ...)) format
		     "~*~{~{~{~,9,,,,,'eg~^ ~}~&~}e~&~}")
		    ((eq (car command) :3d-data) ;; expects ((((x y z ...) (x y z ...) ...) ((x y z ...) (x y z ...) ...))) format
		     "~*~{~{~{~{~,9,,,,,'eg~^ ~}~&~}~^~%~}e~&~}")
		    ((string= (cadr command) "unset") ;; unset commands
		     "unset ~a~&")
		    ((string= (cadr command) "") ;; empty commands
		     "set ~a ~a~&")
		    ((char= (char (cadr command) 0) #\#) ;; if first char is #, treat as comment
		     "#set ~a ~a~&")
		    ;; If output is not stdout, do not allow terminal operations
		    ;; Loops until a valid command is found
		    ((eq (char (symbol-name (car command)) 0) #\T)
		     (let ((output (retrieve :output)))
		       (do ((i 0 (1+ i)))
			   ((or (>= i 20) (string/= output "")) output)
			 (sleep 0.025)
			 (setf output (get-all-gnuplot-error-output nil)))
		       (if (string= (elt (split-string #\ (elt (split-string #\newline output) 1)) 4) "STDOUT")
			   "set ~a ~a~&"
			   "")))
		    (t
		     "set ~a ~a~&")))
	    commands))
	 (command-strings
	   (mapcar
	    (lambda (format-string command)
	      (format nil
		      format-string
		      (string-downcase (symbol-name (car command)))
		      (cadr command)))
	    format-strings commands)))
    (apply #'concatenate 'string command-strings)))

(defun sequence-depth (sequence)
  "Returns the depth of the first most number.
=> (sequence-depth '(((1 3 4))))
=> 3"
  (if (or (consp sequence) (vectorp sequence))
      (1+ (sequence-depth (elt sequence 0)))
      0))

(defun which-data-organization (x)
  "Check if 'x' is organized like
String: \"...\"
Symbol: :my-symbol
0D: x
1D: (x x x ...)
2D: ((x y ...) (x y ...) ...)
3D: (((x y z ...) (x y z ...) ...) ((x y z ...) (x y z ...) ...) ...)."
  (cond ((stringp x)
	 :string)
	((symbolp x)
	 :symbol)
	(t
	 (intern (format nil "~dD" (sequence-depth x)) 'keyword))))

(defun what-is-the-next-plot-object (command-list)
  "Takes in a plot command list, parses it into symbols, and returns possible matches for the next largest plottable set of data."
  (let ((command-list-pattern (mapcar #'which-data-organization command-list)))
    (labels ((commands-pattern-eq (match-pattern)
	       "Check if the command-pattern contains the match-pattern."
	       (if (> (length match-pattern) (length command-list-pattern))
		   nil
		   (every #'eq command-list-pattern match-pattern))))
      (cond ((null command-list)
	     nil)
	    ((commands-pattern-eq '(:symbol))
	     :symbol)
	    ((commands-pattern-eq '(:string))
	     :string)
	    ((commands-pattern-eq '(:1d :string))
	     :1d-string)
	    ((commands-pattern-eq '(:2d :string))
	     :2d-string)
	    ((commands-pattern-eq '(:3d :string))
	     :3d-string)
	    ((commands-pattern-eq '(:1d :1d :string))
	     :1d-1d-string)
	    ((commands-pattern-eq '(:1d :1d :2d :string))
	     :1d-1d-2d-string)
	    ((commands-pattern-eq '(:1d :1d :2d))
	     :1d-1d-2d)
	    ((commands-pattern-eq '(:1d :1d))
	     :1d-1d)
	    ((commands-pattern-eq '(:3d))
	     :3d)
	    ((commands-pattern-eq '(:2d))
	     :2d)
	    ((commands-pattern-eq '(:1d))
	     :1d)))))

(defstruct plot-object
  "Structure for holding classified plot args. Code is a symbol (a command, :plot, or :plot-3d). Data is the command data or the plot data. Format is a plot format string."
  (code nil :type symbol) ;; plot or plot-3d
  (data nil :type (or string list))
  (format "" :type string))

(defvar ignore-ys-in-text nil)
(defun parse-plot-args (args)
  "Parses arguments passed to one of the plot functions. Returns \"plot-object\"s to be turned into strings and then send to the gnuplot process. Accepts data and symbols to simultaneously to plotting of data and modification of plot settings."
  (labels ((maybe-add-dash (string)
	     (if (string= "" string) "'-'" (add-dash-to-string string))))
    (if (null args)
	nil
	(multiple-value-bind (object rest)
	    (ecase (what-is-the-next-plot-object args)
	      (:symbol (values (make-plot-object :code (first args) :data (second args)) (cddr args)))
	      (:string (values (make-plot-object :code (if ignore-ys-in-text :plot (if (position #\y (first args)) :plot-3d :plot)) :format (first args)) (cdr args)))
	      (:1d (values (make-plot-object :code :plot :data (transpose (list (first args))) :format (auto-label)) (cdr args)))
	      (:1d-1d (values (make-plot-object :code :plot :data (transpose (subseq args 0 2)) :format (auto-label)) (cddr args)))
	      (:1d-1d-2d (values (make-plot-object :code :plot-3d :data (x-list-y-list-z-matrix-to-3d-data (car args) (cadr args) (caddr args)) :format (auto-label :dim :3d)) (cdddr args)))
	      (:2d (values (make-plot-object :code :plot :data (car args) :format (auto-label)) (cdr args)))
	      (:3d (values (make-plot-object :code :plot-3d :data (car args) :format (auto-label :dim :3d)) (cdr args)))
	      (:1d-string (values (make-plot-object :code :plot :data (transpose (list (first args))) :format (maybe-add-dash (second args))) (cddr args)))
	      (:1d-1d-string (values (make-plot-object :code :plot :data (transpose (subseq args 0 2)) :format (maybe-add-dash (third args))) (cdddr args)))
	      (:1d-1d-2d-string (values (make-plot-object :code :plot-3d :data (x-list-y-list-z-matrix-to-3d-data (car args) (cadr args) (caddr args)) :format (maybe-add-dash (fourth args))) (cddddr args)))
	      (:2d-string (values (make-plot-object :code :plot :data (car args) :format (maybe-add-dash (second args))) (cddr args)))
	      (:3d-string (values (make-plot-object :code :plot-3d :data (car args) :format (maybe-add-dash (second args))) (cddr args))))
	  (append (parse-plot-args rest) (list object))))))

(defstruct segregated-plot-objects
  "Structure for holding plot commands. plot-type is :plot or :plot-3d. plottables is all the plot commands (data and format or just straight string commands). settings are all the plot settings (symbol and data)."
  (plot-type nil :type symbol) ;; 2d or 3d
  (plottables nil :type list) ;; data or strings
  (settings nil :type list)) ;; symbols and strings

(defun segregate-plots (args &optional (plots nil) (settings nil))
  "Returns a list of the plottable things (\"sin(x)\", '(1 2 3 4 5), etc.) and a list of settings (:xrange, :key, :ylabel, etc.)"
  (if (null args)
      (make-segregated-plot-objects :plottables plots :settings settings)
      (case (plot-object-code (car args))
	((:plot :plot-3d :string) (segregate-plots (rest args) (append (list (first args)) plots) settings))
	(otherwise (segregate-plots (rest args) plots (append (list (first args)) settings))))))

(defun make-sure-codes-match (seg-plot-objects)
  "Changes the codes of plottable objects. If a string is found with a 'y' present, assumes plot-3d unless 'ignore-ys-in-text' is true. e.g. 'cos(x)+sin(y)' vs 'airy(x)'."
  (if (segregated-plot-objects-plottables seg-plot-objects)
      (let ((preffered-code (plot-object-code (car (segregated-plot-objects-plottables seg-plot-objects)))))
	(setf (segregated-plot-objects-plot-type seg-plot-objects) preffered-code)
	(dolist (element (segregated-plot-objects-plottables seg-plot-objects) seg-plot-objects)
	  (if (not (eq preffered-code (plot-object-code element)))
	      (error "Attempting to mix 2d and 3d plots.~%If this is incorrect, try (setf plt:ignore-ys-in-text t)."))))
      seg-plot-objects))

(defun add-plots-to-current-plots (seg-plot-objects &optional (plots *current-plots*) overwrite)
  "Combines the 'plots' object (default is global *current-plots*) with the objects found in 'seg-plot-objects'."
  (let ((plots (let ((maybe-plots plots)) (if maybe-plots maybe-plots (make-gnuplot-plot-command))))
	(new-formats nil)
	(new-data nil))
    (when overwrite (setf plots (make-gnuplot-plot-command)))
    (setf (gnuplot-plot-command-type plots) (segregated-plot-objects-plot-type seg-plot-objects))
    (dolist (element (segregated-plot-objects-plottables seg-plot-objects))
      (push (plot-object-format element) new-formats)
      (when (plot-object-data element) (push (plot-object-data element) new-data)))
    (setf (gnuplot-plot-command-format plots) (append (gnuplot-plot-command-format plots) (reverse new-formats)))
    (setf (gnuplot-plot-command-data plots) (append (gnuplot-plot-command-data plots) (reverse new-data)))
    plots))

(defun rearrange-plots (new-order)
  "Reorders the format and plot data information found in *current-plots* to be compliant with 'new-order'.
E.g. if the current order is (0 1 2 3 4 5) and 'new-order' is '(0 3 2 5 4 1), the returned order is (0 3 2 5 4 1)."
  (setf (gnuplot-plot-command-format *current-plots*)
	(mapcar (lambda (old-index) (elt (gnuplot-plot-command-format *current-plots*) old-index)) new-order))
  (setf (gnuplot-plot-command-data *current-plots*)
	(mapcar (lambda (old-index) (elt (gnuplot-plot-command-data *current-plots*) old-index)) new-order))
  nil)

(defun send-current-plots-to-gnuplot (seg-plot-objects &optional (plots *current-plots*))
  "Checks type of plot-object, formats the commands appropriately and sends to gnuplot for plotting."
  (let ((plot-type (segregated-plot-objects-plot-type seg-plot-objects)))
    (send-strings
     (format-commands
      (apply #'append
	     (mapcar (lambda (x) (list (plot-object-code x) (plot-object-data x))) (segregated-plot-objects-settings seg-plot-objects))
	     (list (list (list (if (eq plot-type :plot-3d) :splot :plot) (gnuplot-plot-command-format plots))
			 (list (if (eq plot-type :plot-3d) :3d-data :data) (gnuplot-plot-command-data plots))))))
     :show)
    nil))

(defun resend-plots ()
  "Sends the *current-plots* object to be replotted. Useful when rearranging plot order with (rearrange-plots ...)"
  (send-current-plots-to-gnuplot (make-segregated-plot-objects)))

;; this is the intended entry point
(defun plot (&rest args)
  "Plots data formats and plot options passed to it. Accepts data as 1d, 2d, 3d formats as well as 1d-1d and 1d-1d-2d. Format strings can be passed after data to specify titles, linestyles, etc. Also accepts pure strings to plot with default gnuplot capabilities [files or common functions]. 2d data is expected in x-y pairs. 3d data is expected in x-y-z pairs. 1d-1d-2d data expects and x-list, y-list, then z-matrix assuming a grid of the supplied x and y values.
(plt:plot \"sin(x)\" '(1 2 3) '(6 5 4) \"w lp title 'foobar'\" '((0 7) (2 5) (4 2)) :xlabel \"'Time'\" :grid \"\")"
  (setf *auto-label* 0)
  (let* ((seg-plots (make-sure-codes-match (segregate-plots (parse-plot-args args)))))
    (send-current-plots-to-gnuplot seg-plots
                                   (setf *current-plots* (add-plots-to-current-plots seg-plots *current-plots* t)))
    (when *save-all* (save-last-plot))))

(defun plot-add (&rest args)
  "Add plot generated by 'args' to the most recent plot"
  (let* ((seg-plots (make-sure-codes-match (segregate-plots (parse-plot-args args)))))
    (send-current-plots-to-gnuplot seg-plots
                                   (setf *current-plots* (add-plots-to-current-plots seg-plots *current-plots*)))
    (when *save-all* (save-last-plot))))


;; TODO could clip the "Press return for more: " bits and merge it all into one string
;; TODO could clip the specific usage help and provide that special - Start at "Syntax:\n" and go until "\n\n"
(defun help (topic &optional return-string? (gnuplot-instance *gnuplot*))
  "Prints all the gnuplot help info concerning 'topic'. Set 'return-string?' to t to return the help information as a string."
  (let* ((query (if (symbolp topic) (string-downcase (symbol-name topic)) topic))
	 (return-string (send-strings (format nil "help ~a" query) :show gnuplot-instance)))
    (if return-string? return-string nil)))

(defun show (topic &optional return-string? (gnuplot-instance *gnuplot*))
  "Prints all the gnuplot show info concerning 'topic'. Set 'return-string?' to t to return the show information as a string."
  (let* ((query (if (symbolp topic) (string-downcase (symbol-name topic)) topic))
	 (return-string (send-strings (format nil "show ~a" query) :show gnuplot-instance)))
    (if return-string? return-string nil)))

(defun retrieve (topic)
  "Returns all the gnuplot show info concerning 'topic' as a list of strings. Does not print the 'show topic' information."
  (let ((query (if (symbolp topic) (string-downcase (symbol-name topic)) topic)))
    (send-strings (format nil "show ~a" query) :get)))

(defun help-cl-gnuplot ()
  "Prints useful entry points and help for the intended use of cl-gnuplots functionality."
  (princ "* 'plot' and 'plot-add' for core functionality
** 'plot' can recognize 1d, 2d, and 3d formats. Typical usage is (plt:plot 1-2-or-3d-data \"plot format string [line style, title, etc.]\" next-1-2-or-3d-data \"optional next format string\" ... )
** Linewidth and font size should increase with terminal size:
** - 480,270 use lw 1 font \",10\"
** - 960,540 use lw 2 font \",20\"
** - 1920,1080 use lw 4 font \",40\"
* 'send-plot-options' to see (with help of IDE) many useful plot options with informative defaults
* 'help' to get gnuplot's help info about a topic, i.e. (help :xtics) or (help :terminal\\ pngcario)
* Set plt:*save-all* to 't' to save all generated plots automatically
* 'reset' to restore most default gnuplot-settings
** Does NOT restore term, output, loadpath, linetype, fit, encoding, decimalsign, locale, psdir, overflow, or multiplot
* 'replot' to ... replot
** Only replots the last plot made in a multiplot setting
* 'restart-gnuplot' to get out of a pinch - also sets gnuplot's directory to the cwd of the lisp process
* If gnuplot is completely frozen from an improper command, kill the process manually in htop or task manager for example")
  nil)

(defun restart-gnuplot ()
  "Closes global gnuplot process and opens a new one. Also reset the global *current-plots*."
  (quit-gnuplot)
  (init-gnuplot)
  (send-command :terminal "qt size 1920,1080 linewidth 4 font 'Arial,40'"))

(defun switch-save-all ()
  (format nil "*save-all* is now ~a" (setf *save-all* (not *save-all*))))


(export '(linspace range transpose basic-read-file *gnuplot* *current-plots* init-gnuplot quit-gnuplot get-all-gnuplot-error-output send-strings replot reset send-strings-and-replot send-command send-command-and-replot plot plot-add rearrange-plots resend-plots plot3d plot3d-add plot3d-with-script help show retrieve help-cl-gnuplot restart-gnuplot send-plot-options send-plot-options-and-replot save-plot save-last-plot save-gnuplot-script load-gnuplot-script switch-save-all 3d-data-to-x-list-y-list-z-list x-list-y-list-z-matrix-to-3d-data ignore-ys-in-text))

;; TODO heteroaxis plot

;; TODO add default plot options
;; TODO consider options first then plot second
;; TODO why does it need to be run multiple times to work correctly - maybe MUST set terminal and such (should have a default) before set multiplot?
;; TODO expects you to know everything up front, no option to add multiplots afterwards via a regular plot-data command
(defun multiplot (mp-options-string &rest plots-and-options)
  "First takes multiplot options as found with (plt:show :multiplot). Setting 'layout rows,cols' (yplots,xplots) can be quite useful. Then takes a list of plot arguments like for the plt:plot command, like (list '(2 3 4 5) \"w lp title \\\"test multiplot\\\"\" ...). Finally takes plot options as given with the plt:send-command function, like (list :xrange [0:4] :ylabel \"\\\"Current\\\"\" ...)
Altogether:
(multiplot \"layout 2,2 title \\\"Multiplot Title\\\"\"
	     (list '(1 2 3 4) \"w lp title \\\"plot 1\\\"\" '(2 1 4 3) \"w lp title \\\"Double plot!\\\"\" :xlabel \"\\\"Amps\\\"\" :ylabel \"\\\"Pain\\\"\" :key \"top left\" :grid \"\")
	     (list '(10 25 30) \"w lp title \\\"plot 2\\\"\" :xlabel \"\\\"Bees\\\"\")
	     (list '(1 0 3 10) \"w lp title \\\"plot 3\\\"\" :xlabel \"\\\"Volts\\\"\" :key \"unset\")
	     (list '(-2 -4 -6 2) \"w lp title \\\"plot 4\\\"\" :xlabel \"\\\"Dogs\\\"\" :grid \"unset\"))"
  (unwind-protect
       (progn (send-strings (format nil "set multiplot ~a" mp-options-string))
	      (mapcar (lambda (x) (apply #'plot x)) plots-and-options))
    (send-strings "unset multiplot")))

(defun plot-function (&key function x y plot-format plot-options add)
  "Quickly plot a function of 1 or 2 variables over x (and y) list inputs.
(plot-function :function (lambda (x) (- (expt x 2) (* 4 x) 4))
		    :x (linspace -10 10 :len 100) 
		    :plot-format \"w lp title \\\"Quick Parabola\\\"\"
		    :plot-options (list :xlabel \"\\\"Time\\\"\" :ylabel \"\\\"Amplitude\\\"\"))"
  (let ((plot-fun (if add #'plot-add #'plot)))
    (cond ((null y)
	   (if plot-format
	       (funcall plot-fun x (mapcar function x) plot-format)
	       (funcall plot-fun x (mapcar function x)))
	   (apply #'send-plot-options-and-replot plot-options))
	  (t
	   (if plot-format
	       (funcall plot-fun x y (map-nest-2 function x y) plot-format)
	       (funcall plot-fun x y (map-nest-2 function x y)))
	   (apply #'send-plot-options-and-replot plot-options)))))

(defun mpltest ()
  (multiplot "layout 2,2 title \"Multiplot Title\""
	     (list '(1 2 3 4) "w lp title \"plot 1\"" '(2 1 4 3) "w lp title \"Double plot!\"" :xlabel "\"Amps\"" :ylabel "\"Pain\"" :key "top left" :grid "")
	     (list '(10 25 30) "w lp title \"plot 2\"" :xlabel "\"Bees\"")
	     (list '(1 0 3 10) "w lp title \"plot 3\"" :xlabel "\"Volts\"" :key "unset")
	     (list '(-2 -4 -6 2) "w lp title \"plot 4\"" :xlabel "\"Dogs\"" :grid "unset")))

;; Establish default sessions and settings
;; TODO check if qt is available, then wx, then x11
(unless (and (streamp (gnuplot-error-stream *gnuplot*))
	     (open-stream-p (gnuplot-error-stream *gnuplot*)))
  (init-gnuplot)
  (send-plot-options :terminal "qt size 1920,1080 linewidth 4 font 'Arial,40'"))

(export '(multiplot plot-function))
