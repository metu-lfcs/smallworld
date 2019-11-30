	;;;
;;; Some auxiliary functions
;;;

(defpackage :aux 
  (:use :common-lisp)
  (:export :multiset-table
		   :uniq
		   :tsv-to-list
		   :strip-package-deco
		   :starts-with-p
		   :empty-string-p
		   :read-from-file
		   :string-to-list
		   :csv-to-str-list
		   :shuffle-list
		   :read-file-as-string 
		   :translate-string
		   :replace-char-with-str
		   :restore-left-assoc
		   :symbol-starts-with
		   ))

(in-package aux)

(defmacro flip-bool (b)
  `(setf ,b (not ,b)))

(defun multiset-table (&optional (ht (make-hash-table)))
  "a closure based hash table implementation of alist
   to put, call with key and val -- duplicates are allowed;
   to get, call with just key
   call with :check key to check existence
   call with :count to get count
   call with :keys to get the list of keys"
  #'(lambda (&rest input)
	  (let ((head (car input))
			(tail (cadr input)))
		(cond ((equal head :count) (hash-table-count ht))
			  ((equal head :keys)
			   (let ((store))
				 (maphash 
				   #'(lambda (k v)
					   (declare (ignore v))
					   (push k store))
				   ht)
				 (reverse store)))
			  ((equal head :check) 
			   (nth-value 1 (gethash tail ht)))
			  ((equal head :get-table) ht)
			  (t 
				(if tail
				  (if (nth-value 1 (gethash head ht))
					(setf (gethash head ht) (cons tail (gethash head ht)))
					(setf (gethash head ht) (list tail)))
				  (if (nth-value 1 (gethash head ht))
					(nth-value 0 (gethash head ht))
					(error "Key unknown."))))))))

(defun uniq (lst &optional (comparator #'equal))
  "like Unix uniq"
  (if (endp lst)
	nil
	(cons (car lst)
		   (uniq (remove-if 
				   #'(lambda (x)
					   (funcall comparator (car lst) x))
				   (cdr lst))
				 comparator))))

(defun tsv-to-list (path)
  "convert a tab-separated value file to a list"
  (labels ((text-to-list (text)
						 (read-from-string 
						   (concatenate 'string "(" text ")")))
		   (read-lines (str &optional acc)
					   (let ((line (read-line str nil 'eof)))
						 (if (equal line 'eof)
						   acc
						   (read-lines 
							 str 
							 (append
							   acc
							   (list (text-to-list line))))))))
	(with-open-file (input-stream 
					  (if (typep path 'string)
						(make-pathname :name path)
						path)
					  :direction :input)
	  (read-lines input-stream))))

(defun strip-package-deco (expr)
  (read-from-string
	(format nil "~A" expr)))

(defun starts-with-p (str prefix)
  "check whether the str starts with the prefix -- both are strings"
  (and 
	(>= (length str) (length prefix))
	(string-equal prefix (subseq str 0 (length prefix)))))

(defun symbol-starts-with (sym chr)
  (char= (char (symbol-name sym) 0) chr))

(defun empty-string-p (str) 
  (and 
	(stringp str)
	(zerop (length str))))

(defun read-from-file (pathname)
  "return a list of lisp objects in the file at the pathname object"
  (with-open-file (str pathname :direction :input)
	(do ((x (read str nil :eof) (read str nil :eof))
		 (store nil (cons x store)))
	  ((eq x :eof) (reverse store)))))

(defun string-to-list (str)
  " \"a b c\" => (a b c)"
  (read-from-string (concatenate 'string "(" str ")")))

(defun csv-to-str-list (pname &optional (field-marker #\,) (text-delimiter #\"))  
  "read a csv file into a list of string (cell) lists"
  (labels ((proc-line (line)
					  (let ((store nil)
							(cell-buffer (make-string 1000))
							(pos 0)
							(protect nil)) ; double quote delimited protect zone is initially inactive
						(do ((index 0 (+ index 1)))
						  ((= index (length line))
						   (reverse
							 (cons (subseq cell-buffer 0 pos) store)))
						  (let ((current-char (char line index)))
							(cond ((eq current-char text-delimiter) (flip-bool protect))
								  (protect 
									(setf (char cell-buffer pos) current-char)
									(incf pos))
								  ((eq current-char field-marker)
								   (push (subseq cell-buffer 0 pos) store)
								   (setf pos 0))
								  (t
									(setf (char cell-buffer pos) current-char)
									(incf pos)))))))) 
	  (with-open-file (str pname :direction :input)
		(do ((x (read-line str nil :eof) (read-line str nil :eof))
			 (store nil (cons (proc-line x) store)))
		  ((eq x :eof) (reverse store))))))


(defun read-file-as-string (pname)
  "read the file at pname into a string without lisp-reading it"
  (with-open-file (str pname :direction :input) 
	(do ((line (read-line str nil :eof) (read-line str nil :eof))
		 (store "" (concatenate 'string store (format nil "~A~%" line))))
	  ((eq line :eof) store))))

(defun translate-string (str table)
  "translate the string str according to the character map table -- a dotted alist"
  (let ((master (copy-seq str)))
	(dotimes (index (length master) master)
	  (let ((replace-char (cdr (assoc (char master index) table))))
		(if replace-char
		  (setf (char master index) replace-char))))))

(defun random-pick (seq)
  "randomly pick an element from a sequence"
  (elt seq (random (length seq))))

(defun shuffle-list (lst &optional shuffled)
  (labels ((remove-first (item lst)
						 (if (equal (car lst) item)
						   (cdr lst)
						   (cons (car lst) (remove-first item (cdr lst))))))
	
  (if (endp lst)
	shuffled
	(let ((pick (random-pick lst)))
	  (shuffle-list (remove-first pick lst) (cons pick shuffled))))))

(defun replace-char-with-str (chr replace-str main-str)
  (let ((pos (position chr main-str)))
	(if pos
	  (concatenate 'string 
				 (subseq main-str 0 pos)
				 replace-str
				 (subseq main-str (+ pos 1) (length main-str)))
	  main-str)))

(defun restore-left-assoc (lst &optional store)
  "restore parentheses left associatively
  (a b c) => ((a b) c) "
  (cond ((endp lst) store)
		((endp store) (restore-left-assoc (cdr lst) (list (car lst))))
		(t (restore-left-assoc (cdr lst) (list (list (car store) (car lst)))))))

