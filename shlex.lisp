(in-package :shlex)

(def non-posix-word-chars
  "abcdfeghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")

(def posix-word-chars
  (string+ non-posix-word-chars
           "ßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ"))

(def default-commenters
  "#")

(def default-whitespace
  #.(string+ #\Space #\Tab #\Newline #\Return #\Linefeed))

(defun map-lexer-tokens (fn lexer)
  (fbind fn
    (with-slots (eof) lexer
      (loop for token = (get-token lexer)
            until (equal token eof)
            do (fn token)))))

(defun lexer-tokens (lexer)
  (collecting
    (map-lexer-tokens #'collect lexer)))

(defgeneric read-token (lexer)
  (:documentation "Read a raw token."))

(defgeneric get-token (lexer)
  (:documentation "Return the next token."))

(define-condition shlex-error (error)
  ((lexer :initarg :lexer)
   (base-message :accessor base-message)))

(define-condition no-closing-quotation (shlex-error)
  ((base-message :initform "Missing closing quotation in string"))
  (:report (lambda (c s)
             (with-slots (base-message lexer) c
               (format s "~a (line ~d):~%~a"
                       (base-message c)
                       (lexer-line-number lexer)
                       (lexer-input lexer))))))

(define-condition no-escaped-character (shlex-error)
  ((base-message :initform "No escaped character in string"))
  (:report (lambda (c s)
             (with-slots (base-message lexer) c
               (format s "~a (line ~d):~%~a"
                       (base-message c)
                       (lexer-line-number lexer)
                       (lexer-input lexer))))))

(defclass shlex ()
  ((pushback :initform nil)
   (pushback-chars :initform (queue))
   (punctuation-chars
    :documentation "Characters that will be considered punctuation.
Runs of punctuation are returned as a single token."
    :type string)
   (line-number
    :initform 1
    :reader lexer-line-number
    :documentation "Source line number.")
   (instream :type stream :initarg :instream)
   (state :type (or null character) :initform #\Space)
   (posix
    :initarg :posix
    :type boolean
    :documentation "If true, use POSIX-esque parsing rules.")
   (c-strings
    :initarg :c-strings
    :type boolean
    :documentation "If true, interpret ANSI-C quoting.")
   (whitespace-split
    :type boolean
    :initarg :whitespace-split
    :documentation "If true, only split tokens on whitespaces.")
   (whitespace
    :initarg :whitespace
    :type string
    :documentation "Characters that bound tokens.")
   (commenters
    :initarg :commenters
    :type string
    :documentation "String of characters to recognize as comment beginnners.")
   (escape
    :type string
    :initarg :escape
    :documentation "Characters to be considered an escape.")
   (word-chars
    :type string
    :documentation "String of characters to accumulate into tokens.")
   (quotes
    :type string
    :initarg :quotes
    :documentation "Characters that will be considered string quotes.")
   (escaped-quotes
    :type string
    :initarg :escaped-quotes
    :documentation "The subset of QUOTES that allow escapes.")
   (eof
    :documentation "The token that determines EOF.")
   (debug-input
    :initarg :debug-input
    :type string
    :reader lexer-input)
   (token-buffer
    :initform (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character)
    :documentation "Token buffer."))
  (:default-initargs
   :posix nil
   :c-strings nil
   :whitespace default-whitespace
   :whitespace-split nil
   :commenters default-commenters
   :quotes "\'\""
   :escape "\\"
   :escaped-quotes "\""
   :debug-input "UNKNOWN"))

(defmethod initialize-instance :after ((self shlex)
                                       &key punctuation-chars posix)
  (with-slots (quotes escaped-quotes) self
    (assert (every (op (find _ quotes)) escaped-quotes)
            nil "Escaped quotes must be a subset of quotes."))
  (with-slots (eof) self
    (setf eof (if posix nil "")))
  (with-slots (word-chars) self
    (setf word-chars
          (if posix posix-word-chars non-posix-word-chars)))
  (cond ((null punctuation-chars)
         (setf punctuation-chars ""))
        ((eql punctuation-chars t)
         (setf punctuation-chars "();<>|&")))
  (with-slots ((pchars punctuation-chars)) self
    (setf pchars punctuation-chars))
  (with-slots (word-chars) self
    (when (not (emptyp punctuation-chars))
      (setf word-chars
            (remove-if (op (find _ punctuation-chars))
                       ;; Chars allowed in file names, args, urls, wildcards.
                       (string+ word-chars "~-+./*?=:,%")))))
  (with-slots (punctuation-chars whitespace-split) self
    (when (and (not (emptyp punctuation-chars))
               whitespace-split)
      (simple-style-warning "Combining ~a and ~a is not advised."
                            :punctuation-chars
                            :whitespace-split)))
  (macrolet ((sort-slot (name)
               "Prepare string slots for search. Order them for
bisection and coerce them to simple arrays of characters."
               `(callf (op (sort-new (coerce _ '(simple-array character (*)))
                                     #'char<))
                       (slot-value self ',name))))
    (sort-slot word-chars)
    (sort-slot punctuation-chars)
    (sort-slot quotes)
    (sort-slot escaped-quotes)
    (sort-slot whitespace)
    (sort-slot commenters)))

(defmethod get-token ((self shlex))
  (with-slots (pushback) self
    (if pushback
        (pop pushback)
        (read-token self))))

(defun find-sorted (char string)
  "Find CHAR in STRING, a sorted string, by bisection."
  (declare ((simple-array character (*)) string)
           (optimize speed (safety 1)))
  (and (characterp char)
       (let* ((len (length string)))
         (if (< len 5)
             (if (< len 2)
                 (if (zerop len) nil
                     (char= (aref string 0) char))
                 (find char string))
             (let ((idx (bisect-left string char #'char<)))
               (and (< idx (length string))
                    (char= char (aref string idx))))))))

(defun string-conc (string x)
  "Add X, a character or string, to the end of STRING, an adjustable
string with a fill pointer."
  (etypecase x
    (character
     (vector-push-extend x string))
    ((string 0))
    ((string 1)
     (string-conc string (aref x 0)))
    (string
     (with-output-to-string (out string)
       (declare (dynamic-extent out))
       (write-string x out))))
  string)

(defmethods shlex (self token-buffer)
  (:method token (self)
    token-buffer)
  (:method (setf token) ((value string) self)
    ;; Clear the buffer.
    (setf token-buffer
          (make-array (max 10 (length value))
                      :element-type 'character
                      :adjustable t
                      :fill-pointer (length value)))
    ;; Can't use initial-contents.
    (replace token-buffer value)
    token-buffer))

(defmethod read-token ((self shlex))
  "Read a raw token."
  (nest
   (with-slots (pushback
                pushback-chars
                punctuation-chars
                instream
                line-number
                state
                posix
                whitespace
                whitespace-split
                commenters
                escape
                word-chars
                quotes
                escaped-quotes
                c-strings)
       self)
   ;; Note that reading `token' is unsafe and resets it.
   (with-accessors ((token token))
       self)
   (let ((punctuation-chars? (not (emptyp punctuation-chars)))
         (escaped-state #\ )
         (quoted? nil))
     (declare (boolean quoted?)
              (character escaped-state)))
   (nlet rec ()
     (let ((next-char
             (if (and punctuation-chars?
                      (not (queue-empty-p pushback-chars)))
                 (deq pushback-chars)
                 (read-char instream nil nil))))
       (when (eql next-char #\Newline)
         (incf line-number))
       (cond ((null state)
              ;; Past end of file.
              (setf token ""))
             ((eql state #\ )
              (cond ((no next-char)
                     ;; End of file.
                     (setf state nil))
                    ((find-sorted next-char whitespace)
                     (unless (or (not (emptyp token))
                                 (and posix quoted?))
                       (rec)))
                    ((find-sorted next-char commenters)
                     (read-line instream)
                     (incf line-number)
                     (rec))
                    ((and posix (find-sorted next-char escape))
                     (setf escaped-state #\a
                           state next-char)
                     (rec))
                    ((find-sorted next-char word-chars)
                     (setf token (string next-char)
                           state #\a)
                     (rec))
                    ((find-sorted next-char punctuation-chars)
                     (setf token (string next-char)
                           state #\c)
                     (rec))
                    ((find-sorted next-char quotes)
                     (unless posix
                       (setf token (string next-char)))
                     (setf state next-char)
                     (rec))
                    (whitespace-split
                     (setf token (string next-char)
                           state #\a)
                     (rec))
                    (t
                     (setf token (string next-char))
                     (unless (or (not (emptyp token))
                                 (and posix quoted?))
                       (rec)))))
             ;; Inside a quote.
             ((and (characterp state)
                   (find-sorted state quotes))
              (unless next-char         ;end of file
                (error 'no-closing-quotation :lexer self))
              (cond ((eql next-char state)
                     (if (not posix)
                         (progn
                           (string-conc token next-char)
                           (setf state #\ ))
                         (progn
                           (setf state #\a
                                 quoted? t)
                           (rec))))
                    ((and posix
                          (find-sorted next-char escape)
                          (find-sorted state escaped-quotes))
                     (setf escaped-state state
                           state next-char
                           quoted? t)
                     (rec))
                    (t (string-conc token next-char)
                       (setf quoted? t)
                       (rec))))
             ;; After an escape character.
             ((find-sorted state escape)
              (unless next-char
                (error 'no-escaped-character :lexer self))
              ;; Ignore escaped newline: it is just a continuation
              ;; character.
              (when (and posix (eql next-char #\Newline))
                (rec))
              ;; In posix shells, only the quote itself or the escape
              ;; character may be escaped within quotes.
              (when (and (find-sorted escaped-state quotes)
                         (not (eql next-char state))
                         (not (eql next-char escaped-state)))
                (string-conc token state))
              (string-conc token next-char)
              (setf state escaped-state)
              (rec))
             ((or (eql state #\a)
                  (eql state #\c))
              (cond ((no next-char)
                     ;; End of file.
                     (setf state nil))
                    ((find-sorted next-char whitespace)
                     (setf state #\ )
                     (unless (or (not (emptyp token))
                                 (and posix quoted?))
                       (rec)))
                    ((find-sorted next-char commenters)
                     (read-line instream)
                     (incf line-number)
                     (if (not posix)
                         (rec)
                         (progn
                           (setf state #\ )
                           (unless (or (not (emptyp token))
                                       (and posix quoted?))
                             (rec)))))
                    ((eql state #\c)
                     (if (find-sorted next-char punctuation-chars)
                         (progn
                           (string-conc token next-char)
                           (rec))
                         (progn
                           (unless (find-sorted next-char whitespace)
                             (enq next-char pushback-chars))
                           (setf state #\ ))))
                    ((and posix (find-sorted next-char quotes))
                     (setf state next-char)
                     (rec))
                    ((and posix (find-sorted next-char escape))
                     (setf escaped-state #\a
                           state next-char)
                     (rec))
                    ((or (find-sorted next-char word-chars)
                         (find-sorted next-char quotes)
                         whitespace-split)
                     (string-conc token next-char)
                     (rec))
                    (t
                     (if punctuation-chars?
                         (enq next-char pushback-chars)
                         (push (string next-char) pushback))
                     (setf state #\ )
                     (unless (or (not (emptyp token))
                                 (and posix quoted?))
                       (rec)))))
             ;; We're done; fall through.
             (t)))
     (let ((result (shiftf token "")))
       (if (and posix
                (not quoted?)
                (emptyp result))
           nil
           result)))))

(def safe-char-map
  (lret ((map (make-array 128 :element-type 'bit)))
    (loop for (start end) in '((#\a #\z) (#\A #\Z) (#\0 #\9))
          do (loop for code from (char-code start) to (char-code end)
                   do (setf (aref map code) 1)))
    (loop for c across "_@%+=:,./-"
          do (setf (aref map (char-code c)) 1)))
  "Bitmap for a safe char.")

(defsubst safe-char? (char)
  (declare (optimize speed (safety 1))
           (type (simple-array bit (128)) safe-char-map))
  (let ((code (char-code char)))
    (and (< code 128)
         (eql 1 (aref safe-char-map code)))))

(defun quote (s)
  "Return a shell-escaped version of string S."
  (check-type s string)
  (cond ((emptyp s)
         "''")
        ((every #'safe-char? s)
         s)
        (t (string+ "'"
                    (string-replace-all "'"
                                        s
                                        "'\"'\"'")
                    "'"))))

(defun split (source &rest args &key &allow-other-keys)
  "Split SOURCE, a string or stream, into tokens using shell-like rules.

Pass `:posix` (true by default) to obey POSIX parsing rules.

Pass `:punctuation-chars` (nil by default) if you want runs of punctuation characters (semicolons, greater and less than signs, ampersands, etc.) to be parsed as separate tokens."
  (collecting
    (apply #'map-tokens #'collect source args)))

(defgeneric map-tokens (fn source &key)
  (:documentation "Map FN over the tokens of SOURCE, a string or stream, using shell-like rules.
See the documentation of `split' for a discussion of possible options."))

(defmethod map-tokens (fn (s string) &rest args &key &allow-other-keys)
  (with-input-from-string (in s)
    (apply #'map-tokens fn in :debug-input s
           args)))

(defmethod map-tokens (fn (in stream) &rest args
                       &key comments (posix t)
                            debug-input
                            punctuation-chars
                            (whitespace-split (not punctuation-chars))
                       &allow-other-keys)
  (let ((lexer (apply #'make 'shlex
                      :debug-input (or debug-input "")
                      :instream in
                      :posix posix
                      :whitespace-split whitespace-split
                      :commenters (if comments
                                      default-commenters
                                      "")
                      (remove-from-plist args :comments :debug-input))))
    (map-lexer-tokens fn lexer)))

(define-do-macro do-tokens ((token (source &rest args &key &allow-other-keys)
                                   &optional return)
                            &body body)
  "Iterate over the tokens of SOURCE, using shell-like rules."
  `(map-tokens (lambda (,token)
                 ,@body)
               ,source ,@args))

(defun ensure-string-stream (s)
  (etypecase s
    (string (make-string-input-stream s))
    (stream s)))

(defun make-lexer (s &rest args &key &allow-other-keys)
  (apply #'make
         'shlex
         :instream (ensure-string-stream s)
         args))

(define-compiler-macro make-lexer (s &rest args)
  `(make 'shlex
         :instream (ensure-string-stream ,s)
         ,@args))
