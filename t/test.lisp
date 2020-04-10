(in-package :shlex/test)

(def data
  (read-file-into-string
   (asdf:system-relative-pathname :shlex/test "t/data.txt")
   :external-format :utf-8))

(def posix-data
  (read-file-into-string
   (asdf:system-relative-pathname :shlex/test "t/posix-data.txt")
   :external-format :utf-8))

(defun split-test-cases (string)
  (loop for line in (lines string)
        for string = (string-replace-all "\\n" line #.(string #\Newline))
        collect (butlast (split-sequence #\| string))))

(def data-test-cases
  (split-test-cases data))

(def posix-test-cases
  (split-test-cases posix-data))

(defconst ascii-lowercase
  (map 'string #'code-char (range (char-code #\a) (char-code #\z))))

(defconst ascii-uppercase
  (map 'string #'code-char (range (char-code #\A) (char-code #\Z))))

(defconst ascii-letters
  (string+ ascii-lowercase ascii-uppercase))

(defconst digits "0123456789")

(def-suite shlex)

(in-suite shlex)

(defun run-shlex-tests ()
  (5am:run! 'shlex))

(defmacro test (name &body body)
  `(5am:test ,name (local ,@body)))

(defun split-test (test-cases &rest kwargs &key &allow-other-keys)
  (dolist (test-case test-cases)
    (destructuring-bind (string &rest good-split) test-case
      (let ((l (apply #'shlex:split string kwargs)))
        (is (equal l good-split)
            "~s: ~s is wrong, should be ~s"
            string l good-split)))))

(test split-posix
  "Test data splitting with posix parser."
  (split-test posix-test-cases :comments t))

(test split-non-posix
  "Test data splitting with non-posix parser."
  (split-test data-test-cases
              :comments t
              :posix nil
              :whitespace-split nil))

(def amps-and-pipes
  '("&&" "&" "|&" ";&" ";;&" "||" "|" "&|" ";|" ";;|"))

(test syntax-split-ampersand-and-pipe
  "Test handling of syntax splitting of &, |."
  (dolist (delimiter amps-and-pipes)
    (let ((src (list
                (fmt "echo hi ~a echo bye" delimiter)
                (fmt "echo hi~a echo bye" delimiter)
                (fmt "echo hi~aecho bye" delimiter)))
          (ref
            (list "echo" "hi" delimiter "echo" "bye")))
      (dolist (ss src)
        (is (equal ref (shlex:split ss :punctuation-chars t)))))))

(test syntax-split-semicolon
  "Test handling of syntax splitting of ;."
  (dolist (delimiter '(";" ";;" ";&" ";;&"))
    (let ((src (list (fmt "echo hi ~a echo bye" delimiter)
                     (fmt "echo hi~a echo bye" delimiter)
                     (fmt "echo hi~aecho bye" delimiter)))
          (ref (list "echo" "hi" delimiter "echo" "bye")))
      (dolist (ss src)
        (is (equal ref (shlex:split ss :punctuation-chars t)))))))

(test syntax-split-redirect
  "Test handling of syntax splitting of >"
  (dolist (delimiter '("<" "|"))
    (let ((src (list (fmt "echo hi ~a out" delimiter)
                     (fmt "echo hi~a out" delimiter)
                     (fmt "echo hi~a out" delimiter)))
          (ref (list "echo" "hi" delimiter "out")))
      (dolist (ss src)
        (is (equal ref (shlex:split ss :punctuation-chars t)))))))

(test syntax-split-paren
      "Test handling of syntax splitting of ()."
      (def ref '("(" "echo" "hi" ")"))
      (def src (list "( echo hi )" "(echo hi)"))
      (dolist (ss src)
        (is (equal ref (shlex:split ss :punctuation-chars t)))))

(test syntax-split-custom
      "Test handling of syntax splitting with custom chars"
      (def ref '("~/a" "&" "&" "b-c" "--color=auto" "||" "d" "*.py?"))
      (def ss "~/a && b-c --color=auto || d *.py?")
      (is (equal ref (shlex:split ss :punctuation-chars "|"))))

(test token-types
  (loop for (source expected) in '(("a && b || c"
                                    (("a" "a") ("&&" "c") ("b" "a")
                                     ("||" "c") ("c" "a"))))
        for s = (make-lexer source :punctuation-chars t)
        for observed = '()
        do (loop for tok = (shlex::get-token s)
                 if (equal tok (slot-value s 'shlex::eof))
                   do (loop-finish)
                 else if (find (aref tok 0) (slot-value s 'shlex::punctuation-chars))
                        do (push (list tok "c") observed)
                 else do (push (list tok "a") observed))
        finally (is (equal (reverse observed) expected))))

(test punctuation-in-word-chars
  "Test that any punctuation chars are removed from word chars."
  (def s (make-lexer "a_b__c" :punctuation-chars "_"))
  (is-false (find "_" (slot-value s 'shlex::word-chars)))
  (is (equal (lexer-tokens s) '("a" "_" "b" "__" "c"))))

(test punctuation-with-whitespace-split
      "Test that with whitespace-split, behaviour is as expected"
      ;; whitespace-split is nil, so splitting will be based on
      ;; punctuation-chars
      (is (equal (shlex:split "a  && b  ||  c" :punctuation-chars "&")
                 '("a" "&&" "b" "|" "|" "c")))
      ;; whitespace-split is True, so splitting will be based on
      ;; white space
      (is (equal (shlex:split "a  && b  ||  c" :punctuation-chars "&"
                                         :whitespace-split t)
                 '("a" "&&" "b" "||" "c"))))

(test punctuation-with-posix
  "Test that punctuation-chars and posix behave correctly together."
  ;; see Issue #29132
  (is (equal (shlex:split "f >\"abc\"" :posix t :punctuation-chars t)
             '("f" ">" "abc")))
  (is (equal (shlex:split "f >\\\"abc\\\"" :posix t :punctuation-chars t)
             '("f" ">" "\"abc\""))))

(test empty-string-handling
  "Test that parsing of empty strings is correctly handled."
  (def expected '("" ")" "abc"))
  (loop for punct in '(nil t)
        for slist = (shlex:split "'')abc" :posix t
                                          :punctuation-chars punct
                                          :whitespace-split nil)
        do (is (equal slist expected)))
  (setf expected '("''" ")" "abc"))
  (is (equal (shlex:split "'')abc" :punctuation-chars t
                                   :posix nil)
             expected)))

(test quote
  (def safeunquoted (string+ ascii-letters digits "@%_-+=:,./"))
  (def unicode-sample (map 'string #'code-char '(#xe9 #xe0 #xdf))) ; e + acute accent, a + grave, sharp s
  (def unsafe (string+ "\"`$\\!" unicode-sample))
  (is (equal (shlex:quote "") "''"))
  (is (equal (shlex:quote safeunquoted) safeunquoted))
  (is (equal (shlex:quote "test file name") "'test file name'"))
  (loop for u across unsafe
        do (is (equal (shlex:quote (fmt "test~aname" u))
                      (fmt "'test~aname'" u))))
  (loop for u across unsafe
        do (is (equal (shlex:quote (fmt "test~a'name'" u))
                      (fmt "'test~a'\"'\"'name'\"'\"''" u)))))

(test escaped-newline
  (is (equal '("hello" "world")
             (shlex:split "hello
world")))
  (is (equal '("hello\\" "world")
             (shlex:split "hello\\
world" :posix nil)))
  (is (equal '("helloworld")
             (shlex:split "hello\\
world"))
      (equal (fmt "hello~%world")
             (shlex:split "'hello\
world'"))))

(test chmod
  (is (equal '("chmod" "+x" "foo")
             (shlex:split "chmod +x foo"
                          :punctuation-chars t))))

(test url
  (is (equal '("http://example.com")
             (shlex:split "http://example.com"
                          :punctuation-chars t))))

(test commas
  (is (equal '("cut" "-f1,3")
             (shlex:split "cut -f1,3"
                          :punctuation-chars t))))
