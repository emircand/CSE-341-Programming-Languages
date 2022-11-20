(defvar delimiter " ")
(defvar Operator (list "+" "-" "/" "%" "**" "*" "(" ")" "\"" ","))
(defvar Keywords (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(defvar comment ";")
(defvar cc 0)


; gppinterpreter function
; Reads file (or user input) line by line, for each line calls split_line function if that line does not start with ";"
(defun gppinterpreter (&optional input)
(if (equal input nil)
   (progn
      (loop
         (print ">>>")
         (setf line (read-line))
         (setf line (string line))
         (if (not(string-equal (char line 0) comment))
               (progn
                  (setf line (string-trim '(#\Space #\Tab #\Newline) line))

                  (split_line line)
               )
               (format t "('~a' 'COMMENT')" line)
         )
         (when (equal line nil) (return-from gppinterpreter nil))
      )
   )
   (progn
      (with-open-file (stream input)
         (loop for line = (read-line stream nil)
            while line
            collect line
            do
            (if (not(= (length line) 0))
               (if (not(string-equal (aref line 0) comment))
                  (progn
                     (setf line (string-trim '(#\Space #\Tab #\Newline) line))
                     (split_line line)
                  )
                  (format t "('~a' 'COMMENT')" line)
               )
            )
         )
      )  
   
   )

)
)
; Splits line by delimiter. Calls split_token fuction to split the tokens if they are sticked ( For example "1)" will be "1" and ")" )
(defun split_line (line)
   (defvar _temp_ (list))
   (loop for i from 0 to (- (length line) 1)
      do
     (if (or (string-equal (aref line i) delimiter) (= i (- (length line) 1))) 
         (progn
            (if (= i (- (length line) 1))
               (setq _temp_ (append _temp_ (list (aref line i))))      
            )
            (split_token _temp_)
            (setq _temp_ nil)
         ) ;if it is true
         (progn
            (setq _temp_ (append _temp_ (list (aref line i))))

         ) ;if it is null
     ) 
   )
)
;Splits token if there is sticked words.
(defun split_token (splitted)
   (setf word "")
   (defvar _token_ (list))
   ;(print "SPLITTED: ")
   ;(print splitted)
   (setq last_element "")
   (if (not (isWord splitted) ) 
       (progn
         (loop for x in splitted
            do
               (setq last_element x)
               (if (isOperatorChar x)
                  (progn
                     (if (not(string-equal "" word))
                        (progn
                           (setq _token_ (append _token_ (list word)))
                           (setf word "")
                        )
                     )
                     (setq _token_ (append _token_ (list x)))
                  ) ;TRUE
                 (setf word (concatenate 'string word (string x))) ;FALSE
               )
         )
         (if (not (isOperatorChar last_element))
            (setq _token_ (append _token_ (list word)))
         )
      )
      (progn
         (loop for x in splitted
            do
            (setf word (concatenate 'string word (string x)))
         )
         (setq _token_ (append _token_ (list word)))
      )
   )
   (checkToken _token_ word)
   (setq _token_ nil)
)
; Checks token and prints the appropriate tokens
(defun checkToken (tokenList word)
   ;(print "TOKEN: ")
   ;(print tokenList)
   (loop for x in tokenList
      do 
      (setq flag 0)
      (loop for op in Operator
         do (if (string-equal x op)
            (progn
               (printOperator x)
               (setq flag 1)
            )  
         )
      )
      (if (equal flag 0)
         (loop for kw in Keywords
            do (if (string-equal x kw)
                (progn
                  (printKeyword x)
                  (setq flag 1)
                )
            )
         ) 
      )
      (if (equal flag 0)
         (if (equal (isValueInteger x) t)
            (progn
               (printValuei x)
               (setq flag 1)
            )
         )
      )
      (if (equal flag 0)
         (if (equal (isValueReal x) t)
            (progn
               (printValuef x)
               (setq flag 1)
            )
         )
      )
      (if (equal flag 0)
         (if (isIdentifier x)
            (progn
               (printIdentifier x)
            )
         )
      )
   )
)
; Checks the parameter is identifier or not
(defun isIdentifier (var)
   (setq returnValue T)
   (loop for i from 0 to (- (length var) 1)
      do
      (if (= i 0)
         (if (alpha-char-p (aref var i))
            (setq returnValue T)
            (setq returnValue nil)
         )
         (if (or (alpha-char-p (aref var i)) (isDigit (aref var i)))
            (setq returnValue T)
            (return-from isIdentifier nil)
         )
      )
   )
   (return-from isIdentifier returnValue)
)

; Checks the character is digit or not
(defun isDigit (character)
   (loop for i from 0 to 9 
      do
      (if (equal (digit-char i) character)
         (return-from isDigit T)
      )
   )
   (return-from isDigit nil)
)
; Prints token for identifier
(defun printIdentifier (var)
   (format t "('~a' 'IDENTIFIER')" var)
)
; Prints token for value
(defun printValuei (var)
   (format t "('~a' 'VALUEI')" var)
)

(defun printValuef(var)
    (format t "('~a' 'VALUEF')" var)
)

; Checks the value rules for integers 
(defun isValueInteger (var)
   (setq returnValue T)
   (if (= (length var) 1)
      (if (string-equal (aref var 0) "0")
         (return-from isValueInteger T)
         (if (isDigit (aref var 0))
            (return-from isValueInteger T)
            (return-from isValueInteger nil)
         )
      )
      (progn
         (loop for i from 0 to (- (length var) 1)
             do
            (if (= i 0)
               (if (string-equal (aref var i) "0")
                  (return-from isValueInteger nil)
                  (if (isDigit (aref var i))
                    (setq returnValue T)
                    (return-from isValueInteger nil)
                  )
               )
               (if (isDigit (aref var i))
                  (setq returnValue T)
                  (return-from isValueInteger nil)
               )
            )
         )
         (return-from isValueInteger returnValue)  
      )
   )
)
; Checks the value rules for real numbers
(defun isValueReal (var)

   (setq returnValue T)
   (setq right_side 0)
   (loop for i from 0 to (- (length var) 1)
      do
      (if (= right_side 0)
         (progn
            (if (= i 0)
               (if (string-equal (aref var i) "0")
                     (return-from isValueReal nil)
                     (if (isDigit (aref var i))
                        (setq returnValue T)
                        (return-from isValueReal nil)
                     )
               )
               (if (isDigit (aref var i))
                  (setq returnValue T)
                  (if (string-equal "f" (aref var i))
                     (setq right_side 1)
                     (return-from isValueReal nil)
                  )
               )
            )
         ) ; left_side
         
         (if (isDigit (aref var i))
            (setq returnValue T)
            (return-from isValueReal nil) 
         )   ; right_side
      )
   )
   (return-from isValueReal returnValue)
)
; Prints tokens of keywords
(defun printKeyword (var)
   (cond
      ((string-equal var "and") (print "('and' 'KW_AND')"))
      ((string-equal var "or") (print "('or' 'KW_OR')"))
      ((string-equal var "not") (print "('not' 'KW_NOT')"))
      ((string-equal var "equal") (print "('equal' 'KW_EQUAL')"))
      ((string-equal var "less") (print "('less' 'KW_LESS')"))
      ((string-equal var "nil") (print "('nil' 'KW_NIL')"))
      ((string-equal var "list") (print "('list' 'KW_LIST')"))
      ((string-equal var "append") (print "('append' 'KW_APPEND')"))
      ((string-equal var "concat") (print "('concat' 'KW_CONCAT')"))
      ((string-equal var "set") (print "('set' 'KW_SET')"))
      ((string-equal var "deffun") (print "('deffun' 'KW_DEFFUN')"))
      ((string-equal var "for") (print "('for' 'KW_FOR')"))
      ((string-equal var "if") (print "('if' 'KW_IF')"))
      ((string-equal var "exit") (print "('exit' 'KW_EXIT')"))
      ((string-equal var "load") (print "('load' 'KW_LOAD')"))
      ((string-equal var "disp") (print "('disp' 'KW_DISP')"))
      ((string-equal var "true") (print "('true' 'KW_TRUE')"))
      ((string-equal var "false") (print "('false' 'KW_FALSE')"))
   )
)
; Prints token for operators
(defun printOperator (var)
   (cond
      ((string-equal var "+") (print "('+' 'OP_PLUS')"))
      ((string-equal var "-") (print "('-' 'OP_MINUS')"))
      ((string-equal var "/") (print "('/' 'OP_DIV')"))
      ((string-equal var "%") (print "('%' 'OP_DIV2')"))
      ((string-equal var "**") (print "('**' 'OP_DBLMULT')"))
      ((string-equal var "*") (print "('*' 'OP_MULT')"))
      ((string-equal var "(") (print "('(' 'OP_OP')"))
      ((string-equal var ")") (print "(')' 'OP_CP')"))
      ((string-equal var "\"") 
         (if (= cc 0)
            (progn
               (print "OP_OC")
               (setf cc 1)
            )
            (progn
               (print "OP_CC")
               (setf cc 0)
            )
         )
      )
      ((string-equal var ",") (print "(',' 'OP_COMMA')")) 
   )
)
; Checks the given parameter is operator or not
(defun isOperatorChar (c)
   (loop for op in Operator
      do
         (if (string-equal c op)
            (return-from isOperatorChar t)                 
         )
   )
   (return-from isOperatorChar nil)
)
; Checks the given parameter is word or not
(defun isWord (splitted)
   (loop for x in splitted
      do
      (loop for y in Operator
         do (if (string-equal x y)
              (return-from isWord nil)
            )
      )
   )
   (return-from isWord T)
)
; Starts the lisp program and calls gppinterpreter function (Which is above)
(defun start ()
   (terpri)
   (terpri)
   (print " -     LISP PROGRAM     - ")
   (terpri)
   (write "Please enter a file name or enter '!' to continue (CTRL + C to exit) : ")
   (setf input (read))
   (if (string-equal "!" input) 
      (gppinterpreter )
      (gppinterpreter (string-downcase input))
   )
   
)

(start )

