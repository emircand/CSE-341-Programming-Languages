(defvar delimiter " ")
(defvar Operator (list "+" "-" "/" "*" "(" ")" "," "and" "or" "not" "eq" "gt" "set"))
(defvar Keywords (list "nil" "defvar" "deffun" "while" "if" "load" "disp" "true" "false"))
(defvar comment ";")
(defvar cc 0)
(defvar token-list (list))
(defvar variables (list))


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
               (print "COMMENT")
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
                  (print "COMMENT")
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
   (checkToken token-list)
   (setq token-list nil)
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
   (loop for x in _token_
        do
        (if (not (string-equal x ""))
            (setq token-list (append token-list (list x)))
        )
   )
   (setq _token_ nil)
)
; Checks token and prints the appropriate tokens
(defun checkToken (separated-list)
   ;(print "TOKEN: ")
   (setq token-list nil)
   ;(print separated-list)
   (loop for x in separated-list
      do 
      (setq flag 0)
      (loop for op in Operator
         do (if (string-equal x op)
            (progn
               (setq token-list (append token-list (list (getOperator x))))
               (setq flag 1)
            )  
         )
      )
      (if (equal flag 0)
         (loop for kw in Keywords
            do (if (string-equal x kw)
                (progn
                (setq token-list (append token-list (list (getKeyword x))))
                (setq flag 1)
                )
            )
         ) 
      )
      (if (equal flag 0)
         (if (or (isValueReal x) (isValueInteger x))
            (progn
               (setq token-list (append token-list (list x)))
               (setq flag 1)
            )
         )
      )
      (if (equal flag 0)
         (if (isIdentifier x)
            (progn
               (setq token-list (append token-list (list x)))
            )
         )
      )
   )

   (setq num-par 0)
   (setq no_parantheses 1)
   (loop for x in token-list
        do
            (if (string-equal x "OP")
                (progn
                    (setq num-par (+ num-par 1))
                    (setq no_parantheses 0)
                )
               
            )
             (if (string-equal x "CP")
               (progn
                    (setq num-par (- num-par 1))
                    (setq no_parantheses 0)
                )
            )   
    )

    (if (or (not (equal num-par 0)) (equal no_parantheses 1)) 
        (progn
            (print "Syntax ERROR! Parantheses missing.")
            (return-from checkToken nil)
        )
    )

   (print "Syntax OK")
   (print "Result: ")
   (print (syntax token-list))

)



(defun syntax (arg-list)
  (let ((a 0) (b 0) (f_exp 0))
    (cond
      ((string-equal (nth 1 arg-list) "OP_PLUS")
        (if (string-equal (nth 2 arg-list) "OP")
          (setq a (syntax (sublist arg-list 2 (- (length arg-list) 1))))
          (setq a (parse-integer (nth 2 arg-list)))
        )
        (if (string-equal (nth 3 arg-list) "OP")
          (setq b (syntax (sublist arg-list 3 (- (length arg-list) 1))))
          (setq b (parse-integer (nth 3 arg-list)))
        )
        (if (or (equal a nil) (equal b nil))
          (progn
            (print "SYNTAX ERROR!")
            (return-from syntax nil))
          (return-from syntax (+ a b))
        )
      )
      ((string-equal (nth 1 arg-list) "OP_MINUS")
        (if (string-equal (nth 2 arg-list) "OP")
          (setq a (syntax (sublist arg-list 2 (- (length arg-list) 1))))
          (setq a (parse-integer (nth 2 arg-list)))
        )
        (if (string-equal (nth 3 arg-list) "OP")
          (setq b (syntax (sublist arg-list 3 (- (length arg-list) 1))))
          (setq b (parse-integer (nth 3 arg-list)))
        )
        (if (or (equal a nil) (equal b nil))
          (progn
            (print "SYNTAX ERROR!")
            (return-from syntax nil))
          (return-from syntax (- a b))
        )
      )
      ((string-equal (nth 1 arg-list) "OP_DIV")
        (if (string-equal (nth 2 arg-list) "OP")
          (setq a (syntax (sublist arg-list 2 (- (length arg-list) 1))))
          (setq a (parse-integer (nth 2 arg-list)))
        )
        (if (string-equal (nth 3 arg-list) "OP")
          (setq b (syntax (sublist arg-list 3 (- (length arg-list) 1))))
          (setq b (parse-integer (nth 3 arg-list)))
        )
        (if (or (equal a nil) (equal b nil))
          (progn
            (print "SYNTAX ERROR!")
            (return-from syntax nil))
          (return-from syntax (floor a b))
        )
      )
      ((string-equal (nth 1 arg-list) "OP_MULT")
        (if (string-equal (nth 2 arg-list) "OP")
          (setq a (syntax (sublist arg-list 2 (- (length arg-list) 1))))
          (setq a (parse-integer (nth 2 arg-list)))
        )
        (if (string-equal (nth 3 arg-list) "OP")
          (setq b (syntax (sublist arg-list 3 (- (length arg-list) 1))))
          (setq b (parse-integer (nth 3 arg-list)))
        )
        (if (or (equal a nil) (equal b nil))
          (progn
            (print "SYNTAX ERROR!")
            (return-from syntax nil))
          (return-from syntax (* a b))
        )
      )
      ((string-equal (nth 1 arg-list) "OP_SET")
       (setq b (if (string-equal "OP" (nth 3 arg-list))
                   (syntax (sublist arg-list 3 (- (length arg-list) 1)))
                 (nth 3 arg-list)))
         (if (contains (nth 2 arg-list))
           (update (nth 2 arg-list) b)
         (setq variables (append variables (list (list (nth 2 arg-list) b)))))
         b
      )

        ( (string-equal (nth 1 arg-list) "OP_AND")  
            (progn
                (if (not (string-equal "OP" (nth 2 arg-list)))
                  (progn 
                     (if (isIdentifier (nth 2 arg-list))
                     (setq a (parse-integer (getValue (nth 2 arg-list))))
                     (setq a (parse-integer (nth 2 arg-list)))
                     )
                  )
                    (progn
                        (setq a (syntax (sublist arg-list 2  (- (length arg-list) 1))))
                        (setq f_exp 1)
                    )
                )
                (if (equal f_exp 0)
                    (if (not (string-equal "OP" (nth 3 arg-list)))
                        (progn 
                           (if (isIdentifier (nth 3 arg-list))
                           (setq b (parse-integer (getValue (nth 3 arg-list))))
                           (setq b (parse-integer (nth 3 arg-list)))
                           )
                        )
                        (setq b (syntax (sublist arg-list 3  (- (length arg-list) 1))))
                    )
                    (if (not (string-equal "OP" (nth 6 arg-list)))
                        (progn 
                           (if (isIdentifier (nth 6 arg-list))
                           (setq b (parse-integer (getValue (nth 6 arg-list))))
                           (setq b (parse-integer (nth 6 arg-list)))
                           )
                        )
                        (setq b (syntax (sublist arg-list 6  (- (length arg-list) 1))))
                    )
                )
                (if (or (equal a nil) (equal b nil))
                  (progn
                     (print "SYNTAX ERROR!")
                     (return-from syntax nil)
                  )
                  (return-from syntax (and a b))
                )
            )
        )
        ( (string-equal (nth 1 arg-list) "OP_OR")  
            (progn
                (if (not (string-equal "OP" (nth 2 arg-list)))
                  (progn 
                     (if (isIdentifier (nth 2 arg-list))
                     (setq a (parse-integer (getValue (nth 2 arg-list))))
                     (setq a (parse-integer (nth 2 arg-list)))
                     )
                  )
                    (progn
                        (setq a (syntax (sublist arg-list 2  (- (length arg-list) 1))))
                        (setq f_exp 1)
                    )
                )
                (if (equal f_exp 0)
                    (if (not (string-equal "OP" (nth 3 arg-list)))
                        (progn 
                           (if (isIdentifier (nth 3 arg-list))
                           (setq b (parse-integer (getValue (nth 3 arg-list))))
                           (setq b (parse-integer (nth 3 arg-list)))
                           )
                        )
                        (setq b (syntax (sublist arg-list 3  (- (length arg-list) 1))))
                    )
                    (if (not (string-equal "OP" (nth 6 arg-list)))
                        (progn 
                           (if (isIdentifier (nth 6 arg-list))
                           (setq b (parse-integer (getValue (nth 6 arg-list))))
                           (setq b (parse-integer (nth 6 arg-list)))
                           )
                        )
                        (setq b (syntax (sublist arg-list 6  (- (length arg-list) 1))))
                    )
                )
                
                (if (or (equal a nil) (equal b nil))
                  (progn
                     (print "SYNTAX ERROR!")
                     (return-from syntax nil)
                  )
                  (return-from syntax (or a b))
                )
            )
        )
         ((string-equal (nth 1 arg-list) "KW_IF")
            (let ((predicate (syntax (sublist arg-list 2 3)))
                  (consequent (syntax (sublist arg-list 4 5)))
                  (alternative (syntax (sublist arg-list 6 7))))
            (if predicate
               consequent
               alternative))
         )
         ((string-equal (nth 1 arg-list) "KW_WHILE")
            (let ((predicate (syntax (sublist arg-list 2 3)))
                  (body (syntax (sublist arg-list 4 5))))
         (loop while predicate
            do
            (setq predicate (syntax (sublist arg-list 2 3)))
            (setq body (syntax (sublist arg-list 4 5))))))
         ( (string-equal (nth 1 arg-list) "OP_NOT")  
               (progn
                (if (not (string-equal "OP" (nth 2 arg-list)))
                  (progn 
                     (if (isIdentifier (nth 2 arg-list))
                     (setq a (parse-integer (getValue (nth 2 arg-list))))
                     (setq a (parse-integer (nth 2 arg-list)))
                     )
                  )
                    (progn
                        (setq a (syntax (sublist arg-list 2  (- (length arg-list) 1))))
                        (setq f_exp 1)
                    )
                )
                
                (if (equal a nil)
                  (progn
                     (print "SYNTAX ERROR!")
                     (return-from syntax nil)
                  )
                  (return-from syntax (not a))
                )
            )
         ) 
         ((string-equal (nth 1 arg-list) "OP_EQ")
            (if (not (string-equal "OP" (nth 2 arg-list)))
               (progn
                  (setq a (if (isIdentifier (nth 2 arg-list))
                              (parse-integer (getValue (nth 2 arg-list)))
                           (parse-integer (nth 2 arg-list))))
                  (setq f_exp 1)
               )
            (setq a (syntax (sublist arg-list 2 (- (length arg-list) 1)))))
            (if (equal f_exp 0)
               (if (not (string-equal "OP" (nth 3 arg-list)))
                  (progn
                     (setq b (if (isIdentifier (nth 3 arg-list))
                                 (parse-integer (getValue (nth 3 arg-list)))
                              (parse-integer (nth 3 arg-list))))
                     (setq f_exp 1))
                  (setq b (syntax (sublist arg-list 3 (- (length arg-list) 1)))))
            (if (not (string-equal "OP" (nth 6 arg-list)))
                  (progn
                  (setq b (if (isIdentifier (nth 6 arg-list))
                              (parse-integer (getValue (nth 6 arg-list)))
                              (parse-integer (nth 6 arg-list))))
                  (setq f_exp 1))
               (setq b (syntax (sublist arg-list 6 (- (length arg-list) 1))))))
            (if (or (equal a nil) (equal b nil))
               (progn
                  (print "SYNTAX ERROR!")
                  (return-from syntax nil))
            (return-from syntax (equal a b)) 
            ) 
         )
         (
         (string-equal (nth 1 arg-list) "OP_GT")
            (if (not (string-equal "OP" (nth 2 arg-list)))
               (progn
                  (setq a (if (isIdentifier (nth 2 arg-list))
                              (parse-integer (getValue (nth 2 arg-list)))
                           (parse-integer (nth 2 arg-list))))
                  (setq f_exp 1))
               (setq a (syntax (sublist arg-list 2 (- (length arg-list) 1)))))
            (if (equal f_exp 0)
               (if (not (string-equal "OP" (nth 3 arg-list)))
                  (progn
                     (setq b (if (isIdentifier (nth 3 arg-list))
                                 (parse-integer (getValue (nth 3 arg-list)))
                              (parse-integer (nth 3 arg-list))))
                     (setq f_exp 1))
                  (setq b (syntax (sublist arg-list 3 (- (length arg-list) 1)))))
               (if (not (string-equal "OP" (nth 6 arg-list)))
                  (progn
                  (setq b (if (isIdentifier (nth 6 arg-list))
                              (parse-integer (getValue (nth 6 arg-list)))
                              (parse-integer (nth 6 arg-list))))
                  (setq f_exp 1))
                  (setq b (syntax (sublist arg-list 6 (- (length arg-list) 1))))))
               (if (or (equal a nil) (equal b nil))
                  (progn
                  (print "SYNTAX ERROR!")
                  (return-from syntax nil))
               (return-from syntax (> a b))
               )
         )

         ( (string-equal (getkeyword (nth 1 arg-list)) "DEFV")
         (if (not (string-equal "OP" (nth 2 arg-list)))
            (progn
               (setq a (if (isIdentifier (nth 2 arg-list))
                           (parse-integer (getValue (nth 2 arg-list)))
                         (parse-integer (nth 2 arg-list))))
               (setq f_exp 1))
            (setq a (syntax (sublist arg-list 2 (- (length arg-list) 1)))))
         (if (equal f_exp 0)
            (if (not (string-equal "OP" (nth 3 arg-list)))
               (progn
                  (setq b (if (isIdentifier (nth 3 arg-list))
                               (parse-integer (getValue (nth 3 arg-list)))
                             (parse-integer (nth 3 arg-list))))
                  (setq f_exp 1))
               (setq b (syntax (sublist arg-list 3 (- (length arg-list) 1)))))
            (if (not (string-equal "OP" (nth 6 arg-list)))
               (progn
                 (setq b (if (isIdentifier (nth 6 arg-list))
                             (parse-integer (getValue (nth 6 arg-list)))
                           (parse-integer (nth 6 arg-list))))
                 (setq f_exp 1))
               (setq b (syntax (sublist arg-list 6 (- (length arg-list) 1))))))
            (if (or (equal a nil) (equal b nil))
               (progn
               (print "SYNTAX ERROR!")
               (return-from syntax nil))
            (return-from syntax (setq a b)))
         )
         ( (string-equal (getkeyword (nth 1 arg-list)) "KW_LIST")
            (let ((temp (list)))
               (loop for i from 2 to (- (length arg-list) 2)
                  do
                  (setq temp (append temp (list (nth i arg-list))))

               )
               (return-from syntax temp)
            )
         )  
        )
    )
    
)

(defun contains (var)
(loop for i from 0 to (- (length variables) 1)
   do
   (if (string-equal var (nth 0 (nth i variables)))
      (return-from contains T)
   )
)
nil
)

(defun update (var val)

(let ((temp (list)))

   (loop for i from 0 to (- (length variables) 1)
      do
      (if (string-equal var (nth 0 (nth i variables))) 
         (setq temp (append temp (list (list var val))))
         (setq temp (append temp (list (list var (nth 1 (nth i variables))))))
      )
   )

   (setq variables temp)

)

)

(defun getValue (var)
(loop for i from 0 to (- (length variables) 1)
   do
   (if (string-equal var (nth 0 (nth i variables)))
      (return-from getValue (nth 1 (nth i variables)))
   )
)
nil
)

(defun sublist (arr start end)

   (let ((temp (list)))

   (loop for i from start to end
         do
         (setq temp (append temp (list (nth i arr))))
      )
   (return-from sublist temp)

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
            (return-from isIdentifier nil)
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
(defun getKeyword (var)
   (cond
      ((string-equal var "defvar") (return-from getKeyword "DEFV"))
      ((string-equal var "deffun") (return-from getKeyword "DEFF"))
      ((string-equal var "nil") (return-from getKeyword "KW_NIL"))
      ((string-equal var "while") (return-from getKeyword "KW_WHILE"))
      ((string-equal var "if") (return-from getKeyword "KW_IF"))
      ((string-equal var "exit") (return-from getKeyword "KW_EXIT"))
      ((string-equal var "load") (return-from getKeyword "KW_LOAD"))
      ((string-equal var "disp") (return-from getKeyword "KW_DISP"))
      ((string-equal var "true") (return-from getKeyword "KW_TRUE"))
      ((string-equal var "false") (return-from getKeyword "KW_FALSE"))
      ((string-equal var "list") (return-from getKeyword "KW_LIST"))
   )
)
; Prints token for operators
(defun getOperator (var)
   (cond
      ((string-equal var "and") (return-from getOperator "OP_AND"))
      ((string-equal var "or") (return-from getOperator "OP_OR"))
      ((string-equal var "not") (return-from getOperator "OP_NOT"))
      ((string-equal var "eq") (return-from getOperator "OP_EQ"))
      ((string-equal var "gt") (return-from getOperator "OP_GT"))
      ((string-equal var "set") (return-from getOperator "OP_SET"))
      ((string-equal var "+") (return-from getOperator "OP_PLUS"))
      ((string-equal var "-") (return-from getOperator "OP_MINUS"))
      ((string-equal var "/") (return-from getOperator "OP_DIV"))
      ((string-equal var "*") (return-from getOperator "OP_MULT"))
      ((string-equal var "(") (return-from getOperator "OP"))
      ((string-equal var ")") (return-from getOperator "CP"))
      ((string-equal var ",") (return-from getOperator "OP_COMMA")) 
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