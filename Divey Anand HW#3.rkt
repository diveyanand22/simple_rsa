#lang slideshow

;;Question 1 - Write a function that converts an input string to a list of digits, using the encoding described above. Write the inverse function, which
;;converts a list of digits to a string. Or, not exactly the inverse, because it can’t undo the fact that all letters have become uppercase and all
;;miscellaneous characters have been converted to space.

(define (normalize str)
  ;;creates a list of characters from the given string
  (define charList (string->list str))
  
  ;;converts each character in the list to uppercase, and makes a string from the resulting list
  (list->string (map char-upcase charList)))

;;helper function that operates on each element of the list to encrypt it
(define (encodeHelper c)
  (cond
    [(char-alphabetic? c) (- (char->integer c) 54)]
    [(char-numeric? c) (char->integer c)]
    [else 10]))

;;encodes a user-input string to a list of digits
(define (encodeStr str)
  (define charList (string->list (normalize str)))
  (map encodeHelper charList))

;helper function that operates on each element of the list to decrypt it
(define (decodeHelper c)
  (cond
    [(or (= c 0) (= c 10)) (integer->char 32)]
    [(and (>= c 11) (<= c 36)) (integer->char (+ c 54))]
    [(and (>= c 48) (<= c 57)) (integer->char c)]
    [else (integer->char 35)]))

;converts the list of integers to a string, where each integer is replaced by a character as specified
(define (decodeLst lst)
  (define decodedLst (map decodeHelper lst))
  (list->string decodedLst))


;;Question 2 - Write a simple recursive function that takes three inputs (b, n, k) and computes b^n (mod k). You’ll want to have separate cases, depending
;;on whether n is odd or even. To keep intermediate values small, reduce the output mod k at each main step (e.g. each recursive class).

; calculates b^n (mod k) recursively
(define (moduloCalc b n k)

  ;base cases
  (if (= b 0) 0

      ; recursive case
      (if (= n 0) (remainder 1 k)
         (cond
           ;if n is even calls the function for (n/2)th power of b
            [(even? n) (let ([x (moduloCalc b (quotient n 2) k)]) (remainder (* x x) k))]

           ;if n is even calls the function for (n/2) + 1 th power of b
            [else (let ([x (moduloCalc b (quotient n 2) k)]) (remainder (* x x b) k))]))))


;;Question 3 - Write a Racket function that converts a string of characters to its RSA encoding (i.e. a list of integers). This function should call some of
;;your previously written functions. Also, it should print out the key intermediate step: the list of character codes before they are run through RSA.

;;encodes a string to a list of numbers using the formula c = (m^e)(modN) where m is the character code of an alphabet in the string and c is its encrypted form.
;;e is the public key and N is a 3-digit number
(define (rsaEncode str N e)
  (let ([charCodesList (encodeStr str)])

    ;;prints the intermediate list of character codes which will be encrypted
    (writeln charCodesList)
    (map (lambda (charCode)
           (moduloCalc charCode e N)) charCodesList)))

;;decodes a list of numbers to a string using the formula m = (c^d)(modN) where c is the encryped form of a letter in the string and m is the original.
;;d is the private key and N is a 3-digit number
(define (rsaDecode lst N d)
  (define charCodesList (map (lambda (intCode)
           (moduloCalc intCode d N)) lst))

  ;;prints the intermediate list of decoded character codes
  (writeln charCodesList)
  
  (let ([charList (decodeLst charCodesList)])

    ;;prints the decoded string
    (writeln charList)))


;;Question 4 - Write a function that converts a list of 2-digit codes to a list of 3-digit codes. Write a second function that reverses this process, i.e. converts
;;sequence of 3-digit codes to 2-digit codes. Finally build a second version of your RSA encoding and decoding functions for use with a 4-digit value of N.

;;splits a string into groups of three recursively
(define (twoThreeSplitter str)

  ;;defining an empty list which will be filled with groups of digits in string format
  (define stringList (list ))

  ;;cases which are handled in accordance to the string length's divisibility with 3.
    (cond
    [(= (string-length str) 1) (list (string-append str "00"))]
    [(= (string-length str) 2) (list (string-append str "0"))]
    [(= (string-length str) 3) (list str)]
    [else (append stringList (list(substring str 0 3)) (twoThreeSplitter (substring str 3)))]))

;; converts a list of numbers stored in groups of 2 to a list of numbers stored in groups of 3
(define (twoToThree lst)

  ;;defines a string consisting of all the numbers in the list joined together
  (define numString (string-join (map number->string lst) ""))  
  (define threeList (twoThreeSplitter numString))

  ;;converts the list of strings to a list of numbers
  (map string->number threeList))


;;splits a string into groups of two recursively
(define (threeTwoSplitter str)

  ;;defining an empty list which will be filled with groups of digits in string format
  (define stringList (list ))

  ;;cases which are handled in accordance to the string length's divisibility with 2.
    (cond
    [(= (string-length str) 1) (list (string-append str "0"))]
    [(= (string-length str) 2) (list str)]
    [else (append stringList (list(substring str 0 2)) (threeTwoSplitter (substring str 2)))]))

;; converts a list of numbers stored in groups of 3 to a list of numbers stored in groups of 2
(define (threeToTwo lst)

  ;;defines a string consisting of all the numbers in the list joined together
  (define numString (string-join (map number->string lst) ""))  
  (define twoList (threeTwoSplitter numString))

  ;;converts the list of strings to a list of numbers
  (map string->number twoList))

;;encodes a string to a list of numbers using the formula c = (m^e)(modN) where m is the character code of an alphabet in the string and c is its encrypted form.
;;e is the public key and N is a 4-digit number
(define (fourRSAEncode str N e)
  (let ([charCodesList (encodeStr str)])

    ;;splits the list of character codes to be encrypted into groups of 3
    (define threeCodesList (twoToThree charCodesList))

    ;;prints the intermediate list of character codes
    (writeln threeCodesList)
    (map (lambda (charCode)
           (moduloCalc charCode e N)) threeCodesList)))

;;decodes a list of numbers to a string using the formula m = (c^d)(modN) where c is the encryped form of a letter in the string and m is the original.
;;d is the private key and N is a 4-digit number
(define (fourRSADecode lst N d)
  (define threeCodesList (map (lambda (intCode)
           (moduloCalc intCode d N)) lst))
  
  ;;prints the intermediate list of decoded character codes
  (writeln threeCodesList)

  ;;splits the list of decrypted character codes into groups of 2 so tet can be decoded to a string
  (define twoCodesList (threeToTwo threeCodesList))
  (let ([charList (decodeLst twoCodesList)])

    ;;prints the decoded list
    (writeln charList)))