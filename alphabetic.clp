/*
* Finn Frankis
* January 30, 2019
*
* Determines how many of each letter is contained in a given ASCII-based string (not case-sensitive) by sorting
* all the characters within a string by their ASCII values and determining the counts of all the letters.
* Uses this data to generate and print an alphabetic histogram. 
*
* Includes a helpful method (slice$) for breaking a given string into a list of characters 
* and another (sliceToken$) for breaking a single non-whitespace word into a list of characters.
*/

(batch util/utilities.clp)
(batch util/asciiToChar.clp)

(bind ?TOTAL_ASCII_CHARACTERS 256)
(bind ?INITIAL_LIST_VALUE 0)

(bind ?STRING_TO_PROCESS "!\"#$%&'()*+,-./0123456789:<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ")

/*
* Determines and prints a histogram of how many of each letter appears in a given ASCII-based string (not case-sensitive)
* by sorting all the characters within a string by their ASCII values and summing the uppercase and lowercase 
* counts of each letter.
* 
* Will ignore all non-ASCII characters and give a brief warning for each that is included.
*/
(deffunction alphaHistogram (?n)
   (bind ?sliced (slice$ ?n))
   (bind ?histogram (create$))

   (for (bind ?i 1) (<= ?i ?TOTAL_ASCII_CHARACTERS) (++ ?i)
      (bind ?histogram (insert$ ?histogram ?i ?INITIAL_LIST_VALUE)) ; prepopulate list to store 256 elements all equal to zero
   )

   (foreach ?char ?sliced
      (bind ?insertionIndex (getListIndex ?char))

      (if (or (> ?insertionIndex ?TOTAL_ASCII_CHARACTERS) (< ?insertionIndex 0)) then
         (printline (str-cat ?char " (value " (asc ?char) ") is not a valid ASCII character. Will be ignored."))
       else 
         (bind ?currentVal (nth$ ?insertionIndex ?histogram))
         (bind ?histogram (replace$ ?histogram ?insertionIndex ?insertionIndex (++ ?currentVal))) ; increment current letter's count
      )
   ) ; foreach ?char ?sliced

   (for (bind ?i (getListIndex "a")) (<= ?i (getListIndex "z")) (++ ?i)
      (bind ?lowercaseCount (nth$ ?i ?histogram))
      (bind ?uppercaseLowercaseDifference (- (asc "A") (asc "a")))
      (bind ?uppercaseCount (nth$ (+ ?i ?uppercaseLowercaseDifference) ?histogram))

      (printline (str-cat (getAsciiCharacter ?i) ": " (+ ?lowercaseCount ?uppercaseCount)))
   )

   (return)
) ; alphaHistogram (?n)

/*
* Returns the representation of a given string as a list of characters, in the same order as provided
* by first exploding the string into its various words and then iterating over each character in every word.
* Removes all whitespace. Input must be a string for a logical output.
*/
(deffunction slice$ (?str)
   (bind ?list (explode$ ?str))

   (for (bind ?i 1) (<= ?i (length$ ?list)) (++ ?i) 
      (bind ?token (nth$ ?i ?list))
      (bind ?list (replace$ ?list ?i ?i (sliceToken$ ?token)))
   )

   (return ?list)
) ; slice$ (?str)

/*
* Breaks a given non-whitespace token into a list of its constituent characters by iterating over each character
* and adding it to a list.
* Input must be a string without whitespace for a logical output.
*/
(deffunction sliceToken$ (?str)
   (bind ?returnVal (create$))

   (for (bind ?i 1) (<= ?i (str-length ?str)) (++ ?i) ; iterate through every character in the string
      (bind ?returnVal (insert$ ?returnVal ?i (sub-string ?i ?i ?str)))
   )

   (return ?returnVal)
) ; sliceToken$ (?str)

/*
* Returns the list index of a given ASCII character in the histogram to be generated.
* List indices start at 1 while ASCII indices starts at 0: the ASCII value of the given character
* must be incremented to get list index.
*/
(deffunction getListIndex (?ascii)
   (return (+ (asc ?ascii) 1))  
) ; getListIndex (?ascii)

/**
* Returns the ASCII character which corresponds to a given list index in the 1-based histogram.
* List indices start at 1 while ASCII indices starts at 0: the list index value must be decremented 
* to become zero-based and then converted back to a character to get list index.
*/ 
(deffunction getAsciiCharacter (?listIndex)
   (return (toChar (- ?listIndex 1)))
) ; getAsciiCharacter (?listIndex)

(alphaHistogram ?STRING_TO_PROCESS)