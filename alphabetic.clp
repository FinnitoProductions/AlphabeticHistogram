/*
* Finn Frankis
* January 30, 2019
*
* Determines how many of each letters are contained in a given string (not case-sensitive).
* Uses this data to generate and print a histogram.
*/

(batch util/utilities.clp)
(batch util/asciiToChar.clp)

(bind ?STRING_TO_PROCESS (join$ (asciiList$)))

(bind ?TOTAL_ASCII_CHARACTERS 256)
(bind ?INITIAL_LIST_VALUE 0)

/*
* Determines and prints how many of each letter appears in a given string ?n - not case-sensitive.
*/
(deffunction alphaHistogram (?n)
   (bind ?sliced (slice$ (lowcase ?n)))
   (bind ?histogram (create$))

   (for (bind ?i 1) (<= ?i ?TOTAL_ASCII_CHARACTERS) (++ ?i)
      (bind ?histogram (insert$ ?histogram ?i ?INITIAL_LIST_VALUE))
   )

   (foreach ?char ?sliced
      (bind ?insertionIndex (getListIndex ?char))

      (if (> ?insertionIndex (- ?TOTAL_ASCII_CHARACTERS 1)) then
         (printline (str-cat ?char " is not a valid ASCII character. Will be ignored."))
       else 
         (bind ?currentVal (nth$ ?insertionIndex ?histogram))
         (bind ?histogram (replace$ ?histogram ?insertionIndex ?insertionIndex (++ ?currentVal)))
      )
   ) ; foreach ?char ?sliced

   (for (bind ?i (getListIndex "a")) (<= ?i (getListIndex "z")) (++ ?i)
      (printline (str-cat (getAsciiCharacter ?i) ": " (nth$ ?i ?histogram)))
   ) 

   (return)
) ; alphaHistogram (?n)

/*
* Returns the representation of a given string as a list of characters, in the same order as provided. 
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
* Returns the representation of a given list of characters as a string by concatenating each character.
* Input must be a list of strings for a logical output.
*/
(deffunction join$ (?list)
   (bind ?returnStr "")

   (foreach ?str ?list
      (bind ?returnStr (str-cat ?returnStr ?str))
   )

   (return ?returnStr)
) ; join$ (?list)

/*
* Breaks a given non-whitespace value into its constitutent characters, ordered in a list.
*/
(deffunction sliceToken$ (?str)
   (bind ?returnVal (create$))

   (for (bind ?i 1) (<= ?i (str-length ?str)) (++ ?i)
      (bind ?returnVal (insert$ ?returnVal ?i (sub-string ?i ?i ?str)))
   )

   (return ?returnVal)
) ; sliceToken$ (?str)

/*
* Returns the list index of a given ASCII character in the histogram.
*/
(deffunction getListIndex (?ascii)
   (return (+ (asc ?ascii) 1))
) ; getListIndex (?ascii)

/**
* Returns the ASCII character which corresponds to a given list index in the histogram.
*/ 
(deffunction getAsciiCharacter (?listIndex)
   (return (toChar (- ?listIndex 1)))
) ; getAsciiCharacter (?listIndex)

(alphaHistogram ?STRING_TO_PROCESS)