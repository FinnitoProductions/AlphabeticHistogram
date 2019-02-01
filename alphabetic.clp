/*
* Finn Frankis
* January 30, 2019
*
* Determines how many of each letters are contained in a given string (not case-sensitive).
* Uses this data to generate and print a histogram.
*
* alphaHistogram - determines how many of each letter appears in a string and produces a histogram from this data
* slice$ - converts a given string into a list of each of its respective characters
*/

(batch util/utilities.clp)

(bind ?STRING_TO_PROCESS "A
B
c
D
E
fz1")

(bind ?TOTAL_ASCII_CHARACTERS 128)
(bind ?INITIAL_LIST_VALUE 0)
(bind ?LETTERS_TO_INDEX "abcdefghijklmnopqrstuvwxyz")

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
        (bind ?insertionIndex (+ (asc ?char) 1))
        (bind ?currentVal (nth$ ?insertionIndex ?histogram))

        (bind ?histogram (replace$ ?histogram ?insertionIndex ?insertionIndex (++ ?currentVal)))
    )

    (foreach ?char (slice$ ?LETTERS_TO_INDEX)
        (printline (str-cat ?char ": " (nth$ (+ (asc ?char) 1) ?histogram)))
    ) 

    (return)
) ; alphaHistogram (?n)

/*
* Returns the representation of a given string as a list of characters, in the same order as provided. 
*/
(deffunction slice$ (?str)
    (bind ?returnVal (create$))

    (for (bind ?i 1) (<= ?i (str-length ?str)) (++ ?i)
        (bind ?returnVal (insert$ ?returnVal (+ (length$ ?returnVal) 1) (sub-string ?i ?i ?str)))
    )

    (return ?returnVal)
) ; slice$ (?str)

(alphaHistogram ?STRING_TO_PROCESS)
