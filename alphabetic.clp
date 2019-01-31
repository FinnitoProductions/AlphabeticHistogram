/*
* Finn Frankis
* January 30, 2019
*
* Breaks a given string into its constitutent letters.
*/

(batch util/utilities.clp)

/*
* Determines and prints how many of each letter appears in a given string ?n - not case-sensitive.
*/
(deffunction alphaHistogram (?n)
    (bind ?sliced (slice$ (lowcase ?n)))
    (bind ?histogram (create$))

    (for (bind ?i 1) (< ?i 27) (++ ?i) 
        (bind ?histogram (insert$ ?histogram ?i 0))
    )

    (foreach ?char ?sliced
        (bind ?insertionIndex (+ (- (asc ?char) (asc "a")) 1))
        (bind ?currentVal (nth$ ?insertionIndex ?histogram))

        (bind ?histogram (replace$ ?histogram ?insertionIndex ?insertionIndex (++ ?currentVal)))
    )

    (foreach ?val ?histogram (printline ?val))
)

/*
* Returns the representation of a given string as a list of characters, in the same order as provided. 
*/
(deffunction slice$ (?str)
    (bind ?returnVal (create$))

    (for (bind ?i 1) (<= ?i (str-length ?str)) (++ ?i)
        (bind ?returnVal (insert$ ?returnVal (+ (length$ ?returnVal) 1) (sub-string ?i ?i ?str)))
    )

    (return ?returnVal)
)

(alphaHistogram "bcdefghijklmnopqrstuvwxy")
