import Data.Char

-- Every element in the left subtree is <= node value.
data Tree a = Leaf | Node {
                          value :: a,
                          left :: Tree a,
                          right :: Tree a
                          } deriving (Eq, Show, Read, Ord)

decode :: String -> [String]
decode a = (decode' (take 1 a) (drop 1 a))
    ++ if len >= 2 then (decode' (take 2 a) (drop 2 a))
        ++ if len >= 3 then (decode' (take 3 a) (drop 3 a))
            ++ if len >= 4 then (decode' (take 4 a) (drop 4 a))
            else []
        else []
    else []
    where len = length a

decode' :: String -> String -> [String]
decode' c [] = if decoded /= '?' then [[decoded]] else []
    where decoded = decode'' c
decode' c r = if decoded /= '?' then map (decoded:) (decode r) else []
    where decoded = decode'' c

decode'' :: String -> Char
decode'' a = decode''' a morseTree

decode''' :: String -> Tree Char -> Char
decode''' ('.':rest) node = decode''' rest (left node)
decode''' ('-':rest) node = decode''' rest (right node)
decode''' [] node = value node
decode''' _ _ = '?' --Invalid morse code letter

encode :: String -> String
encode [] = ""
encode (curr:remainder) = (encode' (toUpper curr)) ++ (encode remainder)

encode' :: Char -> String
encode' 'A' = ".-"
encode' 'B' = "-..."
encode' 'C' = "-.-."
encode' 'D' = "-.."
encode' 'E' = "."
encode' 'F' = "..-."
encode' 'G' = "--."
encode' 'H' = "...."
encode' 'I' = ".."
encode' 'J' = ".---"
encode' 'K' = "-.-"
encode' 'L' = ".-.."
encode' 'M' = "--"
encode' 'N' = "-."
encode' 'O' = "---"
encode' 'P' = ".--."
encode' 'Q' = "--.-"
encode' 'R' = ".-."
encode' 'S' = "..."
encode' 'T' = "-"
encode' 'U' = "..-"
encode' 'V' = "...-"
encode' 'W' = ".--"
encode' 'X' = "-..-"
encode' 'Y' = "-.--"
encode' 'Z' = "--.."
encode' a = "" -- invalid char


generateMorseTree :: Tree Char
generateMorseTree = Node '?'
                (Node 'E'
                    (Node 'I'
                        (Node 'S'
                            (Node 'H' Leaf Leaf)
                            (Node 'V' Leaf Leaf)
                        )
                        (Node 'U'
                            (Node 'F' Leaf Leaf)
                            (Leaf)
                        )
                    )
                    (Node 'A'
                        (Node 'R'
                            (Node 'L' Leaf Leaf)
                            (Leaf)
                        )
                        (Node 'W'
                            (Node 'P' Leaf Leaf)
                            (Node 'J' Leaf Leaf)
                        )
                    )
                )
                (Node 'T'
                    (Node 'N'
                        (Node 'D'
                            (Node 'B' Leaf Leaf)
                            (Node 'X' Leaf Leaf)
                        )
                        (Node 'K'
                            (Node 'C' Leaf Leaf)
                            (Node 'Y' Leaf Leaf)
                        )
                    )
                    (Node 'M'
                        (Node 'G'
                            (Node 'Z' Leaf Leaf)
                            (Node 'Q' Leaf Leaf)
                        )
                        (Node 'O' Leaf Leaf)
                    )
                )
morseTree = generateMorseTree

main = interact encode