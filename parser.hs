
data Parser s t = P ([s] -> (t, [s]))

--Parser for the letter a.
pLetterA :: Parser Char Char
pLetterA = P (\inp -> case inp of
                 (s:ss)
                     | s = 'a' -> [('a', s)]
                     otherwise -> []
             )

pSymbol :: Eq s => Parser s s
pSymbol = P (\inp -> case inp of
                (s:ss)
                    | x == a    -> [(s,ss)]
                    | otherwise -> []
            )

pIdent = pSym Identifier