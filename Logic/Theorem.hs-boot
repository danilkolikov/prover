module Logic.Theorem where

import Logic.Base(Theorem, Header, Expression, Dictionary)

(|>) :: Theorem -> String -> Theorem
(|>>) :: Theorem -> Expression -> Theorem

(><) :: Theorem -> Theorem -> Theorem
(>?<) :: Theorem -> (String, Theorem) -> Theorem
(>??<) :: Theorem -> (Header, Theorem) -> Theorem

match :: Header -> Theorem -> Maybe Dictionary
replace :: Dictionary -> Theorem -> Theorem
replaceAll :: Dictionary -> Theorem -> Theorem
shortify :: Theorem -> Theorem

deduceLast :: Theorem -> Theorem
prove :: Expression -> Either [(String, Expression)] Theorem
