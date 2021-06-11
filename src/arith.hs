data Term
  = TermTrue
  | TermFalse
  | TermZero
  | TermSucc Term
  | TermPred Term
  | TermIsZero Term
  | TermIf Term Term Term
  deriving (Show, Eq)

data EvalErr
  = Error
  deriving (Show, Eq)

evalSub :: Term -> Either Term EvalErr
evalSub t = case t of
  TermIf TermTrue t2 t3 -> Left t2
  TermIf TermFalse t2 t3 -> Left t3
  TermIf t1 t2 t3 -> case evalSub t1 of
    Left t1' -> Left $ TermIf t1' t2 t3
    e -> e
  TermSucc t1 -> case evalSub t1 of
    Left t1' -> Left $ TermSucc t1'
    e -> e
  TermPred TermZero -> Left TermZero
  TermPred (TermSucc nv1) -> Left nv1
  TermPred t1 -> case evalSub t1 of
    Left t1' -> Left $ TermPred t1'
    e -> e
  TermIsZero TermZero -> Left TermTrue
  TermIsZero (TermSucc nv1) -> Left TermFalse
  TermIsZero t1 -> case evalSub t1 of
    Left t1' -> Left $ TermIsZero t1'
    e -> e
  e -> Right Error

eval :: Term -> Either Term EvalErr
eval t = case evalSub t of
  Left t' -> eval t'
  Right e -> Left t

main :: IO ()
main = do
  let a = TermIf TermFalse TermFalse TermTrue
  let b = TermIf (TermIsZero (TermPred (TermSucc TermZero))) TermTrue TermFalse
  print (eval a)
  print (eval b)
