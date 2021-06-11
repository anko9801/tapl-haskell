data Term =
    TremVar Int Int
  | TermAbs String Term
  | TermApp Term Term
  deriving (Eq)

type Binding = ()
type Context = [(String, Binding)]

isValue :: Context -> Term -> Bool
isValue _ t = case t of
  TermAbs _ _ -> True
  _ -> False

eval :: Context -> Term -> Maybe Term
eval ctx t =
  if isValue ctx t
    then Just t
    else case eval1 ctx t of
      Just t' -> eval ctx t'
      Nothing -> Nothing

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t = case t of
  TermApp (TermAbs x t12) t2 ->
    if isValue ctx t2
      then -- t2はこれ以上評価できないので，t12中の変数xをt2で置き換える動作を行う
        Just (substituteTop t2 t12)
      else -- t2を評価した形に変形する
      -- `TermApp (TermAbs x t12) 評価後のt2` という形になり，もう一度評価される
        fmap (TermApp (TermAbs x t12)) (eval1 ctx t2)
  TermApp t1 t2 -> fmap (`TermApp` t2) (eval1 ctx t1)
  _ -> Nothing

main :: IO ()
main = do
  print
