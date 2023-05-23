import Data.List
-- import Graphics.Win32.GDI.AlphaBlend (BLENDFUNCTION(alphaFormat))

data Expr = Variable String
          | Function String Expr
          | Application Expr Expr

instance Show Expr where
    show (Variable x) = x
    show (Function x (Application e1 e2)) = ('λ':x) ++ ".(" ++ show e ++ ")"
        where e = Application e1 e2
    show (Function x e) = ('λ':x) ++ ('.':show e)
    show (Application e1 (Application u v)) = show e1 ++ " (" ++ show e2 ++ ")"
        where e2 = Application u v
    show (Application e1 e2) = show e1 ++ (' ':show e2)

free_vars :: Expr -> [String]
free_vars =
    \s -> case s of
        (Variable x) -> [x]
        (Function x e) -> delete x (free_vars e)
        (Application e1 e2) -> free_vars e1 `union` free_vars e2

-- equality instance
instance Eq Expr where
    (==) e1 e2 = equal e1 e2 []
      where
        equal :: Expr -> Expr -> [(String, String)] -> Bool
        equal (Variable x) (Variable y) env = case lookup x env of
                                                (Just xv) -> xv == y
                                                Nothing -> x == y

        equal (Function x e1) (Function y e2) env = equal e1 e2 ((x,y):env)
        equal (Application e1 e2) (Application e3 e4) env = equal e1 e3 env && equal e2 e4 env
        -- TODO 3. add equal instance for Macro
        -- before default case !!!
        equal _ _ _ = False

a = Application
f = Function
v = Variable

vx = v "x"
vy = v "y"

j = f "y" (a vx vy) -- \y.(x y)
k = f "x" (v "y") -- \x.y
l = f "y" $ v "x"

ans = f "y" (a (f "x" vy) vy)

alphabet = ['a'..'z']

transformation :: String -> [String]
transformation = map (\x -> [x])

filterOut :: [String] -> [String] -> [String]
filterOut xs ys = filter (\x -> notElem x ys) xs

replace :: [String] -> Expr -> Expr
replace freeVars e =
    case e of
        (Variable x) ->
            if x `elem` freeVars then Variable (head (filterOut (transformation alphabet) freeVars)) else e
        (Function x e1) ->
            if x `elem` freeVars then
                let
                    newVar = head (filterOut (transformation alphabet) freeVars)
                    -- newFreeVars = newVar : freeVars
                in
                    Function newVar (replace freeVars e1)
            else
                Function x (replace freeVars e1)
        (Application e1 e2) ->
            Application (replace freeVars e1) (replace freeVars e2)

o = f "x" (a vy (v "z"))
p = f "y" (a (v "x") (v "z"))

alfa = ['c'..'z']

letters = concatMap (\n -> map (take n) $ tails ['a'..'z']) [1..]
charList = concatMap (\n -> map (take n) $ tails ['a'..'z']) [1..]

cv = concatMap (\n -> sequence (take n (tails ['a' .. 'z']))) [1..]



