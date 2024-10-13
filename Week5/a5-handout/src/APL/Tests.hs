module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), subExp, VName)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import Data.Char()
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  --, oneof
  , sized
  , frequency
  , elements
  , suchThat
  )

instance Arbitrary Exp where
  arbitrary = sized (\n -> genExp n [])

  shrink (Add e1 e2) =
    e1 : e2 :
    [Add e1' e2 | e1' <- shrink e1] ++
    [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 :
    [Sub e1' e2 | e1' <- shrink e1] ++
    [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 :
    [Mul e1' e2 | e1' <- shrink e1] ++
    [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 :
    [Div e1' e2 | e1' <- shrink e1] ++
    [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 :
    [Pow e1' e2 | e1' <- shrink e1] ++
    [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 :
    [Eql e1' e2 | e1' <- shrink e1] ++
    [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    cond : e1 : e2 :
    [If cond' e1 e2 | cond' <- shrink cond] ++
    [If cond e1' e2 | e1' <- shrink e1] ++
    [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : e2 :
    [Let x e1' e2 | e1' <- shrink e1] ++
    [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    e : [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 :
    [Apply e1' e2 | e1' <- shrink e1] ++
    [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 :
    [TryCatch e1' e2 | e1' <- shrink e1] ++
    [TryCatch e1 e2' | e2' <- shrink e2]
  shrink (Var _) = []
  shrink (CstInt _) = []
  shrink (CstBool _) = []

-- genVarName :: Gen VName
-- genVarName = frequency
--   [ (7, genVarNameWithLength 2 4) -- at least 50% chance
--   , (3, arbitrary)                
--   ]

genVarNameWithLength :: Int -> Int -> Gen VName
genVarNameWithLength minLen maxLen = do
  len <- elements [minLen..maxLen]
  name <- sequence $ replicate len genVarChar
  return name

genVarChar :: Gen Char
genVarChar = elements $ ['a'..'z'] ++ ['A'..'Z']

genExp :: Int -> [VName] -> Gen Exp
genExp size vars
  | size <= 0 = frequency
      [ (30, CstInt <$> arbitrary)
      , (1, Var <$> genVarNameWithLength 2 4) -- Increase non-trivial variables
      ]
  | otherwise = do
      let varGen =
            [ (200, Var <$> elements vars) | not (null vars) ] ++
            [ (1, Var <$> genVarNameWithLength 2 4) ] -- Potential variable errors
      frequency $ concat
        [ [(3, CstInt <$> arbitrary)
          , (2, CstBool <$> arbitrary)]
        , varGen
        , [ (4, genBinaryOp Add vars)
          , (4, genBinaryOp Sub vars)
          , (4, genBinaryOp Mul vars)
          , (3, genDiv vars)    -- Decreased weight
          , (3, genPow vars)    -- Decreased weight
          , (4, genEql vars)
          , (4, genIf vars)
          , (6, genLet vars)    -- Increased weight
          , (6, genLambda vars) -- Increased weight
          , (3, genApply vars)
          , (3, genTryCatch vars)
          ]
        ]
  where
    genBinaryOp op varList = do
      e1 <- genExp (size `div` 2) varList
      e2 <- genExp (size `div` 2) varList
      return $ op e1 e2

    genDiv varList = do
      e1 <- genExp (size `div` 2) varList
      e2 <- frequency
        [ (1, return $ CstInt 0)              -- Increase chance of DivisionByZero
        , (4, genExp (size `div` 2) varList)
        ]
      return $ Div e1 e2

    genPow varList = do
      e1 <- genExp (size `div` 2) varList
      e2 <- frequency
        [ (1, CstInt <$> arbitrary `suchThat` (< 0)) -- Increase chance of NegativeExponent
        , (4, genExp (size `div` 2) varList)
        ]
      return $ Pow e1 e2

    genEql varList = do
      e1 <- genExp (size `div` 2) varList
      e2 <- frequency
        [ (1, CstBool <$> arbitrary)          -- Increase type mismatch chance
        , (4, genExp (size `div` 2) varList)
        ]
      return $ Eql e1 e2

    genIf varList = do
      cond <- frequency
        [ (1, genExp (size `div` 3) varList)     -- Increase NonBoolean chance
        , (4, CstBool <$> arbitrary)
        ]
      e1 <- genExp (size `div` 3) varList
      e2 <- genExp (size `div` 3) varList
      return $ If cond e1 e2

    genLet varList = do
      var <- genVarNameWithLength 2 4
      e1 <- genExp (size `div` 2) varList
      e2 <- genExp (size `div` 2) (var : varList)
      return $ Let var e1 e2

    genLambda varList = do
      var <- genVarNameWithLength 2 4
      body <- genExp (size - 1) (var : varList)
      return $ Lambda var body

    genApply varList = do
      e1 <- genExp (size `div` 2) varList
      e2 <- genExp (size `div` 2) varList
      return $ Apply e1 e2

    genTryCatch varList = do
      e1 <- genExp (size `div` 2) varList
      e2 <- genExp (size `div` 2) varList
      return $ TryCatch e1 e2

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted _ = undefined

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors _ = undefined

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  , ("parsePrinted", property parsePrinted)
  ]
