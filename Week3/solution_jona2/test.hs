module Main (main) where

import APL.Parser (parseAPL)
import APL.AST (Exp(..))
import System.IO (hPutStrLn, stdout)
import Control.Monad (forM_)

main :: IO ()
main = do
  let examples =
        [ "x y z",
          "x(y z)",
          "x if x then y else z"
        ]
  forM_ examples $ \input -> do
    putStrLn $ "> parseAPL \"\" \"" ++ input ++ "\""
    case parseAPL "" input of
      Left err -> putStrLn $ "Left " ++ show err
      Right expr -> putStrLn $ "Right " ++ show expr
    putStrLn ""
