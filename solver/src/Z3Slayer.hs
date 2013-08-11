{-# LANGUAGE LambdaCase #-}
module Z3Slayer where

import System.Process
import Control.Applicative
import Control.Monad

z3Slayer = do
    procs <- map words <$> lines <$> readProcess "/bin/ps" [] ""
    forM_ (filter (elem "<defunct>") procs) $ \case
      (pid:_) -> do
        putStrLn $ "kill -9 " ++ pid
        system $ "kill -9 " ++ pid
        return ()
      _ -> return ()
