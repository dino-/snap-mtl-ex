{-# LANGUAGE OverloadedStrings #-}

module Main
  where

import Control.Concurrent.STM.TVar ( TVar, newTVar, readTVar, writeTVar )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.STM ( atomically )
import Control.Monad.Trans.Reader ( ReaderT, asks, runReaderT )
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import Snap.Core
import Snap.Http.Server


-- This will store the server state, all the unique words that were added.
newtype Words = Words (S.Set B.ByteString)

-- This data structure wraps the state value (above) in a TVar so we can share
-- it across request handlers
data State = State { wordSet :: TVar Words }

-- The state is made available through the Reader monad. This type will be used
-- to combine effects with the stock Snap monad
type AppM a = ReaderT State Snap a

runApp :: Config Snap a -> State -> AppM () -> IO ()
runApp conf env ev = httpServe conf (runReaderT ev env)


main :: IO ()
main = do
  initialState <- State <$> (atomically . newTVar $ Words S.empty)
  runApp (setPort 8000 defaultConfig) initialState site


site :: AppM ()
site = route
  [ ("addword/:word", method GET addWord)
  , ("getwords", method GET getWords)
  ]

-- In these AppM-typed handlers, we only need to lift IO actions, not Snap
-- actions because the Snap monad is instanced into the mtl type classes.

addWord :: AppM ()
addWord = do
  mbWord <- getParam "word"
  maybe badRequest handleRequest mbWord

  where
    handleRequest word' = do
      tvWords <- asks wordSet
      liftIO $ atomically $ do
        (Words ws) <- readTVar tvWords
        writeTVar tvWords . Words $ S.insert word' ws
      writeBS "Ok, thanks for the word!"

    badRequest = do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      writeBS "Something unexpected happened, we shouldn't be here"
      finishWith =<< getResponse


getWords :: AppM ()
getWords = do
  writeBS "The unique words we've seen: "
  tvWords <- asks wordSet
  displayWords <- liftIO $ atomically $ do
    (Words ws) <- readTVar tvWords
    return . B.intercalate " " . S.toList $ ws
  writeBS displayWords
