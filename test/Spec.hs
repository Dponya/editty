{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Test.Hspec

import qualified Document.Change as Change
import qualified Document.App as App
import qualified Document.Db as Db

main :: IO ()
main = do
  dbPool <- Db.initialisePool "host=localhost port=5432 user=usr dbname=editty password=mysecretpassword"
  let testEnv = (Change.Env { Change.pool = dbPool })
  let cleanUp = App.runApp Db.cleanUpDocuments testEnv

  hspec do
    describe "Document change handling" $
      after_ cleanUp $
        it "is document finally will be resolved to goat" do
        let op1 = App.Insert "t" 2
        let op2 = App.Insert "a" 2
        _ <- App.runApp Db.createDocument testEnv
        _ <- App.runApp (Db.pushPairOp op1) testEnv 

        res <- App.runApp (Change.handle op2) testEnv

        res `shouldBe` Change.Syncd "t"
    context "When queue doesn't have an any operations" $
      after_ cleanUp $
        it "is handling just says that we should wait" do
        let op1 = App.Insert "a" 0
        _ <- App.runApp Db.createDocument testEnv

        res <- App.runApp (Change.handle op1) testEnv

        res `shouldBe` Change.ShouldWait