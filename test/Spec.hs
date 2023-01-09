{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Test.Hspec

import qualified Data.Text as T 
import qualified Document.Change as Change
import qualified Document.App as App
import qualified Document.Db as Db

import Document.Change
import Document.Change.Data

main :: IO ()
main = do
  dbPool <- Db.initialisePool
    "host=localhost port=5432 user=usr dbname=editty password=mysecretpassword"
  --- aliases
  let testEnv = Env { pool = dbPool }
  let cleanUp = App.runApp Db.cleanUpDocuments testEnv
  let changeHandle op = App.runApp (handle op) testEnv

  hspec do
    describe "Document Change handling" $
      after_ cleanUp $
        it "is Bob's operation will be transformed to 11 pos with 3 revision" do
        let aliceOp1 = Insert "Hello" 0 "Alice" 0
        let aliceOp2 = Insert " World" 5 "Alice" 2
        _ <- App.runApp Db.createDocument testEnv
        _ <- changeHandle aliceOp1
        _ <- changeHandle aliceOp2
        let bobOp1 = Insert "!" 5 "Bob" 2

        res <- changeHandle bobOp1

        res `shouldBe` Result
            (Acknowledge 3 "Bob")
            (ConsumeBroadcast (Insert "!" 11 "Bob" 3))
