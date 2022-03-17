module Parser.SimpleSpec (spec) where

import Parser.Duration
import Relude
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
    it "parses 6 minutes" $
        parseDuration "6 minutes" `shouldParse` 360
    it "parses 2 hours" $
        parseDuration "2 hours" `shouldParse` 7200
    it "parses 1 day" $
        parseDuration "1 day" `shouldParse` 86400

    it "returns an error when parsing garbage" $
        parseDuration `shouldFailOn` "garbage"