module Parser.SimpleSpec (spec) where

import Parser.Duration
import Relude
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = do
    it "parses 6 minutes" $
        parseDuration "6 minutes" `shouldParse` 360