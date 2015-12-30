module ShellParseSpec where


import GHCMod.Options.ShellParse

import Test.Hspec

spec :: Spec
spec =
  describe "parseCmdLine" $ do
    it "splits arguments" $ do
      parseCmdLine "test command line" `shouldBe` ["test", "command", "line"]
      parseCmdLine "\\test command line" `shouldBe` ["test", "command", "line"]
    it "honors quoted segments if turned on" $
      parseCmdLine "\\test command line \STXwith quoted segment\ETX"
        `shouldBe` ["test", "command", "line", "with quoted segment"]
    it "doesn't honor quoted segments if turned off" $
      parseCmdLine "test command line \STXwith quoted segment\ETX"
        `shouldBe` words "test command line \STXwith quoted segment\ETX"
    it "squashes multiple spaces" $ do
      parseCmdLine "test       command"
        `shouldBe` ["test", "command"]
      parseCmdLine "\\test       command"
        `shouldBe` ["test", "command"]
