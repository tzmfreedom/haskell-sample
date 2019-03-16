import Test.Hspec
--import Test.QuickCheck
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "generateHTMLFromMarkdown" $ do
    it "return base layout with blank" $ do
      generateHtmlFromMarkdown " " `shouldBe` "<html><head></head><body><p> </p></body></html>"

    it "return linebreak" $ do
      generateHtmlBodyFromMarkdown "  \n" `shouldBe` "<p><br/></p>"

    it "return head1" $ do
      generateHtmlBodyFromMarkdown "# foo" `shouldBe` "<h1>foo</h1>"

    it "return head2" $ do
      generateHtmlBodyFromMarkdown "## foo" `shouldBe` "<h2>foo</h2>"

    it "return head3" $ do
      generateHtmlBodyFromMarkdown "### foo" `shouldBe` "<h3>foo</h3>"

    it "return list with linebreak" $ do
      generateHtmlBodyFromMarkdown "* foo\n* bar\n* baz\n" `shouldBe` "<ul><li>foo</li><li>bar</li><li>baz</li></ul>"

    it "return list without linebreak" $ do
      generateHtmlBodyFromMarkdown "* foo\n* bar\n* baz" `shouldBe` "<ul><li>foo</li><li>bar</li><li>baz</li></ul>"
