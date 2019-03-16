import Test.Hspec
--import Test.QuickCheck
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "generateHTMLFromMarkdown" $ do
    it "return base layout with blank" $ do
      generateHtmlFromMarkdown " " `shouldBe` "<html><head></head><body> </body></html>"

    it "return linebreak" $ do
      generateHtmlBodyFromMarkdown "  \n" `shouldBe` "<br/>"

    it "return head1" $ do
      generateHtmlBodyFromMarkdown "# foo" `shouldBe` "<h1>foo</h1>"

    it "return head2" $ do
      generateHtmlBodyFromMarkdown "## foo" `shouldBe` "<h2>foo</h2>"

    it "return head3" $ do
      generateHtmlBodyFromMarkdown "### foo" `shouldBe` "<h3>foo</h3>"

    it "return head and list" $ do
      generateHtmlBodyFromMarkdown "### foo\n* foo" `shouldBe` "<h3>foo</h3><ul><li>foo</li></ul>"

    it "return head and list with blankline" $ do
      generateHtmlBodyFromMarkdown "### foo\n\n* foo" `shouldBe` "<h3>foo</h3><ul><li>foo</li></ul>"

    it "return head and list with space" $ do
      generateHtmlBodyFromMarkdown "### foo\n  \n* foo" `shouldBe` "<h3>foo</h3><br/><ul><li>foo</li></ul>"

    it "return list with linebreak" $ do
      generateHtmlBodyFromMarkdown "* foo\n* bar\n* baz\n" `shouldBe` "<ul><li>foo</li><li>bar</li><li>baz</li></ul>"

    it "return list without linebreak" $ do
      generateHtmlBodyFromMarkdown "* foo\n* bar\n* baz" `shouldBe` "<ul><li>foo</li><li>bar</li><li>baz</li></ul>"

    it "return string" $ do
      generateHtmlBodyFromMarkdown "foo bar baz" `shouldBe` "<p>foo bar baz</p>"

    it "return string with softbreak" $ do
      generateHtmlBodyFromMarkdown "foo\nbar\nbaz" `shouldBe` "<p>foo bar baz</p>"

    it "return string with linebreak" $ do
      generateHtmlBodyFromMarkdown "foo  \nbar\nbaz" `shouldBe` "<p>foo<br/>bar baz</p>"
