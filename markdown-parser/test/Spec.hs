import Test.Hspec
--import Test.QuickCheck
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "generateHTMLFromMarkdown" $ do
    it "base layout with blank" $ do
      generateHtmlFromMarkdown " " `shouldBe` "<html><head></head><body><p> </p></body></html>"

    it "linebreak" $ do
      generateHtmlBodyFromMarkdown "  \n" `shouldBe` "<br/>"

    it "head1" $ do
      generateHtmlBodyFromMarkdown "# foo" `shouldBe` "<h1>foo</h1>"

    it "head2" $ do
      generateHtmlBodyFromMarkdown "## foo" `shouldBe` "<h2>foo</h2>"

    it "head3" $ do
      generateHtmlBodyFromMarkdown "### foo" `shouldBe` "<h3>foo</h3>"

    it "head and list" $ do
      generateHtmlBodyFromMarkdown "### foo\n* foo" `shouldBe` "<h3>foo</h3><ul><li>foo</li></ul>"

    it "head and list with blankline" $ do
      generateHtmlBodyFromMarkdown "### foo\n\n* foo" `shouldBe` "<h3>foo</h3><ul><li>foo</li></ul>"

    it "head and list with space" $ do
      generateHtmlBodyFromMarkdown "### foo\n  \n* foo" `shouldBe` "<h3>foo</h3><br/><ul><li>foo</li></ul>"

    it "list with linebreak" $ do
      generateHtmlBodyFromMarkdown "* foo\n* bar\n* baz\n" `shouldBe` "<ul><li>foo</li><li>bar</li><li>baz</li></ul>"

    it "list without linebreak" $ do
      generateHtmlBodyFromMarkdown "* foo\n* bar\n* baz" `shouldBe` "<ul><li>foo</li><li>bar</li><li>baz</li></ul>"

    it "string" $ do
      generateHtmlBodyFromMarkdown "foo bar baz" `shouldBe` "<p>foo bar baz</p>"

    it "string with softbreak" $ do
      generateHtmlBodyFromMarkdown "foo\nbar\nbaz" `shouldBe` "<p>foo bar baz</p>"

    it "string with linebreak" $ do
      generateHtmlBodyFromMarkdown "foo  \nbar\nbaz" `shouldBe` "<p>foo<br/>bar baz</p>"

    it "code" $ do
      generateHtmlBodyFromMarkdown "```\nhoge\nfuga\n```" `shouldBe` "<code>hoge\nfuga</code>"

    it "simple case1" $ do
      generateHtmlBodyFromMarkdown "## This is title\n\n### This is subtitle\n\nThis is sentence\n\n* foo\n* bar\n* baz\n\nsentence2" `shouldBe` do
        "<h2>This is title</h2><h3>This is subtitle</h3><p>This is sentence</p><ul><li>foo</li><li>bar</li><li>baz</li></ul><p>sentence2</p>"

    it "simple case2" $ do
      generateHtmlBodyFromMarkdown "sentence  \nfoo\n\n  \nbar" `shouldBe` do "<p>sentence<br/>foo</p><br/><p>bar</p>"

    it "strong1" $ do
      generateHtmlBodyFromMarkdown "*aaa*bbb" `shouldBe` do "<p><strong>aaa</strong>bbb</p>"

    it "strong2" $ do
      generateHtmlBodyFromMarkdown "*aaa *bbb" `shouldBe` do "<p><strong>aaa </strong>bbb</p>"
