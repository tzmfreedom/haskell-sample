import Test.Hspec
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "generateHTMLFromMarkdown" $ do
    it "select one field" $ do
      parseSOQL "SELECT Id FROM Account" `shouldBe` SOQL{fields=[(Field "Id")], object=SObject "Account", whereClause=Nothing}

    it "select mulitple field" $ do
      parseSOQL "SELECT Id, Name FROM Account" `shouldBe` SOQL{fields=[(Field "Id"), (Field "Name")], object=SObject "Account", whereClause=Nothing}

    it "select mulitple field include subquery" $ do
      parseSOQL "SELECT Id, Name, (SELECT Id, LastName FROM Contacts) FROM Account" `shouldBe` do
        SOQL{fields=[(Field "Id"), (Field "Name"), (SubQuery (SOQL{fields=[Field "Id", Field "LastName"], object=SObject "Contacts", whereClause=Nothing}))], object=SObject "Account", whereClause=Nothing}

    describe "single where condition" $ do
      it "equal string" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name = 'foo'" `shouldBe` do
          SOQL{fields=[Field "Id"], object=SObject "Account", whereClause=Just (SingleCondition (Field "Name") "=" (SOQLString "foo"))}

      it "less than number" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name < 1" `shouldBe` do
          SOQL{fields=[Field "Id"], object=SObject "Account", whereClause=Just (SingleCondition (Field "Name") "<" (SOQLInt 1))}


      it "less than equal number" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name <= 20" `shouldBe` do
          SOQL{fields=[Field "Id"], object=SObject "Account", whereClause=Just (SingleCondition (Field "Name") "<=" (SOQLInt 20))}

      it "greater than number" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name > 0" `shouldBe` do
          SOQL{fields=[Field "Id"], object=SObject "Account", whereClause=Just (SingleCondition (Field "Name") ">" (SOQLInt 0))}

      it "greater than equal number" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name >= -1" `shouldBe` do
          SOQL{fields=[Field "Id"], object=SObject "Account", whereClause=Just (SingleCondition (Field "Name") ">=" (SOQLInt $ -1))}

      it "not equal bool, true" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name != true" `shouldBe` do
          SOQL{fields=[Field "Id"], object=SObject "Account", whereClause=Just (SingleCondition (Field "Name") "!=" (SOQLBool True))}

      it "not equal bool, false" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name != false" `shouldBe` do
          SOQL{fields=[Field "Id"], object=SObject "Account", whereClause=Just (SingleCondition (Field "Name") "!=" (SOQLBool False))}
