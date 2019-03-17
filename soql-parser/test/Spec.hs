import Test.Hspec
import Control.Exception (evaluate)
import SOQL

main :: IO ()
main = hspec $ do
  describe "parseSOQL" $ do
    it "select one field" $ do
      parseSOQL "SELECT Id FROM Account" `shouldBe` SOQL{soqlFields=[(Field "Id")], soqlObject=SObject "Account", soqlWhereClause=Nothing}

    it "select mulitple field" $ do
      parseSOQL "SELECT Id, Name FROM Account" `shouldBe` SOQL{soqlFields=[(Field "Id"), (Field "Name")], soqlObject=SObject "Account", soqlWhereClause=Nothing}

    it "select mulitple field include subquery" $ do
      parseSOQL "SELECT Id, Name, (SELECT Id, LastName FROM Contacts) FROM Account" `shouldBe` do
        SOQL{soqlFields=[(Field "Id"), (Field "Name"), (SubQuery (SOQL{soqlFields=[Field "Id", Field "LastName"], soqlObject=SObject "Contacts", soqlWhereClause=Nothing}))], soqlObject=SObject "Account", soqlWhereClause=Nothing}

    describe "single where condition" $ do
      it "equal string" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name = 'foo'" `shouldBe` do
          SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") "=" (SOQLString "foo"))}

      it "less than number" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name < 1" `shouldBe` do
          SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") "<" (SOQLInt 1))}


      it "less than equal number" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name <= 20" `shouldBe` do
          SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") "<=" (SOQLInt 20))}

      it "greater than number" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name > 0" `shouldBe` do
          SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") ">" (SOQLInt 0))}

      it "greater than equal number" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name >= -1" `shouldBe` do
          SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") ">=" (SOQLInt $ -1))}

      it "not equal bool, true" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name != true" `shouldBe` do
          SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") "!=" (SOQLBool True))}

      it "not equal bool, false" $ do
        parseSOQL "SELECT Id FROM Account WHERE Name != false" `shouldBe` do
          SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") "!=" (SOQLBool False))}

    describe "multiple condition" $ do
      it "A AND B" $ do
        parseSOQL "SELECT Id FROM Account WHERE Id = 1 AND Name = 2" `shouldBe` do
          SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (MultipleCondition
            (SingleCondition (Field "Id") "=" (SOQLInt 1))
            "AND"
            (SingleCondition (Field "Name") "=" (SOQLInt 2))
            )}

      it "A OR B" $ do
        parseSOQL "SELECT Id FROM Account WHERE Id = 1 OR Name = 2" `shouldBe` do
          SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (MultipleCondition
            (SingleCondition (Field "Id") "=" (SOQLInt 1))
            "OR"
            (SingleCondition (Field "Name") "=" (SOQLInt 2))
            )}

      it "A AND B OR C" $ do
        parseSOQL "SELECT Id FROM Account WHERE Id = 1 AND Name = 2 OR Foo = 3" `shouldBe` do
          SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (MultipleCondition
            (SingleCondition (Field "Id") "=" (SOQLInt 1))
            "AND"
            (MultipleCondition
              (SingleCondition (Field "Name") "=" (SOQLInt 2))
              "OR"
              (SingleCondition (Field "Foo") "=" (SOQLInt 3))
              ))}

  describe "generateSOQL" $ do
    it "select one field" $ do
      generateSOQL SOQL{soqlFields=[(Field "Id")], soqlObject=SObject "Account", soqlWhereClause=Nothing} `shouldBe` "SELECT Id FROM Account"

    it "select mulitple field" $ do
      generateSOQL SOQL{soqlFields=[(Field "Id"), (Field "Name")], soqlObject=SObject "Account", soqlWhereClause=Nothing} `shouldBe` "SELECT Id, Name FROM Account"

    it "select mulitple field include subquery" $ do
      generateSOQL SOQL{
        soqlFields=[(Field "Id"),
          (Field "Name"),
          (SubQuery (SOQL{
            soqlFields=[Field "Id", Field "LastName"],
            soqlObject=SObject "Contacts",
            soqlWhereClause=Nothing
            }))
          ],
          soqlObject=SObject "Account",
          soqlWhereClause=Nothing
          } `shouldBe` do
        "SELECT Id, Name, (SELECT Id, LastName FROM Contacts) FROM Account"

    describe "single where condition" $ do
      it "equal string" $ do
        generateSOQL SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") "=" (SOQLString "foo"))}  `shouldBe` do
          "SELECT Id FROM Account WHERE Name = 'foo'"

      it "less than number" $ do
        generateSOQL SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") "<" (SOQLInt 1))} `shouldBe` do
          "SELECT Id FROM Account WHERE Name < 1"

      it "less than equal number" $ do
        generateSOQL SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") "<=" (SOQLInt 20))} `shouldBe` do
          "SELECT Id FROM Account WHERE Name <= 20"

      it "greater than number" $ do
        generateSOQL SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") ">" (SOQLInt 0))} `shouldBe` do
          "SELECT Id FROM Account WHERE Name > 0"

      it "greater than equal number" $ do
        generateSOQL SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") ">=" (SOQLInt $ -1))} `shouldBe` do
          "SELECT Id FROM Account WHERE Name >= -1"

      it "not equal bool, true" $ do
        generateSOQL SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") "!=" (SOQLBool True))} `shouldBe` do
          "SELECT Id FROM Account WHERE Name != true"

      it "not equal bool, false" $ do
        generateSOQL SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (SingleCondition (Field "Name") "!=" (SOQLBool False))} `shouldBe` do
          "SELECT Id FROM Account WHERE Name != false"

    describe "multiple condition" $ do
      it "A AND B" $ do
        generateSOQL SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (MultipleCondition
                                 (SingleCondition (Field "Id") "=" (SOQLInt 1))
                                 "AND"
                                 (SingleCondition (Field "Name") "=" (SOQLInt 2))
                                 )} `shouldBe` do
          "SELECT Id FROM Account WHERE Id = 1 AND Name = 2"


      it "A OR B" $ do
        generateSOQL SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (MultipleCondition
                                 (SingleCondition (Field "Id") "=" (SOQLInt 1))
                                 "OR"
                                 (SingleCondition (Field "Name") "=" (SOQLInt 2))
                                 )} `shouldBe` do
          "SELECT Id FROM Account WHERE Id = 1 OR Name = 2"

      it "A AND B OR C" $ do
        generateSOQL SOQL{soqlFields=[Field "Id"], soqlObject=SObject "Account", soqlWhereClause=Just (MultipleCondition
                                 (SingleCondition (Field "Id") "=" (SOQLInt 1))
                                 "AND"
                                 (MultipleCondition
                                   (SingleCondition (Field "Name") "=" (SOQLInt 2))
                                   "OR"
                                   (SingleCondition (Field "Foo") "=" (SOQLInt 3))
                                   ))} `shouldBe` do
          "SELECT Id FROM Account WHERE Id = 1 AND Name = 2 OR Foo = 3"


