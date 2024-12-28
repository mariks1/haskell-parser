{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Types
import Parser.JSON (parseJSON)
import Converter (nodeToJSON, prettyNodeToXML)
import Parser.XML (parseXML)

main :: IO ()
main = hspec $ do
  describe "Complex JSON to XML Conversion" $ do
    it "converts nested JSON with arrays and objects to equivalent XML" $ do
      let jsonStr = "{\"employees\": [{\"name\": \"Alice\", \"age\": 30}, {\"name\": \"Bob\", \"age\": 25}], \"company\": {\"name\": \"Tech Corp\", \"location\": \"New York\"}}"
          expectedXML = "<root>\n  <employees>\n    <array>\n      <item>\n        <name>\n          Alice\n        </name>\n        <age>\n          30.0\n        </age>\n\n      </item>\n      <item>\n        <name>\n          Bob\n        </name>\n        <age>\n          25.0\n        </age>\n\n      </item>\n    </array>\n\n  </employees>\n  <company>\n    <name>\n      Tech Corp\n    </name>\n    <location>\n      New York\n    </location>\n\n  </company>\n</root>\n"
          jsonNode = parseJSON jsonStr
          xmlOutput = prettyNodeToXML jsonNode

      xmlOutput `shouldBe` expectedXML

    it "handles complex nested structures with mixed content" $ do
      let jsonStr = "{\"greeting\": \"Hello, \", \"user\": {\"name\": \"Alice\", \"messages\": [{\"text\": \"How are you?\"}, {\"text\": \"See you later!\"}]}}"
          expectedXML = "<root>\n  <greeting>\n    Hello, \n  </greeting>\n  <user>\n    <name>\n      Alice\n    </name>\n    <messages>\n      <array>\n        <item>\n          <text>\n            How are you?\n          </text>\n\n        </item>\n        <item>\n          <text>\n            See you later!\n          </text>\n\n        </item>\n      </array>\n\n    </messages>\n\n  </user>\n</root>\n"
          jsonNode = parseJSON jsonStr
          xmlOutput = prettyNodeToXML jsonNode

      xmlOutput `shouldBe` expectedXML

  describe "Complex XML to JSON Conversion" $ do
    it "converts complex XML to equivalent JSON" $ do
      let xmlStr = "<company><name>Tech Corp</name><employees><employee><name>Alice</name><age>30</age></employee><employee><name>Bob</name><age>25</age></employee></employees></company>"
          expectedJSON = "{\n  \"company\": [\n    {\n      \"name\": \"Tech Corp\"\n    },\n    {\n      \"employees\": [\n        {\n          \"employee\": [\n            {\n              \"name\": \"Alice\"\n            },\n            {\n              \"age\": \"30\"\n            }\n          ]\n        },\n        {\n          \"employee\": [\n            {\n              \"name\": \"Bob\"\n            },\n            {\n              \"age\": \"25\"\n            }\n          ]\n        }\n      ]\n    }\n  ]\n}"
          xmlNode = parseXML xmlStr
          jsonOutput = nodeToJSON xmlNode

      jsonOutput `shouldBe` expectedJSON

    it "handles nested XML structures with arrays" $ do
      let xmlStr = "<user><name>Alice</name><messages><message>Hello!</message><message>Goodbye!</message></messages></user>"
          expectedJSON = "{\n  \"user\": [\n    {\n      \"name\": \"Alice\"\n    },\n    {\n      \"messages\": [\n        {\n          \"message\": \"Hello!\"\n        },\n        {\n          \"message\": \"Goodbye!\"\n        }\n      ]\n    }\n  ]\n}"
          xmlNode = parseXML xmlStr
          jsonOutput = nodeToJSON xmlNode

      jsonOutput `shouldBe` expectedJSON

    it "converts XML with attributes to JSON" $ do
      let xmlStr = "<product><name>Gadget</name><price>99.99</price></product>"
          expectedJSON = "{\n  \"product\": [\n    {\n      \"name\": \"Gadget\"\n    },\n    {\n      \"price\": \"99.99\"\n    }\n  ]\n}"
          xmlNode = parseXML xmlStr
          jsonOutput = nodeToJSON xmlNode

      jsonOutput `shouldBe` expectedJSON

    it "handles mixed types and nested arrays" $ do
      let xmlStr = "<items><item><name>Item1</name><price>10.0</price></item><item><name>Item2</name><price>15.0</price></item></items>"
          expectedJSON = "{\n  \"items\": [\n    {\n      \"item\": [\n        {\n          \"name\": \"Item1\"\n        },\n        {\n          \"price\": \"10.0\"\n        }\n      ]\n    },\n    {\n      \"item\": [\n        {\n          \"name\": \"Item2\"\n        },\n        {\n          \"price\": \"15.0\"\n        }\n      ]\n    }\n  ]\n}"
          xmlNode = parseXML xmlStr
          jsonOutput = nodeToJSON xmlNode

      jsonOutput `shouldBe` expectedJSON
