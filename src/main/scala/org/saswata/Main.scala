package org.saswata

import org.saswata.JsonParserCombinator.ParserState

object Main {
  val input: String =
    """
      |{
      |    "hello": {
      |        "i am": { "cow" : "boy" },
      |        "you are": {"cows": [2, {"bessy" : 666}, null]}
      |    },
      |    "bad" : false,
      |    "world": -313.37,
      |    "foo" : [{"a" : 1}, 2.23, +12.43],
      |    "bye": "314",
      |    "good":true,
      |    "now": null,
      |    "ok"   :   false
      |}
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    test(new Parser(input).parse())
    test(JsonParserCombinator.parse(input))

    v2_test()
  }

  def test(json: JValue): Unit = {
    println(json)

    val lookup =
      Seq("hello", "you are", "cows", "1", "bessy").foldLeft(json)(
        (acc, k) =>
          acc match {
            case obj: JObj => obj(k)
            case arr: JArr => arr(k.toInt)
            case _ => throw new IllegalArgumentException(s"Failed Key lookup $k")
          }
      )

    println(lookup)
  }

  def v2_test(): Unit = {
    println(JsonParserCombinator.falseValue(ParserState("sadasd")))
    println(JsonParserCombinator.trueValue(ParserState("true sds ")))
    println(JsonParserCombinator.nullValue(ParserState("   null dsf   ")))
    println(JsonParserCombinator.nullValue(ParserState("a   null dsf   ", 3)))
    println(JsonParserCombinator.openObj(ParserState("a   {null dsf   ", 3)))
    println(JsonParserCombinator.quote(ParserState("a   \"null dsf   ", 3)))
    println(JsonParserCombinator.stringValue(ParserState("a   \"null dsf\"   ", 3)))

    println("-------------------")

    println(JsonParserCombinator.jValue(ParserState("   123 ")))
    println(JsonParserCombinator.jValue(ParserState("   123.456 ")))
    println(JsonParserCombinator.jValue(ParserState("   +123.456 ")))
    println(JsonParserCombinator.jValue(ParserState("   -123.456 ")))
    println(JsonParserCombinator.jValue(ParserState("   1e5 ")))
    println(JsonParserCombinator.jValue(ParserState("   + 23 ")))
    println(JsonParserCombinator.jValue(ParserState("   123.12.2 ")))
    println(JsonParserCombinator.jValue(ParserState("   +23-4 ")))

    println("-------------------")

    println(JsonParserCombinator.jValue(ParserState("   null ")))
    println(JsonParserCombinator.jValue(ParserState("   true ")))
    println(JsonParserCombinator.jValue(ParserState("   false ")))
    println(JsonParserCombinator.jValue(ParserState("   \"whoo\" ")))
    println(JsonParserCombinator.jValue(ParserState("   \"\" ")))
    println(JsonParserCombinator.jValue(ParserState("   \"           \" ")))
    println(JsonParserCombinator.jValue(ParserState("   \"whoo ")))
    println(JsonParserCombinator.jValue(ParserState("   whoo ")))

    println("-------------------")
    println(JsonParserCombinator.jField(ParserState("   \"key\" : \"    value    \" ")))
    println(JsonParserCombinator.jValue(ParserState(" {  \"key\" : \"    value    \"   }  ")))
    println(JsonParserCombinator.jValue(ParserState(" [   {  \"key\" : \"    value    \"   } ] ")))
    println(JsonParserCombinator.jValue(ParserState(" [   \"a\" , \"b\"  ,  {  \"key\" :   \"    value    \" , \"k1\" : true  }, null ] ")))
    println(JsonParserCombinator.jValue(ParserState("[f]")))

    println(JsonParserCombinator.parse("[]"))
    println(JsonParserCombinator.parse("  {   }  "))
    println(JsonParserCombinator.parse("  [ {   } , [], [{ } ]  ] "))
    println(JsonParserCombinator.parse("  { \"k1\" :  [  ]   } "))
    println(JsonParserCombinator.parse("  [ { \"k1\" :  {   }  } ] "))

    println("-------------------")
    println(JsonParserCombinator.jValue(ParserState(" {} {}")))
  }
}
