package org.saswata

import org.saswata.SimpleParser.ParserState

object Main {

  def main(args: Array[String]): Unit = {
    v2()
  }

  def v1(): Unit = {
    val input =
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

    val json: JValue = new Parser(input).parse()
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

  def v2(): Unit = {
    println(SimpleParser.falseValue(ParserState("sadasd")))
    println(SimpleParser.trueValue(ParserState("true sds ")))
    println(SimpleParser.nullValue(ParserState("   null dsf   ")))
    println(SimpleParser.nullValue(ParserState("a   null dsf   ", 3)))
    println(SimpleParser.openObj(ParserState("a   {null dsf   ", 3)))
    println(SimpleParser.quote(ParserState("a   \"null dsf   ", 3)))
    println(SimpleParser.stringValue(ParserState("a   \"null dsf\"   ", 3)))

    println("-------------------")

    println(SimpleParser.jValue(ParserState("   null ")))
    println(SimpleParser.jValue(ParserState("   true ")))
    println(SimpleParser.jValue(ParserState("   false ")))
    println(SimpleParser.jValue(ParserState("   \"whoo\" ")))
    println(SimpleParser.jValue(ParserState("   \"\" ")))
    println(SimpleParser.jValue(ParserState("   \"           \" ")))
    println(SimpleParser.jValue(ParserState("   \"whoo ")))
    println(SimpleParser.jValue(ParserState("   whoo ")))

    println("-------------------")
    println(SimpleParser.jField(ParserState("   \"key\" : \"    value    \" ")))
    println(SimpleParser.jValue(ParserState(" {  \"key\" : \"    value    \"   }  ")))
    println(SimpleParser.jValue(ParserState(" [   {  \"key\" : \"    value    \"   } ] ")))
    println(SimpleParser.jValue(ParserState(" [   \"a\" , \"b\"  ,  {  \"key\" :   \"    value    \" , \"k1\" : true  }, null ] ")))
    println(SimpleParser.jValue(ParserState("[f]")))
  }
}
