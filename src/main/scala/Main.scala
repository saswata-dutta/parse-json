object Main {

  def main(args: Array[String]): Unit = {
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
        |    "now": null
        |}
        |""".stripMargin

    val json: Json = new Parser(input).parse()
    println(json)

    val lookup =
      Seq("hello", "you are", "cows", "1", "bessy").foldLeft(json)(
        (acc, k) =>
          acc match {
            case obj: JObj => obj(k)
            case arr: JArr => arr(k.toInt)
            case _         => throw new IllegalArgumentException(s"Failed Key lookup $k")
          }
      )

    println(lookup)
  }
}
