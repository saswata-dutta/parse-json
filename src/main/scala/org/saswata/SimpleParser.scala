package org.saswata

object SimpleParser {
  // https://www.json.org/json-en.html

  def skipWs(input: String, index: Int): Int = {
    var pos = index
    while (pos < input.length && input(pos).isWhitespace) {
      pos += 1
    }
    pos
  }

  def skipTillQuote(input: String, index: Int): Option[Int] = {
    var pos = index
    while (pos < input.length && input(pos) != '"') {
      pos += 1
    }

    if (pos == input.length) Option.empty else Some(pos)
  }

  def matchAt(target: String, input: String, index: Int): Option[Int] = {
    var pos = skipWs(input, index)
    var i = 0
    while (pos < input.length && i < target.length && input(pos) == target(i)) {
      i += 1
      pos += 1
    }

    if (i == target.length) Some(pos)
    else Option.empty
  }

  case class Match(begin: Int, end: Int)

  case class Mismatch(expected: String, begin: Int)

  case class ParserState(input: String, index: Int = 0,
                         matches: Vector[Match] = Vector.empty[Match],
                         mismatches: Set[Mismatch] = Set.empty[Mismatch],
                         jValues: Vector[JValue] = Vector.empty[JValue]) {

    def isFailed: Boolean = mismatches.nonEmpty
  }

  type Parser = ParserState => ParserState

  def lit(target: String)(state: ParserState): ParserState = {
    if (state.isFailed) state
    else {
      matchAt(target, state.input, state.index) match {
        case Some(i) => state.copy(index = i, matches = state.matches :+ Match(i - target.length, i))
        case _ => state.copy(mismatches = state.mismatches + Mismatch(target, state.index))
      }
    }
  }

  def stringChars(state: ParserState): ParserState = {
    if (state.isFailed) state
    else {
      skipTillQuote(state.input, state.index) match {
        case Some(i) => state.copy(index = i, matches = state.matches :+ Match(state.index, i))
        case _ => state.copy(mismatches = state.mismatches + Mismatch("String to be closed", state.index))
      }
    }
  }

  def sequence(parsers: Seq[Parser])(state: ParserState): ParserState = {

    var i = 0
    var curState = state
    while (i < parsers.length && !curState.isFailed) {
      curState = parsers(i)(curState)
      i += 1
    }

    curState
  }

  def choice(parsers: Seq[Parser])(state: ParserState): ParserState = {
    if (state.isFailed) state
    else {
      var i = 0
      var mismatches = Set.empty[Mismatch]
      while (i < parsers.length) {
        val newState = parsers(i)(state)
        if (!newState.isFailed) return newState
        mismatches = mismatches ++ newState.mismatches
        i += 1
      }

      state.copy(mismatches = mismatches)
    }
  }

  // punctuations
  val comma: Parser = lit(",")
  val colon: Parser = lit(":")
  val quote: Parser = lit("\"")
  val openObj: Parser = lit("{")
  val closeObj: Parser = lit("}")
  val openArr: Parser = lit("[")
  val closeArr: Parser = lit("]")


  def applyJValue(parser: Parser, transformation: Parser): Parser = {
    parser.andThen(state =>
      if (state.isFailed) state else {
        transformation(state)
      }
    )
  }

  // terminals
  val nullValue: Parser = applyJValue(lit("null"), state => {
    state.copy(
      matches = state.matches.init,
      jValues = state.jValues :+ JNull
    )
  })

  val trueValue: Parser = applyJValue(lit("true"), state => {
    state.copy(
      matches = state.matches.init,
      jValues = state.jValues :+ JTrue
    )
  })

  val falseValue: Parser = applyJValue(lit("false"), state => {
    state.copy(
      matches = state.matches.init,
      jValues = state.jValues :+ JFalse
    )
  })

  val stringValue: Parser = applyJValue(sequence(Seq(quote, stringChars, quote)), state => {
    val pos = state.matches(state.matches.length - 2)
    val str = state.input.slice(pos.begin, pos.end)
    state.copy(
      matches = state.matches.dropRight(3),
      jValues = state.jValues :+ JStr(str)
    )
  })

  // non terminals
  val jValue: Parser = choice(Seq(nullValue, trueValue, falseValue, stringValue))
  val jField: Parser = applyJValue(sequence(Seq(stringValue, colon, jValue)), state => {
    // drop the colon
    state.copy(matches = state.matches.init)
  })
}
