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
                         pending: Vector[Match] = Vector.empty[Match],
                         mismatches: Set[Mismatch] = Set.empty[Mismatch],
                         stack: Vector[JValue] = Vector.empty[JValue]) {

    def isFailed: Boolean = mismatches.nonEmpty
  }

  type Parser = ParserState => ParserState

  def lit(target: String)(state: ParserState): ParserState = {
    if (state.isFailed) state
    else {
      matchAt(target, state.input, state.index) match {
        case Some(i) => state.copy(index = i, pending = state.pending :+ Match(i - target.length, i))
        case _ => state.copy(mismatches = state.mismatches + Mismatch(target, state.index))
      }
    }
  }

  def stringChars(state: ParserState): ParserState = {
    if (state.isFailed) state
    else {
      skipTillQuote(state.input, state.index) match {
        case Some(i) => state.copy(index = i, pending = state.pending :+ Match(state.index, i))
        case _ => state.copy(mismatches = state.mismatches + Mismatch("String to be closed", state.index))
      }
    }
  }

  def sequence(parsers: Parser*)(state: ParserState): ParserState = {
    require(!parsers.contains(null))

    var i = 0
    var curState = state
    while (i < parsers.length && !curState.isFailed) {
      curState = parsers(i)(curState)
      i += 1
    }

    curState
  }

  def choice(parsers: Parser*)(state: ParserState): ParserState = {
    require(!parsers.contains(null))

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

  def repeat(itemParser: Parser, sepParser: Parser)(state: ParserState): ParserState = {
    var curState = state
    while (!curState.isFailed) {
      curState = itemParser(curState)
      if (!curState.isFailed) {
        val state1 = sepParser(curState)
        if (state1.isFailed) {
          return curState
        } else {
          curState = state1
        }
      }
    }
    curState
  }

  def applyJValue(parser: Parser, transformation: Parser): Parser = {
    parser.andThen(state =>
      if (state.isFailed) state else {
        transformation(state).copy(pending = Vector.empty)
      }
    )
  }

  // punctuations
  val comma: Parser = lit(",")
  val colon: Parser = lit(":")
  val quote: Parser = lit("\"")
  val openObj: Parser = applyJValue(lit("{"), state => state.copy(stack = state.stack :+ _JObjMarker))
  val closeObj: Parser = lit("}")
  val openArr: Parser = applyJValue(lit("["), state => state.copy(stack = state.stack :+ _JArrMarker))
  val closeArr: Parser = lit("]")

  def jValue: Parser = choice(nullValue, trueValue, falseValue, stringValue, jObj, jArr)

  // terminals
  val nullValue: Parser = applyJValue(lit("null"), state => {
    state.copy(
      stack = state.stack :+ JNull
    )
  })

  val trueValue: Parser = applyJValue(lit("true"), state => {
    state.copy(
      stack = state.stack :+ JTrue
    )
  })

  val falseValue: Parser = applyJValue(lit("false"), state => {
    state.copy(
      stack = state.stack :+ JFalse
    )
  })

  val stringValue: Parser = applyJValue(sequence(quote, stringChars, quote), state => {
    val pos = state.pending(state.pending.length - 2)
    val str = state.input.slice(pos.begin, pos.end)
    state.copy(
      stack = state.stack :+ JStr(str)
    )
  })

  // non terminals
  val jField: Parser = applyJValue(sequence(stringValue, colon, jValue), state => {
    val pair = _JField(
      key = state.stack(state.stack.length - 2).asInstanceOf[JStr],
      value = state.stack(state.stack.length - 1))

    state.copy(stack = state.stack.dropRight(2) :+ pair)
  })

  private def skipTill(stack: Vector[JValue], marker: JValue): (Vector[JValue], Vector[JValue]) = {
    val l = stack.length - 1
    var i = l
    while (i >= 0 && stack(i) != marker) {
      i -= 1
    }

    (stack.take(i), stack.takeRight(l - i))
  }

  val jObj: Parser = applyJValue(sequence(openObj, repeat(jField, comma), closeObj), state => {

    val (left, right) = skipTill(state.stack, _JObjMarker)

    val fields = right.collect {
      case _JField(JStr(key), value) => key -> value
    }.toMap

    state.copy(stack = left :+ JObj(fields))
  })

  val jArr: Parser = applyJValue(sequence(openArr, repeat(jValue, comma), closeArr), state => {
    val (left, right) = skipTill(state.stack, _JArrMarker)

    state.copy(stack = left :+ JArr(right))
  })
}
