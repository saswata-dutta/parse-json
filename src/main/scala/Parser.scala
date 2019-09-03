import scala.collection.mutable

class Parser(val input: String) {
  import Parser._

  private var offset: Int = 0

  def skipWhiteSpace(): Unit =
    while (input(offset).isWhitespace) offset += 1

  def parse(): Json = {
    skipWhiteSpace()

    if (offset >= input.length) throw new IllegalArgumentException(s"Unexpected end of input")
    val current = input(offset)
    current match {
      case DOUBLE_QUOTE   => JStr(parseStr())
      case c if c.isDigit => JNum(parseNum())
      case '+' | '-'      => JNum(parseNum())
      case 't' | 'f'      => JBool(parseBool())
      case 'n'            => parseNull()
      case '{'            => parseObj()
      case '['            => parseArr()
      case x              => throw new IllegalArgumentException(s"Unexpected $x at ${input.substring(offset)}")
    }
  }

  def parseNull(): Json =
    parseSymbol(input, "null", offset) match {
      case Some(pos) =>
        offset = pos
        JNull
      case None =>
        throw new IllegalArgumentException(s"Failed to parse null at ${input.substring(offset)}")
    }

  def parseStr(): String = {
    val start = offset

    skipChar(DOUBLE_QUOTE)
    while (offset < input.length && input(offset) != DOUBLE_QUOTE) offset += 1

    if (offset < input.length) {
      skipChar(DOUBLE_QUOTE)
      input.slice(start + 1, offset - 1)
    } else
      throw new IllegalArgumentException("Unexpected End of Input after String")
  }

  def parseNum(): Double = {
    val start = offset
    while (offset < input.length
           && !(input(offset).isWhitespace
           || input(offset) == ']'
           || FIELD_TERMINATORS.contains(input(offset)))) {
      offset += 1
    }
    if (offset < input.length)
      input.slice(start, offset).toDouble
    else
      throw new IllegalArgumentException(
        s"Unexpected End of Input after number ${input.substring(start)}"
      )
  }

  def parseBool(): Boolean =
    parseSymbol(input, "true", offset).orElse(parseSymbol(input, "false", offset)) match {
      case Some(pos) =>
        val symbol = input.substring(offset, pos)
        offset = pos
        symbol.toBoolean
      case None =>
        throw new IllegalArgumentException(s"Failed to parse bool at ${input.substring(offset)}")
    }

  def parseObj(): JObj = {
    val fields = mutable.Buffer[(String, Json)]()
    skipChar('{')

    var done = false
    while (!done) {
      skipWhiteSpace()
      val key = parseStr()

      skipWhiteSpace()
      skipChar(':')
      skipWhiteSpace()

      val value = parse()
      fields.append((key, value))
      skipWhiteSpace()

      if (!FIELD_TERMINATORS.contains(input(offset)))
        throw new IllegalArgumentException(s"Failed to parse object at ${input.substring(offset)}")

      if (input(offset) == '}') {
        done = true
        skipChar('}')
      } else {
        skipChar(',')
      }
    }

    JObj(fields.toMap)
  }

  def parseArr(): JArr = {
    val elems = mutable.Buffer[Json]()
    skipChar('[')

    var done = false
    while (!done) {
      skipWhiteSpace()
      val elem = parse()
      elems.append(elem)
      skipWhiteSpace()

      if (input(offset) == ']') {
        done = true
        skipChar(']')
      } else {
        skipChar(',')
      }
    }
    JArr(elems.toSeq)
  }

  def skipChar(char: Char): Unit =
    if (input(offset) == char) offset += 1
    else throw new IllegalArgumentException(s"Expected $char at ${input.substring(offset)}")
}

object Parser {
  val DOUBLE_QUOTE: Char = "\"".head // 34.toChar
  val FIELD_TERMINATORS: Set[Char] = Set(',', '}')

  def parseSymbol(in: String, symbol: String, pos: Int): Option[Int] =
    if (in.substring(pos, pos + symbol.length) == symbol)
      Option(pos + symbol.length)
    else Option.empty[Int]
}
