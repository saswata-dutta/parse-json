sealed trait Json {
  def apply(i: Int): Json = ???
  def apply(k: String): Json = ???
}

final case class JStr(value: String) extends Json
final case class JNum(value: Double) extends Json
final case class JBool(value: Boolean) extends Json
final case class JObj(fields: Map[String, Json]) extends Json {
  override def apply(k: String): Json = {
    assert(k.trim.nonEmpty, s"Found empty key $k")
    fields(k)
  }
}
final case class JArr(elements: Seq[Json]) extends Json {
  override def apply(i: Int): Json = {
    assert(i >= 0 && i < elements.length, s"Found out of bound index $i")
    elements(i)
  }
}
case object JNull extends Json
