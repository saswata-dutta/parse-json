package org.saswata

sealed trait JValue {
  def apply(i: Int): JValue = ???

  def apply(k: String): JValue = ???
}

case object JNull extends JValue

case object JTrue extends JValue

case object JFalse extends JValue

final case class JStr(value: String) extends JValue

final case class JNum(value: Double) extends JValue

final case class JObj(fields: Map[String, JValue]) extends JValue {
  override def apply(k: String): JValue = {
    require(k.trim.nonEmpty, s"Found empty key $k")
    fields(k)
  }
}

final case class JArr(elements: Seq[JValue]) extends JValue {
  override def apply(i: Int): JValue = {
    require(i >= 0 && i < elements.length, s"Found out of bound index $i")
    elements(i)
  }
}
