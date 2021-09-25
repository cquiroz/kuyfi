package kuyfi

import java.util.{ List => JList, Map => JMap, Set => JSet }
import scala.jdk.CollectionConverters._

object JDKConv {
  def toJava[A, B](m: Map[A, B]): JMap[A, B] = m.asJava
  def toJava[A](m: List[A]): JList[A]        = m.asJava
  def toScala[A](m: JSet[A]): Set[A]         = m.asScala.toSet
}
