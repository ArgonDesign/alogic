
package alogic

object AlogicMain extends App {

  val s = AParser("../build/apb_splitter.alogic")

  println(s.parseTree)
}
