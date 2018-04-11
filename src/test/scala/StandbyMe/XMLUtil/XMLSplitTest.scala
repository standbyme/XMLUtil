package StandbyMe.XMLUtil

import org.scalatest.FunSuite
import scala.xml.Node

class XMLSplitTest extends FunSuite {
  def extract_simple_text_from_node(node: Node): String = node.text.trim.replace("\n", "").replace(" ", "")

  val keywords = Set("Keyword1:", "Keyword2:")
  val doc: scala.xml.Elem =
    <body>
      <k>Head</k>
      <a>Keyword1:</a>
      <b>
        <c>
          <d>val</d>
        </c>
        <e>ue</e>
        <f>Keyword2:</f>
        <g>123
          <h>456</h>
        </g>
      </b>
      <i>789</i>
    </body>

  def strategy(node: Node): Boolean = keywords.contains(extract_simple_text_from_node(node))

  private val result = XMLSplit(strategy)((doc \\ "_").toList)
  println(result.init)
  println(result.pair__list.length)
  result.pair__list.foreach(println)
}
