package StandbyMe

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.xml.Node

package object XMLUtil {

  case class XMLSplitResult(init: List[Node], pair__list: List[(Node, List[Node])])

  def XMLSplit(strategy: (Node => Boolean))(node__list: List[Node]): XMLSplitResult = {

    def amount_of_descendants_of_node(node: Node): Int = (node \\ "_").length

    @tailrec
    def remove_children_helper(unreversed_result: List[Node], node__list: List[Node]): List[Node] = {
      node__list match {
        case Nil => unreversed_result.reverse
        case node :: _ => remove_children_helper(node :: unreversed_result, node__list.drop(amount_of_descendants_of_node(node)))
      }
    }

    def remove_children(node__list: List[Node]): List[Node] = {
      remove_children_helper(Nil, node__list)
    }

    def is_any_descendant_of_node_keynode(node: Node): Boolean = (node \\ "_").exists(strategy)

    def cut(node__list: List[Node]): (Node, List[Node], List[Node]) = {
      val keynode = node__list.head
      val (original_content_of_keynode, rest) = node__list.drop(amount_of_descendants_of_node(keynode)).span(!strategy(_))
      val content_of_keynode = remove_children(original_content_of_keynode.filterNot(is_any_descendant_of_node_keynode))
      (keynode, content_of_keynode, rest)
    }

    def helper(result_buffer: ListBuffer[(Node, List[Node])], node__list: List[Node]): List[(Node, List[Node])] = {
      node__list match {
        case Nil => result_buffer.toList
        case _ =>
          val (keynode, content_of_keynode, rest) = cut(node__list)
          helper(result_buffer += ((keynode, content_of_keynode)), rest)
      }
    }

    val (original_init, tail) = node__list.span(!strategy(_))
    val init = remove_children(original_init.filterNot(is_any_descendant_of_node_keynode))
    XMLSplitResult(init, helper(ListBuffer(), tail))
  }
}
