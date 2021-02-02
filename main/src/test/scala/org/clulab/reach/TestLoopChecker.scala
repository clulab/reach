package org.clulab.reach

import org.clulab.reach.utils.LoopChecker

import scala.collection.mutable.{Buffer => MutableBuffer}

class TestLoopChecker extends ReachTest {

  class Node(val id: Int) {
    val children: MutableBuffer[Node] = MutableBuffer.empty
  }

  class NodeLoopChecker() extends LoopChecker[Node] {
    override def getChildren(node: Node): Seq[Node] = node.children
  }

  val nodeLoopChecker = new NodeLoopChecker()

  behavior of "LoopChecker"

  it should "not find nonexistent loops" in {
    val nodes = Seq(
      new Node(0),
      new Node(1),
      new Node(2),
      new Node(3)
    )

    nodes(0).children += nodes(1)
    nodes(0).children += nodes(2)
    nodes(1).children += nodes(2)
    nodes(3).children += nodes(0)
    nodes(3).children += nodes(1)
    nodes(3).children += nodes(2)

    nodeLoopChecker.checkForLoops(nodes) should be (false)
  }

  it should "find existent loops" in {
    val nodes = Seq(
      new Node(0),
      new Node(1),
      new Node(2),
      new Node(3)
    )

    nodes(0).children += nodes(1)
    nodes(0).children += nodes(2)
    nodes(1).children += nodes(2)
    nodes(3).children += nodes(0)
    nodes(3).children += nodes(1)
    nodes(2).children += nodes(3) // This one is switched around.

    nodeLoopChecker.checkForLoops(nodes) should be (true)
  }
}
