package ai.lum.nxmlreader.standoff

import ai.lum.common.Interval

object Implicits {

  implicit class RichTree(tree: Tree) {
    def sectionNames(sectionCounter: Int = 0): List[String] = {
      // No section name if it is the root
      tree.parent match {
        case None =>
          Nil
        case Some(parent) =>
          tree.label match {
            // Abstract will always be named the same as its tag name
            case "abstract" | "article-title" =>
              tree.label :: parent.sectionNames(sectionCounter)
            // This is the general case. Since we transition to a new section, increment the counter on all recursive calls
            case "sec" =>
              // If the sec tag has a sec-type label, use it as the section name
              if (tree.attributes contains "sec-type")
                tree.attributes("sec-type") :: parent.sectionNames(sectionCounter + 1)
              else
              // If the first is child is a "title" tag, use its text as section name
                tree.children.headOption match {
                  case Some(child) if child.label == "title" =>
                    child.text.toLowerCase.trim :: parent.sectionNames(sectionCounter + 1)
                  // Otherwise use the running counter of the section
                  case _ =>
                    s"sec-$sectionCounter" :: parent.sectionNames(sectionCounter + 1)
                }
            // If it is any other node, use the parent's section name
            case _ =>
              parent.sectionNames()
          }
      }

    }

    /**
      * @return Map of the character intervals of terminal nodes to their section names hierachy
      */
    def sectionNamesIntervals: Map[Interval, Seq[String]] =
      (tree.getTerminals() map {
        terminal =>
          terminal.characterInterval -> terminal.sectionNames()
      }).toMap
  }
}
