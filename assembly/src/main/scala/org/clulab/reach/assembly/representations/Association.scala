package org.clulab.reach.assembly.representations

import org.clulab.odin.Mention
import org.clulab.reach.assembly.{AssemblyManager, IDPointer}

class Association(
                  val uniqueID: IDPointer,
                  val controllerPointers: Set[IDPointer],
                  val controlledPointers: Set[IDPointer],
                  val polarity: String,
                  val sourceMention: Option[Mention],
                  val manager: AssemblyManager
                ) extends ComplexEvent {



  override val eerString = "assembly.Association"

}

object Association{
  def apply(uniqueID: IDPointer,
            themePointers: Set[IDPointer],
            polarity: String,
            sourceMention: Option[Mention],
            manager: AssemblyManager) =
    new Association(uniqueID,
      themePointers.take(1),
      themePointers.drop(1),
      polarity,
      sourceMention,
      manager)
}
