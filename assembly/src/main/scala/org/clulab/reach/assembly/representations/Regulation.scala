package org.clulab.reach.assembly.representations

import org.clulab.reach.assembly.AssemblyManager
import org.clulab.reach.assembly._
import org.clulab.odin.Mention


/**
 * Representation of a Regulation.
 *
 * @param uniqueID the [[IDPointer]] assigned to this [[Regulation]]
 * @param controllerPointers a Set of [[IDPointer]] corresponding to the Mentions serving as controllers to the [[Regulation]] <br>
 *                           It is a set because each Mention of a Regulation may have more than one controller, and each Mention contained in [[AssemblyManager.mentionToID]] points to exactly one [[IDPointer]] which corresponds to exactly one [[EntityEventRepresentation]] in [[AssemblyManager.idToEER]]
 * @param controlledPointers a Set of [[IDPointer]] corresponding to the Mentions serving as the controlled to the [[Regulation]] <br>
 *                           It is a set because each Mention of a Regulation may have more than one controlled, and each Mention contained in [[AssemblyManager.mentionToID]] points to exactly one [[IDPointer]] which corresponds to exactly one [[EntityEventRepresentation]] in [[AssemblyManager.idToEER]]
 * @param polarity whether the [[Regulation]] is [[AssemblyManager.positive]], [[AssemblyManager.negative]], or [[AssemblyManager.unknown]]
 * @param sourceMention the Mention from which this [[Regulation]] was constructed
 * @param manager a pointer to the [[AssemblyManager]] instance that produced this [[Regulation]]
 */
class Regulation(
  val uniqueID: IDPointer,
  val controllerPointers: Set[IDPointer],
  val controlledPointers: Set[IDPointer],
  val polarity: String,
  val sourceMention: Option[Mention],
  val manager: AssemblyManager
)  extends ComplexEvent {

  override val eerString = "assembly.Regulation"

}
