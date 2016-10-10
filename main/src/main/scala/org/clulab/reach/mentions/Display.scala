package org.clulab.reach.mentions

import org.clulab.odin.Mention

trait Display {
  this: Mention =>

  // by default the displayLabel is set to the main label
  // but it can be modified
  var displayLabel: String = this.label
}
