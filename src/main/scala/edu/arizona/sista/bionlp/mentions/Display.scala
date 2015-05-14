package edu.arizona.sista.bionlp.mentions

import edu.arizona.sista.odin.Mention

trait Display {
  this: Mention =>

  // by default the displayLabel is set to the main label
  // but it can be modified
  var displayLabel: String = this.label
}
