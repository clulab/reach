package edu.arizona.sista


package object assembly {

  // The type used to store the grounding ID
  // TODO: Should we include species, kb source, etc as part of this ID?
  // this is all available to us from grounding
  type GroundingID = String
  // unique ID associated with each Mention
  type IDPointer = Int



}
