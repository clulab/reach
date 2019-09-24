package org.clulab.reach.context.feature_utils

// this object is used in the feature extractor to compare binned distances
object FeatureProcessing{
  def binSentenceDistance(d:Int):BinnedDistance.Value = {
    if(d == 0)
      BinnedDistance.SAME
    else if(d <= 13)
      BinnedDistance.CLOSE
    else
      BinnedDistance.FAR
  }

  def binDependencyDistance(d:Int):BinnedDistance.Value = {
    if(d <= 0)
      BinnedDistance.CLOSE
    else
      BinnedDistance.FAR
  }

  def clusterPOSTag(tag:String):String ={

    if(tag.startsWith("NN"))
      return "NN"
    else if(tag.startsWith("VB"))
      return "VB"
    else if(Set(",", "-RRB-", ".", ":", ";", "-LRB-").contains(tag))
      return "BOGUS"
    else
      return tag
  }

  def clusterDependency(d:String):String = {
    if(d.startsWith("prep"))
      "prep"
    else if(d.startsWith("conj"))
      "conj"
    else if(d.endsWith("obj"))
      "obj"
    else if(d.endsWith("mod"))
      "mod"
    else if(d.contains("subj"))
      "subj"
    else
      d
  }


}
