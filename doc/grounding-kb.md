# How to add a new grounding KB

1. Create a new KBML method in org.clulab.reach.grounding.ReachIMKBMentionLookups, and store it as a val as well. See for example ReachIMKBMentionLookups.StaticProtein.

2. Add the above KBML to the list of KBs to be searched in org.clulab.reach.grounding.ReachEntityLookup.

