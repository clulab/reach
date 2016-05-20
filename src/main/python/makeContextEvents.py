''' Creates the context_events file out of a curated tsv file '''
import sys, csv
import numpy as np
from collections import defaultdict

def main(path, vocabularyPath, statesPath):
    ''' Parses a TSV file and outputs the context_events file '''

    # Set this variable to true to overwrite the states file to inject the manual annotations
    inject_annotations = False


    # Load the states matrix
    if inject_annotations:
        states = np.loadtxt(statesPath).astype(int)

    with open(vocabularyPath) as f:
        vocabulary = [l[:-1] for l in f]

    with open(path) as f:
        reader = csv.reader(f, delimiter='\t')

        # Key: Ann ID - Val: Line num
        tbAnnotations = {}

        # Key: Evt ID - Val: list of ctx ids
        evtCtx = defaultdict(list)

        # Key: Ctx ID - Val: voc key
        ctxIds = {}

        for ix, row in enumerate(reader):
            # 1 Line id
            # 2 CtxIDs
            # 3 Ctx transcriptions
            # 4 Textbound annotations
            # 5 evt-ctx relations
            # 6 evt transcriptions
            localTbAnns = map(lambda s: s.strip().upper(), row[3].split(','))
            localEvtCtx = map(lambda s: s.strip().upper(), row[4].split(','))
            localCtxId = map(lambda s: s.strip().upper(), row[1].split(','))

            for ann in localTbAnns:
                if ann != '':
                    if ann in tbAnnotations:
                        print "#WARNING: %s repeated in line %i" % (ann, ix)
                    else:
                        tbAnnotations[ann] = ix

            for evt in localTbAnns:
                if evt != '':
                    if evt[0] == 'E': # Only for events
                        evtCtx[evt] = localEvtCtx

            localContexts = zip([a for a in localTbAnns if a != '' and a[0] != 'E'], localCtxId)
            for ctx, cid in localContexts:
                ctxIds[ctx] = cid


    # Now print the lines`
    for ann in tbAnnotations:
        # Only for events
        if ann[0] == 'E':
            ec = evtCtx[ann]
            eventIndex = tbAnnotations[ann]

            for c in ec:
                if c[0] == 'S':
                    ctxType = 'Species'
                elif c[0] == 'C':
                    ctxType = 'Cell'
                elif c[0] == 'T':
                    ctxType = 'Tissue'
                else:
                    ctxType = 'UNKNOWN'

                assert c in tbAnnotations, "ERROR: %s in line %i not pressent as a TB annotations" % (c, eventIndex)
                ctxIx = tbAnnotations[c]

                ctxId = -1
                # import ipdb; ipdb.set_trace()

                for ix, x in enumerate(vocabulary):
                    if ctxIds[c].split(':')[-1] in x.upper():
                        ctxId = ix

                if ctxId == -1:
                    print "#WARNING %s not present in the dictonaries" % ctxIds[c].split(':')[-1]

                # update the states matrix with a 1
                if inject_annotations:
                    states[ctxIx, ctxId] = 1

                print 'Event\t%i\t%s\t%i\t%s' % (eventIndex, ctxType, ctxId, ctxIx)


    # Save the states matrix
    if inject_annotations:
        np.savetxt(statesPath, states, fmt="%i")



if __name__ == '__main__':
    # Only one file at a time
    main(sys.argv[1], sys.argv[2], sys.argv[3])
