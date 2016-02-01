''' Parses curated annotation files and outputs interval files for makeHtml '''
import csv
import sys
from alignment.sequence import Sequence
from alignment.vocabulary import Vocabulary
from alignment.sequencealigner import SimpleScoring, StrictGlobalSequenceAligner

def readTSV(path):
	lines = []
	with open(path) as f:
		reader = csv.reader(f, delimiter='\t')
		for row in reader:
			ctx, tbound, contextId, evt, text = row[2].lower().strip(), row[3].lower().strip(), row[4].lower().strip(), row[5].lower().strip(), row[6].lower().strip()
			lines.append((ctx, tbound, evt, contextId, text))

	return lines

def makeCtxIntervals(lines):
	ret = []
	for ix, row in enumerate(lines):
		intervals = makeCtxInterval(row)
		for interval in [("%i-%i" % (i[0], i[1]), i[2].upper())  for i in intervals]:
			ret.append("%i %s\t%s" % (ix, interval[0], interval[1]))

	return ret

def makeEventIntervals(lines):
	ret = []
	for ix, row in enumerate(lines):
		intervals = makeEventInterval(row)
		for interval in [("%i-%i" % (i[0], i[1]), i[2].upper())  for i in intervals]:
			ret.append("%i %s\t%s" % (ix, interval[0], interval[1]))

	return ret

def makeEventInterval(row):
	ret = []
	_, tbound, evt, ctxId, text = row

	evt = evt.split('|') if evt != '' else []
	words = text.split()
	ctxId = ctxId.strip()

	# Alignment algorithm
	sentenceSeq = Sequence(words)
	evtSequences = [Sequence(e.split()) for e in evt]

	# Build vocabulary
	voc = Vocabulary()
	sentenceEncoded = voc.encodeSequence(sentenceSeq)

	# Do the actual alignment
	scoring = SimpleScoring(2, -1)
	aligner = StrictGlobalSequenceAligner(scoring, -2)

	for es in evtSequences:
		esEncoded = voc.encodeSequence(es)
		score, encodeds = aligner.align(sentenceEncoded, esEncoded, backtrace=True)
		# Pull the intervals
		start = end = None
		for ix, pair in enumerate(encodeds[0]):
			if pair[0] == pair[1]:
				#assert pair[0] != 0, "Something funky with alignments of ix: %i" % ix
				# This is a match
				if start is None:
					start = ix

				# This is the last token of the sentence, hence of the annotation
				if ix == len(esEncoded) - 1:
					end = ix

			elif pair[0] == 0 or pair[1] == 0:
				# Get the rest of the annotation codes
				rest = [x[1] for x in encodeds[0]][ix:]

				if sum(rest) == 0:
					#This was the last word of the annotation
					end = ix
		if start is not None and end is not None:
			ret.append((start, end, ctxId))
		else:
			print "Problem finding %s in '%s'" % (' '.join(es), text)


	# TODO: Matxh labels with intervals

	return ret

def makeEventLineIntervals(lines):
	ret = []

	for ix, row in enumerate(lines):
		_, tbound, __, text = row
		# This is just while we get the event annotations
		if 'e' in tbound:
			words = text.split()
			ret.append('%i %i-%i' % (ix, 0, len(words)-1))
		else:
			ret.append('%i ' % ix)

	return ret



def makeCtxInterval(row):
	ret = []
	ctx, tbound, __, ___, text = row

	ctx = ctx.split(',') if ctx != '' else []
	tbound = filter(lambda x: x[0] != 'e', tbound.split(',')) if tbound != '' else []

	#assert len(tbound) == len(ctx), "Missmatching number of annotations and TB labels"

	words = text.split()

	offset = 0
	for ct, tb in zip(ctx, tbound):
		# remove cell/cells
		ct = ct.replace('cells', '')
		ct = ct.replace('cell', '')
		ct.strip()

		start = end = None

		# Split the words
		c = ct.split()
		if len(c) > 0:
			first, last = c[0], c[-1]

			ix = 0


			# Find the location on text
			for ix, word in enumerate(words):
				if word == first:
					start = offset + ix
					offset += ix
					break

			words = words[ix:]

			for ix, word in enumerate(words):
				if word == last:
					end = offset + ix
					offset += ix
					break

			words = words[ix:]

		if start is not None and end is not None:
			ret.append((start, end, tb))
		elif len(c) > 0:
			print "Problem finding %s in '%s'" % (' '.join(c), text)

	return ret

def main(paths):
	for path in paths:
		tsv = readTSV(path)
		ctx = makeCtxIntervals(tsv)
		# events = makeEventLineIntervals(tsv)
		events = makeEventIntervals(tsv)

		doc = path.split('.')[0]

		with open('%s.ctxAnnCtx' % doc, 'w') as f, open('%s.ctxAnnEvt' % doc, 'w') as g:
			for c in ctx:
				f.write('%s\n' % c)

			for e in events:
				g.write('%s\n' % e)

if __name__ == '__main__':
	main(sys.argv[1:])
