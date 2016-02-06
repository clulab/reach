''' Creates an HTML file with reach's and manual annotations for context display and analysis '''

import sys
import shutil, os, operator
from glob import glob
from collections import defaultdict

css = 'context.css'

def contains(a, b):
	if a[0] <= b[0] and a[1] >= b[1]:
		return True
	else:
		return False

def overlaps(a, b):
	if a[0] <= b[0] and a[1] >= b[0] and a[1] < b[1]:
		return True
	else:
		return False

def merge(a, b):
	return (a[0], b[1], a[2])

def readSentences(path):
	ret =[]
	with open(path) as f:
		for line in f:
			ret.append(line[:-1].split())

	return ret

def readIntervals(path):
	ret = defaultdict(list)
	with open(path) as f:
		for line in f:
			line = line[:-1]
			tokens = line.split(' ', 1)

			key = tokens[0]

			tokens = tokens[1].split('\t')
			if len(tokens) == 2:
				contents, labels = tokens
			else:
				contents, labels = tokens[0], ''

			key = int(key)

			for it in contents.split():
				if it != '':
					it = it.split('-')
					start, end = int(it[0]), int(it[1])
					ret[key].append((start, end, labels))



	# Remove duplicate intervals and merge intervals
	for key in ret:
		s = set(ret[key])
		# Sort them
		s = sorted(list(s), key=operator.itemgetter(1, 2))

		merged = []
		if len(s) > 1:
			prev = s[0]
			for curr in s[1:]:
				if contains(prev, curr):
					merged.append(prev)
				elif contains(curr, prev):
					merged.append(curr)
				elif overlaps(prev, curr):
					merged.append(merge(prev, curr))
				else:
					merged.append(prev)
					merged.append(curr)

				prev = curr
		else:
			merged = s

		ret[key] = list(set(merged))




	return ret

def buildSentence(sentence, intervals, intervalNames):
	ret = ''

	for ix, word in enumerate(sentence):
		iOpen = False
		for its, name in zip(intervals, intervalNames):
			labels = [it[2] for it in its]
			# Generate the string for the labels
			x = []
			for l in labels:
				if l != '':
					x.append('[%s] ' % l)
				else:
					x.append('')

			labels = x

			starts = [it[0] for it in its]
			for s, l in zip(starts, labels):
				if ix == s: # It's a start of an annotation
					iOpen = True
					ret += "<span class='%s'>%s" % (name ,l)

		ret += '%s ' % word

		for its, name in zip(intervals, intervalNames):
			ends = [it[1] for it in its]
			for e in ends:
				if ix == e:
					iOpen = False
					ret += '</span>'

	if iOpen:
		ret += '</span>'
	ret += '</br>'

	return ret

def buildContents(sentences, intervalDicts, intervalNames):
	lines = []

	for ix, sentence in enumerate(sentences):
		lines.append('<b><i>%i:</i></b> %s' % (ix, buildSentence(sentence, [d[ix] for d in intervalDicts],intervalNames)))

	return '\n'.join(lines)

def buildMarkup(title, sentences, intervalDicts, intervalNames):
	html = """<html>
	<head>
		<link rel="stylesheet" type="text/css" href="%s">
		<title>%s</title>
	</head>
	<body>
		%s
	</body>
</html>""" % (css, title, buildContents(sentences, intervalDicts, intervalNames))

	return html

# def main(dir, types):
#
# 	paths = glob('%s/*ctx*' % dir)
# 	docs = {p.split('.')[0] for p in paths}
# 	for doc in docs:
# 		sentences = readSentences('%s.ctxSentences' % doc)
# 		dicts = []
# 		for name in types:
# 			dicts.append(readIntervals('%s.%s' % (doc, name)))
#
# 		html = buildMarkup(doc, sentences, dicts, types)
#
# 		with open('%s.html' % doc, 'w') as f:
# 			f.write(html)
#
#
# 	shutil.copyfile(css, '%s/%s' % (dir, css))

def main(dir, types):

	sentences = readSentences(os.path.join(dir, 'sentences.txt'))
	dicts = []
	dicts.append(readIntervals(os.path.join(dir, 'event_intervals.txt')))
	dicts.append(readIntervals(os.path.join(dir, 'mention_intervals.txt')))
	dicts.append(readIntervals(os.path.join(dir, 'manual_context_intervals.txt')))
	dicts.append(readIntervals(os.path.join(dir, 'manual_event_intervals.txt')))

	html = buildMarkup("index", sentences, dicts, types)

	with open('%s.html' % "index", 'w') as f:
		f.write(html)


	shutil.copyfile(css, '%s/%s' % (dir, css))

if __name__ == '__main__':
	dir = sys.argv[1]
	dictNames = ['ctxEvents', 'ctxMentions', 'ctxAnnCtx', 'ctxAnnEvt']
	#dictNames = ['ctxEvents', 'ctxAnnCtx', 'ctxAnnEvt']

	main(dir, dictNames)
