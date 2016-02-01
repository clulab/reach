''' Creates an HTML file with reach's and manual annotations for context display and analysis '''

import sys
import shutil
from glob import glob
from collections import defaultdict

css = 'context.css'

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

	return ret

def buildSentence(sentence, intervals, intervalNames):
	ret = ''

	for ix, word in enumerate(sentence):
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
					ret += "<span class='%s'>%s" % (name ,l)

		ret += '%s ' % word

		for its, name in zip(intervals, intervalNames):
			ends = [it[1] for it in its]
			for e in ends:
				if ix == e:
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

def main(dir, types):

	paths = glob('%s/*ctx*' % dir)
	docs = {p.split('.')[0] for p in paths}
	for doc in docs:
		sentences = readSentences('%s.ctxSentences' % doc)
		dicts = []
		for name in types:
			dicts.append(readIntervals('%s.%s' % (doc, name)))

		html = buildMarkup(doc, sentences, dicts, types)

		with open('%s.html' % doc, 'w') as f:
			f.write(html)


	shutil.copyfile(css, '%s/%s' % (dir, css))

if __name__ == '__main__':
	dir = sys.argv[1]
	dictNames = ['ctxEvents', 'ctxMentions', 'ctxAnnCtx', 'ctxAnnEvt']

	main(dir, dictNames)
