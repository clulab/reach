import os
import csv
import requests

# Basic positioning of folders
here = os.path.dirname(os.path.abspath(__file__))
kb_dir = os.path.join(here, os.pardir, 'src', 'main', 'resources', 'org',
                      'clulab', 'reach', 'kb')


if __name__ == '__main__':
    url = 'https://www.uniprot.org/docs/subcell.txt'
    res = requests.get(url)
    res.raise_for_status()
    header, entry_block = res.text.split('_' * 75)
    entries = entry_block.split('//')
    synonyms = []
    for entry in entries:
        slid = None
        name = None
        syns = []
        lines = entry.split('\n')
        for line in lines:
            # We have to remove the last character
            # which is a period
            if line.startswith('ID'):
                name = line[5:].strip()[:-1]
            if line.startswith('AC'):
                slid = line[5:].strip()
            if line.startswith('SY'):
                # We have to remove the last character
                # which is a period
                syns = line[5:].strip()[:-1].split('; ')
        if slid:
            if name:
                synonyms.append((name, slid))
            if syns:
                synonyms.extend([(syn, slid) for syn in syns])
    # Deduplicate
    synonyms = list(set(synonyms))
    # Sort by synonym
    synonyms = sorted(synonyms, key=lambda x: x[0])
    # Add naive plural forms
    synonyms.extend([(syn + 's', slid) for syn, slid in synonyms
                     if not syn.endswith('s')])
    fname = os.path.join(kb_dir, 'uniprot-subcellular-locations.tsv')
    with open(fname, 'w') as fh:
        writer = csv.writer(fh, delimiter='\t')
        writer.writerows(synonyms)
