"""This script updates bio_processes.tsv based on names and synonyms from GO.
It also incorporates old, presumably manually collected GO and MeSH entries
some of which provide synonyms that the official GO download doesn't.
This script therefore adds these, as long as they are not redundant.
"""
import os
import re
import csv
import gzip
import obonet
from collections import defaultdict


exclude_list = {'behavior', 'behaviour', 'breakdown', 'cis-autophosphorylation',
                'degradation', 'demethylation', 'dephosphorylation',
                'depurination', 'depyrimidination', 'desumoylation',
                'deubiquitination', 'excretion', 'flight', 'glycosylation',
                'growth', 'localisation', 'localization', 'memory',
                'methylation',
                'phosphorylation', 'polyubiquitin', 'prenylation', 'rhythm',
                'secretion', 'signaling', 'signalling', 'sumoylation',
                'transcription', 'transactivation', 'trans-autophosphorylation',
                'translation',
                }


def get_synonyms(syns_entry):
    """Return synonyms for a given entry."""
    synonyms = []
    for synonym in syns_entry:
        match = re.match(r'^\"(.+)\" (EXACT|RELATED|NARROW|BROAD)',
                         synonym)
        syn, status = match.groups()
        if status in allowed_synonyms:
            synonyms.append(syn)
    return synonyms


def length_filter(txt):
    # We filter out single-character names and names that are very long
    # and so are unlikely to be ever found in text
    return 2 <= len(txt) <= 50


def filter_exclude_list(txt):
    return txt not in exclude_list


def read_manual_entries():
    """Load the old manual entries."""
    fname = os.path.join(kb_dir, 'bio_process_manual.tsv')
    with open(fname, 'r') as fh:
        reader = csv.reader(fh, delimiter='\t')
        return [row for row in reader]


def get_entries_by_id(entries):
    entries_by_id = defaultdict(set)
    for txt, id, _, ns in entries:
        entries_by_id[id].add((txt, ns))
    return entries_by_id


if __name__ == '__main__':
    # Basic positioning
    here = os.path.dirname(os.path.abspath(__file__))
    kb_dir = os.path.join(here, os.pardir, 'src', 'main', 'resources', 'org',
                          'clulab', 'reach', 'kb')
    resource_fname = os.path.join(kb_dir, 'bio_process.tsv')

    # Download GO resource file
    url = 'http://purl.obolibrary.org/obo/go.obo'
    g = obonet.read_obo(url)
    allowed_synonyms = {'EXACT', 'RELATED'}
    entries = []
    for node, data in g.nodes(data=True):
        name = data['name']
        if data['namespace'] != 'biological_process':
            continue
        synonyms = get_synonyms(data.get('synonym', []))
        entries += [(txt, node, '', 'go') for txt in ([name] + synonyms)
                    if (length_filter(txt) and filter_exclude_list(txt))]

    # Here we sort out redundancies between old and new entries and add old
    # ones only if they are not redundant
    new_entries_by_id = get_entries_by_id(entries)
    manual_entries = read_manual_entries()
    manual_entries_by_id = get_entries_by_id(manual_entries)

    for id, txt_ns in manual_entries_by_id.items():
        for txt, ns in txt_ns:
            # Make sure we don't already have that synonym
            if ns == 'go' and (txt not in new_entries_by_id[id]):
                print('Adding %s: %s' % (txt, id))
                entries.append((txt, id, '', ns))
            # Make sure the same synonym isn't in GO if this is from MeSH
            elif ns == 'mesh' and (txt not in {e[0] for e in entries}):
                print('Adding %s: %s' % (txt, id))
                entries.append((txt, id, '', ns))

    # We sort the entries first by the synonym but in a way that special
    # characters and capitalization is ignored, then sort by ID
    entries = sorted(set(entries), key=(lambda x:
                                   (re.sub('[^A-Za-z0-9]', '', x[0]).lower(),
                                    x[1])))
    # Now dump the entries into an updated TSV file
    with open(resource_fname, 'w') as fh:
        writer = csv.writer(fh, delimiter='\t')
        for entry in entries:
            writer.writerow(entry)

    with open(resource_fname, 'rb') as f1, \
            gzip.open(resource_fname + '.gz', 'wb') as f2:
        f2.writelines(f1)