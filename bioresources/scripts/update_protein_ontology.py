import os
import re
import csv
import obonet
import pickle

from collections import Counter


def get_synonyms(syns_entry):
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


def character_filter(txt):
    cnt = Counter([c for c in txt])
    special_chars = {'(', ')', '[', ']', '{', '}', ','}
    num_special_chars = sum(cnt.get(c, 0) for c in special_chars)
    return num_special_chars < 6


def mod_filter(txt):
    return "MOD:" not in txt


def is_derived_from(pro_id, data):
    if not re.match(r'^PR:(\d+)$', pro_id):
        return False

    relationship = data.get('relationship', [])
    if any(re.match(r'derives\_from PR:', rel) for rel in relationship):
        return True
    return False


def isa_cleavage_product(pro_id, data, g):
    isa = data.get('is_a', [])
    for node_id in isa:
        if 'proteolytic cleavage product' in g.nodes[node_id]['name']:
            return True
    return False


def is_cleavage_and_modification(data):

    comments = data.get('comment')
    definition = data.get('def')

    if comments is None or definition is None:
        return False

    return re.match(r'Category\=modification', comments) and ('cleavage' in definition)


def accept_entry(name, synonym, pro_id, data):

    # Sanity check filters
    if not (length_filter(synonym) and character_filter(synonym) and \
            mod_filter(synonym)):
        return False

    # Remove entries like "YWHAB/ClvPrd", these are not useful
    # synonyms
    if re.match(r'^([^/]+)/(ClvPrd|UnMod|iso:\d+/UnMod)$', synonym):
        return False

    # Remove synonyms like UniProtKB:P0DTD1, 5325-5925
    if re.match(r'^UniProtKB:([^ ]+)', synonym):
        return False

    # Finds guaranteed protein cleavages from relationship tag
    if is_derived_from(pro_id, data):
        return True

    # Experimental Filter for finding additional cleavages
    if is_cleavage_and_modification(data):
        return True

    if isa_cleavage_product(pro_id, data, g):
        return True

    return False


def read_cached_url(url, cache_file):
    if not os.path.exists(cache_file):
        print('Loading %s' % url)
        g = obonet.read_obo(url)
        with open(cache_file, 'wb') as fh:
            pickle.dump(g, fh)
        return g
    else:
        print('Loading %s' % cache_file)
        with open(cache_file, 'rb') as fh:
            return pickle.load(fh)


if __name__ == '__main__':

    # Basic positioning
    here = os.path.dirname(os.path.abspath(__file__))
    kb_dir = os.path.join(here, os.pardir, 'src', 'main', 'resources', 'org',
                          'clulab', 'reach', 'kb')
    resource_fname = os.path.join(kb_dir, 'protein-ontology-fragments.tsv')

    # Download Protein Ontology resource file
    url = 'https://proconsortium.org/download/current/pro_reasoned.obo'
    g = read_cached_url(url, 'pro_obo.pkl')
    allowed_synonyms = {'EXACT', 'RELATED'}
    entries = []

    for node, data in g.nodes(data=True):
        name = data['name']
        pro_id = node
        raw_synonyms = data.get('synonym', [])

        synonyms = get_synonyms(raw_synonyms)

        # Format of the output defined here
        entries += [(txt, pro_id) for txt in ([name] + synonyms)
                    if accept_entry(name, txt, pro_id, data)]

    # We sort the entries first by the synonym but in a way that special
    # characters and capitalization is ignored, then sort by ID
    entries = sorted(entries, key=(lambda x:
                                   (re.sub('[^A-Za-z0-9]', '', x[0]).lower(),
                                    x[1])))

    # Now dump the entries into an updated TSV file
    with open(resource_fname, 'w', newline='') as fh:
        writer = csv.writer(fh, delimiter="\t")
        for entry in entries:
            writer.writerow(entry)
