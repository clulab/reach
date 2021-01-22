import re
import os
import csv
import gzip
import pickle
import obonet


extra_synonyms = {
    # CD4-positive, alpha-beta T cell
    # This is actually a slightly narrower category but is a "close enough"
    # match for these synonyms. We then override these in the overrides
    # file to exact matches in MeSH.
    'CL:CL:0000624': [
        'CD4+', 'CD4 +', 'CD4+ T cell',
        'CD4+ T cells', 'CD4 + T cells', 'CD4 þ T cells', 'CD4 ϩ T cells',
        'CD4 T cells'
    ],
    'CL:0000492': [
        'CD4+ T helper', 'CD4+ T helper cells'
    ],
    'CL:0000625': [
        'CD8+', 'CD8 +', 'CD8+ T cell',
        'CD8+ T cells', 'CD8 + T cells', 'CD8 þ T cells', 'CD8 ϩ T cells',
        'CD8 T cells'
    ],
}


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


def get_synonyms(syns_entry):
    synonyms = []
    for synonym in syns_entry:
        match = re.match(r'^\"(.+)\" (EXACT|RELATED|NARROW|BROAD)',
                         synonym)
        syn, status = match.groups()
        if status in allowed_synonyms:
            synonyms.append(syn)
    return synonyms


def pluralize(txt):
    if txt.endswith(('s', 'x', 'o', 'ch', 'sh')):
        return txt + 'es'
    elif txt.endswith('y'):
        return txt[:-1] + 'ies'
    else:
        return txt + 's'


def accept_entry(name, txt, cl_id, data):
    # Some synonyms are description sentences that we skip
    if txt.endswith('.'):
        return False
    return True


if __name__ == '__main__':
    # Basic positioning
    here = os.path.dirname(os.path.abspath(__file__))
    kb_dir = os.path.join(here, os.pardir, 'src', 'main', 'resources', 'org',
                          'clulab', 'reach', 'kb')
    resource_fname = os.path.join(kb_dir, 'CellOntology.tsv')

    # Download Protein Ontology resource file
    url = 'http://purl.obolibrary.org/obo/cl.obo'
    g = read_cached_url(url, 'cl_obo.pkl')
    allowed_synonyms = {'EXACT', 'RELATED'}
    entries = []

    for node, data in g.nodes(data=True):
        if not node.startswith('CL:'):
            continue
        name = data['name']
        cl_id = node
        raw_synonyms = data.get('synonym', [])

        synonyms = get_synonyms(raw_synonyms)

        pluralized = [pluralize(n) for n in [name] + synonyms]

        # Format of the output defined here
        entries += [(txt, cl_id) for txt in ([name] + synonyms + pluralized)
                    if accept_entry(name, txt, cl_id, data)]

    # Add extra synonyms
    for cl_id, synonyms in extra_synonyms.items():
        entries += [(txt, cl_id) for txt in synonyms]

    # We sort the entries first by the synonym but in a way that special
    # characters and capitalization is ignored, then sort by ID
    entries = sorted(set(entries))

    # Now dump the entries into an updated TSV file
    with open(resource_fname, 'w', newline='') as fh:
        writer = csv.writer(fh, delimiter="\t", lineterminator='\n')
        for entry in entries:
            writer.writerow(entry)
    # And then into a GZ file
    with open(resource_fname, 'rb') as f1, \
            gzip.open(resource_fname + '.gz', 'wb') as f2:
        f2.writelines(f1)
