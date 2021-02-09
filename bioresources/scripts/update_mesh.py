import os
import re
import csv
import gzip
import xml.etree.ElementTree as ET
from urllib.request import urlretrieve


def _get_term_names(record, name):
    # We then need to look for additional terms related to the
    # preferred concept to get additional names
    concepts = record.findall('ConceptList/Concept')
    all_term_names = []
    for concept in concepts:
        # We only look at the preferred concept here
        if concept.attrib['PreferredConceptYN'] == 'Y':
            terms = concept.findall('TermList/Term')
            for term in terms:
                term_name = term.find('String').text
                if term_name != name:
                    all_term_names.append(term_name)
    return all_term_names


def get_mesh_names(et):
    names = {}
    for record in et.iterfind('DescriptorRecord'):
        # We first get the ID and the name
        uid = record.find('DescriptorUI').text
        # Remove entries that we classify as bioprocesses
        if uid in bioprocess_filter:
            continue
        tree_numbers = record.findall('TreeNumberList/TreeNumber')
        # Diseases are in the C subtree
        if not any(t.text[0] == 'C' for t in tree_numbers):
            continue
        name = record.find('DescriptorName/String').text
        synonyms = _get_term_names(record, name)
        names[uid] = [name] + synonyms
    # Add some manual mappings
    for synonym, uid in manual_add:
        names[uid].append(synonym)
    return names


def entries_from_names(names):
    entries = []
    for uid, synonyms in names.items():
        for synonym in synonyms:
            entries.append((synonym, uid))
    print('Got a total of %d entries' % len(entries))
    return entries


def load_mesh_resource_file():
    url = 'ftp://nlmpubs.nlm.nih.gov/online/mesh/2019/xmlmesh/desc2019.gz'
    desc_path = os.path.join(here, 'mesh_desc2019.gz')
    if not os.path.exists(desc_path):
        print('Download MeSH descriptors from %s' % url)
        urlretrieve(url, desc_path)
        print('Done downloading MeSH descriptors')
    # Process the XML and find descriptor records
    with gzip.open(desc_path) as desc_file:
        print('Parsing MeSH descriptors')
        et = ET.parse(desc_file)
    return et


# These IDs appear in bio_process.tsv and are better classified as
# biological processes (they are both under the C and the G MeSH tree).
bioprocess_filter = {'D043171', 'D042822'}

# These were in bio_process.tsv, are actually diseases, but these synonyms
# aren't provided by MeSH so we add them here
manual_add = [('infiltration', 'D009361'),
              ('invasion', 'D009361'),
              ('invasiveness', 'D009361'),
              ('I/R', 'D015427'),
              ('ischemia', 'D007511'),
              ('Ischemia-reperfusion', 'D015427'),
              ('tumorigenesis', 'D063646')]


if __name__ == '__main__':
    # Basic positioning
    here = os.path.dirname(os.path.abspath(__file__))
    kb_dir = os.path.join(here, os.pardir, 'src', 'main', 'resources', 'org',
                          'clulab', 'reach', 'kb')
    resource_fname = os.path.join(kb_dir, 'mesh-disease.tsv')

    et = load_mesh_resource_file()
    mesh_names = get_mesh_names(et)

    # We sort the entries first by the synonym but in a way that special
    # characters and capitalization are ignored, then sort by ID
    entries = entries_from_names(mesh_names)
    entries = sorted(entries, key=(lambda x:
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
