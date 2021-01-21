"""This script helps identify entries in PubChem.tsv that systematically
lead to incorrect groundings and should therefore be removed."""

import os
import re
from indra.databases import chebi_client

if __name__ == '__main__':
    # Basic positioning
    here = os.path.dirname(os.path.abspath(__file__))
    kb_dir = os.path.join(here, os.pardir, 'src', 'main', 'resources', 'org',
                          'clulab', 'reach', 'kb')
    resource_fname = os.path.join(kb_dir, 'PubChem.tsv')

    keep_rows = []
    with open(resource_fname, 'r') as fh:
        for row in fh.readlines():
            if '\t' not in row:
                continue
            txt, id = [x.strip() for x in row.split('\t')]
            if re.match(r'^[A-Z][A-Z]$', txt):
                chebi_id = chebi_client.get_chebi_id_from_pubchem(id)
                name = chebi_client.get_chebi_name_from_id(chebi_id)
                if name and  '-' in name and len(name) == 7:
                    continue
            keep_rows.append(row)
    with open(resource_fname, 'w') as fh:
        for row in keep_rows:
            fh.write(row)
