"""This is a one-off script to sort out redundancies between existing
overrides and overrides from FamPlex. It handles the following cases:
1. NER-Grounding-Override.tsv has a mapping to PF where we know that the given
PF ID maps to a FPLX ID. In this case, if the
same entity text is mapped in FamPlex, we remove the mapping to PF. If the
save entity text doesn't exist in FamPlex, we add it to FamPlex to make
sure we don't lose that override.
2. NER-Grounding-Override.tsv has a mapping whose entity text matches and
entity text in FPLX. In this case, we make sure that the grounding in FamPlex
matches the grounding in NER-Grounding-Override.tsv. If not, we manually
resolve it to keep a single valid override.
"""
import os
import requests

base_url = 'https://raw.githubusercontent.com/sorgerlab/famplex/master/'


def get_non_fplx_overrides():
    overrides = {}
    with open(override_fname, 'r') as fh:
        lines = [l.strip().split('\t') for l in fh.readlines()]
        for line in lines:
            # Skip comment lines
            if line[0][0] == '#':
                continue
            # Skip FPLX lines
            if line[3] == 'fplx':
                continue
            if line[0] in overrides:
                print('%s is already being overriden' % line[0])
            overrides[line[0]] = line[1:]
    return overrides


def get_fplx_groundings():
    groundings_rows = \
        [line.strip().split('\t') for line in
         requests.get(base_url +
                      'export/famplex_groundings.tsv').text.split('\n')]
    groundings = {line[0]: line[1:] for line in groundings_rows}
    return groundings


def get_fplx_pf_mappings():
    equiv_rows = \
        [line.strip().split(',') for line in
         requests.get(base_url + 'equivalences.csv').text.split('\n')]
    equivs = {line[1]: line[2] for line in equiv_rows if line[0] == 'PF'}
    return equivs


if __name__ == '__main__':
    # Basic positioning of folders
    here = os.path.dirname(os.path.abspath(__file__))
    kb_dir = os.path.join(here, os.pardir, 'src', 'main', 'resources', 'org',
                          'clulab', 'reach', 'kb')
    override_fname = os.path.join(kb_dir, 'NER-Grounding-Override.tsv')
    non_fplx_overrides = get_non_fplx_overrides()
    fplx_groundings = get_fplx_groundings()
    pf_to_fplx_mappings = get_fplx_pf_mappings()

    add_to_fplx = []
    remove_override = []
    compare_override = []
    for txt, override in non_fplx_overrides.items():
        fplx_grounding = fplx_groundings.get(txt)
        if override[2] == 'pfam':
            fplx_mapping = pf_to_fplx_mappings.get(override[0])
            if fplx_mapping:
                if txt not in fplx_groundings:
                    add_to_fplx.append((txt, 'FPLX', fplx_mapping))
                else:
                    remove_override.append((txt, override, fplx_grounding))
        else:
            if fplx_grounding:
                # This is the case when it's a matching grounding
                if override[2] == fplx_grounding[1]:
                    if override[0] == fplx_grounding[0]:
                        continue

                compare_override.append((txt, override, fplx_grounding))

