import os
import requests

base_url = ('https://raw.githubusercontent.com/sorgerlab/famplex/master/'
            'export/')
famplex_groundings = 'famplex_groundings.tsv'


def get_real_lines(fname):
    with open(fname, 'r') as fh:
        lines = [l.strip() for l in fh.readlines() if l.strip() and
                 not l.startswith('#')]
    lines = [l.split('\t') for l in lines]
    return lines


def get_other_strings(fname):
    kb_files = [row[0] + '.tsv' for row in get_real_lines(fname)]
    strings = []
    for kb_fname in kb_files:
        # We skip famplex here to avoid redundancy
        if kb_fname == 'famplex.tsv':
            continue
        strings += [row[0] for row in
                    get_real_lines(os.path.join(kb_dir, kb_fname))]
    return set(strings)


def get_overrides(overrides_rows, other_strings, famplex_only=True):
    overrides = []
    for txt, db_id, db_ns, type in overrides_rows:
        # In famplex_only mode, skip and non-fplx groundings
        if famplex_only and db_ns != 'fplx':
            continue
        # If this is not an actual override, skip it
        if txt not in other_strings:
            continue
        overrides.append((txt, db_id, '', db_ns, type))
    return overrides


def extend_overrides(override_fname, overrides):
    with open(override_fname, 'r') as fh:
        lines = [l.strip() for l in fh.readlines()]
    try:
        idx = lines.index('# FamPlex overrides')
    except ValueError:
        idx = None
    lines_out = lines[:idx+2] if idx else lines
    # Add comment block if it is not already there
    lines_out += [] if idx else ['#', '# FamPlex overrides', '#']
    lines_out += ['\t'.join(l) for l in overrides]
    with open(override_fname, 'w') as fh:
        for line in lines_out:
            fh.write('%s\n' % line)


if __name__ == '__main__':
    # Basic positioning of folders
    here = os.path.dirname(os.path.abspath(__file__))
    kb_dir = os.path.join(here, os.pardir, 'src', 'main', 'resources', 'org',
                          'clulab', 'reach', 'kb')
    groundings_fname = os.path.join(kb_dir, 'famplex.tsv')
    override_fname = os.path.join(kb_dir, 'NER-Grounding-Override.tsv')

    groundings_rows = [line.strip().split('\t') for line in
                       requests.get(base_url +
                                    famplex_groundings).text.split('\n')]

    # Download and write to groundings file
    with open(groundings_fname, 'w') as fh:
        for line in groundings_rows:
            if line[2] == 'fplx':
                fh.write('%s\t%s\n' % (line[0], line[1]))

    # Now get all the "other" strings so we can figure out what to add
    # to the overrides file
    other_strings = get_other_strings(os.path.join(here, os.pardir,
                                      'ner_kb.config'))

    overrides = get_overrides(groundings_rows, other_strings,
                              famplex_only=True)

    extend_overrides(override_fname, overrides)
