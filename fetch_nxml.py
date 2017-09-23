#!/usr/bin/env python
# -*- coding: utf-8 -*

try:
    # python 3.X
    from urllib.request import urlopen, urlretrieve
except:
    # python 2.7
    from urllib2 import urlopen
    from urllib import urlretrieve
import re
import argparse
import sys
import os

"""
usage: python fetch_nxml.py --pmcids PMC1234 PMC1235
"""
# pubmed documentation: http://www.ncbi.nlm.nih.gov/books/NBK25499/


class PubMedResources(object):

    @staticmethod
    def retrieve_nxml_abstract(pmid, outfile = None):
        """
        Retrieves nxml file of the abstract associated with the provided pmid
        """
        query = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id={}&rettype=abstract".format(pmid)
        nxml_file = outfile or "{}.nxml".format(pmid)
        urlretrieve(query, nxml_file)

    @staticmethod
    def retrieve_nxml_paper(pmcid, outfile = None):
        """
        Retrieves nxml file for the provided pmcid
        """
        query = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pmc&id={}".format(pmcid)
        nxml_file = outfile or "{}.nxml".format(pmcid)
        urlretrieve(query, nxml_file)


class PubMedEntry(object):

    def __init__(self, someid):
        self.convert_ids(someid)

    def convert_ids(self, someid):
        # id conversion api:  http://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
        url = "http://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids={}".format(someid)
        with urlopen(url) as response:
            html = response.read().decode("utf-8")
            self.pmcid = re.findall("(?<!version) pmcid=\"(.*?)\"", html)[0]
            self.pmid = re.findall("(?<!version) pmid=\"(.*?)\"", html)[0]


def parse_args():
    parser = argparse.ArgumentParser(description='Retrieve nxml from an PMCID')
    parser.add_argument('--pmcids', #"-i",
                        dest="pmcids",
                        nargs='+',
                        required=True,
                        help='a list of PMCIDs (delimited by whitespace)'
                        )
                        
    return parser.parse_args()

if __name__ == "__main__":
    args = parse_args()
    for pmcid in args.pmcids:
        outfile = "{}.nxml".format(pmcid)
        PubMedResources.retrieve_nxml_paper(pmcid, outfile)
        error_msg = "error=\"The following PMCID is not available: "
        if error_msg in open(outfile, 'r').read():
            os.remove(outfile)
            print("Unable to retrieve nxml for {}".format(pmcid))
        else:
            print("retrieved {}".format(outfile))
