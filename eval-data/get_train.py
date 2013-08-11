#!/usr/bin/python
# -*- coding: utf-8 -*-
"""One line explanation of get_bonus.py.

More explanations of get_bonus.py."""

from argparse import ArgumentParser
import json
import sys
import time
import urllib2

def Parse():
    parser = ArgumentParser(description='Collect training programs.')
    parser.add_argument('--size', type=int, default=42)
    parser.add_argument('--previous_input')
    parser.add_argument('-n', type=int, default=1)
    return parser.parse_args()


def GetTraining(size):
    url = ('http://icfpc2013.cloudapp.net/train?auth=%svpsH1H' %
           '0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0Kj')
    
    query = {'size': size}
    query_string = json.dumps(query)
    urlin = urllib2.urlopen(url, query_string)
    a_string = urlin.read()
    urlin.close()
    answer = json.loads(a_string)
    return answer


def main(argv):
    flags = Parse()
    result = []
    if flags.previous_input:
        fin = open(flags.previous_input)
        result = json.load(fin)
        fin.close()
    for i in xrange(flags.n):
        result.append(GetTraining(flags.size))
        time.sleep(10)
    sys.stdout.write(json.dumps(result))


if __name__ == '__main__':
    main(sys.argv)
