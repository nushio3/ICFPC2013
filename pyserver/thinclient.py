#!/usr/bin/python
# -*- coding: utf-8 -*-
"""One line explanation of thinclient.py.

More explanations of thinclient.py."""

from argparse import ArgumentParser
import urllib2
import sys


def Parse():
    parser = ArgumentParser(description='Thin post client.')
    parser.add_argument('--url')
    parser.add_argument('--body')
    return parser.parse_args()


def Post(url, body):
    urlin = urllib2.urlopen(url, body)
    res = urlin.read()
    urlin.close()
    return res


def main(argv):
    flags = Parse()
    try:
        sys.stdout.write('%s\n' % Post(flags.url, flags.body))
    except urllib2.HTTPError, e:
        sys.stderr.write('Error: %s\n' % str(e))
    return

if __name__ == '__main__':
    main(sys.argv)
