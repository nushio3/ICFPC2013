#!/usr/bin/python
# -*- coding: utf-8 -*-
"""One line explanation of generator.py.

More explanations of generator.py."""

import json
import math
import random
import sys
import urllib2
from argparse import ArgumentParser

# return an integer array a where len(a) == n, sum(a) == s and each element >= 1
def distributeValue(s, n):
    q = [0] * (n - 1) + [1] * (s - n)
    random.shuffle(q)
    result = []
    accum = 1
    for x in q:
        if x == 1:
            accum += 1
        else:
            result.append(accum)
            accum = 1
    result.append(accum)
    return result

# Generate an expression of size n, with variable v, new variable uses prefix.
# Generates fold only when foldable == True.
def GenExpression(n, v, prefix, foldable):
    op1 = ["not", "shl1", "shr1", "shr4", "shr16"]
    op2 = ["and", "or", "xor", "plus" ]
    if n == 1:
        return random.choice(['0', '1'] + v)
    if n >= 5 and random.uniform(0, 1) < 0.25 and foldable:
        var1 = prefix + '0'
        var2 = prefix + '1'
        s1, s2, s3 = distributeValue(n - 2, 3)
        return '(fold %s %s (lambda (%s %s) %s))' % (
            GenExpression(s1, v, prefix + 'x', False),
            GenExpression(s2, v, prefix + 'y', False),
            var1,
            var2,
            GenExpression(s3, v + [var1, var2], prefix + 'z', False))
    if n >= 4 and random.uniform(0, 1) < 0.333:
        s1, s2, s3 = distributeValue(n - 1, 3)
        # Bug: generates more than one fold.
        return '(if0 %s %s %s)' % (
            GenExpression(s1, v, prefix + 'x', foldable),
            GenExpression(s2, v, prefix + 'y', foldable),
            GenExpression(s3, v, prefix + 'z', foldable))
    if n >= 3 and random.uniform(0, 1) < 0.5:
        s1, s2 = distributeValue(n - 1, 2)
        # Bug: generates more than one fold.
        return '(%s %s %s)' % (
            random.choice(op2),
            GenExpression(s1, v, prefix + 'u', foldable),
            GenExpression(s2, v, prefix + 'v', foldable))
    return '(%s %s)' % (random.choice(op1),
                        GenExpression(n - 1, v, prefix, foldable))

# Generates a program of size n.
def GenProgram(n):
    assert n >= 3
    return '(lambda (a) %s)' % GenExpression(n - 1, ['a'], 'x', True)


# Generate dataset of size n.
def GenData(n):
    normal = range(16)
    twox = [2**i for i in xrange(64)]
    uniform = [random.randrange(2**64) for p in xrange(64)]
    loguni = [long(math.exp(random.uniform(0, math.log(2)*64)))
              for q in xrange(64)]
    unloguni = [2**64 + ~long(math.exp(random.uniform(0, math.log(2)*64)))
                for q in xrange(64)]
    combined = normal + twox + uniform + loguni + unloguni
    uniqified = list(set(combined))
    random.shuffle(uniqified)
    return sorted(uniqified[:n])


# Send program and data to the eval server.
def RunEval(program, data):
    url = ('http://icfpc2013.cloudapp.net/eval?auth=%svpsH1H' %
           '0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0Kj')
    query = {'program': program, 'arguments': ['0x%016X' % d for d in data]}
    q_s = json.dumps(query)
    #sys.stdout.write('req: %s\n' % q_s)
    #sys.stdout.flush()
    urlin = urllib2.urlopen(url, q_s)
    a_s = urlin.read()
    urlin.close()
    #sys.stdout.write('ans: %s\n' % a_s)
    #sys.stdout.flush()
    answer = json.loads(a_s)
    assert answer['status'] == 'ok', 'q: %s, a: %s' % (program, answer['message'])
    return answer['outputs']

# Output results.
def OutputResult(p, data, answers):
    sys.stdout.write('%s\n' % p)
    for d, a in zip(data, answers):
        sys.stdout.write('0x%016X %s\n' % (d, a))

def main(argv):
    # opts = ParseArgs()
    p = GenProgram(30)
    d = GenData(256)
    a = RunEval(p, d)
    OutputResult(p, d, a)

if __name__ == '__main__':
    main(sys.argv)
