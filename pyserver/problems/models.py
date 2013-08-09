from django.db import models

import json
import os
import sys

def loadModelFromFile(filename):
    fin = open(filename, 'rb')
    result = json.load(fin)
    fin.close()
    return result

def loadModel():
    return loadModelFromFile(os.environ.get('ICFPC2013PROBLEMS',
                                            '../myproblems.json'))
