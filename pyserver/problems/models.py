from django.db import models

import json
import sys

def loadModel(filename):
    fin = open(filename, 'rb')
    result = json.load(fin)
    fin.close()
    return result
