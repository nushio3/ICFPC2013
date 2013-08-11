from django.db import models

import json
import os
import sys
import urllib2

def loadModelFromFile(filename):
    fin = open(filename, 'rb')
    result = json.load(fin)
    fin.close()
    return result

def GetModelTimestamp():
    return os.stat(GetModelFilename()).st_mtime

def GetModelFilename():
    return os.environ.get('ICFPC2013PROBLEMS', '../myproblems.json')

def GetTrainingFilename():
    return os.environ.get('ICFPC2013TRAINING', '../training.json')

def loadModel():
    return loadModelFromFile(GetModelFilename())

def LoadTraining():
    return loadModelFromFile(GetTrainingFilename())

AUTOLOAD_ENVIRONMENT = 'ICFPC2013AUTOLOAD'

def GetAutoLoadStatus():
    return os.environ.get(AUTOLOAD_ENVIRONMENT, '') == 'true'

def SetAutoLoadStatus(status):
    if status:
        str_status = 'true'
    else:
        str_status = ''
    os.environ[AUTOLOAD_ENVIRONMENT] = str_status


def MaybeAutoLoad():
    if GetAutoLoadStatus():
        ForceReload()


def ForceReload():
    """Sends request to the contest server to update myproblems.json."""
    url = 'http://icfpc2013.cloudapp.net/myproblems?auth=0017eB6c6r7IJcmlTb3v4kJdHXt1re22QaYgz0KjvpsH1H'
    reader = urllib2.urlopen(url)
    problems_string = reader.read()
    reader.close()
    try:
        fout = open(GetModelFilename(), mode='w')
        fout.write(problems_string)
        fout.close()
    except IOError:
        sys.stderr.write('failed to write to model file.')
    return
