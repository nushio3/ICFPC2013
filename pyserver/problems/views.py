from django.core.urlresolvers import reverse
from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render, get_object_or_404
from django.template import RequestContext, loader

import problems.models
import time

# Create your views here.

def byId(p):
    return p['id']

def bySize(p):
    return (p['size'], len(p['operators']), ','.join(p['operators']), p['id'])

def isSolved(p):
    return 'solved' in p

def getTimeLeft(p):
    return p.get('timeLeft', 300)

def isUnderSolve(p):
    return not isSolved(p) and 0 < getTimeLeft(p) < 300

def isDead(p):
    return not isSolved(p) and getTimeLeft(p) == 0

def isUntouched(p):
    return not isSolved(p) and getTimeLeft(p) == 300

def index(request):
    all_problems = problems.models.loadModel()
    key = request.GET.get('key', 'size')
    key_list = {'id': byId, 'size': bySize}
    all_problems.sort(key=key_list[key])

    under_solve_problems = filter(lambda p: isUnderSolve(p), all_problems)
    solved_problems = filter(lambda p: isSolved(p), all_problems)
    dead_problems = filter(lambda p: isDead(p), all_problems)
    untouched_problems = filter(lambda p: isUntouched(p), all_problems)
    last_modified_walltime = problems.models.GetModelTimestamp()
    last_modified_string = time.strftime('%Y-%m-%d %H:%M:%S',
                                         time.localtime(last_modified_walltime))
    autoload_status = problems.models.GetAutoLoadStatus()
    context = {
        'under_solve_problems': under_solve_problems,
        'solved_problems': solved_problems,
        'dead_problems': dead_problems,
        'untouched_problems': untouched_problems,
        'last_modified': last_modified_string,
        'autoload_status': autoload_status
        }
    return render(request, 'problems/list.html', context)


def reload(request):
    """Force refresh problem list."""
    problems.models.ForceReload()
    return HttpResponseRedirect(reverse('problems:index'))


def setreload(request, status):
    """Force refresh problem list."""
    problems.models.SetAutoLoadStatus(status == 'True')
    return HttpResponseRedirect(reverse('problems:index'))


def maybereload(request):
    problems.models.MaybeAutoLoad()
    return HttpResponseRedirect(reverse('problems:index'))
