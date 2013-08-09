from django.http import HttpResponse
from django.shortcuts import render, get_object_or_404
from django.template import RequestContext, loader

import problems.models

# Create your views here.

def byId(p):
    return p['id']

def bySize(p):
    return (p['size'], ','.join(p['operators']))

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
    key = request.GET.get('key', 'id')
    key_list = {'id': byId, 'size': bySize}
    all_problems.sort(key=key_list[key])

    under_solve_problems = filter(lambda p: isUnderSolve(p), all_problems)
    solved_problems = filter(lambda p: isSolved(p), all_problems)
    dead_problems = filter(lambda p: isDead(p), all_problems)
    untouched_problems = filter(lambda p: isUntouched(p), all_problems)
    context = {
        'under_solve_problems': under_solve_problems,
        'solved_problems': solved_problems,
        'dead_problems': dead_problems,
        'untouched_problems': untouched_problems
        }
    return render(request, 'problems/list.html', context)
