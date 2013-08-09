from django.http import HttpResponse
from django.shortcuts import render, get_object_or_404
from django.template import RequestContext, loader

import problems.models

# Create your views here.

def byId(p):
    return p['id']

def bySize(p):
    return p['size']

def index(request):
    unsorted_problems = problems.models.loadModel('../myproblems.json')
    key = request.GET.get('key', 'id')
    key_list = {'id': byId, 'size': bySize}
    ps = sorted(unsorted_problems, key=key_list[key])
    context = {
        'problems': ps
        }
    return render(request, 'problems/list.html', context)
