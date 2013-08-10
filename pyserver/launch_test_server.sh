#!/bin/bash
# Launch the django server in test environment.
export ICFPC2013AUTOLOAD=''
ICFPC2013PROBLEMS=~/myproblems.json python manage.py runserver 0.0.0.0:8987
