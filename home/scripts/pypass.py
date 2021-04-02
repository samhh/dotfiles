#! /usr/bin/env python2

from subprocess import check_output

def get_pass(path):
    return check_output("pass show " + path, shell=True).splitlines()[0]

