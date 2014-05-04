#!/usr/bin/env python3.3

__doc__ = '''Runs tests on all registered implementations. Uses the tester program from the EH repository.'''

import json
import os
import subprocess

SCHEME_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir))

def main():
    implementations = json.loads(open(os.path.join(SCHEME_ROOT, "common/implementations.json"), "r").read())

    os.chdir(os.path.join(SCHEME_ROOT, "test/"))
    os.environ["SCHEME_ROOT"] = SCHEME_ROOT

    for implementation in implementations:
        print("=== RUNNING TESTS FOR: {} ===".format(implementation['name']))
        subprocess.call(['tester', '-p', '{root}/{name}/{executable}'.format(root=SCHEME_ROOT, **implementation)], env=os.environ)

if __name__ == '__main__':
    main()
