""" Signal tests"""
import numpy as np
import pbench
import timeit
from fbench import fbench

def run(test_functions, N, runs=1000):
    results = []
    global rank

    titlefmt = '{:<40s} {:6s} {:>15} {:>15}'
    linefmt = '{:<40s} {:6d} {:>15.9f} {:>15.9f}'

    print(titlefmt.format('Test function', 'MatRnk', 'Total time', 'Time per run'))
    for n in N:
        rank = n
        for t in test_functions:
            r = min(timeit.Timer(t , globals=globals()).repeat(repeat=5, number=runs))
            print(linefmt.format(t, rank, r, r/runs))
            results.append((t, n, r, r/runs))

    return results
