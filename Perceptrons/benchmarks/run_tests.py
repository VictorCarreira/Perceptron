""" Signal tests"""
import numpy as np

import plots
import tests
import pbench
from fbench import fbench


# Functions to test (from pbench and fbench source codes)
test_functions = [ 
        'pbench.functional_test_blas(rank)', 
        'fbench.functional_test(rank)'
]

# Only change from here if you know what you're doing
if __name__ == '__main__':

    # Test for break point 
    N = list(range(2,70,2))
    results1 = tests.run(test_functions, N)
    plot = plots.plotlines(results1, test_functions, N)
    plot.savefig('python_vs_fortran_breakpoint.png')

    N = (10, 50, 100, 1000)
    results2 = tests.run(test_functions, N)
    plot = plots.plotbars(results2, test_functions, N)
    plot.savefig('python_vs_fortran_by_rank.png')

