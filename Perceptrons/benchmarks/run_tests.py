""" Signal tests"""
import numpy as np

import plots
import tests
import pbench
from fbench import fbench


# Matrix rank to test
N = (10, 50, 100, 1000)

# Functions to test (from pbench and fbench source codes)
test_functions = [ 
        'pbench.functional_test_blas(rank)', 
        'fbench.functional_test(rank)'
]

# Only change from here if you know what you're doing
if __name__ == '__main__':
    results = tests.run(test_functions, N)
    plot = plots.plotbars(results, test_functions, N)
    plot.savefig('python_vs_fortran.png')
    plot.show()

