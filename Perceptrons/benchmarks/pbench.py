""" Signal tests"""

import numpy as np
from numba import jit
import timeit
import matplotlib.pyplot as plt
import scipy.linalg as la

def sgn(h):
    """Signal function"""
    return -1 if h < 0 else 1

def lincomb(A, B):
    """Linear combinator between two matrices"""
    return np.einsum('ji,ij->', A, B)

def lincombrav(A, B):
    return A.ravel().dot(B.ravel('F'))

def lincombblas(A,B):
    return la.blas.ddot(A.ravel(), B.ravel('F'))

def functional_test_ravel(n):
    """Functional tests (Victor experiment)"""

    w = 2*np.random.random(n**2).reshape(n,n)-1
    x = 2*np.random.random(n**2).reshape(n,n)-1

    return sgn(lincombrav(w, x))

def functional_test_einsum(n):
    """Functional tests (Victor experiment)"""

    w = 2*np.random.random(n**2).reshape(n,n)-1
    x = 2*np.random.random(n**2).reshape(n,n)-1

    return  sgn(lincomb(w, x))

def functional_test_blas(n):
    """Functional tests (Victor experiment)"""

    w = 2*np.random.random(n**2).reshape(n,n)-1
    x = 2*np.random.random(n**2).reshape(n,n)-1

    return  sgn(lincombblas(w, x))

@jit()
def functional_test_numbaein(n):
    """Functional tests (Victor experiment)"""

    w = 2*np.random.random(n**2).reshape(n,n)-1
    x = 2*np.random.random(n**2).reshape(n,n)-1

    return sgn(lincomb(w, x))


@jit()
def functional_test_numbarav(n):
    """Functional tests (Victor experiment)"""

    w = 2*np.random.random(n**2).reshape(n,n)-1
    x = 2*np.random.random(n**2).reshape(n,n)-1

    return sgn(lincomb(w, x))


