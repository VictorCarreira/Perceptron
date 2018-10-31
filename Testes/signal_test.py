import numpy as np

def signal(h):
    return -1 if h < 0 else 1

def lincomb(A, B):

    AT = A.T

    dp = 0 
    for i in range(A.shape[0]):
        dp += AT[:,i].dot(B[i,:])

    return dp
        
    
w = np.array([
            [1, 2, 3],
            [43, 23, 35],
            [12, 23, 34]
    ], dtype='float')

xx = np.array([
        [ 4, 5, 6 ],
        [ 76, 76, 8],
        [ 45, 56, 67]
    ], dtype='float')
        

activator = signal
b = activator(lincomb(w, xx))

