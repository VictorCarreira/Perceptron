import numpy as np
import matplotlib.pyplot as plt


def plotbars(results, test_functions, N):
    Nsz = len(N)
    M = len(test_functions)

    fig, ax = plt.subplots()

    ind = np.arange(int(Nsz))
    width = 1/(M+1)

    p = []
    for n in range(M):
        g = [ w*1000 for (x,y,z,w) in results if x==test_functions[n]]
        p.append(ax.bar(ind+n*width, g, width, bottom=0))

    ax.legend([ l[0] for l in p ], test_functions)
    ax.set_xticks(ind-width/2+((M/2) * width))
    ax.set_xticklabels(np.array(N).astype(str))
    ax.set_xlabel('Rank of square random matrix')
    ax.set_ylabel('Average time(ms) per run')
    ax.set_yscale('log')

    return fig
