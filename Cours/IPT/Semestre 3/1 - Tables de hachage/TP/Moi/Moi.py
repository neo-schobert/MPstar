# A1

# La fonction gamma est injective car chaque caractère du string s est codée sur 1 octet et on créé un polynome 
# en 2^8 avec chaque coefficient égal à un caractère (dans l'ordre) de s.
# Ainsi, si gamma(s)= gamma(s'), la décomposition polynomiale impose s = s'.



# A2


from math import floor, sqrt
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt

# INITIALISATION

n=47057
alpha=(sqrt(5)-1)/2


# CODE

def gamma(s):
    sum = 0
    r = len(s)
    for k in range(r):
        sum += ord(s[k])*(2**8*k)
    return sum


def h_d(s,n):
    return gamma(s) % n

def h_alpha(s,n):
    return floor(n*(alpha*gamma(s) % 1))



# A3

file = open("/home/neo/Bureau/Education/Cours MP*/Cours/IPT/Semestre 3/1 - Tables de hachage/TP/english_words.txt","r")

lWords = [line for line in file]

# A4

# A5



# A6



def uniforme_test(h,tableSize):
    codes = []
    for k in lWords:
        codes.append(h(k,tableSize))
    plt.figure()
    plt.hist(codes,bins =5000)
    plt.show()
    return stats.kstest(np.array(codes), "uniform")


print(uniforme_test(h_d,n))
print(uniforme_test(h_alpha,n))




# B1

def import_csv():
    file2 = open("/home/neo/Bureau/Education/Cours MP*/Cours/IPT/Semestre 3/1 - Tables de hachage/TP/capitals.csv","r")
    lInit = [k for k in file2]
    lCap = []
    lPays = []
    for i in range (3,len(lInit)):
        if i%3==0:
            lPays.append(lInit[i])
        elif i%3 == 1:
            lCap.append(lInit[i])
    lFinal =[]
    for k in range(len(lCap)):
        lFinal.append((lPays[k],lCap[k]))
    return lFinal

# B2