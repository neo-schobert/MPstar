
# C

import numpy as np


# 1 On supprime insère n , i puis on supprime i après le h et le dernier n.



# 2 Oui car la sous-structure obéit aux mêmes règles rigoureusement que la structure elle même. On passe de l'un à l'autre par une simple formule utilisant strictement les éléments de la sous-structure.


# 3

L = ["|   | C | H | i | E | N |","| N | 1 | 1 | 1 | 1 | 0 |","| I | 1 | 1 | 0 | 1 | 1 |", "| C | 0 | 1 | 1 | 1 | 1 |","| H | 1 | 0 | 1 | 1 | 1 |", "| E | 1 | 1 | 1 | 0 | 1 |"]

for k in L:
    print(k)
    
Lf = ["|   | C | H | i | E | N |","| N | 1 | 2 | 3 | 4 | 4 |","| I | 2 | 2 | 2 | 3 | 4 |", "| C | 2 | 3 | 3 | 3 | 4 |","| H | 3 | 2 | 3 | 4 | 4 |", "| E | 4 | 3 | 3 | 3 | 4 |"]
    
for k in Lf:
    print(k)
    
    
# 4 
def distance(a,b):
    n = len(a)
    m = len(b)
    if n == 0:
        return m
    elif m == 0:
        return n
    else:
        tab = np.zeros((n+1, m+1))
        print(tab)
        for k in range(m+1):
            tab[0][k] = k
        for k in range(n+1):
            tab[k][0] = k
        print(tab)
        cout = np.zeros((n,m))
        for i in range(n):
            for j in range(m):
                cout[i][j] = int(a[i] != b[j])
        print(cout)
        for i in range(1,n+1):
            for j in range(1,m+1):
                tab[i][j] = min(tab[i-1][j] + 1,tab[i][j-1] + 1, tab[i-1][j-1] + cout[i-1][j-1])
        print(tab)
        return tab[n][m]
        
        
    
print(distance("niche","chien"))





# 5

def distance_rec(a,b):
    n = len(a)
    m = len(b)
    if n == 0:
        return m
    elif m == 0:
        return n
    else:
        def aux(i,j):
            if i == 0:
                return j
            elif j == 0:
                return i
            else:
                return min(aux(i-1,j) + 1,aux(i,j-1) + 1, aux(i-1,j-1) + int(a[i-1] != b[j-1]))
        return aux(n,m) 


print(distance_rec("chien","niche"))





# D

# 1 L(a,b) vaut ici 4, avec la sous-chaîne AT et la sous chaîne GC. 



# 2

def is_ss(ch,sch):
    ind = 0
    for k in sch:
        cond = False
        cond2 = True
        while ind < len(ch) and cond2:
            cond = True
            if ch[ind] == k:
                cond2 = False
            ind+=1    
        if not cond:
            return False
    return True




# 3

def is_common_ss(a,b,sch):
    return is_ss(a,sch) and is_ss(b,sch)




# 4 On remarque que on peut formuler le probleme avec une suite l_i_j vérifiant la relation de récurrence :

# l_i_j = 0 si i = 0 ou j = 0
# l_i_j = 1 + l_{i-1}_{j-1} si a_i = b_j
# max(l_{i-1}_j, l_i_{j-1}) sinon

# Sa structure est donc optimale.


# 5
# FLEMME




# 6


def l(a,b):
    i=len(a)
    j=len(b)
    if i == 0 or j == 0:
        return 0
    elif a[i-1] == b[j-1]:
        return 1 + l(a[0:i-1],b[0:j-1])
    else:
        return max(l(a[0:i-1],b),l(a,b[0:j-1]))


# pas une version mémoisation (IL FAUT STOCKER LES DONNEES)

# 7

# FLEMME


# E

# 1









