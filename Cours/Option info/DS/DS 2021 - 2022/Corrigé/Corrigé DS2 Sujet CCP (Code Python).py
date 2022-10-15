## DS2 Option Info 2021-2022 : CCP 2018

## III.2 : Codage par plages RLE

# 34. On utilise une liste de tuples (entier, chaîne de caractères).

# 35. On utilise deux boucles while imbriquées (mais on ne parcourt quand même le mot qu'une seule fois) : la première sert à nous stopper quand on a atteint la fin du mot, la deuxième lorsqu'on arrête une chaîne de lettres consécutives.

def RLE(mot):
    n = len(mot)
    L = []
    i = 0
    while i < n:
        lettre = mot[i]
        c = 1 #on compte les occurrences consécutives de la lettre
        j = i+1
        while j < n and mot[j] == lettre:
            c += 1
            j += 1
        L.append((c,lettre))
        i = j
    return L

# 36.

def decode(codeRLE):
    s = ""
    for i in range(len(codeRLE)):
        c,lettre = codeRLE[i]
        s = s + c*lettre
    return s


