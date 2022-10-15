# B
# 1



N_ROWS = 6
N_COLS = 15




dicta = {"N" : False, "E" : True, "S" : True, "W" : True}


dictb = {"N" : False, "E" : False, "S" : True, "W" : False}


dictc = {"N" : True, "E" : False, "S" : False, "W" : False }


dictd = {"N" : True, "E" : True, "S" : False, "W" : True}



# 2

def create_closed_cell():
    return {"N" : True, "E" : True, "S" : True, "W" : True}

# 3

def is_closed(cell):
    return (cell["N" & "E" & "S" & "W"])


# 4

def create_all_closed_maze(n_rows,n_cols):
    return [[create_closed_cell()]*n_cols] * n_rows


# 5

def tear_down(maze,i,j,direction):
    if direction == "N":
        maze[i][j]["N"] = False
        maze[i + 1][j]["S"] = False
    elif direction == "S":
        maze[i][j]["S"] = False
        maze[i - 1][j]["N"] = False
    elif direction == "W":
        maze[i][j]["W"] = False
        maze[i][j - 1]["E"] = False
    elif direction == "E":
        maze[i][j]["E"] = False
        maze[i][j + 1]["W"] = False
    else:
        raise Exception("Unknown direction --> " + direction)

# En python, les dictionnaires sont passés en référence dans la fonction. Inversement le type int est passé en valeur.

# 6

visited = [[False]*N_ROWS]*N_COLS


# 7

def dir_unvisited_neighbours(visited,i,j):
    lst = []
    if ((i < len(visited[0])-1) and (not visited[i+1][j])):
        lst.append("N")
    if (i>0 and (not visited[i-1][j])):
        lst.append("S")
    if ((j< len(visited)-1) and (not visited[i][j+1])):
        lst.append("E")
    if ((j>0 and (not visited[i][j-1]))):
        lst.append("W")
    return lst


# C
# 1
import maze_example as mz



# C
# 2
print(len(mz.maze_ex),"Cols", len(mz.maze_ex[0]) ,"Rows")

# C'est inversé je crois.

# 3
import matplotlib.pyplot as plt
import numpy as np

# 4
def draw_visited(maze,visited):
    plt.figure()
    for i in range(len(visited)):
        for j in range(len(visited[0])):
            if (visited[i][j]):
                 plt.plot(j,i, color ='black',marker = '.')
    plt.ylim(-1,len(maze),1)
    plt.xlim(-1,len(maze[0]),1)
    plt.show()
    return

# draw_visited(mz.maze_ex,mz.visited_ex)



# 5

def draw_path(maze,path):
    plt.figure()
    if path:
        from_x,from_y = path[0][1], path[0][0]
        plt.plot(from_x,from_y,color = 'red', marker = '*')
    for index in range(1,len(path)):
        to_x,to_y = path[index][1], path[index][0] 
        plt.plot([from_x,to_x],[from_y,to_y], color ='red')
        plt.plot(to_x,to_y,color = 'red', marker = '*')
        from_x,from_y = to_x,to_y
    plt.ylim(-1,len(maze),1)
    plt.xlim(-1,len(maze[0]),1)
    plt.show()
    return


# draw_path(mz.maze_ex,mz.path_ex)





# 6

def draw_maze(maze):
    if maze:
        plt.figure()
        def draw_cell(dict,abs,ord):
            if dict["N"]:
                plt.plot([abs-0.5,abs+0.5],[ord+0.5,ord+0.5],color = 'blue')
            if dict["W"]:
                plt.plot([abs-0.5,abs-0.5],[ord-0.5,ord+0.5],color = 'blue')
            if dict["E"]:
                plt.plot([abs+0.5,abs+0.5],[ord-0.5,ord+0.5],color = 'blue')
            if dict["S"]:
                plt.plot([abs-0.5,abs+0.5],[ord-0.5,ord-0.5],color = 'blue')
        n = len(maze)   
        m = len(maze[0]) 
        for i in range(m):
            for j in range(n):
                draw_cell(maze[j][i],i,j)
    plt.ylim(-1,len(maze),1)
    plt.xlim(-1,len(maze[0]),1)
    plt.show()


# draw_maze(mz.maze_ex)



# D
# 1
# La ligne 4 permet de tester si la case étudiée est la sortie.
# On la traduit par if m[i][j]["Exit"]:
 


# 2
# Non, ce cas n'est pas possible grâce à la ligne 8 qui écarte les cases déjà visité de l'étude.



# 3

# Non pas forcément, l'appel récursif s'arrête lorsque la case est une sortie. Prenons le cas du labyrinthe:
# - - - - - -
#|d     e    |
# - - - - - -
# Ici,  les cases après e ne sont pas visitées.


# 4

def algo_1(maze,i,j,visited,path):
    visited[i][j] = True
    path.append(i,j)
    if maze[i][j]["Exit"]:
        return True
    else:
        if not(maze[i][j]["N"]):
            p,q = i,j+1
            if not(visited[p][q]):
                if algo_1(maze,p,q,visited,path):
                    return True
                else:
                    del path[-1]
        if not(maze[i][j]["S"]):
            p,q = i,j-1
            if not(visited[p][q]):
                if algo_1(maze,p,q,visited,path):
                    return True
                else:
                    del path[-1]
        if not(maze[i][j]["W"]):
            p,q = i-1,j
            if not(visited[p][q]):
                if algo_1(maze,p,q,visited,path):
                    return True
                else:
                    del path[-1]
        if not(maze[i][j]["E"]):
            p,q = i+1,j
            if not(visited[p][q]):
                if algo_1(maze,p,q,visited,path):
                    return True
                else:
                    del path[-1]
    return False



# 5
# Oui on pourrait ajouter des sorties: il faut alors rajouter des "Exit".


# E
# 1
from random import randrange, choice


# 2
# def explore_generate(n_rows,n_cols):
#     maze = create_all_closed_maze(n_rows,n_cols)
#     visited = [[False]*n_cols] * n_rows
#     i,j = randrange(n_rows), randrange(n_cols)
#     pile = []
#     pile.append((i,j))
#     visited[i][j] = True
#     while len(pile) > 0:
#         (i,j) = pile.pop()
#         n_dir = dir_unvisited_neighbours(visited,i,j)
#         if len(n_dir) >= 2:
#             pile.append((i,j))
#         if len(n_dir) > 0:
#             dir = choice(n_dir)
#             if dir=="N":
#                 p,q = i+1,j
#             elif dir=="W":
#                 p,q = i,j-1
#             elif dir=="E":
#                 p,q=i,j+1
#             else:
#                 p,q=i-1,j
#         tear_down(maze,i,j,dir)
#         pile.append((p,q))
#         visited[p][q] = True
#     return maze


# 2
def generate_maze(n_rows, n_cols):
    maze = create_all_closed_maze(n_rows,n_cols)
    visited = [[False for j in range(n_cols)] for i in range(n_rows)]
    i, j = randrange(n_rows), randrange(n_cols) # select a start cell
    pile = []
    pile.append((i, j))
    visited[i][j] = True
    # print("Starting --> ", i, j)
    while len(pile) > 0:
        (i, j) = pile.pop()
        n_dir = dir_unvisited_neighbours(visited, i, j)
        if len(n_dir) > 1:
            pile.append((i, j)) # do not forget to deal with the others neighbours later
        if len(n_dir) > 0: # if there are neighbours
            dir = choice(n_dir) # pick a random direction among possible neighbours
            # print("Direction -> ", dir)
        if dir == "N":
            tear_down(maze, i, j, "N")
            pile.append((i + 1, j))
            visited[i + 1][j] = True
        elif dir == "S":
            tear_down(maze, i, j, "S")
            pile.append((i - 1, j))
            visited[i - 1][j] = True
        elif dir == "W":
            tear_down(maze, i, j, "W")
            pile.append((i, j - 1))
            visited[i][j - 1] = True
        else:
            tear_down(maze, i, j, "E")
            pile.append((i, j + 1))
            visited[i][j + 1] = True

# 3

maze = generate_maze(5,5)

draw_maze(maze)


# 3

# On est en O(n_rows*n_cols)

# 4

# On est en O(n^2)


# Oui on peut toujours trouver la sortie d'un labyrinthe ainsi généré à condition qu'il soit de taille supérieur à 2*2.


# F

# 1
