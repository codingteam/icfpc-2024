#!/usr/bin/env python3

import glob

files = glob.glob("*.txt")


def check(task):
    ok = True
    for c in task:
        if c not in ['.', '#', 'L', '\n']:
            ok = False
            break
    return ok


def to_2D(task):
    res = []
    lines = task.split("\n")
    for line in lines:
        res.append([*line])
    while [] in res:
        res.remove([])
    return res


def print_field(task2D):
    for line in task2D:
        print(line)


def do_instructions(task2D, solution):
    x = 0
    y = 0
    for i in range(0, len(task2D)):
        for j in range(0, len(task2D[0])):
            if task2D[i][j] == 'L':
                x = i
                y = j
    for i in range(0, len(solution)):
        c = solution[i]
        if c == 'U':
            if task2D[x-1][y] == '#':
                print(f'command {i}:{c} is improper!')
            else:
                task2D[x][y] = ' '
                x -= 1
                task2D[x][y] = 'L'
        elif c == 'D':
            if task2D[x+1][y] == '#':
                print(f'command {i}:{c} is improper!')
            else:
                task2D[x][y] = ' '
                x += 1
                task2D[x][y] = 'L'
        elif c == 'L':
            if task2D[x][y-1] == '#':
                print(f'command {i}:{c} is improper!')
            else:
                task2D[x][y] = ' '
                y -= 1
                task2D[x][y] = 'L'
        elif c == 'R':
            if task2D[x][y+1] == '#':
                print(f'command {i}:{c} is improper!')
            else:
                task2D[x][y] = ' '
                y += 1
                task2D[x][y] = 'L'
        else:
            print(f'command {i}:{c} is improper!')
    print(f"Solution cost: {len(solution)+18}")
    print(solution)
    return task2D


for f in files:
    task = open(f).read()
    f = f.split(".")[0]
    if not check(task):
        continue
    task2D = to_2D(task)
    print("Initial:")
    print_field(task2D)
    try:
        load_instr = open(f"{f}.sol").read()
        if load_instr.split("\n")[0] == "":
            task2D = do_instructions(task2D, "")
        else:
            task2D = do_instructions(task2D, load_instr)
    except:
        task2D = do_instructions(task2D, "")
    print("Final:")
    print_field(task2D)
