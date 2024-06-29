#!/usr/bin/env python3

import glob
import copy
import numpy as np

solutions = []

def rec(Npoints, pairs, key, solution, it, maxit=100):
  global solutions
  if it > maxit:
    return
  if len(solution[1].keys()) == Npoints:
    print(solution)
    solutions.append(solution)
    return
  keys = [key]
  if "ship" not in key:
    key_rev = key.split("-")[1] + "-" + key.split("-")[0]
#    keys.append(key_rev)
  for key in keys:
    if key not in pairs.keys():
      continue
    for val in pairs[key]:
      new_sol = copy.deepcopy(solution)
      new_sol[0].append(val)
      kval = val.split("-")[1]
      if kval in new_sol[1].keys():
        if new_sol[1][kval] > 1:
          return
        new_sol[1][kval] += 1
      else:
        new_sol[1][kval] = 1
      rec(Npoints, pairs, val, new_sol, it+1, maxit)

def task(data):
    global solutions
    pairs = []
    solutions = []
    for line in data.split("\n"):
      if "is unreachable!" in line:
        print(line)
        continue
      if line == "":
        continue
      pairs.append(line.replace(" ","").split(":"))
    dictpairs = {}
    Npoints = 0
    for srt, end in pairs:
      if srt in dictpairs.keys():
        dictpairs[srt].append(end)
      else:
        dictpairs[srt] = [end]
      if "ship" not in srt:
        Npoints = max(Npoints, int(srt.split("-")[0]), int(srt.split("-")[1]), int(end.split("-")[0]), int(end.split("-")[1]))
    print(Npoints)
    print(dictpairs)
    for key in dictpairs.keys():
      if "ship" in key:
        for val in dictpairs[key]:
          rec(Npoints, dictpairs, val, [[key, val], { key.split("-")[1] : 1, val.split("-")[1] : 1 }], 0)
    return solutions

def process_solutions(solutions, task_id):
    coords = open(f"spaceship{task_id}.txt_uniq").read().split("\n")
    while "" in coords:
      coords.remove("")
    coords = [ [int(c.split(" ")[0]), int(c.split(" ")[1])] for c in coords ]
    coords = np.asarray(coords)
    print(coords)
    for solution in solutions:
      pos = np.array([0,0])
      curspeed = np.array([0,0])
      path = solution[0]
      visited = solution[1]
      textpath = ""
      for point in path:
        nextpos_id = int(point.split("-")[1]) - 1
        nextpos = coords[nextpos_id]
        new_speed = nextpos - pos
        dspeed = new_speed - curspeed
        print(dspeed)
        if np.all(dspeed == [0,0]):
          textpath += "5"
        elif np.all(dspeed == [-1,-1]):
          textpath += "1"
        elif np.all(dspeed == [-1,0]):
          textpath += "4"
        elif np.all(dspeed == [-1,+1]):
          textpath += "7"
        elif np.all(dspeed == [0,-1]):
          textpath += "2"
        elif np.all(dspeed == [0,+1]):
          textpath += "8"
        elif np.all(dspeed == [+1,-1]):
          textpath += "3"
        elif np.all(dspeed == [+1,0]):
          textpath += "6"
        elif np.all(dspeed == [+1,+1]):
          textpath += "9"
        else:
          textpath += "WTF"
        pos = nextpos
        curspeed = new_speed
      print(textpath, len(textpath))


def main():
  pairsf = glob.glob("spaceship2.pairs")

  for pairf in pairsf:
    data = open(pairf).read()
    solutions = task(data)
    process_solutions(solutions, 2)

main()