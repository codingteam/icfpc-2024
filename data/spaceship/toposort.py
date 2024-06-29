#!/usr/bin/env python3

import numpy as np
import sys

def sort2D(coords, N = 5):
  def distance(point, data):
    dist = 0
    for p in data:
      dist += abs(p[0] - point[0]) + abs(p[1] - point[1])
    return dist
  res = []
  last_N = []
  last = np.asarray([ 0, 0, 0 ])
  last_N.append(last)
  for iter_ in range(0, len(coords)):
    print(iter_, ' / ', len(coords))
    to_add = len(coords)
    dist = 2**30
    for coord_id in range(0, len(coords)):
      coord = coords[coord_id]
      if coord[2] == 1:
        continue
      d = distance(coord, last_N)
      if d < dist:
        to_add = coord_id
        dist = d
    if to_add == len(coords):
      print("ERROR!")
      sys.exit(1)
    coords[to_add][2] = 1 # disable point
    last_N.append([coords[to_add][0], coords[to_add][1]])
    res.append([coords[to_add][0], coords[to_add][1]])
    if len(last_N) > N:
      last_N.pop(0)
  return res

def load_coords(filename):
  data = open(filename).read().split("\n")
  while "" in data:
    data.remove("")
  coords = [ [ int(c.split(" ")[0]), int(c.split(" ")[1]), 0 ] for c in data ]
  coords = np.asarray(coords)
  return coords

def save_coords(filename, coords):
  coords = [ str(c[0]) + " " + str(c[1]) for c in coords ]
  open(filename + "_sort", "w").write("\n".join(coords))

def main(task_id):
  filename = f"spaceship{task_id}.txt"
  coords = load_coords(filename)
  coords = sort2D(coords)
  save_coords(filename, coords)

main(sys.argv[1])
