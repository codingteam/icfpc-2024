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
  last = np.asarray([ 0, 0 ])
  last_N.append(last)
  orig_len = len(coords)
  for iter_ in range(0, orig_len):
    if iter_ % 10 == 0:
      print(iter_, ' / ', orig_len)
    to_add = len(coords)
    dist = 2**30
    for coord_id in range(0, len(coords)):
      coord = coords[coord_id]
      d = distance(coord, last_N)
      if d < dist:
        to_add = coord_id
        dist = d
    last_N.append([coords[to_add][0], coords[to_add][1]])
    res.append([coords[to_add][0], coords[to_add][1]])
    coords.pop(to_add)
    if len(last_N) > N:
      last_N.pop(0)
  return res

def load_coords(filename):
  data = open(filename).read().split("\n")
  while "" in data:
    data.remove("")
  coords = [ [ int(c.split(" ")[0]), int(c.split(" ")[1]) ] for c in data ]
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
