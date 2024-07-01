#!/usr/bin/env python3

import numpy as np
import sys

def sort2D(coords, last = [ 0, 0 ], N = 5):
  def distance(point, data):
    dist = 0
    for p in data:
      dist += abs(p[0] - point[0]) + abs(p[1] - point[1])
    return dist
  res = []
  last_N = []
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

def find_min_dist(last, coords):
  def distance(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])
  dist = 2**30
  for coord in coords:
    d = distance(last, coord)
    if d < dist:
      dist = d
  return dist

def find_closest_cluster(last, clusters):
  maxk = max(clusters.keys())
  closest = maxk
  mindist = find_min_dist(last, clusters[maxk])
  for i in range(0, maxk):
    if i in clusters.keys():
      d = find_min_dist(last, clusters[i])
      if d < mindist:
        mindist = d
        closest = i
  return closest

def load_coords(filename):
  data = open(filename).read().split("\n")
  while "" in data:
    data.remove("")
  coords = [ list(map(int, c.split(" "))) for c in data ]
  return coords

def coords_to_clusters(coords):
  if len(coords[0]) == 2:
    return { 0 : coords }
  clusters = {}
  for coord in coords:
    if coord[2] in clusters.keys():
      clusters[coord[2]].append([ coord[0], coord[1] ])
    else:
      clusters[coord[2]] = [ [ coord[0], coord[1] ] ]
  return clusters

def sort_clusters(clusters):
  coords = []
  last = [0, 0]
  for i in range(0, len(clusters.keys())):
    closest_cluster = find_closest_cluster(last, clusters)
    coords.extend(sort2D(clusters[closest_cluster]))
    del clusters[closest_cluster]
    last = coords[-1]
  return coords

def save_coords(filename, coords):
  coords = [ str(c[0]) + " " + str(c[1]) for c in coords ]
  open(filename + "_sort", "w").write("\n".join(coords))

def main(task_id):
  filename = f"spaceship{task_id}.txt"
  coords = load_coords(filename)
  clusters = coords_to_clusters(coords)
  coords = sort_clusters(clusters)
  save_coords(filename, coords)

main(sys.argv[1])
