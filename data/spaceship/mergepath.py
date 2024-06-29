#!/usr/bin/env python3

import glob

files = glob.glob("*.sol0")

for f in files:
  steps0 = open(f).read()
  print("Task: ", f)
#  print("Original str: ", steps0.replace("0",""))
  print("Original len: ", len(steps0.replace("0","")))
  steps = steps0.split("0")
  while "" in steps:
    steps.remove("")
  res = ""
  st = 'UD'
  for step in steps:
    UD = ""
    RL = ""
    st1 = st
    for c in step:
      if c == '6' or c == '4':
        st1 = 'UD'
      elif c == '8' or c == '2':
        st1 = 'RL'
      if st1 == 'UD':
        UD += c
      else:
        RL += c

#    print(UD)
#    print(RL)
    if st1 == 'RL':
      if len(RL) > len(UD):
        UD += "5" * (len(RL) - (len(UD)))
      elif len(RL) < len(UD):
        RL = "5" * (len(UD) - (len(RL))) + RL
    else:
      if len(RL) > len(UD):
        UD = "5" * (len(RL) - (len(UD))) + UD
      elif len(RL) < len(UD):
        RL += "5" * (len(UD) - (len(RL)))
#    print(UD)
#    print(RL)

    for c1, c2 in zip(RL, UD):
      c1 = ord(c1) - ord('0')
      c2 = ord(c2) - ord('0')
      res += str(c1 + c2 - 5)
    st = st1
  open(f[:-1], "w").write(res)
  print("Combined len: ", len(res))
