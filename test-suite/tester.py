#!/usr/bin/python
from sys        import *
from subprocess import *
import os


coqdoc_output = argv[1]
ref           = argv[2]
ret_val       = -1

try:
  ret_output = check_output(["diff", coqdoc_output, ref])
except CalledProcessError as e:
  ret_val = e.returncode
  ret_output = e.output

fail="\033[1;31m"
win ="\033[1;32m"
end ="\033[1;m"

if ret_val != -1:
  print (fail + "[FAILED] " + coqdoc_output + end)
  out_name, ext = os.path.splitext(os.path.basename(coqdoc_output))
  out_name = 'log/' + out_name + '.log'
  out_f = open(out_name, 'w')
  out_f.write(ret_output)

else:
  print (win + "[SUCCESS] " + coqdoc_output + end)
