#!/usr/bin/env python

import termios
import sys

fd = sys.stdin.fileno()
attrs = termios.tcgetattr(fd)
attrs[3] = attrs[3] | termios.ECHO
termios.tcsetattr(fd, termios.TCSADRAIN, attrs)
