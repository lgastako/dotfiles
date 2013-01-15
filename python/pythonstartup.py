import os
import atexit
import readline
import rlcompleter


history_path = os.path.expanduser("~/dotfiles/python/history")

def f():
    readline.write_history_file(history_path)

atexit.register(f)

if os.path.exists(history_path):
    readline.read_history_file(history_path)

