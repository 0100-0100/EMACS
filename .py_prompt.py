""" Module for defining the custom prompt used by python. """
import sys

class LineCount(object):
    """ Class for use as the line counter. """
    def __init__(self):
        self.line = 0

    def __str__(self):
        self.line += 1


class PS1(LineCount):
    def __str__(self):
        self.line += 1
        return "\033[38;5;220m{} >>> \u2502 \033[0m".format(self.line)

class PS2(LineCount):
    def __str__(self):
        self.line += 1
        return "\033[38;5;246m{} ... \u2502 \033[0m".format(self.line)

sys.ps1 = PS1()
sys.ps2 = PS2()
