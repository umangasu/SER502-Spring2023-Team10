import glob
import os
import sys
from _path import ROOT_DIRECTORY
from src.tokenize.lexer import tokenize


def main():
    try:
        os.chdir(f"{ROOT_DIRECTORY}/data")
        executables = []
        for file in glob.glob("*.eva"):
            executables.append(file)
        print('Select the index of the file you want to execute: \n')
        for executable in range(len(executables)):
            print(f"""[{executable}] {executables[executable]}""")
        fileindex = int(input())
        print(executables[fileindex])
        tokenize(executables[fileindex])

    except KeyboardInterrupt:
        try:
            sys.exit(0)
        except SystemExit:
            os._exit(0)


if __name__ == '__main__':
    main()
