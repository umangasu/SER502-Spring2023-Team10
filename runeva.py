import glob
import os
import sys
from _path import ROOT_DIRECTORY
from src.tokenize.lexer import tokenize
import subprocess

def main():
    try:
        if len(sys.argv) < 2:
            print("Usage: python runeva.py <filename>")
            return
        filename = sys.argv[1]
        os.chdir(f"{ROOT_DIRECTORY}/data")
        tokenize(filename)
        token_file = "tokens.txt"
        os.chdir(ROOT_DIRECTORY)    
        result = subprocess.run(['swipl', '-f', 'tokenize.pl', '-g', f'convert("{token_file}"),halt.'], capture_output=True, text=True)
        print(result)

    except KeyboardInterrupt:
        try:
            sys.exit(0)
        except SystemExit:
            os._exit(0)


main()
