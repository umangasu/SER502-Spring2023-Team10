# Author - Umang Sahastransu, Sanika Yatin Gandhe
# Purpose - Lexical Analysing
# Version - Final
# Date - 29 April 2023

import os.path
import re
from _path import ROOT_DIRECTORY

# Define the regular expressions for each token
tokens = [
    ('INT', r'\bint\b'),
    ('FLOAT', r'\bfloat\b'),
    ('STRING', r'\bstring\b'),
    ('IF', r'\bif\b'),
    ('ELSE', r'\belse\b'),
    ('WHILE', r'\bwhile\b'),
    ('FOR', r'\bfor\b'),
    ('IN', r'\bin\b'),
    ('RANGE', r'\brange\b'),
    ('PRINTEXP', r'print\s*\(\s*("[^"]*"|\'[^\']*\'|[^\)]*)\s*\)'),
    ('PRINT', r'\bprint\b'),
    ('EQ', r'=='),
    ('LE', r'<='),
    ('GE', r'>='),
    ('NE', r'!='),
    ('AND', r'&&'),
    ('OR', r'\|\|'),
    ('ASSIGN', r'='),
    ('LT', r'<'),
    ('GT', r'>'),
    ('N', r'!'),
    ('QUESTION', r'\?'),
    ('COLON', r':'),
    ('SEMICOLON', r';'),
    ('LBRACE', r'{'),
    ('RBRACE', r'}'),
    ('LPAREN', r'\('),
    ('RPAREN', r'\)'),
    ('ID', r'[a-zA-Z_]\w*'),
    ('NUM', r'\d+'),
    ('INC', r'\++'),
    ('DEC', r'\--'),
    ('PLUS', r'\+'),
    ('MINUS', r'-'),
    ('MUL', r'\*'),
    ('DIV', r'/'),
    ('MOD', r'%'),
    ('COMMA', r','),
    ('FORLOOP', r'for\s*\('),
    ('RANGELOOP', r'range\s*\('),
    ('WHITESPACE', r'\s+'),
    ('PRINT_TEXT', r'("[^"]*"|\'[^\']*\'|[^\)]*)')
]

tokenized_output = []


# Define the function to tokenize the input string
def tokenize(filename):
    with open(os.path.dirname(__file__) + '/../../data/' + filename) as file:
        program = file.read()
    os.chdir(f"{ROOT_DIRECTORY}/src/compiler")     
    text_file = open("tokens.evac", "w")
    # Combine the regular expressions into a single pattern
    pattern = '|'.join('(?P<%s>%s)' % pair for pair in tokens)
    # Tokenize the input string
    for match in re.finditer(pattern, program):
        token_type = match.lastgroup
        token_value = match.group()
        # Ignore whitespace tokens
        if token_type == 'WHITESPACE':
            continue
        elif token_type == 'PRINTEXP':
            print_value = token_value[6:len(token_value) - 1].split(',')
            tokenized_output.append("print" + '\n' + "(" + '\n')
            for value in print_value:
                tokenized_output.append( value.strip() + '\n')
                tokenized_output.append(","+"\n")
            tokenized_output.pop()    
            tokenized_output.append( ")" + '\n')
            continue
        tokenized_output.append(token_value + '\n')
    i = 0    
    while i < len(tokenized_output):
        if tokenized_output[i][0] == '"' and tokenized_output[i][-2] == '"':
            var = tokenized_output[i][1:-2]
            tokenized_output.insert(i, '"' + '\n')
            tokenized_output.insert(i+1, var + '\n')
            tokenized_output.insert(i+2, '"' + '\n')
            tokenized_output.pop(i+3)
            i+=2
        i+=1
    tokenized_output.pop()
    text_file.write(''.join(tokenized_output))

    return tokenized_output
