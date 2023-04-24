import os.path
import re

# Define the regular expressions for each token
tokens = [
    ('INT', r'\bint\b'),
    ('IF', r'\bif\b'),
    ('ELSE', r'\belse\b'),
    ('WHILE', r'\bwhile\b'),
    ('FOR', r'\bfor\b'),
    ('IN', r'\bin\b'),
    ('RANGE', r'\brange\b'),
    ('PRINT', r'\bprint\b'),
    ('ASSIGN', r'='),
    ('EQ', r'=='),
    ('LE', r'<='),
    ('QUESTION', r'\?'),
    ('COLON', r':'),
    ('SEMICOLON', r';'),
    ('LBRACE', r'{'),
    ('RBRACE', r'}'),
    ('LPAREN', r'\('),
    ('RPAREN', r'\)'),
    ('ID', r'[a-zA-Z_]\w*'),
    ('NUM', r'\d+'),
    ('PLUS', r'\+'),
    ('MINUS', r'-'),
    ('MUL', r'\*'),
    ('DIV', r'/'),
    ('MOD', r'%'),
    ('COMMA', r','),
    ('FORLOOP', r'for\s*\('),
    ('RANGELOOP', r'range\s*\('),
    ('WHITESPACE', r'\s+')
]

tokenized_output = []


# Define the function to tokenize the input string
def tokenize(filename):
    with open(os.path.dirname(__file__) + '/../../data/' + filename) as file:
        program = file.read()
    text_file = open("sample.txt", "w")
    # Combine the regular expressions into a single pattern
    pattern = '|'.join('(?P<%s>%s)' % pair for pair in tokens)
    # Tokenize the input string
    for match in re.finditer(pattern, program):
        token_type = match.lastgroup
        token_value = match.group()
        # Ignore whitespace tokens
        if token_type == 'WHITESPACE':
            continue
        tokenized_output.append('\'' + token_value + '\'\n')
    text_file.write(''.join(tokenized_output))

    return tokenized_output
