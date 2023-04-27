import os.path
import re

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
        elif token_type == 'PRINT_TEXT':
            print("Print text : ", token_value)
            if len(token_value) > 0:
                if token_value[0] == '"' and token_value[len(token_value) - 1] == '"':
                    tokenized_output.append('\'\"\'\n')
                    tokenized_output.append('\'' + token_value[1:len(token_value) - 1] + '\'\n')
                    tokenized_output.append('\'\"\'\n')
        elif token_type == 'PRINTEXP':
            print_value = token_value[6:len(token_value) - 1].split(',')
            print('print value : ', print_value)
            tokenized_output.append('\'' + "print" + '\'\n' + '\'' + "(" + '\'\n')
            for value in print_value:
                raw_value = value.strip()
                if raw_value[0] == '"' and raw_value[len(raw_value) - 1] == '"':
                    tokenized_output.append('\'\"\'\n')
                    tokenized_output.append('\'' + raw_value[1:len(raw_value) - 1] + '\'\n')
                    tokenized_output.append('\'\"\'\n')
                else:
                    raw_value = '\'' + raw_value + '\'\n'
                    tokenized_output.append(raw_value)
            tokenized_output.append('\'' + ")" + '\'\n')
            continue
        else:
            tokenized_output.append('\'' + token_value + '\'\n')
    tokenized_output.pop()
    text_file.write(''.join(tokenized_output))

    return tokenized_output
