import os.path
with open(os.path.dirname(__file__) + '/../../data/dummy.eva') as file:
    code = file.read()

lines = code.split('\n')

lines = [line.strip() for line in lines]


tokens = []
for line in lines:
    line_tokens = line.split()
    for token in line_tokens:
        if token[-1] == ';':
            tokens.append(token[:len(token)-1])
            tokens.append(',')
            tokens.append(';')
            tokens.append(',')
        else:
            tokens.append(token) 
            tokens.append(',')               


output = tokens[:len(tokens)-1]

print(output)
print(''.join(output))
