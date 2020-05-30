f = open('input-avg-b.txt', 'r')
o = open('output-avg-b.txt', 'w')
dict = {}
allitens = ''
for line in f:
    lineSplit1 = line.split('=>')
    lineSplit = lineSplit1[0].replace('"','').replace('"','').replace('{','').replace('}','').replace(' ','').split(',')
    for val in lineSplit:
        valSplit = val.split('=')
        question = {}
        questionKey = valSplit[0].replace('\n','')
        answer = valSplit[1].replace('\n','')
        if questionKey in dict.keys():
            question = dict[questionKey]
        else:
            dict[questionKey] = question
        if answer in question.keys():
            question[answer] += 1
        else:
            question[answer] = 1
        allitens += val.replace('\n','') + ','

o.write(allitens)
f.close()
o.close()
