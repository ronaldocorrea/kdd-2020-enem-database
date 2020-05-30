def createTransactionFiles(infile, outfile):
    f = open(infile, 'r')
    fOut = open(outfile, 'w')

    header = []
    for i, line in enumerate(f):
        if i == 0:
            header = line.replace("\"","").split(',')
            i += 1
            continue
        newLine = ''
        lineSplit = line.replace("\"","").split(',')
        for j, key in enumerate(header):
            if lineSplit[j] != 'NI' and lineSplit[j] != 'Nao Informado' :
                newLine += key.replace('\n','') + '=' + lineSplit[j].replace('\n','') + ','

        fOut.write(newLine.rstrip(',') + '\n')

    fOut.close()
    f.close()


createTransactionFiles('../r-workspace/output/sp_dfRegrasAssociacao.csv', '../r-workspace/output/sp_transactions.csv')
# createTransactionFiles('../r-workspace/output/spAvgNotaA_dfRegrasAssociacao.csv', '../r-workspace/output/spAvgNotaA_transactions.csv')
# createTransactionFiles('../r-workspace/output/spAvgNotaB_dfRegrasAssociacao.csv', '../r-workspace/output/spAvgNotaB_transactions.csv')
# createTransactionFiles('../r-workspace/output/spAvgNotaC_dfRegrasAssociacao.csv', '../r-workspace/output/spAvgNotaC_transactions.csv')
# createTransactionFiles('../r-workspace/output/spCapital_dfRegrasAssociacao.csv', '../r-workspace/output/spCapital_transactions.csv')
# createTransactionFiles('../r-workspace/output/spNotCapital_dfRegrasAssociacao.csv', '../r-workspace/output/spNotCapital_transactions.csv')
# createTransactionFiles('../r-workspace/output/ceCapital_dfRegrasAssociacao.csv', '../r-workspace/output/ceCapital_transactions.csv')
# createTransactionFiles('../r-workspace/output/ceNotCapital_dfRegrasAssociacao.csv', '../r-workspace/output/ceNotCapital_transactions.csv')
