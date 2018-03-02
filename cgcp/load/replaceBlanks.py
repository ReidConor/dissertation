import csv,sys
fin = open(sys.argv[1])
fout = open(sys.argv[2],'w')
reader = csv.reader(fin, delimiter=';', quotechar='"')
writer = csv.writer(fout, delimiter=';', quotechar='"', quoting=csv.QUOTE_MINIMAL)
for row in reader:
    new = []
    for s in row:
        if s=='':
            new.append('NULL')
        else:
            new.append(s)
    writer.writerow(new)
fin.close()
fout.close()
