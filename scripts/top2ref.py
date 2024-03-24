"""
This script is designed to translate "top" and "bottom" SNPs to "fwdtop" and
"fwdbottom" based on the file, top2ref_snptab.csv for all genotpyes in the
file, Parviglumis_TopStrand_FinalReport.txt
Created By David E. Hufnagel on Sun Oct 14 15:37:10 2018
"""

import sys

parvFd = open("Parviglumis_TopStrand_FinalReport.txt")
gerkeFd = open("Gerke_Map.txt")
conversionFd = open("top2ref_snptab.csv")
out = open("Parviglumis_TopStrand_FinalReport_top2ref.txt", "w")


#Go through gerkeFd and make a dict of key: chr_position  val: markerName
gerkeDict = {}
gerkeFd.readline()
for line in gerkeFd:
    lineLst = line.strip().split("\t")
    markerName = lineLst[0]
    key = "%s_%s" % (lineLst[4],lineLst[2])
    gerkeDict[key] = markerName  

#Go through conversionFd and make a dict of key: markerName  val: dict of key:top or bot val: fwdtop or fwdbot (respectively)
convDict = {}
conversionFd.readline()
for line in conversionFd:
    lineLst = line.strip().split(",")
    locus = lineLst[0]
    markerName = gerkeDict[locus]
    val = {lineLst[4]: lineLst[6], lineLst[5]: lineLst[7], "-": "N"}
    convDict[markerName] = val

#Go through parvFd and output the file with all genotype conversions
title = parvFd.readline(); out.write(title)
for line in parvFd:
    lineLst = line.strip().split("\t")
    markerName = lineLst[0]
    newLineLst = []
    
    if markerName in convDict: #We don't have all markers in the conversion file and the gerke file
        newLineLst.append(markerName)
        for geno in lineLst[1:]:
                newGeno = convDict[markerName][geno[0]] + convDict[markerName][geno[1]]
                newLineLst.append(newGeno)
            
        newline = "\t".join(newLineLst) + "\n"
        out.write(newline)



parvFd.close()
gerkeFd.close()
conversionFd.close()
out.close()
