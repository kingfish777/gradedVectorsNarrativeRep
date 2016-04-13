
library(deal)


#fN <- "/home/kingfish/datafarm/d3jss/gib/1/TREATS==COEXISTS_WITH-INV/Citalopram/dataEXCLUSIONS.txt"

#datum <- readVec("/home/kingfish/datafarm/d3jss/gib/1/TREATS==COEXISTS_WITH-INV/Citalopram/dataEXCLUSIONS.txt")
#makeUnique(fN)
#system("cat /home/kingfish/datafarm/d3jss/gib/1/TREATS==COEXISTS_WITH-INV/Citalopram/dataEXCLUSIONS.txt")

readVecSep <- function(xyz, sp) {
  if (file.exists(xyz) && (file.info(xyz)$size > 0)) {
    unique(as.character(unlist(read.table(xyz, stringsAsFactors = F, sep = sp, skipNul = TRUE, header = FALSE, allowEscapes = TRUE))))
  } else { c() }  
}



readVec <- function(xyz) {
  if (file.exists(xyz) && (file.info(xyz)$size > 0)) {
    unique(as.character(unlist(read.table(xyz, stringsAsFactors = F, skipNul = TRUE, header = FALSE))))
  } else { c() }  
}

readVecT <- function(xyz) {
  if (file.exists(xyz) && (file.info(xyz)$size > 0)) {
    unique(as.character(unlist(read.table(xyz, stringsAsFactors = F, skipNul = TRUE, header = TRUE))))
  } else { c() }  
}

readVecSepT <- function(xyz, sp) {
  if (file.exists(xyz) && (file.info(xyz)$size > 0)) {
    unique(as.character(unlist(read.table(xyz, stringsAsFactors = F, skipNul = TRUE, header = TRUE))))
  } else { c() }  
}

writeVec <- function(dat,fn) {
  write.table(unique(dat), file=fn, quote=FALSE, row.names=FALSE, eol="\n", col.names=FALSE, append=FALSE)
}

makeUnique <- function(fName) {
  data <- readVec(fName)
  writeVec(fn = fName, dat = data)
}


#writeVec(dat = dat, fn = "/home/kingfish/datafarm/d3jss/gib/1/TREATS==COEXISTS_WITH-INV/Citalopram/dataEXCLUSIONS.txt")

setwd("/home/kingfish/UTH/getPathwaysStandalone/")

adr <- "gib"

# read in ADR 

# write to doc

#system(paste("sudo ./qdis.sh ", adr, "Cui.txt > ", adr, "ResultsFile.txt", sep=""))
adrProteins.fn <- paste(adr, "Proteins.txt", sep="")
adrProteins.fn
command <- paste("./sedDisease.sh ", adr, " > ",  adrProteins.fn, sep="")
print(command)
system(command)
system("cat gibProteins.txt")
#system(paste("./sedDisease.sh ", adr, " > ",  adr, "Proteins.txt", sep=""))
adrProteins <- readVecSep(adrProteins.fn, sp=";")
print(adrProteins)
writeVec(dat = adrProteins, fn = adrProteins.fn)
system(paste("./clean.sh ", adr, " > ", adr, "Proteins.txt", sep=""))
system("cat gibProteins.txt")
#readVecSep(adrProteins.fn, sp=";")

getIntersection <- function(fn1, fn2, fn3) {
  system(paste("java -jar ADRPathways.jar ", fn1, " ", fn2, " ", fn3, " datafarm datafarm"))
}


controlInds <- c("1", "0")

# write each doc to file
for (ci in controlInds) {
  # read in drug list
  drugs <- readVec(dat = paste(adr, "_drug", controlId, ".txt"))
  for (drug in drugs) {
    #gdid #hits runADR
    drug <- "Ibuprofen"
    command <- paste("./gdid.sh ", drug, " > drugUniProtID.txt", sep="")
    system("cat drugUniProtID.txt ")
    uniProtID.drug <- readVec("drugUniProtID.txt")
    print(uniProtID.drug)
    command <- paste("./gdpathways.sh ", uniProtID.drug, " > drugpathways.txt ", sep="")
    print(command)
    system(command)
    drugPathways <- read.table("drugpathways.txt", sep="\t", header = TRUE)[,3:5]
    uniProtIDs.proteinPathways <- drugPathways[2]
    print(uniProtIDs.proteinPathways)
    writeVec(dat = uniProtIDs.proteinPathways, fn = "drugProteinPathways.txt")
    readVec("drugProteinPathways.txt")
    getIntersection("drugProteinPathways.txt", adrProteins.fn, "newoutput.txt")
    system("cat newoutput.txt ")
    #command <- paste("more drugpathways.txt | awk -F ", " {'print $4'}", sep="")
    #print(command)
    #system(command)
    #dat <- system(command)
    #readVecSep("drugpathways.txt", sep=";")
    #get drug proteins
    
    #get drug pathways
    
    #get SNPSs
    
    getIntersection("drugProteinPathways.txt", adrProteins.fn, fn3)
  }
}


# take protein results

#  write to doc 
