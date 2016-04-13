#next:
#  finish execution script(s) 
#after that: 
#  archive results ...
#create confusion matrices ... 


library(TraMineR)
library(stringdist)
library(stringr)
library(tm)
system("rm proppAfan.csv")
setwd("/home/kingfish/Propp/goldStandard")
# setwd("/Users/smalec/propptest/Propp/goldStandard")
text <- system.file("texts", "txt", package="tm")
# read in corpus
corpus <- Corpus(DirSource())
#taleStructure <- list(100)
strReplacement = "_" # strReplacement
tale.100 <- str_replace_all(paste0(corpus[[1]]$content, collapse = ""), "\t", strReplacement)
tale.101 <- str_replace_all(paste0(corpus[[2]]$content, collapse = ""), "\t", strReplacement)
tale.104 <- str_replace_all(paste0(corpus[[3]]$content, collapse = ""), "\t", strReplacement)
tale.105 <- str_replace_all(paste0(corpus[[4]]$content, collapse = ""), "\t", strReplacement)
tale.106 <- str_replace_all(paste0(corpus[[5]]$content, collapse = ""), "\t", strReplacement)
tale.108 <- str_replace_all(paste0(corpus[[6]]$content, collapse = ""), "\t", strReplacement)
tale.113 <- str_replace_all(paste0(corpus[[7]]$content, collapse = ""), "\t", strReplacement)
tale.114 <- str_replace_all(paste0(corpus[[8]]$content, collapse = ""), "\t", strReplacement)
tale.115 <- str_replace_all(paste0(corpus[[9]]$content, collapse = ""), "\t", strReplacement)
tale.125 <- str_replace_all(paste0(corpus[[10]]$content, collapse = ""), "\t", strReplacement)

tale.126 <- str_replace_all(paste0(corpus[[11]]$content, collapse = ""), "\t", strReplacement)
tale.127 <- str_replace_all(paste0(corpus[[12]]$content, collapse = ""), "\t", strReplacement)
tale.128 <- str_replace_all(paste0(corpus[[13]]$content, collapse = ""), "\t", strReplacement)
tale.131 <- str_replace_all(paste0(corpus[[14]]$content, collapse = ""), "\t", strReplacement)
tale.132 <- str_replace_all(paste0(corpus[[15]]$content, collapse = ""), "\t", strReplacement)
tale.133 <- str_replace_all(paste0(corpus[[16]]$content, collapse = ""), "\t", strReplacement)
tale.135 <- str_replace_all(paste0(corpus[[17]]$content, collapse = ""), "\t", strReplacement)
tale.136 <- str_replace_all(paste0(corpus[[18]]$content, collapse = ""), "\t", strReplacement)
tale.137 <- str_replace_all(paste0(corpus[[19]]$content, collapse = ""), "\t", strReplacement)
tale.138 <- str_replace_all(paste0(corpus[[20]]$content, collapse = ""), "\t", strReplacement)

tale.139 <- str_replace_all(paste0(corpus[[21]]$content, collapse = ""), "\t", strReplacement)
tale.140 <- str_replace_all(paste0(corpus[[22]]$content, collapse = ""), "\t", strReplacement)
tale.141 <- str_replace_all(paste0(corpus[[23]]$content, collapse = ""), "\t", strReplacement)
tale.143 <- str_replace_all(paste0(corpus[[24]]$content, collapse = ""), "\t", strReplacement)
tale.144 <- str_replace_all(paste0(corpus[[25]]$content, collapse = ""), "\t", strReplacement)
tale.145 <- str_replace_all(paste0(corpus[[26]]$content, collapse = ""), "\t", strReplacement)
tale.148 <- str_replace_all(paste0(corpus[[27]]$content, collapse = ""), "\t", strReplacement)
tale.149 <- str_replace_all(paste0(corpus[[28]]$content, collapse = ""), "\t", strReplacement)
tale.150 <- str_replace_all(paste0(corpus[[29]]$content, collapse = ""), "\t", strReplacement)
tale.151 <- str_replace_all(paste0(corpus[[30]]$content, collapse = ""), "\t", strReplacement)

tale.152 <- str_replace_all(paste0(corpus[[31]]$content, collapse = ""), "\t", strReplacement)
tale.153 <- str_replace_all(paste0(corpus[[32]]$content, collapse = ""), "\t", strReplacement)
tale.154 <- str_replace_all(paste0(corpus[[33]]$content, collapse = ""), "\t", strReplacement)
tale.155 <- str_replace_all(paste0(corpus[[34]]$content, collapse = ""), "\t", strReplacement)
tale.156 <- str_replace_all(paste0(corpus[[35]]$content, collapse = ""), "\t", strReplacement)
tale.159 <- str_replace_all(paste0(corpus[[36]]$content, collapse = ""), "\t", strReplacement)
tale.161 <- str_replace_all(paste0(corpus[[37]]$content, collapse = ""), "\t", strReplacement)
tale.162 <- str_replace_all(paste0(corpus[[38]]$content, collapse = ""), "\t", strReplacement)
tale.163 <- str_replace_all(paste0(corpus[[39]]$content, collapse = ""), "\t", strReplacement)
tale.164 <- str_replace_all(paste0(corpus[[40]]$content, collapse = ""), "\t", strReplacement)

tale.166 <- str_replace_all(paste0(corpus[[41]]$content, collapse = ""), "\t", strReplacement)
tale.167 <- str_replace_all(paste0(corpus[[42]]$content, collapse = ""), "\t", strReplacement)
tale.093 <- str_replace_all(paste0(corpus[[43]]$content, collapse = ""), "\t", strReplacement)
tale.095 <- str_replace_all(paste0(corpus[[44]]$content, collapse = ""), "\t", strReplacement)
tale.098 <- str_replace_all(paste0(corpus[[45]]$content, collapse = ""), "\t", strReplacement)


readVec <- function(xyz) {
  if (file.exists(xyz) && (file.info(xyz)$size > 0)) {
    as.character(unlist(read.table(xyz, stringsAsFactors = F)))
  } else { "0" }  
}



writeVec <- function(dat,fn) {
  write.table(dat, file=fn, quote=FALSE, row.names=FALSE, eol="\n", col.names=TRUE, sep = ";")
}



taleStructuresAll <- c(tale.093, tale.095, tale.098, tale.100, tale.101, 
                       tale.104, tale.105, tale.106, tale.108, tale.113, 
                       tale.114, tale.115, tale.125, tale.126, tale.127, 
                       tale.128, tale.131, tale.132, tale.133, tale.135, 
                       tale.136, tale.137, tale.138, tale.139, tale.140, 
                       tale.141, tale.143, tale.144, tale.145, tale.148,
                       tale.149, tale.150, tale.151, tale.152, tale.153,
                       tale.154, tale.155, tale.156, tale.159, tale.161, 
                       tale.162, tale.163, tale.164, tale.166, tale.167) 

taleNamesAll <- c("093_Soltseva_Sestra.txt", "095_Morozko.txt", "098_Doch_Padcheritsa.txt", "100_Kroshechka-Khavroshechka.txt", "101_Burenushka.txt",
                  "104_Vasilisa_Prekrasnaja.txt", "105_Baba_Jaga_Zamoryshek.txt","106_Baba_Jaga_Zhikhar.txt","108_Witch_and_Ivan.txt","113_Magic_Swan-Geese.txt",
                  "114_Knjaz_Danila.txt", "115_Pravda_Krivda.txt","125_Ivan_Tsarevich.txt","126_Father_Brass.txt","127_Kupecheskaja_Doch_i_Sluzhanka.txt",
                  "128_Kingdoms_of_Gold_Copper_and_Silver.txt","131_Frolka-Siden.txt","132_Norka_Zver.txt","133_Pokatigoroshek.txt","135_Ivan_Popolyov.txt",
                  "136_Burja-Bogatyr_Ivan_Korovij_Syn.txt","137_Ivan_Bykovich.txt", "138_Ivan_Krestjankii.txt","139_Ivan_Suchenko.txt", "140_DawnDayEvening.txt",
                  "141_Medvedko_Usynja_i_Dubynja-bogatyri.txt","143_Nadzej_Papov_Unuk.txt","144_letuchij_korabl.txt","145_Sem_Semionov.txt","148_Nikita-Kozhemjaka.txt",
                  "149_Zmej_i_Tsygan.txt","150_Batrak_FarmHand.txt","151_Shabarsha.txt","152_Ivanko_Medvedko.txt", "153_Soldier_and_Princess.txt","154_Beglyj_Soldat_i_Chert.txt","155_Two_Ivans.txt","156_Koschej_Bessmertnyj.txt","159_Maria_Marievna.txt",
                  "161_Prince_Ivan_i_Belyj_Poljanin.txt","163_Bukhtan_Bukhtanovich.txt", "164_Cosmo_Get-Rich-Quick.txt","166_Emilja_Durak.txt", "167_As_the_Wand_Orders.txt")


#taleSet.all.nl <- list("all", "nl", taleNamesAll, taleStructuresAll)
taleSet.all.lem <- list("all", "lem", taleNamesAll, taleStructuresAll)




taleStructuresClean <- c(tale.093, tale.095, tale.098, tale.100, tale.101, 
                         tale.104, tale.105, tale.106, tale.108, tale.113, 
                         tale.114, tale.115, tale.125, tale.126, tale.127, 
                         tale.128, tale.131, tale.132, tale.133, tale.135, 
                         tale.136, tale.137, tale.139, tale.140, 
                         tale.141, tale.143, tale.144, tale.145, tale.148,
                         tale.149, tale.150, tale.151, tale.152, tale.153,
                         tale.154,  tale.156, tale.161, 
                         tale.162, tale.163, tale.164, tale.166, tale.167) 

taleNamesClean <- c("093_Soltseva_Sestra.txt", "095_Morozko.txt", "098_Doch_Padcheritsa.txt", "100_Kroshechka-Khavroshechka.txt", "101_Burenushka.txt",
                    "104_Vasilisa_Prekrasnaja.txt", "105_Baba_Jaga_Zamoryshek.txt","106_Baba_Jaga_Zhikhar.txt","108_Witch_and_Ivan.txt","113_Magic_Swan-Geese.txt",
                    "114_Knjaz_Danila.txt", "115_Pravda_Krivda.txt","125_Ivan_Tsarevich.txt","126_Father_Brass.txt","127_Kupecheskaja_Doch_i_Sluzhanka.txt",
                    "128_Kingdoms_of_Gold_Copper_and_Silver.txt","131_Frolka-Siden.txt","132_Norka_Zver.txt","133_Pokatigoroshek.txt","135_Ivan_Popolyov.txt",
                    "136_Burja-Bogatyr_Ivan_Korovij_Syn.txt","137_Ivan_Bykovich.txt", "139_Ivan_Suchenko.txt", "140_DawnDayEvening.txt",
                    "141_Medvedko_Usynja_i_Dubynja-bogatyri.txt","143_Nadzej_Papov_Unuk.txt","144_letuchij_korabl.txt","145_Sem_Semionov.txt","148_Nikita-Kozhemjaka.txt",
                    "149_Zmej_i_Tsygan.txt","150_Batrak_FarmHand.txt","151_Shabarsha.txt","152_Ivanko_Medvedko.txt", "153_Soldier_and_Princess.txt","154_Beglyj_Soldat_i_Chert.txt","156_Koschej_Bessmertnyj.txt",
                    "161_Prince_Ivan_i_Belyj_Poljanin.txt","163_Bukhtan_Bukhtanovich.txt", "164_Cosmo_Get-Rich-Quick.txt","166_Emilja_Durak.txt", "167_As_the_Wand_Orders.txt")


#taleSet.cleanAll.nl <- list("all", "nl", taleNamesAll, taleStructuresAll)
taleSet.cleanAll.lem <- list("all", "lem", taleNamesAll, taleStructuresAll)




taleStructuresSM <- c(tale.100, 
                      tale.108, tale.113, 
                      tale.114, tale.127, 
                      tale.131, tale.135, 
                      tale.140, 
                      tale.144, tale.145, tale.148,
                      tale.149, tale.151, tale.152, tale.153,
                      tale.154,  
                      tale.162, tale.163) 

taleNamesSM <- c("100_Kroshechka-Khavroshechka.txt", 
                 "108_Witch_and_Ivan.txt","113_Magic_Swan-Geese.txt",
                 "114_Knjaz_Danila.txt", "127_Kupecheskaja_Doch_i_Sluzhanka.txt",
                 "131_Frolka-Siden.txt","135_Ivan_Popolyov.txt",
                 "140_DawnDayEvening.txt",
                 "144_letuchij_korabl.txt","145_Sem_Semionov.txt","148_Nikita-Kozhemjaka.txt",
                 "149_Zmej_i_Tsygan.txt","151_Shabarsha.txt","152_Ivanko_Medvedko.txt", "153_Soldier_and_Princess.txt","154_Beglyj_Soldat_i_Chert.txt",
                 "162_Crystal_Mountain.txt","163_Bukhtan_Bukhtanovich.txt")


#taleSet.singlemove.nl <- list("singlemove", "nl", taleNamesSM, taleStructuresSM)
taleSet.singlemove.lem <- list("singlemove", "lem", taleNamesSM, taleStructuresSM)


taleStructuresSM2 <- c(tale.093, tale.095, tale.098, tale.100, 
                       tale.108, tale.113, 
                       tale.114, tale.127, 
                       tale.131, tale.135, 
                       tale.140, 
                       tale.144, tale.145, tale.148,
                       tale.149, tale.151, tale.152, tale.153,
                       tale.154,  
                       tale.162, tale.163) 

taleNamesSM2 <- c("093_Soltseva_Sestra.txt", "095_Morozko.txt", "098_Doch_Padcheritsa.txt", "100_Kroshechka-Khavroshechka.txt", 
                  "108_Witch_and_Ivan.txt","113_Magic_Swan-Geese.txt",
                  "114_Knjaz_Danila.txt", "127_Kupecheskaja_Doch_i_Sluzhanka.txt",
                  "131_Frolka-Siden.txt","135_Ivan_Popolyov.txt",
                  "140_DawnDayEvening.txt",
                  "144_letuchij_korabl.txt","145_Sem_Semionov.txt","148_Nikita-Kozhemjaka.txt",
                  "149_Zmej_i_Tsygan.txt","151_Shabarsha.txt","152_Ivanko_Medvedko.txt", "153_Soldier_and_Princess.txt","154_Beglyj_Soldat_i_Chert.txt",
                  "162_Crystal_Mountain.txt","163_Bukhtan_Bukhtanovich.txt")

#taleSet.singlemove2.nl <- list("singlemove2", "nl", taleNamesSM2, taleStructuresSM2)
taleSet.singlemove2.lem <- list("singlemove2", "lem", taleNamesSM2, taleStructuresSM2)



taleStructuresDM <- c(tale.093, tale.095, tale.098, tale.101, 
                      tale.104, tale.105, 
                      tale.115, tale.125, tale.126,  
                      tale.128, tale.133,  
                      tale.137, tale.139,  
                      tale.141, tale.143,
                      tale.150, tale.164, tale.166) 

taleNamesDM <- c("095_Morozko.txt", "098_Doch_Padcheritsa.txt", "101_Burenushka.txt",
                 "104_Vasilisa_Prekrasnaja.txt", "105_Baba_Jaga_Zamoryshek.txt", 
                 "115_Pravda_Krivda.txt","125_Ivan_Tsarevich.txt","126_Father_Brass.txt",
                 "128_Kingdoms_of_Gold_Copper_and_Silver.txt","133_Pokatigoroshek.txt","135_Ivan_Popolyov.txt",
                 "136_Burja-Bogatyr_Ivan_Korovij_Syn.txt","137_Ivan_Bykovich.txt", "139_Ivan_Suchenko.txt",
                 "141_Medvedko_Usynja_i_Dubynja-bogatyri.txt","143_Nadzej_Papov_Unuk.txt",
                 "150_Batrak_FarmHand.txt", "164_Cosmo_Get-Rich-Quick.txt","166_Emilja_Durak.txt")

#taleSet.doublemove.nl <- list("doublemove", "nl", taleNamesDM, taleStructuresDM)
taleSet.doublemove.lem <- list("doublemove", "lem", taleNamesDM, taleStructuresDM)


taleStructuresTM <- c(tale.093, tale.106,
                      tale.132,
                      tale.136, tale.138, 
                      tale.156, tale.159, 
                      tale.167) 

taleNamesTM <- c("093_Soltseva_Sestra.txt",
                 "106_Baba_Jaga_Zhikhar.txt","132_Norka_Zver.txt",
                 "136_Burja-Bogatyr_Ivan_Korovij_Syn.txt", "138_Ivan_Krestjankii.txt",
                 "156_Koschej_Bessmertnyj.txt","159_Maria_Marievna.txt",
                 "167_As_the_Wand_Orders.txt")

#taleSet.triplemove.nl <- list("triplemove", "nl", taleNamesTM, taleStructuresTM)
taleSet.triplemove.lem <- list("triplemove", "lem", taleNamesTM, taleStructuresTM)


taleStructuresWM <- c(tale.093, tale.095, tale.098, tale.106,
                      tale.132, 
                      tale.136, tale.138, 
                      tale.156, tale.159, 
                      tale.167) 

taleNamesWM <- c("093_Soltseva_Sestra.txt","095_Morozko.txt", "098_Doch_Padcheritsa.txt", 
                 "106_Baba_Jaga_Zhikhar.txt","132_Norka_Zver.txt",
                 "136_Burja-Bogatyr_Ivan_Korovij_Syn.txt", "138_Ivan_Krestjankii.txt",
                 "156_Koschej_Bessmertnyj.txt","159_Maria_Marievna.txt",
                 "167_As_the_Wand_Orders.txt")

#taleSet.triplemove.nl <- list("triplemove", "nl", taleNamesTM, taleStructuresTM)
taleSet.w.lem <- list("triplemove", "lem", taleNamesWM, taleStructuresWM)



#cases <- list(taleSet.triplemove.nl, taleSet.triplemove.lem, taleSet.doublemove.nl, taleSet.doublemove.lem, taleSet.singlemove2.nl, taleSet.singlemove2.lem, 
#              taleSet.cleanAll.nl, taleSet.cleanAll.lem, taleSet.all.nl, taleSet.all.lem)

cases <- list(taleSet.w.lem, taleSet.triplemove.lem, taleSet.doublemove.lem, taleSet.singlemove2.lem,  taleSet.singlemove.lem, taleSet.cleanAll.lem, taleSet.all.lem)

cases <- list(taleSet.singlemove.lem)
setwd("/db/propptester/fresh")
# setwd("/Users/smalec/propptest/fresh")
#getwd()
sourceDir <- "afanlem_all"
targetDir <- "afanlem"
resultsDir <- "results"
training <- c("BNC") #, "NOB") # BNC training or no BNC
gv <- c("1") #, "0") # graded vectors? yes, no
#target <- "afanlem"
resultData <- c()
#for (c in cases[1]) {
c <- cases[1]
c <- taleSet.singlemove.lem
print(paste(as.character(unlist(c, recursive=FALSE)[2])))
move <- as.character(unlist(c, recursive=FALSE)[1])
print(move)
lemNull <- as.character(unlist(c, recursive=FALSE)[2])
print(lemNull)
print(paste(unlist(c[2])))
print(length(as.character(unlist(c, recursive=FALSE)[4])))
n <- length(unlist(c[4])) #^2-2 
#print(getIndex(unlist(c[3]), tale.idx))
#print(n)
#print(n)
#for (m in 1:n) {
m <- 1
#for (g in gv) {
g <- "1"
#for (tr in training) {
tr <- "BNC"
system(paste("rm ", targetDir, "/*", sep="")) # remove old
tale.idx <- as.vector(sample(length(unlist(c[4]))))
tale.idx <- tale.idx[1:as.integer(length(tale.idx)*1)]
print("##############")
getIndex <- function(vec, x) { vec[x] } 
print(paste(move, lemNull, g, m, tr))
print(getIndex(unlist(c[3]), tale.idx))
print(getIndex(unlist(c[4]), tale.idx))
taleNames.orig <- getIndex(unlist(c[3]), tale.idx)
taleNames <- substr(taleNames.orig, start=0, stop=(nchar(taleNames.orig) -4))
print(taleNames)
taleStructures <- getIndex(unlist(c[4]), tale.idx)
dm <- stringdistmatrix(a = taleStructures, b = taleStructures, method = "jw", useNames = TRUE)
dm <- 1/(1+dm)
cleanStruct <- function(v) { str_replace_all(v, "_", "") }
print(sapply(taleStructures, cleanStruct))
taleStructNames <- sapply(taleStructures, cleanStruct)

plot(hclust(dist(dm), method="centroid"))
taleStructNameNames <- c()
for (tn in 1:length(taleNames)) {
  tns <- c(paste(taleNames[tn], "_", taleStructNames[tn], sep=""))
  taleStructNameNames <- c(taleStructNameNames, tns)
  syscom1 <- paste("cp ", sourceDir, "/", taleNames[tn], ".txt ", targetDir, "/", taleNames[tn], ".txt", sep="")
  print(syscom1)
  system(syscom1)
}
#taleNames <- str_replace_all(paste0(taleStructures, collapse = ""), "strReplacement", "")
#colnames(dm) <- taleStructNameNames
#rownames(dm) <- taleStructNameNames
#colnames(dm) <- taleNames.orig
#rownames(dm) <- taleNames.orig
colnames(dm) <- taleNames
rownames(dm) <- taleNames
####goldstandard.hclust <- hclust(d = dist(dm), members = NULL, method = "complete")
#plot(hclust(d = dist(dm), members = NULL, method = "complete"))

#setwd("/home/kingfish/Propp/afanFRESH")
#talesList <- list.files()
input <- c()
for (skazka in taleNames) {
  for (maerchen in taleNames) {
    print(paste("afanlem/", skazka, ".txt|afanlem/", maerchen, ".txt", sep=""))
    si <- paste("afanlem/", skazka, ".txt|afanlem/", maerchen, ".txt", sep="")
    input <- c(input, si)
  }
}
print(input)
writeVec(dat = input, fn = "afanInput1.txt")
system("sed 1d afanInput1.txt > afanInput.txt")
system("rm afanInput1.txt")
system("head afanInput.txt")
print("######HOOKERS####")
print(getwd())
#if (tr == "BNC") {
#  if (g == "1") {
print("removing old index")
system("rm -r tale_index")
print("building new index")
system("java -Xmx30G -cp ../semanticvectors/target/semanticvectors-5.9.jar pitt.search.lucene.IndexFilePositions afanlem")
print("moving index to target location")
system("mv positional_index tale_index")
print("training semanticvectors from sliding window BNC")
system(" java -Xmx60G -cp ../semanticvectors/target/semanticvectors-5.9.jar pitt.search.semanticvectors.BuildIndex -luceneindexpath tale_index -vectortype binary -minfrequency 1 -filteroutnumbers -maxnonalphabetchars -1 -maxfrequency 100000000 -dimension 32000 -termweight idf -initialtermvectors ../bnc/termtermvectors.bin -trainingcycles 2 -elementalmethod contenthash -docindexing incremental -stoplistfile smartstop.txt")
#genSentVecLemBNC.sh
print("building graded sentencevectors with probabilistic normalization")
system("java -cp ../semanticvectors/target/semanticvectors-5.9.jar pitt.search.semanticvectors.orthography.SentenceVectors -luceneindexpath tale_index -trainingcycles 2 -initialtermvectors ../bnc/termtermvectors.bin -vectortype binary -dimension 32000 -elementalmethod contenthash -termweight idf")
#system(" java -cp ../semanticvectors-5.9.jar pitt.search.semanticvectors.CompareTermsBatch -queryvectorfile sentencevectors.bin < afanInput.txt &> output.txt")
system("./compCall.sh")
################# for no GV, use docvectors.bin
#system("./processSMPlem.sh")
system("more output.txt | sed -e 's/Score = //g' | sed -e 's/\\. Terms: /\\|/g' > outputDat.txt")
system("more outputDat.txt | sed -e 's/afanlem\\///g' > output.txt")
system("more output.txt | grep '|' > outputDat.txt")
########## system("rm output.txt")
print("calculating results")
system("cat outputDat.txt")
rawTaleMatSMPlem <- read.table(file = "outputDat.txt", sep="|", header = FALSE, stringsAsFactors = FALSE)
rawTaleMatSMPlem[[2]] <- str_replace(string = rawTaleMatSMPlem[[2]], pattern = "afanlem/", replacement = "")
rawTaleMatSMPlem[[3]] <- str_replace(string = rawTaleMatSMPlem[[3]], pattern = "afanlem/", replacement = "")
print("####################################")
length(rawTaleMatSMPlem)
rawTaleMatSMPlem
rNames <- unique(rawTaleMatSMPlem[[2]])
cNames <- unique(rawTaleMatSMPlem[[3]])
res.matSMPlem <- matrix(dimnames = list(rNames, cNames), nrow = length(rNames), ncol = length(cNames))
res.matSMPlem
rawTaleMatSMPlem
for (index in 1:(nrow(rawTaleMatSMPlem))) {
  for (c in 1:(length(cNames))) {
    for (r in 1:(length(rNames))) {
      matRowName <- rNames[[r]]
      matColumnName <- cNames[[c]]
      rawTaleNameRow <- rawTaleMatSMPlem[index, 2]
      rawTaleNameColumn <- rawTaleMatSMPlem[index, 3]
      if ((matRowName == rawTaleNameRow) && (matColumnName == rawTaleNameColumn)) { 
        print("#######################################################")
        print("YAY")                                                                                    
        print(paste0("mat: ", matRowName, " ", matColumnName, " raw: ", rawTaleNameRow, " ", rawTaleNameColumn))   #try(res.mat[r, c] <- rawTaleMat[index, 1])
        res.matSMPlem[r, c] <- rawTaleMatSMPlem[index, 1] #rawTaleMatSM[[1]][index]
        print(paste0("post: rawTaleMatSMPlem[[1]][[index][[index]]:", rawTaleMatSMPlem[index, 1]))
        print(paste0("pre: res.matV[r, c]: ", res.matSMPlem[r, c]))
      } else { }
    } 
  }
}
res.matSMPlem
dm <- stringdistmatrix(a = taleStructures, b = taleStructures, useNames = TRUE, useBytes = TRUE, method="jw") # weight=c(.5, .6, .3, .1)) 
dm <- 1/(1+dm)
print("######## dm")
print("gold standard")
colnames(dm) <- taleNames
rownames(dm) <- taleNames
print("###########")
print("taleNames")
print(taleNames)

#rownames(dm) <- taleNamesSM
#colnames(dm) <- taleNamesSM
#dm <- stringdistmatrix(a = taleStructures, b = taleStructures, method="hamming")
spearman <- cor(as.vector(as.numeric(res.matSMPlem)), as.vector(as.numeric(dm)), method = "spearman", use = "everything") # .309
######### no stoplist or BNC/TASA: .2542832 ---   just predicates of single move tales
pearson <- cor(as.vector(as.numeric(res.matSMPlem)), as.vector(as.numeric(dm)), method = "pearson", use = "everything") #.705
resultDat <- c()
resultDat <- c(move, lemNull, tr, g, n, spearman, pearson)
print("########RESULTS")
print(as.character(paste("results:", resultDat, sep="")))
#resultData <- c(resultData, resultDat)
#print(resultData)
#writeVec(dat = resultData, fn="proppAfan.csv")

print(paste("goodresults|spearman:",spearman, "|pearson:", pearson, sep=""))

