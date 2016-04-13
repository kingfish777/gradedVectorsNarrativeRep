
library(TraMineR)
library(stringdist)
library(stringr)
library(tm)
setwd("/home/kingfish/Propp/goldStandard")
text <- system.file("texts", "txt", package="tm")
# read in corpus
corpus <- Corpus(DirSource())
#taleStructure <- list(100)
strReplacement = "" # strReplacement
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


taleStructures <- c(tale.093, tale.095, tale.098, tale.100, tale.101, 
                    tale.104, tale.105, tale.106, tale.108, tale.113, 
                    tale.114, tale.115, tale.125, tale.126, tale.127, 
                    tale.128, tale.131, tale.132, tale.133, tale.135, 
                    tale.136, tale.137, tale.138, tale.139, tale.140, 
                    tale.141, tale.143, tale.144, tale.145, tale.148,
                    tale.149, tale.150, tale.151, tale.152, tale.153,
                    tale.154, tale.155, tale.156, tale.159, tale.161, 
                    tale.162, tale.163, tale.164, tale.166, tale.167) 

taleNames <- c("093_Soltseva_Sestra.txt", "095_Morozko.txt", "098_Doch_Padcheritsa.txt", "100_Kroshechka-Khavroshechka.txt", "101_Burenushka.txt",
               "104_Vasilisa_Prekrasnaja.txt", "105_Baba_Jaga_Zamoryshek.txt","106_Baba_Jaga_Zhikhar.txt","108_Witch_and_Ivan.txt","113_Magic_Swan-Geese.txt",
               "114_Knjaz_Danila.txt", "115_Pravda_Krivda.txt","125_Ivan_Tsarevich.txt","126_Father_Brass.txt","127_Kupecheskaja_Doch_i_Sluzhanka.txt",
               "128_Kingdoms_of_Gold_Copper_and_Silver.txt","131_Frolka-Siden.txt","132_Norka_Zver.txt","133_Pokatigoroshek.txt","135_Ivan_Popolyov.txt",
               "136_Burja-Bogatyr_Ivan_Korovij_Syn.txt","137_Ivan_Bykovich.txt", "138_Ivan_Krestjankii.txt","139_Ivan_Suchenko.txt", "140_DawnDayEvening.txt",
               "141_Medvedko_Usynja_i_Dubynja-bogatyri.txt","143_Nadzej_Papov_Unuk.txt","144_letuchij_korabl.txt","145_Sem_Semionov.txt","148_Nikita-Kozhemjaka.txt",
               "149_Zmej_i_Tsygan.txt","150_Batrak_FarmHand.txt","151_Shabarsha.txt","152_Ivanko_Medvedko.txt", "153_Soldier_and_Princess.txt","154_Beglyj_Soldat_i_Chert.txt","155_Two_Ivans.txt","156_Koschej_Bessmertnyj.txt","159_Maria_Marievna.txt",
               "161_Prince_Ivan_i_Belyj_Poljanin.txt","162_Crystal_Mountain.txt","163_Bukhtan_Bukhtanovich.txt", "164_Cosmo_Get-Rich-Quick.txt","166_Emilja_Durak.txt", "167_As_the_Wand_Orders.txt")


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



#taleNames <- c("tale.93", "tale.95", "tale.98", "tale.100", "tale.101")

# taleNames <- c("tale.93", "tale.95", "tale.98", "tale.100", "tale.101", 
#                "tale.104", "tale.105", "tale.106", "tale.108", "tale.113", 
#                "tale.114", "tale.115", "tale.125", "tale.126", "tale.127", 
#                "tale.128", "tale.131", "tale.132", "tale.133", "tale.135", 
#                "tale.136", "tale.137", "tale.138", "tale.139", "tale.140",
#                "tale.141", "tale.143", "tale.144", "tale.145", "tale.148",
#                "tale.149", "tale.150", "tale.151", "tale.152", "tale.153", 
#                "tale.154", "tale.155", "tale.156", "tale.159", "tale.161", 
#                "tale.162", "tale.163", "tale.164", "tale.166", "tale.167") 

#complete set
dm <- stringdistmatrix(a = taleStructures, b = taleStructures, method = "jw", useNames = TRUE)
colnames(dm) <- taleNames
rownames(dm) <- taleNames

#single move subset
dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, method = "jw", useNames = TRUE)
dm <- 1/(1+dm)
colnames(dm) <- taleNamesSM
rownames(dm) <- taleNamesSM


#tale subset version #2
dm <- stringdistmatrix(a = taleStructuresSM2, b = taleStructuresSM2, method = "jw", useNames = TRUE)
dm <- 1/(1+dm)
colnames(dm) <- taleNamesSM
rownames(dm) <- taleNamesSM
colnames(dm) <- taleNamesSM2
rownames(dm) <- taleNamesSM2

hc.dm <- hclust(d <- dist(dm), method = "ward.D")
hc.dm <- hclust(d <- dist(dm), method = "ward.D2")
plot(hc.dm, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using Jaro–Winkler distance as Similarity Metric)")
#length(dm)


plot(hc.dm, hang=1, axes = TRUE, ann=TRUE, main = "Cluster
     Dendrogram of Proppian Narreme Matrix",
     xlab="Tale Name", ylab = "DNM Distance")
phyl <- as.phylo(hc.dm)

library(ape)

plot(phyl, type="fan", edge.col=c("blue", "green", "red")[c(TRUE,
                                                            FALSE) + 1 + (phyl$edge.length > 20)])


cor(as.numeric(1/(1+dm)), as.numeric(res.matSMPlem), method="spearman")
#dm <- stringdistmatrix(a = taleStructures, b = taleStructures, method = "jw", useNames = TRUE)
#dm
#hc.dm <- hclust(d <- dist(dm), method = "ward.D")
#hc.dm <- hclust(d <- dist(dm), method = "ward.D2")
#plot(hc.dm, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using Jaro–Winkler distance as Similarity Metric)")

#stringdistmatrix(a, b, method = c("osa", "lv", "dl", "hamming", "lcs",
#                                  "qgram", "cosine", "jaccard", "jw", "soundex"), useBytes = FALSE,
#                 weight = c(d = 1, i = 1, s = 1, t = 1), maxDist = Inf, q = 1, p = 0,
#                 useNames = c("none", "strings", "names"), ncores = 1, cluster = NULL,
#                 nthread = getOption("sd_num_thread"))  weight = c(d = 1, i = 1, s = 1, t = 1)
taleStructures.orig <- taleStructures
#taleStructures <- tolower(taleStructures)
dm <- stringdistmatrix(a = taleStructures, b = taleStructures, weight=c(1,1,1,0.5))
head(dm)
dm.w2 <- stringdistmatrix(a = taleStructures, b = taleStructures,weight=c(0.5,1,1,1))
head(dm.w2)
dm.w3 <- stringdistmatrix(a = taleStructures, b = taleStructures,weight=c(1,1,1,1))
head(dm.w3)
dm.lv.b <- stringdistmatrix(a = taleStructures, b = taleStructures, method="lv", useNames = TRUE, useBytes = TRUE)
head(dm.lv.b)
dm.osa.b <- stringdistmatrix(a = taleStructures, b = taleStructures, method="osa", useNames = TRUE, useBytes = TRUE)
head(dm.osa.b)
dm <- stringdistmatrix(a = taleStructures, b = taleStructures, method="dl", useNames = TRUE, useBytes = TRUE)
head(dm.dl.b)
dm.hamming.b <- stringdistmatrix(a = taleStructures, b = taleStructures, method="hamming", useNames = TRUE, useBytes = TRUE, weight=c(1, 1, 1, 1))
dm <- stringdistmatrix(a = taleStructures, b = taleStructures, method="hamming", useNames = TRUE, useBytes = TRUE)
dm <- 1/(dm+1)
head(dm.hamming.b)
dm.lcs.b <- stringdistmatrix(a = taleStructures, b = taleStructures, method="lcs", useNames = TRUE, useBytes = TRUE)
head(dm.lcs.b)
dm.jw.b <- stringdistmatrix(a = taleStructures, b = taleStructures, method="jw", p=.01, useNames = TRUE, useBytes = TRUE)
head(dm.lcs.b)
dm.qgram.q1.b <- stringdistmatrix(a = taleStructures, b = taleStructures, method="qgram",p=.01, q=1, useNames = TRUE, useBytes = TRUE)
head(dm.qgram.q1.b)
dm.qgram.q2.b <- stringdistmatrix(a = taleStructures, b = taleStructures, method="qgram",p=.01, q=2, useNames = TRUE, useBytes = TRUE)
head(dm.qgram.q2.b)
dm.qgram.q3.b <- stringdistmatrix(a = taleStructures, b = taleStructures, method="qgram",p=.01, q=3, useNames = TRUE, useBytes = TRUE)
head(dm.qgram.q3.b)

dm.lv.nb <- stringdistmatrix(a = taleStructures, b = taleStructures, method="lv", useNames = TRUE, useBytes = FALSE)
dm.osa.nb <- stringdistmatrix(a = taleStructures, b = taleStructures, method="osa", useNames = TRUE, useBytes = FALSE)
dm.dl.nb <- stringdistmatrix(a = taleStructures, b = taleStructures, method="dl", useNames = TRUE, useBytes = FALSE)
dm.hamming.nb <- stringdistmatrix(a = taleStructures, b = taleStructures, method="hamming", useNames = TRUE, useBytes = FALSE)
dm.lcs.nb <- stringdistmatrix(a = taleStructures, b = taleStructures, method="lcs", useNames = TRUE, useBytes = FALSE)
dm.jw.nb <- stringdistmatrix(a = taleStructures, b = taleStructures, method="jw", p=.01, useNames = TRUE, useBytes = FALSE)
dm.qgram.q1.nb <- stringdistmatrix(a = taleStructures, b = taleStructures, method="qgram",p=.01, q=1, useNames = TRUE, useBytes = FALSE)
dm.qgram.q2.nb <- stringdistmatrix(a = taleStructures, b = taleStructures, method="qgram",p=.01, q=2, useNames = TRUE, useBytes = FALSE)
dm.qgram.q3.nb <- stringdistmatrix(a = taleStructures, b = taleStructures, method="qgram",p=.01, q=3, useNames = TRUE, useBytes = FALSE)


########################3 single moves


dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, weight=c(1,1,1,0.5))
head(dm)
cor(as.numeric(1/(1+dm)), y, method="spearman")
#dm.w2 <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM,weight=c(0.5,1,1,1))
#head(dm.w2)
#dm.w3 <- stringdistmatrix(a = taleStructures, b = taleStructures,weight=c(1,1,1,1))
#head(dm.w3)
dm.lv.b <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, method="lv", useNames = TRUE, useBytes = TRUE)
head(dm.lv.b)
cor(as.numeric(1/(1+dm.lv.b)), y, method="spearman")
dm.osa.b <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, method="osa", useNames = TRUE, useBytes = TRUE)
head(dm.osa.b)
cor(as.numeric(1/(1+dm.osa.b)), y, method="spearman")
#dm <- stringdistmatrix(a = taleStructures, b = taleStructures, method="dl", useNames = TRUE, useBytes = TRUE)
#head(dm.dl.b)
dm.hamming.b <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, method="hamming", useNames = TRUE, useBytes = TRUE) #, weight=c(1, 1, 1, 1))
dm.hamming.b
cor(as.numeric(1/(1+dm.lv.b)), y, method="spearman")
#dm <- stringdistmatrix(a = taleStructures, b = taleStructures, method="hamming", useNames = TRUE, useBytes = TRUE)
#dm <- 1/(dm+1)
#head(dm.hamming.b)
dm.lcs.b <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, method="lcs", useNames = TRUE, useBytes = TRUE)
head(dm.lcs.b)
cor(as.numeric(1/(1+dm.lcs.b)), y, method="spearman")
dm.jw.b <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, method="jw", p=.01, useNames = TRUE, useBytes = TRUE)
cor(as.numeric(1/(1+dm.lcs.b)), y, method="spearman")
dm.qgram.q1.b <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, method="qgram",p=.00, q=1, useNames = TRUE, useBytes = TRUE)
head(dm.qgram.q1.b)
cor(as.numeric(1/(dm.qgram.q1.b+1)), y, method="spearman")


##################################

dm.2 <- 1/(dm+1)
dm.lv.b.2 <- 1/(dm.lv.b+1)
dm.osa.b.2 <- 1/(dm.osa.b+1)
dm.dl.b.2 <- 1/(dm.dl.b+1)
dm.hamming.b.2 <- 1/(dm.hamming.b+1)
dm.lcs.b.2 <- 1/(dm.lcs.b+1)
dm.jw.b.2 <- 1/(dm.jw.b+1)
dm.qgram.q1.b.2 <- 1/(dm.qgram.q1.b+1)  
dm.qgram.q2.b.2 <- 1/(dm.qgram.q2.b+1) 
dm.qgram.q3.b.2 <- 1/(dm.qgram.q3.b+1) 

dm.lv.nb.2 <- 1/(dm.lv.nb+1)
dm.osa.nb.2 <- 1/(dm.osa.nb+1)
dm.dl.nb.2 <- 1/(dm.dl.nb+1)
dm.hamming.nb.2 <- 1/(dm.hamming.nb+1)
dm.lcs.nb.2 <- 1/(dm.lcs.nb+1)
dm.jw.nb.2 <- 1/(dm.jw.nb+1)
dm.qgram.q1.nb.2 <- 1/(dm.qgram.q1.nb+1)  
dm.qgram.q2.nb.2 <- 1/(dm.qgram.q2.nb+1) 
dm.qgram.q3.nb.2 <- 1/(dm.qgram.q3.nb+1) 

colnames(dm) <- taleNames
rownames(dm) <- taleNames

hc.dm <- hclust(d <- dist(dm), method = "centroid")
hc.dm <- hclust(d <- dist(dm), method = "ward.D2")
plot(hc.dm, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using Levenshtein string distance as Similarity Metric)")



################################
setwd("/home/kingfish/Propp/afanFRESH")
text <- system.file("texts", "txt", package="tm")
# read in corpus
corpus <- Corpus(DirSource())
#print(corpus[[3]])
#corpus <- tm_map(FUN = removeNumbers, corpus)
#corpus <- tm_map(FUN = removePunctuation, corpus)
#corpus <- tm_map(FUN = stemDocument, corpus)
#corpus <- tm_map(corpus, removeWords, stopwords(kind = "en"))
corpus[[1]]

corpus
dtm <- DocumentTermMatrix(corpus)
dtm
head(dtm$dimnames$Terms)
#dtm <- removeSparseTerms(dtm, .8)
dtm.mat <- as.matrix(dtm, rownames.force = TRUE)
plot(hclust(dist(dtm.mat)))

#####################################

setwd("/home/kingfish/Propp/afanFRESH")
talesList <- list.files()

for (skazka in talesList) {
  for (maerchen in talesList) {
    print(paste("../fresh/afanFRESH/", skazka, "|../fresh/afanFRESH/", maerchen, sep=""))
  }
}

setwd("/db/propptest/fresh/afanSkazkiSingleMove/")
talesList <- list.files()

for (skazka in talesList) {
  for (maerchen in talesList) {
    print(paste("afanSkazkiSingleMove/", skazka, "|afanSkazkiSingleMove/", maerchen, sep=""))
  }
}


##########################################
library(stringr)
setwd("/home/kingfish/Propp")
rawTaleMat <- read.table(file = "outputdat.txt", sep="|", header = FALSE, stringsAsFactors = FALSE)
rawTaleMat
class(rawTaleMat)
#str_replace(string = rawTaleMat[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMat[[2]] <- str_replace(string = rawTaleMat[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMat[[3]] <- str_replace(string = rawTaleMat[[3]], pattern = "../fresh/afanFRESH/", replacement = "")
#rawTaleMat[[2]][1]

length(rawTaleMat)
rawTaleMat
rNames <- unique(rawTaleMat[[2]])
cNames <- unique(rawTaleMat[[3]])
res.matNW <- matrix(dimnames = list(rNames, cNames), nrow = length(rNames), ncol = length(cNames))
res.matNW

for (index in 1:(nrow(rawTaleMat))) {
  for (c in 1:(length(cNames))) {
    for (r in 1:(length(rNames))) {
      matRowName <- rNames[[r]]
      matColumnName <- cNames[[c]]
      rawTaleNameRow <- rawTaleMat[index, 2]
      rawTaleNameColumn <- rawTaleMat[index, 3]
      if ((matRowName == rawTaleNameRow) && (matColumnName == rawTaleNameColumn)) { 
        print("#######################################################")
        print("YAY")                                                                                    
        print(paste0("mat: ", matRowName, " ", matColumnName, " raw: ", rawTaleNameRow, " ", rawTaleNameColumn))   #try(res.mat[r, c] <- rawTaleMat[index, 1])
        res.matNW[r, c] <- rawTaleMat[[1]][index]
        print(paste0("post: rawTaleMat[[1]][[index][[index]]:", rawTaleMat[index, 1]))
        print(paste0("pre: res.mat[r, c]: ", res.matNW[r, c]))
      }
    } 
  }
}


hc.dm.svNW <- hclust(d <- dist(res.matNW), method = "ward.D2")
#hc.dm <- hclust(d <- dist(dm), method = "ward.D2")
hc.dm.svNW <- hclust(d <- dist(res.matNW), method = "complete")
plot(hc.dm.svNW, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using sentence term vector SUM as similarity metric)")


#########################################################


##########################################
library(stringr)
setwd("/home/kingfish/Propp")
rawTaleMat <- read.table(file = "outputdatWindow.txt", sep="|", header = FALSE, stringsAsFactors = FALSE)
rawTaleMat
class(rawTaleMat)
#str_replace(string = rawTaleMat[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMat[[2]] <- str_replace(string = rawTaleMat[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMat[[3]] <- str_replace(string = rawTaleMat[[3]], pattern = "../fresh/afanFRESH/", replacement = "")
#rawTaleMat[[2]][1]

length(rawTaleMat)
rawTaleMat
rNames <- unique(rawTaleMat[[2]])
cNames <- unique(rawTaleMat[[3]])
res.mat <- matrix(dimnames = list(rNames, cNames), nrow = length(rNames), ncol = length(cNames))
res.mat
rawTaleMat
for (index in 1:(nrow(rawTaleMat))) {
 for (c in 1:(length(cNames))) {
  for (r in 1:(length(rNames))) {
      matRowName <- rNames[[r]]
      matColumnName <- cNames[[c]]
      rawTaleNameRow <- rawTaleMat[index, 2]
      rawTaleNameColumn <- rawTaleMat[index, 3]
      if ((matRowName == rawTaleNameRow) && (matColumnName == rawTaleNameColumn)) { 
        print("#######################################################")
        print("YAY")                                                                                    
        print(paste0("mat: ", matRowName, " ", matColumnName, " raw: ", rawTaleNameRow, " ", rawTaleNameColumn))   #try(res.mat[r, c] <- rawTaleMat[index, 1])
        res.mat[r, c] <- rawTaleMat[[1]][index]
        print(paste0("post: rawTaleMat[[1]][[index][[index]]:", rawTaleMat[index, 1]))
        print(paste0("pre: res.mat[r, c]: ", res.mat[r, c]))
      }
    } 
  }
}
res.mat

hc.dm.sv <- hclust(d <- dist(res.mat), method = "ward.D2")
#hc.dm <- hclust(d <- dist(dm), method = "ward.D2")
hc.dm.sv <- hclust(d <- dist(res.mat), method = "complete")
plot(hc.dm.sv, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using sentence term vector SUM as similarity metric)")


#############################



##########################################
library(stringr)
setwd("/home/kingfish/Propp")
rawTaleMatV <- read.table(file = "outputdatVerbs.txt", sep="|", header = FALSE, stringsAsFactors = FALSE)
rawTaleMatV
class(rawTaleMatV)
#str_replace(string = rawTaleMat[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMatV[[2]] <- str_replace(string = rawTaleMatV[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMatV[[3]] <- str_replace(string = rawTaleMatV[[3]], pattern = "../fresh/afanFRESH/", replacement = "")
#rawTaleMat[[2]][1]

length(rawTaleMatV)
rawTaleMatV
rNames <- unique(rawTaleMat[[2]])
cNames <- unique(rawTaleMat[[3]])
res.matV <- matrix(dimnames = list(rNames, cNames), nrow = length(rNames), ncol = length(cNames))
res.matV
rawTaleMat
for (index in 1:(nrow(rawTaleMatV))) {
  for (c in 1:(length(cNames))) {
    for (r in 1:(length(rNames))) {
      matRowName <- rNames[[r]]
      matColumnName <- cNames[[c]]
      rawTaleNameRow <- rawTaleMat[index, 2]
      rawTaleNameColumn <- rawTaleMat[index, 3]
      if ((matRowName == rawTaleNameRow) && (matColumnName == rawTaleNameColumn)) { 
        print("#######################################################")
        print("YAY")                                                                                    
        print(paste0("mat: ", matRowName, " ", matColumnName, " raw: ", rawTaleNameRow, " ", rawTaleNameColumn))   #try(res.mat[r, c] <- rawTaleMat[index, 1])
        res.matV[r, c] <- rawTaleMatV[[1]][index]
        print(paste0("post: rawTaleMatV[[1]][[index][[index]]:", rawTaleMatV[index, 1]))
        print(paste0("pre: res.matV[r, c]: ", res.matV[r, c]))
      }
    } 
  }
}
res.matV

hc.dm.svV <- hclust(d <- dist(res.matV), method = "ward.D2")
#hc.dm <- hclust(d <- dist(dm), method = "ward.D2")
hc.dm.svV <- hclust(d <- dist(res.matV), method = "complete")
plot(hc.dm.svV, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using sentence term vector SUM as similarity metric)")

###############################

library(stringr)
#setwd("/home/kingfish/Propp")
setwd("/db/propptest/fresh")
system("ls")
rawTaleMatV <- read.table(file = "outputdatVerbs.txt", sep="|", header = FALSE, stringsAsFactors = FALSE)
rawTaleMatV
class(rawTaleMatV)
#str_replace(string = rawTaleMat[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMatV[[2]] <- str_replace(string = rawTaleMatV[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMatV[[3]] <- str_replace(string = rawTaleMatV[[3]], pattern = "../fresh/afanFRESH/", replacement = "")
#rawTaleMat[[2]][1]

length(rawTaleMatV)
rawTaleMatV
rNames <- unique(rawTaleMat[[2]])
cNames <- unique(rawTaleMat[[3]])
res.matV <- matrix(dimnames = list(rNames, cNames), nrow = length(rNames), ncol = length(cNames))
res.matV
rawTaleMat
for (index in 1:(nrow(rawTaleMatV))) {
  for (c in 1:(length(cNames))) {
    for (r in 1:(length(rNames))) {
      matRowName <- rNames[[r]]
      matColumnName <- cNames[[c]]
      rawTaleNameRow <- rawTaleMat[index, 2]
      rawTaleNameColumn <- rawTaleMat[index, 3]
      if ((matRowName == rawTaleNameRow) && (matColumnName == rawTaleNameColumn)) { 
        print("#######################################################")
        print("YAY")                                                                                    
        print(paste0("mat: ", matRowName, " ", matColumnName, " raw: ", rawTaleNameRow, " ", rawTaleNameColumn))   #try(res.mat[r, c] <- rawTaleMat[index, 1])
        res.matV[r, c] <- rawTaleMatV[[1]][index]
        print(paste0("post: rawTaleMatV[[1]][[index][[index]]:", rawTaleMatV[index, 1]))
        print(paste0("pre: res.matV[r, c]: ", res.matV[r, c]))
      }
    } 
  }
}
res.matV

hc.dm.svV <- hclust(d <- dist(res.matV), method = "ward.D2")
#hc.dm <- hclust(d <- dist(dm), method = "ward.D2")
hc.dm.svV <- hclust(d <- dist(res.matV), method = "complete")
plot(hc.dm.svV, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using sentence term vector SUM as similarity metric)")

###############################

library(stringr)
#setwd("/home/kingfish/Propp")
setwd("/db/propptest/fresh")
system("ls")
rawTaleMatV <- read.table(file = "outputdatVerbs.txt", sep="|", header = FALSE, stringsAsFactors = FALSE)
rawTaleMatV
class(rawTaleMatV)
#str_replace(string = rawTaleMat[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMatV[[2]] <- str_replace(string = rawTaleMatV[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMatV[[3]] <- str_replace(string = rawTaleMatV[[3]], pattern = "../fresh/afanFRESH/", replacement = "")
#rawTaleMat[[2]][1]

length(rawTaleMatV)
rawTaleMatV
rNames <- unique(rawTaleMat[[2]])
cNames <- unique(rawTaleMat[[3]])
res.matV <- matrix(dimnames = list(rNames, cNames), nrow = length(rNames), ncol = length(cNames))
res.matV
rawTaleMat
for (index in 1:(nrow(rawTaleMatV))) {
  for (c in 1:(length(cNames))) {
    for (r in 1:(length(rNames))) {
      matRowName <- rNames[[r]]
      matColumnName <- cNames[[c]]
      rawTaleNameRow <- rawTaleMatV[index, 2]
      rawTaleNameColumn <- rawTaleMatV[index, 3]
      if ((matRowName == rawTaleNameRow) && (matColumnName == rawTaleNameColumn)) { 
        print("#######################################################")
        print("YAY")                                                                                    
        print(paste0("mat: ", matRowName, " ", matColumnName, " raw: ", rawTaleNameRow, " ", rawTaleNameColumn))   #try(res.mat[r, c] <- rawTaleMat[index, 1])
        res.matV[r, c] <- rawTaleMatV[[1]][index]
        print(paste0("post: rawTaleMatV[[1]][[index][[index]]:", rawTaleMatV[index, 1]))
        print(paste0("pre: res.matV[r, c]: ", res.matV[r, c]))
      }
    } 
  }
}
res.matV

hc.dm.svV <- hclust(d <- dist(res.matV), method = "ward.D2")
#hc.dm <- hclust(d <- dist(dm), method = "ward.D2")
hc.dm.svV <- hclust(d <- dist(res.matV), method = "complete")
plot(hc.dm.svV, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using sentence term vector SUM as similarity metric)")

###############################

library(stringr)
#setwd("/home/kingfish/Propp")
setwd("/db/propptest/fresh")
system("ls")
rawTaleMatSM <- read.table(file = "outputSM.txt", sep="|", header = FALSE, stringsAsFactors = FALSE)
rawTaleMatSM
class(rawTaleMatSM)
#str_replace(string = rawTaleMat[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMatSM[[2]] <- str_replace(string = rawTaleMatSM[[2]], pattern = "afanSkazkiSingleMove/", replacement = "")
rawTaleMatSM[[3]] <- str_replace(string = rawTaleMatSM[[3]], pattern = "afanSkazkiSingleMove/", replacement = "")
#rawTaleMat[[2]][1]

length(rawTaleMatSM)
rawTaleMatSM
rNames <- unique(rawTaleMatSM[[2]])
cNames <- unique(rawTaleMatSM[[3]])
res.matSM <- matrix(dimnames = list(rNames, cNames), nrow = length(rNames), ncol = length(cNames))
res.matSM
rawTaleMat
for (index in 1:(nrow(rawTaleMatSM))) {
  for (c in 1:(length(cNames))) {
    for (r in 1:(length(rNames))) {
      matRowName <- rNames[[r]]
      matColumnName <- cNames[[c]]
      rawTaleNameRow <- rawTaleMatSM[index, 2]
      rawTaleNameColumn <- rawTaleMatSM[index, 3]
      if ((matRowName == rawTaleNameRow) && (matColumnName == rawTaleNameColumn)) { 
        print("#######################################################")
        print("YAY")                                                                                    
        print(paste0("mat: ", matRowName, " ", matColumnName, " raw: ", rawTaleNameRow, " ", rawTaleNameColumn))   #try(res.mat[r, c] <- rawTaleMat[index, 1])
        res.matSM[r, c] <- rawTaleMatSM[index, 1] #rawTaleMatSM[[1]][index]
        print(paste0("post: rawTaleMatV[[1]][[index][[index]]:", rawTaleMatSM[index, 1]))
        print(paste0("pre: res.matV[r, c]: ", res.matSM[r, c]))
      }
    } 
  }
}
res.matSM
dm.hamming <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, weight=c(.5, .5, 1, 1))

###############################

library(stringr)
#setwd("/home/kingfish/Propp")
setwd("/db/propptest/fresh")
system("ls")
rawTaleMatSMPN <- read.table(file = "outputPN.txt", sep="|", header = FALSE, stringsAsFactors = FALSE)
rawTaleMatSMPN
class(rawTaleMatSM)
#str_replace(string = rawTaleMat[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMatSMPN[[2]] <- str_replace(string = rawTaleMatSMPN[[2]], pattern = "afanSkazkiSingleMove/", replacement = "")
rawTaleMatSMPN[[3]] <- str_replace(string = rawTaleMatSMPN[[3]], pattern = "afanSkazkiSingleMove/", replacement = "")
#rawTaleMat[[2]][1]

length(rawTaleMatSMPN)
rawTaleMatSMPN
rNames <- unique(rawTaleMatSMPN[[2]])
cNames <- unique(rawTaleMatSMPN[[3]])
res.matSMPN <- matrix(dimnames = list(rNames, cNames), nrow = length(rNames), ncol = length(cNames))
res.matSMPN
rawTaleMatSMPN
for (index in 1:(nrow(rawTaleMatSMPN))) {
  for (c in 1:(length(cNames))) {
    for (r in 1:(length(rNames))) {
      matRowName <- rNames[[r]]
      matColumnName <- cNames[[c]]
      rawTaleNameRow <- rawTaleMatSMPN[index, 2]
      rawTaleNameColumn <- rawTaleMatSMPN[index, 3]
      if ((matRowName == rawTaleNameRow) && (matColumnName == rawTaleNameColumn)) { 
        print("#######################################################")
        print("YAY")                                                                                    
        print(paste0("mat: ", matRowName, " ", matColumnName, " raw: ", rawTaleNameRow, " ", rawTaleNameColumn))   #try(res.mat[r, c] <- rawTaleMat[index, 1])
        res.matSMPN[r, c] <- rawTaleMatSMPN[index, 1] #rawTaleMatSM[[1]][index]
        print(paste0("post: rawTaleMatSMPN[[1]][[index][[index]]:", rawTaleMatSMPN[index, 1]))
        print(paste0("pre: res.matV[r, c]: ", res.matSMPN[r, c]))
      }
    } 
  }
}
res.matSMPN
dm.hamming <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, weight=c(.5, .5, 1, 1))

###############################

library(stringr)
#setwd("/home/kingfish/Propp")
setwd("/db/propptest/fresh")
#system("ls")
rawTaleMatSMPlem <- read.table(file = "outputSMPlem.txt", sep="|", header = FALSE, stringsAsFactors = FALSE)
rawTaleMatSMPlem
class(rawTaleMatSMPlem)
#str_replace(string = rawTaleMat[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMatSMPlem[[2]] <- str_replace(string = rawTaleMatSMPlem[[2]], pattern = "afanlem/", replacement = "")
rawTaleMatSMPlem[[3]] <- str_replace(string = rawTaleMatSMPlem[[3]], pattern = "afanlem/", replacement = "")
#rawTaleMat[[2]][1]

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
      }
    } 
  }
}
res.matSMPlem
#dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, method = 'dl', weight=c(1, 1, 0.3, 0.1 ))
#dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, method = "hamming")
dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, method = "jw", useNames = TRUE)
dm <- 1/(1+dm)
colnames(dm) <- taleNamesSM
rownames(dm) <- taleNamesSM
#rownames(dm) <- taleNamesSM
#colnames(dm) <- taleNamesSM
#dm <- stringdistmatrix(a = taleStructures, b = taleStructures, method="hamming")
cor(as.vector(as.numeric(res.matSMPlem)), as.vector(as.numeric(dm)), method = "spearman", use = "everything") # .309
   ######### no stoplist or BNC/TASA: .2542832 ---   just predicates of single move tales
cor(as.vector(as.numeric(res.matSMPlem)), as.vector(as.numeric(dm)), method = "pearson", use = "everything") #.705
   ######### no stoplist or BNC/TASA: .91111095


hc.dm.SMPlem <- hclust(d <- dist(res.matSMPlem), method = "complete")
plot(hc.dm.SMPlem, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using BNC @ 32000 dimensions \n lemmatized predicates of all tales in appendix III)")
x=as.numeric(dm)
y=as.numeric(res.matSMPlem)
spearman = cor(y,x,method="spearman")
pearson = cor(y,x,method="pearson")
plot(y ~ x,xlim=c(-6,+6),ylim=c(-1,+2.5))
title("Correlation:")
abline(v=0)
abline(h=0)
lm1=lm(y ~ x)
abline(lm1,col="red")
legend("topleft",
       c("Red line: regression.",
         sprintf("Spearman: %.5f",spearman),
         sprintf("Pearson:   +%.5f",pearson)
       ))
x.m=x-mean(x)
y.m=y-mean(y)
str(summary(lm((y.m)~(x.m))))
chisq.test(y.m,x.m)

hc.dm <- hclust(d <- dist(dm), method = "complete")
plot(hc.dm, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using sentence term vector SUM as similarity metric)")
hc.dm <- hclust(d <- dist(dm), method = "complete")
plot(hc.dm, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology \n(gold standard for all tales in Appendix III)")

#outputSMPlem.txt
#############################

library(stringr)
#setwd("/home/kingfish/Propp")
setwd("/db/propptest/fresh")
#system("ls")
rawTaleMatTASA <- read.table(file = "outputTASA.txt", sep="|", header = FALSE, stringsAsFactors = FALSE)
rawTaleMatTASA
class(rawTaleMatTASA)
#str_replace(string = rawTaleMat[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMatTASA[[2]] <- str_replace(string = rawTaleMatTASA[[2]], pattern = "afanSkazkiSingleMove/", replacement = "")
rawTaleMatTASA[[3]] <- str_replace(string = rawTaleMatTASA[[3]], pattern = "afanSkazkiSingleMove/", replacement = "")
#rawTaleMat[[2]][1]

length(rawTaleMatTASA)
rawTaleMatTASA
rNames <- unique(rawTaleMatTASA[[2]])
cNames <- unique(rawTaleMatTASA[[3]])
res.matTASA <- matrix(dimnames = list(rNames, cNames), nrow = length(rNames), ncol = length(cNames))
res.matTASA
rawTaleMat
for (index in 1:(nrow(rawTaleMatTASA))) {
  for (c in 1:(length(cNames))) {
    for (r in 1:(length(rNames))) {
      matRowName <- rNames[[r]]
      matColumnName <- cNames[[c]]
      rawTaleNameRow <- rawTaleMatSM[index, 2]
      rawTaleNameColumn <- rawTaleMatSM[index, 3]
      if ((matRowName == rawTaleNameRow) && (matColumnName == rawTaleNameColumn)) { 
        print("#######################################################")
        print("YAY")                                                                                    
        print(paste0("mat: ", matRowName, " ", matColumnName, " raw: ", rawTaleNameRow, " ", rawTaleNameColumn))   #try(res.mat[r, c] <- rawTaleMat[index, 1])
        res.matTASA[r, c] <- rawTaleMatTASA[index, 1] #rawTaleMatSM[[1]][index]
        print(paste0("post: rawTaleMatTASA[[1]][[index][[index]]:", rawTaleMatTASA[index, 1]))
        print(paste0("pre: res.matV[r, c]: ", res.matTASA[r, c]))
      }
    } 
  }
}
res.matTASA


#############################


dm <- 1/(1+dm.hamming)
hc.dm.svSM <- hclust(d <- dist(res.matSM), method = "ward.D2")

###############################

library(stringr)
#setwd("/home/kingfish/Propp")
setwd("/db/propptest/fresh")
system("ls")
rawTaleMatSMnoBNC <- read.table(file = "outputSMnoBNC.txt", sep="|", header = FALSE, stringsAsFactors = FALSE)
rawTaleMatSMnoBNC
class(rawTaleMatSMnoBNC)
#str_replace(string = rawTaleMat[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMatSMnoBNC[[2]] <- str_replace(string = rawTaleMatSMnoBNC[[2]], pattern = "afanSkazkiSingleMove/", replacement = "")
rawTaleMatSMnoBNC[[3]] <- str_replace(string = rawTaleMatSMnoBNC[[3]], pattern = "afanSkazkiSingleMove/", replacement = "")
#rawTaleMat[[2]][1]

length(rawTaleMatSMnoBNC)
rawTaleMatSMnoBNC
rNames <- unique(rawTaleMatSMnoBNC[[2]])
cNames <- unique(rawTaleMatSMnoBNC[[3]])
res.matSMnoBNC <- matrix(dimnames = list(rNames, cNames), nrow = length(rNames), ncol = length(cNames))
res.matSMnoBNC
rawTaleMatSMnoBNC
for (index in 1:(nrow(rawTaleMatSMnoBNC))) {
  for (c in 1:(length(cNames))) {
    for (r in 1:(length(rNames))) {
      matRowName <- rNames[[r]]
      matColumnName <- cNames[[c]]
      rawTaleNameRow <- rawTaleMatSM[index, 2]
      rawTaleNameColumn <- rawTaleMatSM[index, 3]
      if ((matRowName == rawTaleNameRow) && (matColumnName == rawTaleNameColumn)) { 
        print("#######################################################")
        print("YAY")                                                                                    
        print(paste0("mat: ", matRowName, " ", matColumnName, " raw: ", rawTaleNameRow, " ", rawTaleNameColumn))   #try(res.mat[r, c] <- rawTaleMat[index, 1])
        res.matSMnoBNC[r, c] <- rawTaleMatSMnoBNC[index, 1] #rawTaleMatSM[[1]][index]
        print(paste0("post: rawTaleMatV[[1]][[index][[index]]:", rawTaleMatSMnoBNC[index, 1]))
        print(paste0("pre: res.matV[r, c]: ", res.matSMnoBNC[r, c]))
      }
    } 
  }
}
res.matSMnoBNC
dm.hamming <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, weight=c(.5, .5, 1, 1))


##########


#hc.dm <- hclust(d <- dist(dm), method = "ward.D2")
hc.dm.svSM <- hclust(d <- dist(res.matSM), method = "complete")
hc.dm.svSMnoBNC <- hclust(d <- dist(res.matSMnoBNC), method = "complete")

plot(hc.dm.svSM, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using sentence term vector SUM as similarity metric)")
cor(as.vector(as.numeric(res.matSM)), as.vector(as.numeric(res.matSMP)), method = "spearman")
cor(as.vector(as.numeric(res.matSM)), as.vector(as.numeric(1/(1+dm))), method = "spearman")
cor(as.vector(as.numeric(res.matSMP)), as.vector(as.numeric(1/(1+dm))), method = "spearman")
cor(as.vector(as.numeric(res.matSMnoBNC)), as.vector(as.numeric(1/(1+dm))), method = "spearman")
cor(as.vector(as.numeric(res.matSMP)), as.vector(as.numeric(res.matSMnoBNC)), method = "spearman")
cor(as.vector(as.numeric(res.matTASA)), as.vector(as.numeric(1/(1+dm))), method = "spearman")
cor(as.vector(as.numeric(res.matSMPN)), as.vector(as.numeric(1/(1+dm))), method = "spearman")
cor(as.vector(as.numeric(res.matSMPlem)), as.vector(as.numeric(1/(1+dm))), method = "spearman") # .309
res.matSMPlem


cor(as.vector(as.numeric(res.matSM)), as.vector(as.numeric(res.matSMP)), method = "pearson")
cor(as.vector(as.numeric(res.matSM)), as.vector(as.numeric(1/(1+dm))), method = "pearson")
cor(as.vector(as.numeric(res.matSMP)), as.vector(as.numeric(1/(1+dm))), method = "pearson")
cor(as.vector(as.numeric(res.matSMnoBNC)), as.vector(as.numeric(1/(1+dm))), method = "pearson")
cor(as.vector(as.numeric(res.matSMP)), as.vector(as.numeric(res.matSMnoBNC)), method = "pearson")
cor(as.vector(as.numeric(res.matTASA)), as.vector(as.numeric(1/(1+dm))), method = "pearson")
cor(log(as.vector(as.numeric(res.matSMPN))), log(as.vector(as.numeric(1/(1+dm)))), method = "pearson")
cor(log(as.vector(as.numeric(res.matSMPlem))), log(as.vector(as.numeric(1/(1+dm)))), method = "pearson") #.705

res.matSMnoBNC
res.matTASA
#res.matSMP
################################
###############################



library(stringr)
#setwd("/home/kingfish/Propp")
setwd("/db/propptest/fresh")
system("ls")
rawTaleMatSMP <- read.table(file = "outputSMP.txt", sep="|", header = FALSE, stringsAsFactors = FALSE)
rawTaleMatSMP
class(rawTaleMatSMP)
#str_replace(string = rawTaleMat[[2]], pattern = "../fresh/afanFRESH/", replacement = "")
rawTaleMatSMP[[2]] <- str_replace(string = rawTaleMatSMP[[2]], pattern = "afanSkazkiSMPreds/", replacement = "")
rawTaleMatSMP[[3]] <- str_replace(string = rawTaleMatSMP[[3]], pattern = "afanSkazkiSMPreds/", replacement = "")
#rawTaleMat[[2]][1]

length(rawTaleMatSMP)
rawTaleMatSMP
rNames <- unique(rawTaleMatSMP[[2]])
cNames <- unique(rawTaleMatSMP[[3]])
res.matSMP <- matrix(dimnames = list(rNames, cNames), nrow = length(rNames), ncol = length(cNames))
res.matSMP
rawTaleMatSMP
for (index in 1:(nrow(rawTaleMatSMP))) {
  for (c in 1:(length(cNames))) {
    for (r in 1:(length(rNames))) {
      matRowName <- rNames[[r]]
      matColumnName <- cNames[[c]]
      rawTaleNameRow <- rawTaleMatSMP[index, 2]
      rawTaleNameColumn <- rawTaleMatSMP[index, 3]
      if ((matRowName == rawTaleNameRow) && (matColumnName == rawTaleNameColumn)) { 
        print("#######################################################")
        print("YAY")                                                                                    
        print(paste0("mat: ", matRowName, " ", matColumnName, " raw: ", rawTaleNameRow, " ", rawTaleNameColumn))   #try(res.mat[r, c] <- rawTaleMat[index, 1])
        res.matSMP[r, c] <- rawTaleMatSMP[index, 1]
        print(paste0("post: rawTaleMatSMP[[1]][[index][[index]]:", rawTaleMatSMP[index, 1]))
        print(paste0("pre: res.matV[r, c]: ", res.matSMP[r, c]))
      }
    } 
  }
}
res.matSMP

hc.dm.svSMP <- hclust(d <- dist(res.matSMP), method = "ward.D2")
#hc.dm <- hclust(d <- dist(dm), method = "ward.D2")
hc.dm.svSM <- hclust(d <- dist(res.matSMP), method = "complete")
plot(hc.dm.svSMP, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using sentence term vector SUM as similarity metric)")

################################

library(pander)
#goldStandards <- list(dm.lv.b.2, dm.lv.b.osa, dm.dl.b.2, dm.hamming.b.2, dm.lcs.b.2, dm.jw.b.2, dm.lv.nb.2, dm.lv.nb.osa, dm.dl.nb.2, dm.hamming.nb.2, dm.lcs.nb.2, dm.jw.nb.2 )
#goldStandardsNames <- list("dm.lv.b.2", "dm.lv.b.osa", "dm.dl.b.2", "dm.hamming.b.2", "dm.lcs.b.2", "dm.jw.b.2", "dm.lv.nb.2", "dm.lv.nb.osa", "dm.dl.nb.2", "dm.hamming.nb.2", "dm.lcs.nb.2", "dm.jw.nb.2" )
#goldStandards <- list(dm, dm.lv.b.2, dm.osa.b.2, dm.dl.b.2, dm.hamming.b.2, dm.lcs.b.2, dm.jw.nb.2, dm.lv.nb, dm.lv.nb.osa, dm.dl.nb, dm.hamming.nb, dm.lcs.nb, dm.jw.nb, dm.qgram.q1.b.2, dm.qgram.q2.b.2, dm.qgram.q3.b.2 )
#goldStandardsNames <- list("dm", "dm.lv.b.2", "dm.osa.b.2", "dm.dl.b.2", "dm.hamming.b.2", "dm.lcs.b.2", "dm.jw.b.2", "dm.lv.nb.2", "dm.osa.nb", "dm.dl.nb.2", "dm.hamming.nb.2", "dm.lcs.nb.2", "dm.jw.nb.2", "dm.qgram.q1.nb.2", "dm.qgram.q2.nb.2", "dm.qgram.q3.nb.2" )

dm.hamming.b <- stringdistmatrix(a = taleStructures, b = taleStructures, method="hamming", useNames = TRUE, useBytes = TRUE, weight=c(.5, .5, 1, 1))
dm.hamming.b.2 <- 1/(1+dm.hamming.b)
goldStandards <- unlist(ls(pattern = "^dm.*nb.2"))
#goldStandards <- c(goldStandards, "dm", "dm.2", "dm.w2", "dm.w3")
print(goldStandards)
#index <- 1
results <- c()
for (g.dm in goldStandards) {
  print("#########################################") 
  g.dm.name <- g.dm
  print(g.dm.name)
  g.dm <- pander::evals(g.dm)
  g.dm <- as.numeric(unlist(as.vector(g.dm[[1]][2])))
  print("compare no window with gold standard")
  print(cor(as.numeric(as.vector(res.matNW)), as.numeric(as.vector(g.dm)), use="complete.obs", method = "spearman"))
  noWindowWGold <- cor(as.numeric(as.vector(res.matNW)), as.numeric(as.vector(g.dm)), use="complete.obs", method = "spearman")
  windowWGold <- cor(as.numeric(as.vector(res.mat)), as.numeric(as.vector(g.dm)), use="complete.obs", method = "spearman")
  windowVerb <- cor(as.numeric(as.vector(res.matV)), as.numeric(as.vector(g.dm)), use="complete.obs", method = "spearman")
  #print("compare no window with window version")
  #print(cor(as.numeric(as.vector(res.matNW)), as.numeric(as.vector(res.mat)), use="complete.obs"))
  print("compare window version with gold standard")
  print(cor(as.numeric(as.vector(res.mat)), as.numeric(as.vector(g.dm)), use="complete.obs", method = "spearman"))
  print("compare window verbs version with gold standard")
  print(cor(as.numeric(as.vector(res.matV)), as.numeric(as.vector(g.dm)), use="complete.obs", method = "spearman"))
  result <- c(g.dm.name, noWindowWGold, windowWGold, windowVerb)
  results <- rbind(results, result)
  #index <- index + 1
}
colnames(results) <- c("goldstandardconfig", "nowindow", "window")
results

highResults <- c()
stepInc <- .1
highRes <- .02
for (i1 in 1:10) {
  for (i2 in 1:10) {
    for (i3 in 1:10) {
      for (i4 in 1:10) {
        p1 = stepInc*i1
        p2 = stepInc*i2
        p3 = stepInc*i3
        p4 = stepInc*i4
        g.dm <- stringdistmatrix(a = taleStructures, b = taleStructures, useNames = TRUE, useBytes = TRUE, method = "dl", weight=c(p1, p2, p3, p4)) 
        #g.dm <- 1/(1+g.dm)
        #print(cor(as.numeric(as.vector(res.mat)), as.numeric(as.vector(dm.hamming.b)), use="complete.obs", method = "spearman"))
        noWindowWGold <- cor(as.numeric(as.vector(res.matNW)), as.numeric(as.vector(g.dm)), use="complete.obs", method = "spearman")
        windowWGold <- cor(as.numeric(as.vector(res.mat)), as.numeric(as.vector(g.dm)), use="complete.obs", method = "spearman")
        windowVerb <- cor(as.numeric(as.vector(res.matV)), as.numeric(as.vector(g.dm)), use="complete.obs", method = "spearman")
        if ((windowVerb >= .02)) { #} || (noWindowWGold >= .1)) {
          #print(paste0("config: [1]=>", p1, "[2]=>", p2,  "[3]=>", p3, "[4]=>", p4,collapse=""))
          #print(paste0("noWindowWGold: ", noWindowWGold))
          #print(paste0("windowWGold: ", windowWGold))
          if (windowVerb > highRes) {
            highRes <- windowVerb
            dmStr <- paste0("dm <- stringdistmatrix(a = taleStructures, b = taleStructures, useNames = TRUE, useBytes = TRUE, method = 'dl', weight=c(", p1, ", ", p2, ", ", p3, ", ", p4, " )) ", collapse = "")
            highResult <- c(g.dm.name, noWindowWGold, windowWGold, windowVerb, p1, p2, p3, p4, dmStr)
            highResults <- rbind(highResults, highResult)
            #colnames(highResults) <- c("goldstandardconfig", "nowindow", "window", "windowVerb", "p1", "p2", "p3", "p4", "dmStr")
            print(paste0(noWindowWGold, " vs windowWGold: ", windowWGold, " vs windowVerb: ", windowVerb, collapse=""))
            print(paste0("GET HIGH! => ", dmStr, collapse=""))
           }
        }
      }
    }
  }
}


#######################3

highResults <- c()
stepInc <- .05
highRes <- .3
for (i1 in 1:20) {
  for (i2 in 1:20) {
    for (i3 in 1:20) {
      for (i4 in 1:20) {
        p1 = stepInc*i1
        p2 = stepInc*i2
        p3 = stepInc*i3
        p4 = stepInc*i4
        g.dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, method = "dl", weight=c(p1, p2, p3, p4)) 
        g.dm <- 1/(1+g.dm)
        #print(cor(as.numeric(as.vector(res.mat)), as.numeric(as.vector(dm.hamming.b)), use="complete.obs", method = "spearman"))
        #noWindowWGold <- cor(as.numeric(as.vector(res.mat)), as.numeric(as.vector(g.dm)), use="complete.obs", method = "spearman")
        #SM <- cor(as.numeric(as.vector(res.matSM)), as.numeric(as.vector(g.dm)), use="complete.obs", method = "spearman")
        SMP <- cor(as.numeric(as.vector(res.matSMPlem)), as.numeric(as.vector(g.dm)), use="complete.obs", method = "spearman")
        if ((SMP >= .02)) { #} || (noWindowWGold >= .1)) {
          #print(paste0("config: [1]=>", p1, "[2]=>", p2,  "[3]=>", p3, "[4]=>", p4,collapse=""))
          #print(paste0("noWindowWGold: ", noWindowWGold))
          #print(paste0("windowWGold: ", windowWGold))
          if (SMP > highRes) {
            highRes <- SMP
            dmStr <- paste0("dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, method = 'dl', weight=c(", p1, ", ", p2, ", ", p3, ", ", p4, " )) ", collapse = "")
            highResult <- c(g.dm.name, SMP, p1, p2, p3, p4, dmStr)
            highResults <- rbind(highResults, highResult)
            #colnames(highResults) <- c("goldstandardconfig", "nowindow", "window", "windowVerb", "p1", "p2", "p3", "p4", "dmStr")
            #print(paste0(" SM: ", SM, " vs SMP: ", SMP, collapse=""))
            print(paste0(" SMP: ", SMP, collapse=""))
            print(paste0("GET HIGH! => ", dmStr, collapse=""))
          }
        }
      }
    }
  }
}
###dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, method = 'dl', weight=c(1, 1, 0.3, 0.1 ))
dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, method = 'dl', weight=c(0.45, 0.45, 0.15, 0.05 )) 
colnames(dm) <- taleNamesSM
rownames(dm) <- taleNamesSM


hc.dm <- hclust(d <- dist(dm), method = "centroid")
hc.dm <- hclust(d <- dist(1/(1+dm)), method = "complete") #ward.D2, average, complete, centroid, mcquitty
#hc.dm.sm <- hclust(d <- dist(res.matSM), method = "ward.D2")
hc.dm.smp <- hclust(d <- dist(res.matSMPlem), method = "average")
plot(hc.dm, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using Levenshtein string distance as Similarity Metric)")
#plot(hc.dm.sm, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using Levenshtein string distance as Similarity Metric)")
plot(hc.dm.smp, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using Levenshtein string distance as Similarity Metric)")


###############33

#[1] "GET HIGH! => dm.2GET HIGH! => 0.100565295039138GET HIGH! => 0.127451410773279GET HIGH! => 0.5GET HIGH! => 0.6GET HIGH! => 0.3GET HIGH! => 0.1"
dm <- stringdistmatrix(a = taleStructures, b = taleStructures, useNames = TRUE, useBytes = TRUE, weight=c(.5, .6, .3, .1)) 
dm <- stringdistmatrix(a = taleStructures, b = taleStructures, useNames = TRUE, useBytes = TRUE, method = 'dl', weight=c(0.5, 0.5, 1, 0.2 ))
colnames(dm) <- taleNames
rownames(dm) <- taleNames

hc.dm <- hclust(d <- dist(dm.2), method = "centroid")
hc.dm <- hclust(d <- dist(dm), method = "ward.D2")
plot(hc.dm, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using Levenshtein string distance as Similarity Metric)")


hc.dm <- hclust(d <- dist(res.mat), method = "centroid")
hc.dm <- hclust(d <- dist(res.mat), method = "ward.D2")
plot(hc.dm, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using Levenshtein string distance as Similarity Metric)")

hc.dm <- hclust(d <- dist(res.matNW), method = "centroid")
hc.dm <- hclust(d <- dist(res.matNW), method = "ward.D2")
cor(as.numeric(as.vector(dm)), as.numeric(as.vector(res.matNW)), use="complete.obs", method = "spearman")
cor(as.numeric(as.vector(dm)), as.numeric(as.vector(res.mat)), use="complete.obs", method = "spearman")
cor(as.numeric(as.vector(res.mat)), as.numeric(as.vector(res.matV)), use="complete.obs", method = "spearman")
cor(as.numeric(as.vector(dm)), as.numeric(as.vector(res.matV)), use="complete.obs", method = "spearman")

plot(hc.dm, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using Levenshtein string distance as Similarity Metric)")

###############################

y = as.vector(as.numeric(res.matSMPN)); x = as.vector(as.numeric(1/(1+dm)))
y.m <- y - mean(y)
x.m <- x - mean(x)

data.frame(y.m, x.m)

set.seed(1234)
#x <- rnorm(20)
df <- data.frame(x = x.m,
                 y = y.m)

plot(y.m ~ x.m, data = df)

# model
mod <- lm(y.m ~ x.m, data = df)

# predicts + interval
#newx <- seq(min(df$x.m), max(df$x.m))
preds <- predict(mod, newdata = data.frame(x=x.m), 
                 interval = 'confidence')

# plot
plot(y.m ~ x.m, data = df) #, type = 'n')
# add fill
polygon(c(rev(x.m), x.m), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
# model
abline(mod)
# intervals
lines(x.m, preds[ ,3], lty = 'dashed', col = 'red')
lines(x.m, preds[ ,2], lty = 'dashed', col = 'red')

