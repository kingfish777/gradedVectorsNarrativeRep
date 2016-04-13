



   # library(TraMineR)
library(stringdist)
library(stringr)
library(tm)
# setwd("/home/kingfish/Propp/goldStandard")
setwd("/db/propptest/Propp/goldStandard")
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
 # dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, method = "qgram", p=.25, q=1, useNames = TRUE)
 # dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, method = "hamming", useBytes=TRUE)
 # dm <- stringdistmatrix(a = taleStructuresSM, b=taleStructuresSM, method = "jw", p=.25, useNames = TRUE, useBytes = TRUE)

 # dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, method = 'dl', weight=c(.5, .6, .3, .1)) # 0.5, 0.5, 1, 0.2 ))
 # dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, method = 'dl', weight=c(0.45, 0.45, 0.15, 0.05 )) 
  dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, method="jw") # weight=c(.5, .6, .3, .1)) 
 



   dm <- 1/(1+dm)
colnames(dm) <- taleNamesSM
rownames(dm) <- taleNamesSM
#rownames(dm) <- taleNamesSM
#colnames(dm) <- taleNamesSM
#dm <- stringdistmatrix(a = taleStructures, b = taleStructures, method="hamming")
  spearman <- cor(as.vector(as.numeric(res.matSMPlem)), as.vector(as.numeric(dm)), method = "spearman", use = "everything") # .309
   ######### no stoplist or BNC/TASA: .2542832 ---   just predicates of single move tales
  pearson <- cor(as.vector(as.numeric(res.matSMPlem)), as.vector(as.numeric(dm)), method = "pearson", use = "everything") #.705
   ######### no stoplist or BNC/TASA: .91111095

print(paste0("spearman: ", spearman))
print(paste0("pearson:  ", pearson))

hc.dm.SMPlem <- hclust(d <- dist(res.matSMPlem), method = "complete")
plot(hc.dm.SMPlem, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using BNC @ 32000 dimensions \n lemmatized predicates of all tales in appendix III)")
x=as.numeric(dm)
y=as.numeric(res.matSMPlem)
spearman = cor(y,x,method="spearman")
pearson = cor(y,x,method="pearson")
 # plot(y ~ x,xlim=c(-6,+6),ylim=c(-1,+2.5))
 # title("Correlation:")
 # abline(v=0)
 # abline(h=0)
 # lm1=lm(y ~ x)
 # abline(lm1,col="red")
 #legend("topleft",
 #      c("Red line: regression.",
 #        sprintf("Spearman: %.5f",spearman),
 #        sprintf("Pearson:   +%.5f",pearson)
 #      ))


    #str(summary(lm((y.m)~(x.m))))
 # chisq.test(y.m,x.m)

 # hc.dm <- hclust(d <- dist(dm), method = "complete")
 # plot(hc.dm, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using sentence term vector SUM as similarity metric)")
















 dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, method = 'dl', weight=c(0.5, 0.5, 1, 0.2 ))
 
 g <- "" 
highResults <- c()
stepInc <- .1 # .05
highRes <- .1
for (i1 in 1:10) { # 20
  for (i2 in 1:10) {
    for (i3 in 1:10) {
      for (i4 in 1:10) {
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

        pearson <- cor(as.numeric(as.vector(res.matSMPlem)), as.numeric(as.vector(g.dm)), use="complete.obs", method="pearson")
          #print(paste0("config: [1]=>", p1, "[2]=>", p2,  "[3]=>", p3, "[4]=>", p4,collapse=""))
          #print(paste0("noWindowWGold: ", noWindowWGold))
          #print(paste0("windowWGold: ", windowWGold))
          if (SMP > highRes) {
            highRes <- SMP
            dmStr <- paste0("dm <- stringdistmatrix(a = taleStructuresSM, b = taleStructuresSM, useNames = TRUE, useBytes = TRUE, method = 'dl', weight=c(", p1, ", ", p2, ", ", p3, ", ", p4, " )) ", collapse = "")
            highResult <- c(g.dm, SMP, p1, p2, p3, p4, dmStr)
            highResults <- rbind(highResults, highResult)
            #colnames(highResults) <- c("goldstandardconfig", "nowindow", "window", "windowVerb", "p1", "p2", "p3", "p4", "dmStr")
            #print(paste0(" SM: ", SM, " vs SMP: ", SMP, collapse=""))
            print(paste0(" spearman: ", SMP, collapse=""))
            print(paste0(" pearson:  ", pearson, collapse=""))
            print(paste0("GET HIGH! => ", dmStr, collapse=""))
            g <- g.dm
          }
        }
      }
    }
  }
}

   dm <- 1/(1+g.dm)
colnames(dm) <- taleNamesSM
rownames(dm) <- taleNamesSM
#rownames(dm) <- taleNamesSM
#colnames(dm) <- taleNamesSM
#dm <- stringdistmatrix(a = taleStructures, b = taleStructures, method="hamming")
  spearman <- cor(as.vector(as.numeric(res.matSMPlem)), as.vector(as.numeric(dm)), method = "spearman", use = "everything") # .309
   ######### no stoplist or BNC/TASA: .2542832 ---   just predicates of single move tales
  pearson <- cor(as.vector(as.numeric(res.matSMPlem)), as.vector(as.numeric(dm)), method = "pearson", use = "everything") #.705
   ######### no stoplist or BNC/TASA: .91111095

print(paste0("spearman: ", spearman))
print(paste0("pearson:  ", pearson))

hc.dm.SMPlem <- hclust(d <- dist(res.matSMPlem), method = "complete")
plot(hc.dm.SMPlem, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using BNC @ 32000 dimensions \n lemmatized predicates of all tales in appendix III)")
x=as.numeric(dm)
y=as.numeric(res.matSMPlem)
spearman = cor(y,x,method="spearman")
pearson = cor(y,x,method="pearson")
 # plot(y ~ x,xlim=c(-6,+6),ylim=c(-1,+2.5))
 # title("Correlation:")
 # abline(v=0)
 # abline(h=0)
 # lm1=lm(y ~ x)
 # abline(lm1,col="red")
 #legend("topleft",
 #      c("Red line: regression.",
 #        sprintf("Spearman: %.5f",spearman),
 #        sprintf("Pearson:   +%.5f",pearson)
 #      ))


    #str(summary(lm((y.m)~(x.m))))
 # chisq.test(y.m,x.m)

 # hc.dm <- hclust(d <- dist(dm), method = "complete")
 # plot(hc.dm, main = "Cluster Dendrogram of Afanas'ev's Tales,\nCompiled from data in Appendix III of Propp's Morphology\n(using sentence term vector SUM as similarity metric)")

