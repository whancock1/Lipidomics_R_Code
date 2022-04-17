rm(list=ls())

#setwd("/Users/whancock/Documents/R/Lipotype_Lysosomes")
#if (!requireNamespacer("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("qvalue", version = "3.8")

rm(list=ls())

setwd("enter_filepath")
getwd()
ll <- read.csv("enter_filename", header=TRUE)
names(ll)
unique(ll[2])

library(ggplot2)
library(qvalue)


###################PreparingTables

ce <- subset(ll, class=="CE", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
cesums <- c(sum(ce[1],na.rm=TRUE),sum(ce[2],na.rm=TRUE),sum(ce[3],na.rm=TRUE),sum(ce[4],na.rm=TRUE),sum(ce[5],na.rm=TRUE),sum(ce[6],na.rm=TRUE),sum(ce[7],na.rm=TRUE),sum(ce[8],na.rm=TRUE),sum(ce[9],na.rm=TRUE))
ceC311mean <- mean(c(sum(ce[1],na.rm=TRUE),sum(ce[2],na.rm=TRUE),sum(ce[3],na.rm=TRUE)))
ceC313mean <- mean(c(sum(ce[4],na.rm=TRUE),sum(ce[5],na.rm=TRUE),sum(ce[6],na.rm=TRUE)))
ceWTmean <- mean(c(sum(ce[7],na.rm=TRUE),sum(ce[8],na.rm=TRUE),sum(ce[9],na.rm=TRUE)))
ceC311sd <- sd(c(sum(ce[1],na.rm=TRUE),sum(ce[2],na.rm=TRUE),sum(ce[3],na.rm=TRUE)))
ceC313sd <- sd(c(sum(ce[4],na.rm=TRUE),sum(ce[5],na.rm=TRUE),sum(ce[6],na.rm=TRUE)))
ceWTsd <- sd(c(sum(ce[7],na.rm=TRUE),sum(ce[8],na.rm=TRUE),sum(ce[9],na.rm=TRUE)))


cer <- subset(ll, class=="Cer", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
cersums <- c(sum(cer[1],na.rm=TRUE),sum(cer[2],na.rm=TRUE),sum(cer[3],na.rm=TRUE),sum(cer[4],na.rm=TRUE),sum(cer[5],na.rm=TRUE),sum(cer[6],na.rm=TRUE),sum(cer[7],na.rm=TRUE),sum(cer[8],na.rm=TRUE),sum(cer[9],na.rm=TRUE))
cerC311mean <- mean(c(sum(cer[1],na.rm=TRUE),sum(cer[2],na.rm=TRUE),sum(cer[3],na.rm=TRUE)))
cerC313mean <- mean(c(sum(cer[4],na.rm=TRUE),sum(cer[5],na.rm=TRUE),sum(cer[6],na.rm=TRUE)))
cerWTmean <- mean(c(sum(cer[7],na.rm=TRUE),sum(cer[8],na.rm=TRUE),sum(cer[9],na.rm=TRUE)))
cerC311sd <- sd(c(sum(cer[1],na.rm=TRUE),sum(cer[2],na.rm=TRUE),sum(cer[3],na.rm=TRUE)))
cerC313sd <- sd(c(sum(cer[4],na.rm=TRUE),sum(cer[5],na.rm=TRUE),sum(cer[6],na.rm=TRUE)))
cerWTsd <- sd(c(sum(cer[7],na.rm=TRUE),sum(cer[8],na.rm=TRUE),sum(cer[9],na.rm=TRUE)))


cl <- subset(ll, class=="CL", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
clsums <- c(sum(cl[1],na.rm=TRUE),sum(cl[2],na.rm=TRUE),sum(cl[3],na.rm=TRUE),sum(cl[4],na.rm=TRUE),sum(cl[5],na.rm=TRUE),sum(cl[6],na.rm=TRUE),sum(cl[7],na.rm=TRUE),sum(cl[8],na.rm=TRUE),sum(cl[9],na.rm=TRUE))
clC311mean <- mean(c(sum(cl[1],na.rm=TRUE),sum(cl[2],na.rm=TRUE),sum(cl[3],na.rm=TRUE)))
clC313mean <- mean(c(sum(cl[4],na.rm=TRUE),sum(cl[5],na.rm=TRUE),sum(cl[6],na.rm=TRUE)))
clWTmean <- mean(c(sum(cl[7],na.rm=TRUE),sum(cl[8],na.rm=TRUE),sum(cl[9],na.rm=TRUE)))
clC311sd <- sd(c(sum(cl[1],na.rm=TRUE),sum(cl[2],na.rm=TRUE),sum(cl[3],na.rm=TRUE)))
clC313sd <- sd(c(sum(cl[4],na.rm=TRUE),sum(cl[5],na.rm=TRUE),sum(cl[6],na.rm=TRUE)))
clWTsd <- sd(c(sum(cl[7],na.rm=TRUE),sum(cl[8],na.rm=TRUE),sum(cl[9],na.rm=TRUE)))

dag <- subset(ll, class=="DAG", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
dagsums <- c(sum(dag[1],na.rm=TRUE),sum(dag[2],na.rm=TRUE),sum(dag[3],na.rm=TRUE),sum(dag[4],na.rm=TRUE),sum(dag[5],na.rm=TRUE),sum(dag[6],na.rm=TRUE),sum(dag[7],na.rm=TRUE),sum(dag[8],na.rm=TRUE),sum(dag[9],na.rm=TRUE))
dagC311mean <- mean(c(sum(dag[1],na.rm=TRUE),sum(dag[2],na.rm=TRUE),sum(dag[3],na.rm=TRUE)))
dagC313mean <- mean(c(sum(dag[4],na.rm=TRUE),sum(dag[5],na.rm=TRUE),sum(dag[6],na.rm=TRUE)))
dagWTmean <- mean(c(sum(dag[7],na.rm=TRUE),sum(dag[8],na.rm=TRUE),sum(dag[9],na.rm=TRUE)))
dagC311sd <- sd(c(sum(dag[1],na.rm=TRUE),sum(dag[2],na.rm=TRUE),sum(dag[3],na.rm=TRUE)))
dagC313sd <- sd(c(sum(dag[4],na.rm=TRUE),sum(dag[5],na.rm=TRUE),sum(dag[6],na.rm=TRUE)))
dagWTsd <- sd(c(sum(dag[7],na.rm=TRUE),sum(dag[8],na.rm=TRUE),sum(dag[9],na.rm=TRUE)))

hexcer <- subset(ll, class=="HexCer", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
hexcersums <- c(sum(hexcer[1],na.rm=TRUE),sum(hexcer[2],na.rm=TRUE),sum(hexcer[3],na.rm=TRUE),sum(hexcer[4],na.rm=TRUE),sum(hexcer[5],na.rm=TRUE),sum(hexcer[6],na.rm=TRUE),sum(hexcer[7],na.rm=TRUE),sum(hexcer[8],na.rm=TRUE),sum(hexcer[9],na.rm=TRUE))
hexcerC311mean <- mean(c(sum(hexcer[1],na.rm=TRUE),sum(hexcer[2],na.rm=TRUE),sum(hexcer[3],na.rm=TRUE)))
hexcerC313mean <- mean(c(sum(hexcer[4],na.rm=TRUE),sum(hexcer[5],na.rm=TRUE),sum(hexcer[6],na.rm=TRUE)))
hexcerWTmean <- mean(c(sum(hexcer[7],na.rm=TRUE),sum(hexcer[8],na.rm=TRUE),sum(hexcer[9],na.rm=TRUE)))
hexcerC311sd <- sd(c(sum(hexcer[1],na.rm=TRUE),sum(hexcer[2],na.rm=TRUE),sum(hexcer[3],na.rm=TRUE)))
hexcerC313sd <- sd(c(sum(hexcer[4],na.rm=TRUE),sum(hexcer[5],na.rm=TRUE),sum(hexcer[6],na.rm=TRUE)))
hexcerWTsd <- sd(c(sum(hexcer[7],na.rm=TRUE),sum(hexcer[8],na.rm=TRUE),sum(hexcer[9],na.rm=TRUE)))

lpa <- subset(ll, class=="LPA", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
lpasums <- c(sum(lpa[1],na.rm=TRUE),sum(lpa[2],na.rm=TRUE),sum(lpa[3],na.rm=TRUE),sum(lpa[4],na.rm=TRUE),sum(lpa[5],na.rm=TRUE),sum(lpa[6],na.rm=TRUE),sum(lpa[7],na.rm=TRUE),sum(lpa[8],na.rm=TRUE),sum(lpa[9],na.rm=TRUE))
lpaC311mean <- mean(c(sum(lpa[1],na.rm=TRUE),sum(lpa[2],na.rm=TRUE),sum(lpa[3],na.rm=TRUE)))
lpaC313mean <- mean(c(sum(lpa[4],na.rm=TRUE),sum(lpa[5],na.rm=TRUE),sum(lpa[6],na.rm=TRUE)))
lpaWTmean <- mean(c(sum(lpa[7],na.rm=TRUE),sum(lpa[8],na.rm=TRUE),sum(lpa[9],na.rm=TRUE)))
lpaC311sd <- sd(c(sum(lpa[1],na.rm=TRUE),sum(lpa[2],na.rm=TRUE),sum(lpa[3],na.rm=TRUE)))
lpaC313sd <- sd(c(sum(lpa[4],na.rm=TRUE),sum(lpa[5],na.rm=TRUE),sum(lpa[6],na.rm=TRUE)))
lpaWTsd <- sd(c(sum(lpa[7],na.rm=TRUE),sum(lpa[8],na.rm=TRUE),sum(lpa[9],na.rm=TRUE)))

lpc <- subset(ll, class=="LPC", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
lpcsums <- c(sum(lpc[1],na.rm=TRUE),sum(lpc[2],na.rm=TRUE),sum(lpc[3],na.rm=TRUE),sum(lpc[4],na.rm=TRUE),sum(lpc[5],na.rm=TRUE),sum(lpc[6],na.rm=TRUE),sum(lpc[7],na.rm=TRUE),sum(lpc[8],na.rm=TRUE),sum(lpc[9],na.rm=TRUE))
lpcC311mean <- mean(c(sum(lpc[1],na.rm=TRUE),sum(lpc[2],na.rm=TRUE),sum(lpc[3],na.rm=TRUE)))
lpcC313mean <- mean(c(sum(lpc[4],na.rm=TRUE),sum(lpc[5],na.rm=TRUE),sum(lpc[6],na.rm=TRUE)))
lpcWTmean <- mean(c(sum(lpc[7],na.rm=TRUE),sum(lpc[8],na.rm=TRUE),sum(lpc[9],na.rm=TRUE)))
lpcC311sd <- sd(c(sum(lpc[1],na.rm=TRUE),sum(lpc[2],na.rm=TRUE),sum(lpc[3],na.rm=TRUE)))
lpcC313sd <- sd(c(sum(lpc[4],na.rm=TRUE),sum(lpc[5],na.rm=TRUE),sum(lpc[6],na.rm=TRUE)))
lpcWTsd <- sd(c(sum(lpc[7],na.rm=TRUE),sum(lpc[8],na.rm=TRUE),sum(lpc[9],na.rm=TRUE)))

lpco <- subset(ll, class=="LPC O-", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
lpcosums <- c(sum(lpco[1],na.rm=TRUE),sum(lpco[2],na.rm=TRUE),sum(lpco[3],na.rm=TRUE),sum(lpco[4],na.rm=TRUE),sum(lpco[5],na.rm=TRUE),sum(lpco[6],na.rm=TRUE),sum(lpco[7],na.rm=TRUE),sum(lpco[8],na.rm=TRUE),sum(lpco[9],na.rm=TRUE))
lpcoC311mean <- mean(c(sum(lpco[1],na.rm=TRUE),sum(lpco[2],na.rm=TRUE),sum(lpco[3],na.rm=TRUE)))
lpcoC313mean <- mean(c(sum(lpco[4],na.rm=TRUE),sum(lpco[5],na.rm=TRUE),sum(lpco[6],na.rm=TRUE)))
lpcoWTmean <- mean(c(sum(lpco[7],na.rm=TRUE),sum(lpco[8],na.rm=TRUE),sum(lpco[9],na.rm=TRUE)))
lpcoC311sd <- sd(c(sum(lpco[1],na.rm=TRUE),sum(lpco[2],na.rm=TRUE),sum(lpco[3],na.rm=TRUE)))
lpcoC313sd <- sd(c(sum(lpco[4],na.rm=TRUE),sum(lpco[5],na.rm=TRUE),sum(lpco[6],na.rm=TRUE)))
lpcoWTsd <- sd(c(sum(lpco[7],na.rm=TRUE),sum(lpco[8],na.rm=TRUE),sum(lpco[9],na.rm=TRUE)))

lpe <- subset(ll, class=="LPE", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
lpesums <- c(sum(lpe[1],na.rm=TRUE),sum(lpe[2],na.rm=TRUE),sum(lpe[3],na.rm=TRUE),sum(lpe[4],na.rm=TRUE),sum(lpe[5],na.rm=TRUE),sum(lpe[6],na.rm=TRUE),sum(lpe[7],na.rm=TRUE),sum(lpe[8],na.rm=TRUE),sum(lpe[9],na.rm=TRUE))
lpeC311mean <- mean(c(sum(lpe[1],na.rm=TRUE),sum(lpe[2],na.rm=TRUE),sum(lpe[3],na.rm=TRUE)))
lpeC313mean <- mean(c(sum(lpe[4],na.rm=TRUE),sum(lpe[5],na.rm=TRUE),sum(lpe[6],na.rm=TRUE)))
lpeWTmean <- mean(c(sum(lpe[7],na.rm=TRUE),sum(lpe[8],na.rm=TRUE),sum(lpe[9],na.rm=TRUE)))
lpeC311sd <- sd(c(sum(lpe[1],na.rm=TRUE),sum(lpe[2],na.rm=TRUE),sum(lpe[3],na.rm=TRUE)))
lpeC313sd <- sd(c(sum(lpe[4],na.rm=TRUE),sum(lpe[5],na.rm=TRUE),sum(lpe[6],na.rm=TRUE)))
lpeWTsd <- sd(c(sum(lpe[7],na.rm=TRUE),sum(lpe[8],na.rm=TRUE),sum(lpe[9],na.rm=TRUE)))

lpeo <- subset(ll, class=="LPE O-", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
lpeosums <- c(sum(lpeo[1],na.rm=TRUE),sum(lpeo[2],na.rm=TRUE),sum(lpeo[3],na.rm=TRUE),sum(lpeo[4],na.rm=TRUE),sum(lpeo[5],na.rm=TRUE),sum(lpeo[6],na.rm=TRUE),sum(lpeo[7],na.rm=TRUE),sum(lpeo[8],na.rm=TRUE),sum(lpeo[9],na.rm=TRUE))
lpeoC311mean <- mean(c(sum(lpeo[1],na.rm=TRUE),sum(lpeo[2],na.rm=TRUE),sum(lpeo[3],na.rm=TRUE)))
lpeoC313mean <- mean(c(sum(lpeo[4],na.rm=TRUE),sum(lpeo[5],na.rm=TRUE),sum(lpeo[6],na.rm=TRUE)))
lpeoWTmean <- mean(c(sum(lpeo[7],na.rm=TRUE),sum(lpeo[8],na.rm=TRUE),sum(lpeo[9],na.rm=TRUE)))
lpeoC311sd <- sd(c(sum(lpeo[1],na.rm=TRUE),sum(lpeo[2],na.rm=TRUE),sum(lpeo[3],na.rm=TRUE)))
lpeoC313sd <- sd(c(sum(lpeo[4],na.rm=TRUE),sum(lpeo[5],na.rm=TRUE),sum(lpeo[6],na.rm=TRUE)))
lpeoWTsd <- sd(c(sum(lpeo[7],na.rm=TRUE),sum(lpeo[8],na.rm=TRUE),sum(lpeo[9],na.rm=TRUE)))

lpg <- subset(ll, class=="LPG", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
lpgsums <- c(sum(lpg[1],na.rm=TRUE),sum(lpg[2],na.rm=TRUE),sum(lpg[3],na.rm=TRUE),sum(lpg[4],na.rm=TRUE),sum(lpg[5],na.rm=TRUE),sum(lpg[6],na.rm=TRUE),sum(lpg[7],na.rm=TRUE),sum(lpg[8],na.rm=TRUE),sum(lpg[9],na.rm=TRUE))
lpgC311mean <- mean(c(sum(lpg[1],na.rm=TRUE),sum(lpg[2],na.rm=TRUE),sum(lpg[3],na.rm=TRUE)))
lpgC313mean <- mean(c(sum(lpg[4],na.rm=TRUE),sum(lpg[5],na.rm=TRUE),sum(lpg[6],na.rm=TRUE)))
lpgWTmean <- mean(c(sum(lpg[7],na.rm=TRUE),sum(lpg[8],na.rm=TRUE),sum(lpg[9],na.rm=TRUE)))
lpgC311sd <- sd(c(sum(lpg[1],na.rm=TRUE),sum(lpg[2],na.rm=TRUE),sum(lpg[3],na.rm=TRUE)))
lpgC313sd <- sd(c(sum(lpg[4],na.rm=TRUE),sum(lpg[5],na.rm=TRUE),sum(lpg[6],na.rm=TRUE)))
lpgWTsd <- sd(c(sum(lpg[7],na.rm=TRUE),sum(lpg[8],na.rm=TRUE),sum(lpg[9],na.rm=TRUE)))

lpi <- subset(ll, class=="LPI", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
lpisums <- c(sum(lpi[1],na.rm=TRUE),sum(lpi[2],na.rm=TRUE),sum(lpi[3],na.rm=TRUE),sum(lpi[4],na.rm=TRUE),sum(lpi[5],na.rm=TRUE),sum(lpi[6],na.rm=TRUE),sum(lpi[7],na.rm=TRUE),sum(lpi[8],na.rm=TRUE),sum(lpi[9],na.rm=TRUE))
lpiC311mean <- mean(c(sum(lpi[1],na.rm=TRUE),sum(lpi[2],na.rm=TRUE),sum(lpi[3],na.rm=TRUE)))
lpiC313mean <- mean(c(sum(lpi[4],na.rm=TRUE),sum(lpi[5],na.rm=TRUE),sum(lpi[6],na.rm=TRUE)))
lpiWTmean <- mean(c(sum(lpi[7],na.rm=TRUE),sum(lpi[8],na.rm=TRUE),sum(lpi[9],na.rm=TRUE)))
lpiC311sd <- sd(c(sum(lpi[1],na.rm=TRUE),sum(lpi[2],na.rm=TRUE),sum(lpi[3],na.rm=TRUE)))
lpiC313sd <- sd(c(sum(lpi[4],na.rm=TRUE),sum(lpi[5],na.rm=TRUE),sum(lpi[6],na.rm=TRUE)))
lpiWTsd <- sd(c(sum(lpi[7],na.rm=TRUE),sum(lpi[8],na.rm=TRUE),sum(lpi[9],na.rm=TRUE)))

lps <- subset(ll, class=="LPS", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
lpssums <- c(sum(lps[1],na.rm=TRUE),sum(lps[2],na.rm=TRUE),sum(lps[3],na.rm=TRUE),sum(lps[4],na.rm=TRUE),sum(lps[5],na.rm=TRUE),sum(lps[6],na.rm=TRUE),sum(lps[7],na.rm=TRUE),sum(lps[8],na.rm=TRUE),sum(lps[9],na.rm=TRUE))
lpsC311mean <- mean(c(sum(lps[1],na.rm=TRUE),sum(lps[2],na.rm=TRUE),sum(lps[3],na.rm=TRUE)))
lpsC313mean <- mean(c(sum(lps[4],na.rm=TRUE),sum(lps[5],na.rm=TRUE),sum(lps[6],na.rm=TRUE)))
lpsWTmean <- mean(c(sum(lps[7],na.rm=TRUE),sum(lps[8],na.rm=TRUE),sum(lps[9],na.rm=TRUE)))
lpsC311sd <- sd(c(sum(lps[1],na.rm=TRUE),sum(lps[2],na.rm=TRUE),sum(lps[3],na.rm=TRUE)))
lpsC313sd <- sd(c(sum(lps[4],na.rm=TRUE),sum(lps[5],na.rm=TRUE),sum(lps[6],na.rm=TRUE)))
lpsWTsd <- sd(c(sum(lps[7],na.rm=TRUE),sum(lps[8],na.rm=TRUE),sum(lps[9],na.rm=TRUE)))

pa <- subset(ll, class=="PA", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
pasums <- c(sum(pa[1],na.rm=TRUE),sum(pa[2],na.rm=TRUE),sum(pa[3],na.rm=TRUE),sum(pa[4],na.rm=TRUE),sum(pa[5],na.rm=TRUE),sum(pa[6],na.rm=TRUE),sum(pa[7],na.rm=TRUE),sum(pa[8],na.rm=TRUE),sum(pa[9],na.rm=TRUE))
paC311mean <- mean(c(sum(pa[1],na.rm=TRUE),sum(pa[2],na.rm=TRUE),sum(pa[3],na.rm=TRUE)))
paC313mean <- mean(c(sum(pa[4],na.rm=TRUE),sum(pa[5],na.rm=TRUE),sum(pa[6],na.rm=TRUE)))
paWTmean <- mean(c(sum(pa[7],na.rm=TRUE),sum(pa[8],na.rm=TRUE),sum(pa[9],na.rm=TRUE)))
paC311sd <- sd(c(sum(pa[1],na.rm=TRUE),sum(pa[2],na.rm=TRUE),sum(pa[3],na.rm=TRUE)))
paC313sd <- sd(c(sum(pa[4],na.rm=TRUE),sum(pa[5],na.rm=TRUE),sum(pa[6],na.rm=TRUE)))
paWTsd <- sd(c(sum(pa[7],na.rm=TRUE),sum(pa[8],na.rm=TRUE),sum(pa[9],na.rm=TRUE)))

pc <- subset(ll, class=="PC", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
pcsums <- c(sum(pc[1],na.rm=TRUE),sum(pc[2],na.rm=TRUE),sum(pc[3],na.rm=TRUE),sum(pc[4],na.rm=TRUE),sum(pc[5],na.rm=TRUE),sum(pc[6],na.rm=TRUE),sum(pc[7],na.rm=TRUE),sum(pc[8],na.rm=TRUE),sum(pc[9],na.rm=TRUE))
pcC311mean <- mean(c(sum(pc[1],na.rm=TRUE),sum(pc[2],na.rm=TRUE),sum(pc[3],na.rm=TRUE)))
pcC313mean <- mean(c(sum(pc[4],na.rm=TRUE),sum(pc[5],na.rm=TRUE),sum(pc[6],na.rm=TRUE)))
pcWTmean <- mean(c(sum(pc[7],na.rm=TRUE),sum(pc[8],na.rm=TRUE),sum(pc[9],na.rm=TRUE)))
pcC311sd <- sd(c(sum(pc[1],na.rm=TRUE),sum(pc[2],na.rm=TRUE),sum(pc[3],na.rm=TRUE)))
pcC313sd <- sd(c(sum(pc[4],na.rm=TRUE),sum(pc[5],na.rm=TRUE),sum(pc[6],na.rm=TRUE)))
pcWTsd <- sd(c(sum(pc[7],na.rm=TRUE),sum(pc[8],na.rm=TRUE),sum(pc[9],na.rm=TRUE)))

pco <- subset(ll, class=="PC O-", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
pcosums <- c(sum(pco[1],na.rm=TRUE),sum(pco[2],na.rm=TRUE),sum(pco[3],na.rm=TRUE),sum(pco[4],na.rm=TRUE),sum(pco[5],na.rm=TRUE),sum(pco[6],na.rm=TRUE),sum(pco[7],na.rm=TRUE),sum(pco[8],na.rm=TRUE),sum(pco[9],na.rm=TRUE))
pcoC311mean <- mean(c(sum(pco[1],na.rm=TRUE),sum(pco[2],na.rm=TRUE),sum(pco[3],na.rm=TRUE)))
pcoC313mean <- mean(c(sum(pco[4],na.rm=TRUE),sum(pco[5],na.rm=TRUE),sum(pco[6],na.rm=TRUE)))
pcoWTmean <- mean(c(sum(pco[7],na.rm=TRUE),sum(pco[8],na.rm=TRUE),sum(pco[9],na.rm=TRUE)))
pcoC311sd <- sd(c(sum(pco[1],na.rm=TRUE),sum(pco[2],na.rm=TRUE),sum(pco[3],na.rm=TRUE)))
pcoC313sd <- sd(c(sum(pco[4],na.rm=TRUE),sum(pco[5],na.rm=TRUE),sum(pco[6],na.rm=TRUE)))
pcoWTsd <- sd(c(sum(pco[7],na.rm=TRUE),sum(pco[8],na.rm=TRUE),sum(pco[9],na.rm=TRUE)))

pe <- subset(ll, class=="PE", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
pesums <- c(sum(pe[1],na.rm=TRUE),sum(pe[2],na.rm=TRUE),sum(pe[3],na.rm=TRUE),sum(pe[4],na.rm=TRUE),sum(pe[5],na.rm=TRUE),sum(pe[6],na.rm=TRUE),sum(pe[7],na.rm=TRUE),sum(pe[8],na.rm=TRUE),sum(pe[9],na.rm=TRUE))
peC311mean <- mean(c(sum(pe[1],na.rm=TRUE),sum(pe[2],na.rm=TRUE),sum(pe[3],na.rm=TRUE)))
peC313mean <- mean(c(sum(pe[4],na.rm=TRUE),sum(pe[5],na.rm=TRUE),sum(pe[6],na.rm=TRUE)))
peWTmean <- mean(c(sum(pe[7],na.rm=TRUE),sum(pe[8],na.rm=TRUE),sum(pe[9],na.rm=TRUE)))
peC311sd <- sd(c(sum(pe[1],na.rm=TRUE),sum(pe[2],na.rm=TRUE),sum(pe[3],na.rm=TRUE)))
peC313sd <- sd(c(sum(pe[4],na.rm=TRUE),sum(pe[5],na.rm=TRUE),sum(pe[6],na.rm=TRUE)))
peWTsd <- sd(c(sum(pe[7],na.rm=TRUE),sum(pe[8],na.rm=TRUE),sum(pe[9],na.rm=TRUE)))

peo <- subset(ll, class=="PE O-", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
peosums <- c(sum(peo[1],na.rm=TRUE),sum(peo[2],na.rm=TRUE),sum(peo[3],na.rm=TRUE),sum(peo[4],na.rm=TRUE),sum(peo[5],na.rm=TRUE),sum(peo[6],na.rm=TRUE),sum(peo[7],na.rm=TRUE),sum(peo[8],na.rm=TRUE),sum(peo[9],na.rm=TRUE))
peoC311mean <- mean(c(sum(peo[1],na.rm=TRUE),sum(peo[2],na.rm=TRUE),sum(peo[3],na.rm=TRUE)))
peoC313mean <- mean(c(sum(peo[4],na.rm=TRUE),sum(peo[5],na.rm=TRUE),sum(peo[6],na.rm=TRUE)))
peoWTmean <- mean(c(sum(peo[7],na.rm=TRUE),sum(peo[8],na.rm=TRUE),sum(peo[9],na.rm=TRUE)))
peoC311sd <- sd(c(sum(peo[1],na.rm=TRUE),sum(peo[2],na.rm=TRUE),sum(peo[3],na.rm=TRUE)))
peoC313sd <- sd(c(sum(peo[4],na.rm=TRUE),sum(peo[5],na.rm=TRUE),sum(peo[6],na.rm=TRUE)))
peoWTsd <- sd(c(sum(peo[7],na.rm=TRUE),sum(peo[8],na.rm=TRUE),sum(peo[9],na.rm=TRUE)))

pg <- subset(ll, class=="PG", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
pgsums <- c(sum(pg[1],na.rm=TRUE),sum(pg[2],na.rm=TRUE),sum(pg[3],na.rm=TRUE),sum(pg[4],na.rm=TRUE),sum(pg[5],na.rm=TRUE),sum(pg[6],na.rm=TRUE),sum(pg[7],na.rm=TRUE),sum(pg[8],na.rm=TRUE),sum(pg[9],na.rm=TRUE))
pgC311mean <- mean(c(sum(pg[1],na.rm=TRUE),sum(pg[2],na.rm=TRUE),sum(pg[3],na.rm=TRUE)))
pgC313mean <- mean(c(sum(pg[4],na.rm=TRUE),sum(pg[5],na.rm=TRUE),sum(pg[6],na.rm=TRUE)))
pgWTmean <- mean(c(sum(pg[7],na.rm=TRUE),sum(pg[8],na.rm=TRUE),sum(pg[9],na.rm=TRUE)))
pgC311sd <- sd(c(sum(pg[1],na.rm=TRUE),sum(pg[2],na.rm=TRUE),sum(pg[3],na.rm=TRUE)))
pgC313sd <- sd(c(sum(pg[4],na.rm=TRUE),sum(pg[5],na.rm=TRUE),sum(pg[6],na.rm=TRUE)))
pgWTsd <- sd(c(sum(pg[7],na.rm=TRUE),sum(pg[8],na.rm=TRUE),sum(pg[9],na.rm=TRUE)))

pi <- subset(ll, class=="PI", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
pisums <- c(sum(pi[1],na.rm=TRUE),sum(pi[2],na.rm=TRUE),sum(pi[3],na.rm=TRUE),sum(pi[4],na.rm=TRUE),sum(pi[5],na.rm=TRUE),sum(pi[6],na.rm=TRUE),sum(pi[7],na.rm=TRUE),sum(pi[8],na.rm=TRUE),sum(pi[9],na.rm=TRUE))
piC311mean <- mean(c(sum(pi[1],na.rm=TRUE),sum(pi[2],na.rm=TRUE),sum(pi[3],na.rm=TRUE)))
piC313mean <- mean(c(sum(pi[4],na.rm=TRUE),sum(pi[5],na.rm=TRUE),sum(pi[6],na.rm=TRUE)))
piWTmean <- mean(c(sum(pi[7],na.rm=TRUE),sum(pi[8],na.rm=TRUE),sum(pi[9],na.rm=TRUE)))
piC311sd <- sd(c(sum(pi[1],na.rm=TRUE),sum(pi[2],na.rm=TRUE),sum(pi[3],na.rm=TRUE)))
piC313sd <- sd(c(sum(pi[4],na.rm=TRUE),sum(pi[5],na.rm=TRUE),sum(pi[6],na.rm=TRUE)))
piWTsd <- sd(c(sum(pi[7],na.rm=TRUE),sum(pi[8],na.rm=TRUE),sum(pi[9],na.rm=TRUE)))

ps <- subset(ll, class=="PS", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
pssums <- c(sum(ps[1],na.rm=TRUE),sum(ps[2],na.rm=TRUE),sum(ps[3],na.rm=TRUE),sum(ps[4],na.rm=TRUE),sum(ps[5],na.rm=TRUE),sum(ps[6],na.rm=TRUE),sum(ps[7],na.rm=TRUE),sum(ps[8],na.rm=TRUE),sum(ps[9],na.rm=TRUE))
psC311mean <- mean(c(sum(ps[1],na.rm=TRUE),sum(ps[2],na.rm=TRUE),sum(ps[3],na.rm=TRUE)))
psC313mean <- mean(c(sum(ps[4],na.rm=TRUE),sum(ps[5],na.rm=TRUE),sum(ps[6],na.rm=TRUE)))
psWTmean <- mean(c(sum(ps[7],na.rm=TRUE),sum(ps[8],na.rm=TRUE),sum(ps[9],na.rm=TRUE)))
psC311sd <- sd(c(sum(ps[1],na.rm=TRUE),sum(ps[2],na.rm=TRUE),sum(ps[3],na.rm=TRUE)))
psC313sd <- sd(c(sum(ps[4],na.rm=TRUE),sum(ps[5],na.rm=TRUE),sum(ps[6],na.rm=TRUE)))
psWTsd <- sd(c(sum(ps[7],na.rm=TRUE),sum(ps[8],na.rm=TRUE),sum(ps[9],na.rm=TRUE)))

sm <- subset(ll, class=="SM", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
smsums <- c(sum(sm[1],na.rm=TRUE),sum(sm[2],na.rm=TRUE),sum(sm[3],na.rm=TRUE),sum(sm[4],na.rm=TRUE),sum(sm[5],na.rm=TRUE),sum(sm[6],na.rm=TRUE),sum(sm[7],na.rm=TRUE),sum(sm[8],na.rm=TRUE),sum(sm[9],na.rm=TRUE))
smC311mean <- mean(c(sum(sm[1],na.rm=TRUE),sum(sm[2],na.rm=TRUE),sum(sm[3],na.rm=TRUE)))
smC313mean <- mean(c(sum(sm[4],na.rm=TRUE),sum(sm[5],na.rm=TRUE),sum(sm[6],na.rm=TRUE)))
smWTmean <- mean(c(sum(sm[7],na.rm=TRUE),sum(sm[8],na.rm=TRUE),sum(sm[9],na.rm=TRUE)))
smC311sd <- sd(c(sum(sm[1],na.rm=TRUE),sum(sm[2],na.rm=TRUE),sum(sm[3],na.rm=TRUE)))
smC313sd <- sd(c(sum(sm[4],na.rm=TRUE),sum(sm[5],na.rm=TRUE),sum(sm[6],na.rm=TRUE)))
smWTsd <- sd(c(sum(sm[7],na.rm=TRUE),sum(sm[8],na.rm=TRUE),sum(sm[9],na.rm=TRUE)))

tag <- subset(ll, class=="TAG", select = c(CKO311_a, CKO311_b, CKO311_c, CKO313_a, CKO313_b, CKO313_c, WT_HeLa_a, WT_HeLa_b, WT_HeLa_c))
tagsums <- c(sum(tag[1],na.rm=TRUE),sum(tag[2],na.rm=TRUE),sum(tag[3],na.rm=TRUE),sum(tag[4],na.rm=TRUE),sum(tag[5],na.rm=TRUE),sum(tag[6],na.rm=TRUE),sum(tag[7],na.rm=TRUE),sum(tag[8],na.rm=TRUE),sum(tag[9],na.rm=TRUE))
tagC311mean <- mean(c(sum(tag[1],na.rm=TRUE),sum(tag[2],na.rm=TRUE),sum(tag[3],na.rm=TRUE)))
tagC313mean <- mean(c(sum(tag[4],na.rm=TRUE),sum(tag[5],na.rm=TRUE),sum(tag[6],na.rm=TRUE)))
tagWTmean <- mean(c(sum(tag[7],na.rm=TRUE),sum(tag[8],na.rm=TRUE),sum(tag[9],na.rm=TRUE)))
tagC311sd <- sd(c(sum(tag[1],na.rm=TRUE),sum(tag[2],na.rm=TRUE),sum(tag[3],na.rm=TRUE)))
tagC313sd <- sd(c(sum(tag[4],na.rm=TRUE),sum(tag[5],na.rm=TRUE),sum(tag[6],na.rm=TRUE)))
tagWTsd <- sd(c(sum(tag[7],na.rm=TRUE),sum(tag[8],na.rm=TRUE),sum(tag[9],na.rm=TRUE)))

r1 <- c(ceC311mean,ceC313mean,ceWTmean,ceC311sd,ceC313sd,ceWTsd)
r2 <- c(cerC311mean,cerC313mean,cerWTmean,cerC311sd,cerC313sd,cerWTsd)
r3 <- c(clC311mean,clC313mean,clWTmean,clC311sd,clC313sd,clWTsd)
r4 <- c(dagC311mean,dagC313mean,dagWTmean,dagC311sd,dagC313sd,dagWTsd)
r5 <- c(hexcerC311mean,hexcerC313mean,hexcerWTmean,hexcerC311sd,hexcerC313sd,hexcerWTsd)
r6 <- c(lpaC311mean,lpaC313mean,lpaWTmean,lpaC311sd,lpaC313sd,lpaWTsd)
r7 <- c(lpcC311mean,lpcC313mean,lpcWTmean,lpcC311sd,lpcC313sd,lpcWTsd)
r8 <- c(lpcoC311mean,lpcoC313mean,lpcoWTmean,lpcoC311sd,lpcoC313sd,lpcoWTsd)
r9 <- c(lpeC311mean,lpeC313mean,lpeWTmean,lpeC311sd,lpeC313sd,lpeWTsd)
r10 <- c(lpeoC311mean,lpeoC313mean,lpeoWTmean,lpeoC311sd,lpeoC313sd,lpeoWTsd)
r11 <- c(lpgC311mean,lpgC313mean,lpgWTmean,lpgC311sd,lpgC313sd,lpgWTsd)
r12 <- c(lpiC311mean,lpiC313mean,lpiWTmean,lpiC311sd,lpiC313sd,lpiWTsd)
r13 <- c(lpsC311mean,lpsC313mean,lpsWTmean,lpsC311sd,lpsC313sd,lpsWTsd)
r14 <- c(paC311mean,paC313mean,paWTmean,paC311sd,paC313sd,paWTsd)
r15 <- c(pcC311mean,pcC313mean,pcWTmean,pcC311sd,pcC313sd,pcWTsd)
r16 <- c(pcoC311mean,pcoC313mean,pcoWTmean,pcoC311sd,pcoC313sd,pcoWTsd)
r17 <- c(peC311mean,peC313mean,peWTmean,peC311sd,peC313sd,peWTsd)
r18 <- c(peoC311mean,peoC313mean,peoWTmean,peoC311sd,peoC313sd,peoWTsd)
r19 <- c(pgC311mean,pgC313mean,pgWTmean,pgC311sd,pgC313sd,pgWTsd)
r20 <- c(piC311mean,piC313mean,piWTmean,piC311sd,piC313sd,piWTsd)
r21 <- c(psC311mean,psC313mean,psWTmean,psC311sd,psC313sd,psWTsd)
r22 <- c(smC311mean,smC313mean,smWTmean,smC311sd,smC313sd,smWTsd)
r23 <- c(tagC311mean,tagC313mean,tagWTmean,tagC311sd,tagC313sd,tagWTsd)

rs <- rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r21,r22,r23)
HeLameansdtable <- cbind(unique(ll[2]),rs)
colnames(HeLameansdtable) <- c("Class","CKO311 Mean","CKO313 Mean","WT Mean","CKO311 sd","CKO313 sd", "WT sd")

group <- c("1","1","1","2","2","2","3","3","3")
sums <- rbind(cesums,cersums,clsums,dagsums,hexcersums,lpasums,lpcsums,lpcosums,lpesums,lpeosums,lpgsums,lpisums,lpssums,pasums,pcsums,pcosums,pesums,peosums,pgsums,pisums,pssums,smsums,tagsums,group)
colnames(sums) <- c("cko311_a","cko311_b","cko311_c","cko313_a","cko313_b","cko313_c","wt-a","wt-b","wt-c")
tsums <- t(sums)
tsums <- as.table(tsums)
write.csv(tsums, file="testfilelh", append=FALSE)
tsums <- read.csv("testfilelh", header = TRUE)


########################StatisticalTesting
########################StatisticalTesting
C1Wpval <- list()
for(i in c(tsums[c(1:3,7:9),2:(ncol(tsums)-1)]))
{
  boop <- t.test(i~tsums[c(1:3,7:9),ncol(tsums)])$p.value
  C1Wpval[[length(C1Wpval)+1]] = boop
}

C1Wpval2 <- unlist(C1Wpval, recursive = TRUE, use.names = FALSE)
C1Wqvals <- qvalue(C1Wpval2,pi0=1)$qvalues



C3Wpval <- list()
for(i in c(tsums[4:9,2:(ncol(tsums)-1)]))
{
  moop <- t.test(i~tsums[4:9,ncol(tsums)])$p.value
  C3Wpval[[length(C3Wpval)+1]] = moop
}

C3Wpval2 <- unlist(C3Wpval, recursive = TRUE, use.names = FALSE)
C3Wqvals <- qvalue(C3Wpval2,pi0=1)$qvalues


C1C3pval <- list()
for(i in c(tsums[1:6,2:(ncol(tsums)-1)]))
{
  snoop <- t.test(i~tsums[1:6,ncol(tsums)])$p.value
  C1C3pval[[length(C1C3pval)+1]] = snoop
}

C1C3pval2 <- unlist(C1C3pval, recursive = TRUE, use.names = FALSE)
C1C3qvals <- qvalue(C1C3pval2,pi0=1)$qvalues



HeLameanssdqptable_New <- cbind(HeLameansdtable, C1Wpval2, C1Wqvals,C3Wpval2, C3Wqvals,C1C3pval2, C1C3qvals)

########################Output
write.csv(HeLameanssdqptable_New, "HeLaNewOutput.csv", append=FALSE)
write.csv(sums, "HeLaNewSums.csv", append=FALSE)