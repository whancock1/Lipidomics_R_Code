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

ce <- subset(ll, class=="CE", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
cesums <- c(sum(ce[1],na.rm=TRUE),sum(ce[2],na.rm=TRUE),sum(ce[3],na.rm=TRUE),sum(ce[4],na.rm=TRUE),sum(ce[5],na.rm=TRUE),sum(ce[6],na.rm=TRUE),sum(ce[7],na.rm=TRUE),sum(ce[8],na.rm=TRUE),sum(ce[9],na.rm=TRUE),sum(ce[10],na.rm=TRUE),sum(ce[11],na.rm=TRUE),sum(ce[12],na.rm=TRUE))
ceC1mean <- mean(c(sum(ce[1],na.rm=TRUE),sum(ce[2],na.rm=TRUE),sum(ce[3],na.rm=TRUE),sum(ce[4],na.rm=TRUE)))
ceC3mean <- mean(c(sum(ce[5],na.rm=TRUE),sum(ce[6],na.rm=TRUE),sum(ce[7],na.rm=TRUE),sum(ce[8],na.rm=TRUE)))
ceWmean <- mean(c(sum(ce[9],na.rm=TRUE),sum(ce[10],na.rm=TRUE),sum(ce[11],na.rm=TRUE),sum(ce[12],na.rm=TRUE)))
ceC1sd <- sd(c(sum(ce[1],na.rm=TRUE),sum(ce[2],na.rm=TRUE),sum(ce[3],na.rm=TRUE),sum(ce[4],na.rm=TRUE)))
ceC3sd <- sd(c(sum(ce[5],na.rm=TRUE),sum(ce[6],na.rm=TRUE),sum(ce[7],na.rm=TRUE),sum(ce[8],na.rm=TRUE)))
ceWsd <- sd(c(sum(ce[9],na.rm=TRUE),sum(ce[10],na.rm=TRUE),sum(ce[11],na.rm=TRUE),sum(ce[12],na.rm=TRUE)))

cer <- subset(ll, class=="Cer", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
cersums <- c(sum(cer[1],na.rm=TRUE),sum(cer[2],na.rm=TRUE),sum(cer[3],na.rm=TRUE),sum(cer[4],na.rm=TRUE),sum(cer[5],na.rm=TRUE),sum(cer[6],na.rm=TRUE),sum(cer[7],na.rm=TRUE),sum(cer[8],na.rm=TRUE),sum(cer[9],na.rm=TRUE),sum(cer[10],na.rm=TRUE),sum(cer[11],na.rm=TRUE),sum(cer[12],na.rm=TRUE))
cerC1mean <- mean(c(sum(cer[1],na.rm=TRUE),sum(cer[2],na.rm=TRUE),sum(cer[3],na.rm=TRUE),sum(cer[4],na.rm=TRUE)))
cerC3mean <- mean(c(sum(cer[5],na.rm=TRUE),sum(cer[6],na.rm=TRUE),sum(cer[7],na.rm=TRUE),sum(cer[8],na.rm=TRUE)))
cerWmean <- mean(c(sum(cer[9],na.rm=TRUE),sum(cer[10],na.rm=TRUE),sum(cer[11],na.rm=TRUE),sum(cer[12],na.rm=TRUE)))
cerC1sd <- sd(c(sum(cer[1],na.rm=TRUE),sum(cer[2],na.rm=TRUE),sum(cer[3],na.rm=TRUE),sum(cer[4],na.rm=TRUE)))
cerC3sd <- sd(c(sum(cer[5],na.rm=TRUE),sum(cer[6],na.rm=TRUE),sum(cer[7],na.rm=TRUE),sum(cer[8],na.rm=TRUE)))
cerWsd <- sd(c(sum(cer[9],na.rm=TRUE),sum(cer[10],na.rm=TRUE),sum(cer[11],na.rm=TRUE),sum(cer[12],na.rm=TRUE)))

cl <- subset(ll, class=="CL", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
clsums <- c(sum(cl[1],na.rm=TRUE),sum(cl[2],na.rm=TRUE),sum(cl[3],na.rm=TRUE),sum(cl[4],na.rm=TRUE),sum(cl[5],na.rm=TRUE),sum(cl[6],na.rm=TRUE),sum(cl[7],na.rm=TRUE),sum(cl[8],na.rm=TRUE),sum(cl[9],na.rm=TRUE),sum(cl[10],na.rm=TRUE),sum(cl[11],na.rm=TRUE),sum(cl[12],na.rm=TRUE))
clC1mean <- mean(c(sum(cl[1],na.rm=TRUE),sum(cl[2],na.rm=TRUE),sum(cl[3],na.rm=TRUE),sum(cl[4],na.rm=TRUE)))
clC3mean <- mean(c(sum(cl[5],na.rm=TRUE),sum(cl[6],na.rm=TRUE),sum(cl[7],na.rm=TRUE),sum(cl[8],na.rm=TRUE)))
clWmean <- mean(c(sum(cl[9],na.rm=TRUE),sum(cl[10],na.rm=TRUE),sum(cl[11],na.rm=TRUE),sum(cl[12],na.rm=TRUE)))
clC1sd <- sd(c(sum(cl[1],na.rm=TRUE),sum(cl[2],na.rm=TRUE),sum(cl[3],na.rm=TRUE),sum(cl[4],na.rm=TRUE)))
clC3sd <- sd(c(sum(cl[5],na.rm=TRUE),sum(cl[6],na.rm=TRUE),sum(cl[7],na.rm=TRUE),sum(cl[8],na.rm=TRUE)))
clWsd <- sd(c(sum(cl[9],na.rm=TRUE),sum(cl[10],na.rm=TRUE),sum(cl[11],na.rm=TRUE),sum(cl[12],na.rm=TRUE)))

dag <- subset(ll, class=="DAG", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
dagsums <- c(sum(dag[1],na.rm=TRUE),sum(dag[2],na.rm=TRUE),sum(dag[3],na.rm=TRUE),sum(dag[4],na.rm=TRUE),sum(dag[5],na.rm=TRUE),sum(dag[6],na.rm=TRUE),sum(dag[7],na.rm=TRUE),sum(dag[8],na.rm=TRUE),sum(dag[9],na.rm=TRUE),sum(dag[10],na.rm=TRUE),sum(dag[11],na.rm=TRUE),sum(dag[12],na.rm=TRUE))
dagC1mean <- mean(c(sum(dag[1],na.rm=TRUE),sum(dag[2],na.rm=TRUE),sum(dag[3],na.rm=TRUE),sum(dag[4],na.rm=TRUE)))
dagC3mean <- mean(c(sum(dag[5],na.rm=TRUE),sum(dag[6],na.rm=TRUE),sum(dag[7],na.rm=TRUE),sum(dag[8],na.rm=TRUE)))
dagWmean <- mean(c(sum(dag[9],na.rm=TRUE),sum(dag[10],na.rm=TRUE),sum(dag[11],na.rm=TRUE),sum(dag[12],na.rm=TRUE)))
dagC1sd <- sd(c(sum(dag[1],na.rm=TRUE),sum(dag[2],na.rm=TRUE),sum(dag[3],na.rm=TRUE),sum(dag[4],na.rm=TRUE)))
dagC3sd <- sd(c(sum(dag[5],na.rm=TRUE),sum(dag[6],na.rm=TRUE),sum(dag[7],na.rm=TRUE),sum(dag[8],na.rm=TRUE)))
dagWsd <- sd(c(sum(dag[9],na.rm=TRUE),sum(dag[10],na.rm=TRUE),sum(dag[11],na.rm=TRUE),sum(dag[12],na.rm=TRUE)))

hexcer <- subset(ll, class=="HexCer", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
hexcersums <- c(sum(hexcer[1],na.rm=TRUE),sum(hexcer[2],na.rm=TRUE),sum(hexcer[3],na.rm=TRUE),sum(hexcer[4],na.rm=TRUE),sum(hexcer[5],na.rm=TRUE),sum(hexcer[6],na.rm=TRUE),sum(hexcer[7],na.rm=TRUE),sum(hexcer[8],na.rm=TRUE),sum(hexcer[9],na.rm=TRUE),sum(hexcer[10],na.rm=TRUE),sum(hexcer[11],na.rm=TRUE),sum(hexcer[12],na.rm=TRUE))
hexcerC1mean <- mean(c(sum(hexcer[1],na.rm=TRUE),sum(hexcer[2],na.rm=TRUE),sum(hexcer[3],na.rm=TRUE),sum(hexcer[4],na.rm=TRUE)))
hexcerC3mean <- mean(c(sum(hexcer[5],na.rm=TRUE),sum(hexcer[6],na.rm=TRUE),sum(hexcer[7],na.rm=TRUE),sum(hexcer[8],na.rm=TRUE)))
hexcerWmean <- mean(c(sum(hexcer[9],na.rm=TRUE),sum(hexcer[10],na.rm=TRUE),sum(hexcer[11],na.rm=TRUE),sum(hexcer[12],na.rm=TRUE)))
hexcerC1sd <- sd(c(sum(hexcer[1],na.rm=TRUE),sum(hexcer[2],na.rm=TRUE),sum(hexcer[3],na.rm=TRUE),sum(hexcer[4],na.rm=TRUE)))
hexcerC3sd <- sd(c(sum(hexcer[5],na.rm=TRUE),sum(hexcer[6],na.rm=TRUE),sum(hexcer[7],na.rm=TRUE),sum(hexcer[8],na.rm=TRUE)))
hexcerWsd <- sd(c(sum(hexcer[9],na.rm=TRUE),sum(hexcer[10],na.rm=TRUE),sum(hexcer[11],na.rm=TRUE),sum(hexcer[12],na.rm=TRUE)))

lpc <- subset(ll, class=="LPC", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
lpcsums <- c(sum(lpc[1],na.rm=TRUE),sum(lpc[2],na.rm=TRUE),sum(lpc[3],na.rm=TRUE),sum(lpc[4],na.rm=TRUE),sum(lpc[5],na.rm=TRUE),sum(lpc[6],na.rm=TRUE),sum(lpc[7],na.rm=TRUE),sum(lpc[8],na.rm=TRUE),sum(lpc[9],na.rm=TRUE),sum(lpc[10],na.rm=TRUE),sum(lpc[11],na.rm=TRUE),sum(lpc[12],na.rm=TRUE))
lpcC1mean <- mean(c(sum(lpc[1],na.rm=TRUE),sum(lpc[2],na.rm=TRUE),sum(lpc[3],na.rm=TRUE),sum(lpc[4],na.rm=TRUE)))
lpcC3mean <- mean(c(sum(lpc[5],na.rm=TRUE),sum(lpc[6],na.rm=TRUE),sum(lpc[7],na.rm=TRUE),sum(lpc[8],na.rm=TRUE)))
lpcWmean <- mean(c(sum(lpc[9],na.rm=TRUE),sum(lpc[10],na.rm=TRUE),sum(lpc[11],na.rm=TRUE),sum(lpc[12],na.rm=TRUE)))
lpcC1sd <- sd(c(sum(lpc[1],na.rm=TRUE),sum(lpc[2],na.rm=TRUE),sum(lpc[3],na.rm=TRUE),sum(lpc[4],na.rm=TRUE)))
lpcC3sd <- sd(c(sum(lpc[5],na.rm=TRUE),sum(lpc[6],na.rm=TRUE),sum(lpc[7],na.rm=TRUE),sum(lpc[8],na.rm=TRUE)))
lpcWsd <- sd(c(sum(lpc[9],na.rm=TRUE),sum(lpc[10],na.rm=TRUE),sum(lpc[11],na.rm=TRUE),sum(lpc[12],na.rm=TRUE)))

lpco <- subset(ll, class=="LPC O-", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
lpcosums <- c(sum(lpco[1],na.rm=TRUE),sum(lpco[2],na.rm=TRUE),sum(lpco[3],na.rm=TRUE),sum(lpco[4],na.rm=TRUE),sum(lpco[5],na.rm=TRUE),sum(lpco[6],na.rm=TRUE),sum(lpco[7],na.rm=TRUE),sum(lpco[8],na.rm=TRUE),sum(lpco[9],na.rm=TRUE),sum(lpco[10],na.rm=TRUE),sum(lpco[11],na.rm=TRUE),sum(lpco[12],na.rm=TRUE))
lpcoC1mean <- mean(c(sum(lpco[1],na.rm=TRUE),sum(lpco[2],na.rm=TRUE),sum(lpco[3],na.rm=TRUE),sum(lpco[4],na.rm=TRUE)))
lpcoC3mean <- mean(c(sum(lpco[5],na.rm=TRUE),sum(lpco[6],na.rm=TRUE),sum(lpco[7],na.rm=TRUE),sum(lpco[8],na.rm=TRUE)))
lpcoWmean <- mean(c(sum(lpco[9],na.rm=TRUE),sum(lpco[10],na.rm=TRUE),sum(lpco[11],na.rm=TRUE),sum(lpco[12],na.rm=TRUE)))
lpcoC1sd <- sd(c(sum(lpco[1],na.rm=TRUE),sum(lpco[2],na.rm=TRUE),sum(lpco[3],na.rm=TRUE),sum(lpco[4],na.rm=TRUE)))
lpcoC3sd <- sd(c(sum(lpco[5],na.rm=TRUE),sum(lpco[6],na.rm=TRUE),sum(lpco[7],na.rm=TRUE),sum(lpco[8],na.rm=TRUE)))
lpcoWsd <- sd(c(sum(lpco[9],na.rm=TRUE),sum(lpco[10],na.rm=TRUE),sum(lpco[11],na.rm=TRUE),sum(lpco[12],na.rm=TRUE)))

lpe <- subset(ll, class=="LPE", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
lpesums <- c(sum(lpe[1],na.rm=TRUE),sum(lpe[2],na.rm=TRUE),sum(lpe[3],na.rm=TRUE),sum(lpe[4],na.rm=TRUE),sum(lpe[5],na.rm=TRUE),sum(lpe[6],na.rm=TRUE),sum(lpe[7],na.rm=TRUE),sum(lpe[8],na.rm=TRUE),sum(lpe[9],na.rm=TRUE),sum(lpe[10],na.rm=TRUE),sum(lpe[11],na.rm=TRUE),sum(lpe[12],na.rm=TRUE))
lpeC1mean <- mean(c(sum(lpe[1],na.rm=TRUE),sum(lpe[2],na.rm=TRUE),sum(lpe[3],na.rm=TRUE),sum(lpe[4],na.rm=TRUE)))
lpeC3mean <- mean(c(sum(lpe[5],na.rm=TRUE),sum(lpe[6],na.rm=TRUE),sum(lpe[7],na.rm=TRUE),sum(lpe[8],na.rm=TRUE)))
lpeWmean <- mean(c(sum(lpe[9],na.rm=TRUE),sum(lpe[10],na.rm=TRUE),sum(lpe[11],na.rm=TRUE),sum(lpe[12],na.rm=TRUE)))
lpeC1sd <- sd(c(sum(lpe[1],na.rm=TRUE),sum(lpe[2],na.rm=TRUE),sum(lpe[3],na.rm=TRUE),sum(lpe[4],na.rm=TRUE)))
lpeC3sd <- sd(c(sum(lpe[5],na.rm=TRUE),sum(lpe[6],na.rm=TRUE),sum(lpe[7],na.rm=TRUE),sum(lpe[8],na.rm=TRUE)))
lpeWsd <- sd(c(sum(lpe[9],na.rm=TRUE),sum(lpe[10],na.rm=TRUE),sum(lpe[11],na.rm=TRUE),sum(lpe[12],na.rm=TRUE)))

lpeo <- subset(ll, class=="LPE O-", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
lpeosums <- c(sum(lpeo[1],na.rm=TRUE),sum(lpeo[2],na.rm=TRUE),sum(lpeo[3],na.rm=TRUE),sum(lpeo[4],na.rm=TRUE),sum(lpeo[5],na.rm=TRUE),sum(lpeo[6],na.rm=TRUE),sum(lpeo[7],na.rm=TRUE),sum(lpeo[8],na.rm=TRUE),sum(lpeo[9],na.rm=TRUE),sum(lpeo[10],na.rm=TRUE),sum(lpeo[11],na.rm=TRUE),sum(lpeo[12],na.rm=TRUE))
lpeoC1mean <- mean(c(sum(lpeo[1],na.rm=TRUE),sum(lpeo[2],na.rm=TRUE),sum(lpeo[3],na.rm=TRUE),sum(lpeo[4],na.rm=TRUE)))
lpeoC3mean <- mean(c(sum(lpeo[5],na.rm=TRUE),sum(lpeo[6],na.rm=TRUE),sum(lpeo[7],na.rm=TRUE),sum(lpeo[8],na.rm=TRUE)))
lpeoWmean <- mean(c(sum(lpeo[9],na.rm=TRUE),sum(lpeo[10],na.rm=TRUE),sum(lpeo[11],na.rm=TRUE),sum(lpeo[12],na.rm=TRUE)))
lpeoC1sd <- sd(c(sum(lpeo[1],na.rm=TRUE),sum(lpeo[2],na.rm=TRUE),sum(lpeo[3],na.rm=TRUE),sum(lpeo[4],na.rm=TRUE)))
lpeoC3sd <- sd(c(sum(lpeo[5],na.rm=TRUE),sum(lpeo[6],na.rm=TRUE),sum(lpeo[7],na.rm=TRUE),sum(lpeo[8],na.rm=TRUE)))
lpeoWsd <- sd(c(sum(lpeo[9],na.rm=TRUE),sum(lpeo[10],na.rm=TRUE),sum(lpeo[11],na.rm=TRUE),sum(lpeo[12],na.rm=TRUE)))

lpg <- subset(ll, class=="LPG", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
lpgsums <- c(sum(lpg[1],na.rm=TRUE),sum(lpg[2],na.rm=TRUE),sum(lpg[3],na.rm=TRUE),sum(lpg[4],na.rm=TRUE),sum(lpg[5],na.rm=TRUE),sum(lpg[6],na.rm=TRUE),sum(lpg[7],na.rm=TRUE),sum(lpg[8],na.rm=TRUE),sum(lpg[9],na.rm=TRUE),sum(lpg[10],na.rm=TRUE),sum(lpg[11],na.rm=TRUE),sum(lpg[12],na.rm=TRUE))
lpgC1mean <- mean(c(sum(lpg[1],na.rm=TRUE),sum(lpg[2],na.rm=TRUE),sum(lpg[3],na.rm=TRUE),sum(lpg[4],na.rm=TRUE)))
lpgC3mean <- mean(c(sum(lpg[5],na.rm=TRUE),sum(lpg[6],na.rm=TRUE),sum(lpg[7],na.rm=TRUE),sum(lpg[8],na.rm=TRUE)))
lpgWmean <- mean(c(sum(lpg[9],na.rm=TRUE),sum(lpg[10],na.rm=TRUE),sum(lpg[11],na.rm=TRUE),sum(lpg[12],na.rm=TRUE)))
lpgC1sd <- sd(c(sum(lpg[1],na.rm=TRUE),sum(lpg[2],na.rm=TRUE),sum(lpg[3],na.rm=TRUE),sum(lpg[4],na.rm=TRUE)))
lpgC3sd <- sd(c(sum(lpg[5],na.rm=TRUE),sum(lpg[6],na.rm=TRUE),sum(lpg[7],na.rm=TRUE),sum(lpg[8],na.rm=TRUE)))
lpgWsd <- sd(c(sum(lpg[9],na.rm=TRUE),sum(lpg[10],na.rm=TRUE),sum(lpg[11],na.rm=TRUE),sum(lpg[12],na.rm=TRUE)))

lpi <- subset(ll, class=="LPI", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
lpisums <- c(sum(lpi[1],na.rm=TRUE),sum(lpi[2],na.rm=TRUE),sum(lpi[3],na.rm=TRUE),sum(lpi[4],na.rm=TRUE),sum(lpi[5],na.rm=TRUE),sum(lpi[6],na.rm=TRUE),sum(lpi[7],na.rm=TRUE),sum(lpi[8],na.rm=TRUE),sum(lpi[9],na.rm=TRUE),sum(lpi[10],na.rm=TRUE),sum(lpi[11],na.rm=TRUE),sum(lpi[12],na.rm=TRUE))
lpiC1mean <- mean(c(sum(lpi[1],na.rm=TRUE),sum(lpi[2],na.rm=TRUE),sum(lpi[3],na.rm=TRUE),sum(lpi[4],na.rm=TRUE)))
lpiC3mean <- mean(c(sum(lpi[5],na.rm=TRUE),sum(lpi[6],na.rm=TRUE),sum(lpi[7],na.rm=TRUE),sum(lpi[8],na.rm=TRUE)))
lpiWmean <- mean(c(sum(lpi[9],na.rm=TRUE),sum(lpi[10],na.rm=TRUE),sum(lpi[11],na.rm=TRUE),sum(lpi[12],na.rm=TRUE)))
lpiC1sd <- sd(c(sum(lpi[1],na.rm=TRUE),sum(lpi[2],na.rm=TRUE),sum(lpi[3],na.rm=TRUE),sum(lpi[4],na.rm=TRUE)))
lpiC3sd <- sd(c(sum(lpi[5],na.rm=TRUE),sum(lpi[6],na.rm=TRUE),sum(lpi[7],na.rm=TRUE),sum(lpi[8],na.rm=TRUE)))
lpiWsd <- sd(c(sum(lpi[9],na.rm=TRUE),sum(lpi[10],na.rm=TRUE),sum(lpi[11],na.rm=TRUE),sum(lpi[12],na.rm=TRUE)))

pa <- subset(ll, class=="PA", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
pasums <- c(sum(pa[1],na.rm=TRUE),sum(pa[2],na.rm=TRUE),sum(pa[3],na.rm=TRUE),sum(pa[4],na.rm=TRUE),sum(pa[5],na.rm=TRUE),sum(pa[6],na.rm=TRUE),sum(pa[7],na.rm=TRUE),sum(pa[8],na.rm=TRUE),sum(pa[9],na.rm=TRUE),sum(pa[10],na.rm=TRUE),sum(pa[11],na.rm=TRUE),sum(pa[12],na.rm=TRUE))
paC1mean <- mean(c(sum(pa[1],na.rm=TRUE),sum(pa[2],na.rm=TRUE),sum(pa[3],na.rm=TRUE),sum(pa[4],na.rm=TRUE)))
paC3mean <- mean(c(sum(pa[5],na.rm=TRUE),sum(pa[6],na.rm=TRUE),sum(pa[7],na.rm=TRUE),sum(pa[8],na.rm=TRUE)))
paWmean <- mean(c(sum(pa[9],na.rm=TRUE),sum(pa[10],na.rm=TRUE),sum(pa[11],na.rm=TRUE),sum(pa[12],na.rm=TRUE)))
paC1sd <- sd(c(sum(pa[1],na.rm=TRUE),sum(pa[2],na.rm=TRUE),sum(pa[3],na.rm=TRUE),sum(pa[4],na.rm=TRUE)))
paC3sd <- sd(c(sum(pa[5],na.rm=TRUE),sum(pa[6],na.rm=TRUE),sum(pa[7],na.rm=TRUE),sum(pa[8],na.rm=TRUE)))
paWsd <- sd(c(sum(pa[9],na.rm=TRUE),sum(pa[10],na.rm=TRUE),sum(pa[11],na.rm=TRUE),sum(pa[12],na.rm=TRUE)))

pc <- subset(ll, class=="PC", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
pcsums <- c(sum(pc[1],na.rm=TRUE),sum(pc[2],na.rm=TRUE),sum(pc[3],na.rm=TRUE),sum(pc[4],na.rm=TRUE),sum(pc[5],na.rm=TRUE),sum(pc[6],na.rm=TRUE),sum(pc[7],na.rm=TRUE),sum(pc[8],na.rm=TRUE),sum(pc[9],na.rm=TRUE),sum(pc[10],na.rm=TRUE),sum(pc[11],na.rm=TRUE),sum(pc[12],na.rm=TRUE))
pcC1mean <- mean(c(sum(pc[1],na.rm=TRUE),sum(pc[2],na.rm=TRUE),sum(pc[3],na.rm=TRUE),sum(pc[4],na.rm=TRUE)))
pcC3mean <- mean(c(sum(pc[5],na.rm=TRUE),sum(pc[6],na.rm=TRUE),sum(pc[7],na.rm=TRUE),sum(pc[8],na.rm=TRUE)))
pcWmean <- mean(c(sum(pc[9],na.rm=TRUE),sum(pc[10],na.rm=TRUE),sum(pc[11],na.rm=TRUE),sum(pc[12],na.rm=TRUE)))
pcC1sd <- sd(c(sum(pc[1],na.rm=TRUE),sum(pc[2],na.rm=TRUE),sum(pc[3],na.rm=TRUE),sum(pc[4],na.rm=TRUE)))
pcC3sd <- sd(c(sum(pc[5],na.rm=TRUE),sum(pc[6],na.rm=TRUE),sum(pc[7],na.rm=TRUE),sum(pc[8],na.rm=TRUE)))
pcWsd <- sd(c(sum(pc[9],na.rm=TRUE),sum(pc[10],na.rm=TRUE),sum(pc[11],na.rm=TRUE),sum(pc[12],na.rm=TRUE)))

pco <- subset(ll, class=="PC O-", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
pcosums <- c(sum(pco[1],na.rm=TRUE),sum(pco[2],na.rm=TRUE),sum(pco[3],na.rm=TRUE),sum(pco[4],na.rm=TRUE),sum(pco[5],na.rm=TRUE),sum(pco[6],na.rm=TRUE),sum(pco[7],na.rm=TRUE),sum(pco[8],na.rm=TRUE),sum(pco[9],na.rm=TRUE),sum(pco[10],na.rm=TRUE),sum(pco[11],na.rm=TRUE),sum(pco[12],na.rm=TRUE))
pcoC1mean <- mean(c(sum(pco[1],na.rm=TRUE),sum(pco[2],na.rm=TRUE),sum(pco[3],na.rm=TRUE),sum(pco[4],na.rm=TRUE)))
pcoC3mean <- mean(c(sum(pco[5],na.rm=TRUE),sum(pco[6],na.rm=TRUE),sum(pco[7],na.rm=TRUE),sum(pco[8],na.rm=TRUE)))
pcoWmean <- mean(c(sum(pco[9],na.rm=TRUE),sum(pco[10],na.rm=TRUE),sum(pco[11],na.rm=TRUE),sum(pco[12],na.rm=TRUE)))
pcoC1sd <- sd(c(sum(pco[1],na.rm=TRUE),sum(pco[2],na.rm=TRUE),sum(pco[3],na.rm=TRUE),sum(pco[4],na.rm=TRUE)))
pcoC3sd <- sd(c(sum(pco[5],na.rm=TRUE),sum(pco[6],na.rm=TRUE),sum(pco[7],na.rm=TRUE),sum(pco[8],na.rm=TRUE)))
pcoWsd <- sd(c(sum(pco[9],na.rm=TRUE),sum(pco[10],na.rm=TRUE),sum(pco[11],na.rm=TRUE),sum(pco[12],na.rm=TRUE)))

pe <- subset(ll, class=="PE", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
pesums <- c(sum(pe[1],na.rm=TRUE),sum(pe[2],na.rm=TRUE),sum(pe[3],na.rm=TRUE),sum(pe[4],na.rm=TRUE),sum(pe[5],na.rm=TRUE),sum(pe[6],na.rm=TRUE),sum(pe[7],na.rm=TRUE),sum(pe[8],na.rm=TRUE),sum(pe[9],na.rm=TRUE),sum(pe[10],na.rm=TRUE),sum(pe[11],na.rm=TRUE),sum(pe[12],na.rm=TRUE))
peC1mean <- mean(c(sum(pe[1],na.rm=TRUE),sum(pe[2],na.rm=TRUE),sum(pe[3],na.rm=TRUE),sum(pe[4],na.rm=TRUE)))
peC3mean <- mean(c(sum(pe[5],na.rm=TRUE),sum(pe[6],na.rm=TRUE),sum(pe[7],na.rm=TRUE),sum(pe[8],na.rm=TRUE)))
peWmean <- mean(c(sum(pe[9],na.rm=TRUE),sum(pe[10],na.rm=TRUE),sum(pe[11],na.rm=TRUE),sum(pe[12],na.rm=TRUE)))
peC1sd <- sd(c(sum(pe[1],na.rm=TRUE),sum(pe[2],na.rm=TRUE),sum(pe[3],na.rm=TRUE),sum(pe[4],na.rm=TRUE)))
peC3sd <- sd(c(sum(pe[5],na.rm=TRUE),sum(pe[6],na.rm=TRUE),sum(pe[7],na.rm=TRUE),sum(pe[8],na.rm=TRUE)))
peWsd <- sd(c(sum(pe[9],na.rm=TRUE),sum(pe[10],na.rm=TRUE),sum(pe[11],na.rm=TRUE),sum(pe[12],na.rm=TRUE)))

peo <- subset(ll, class=="PE O-", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
peosums <- c(sum(peo[1],na.rm=TRUE),sum(peo[2],na.rm=TRUE),sum(peo[3],na.rm=TRUE),sum(peo[4],na.rm=TRUE),sum(peo[5],na.rm=TRUE),sum(peo[6],na.rm=TRUE),sum(peo[7],na.rm=TRUE),sum(peo[8],na.rm=TRUE),sum(peo[9],na.rm=TRUE),sum(peo[10],na.rm=TRUE),sum(peo[11],na.rm=TRUE),sum(peo[12],na.rm=TRUE))
peoC1mean <- mean(c(sum(peo[1],na.rm=TRUE),sum(peo[2],na.rm=TRUE),sum(peo[3],na.rm=TRUE),sum(peo[4],na.rm=TRUE)))
peoC3mean <- mean(c(sum(peo[5],na.rm=TRUE),sum(peo[6],na.rm=TRUE),sum(peo[7],na.rm=TRUE),sum(peo[8],na.rm=TRUE)))
peoWmean <- mean(c(sum(peo[9],na.rm=TRUE),sum(peo[10],na.rm=TRUE),sum(peo[11],na.rm=TRUE),sum(peo[12],na.rm=TRUE)))
peoC1sd <- sd(c(sum(peo[1],na.rm=TRUE),sum(peo[2],na.rm=TRUE),sum(peo[3],na.rm=TRUE),sum(peo[4],na.rm=TRUE)))
peoC3sd <- sd(c(sum(peo[5],na.rm=TRUE),sum(peo[6],na.rm=TRUE),sum(peo[7],na.rm=TRUE),sum(peo[8],na.rm=TRUE)))
peoWsd <- sd(c(sum(peo[9],na.rm=TRUE),sum(peo[10],na.rm=TRUE),sum(peo[11],na.rm=TRUE),sum(peo[12],na.rm=TRUE)))

pg <- subset(ll, class=="PG", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
pgsums <- c(sum(pg[1],na.rm=TRUE),sum(pg[2],na.rm=TRUE),sum(pg[3],na.rm=TRUE),sum(pg[4],na.rm=TRUE),sum(pg[5],na.rm=TRUE),sum(pg[6],na.rm=TRUE),sum(pg[7],na.rm=TRUE),sum(pg[8],na.rm=TRUE),sum(pg[9],na.rm=TRUE),sum(pg[10],na.rm=TRUE),sum(pg[11],na.rm=TRUE),sum(pg[12],na.rm=TRUE))
pgC1mean <- mean(c(sum(pg[1],na.rm=TRUE),sum(pg[2],na.rm=TRUE),sum(pg[3],na.rm=TRUE),sum(pg[4],na.rm=TRUE)))
pgC3mean <- mean(c(sum(pg[5],na.rm=TRUE),sum(pg[6],na.rm=TRUE),sum(pg[7],na.rm=TRUE),sum(pg[8],na.rm=TRUE)))
pgWmean <- mean(c(sum(pg[9],na.rm=TRUE),sum(pg[10],na.rm=TRUE),sum(pg[11],na.rm=TRUE),sum(pg[12],na.rm=TRUE)))
pgC1sd <- sd(c(sum(pg[1],na.rm=TRUE),sum(pg[2],na.rm=TRUE),sum(pg[3],na.rm=TRUE),sum(pg[4],na.rm=TRUE)))
pgC3sd <- sd(c(sum(pg[5],na.rm=TRUE),sum(pg[6],na.rm=TRUE),sum(pg[7],na.rm=TRUE),sum(pg[8],na.rm=TRUE)))
pgWsd <- sd(c(sum(pg[9],na.rm=TRUE),sum(pg[10],na.rm=TRUE),sum(pg[11],na.rm=TRUE),sum(pg[12],na.rm=TRUE)))

pi <- subset(ll, class=="PI", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
pisums <- c(sum(pi[1],na.rm=TRUE),sum(pi[2],na.rm=TRUE),sum(pi[3],na.rm=TRUE),sum(pi[4],na.rm=TRUE),sum(pi[5],na.rm=TRUE),sum(pi[6],na.rm=TRUE),sum(pi[7],na.rm=TRUE),sum(pi[8],na.rm=TRUE),sum(pi[9],na.rm=TRUE),sum(pi[10],na.rm=TRUE),sum(pi[11],na.rm=TRUE),sum(pi[12],na.rm=TRUE))
piC1mean <- mean(c(sum(pi[1],na.rm=TRUE),sum(pi[2],na.rm=TRUE),sum(pi[3],na.rm=TRUE),sum(pi[4],na.rm=TRUE)))
piC3mean <- mean(c(sum(pi[5],na.rm=TRUE),sum(pi[6],na.rm=TRUE),sum(pi[7],na.rm=TRUE),sum(pi[8],na.rm=TRUE)))
piWmean <- mean(c(sum(pi[9],na.rm=TRUE),sum(pi[10],na.rm=TRUE),sum(pi[11],na.rm=TRUE),sum(pi[12],na.rm=TRUE)))
piC1sd <- sd(c(sum(pi[1],na.rm=TRUE),sum(pi[2],na.rm=TRUE),sum(pi[3],na.rm=TRUE),sum(pi[4],na.rm=TRUE)))
piC3sd <- sd(c(sum(pi[5],na.rm=TRUE),sum(pi[6],na.rm=TRUE),sum(pi[7],na.rm=TRUE),sum(pi[8],na.rm=TRUE)))
piWsd <- sd(c(sum(pi[9],na.rm=TRUE),sum(pi[10],na.rm=TRUE),sum(pi[11],na.rm=TRUE),sum(pi[12],na.rm=TRUE)))

ps <- subset(ll, class=="PS", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
pssums <- c(sum(ps[1],na.rm=TRUE),sum(ps[2],na.rm=TRUE),sum(ps[3],na.rm=TRUE),sum(ps[4],na.rm=TRUE),sum(ps[5],na.rm=TRUE),sum(ps[6],na.rm=TRUE),sum(ps[7],na.rm=TRUE),sum(ps[8],na.rm=TRUE),sum(ps[9],na.rm=TRUE),sum(ps[10],na.rm=TRUE),sum(ps[11],na.rm=TRUE),sum(ps[12],na.rm=TRUE))
psC1mean <- mean(c(sum(ps[1],na.rm=TRUE),sum(ps[2],na.rm=TRUE),sum(ps[3],na.rm=TRUE),sum(ps[4],na.rm=TRUE)))
psC3mean <- mean(c(sum(ps[5],na.rm=TRUE),sum(ps[6],na.rm=TRUE),sum(ps[7],na.rm=TRUE),sum(ps[8],na.rm=TRUE)))
psWmean <- mean(c(sum(ps[9],na.rm=TRUE),sum(ps[10],na.rm=TRUE),sum(ps[11],na.rm=TRUE),sum(ps[12],na.rm=TRUE)))
psC1sd <- sd(c(sum(ps[1],na.rm=TRUE),sum(ps[2],na.rm=TRUE),sum(ps[3],na.rm=TRUE),sum(ps[4],na.rm=TRUE)))
psC3sd <- sd(c(sum(ps[5],na.rm=TRUE),sum(ps[6],na.rm=TRUE),sum(ps[7],na.rm=TRUE),sum(ps[8],na.rm=TRUE)))
psWsd <- sd(c(sum(ps[9],na.rm=TRUE),sum(ps[10],na.rm=TRUE),sum(ps[11],na.rm=TRUE),sum(ps[12],na.rm=TRUE)))

sm <- subset(ll, class=="SM", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
smsums <- c(sum(sm[1],na.rm=TRUE),sum(sm[2],na.rm=TRUE),sum(sm[3],na.rm=TRUE),sum(sm[4],na.rm=TRUE),sum(sm[5],na.rm=TRUE),sum(sm[6],na.rm=TRUE),sum(sm[7],na.rm=TRUE),sum(sm[8],na.rm=TRUE),sum(sm[9],na.rm=TRUE),sum(sm[10],na.rm=TRUE),sum(sm[11],na.rm=TRUE),sum(sm[12],na.rm=TRUE))
smC1mean <- mean(c(sum(sm[1],na.rm=TRUE),sum(sm[2],na.rm=TRUE),sum(sm[3],na.rm=TRUE),sum(sm[4],na.rm=TRUE)))
smC3mean <- mean(c(sum(sm[5],na.rm=TRUE),sum(sm[6],na.rm=TRUE),sum(sm[7],na.rm=TRUE),sum(sm[8],na.rm=TRUE)))
smWmean <- mean(c(sum(sm[9],na.rm=TRUE),sum(sm[10],na.rm=TRUE),sum(sm[11],na.rm=TRUE),sum(sm[12],na.rm=TRUE)))
smC1sd <- sd(c(sum(sm[1],na.rm=TRUE),sum(sm[2],na.rm=TRUE),sum(sm[3],na.rm=TRUE),sum(sm[4],na.rm=TRUE)))
smC3sd <- sd(c(sum(sm[5],na.rm=TRUE),sum(sm[6],na.rm=TRUE),sum(sm[7],na.rm=TRUE),sum(sm[8],na.rm=TRUE)))
smWsd <- sd(c(sum(sm[9],na.rm=TRUE),sum(sm[10],na.rm=TRUE),sum(sm[11],na.rm=TRUE),sum(sm[12],na.rm=TRUE)))

sulf <- subset(ll, class=="Sulf", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
sulfsums <- c(sum(sulf[1],na.rm=TRUE),sum(sulf[2],na.rm=TRUE),sum(sulf[3],na.rm=TRUE),sum(sulf[4],na.rm=TRUE),sum(sulf[5],na.rm=TRUE),sum(sulf[6],na.rm=TRUE),sum(sulf[7],na.rm=TRUE),sum(sulf[8],na.rm=TRUE),sum(sulf[9],na.rm=TRUE),sum(sulf[10],na.rm=TRUE),sum(sulf[11],na.rm=TRUE),sum(sulf[12],na.rm=TRUE))
sulfC1mean <- mean(c(sum(sulf[1],na.rm=TRUE),sum(sulf[2],na.rm=TRUE),sum(sulf[3],na.rm=TRUE),sum(sulf[4],na.rm=TRUE)))
sulfC3mean <- mean(c(sum(sulf[5],na.rm=TRUE),sum(sulf[6],na.rm=TRUE),sum(sulf[7],na.rm=TRUE),sum(sulf[8],na.rm=TRUE)))
sulfWmean <- mean(c(sum(sulf[9],na.rm=TRUE),sum(sulf[10],na.rm=TRUE),sum(sulf[11],na.rm=TRUE),sum(sulf[12],na.rm=TRUE)))
sulfC1sd <- sd(c(sum(sulf[1],na.rm=TRUE),sum(sulf[2],na.rm=TRUE),sum(sulf[3],na.rm=TRUE),sum(sulf[4],na.rm=TRUE)))
sulfC3sd <- sd(c(sum(sulf[5],na.rm=TRUE),sum(sulf[6],na.rm=TRUE),sum(sulf[7],na.rm=TRUE),sum(sulf[8],na.rm=TRUE)))
sulfWsd <- sd(c(sum(sulf[9],na.rm=TRUE),sum(sulf[10],na.rm=TRUE),sum(sulf[11],na.rm=TRUE),sum(sulf[12],na.rm=TRUE)))

tag <- subset(ll, class=="TAG", select = c(C1L1, C1L2, C1L3, C1L4, C3L1, C3L2, C3L3, C3L4, WL1, WL2, WL3, WL4))
tagsums <- c(sum(tag[1],na.rm=TRUE),sum(tag[2],na.rm=TRUE),sum(tag[3],na.rm=TRUE),sum(tag[4],na.rm=TRUE),sum(tag[5],na.rm=TRUE),sum(tag[6],na.rm=TRUE),sum(tag[7],na.rm=TRUE),sum(tag[8],na.rm=TRUE),sum(tag[9],na.rm=TRUE),sum(tag[10],na.rm=TRUE),sum(tag[11],na.rm=TRUE),sum(tag[12],na.rm=TRUE))
tagC1mean <- mean(c(sum(tag[1],na.rm=TRUE),sum(tag[2],na.rm=TRUE),sum(tag[3],na.rm=TRUE),sum(tag[4],na.rm=TRUE)))
tagC3mean <- mean(c(sum(tag[5],na.rm=TRUE),sum(tag[6],na.rm=TRUE),sum(tag[7],na.rm=TRUE),sum(tag[8],na.rm=TRUE)))
tagWmean <- mean(c(sum(tag[9],na.rm=TRUE),sum(tag[10],na.rm=TRUE),sum(tag[11],na.rm=TRUE),sum(tag[12],na.rm=TRUE)))
tagC1sd <- sd(c(sum(tag[1],na.rm=TRUE),sum(tag[2],na.rm=TRUE),sum(tag[3],na.rm=TRUE),sum(tag[4],na.rm=TRUE)))
tagC3sd <- sd(c(sum(tag[5],na.rm=TRUE),sum(tag[6],na.rm=TRUE),sum(tag[7],na.rm=TRUE),sum(tag[8],na.rm=TRUE)))
tagWsd <- sd(c(sum(tag[9],na.rm=TRUE),sum(tag[10],na.rm=TRUE),sum(tag[11],na.rm=TRUE),sum(tag[12],na.rm=TRUE)))


r1 <- c(ceC1mean,ceC3mean,ceWmean,ceC1sd,ceC3sd,ceWsd)
r2 <- c(cerC1mean,cerC3mean,cerWmean,cerC1sd,cerC3sd,cerWsd)
r3 <- c(clC1mean,clC3mean,clWmean,clC1sd,clC3sd,clWsd)
r4 <- c(dagC1mean,dagC3mean,dagWmean,dagC1sd,dagC3sd,dagWsd)
r5 <- c(hexcerC1mean,hexcerC3mean,hexcerWmean,hexcerC1sd,hexcerC3sd,hexcerWsd)
r6 <- c(lpcC1mean,lpcC3mean,lpcWmean,lpcC1sd,lpcC3sd,lpcWsd)
r7 <- c(lpcoC1mean,lpcoC3mean,lpcoWmean,lpcoC1sd,lpcoC3sd,lpcoWsd)
r8 <- c(lpeC1mean,lpeC3mean,lpeWmean,lpeC1sd,lpeC3sd,lpeWsd)
r9 <- c(lpeoC1mean,lpeoC3mean,lpeoWmean,lpeoC1sd,lpeoC3sd,lpeoWsd)
r10 <- c(lpgC1mean,lpgC3mean,lpgWmean,lpgC1sd,lpgC3sd,lpgWsd)
r11 <- c(lpiC1mean,lpiC3mean,lpiWmean,lpiC1sd,lpiC3sd,lpiWsd)
r12 <- c(paC1mean,paC3mean,paWmean,paC1sd,paC3sd,paWsd)
r13 <- c(pcC1mean,pcC3mean,pcWmean,pcC1sd,pcC3sd,pcWsd)
r14 <- c(pcoC1mean,pcoC3mean,pcoWmean,pcoC1sd,pcoC3sd,pcoWsd)
r15 <- c(peC1mean,peC3mean,peWmean,peC1sd,peC3sd,peWsd)
r16 <- c(peoC1mean,peoC3mean,peoWmean,peoC1sd,peoC3sd,peoWsd)
r17 <- c(pgC1mean,pgC3mean,pgWmean,pgC1sd,pgC3sd,pgWsd)
r18 <- c(piC1mean,piC3mean,piWmean,piC1sd,piC3sd,piWsd)
r19 <- c(psC1mean,psC3mean,psWmean,psC1sd,psC3sd,psWsd)
r20 <- c(smC1mean,smC3mean,smWmean,smC1sd,smC3sd,smWsd)
r21 <- c(sulfC1mean,sulfC3mean,sulfWmean,sulfC1sd,sulfC3sd,sulfWsd)
r22 <- c(tagC1mean,tagC3mean,tagWmean,tagC1sd,tagC3sd,tagWsd)

rs <- rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r21,r22)
lysmeansdtable <- cbind(unique(ll[2]),rs)
colnames(lysmeansdtable) <- c("Class","C11 Mean","C13 Mean", "WT mean","C11 sd","C13 sd", "WT sd")

group <- c("1","1","1","1","2","2","2","2","3","3","3","3")
sums <- rbind(cesums,cersums,clsums,dagsums,hexcersums,lpcsums,lpcosums,lpesums,lpeosums,lpgsums,lpisums,pasums,pcsums,pcosums,pesums,peosums,pgsums,pisums,pssums,smsums,sulfsums,tagsums,group)
colnames(sums) <- c("C11-1","C11-2","C11-3","C11-4","C13-1","C13-2","C13-3","C13-4","WT-1","WT-2","WT-3","WT-4")
tsums <- t(sums)
tsums <- as.table(tsums)
write.csv(tsums, file="testfilel", append=FALSE)
tsums <- read.csv("testfilel", header = TRUE)


########################StatisticalTesting
C1Wpval <- list()
for(i in c(tsums[c(1:4,9:12),2:23]))
{
  boop <- t.test(i~tsums[c(1:4,9:12),24])$p.value
  C1Wpval[[length(C1Wpval)+1]] = boop
}

C1Wpval2 <- unlist(C1Wpval, recursive = TRUE, use.names = FALSE)
C1Wqvals <- qvalue(C1Wpval2,pi0=1)$qvalues



C3Wpval <- list()
for(i in c(tsums[5:12,2:23]))
{
  moop <- t.test(i~tsums[5:12,24])$p.value
  C3Wpval[[length(C3Wpval)+1]] = moop
}

C3Wpval2 <- unlist(C3Wpval, recursive = TRUE, use.names = FALSE)
C3Wqvals <- qvalue(C3Wpval2,pi0=1)$qvalues


C1C3pval <- list()
for(i in c(tsums[1:8,2:23]))
{
  snoop <- t.test(i~tsums[1:8,24])$p.value
  C1C3pval[[length(C1C3pval)+1]] = snoop
}

C1C3pval2 <- unlist(C1C3pval, recursive = TRUE, use.names = FALSE)
C1C3qvals <- qvalue(C1C3pval2,pi0=1)$qvalues



lysmeanssdqptable <- cbind(lysmeansdtable, C1Wpval2, C1Wqvals,C3Wpval2, C3Wqvals,C1C3pval2, C1C3qvals)

########################Output
write.csv(lysmeanssdqptable, "NewLysosomesOutput.csv", append=FALSE)
write.csv(sums, "NewLysosomesSums.csv", append=FALSE)