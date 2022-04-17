install.packages("tidyverse")
tidyverse_update()


rm(list = ls(all = TRUE))
library(tidyverse)
library(qvalue)

setwd("enter_filepath")
getwd()

ll <- read.csv("enter_filename", header=TRUE)
is.tibble(ll)
tll <- as_tibble(ll)
is_tibble(tll)

#Code to remove columns with >50% NA values
ntll <- tll %>% 
  purrr::discard(~sum(is.na(.x))/length(.x)* 100 >50)


#Impute all remaining NA values with half of the minimum value in that column
hmlem <- c()
for (j in 3:ncol(ntll)){hmlem <- c(hmlem,min(ntll[,j],na.rm=T)/2)}
for (j in 3:ncol(ntll)){ntll[is.na(ntll[,j]),j] <- hmlem[j-2]}


CKO1_means <- ntll %>%
  filter(Group=="CKO1") %>%
  select(-(Sample:Group)) %>%
  sapply(function(x) mean(x))

CKO1_sd <- ntll %>%
  filter(Group=="CKO1") %>%
  select(-(Sample:Group)) %>%
  sapply(function(x) sd(x))

CKO3_means <- ntll %>%
  filter(Group=="CKO3") %>%
  select(-(Sample:Group)) %>%
  sapply(function(x) mean(x))

CKO3_sd <- ntll %>%
  filter(Group=="CKO3") %>%
  select(-(Sample:Group)) %>%
  sapply(function(x) sd(x))

WT_means <- ntll %>%
  filter(Group=="WT") %>%
  select(-(Sample:Group)) %>%
  sapply(function(x) mean(x))

WT_sd <- ntll %>%
  filter(Group=="WT") %>%
  select(-(Sample:Group)) %>%
  sapply(function(x) sd(x))

Fold_Change1 <- CKO1_means/WT_means
head(folds)

Fold_Change3 <- CKO3_means/WT_means
head(folds)


pvsCKO1 <- ntll %>%
  summarise_each(funs(t.test(.[Group == "CKO1"], .[Group == "WT"])$p.value), vars = CE.14.0.0:TAG.52.2.0)

p_valsCKO1 <- pvsCKO1  %>%
  slice(1) %>% 
  unlist(., use.names=FALSE)

q_valsCKO1 <- qvalue(p_valsCKO1)$qvalues

pvsCKO3 <- ntll %>%
  summarise_each(funs(t.test(.[Group == "CKO3"], .[Group == "WT"])$p.value), vars = CE.14.0.0:TAG.52.2.0)

p_valsCKO3 <- pvsCKO3  %>%
  slice(1) %>% 
  unlist(., use.names=FALSE)

q_valsCKO3 <- qvalue(p_valsCKO3)$qvalues

Species <- names(ntll[3:ncol(ntll)])



BHp_valsCKO1 <- p.adjust(p_valsCKO1, method="BH", n=length(p_valsCKO1))
logFold_ChangeCKO1 <- log2(Fold_Change1)
logBHp_valsCKO1 <- (-log10(BHp_valsCKO1))
logp_valsCKO1 <- (-log10(p_valsCKO1))

BHp_valsCKO3 <- p.adjust(p_valsCKO3, method="BH", n=length(p_valsCKO3))
logFold_ChangeCKO3 <- log2(Fold_Change3)
logBHp_valsCKO3 <- (-log10(BHp_valsCKO3))
logp_valsCKO3 <- (-log10(p_valsCKO3))

stufftable  <- cbind(Species,Fold_Change1,Fold_Change3,logFold_ChangeCKO1,logFold_ChangeCKO3,p_valsCKO1,p_valsCKO3,logp_valsCKO1,logp_valsCKO3,BHp_valsCKO1,BHp_valsCKO3,logBHp_valsCKO1,logBHp_valsCKO3,CKO1_means,CKO1_sd,CKO3_means,CKO3_sd,WT_means,WT_sd)
stufftibble <- as_tibble(stufftable)
ppri <- arrange(stufftibble,desc(logp_vals))
view(ppri)
fcpri1 <- arrange(stufftibble,desc(logFold_ChangeCKO1))
view(fcpri1)


res1 <- cbind(logFold_ChangeCKO1,BHp_valsCKO1)
res3 <- cbind(logFold_ChangeCKO3,BHp_valsCKO3)


pqbh1 <- cbind(p_valsCKO1,q_valsCKO1,BHp_valsCKO1)
pqbh3 <- cbind(p_valsCKO3,q_valsCKO3,BHp_valsCKO3)

#filter(stufftibble, BHp_valsCKO1<0.05 AND BHp_valsCKO1<0.05)
ORsigstufftibble <- filter(stufftibble, BHp_valsCKO1<0.05|BHp_valsCKO3<0.05)
ANDsigstufftibble <- filter(stufftibble, BHp_valsCKO1<0.05&BHp_valsCKO3<0.05)

write.csv(ANDsigstufftibble, "NewLysSigSpeciesBoth11and13.csv", append=FALSE)


if (!requireNamespace('BiocManager', quietly = TRUE))
  install.packages('BiocManager')
BiocManager::install('EnhancedVolcano')

#Adding a column of absolute value of logFoldchanges to facilitate sorting, because for some reason I can't get it to handle logical functions of negative values correctly
AbslogFold_ChangeCKO1 <- abs(logFold_ChangeCKO1)
AbslogFold_ChangeCKO3 <- abs(logFold_ChangeCKO3)
Absstufftable  <- cbind(Species,Fold_Change1,Fold_Change3,logFold_ChangeCKO1,logFold_ChangeCKO3,p_valsCKO1,p_valsCKO3,logp_valsCKO1,logp_valsCKO3,BHp_valsCKO1,BHp_valsCKO3,logBHp_valsCKO1,logBHp_valsCKO3,CKO1_means,CKO1_sd,CKO3_means,CKO3_sd,WT_means,WT_sd,AbslogFold_ChangeCKO1,AbslogFold_ChangeCKO3)
Absstufftibble <- as_tibble(Absstufftable)
AbsANDsigstufftibble <- filter(Absstufftibble, BHp_valsCKO1<0.05&BHp_valsCKO3<0.05)
AbsANDsigFoldChange <- filter(AbsANDsigstufftibble, AbslogFold_ChangeCKO1>1&AbslogFold_ChangeCKO3>1)

labs_list <- arrange(AbsANDsigFoldChange, desc(AbslogFold_ChangeCKO1))
labs_list <- slice(labs_list, 1:40)
volc_labels <- pull(labs_list, var=Species)
volc_labels1 <- c(volc_labels,"PC.18.0.0_22.6.0")
volc_labels2 <- c(volc_labels,"PE.18.0.0_20.5.0")
volc_labels2 <- c(volc_labels2[1:21],volc_labels2[23:41])

library(EnhancedVolcano)


EnhancedVolcano(res1,
                lab = rownames(res1),
                x = 'logFold_ChangeCKO1',
                y = 'BHp_valsCKO1',
                pCutoff = 0.05,
                FCcutoff = 1,
                xlim = c(-4.5, 4.5),
                ylim = c(0,4),
                col = c("grey30", "grey30", "grey30", "red2"),
                selectLab = volc_labels1,
                transcriptLabSize = 6.0,
                axisLabSize = 24,
                transcriptPointSize = 2)

EnhancedVolcano(res3,
                lab = rownames(res3),
                x = 'logFold_ChangeCKO3',
                y = 'BHp_valsCKO3',
                pCutoff = 0.05,
                FCcutoff = 1,
                xlim = c(-3.5, 3.5),
                ylim = c(0,3),
                col = c("grey30", "grey30", "grey30", "red2"),
                selectLab = volc_labels2,
                transcriptLabSize = 6.0,
                axisLabSize = 24,
                transcriptPointSize = 2)

write.csv(AbsANDsigFoldChange, "NewLysSigPsigFCSpeciesBoth11and13.csv", append=FALSE)


sigps <- slice(ppri, 1:47)
sigpsr <- arrange(sigps, Species)
sigFolds <- sigpsr  %>%
  pull(3) %>% 
  unlist(., use.names=FALSE)
sigFolds <- as.numeric(sigFolds)
hmRnames <- sigpsr  %>%
  pull(1) %>% 
  unlist(., use.names=FALSE)
zeros <- rep(0,47)
a<-rbind(zeros,sigFolds)

aa<-rbind(hmRnames,sigFolds)

png("barplot",          
    width = 5*300,        
    height = 2*300,
    res = 300,           
    pointsize = 8)  
barplot2(sigFolds, names.arg=hmRnames, las=2)
dev.off()

