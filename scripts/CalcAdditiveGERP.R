## Looking at additive model vs recessive

Additive <- read.delim("~/Buckler_lab/Hufford_lab/Genetic_Load/Redo/ForAnalysis.txt")

FilteredAdditive <- filter(Additive, GERP > 0)

Inds <- FilteredAdditive[,c(7:54)]

total <- matrix(NA,48,2)

for (j in 1:ncol(Inds)){

A <- substr(Inds[,j],1,1)
B <- substr(Inds[,j],2,2)


for (i in 1:length(A)){
  score <- as.numeric(as.character(FilteredAdditive[i,5]))
  derived <- as.character(FilteredAdditive[i,3])
  if(A[i] == derived){
    A[i] <- score
  } else{
    A[i] <- 0
  }
  if(B[i] == derived){
    B[i] <- score
  } else{
    B[i] <- 0
  }
}

total[j,1] <- colnames(Inds[j])
total[j,2] <- sum(as.numeric(A)) + sum(as.numeric(B))
}

AddGERP <- total




### Now redo for recessive model

Inds <- FilteredAdditive[,c(7:54)]

total <- matrix(NA,48,2)

for (j in 1:ncol(Inds)){
  
  A <- as.character(Inds[,j])
  
  for (i in 1:length(A)){
    score <- as.numeric(as.character(FilteredAdditive[i,5]))
    derived <- as.character(FilteredAdditive[i,4])
    if(A[i] == derived){
      A[i] <- score
    } else{
      A[i] <- 0
    }
    
  }
  
  total[j,1] <- colnames(Inds[j])
  total[j,2] <- sum(as.numeric(A))
}

RecGERP <- total

### Now filtering on additive GERP with scores > 4

HighBinAdditive <- filter(Additive, GERP > 4)

Inds <- HighBinAdditive[,c(7:54)]

total <- matrix(NA,48,2)

for (j in 1:ncol(Inds)){
  
  A <- substr(Inds[,j],1,1)
  B <- substr(Inds[,j],2,2)
  
  
  for (i in 1:length(A)){
    score <- as.numeric(as.character(HighBinAdditive[i,5]))
    derived <- as.character(HighBinAdditive[i,3])
    if(A[i] == derived){
      A[i] <- score
    } else{
      A[i] <- 0
    }
    if(B[i] == derived){
      B[i] <- score
    } else{
      B[i] <- 0
    }
  }
  
  total[j,1] <- colnames(Inds[j])
  total[j,2] <- sum(as.numeric(A)) + sum(as.numeric(B))
}

HighAddGERP <- total



#### Creating bins for figure

## 0 < GERP <= 2
Additive02 <- filter(Additive, GERP > 0 & GERP <= 2)

Inds <- Additive02[,c(7:54)]

total <- matrix(NA,48,2)

for (j in 1:ncol(Inds)){
  
  A <- substr(Inds[,j],1,1)
  B <- substr(Inds[,j],2,2)
  
  
  for (i in 1:length(A)){
    score <- as.numeric(as.character(Additive02[i,5]))
    derived <- as.character(Additive02[i,3])
    if(A[i] == derived){
      A[i] <- score
    } else{
      A[i] <- 0
    }
    if(B[i] == derived){
      B[i] <- score
    } else{
      B[i] <- 0
    }
  }
  
  total[j,1] <- colnames(Inds[j])
  total[j,2] <- sum(as.numeric(A)) + sum(as.numeric(B))
}

AddGERP_02 <- total

## 2 < GERP <= 4
Additive24 <- filter(Additive, GERP > 2 & GERP <= 4)

Inds <- Additive24[,c(7:54)]

total <- matrix(NA,48,2)

for (j in 1:ncol(Inds)){
  
  A <- substr(Inds[,j],1,1)
  B <- substr(Inds[,j],2,2)
  
  
  for (i in 1:length(A)){
    score <- as.numeric(as.character(Additive24[i,5]))
    derived <- as.character(Additive24[i,3])
    if(A[i] == derived){
      A[i] <- score
    } else{
      A[i] <- 0
    }
    if(B[i] == derived){
      B[i] <- score
    } else{
      B[i] <- 0
    }
  }
  
  total[j,1] <- colnames(Inds[j])
  total[j,2] <- sum(as.numeric(A)) + sum(as.numeric(B))
}

AddGERP_24 <- total

## GERP > 4
Additive4 <- filter(Additive, GERP > 4)

Inds <- Additive4[,c(7:54)]

total <- matrix(NA,48,2)

for (j in 1:ncol(Inds)){
  
  A <- substr(Inds[,j],1,1)
  B <- substr(Inds[,j],2,2)
  
  
  for (i in 1:length(A)){
    score <- as.numeric(as.character(Additive4[i,5]))
    derived <- as.character(Additive4[i,3])
    if(A[i] == derived){
      A[i] <- score
    } else{
      A[i] <- 0
    }
    if(B[i] == derived){
      B[i] <- score
    } else{
      B[i] <- 0
    }
  }
  
  total[j,1] <- colnames(Inds[j])
  total[j,2] <- sum(as.numeric(A)) + sum(as.numeric(B))
}

AddGERP_4 <- total


### Recessive bins

## 0 < GERP <= 2
Recessive02 <- filter(Additive, GERP > 0 & GERP <= 2)
Inds <- Recessive02[,c(7:54)]
total <- matrix(NA,48,2)

for (j in 1:ncol(Inds)){
  
  A <- as.character(Inds[,j])
  
  for (i in 1:length(A)){
    score <- as.numeric(as.character(Recessive02[i,5]))
    derived <- as.character(Recessive02[i,4])
    if(A[i] == derived){
      A[i] <- score
    } else{
      A[i] <- 0
    }
    
  }
  
  total[j,1] <- colnames(Inds[j])
  total[j,2] <- sum(as.numeric(A))
}

RecGERP02 <- total

### 2 < GERP <= 4

Recessive24 <- filter(Additive, GERP > 2 & GERP <= 4)
Inds <- Recessive24[,c(7:54)]
total <- matrix(NA,48,2)

for (j in 1:ncol(Inds)){
  
  A <- as.character(Inds[,j])
  
  for (i in 1:length(A)){
    score <- as.numeric(as.character(Recessive24[i,5]))
    derived <- as.character(Recessive24[i,4])
    if(A[i] == derived){
      A[i] <- score
    } else{
      A[i] <- 0
    }
    
  }
  
  total[j,1] <- colnames(Inds[j])
  total[j,2] <- sum(as.numeric(A))
}

RecGERP24 <- total

### GERP > 4

Recessive4 <- filter(Additive, GERP > 4)
Inds <- Recessive4[,c(7:54)]
total <- matrix(NA,48,2)

for (j in 1:ncol(Inds)){
  
  A <- as.character(Inds[,j])
  
  for (i in 1:length(A)){
    score <- as.numeric(as.character(Recessive4[i,5]))
    derived <- as.character(Recessive4[i,4])
    if(A[i] == derived){
      A[i] <- score
    } else{
      A[i] <- 0
    }
    
  }
  
  total[j,1] <- colnames(Inds[j])
  total[j,2] <- sum(as.numeric(A))
}

RecGERP4 <- total

#### Now create figure

library(cowplot)
library(ggplot2)
library(tidyr)


AddGERP_02 <- as.data.frame(AddGERP_02)
AddGERP_02$V1 <- as.character(AddGERP_02$V1)
AddGERP_02$V2 <- as.numeric(as.character(AddGERP_02$V2))
AddGERP_02 <- separate(AddGERP_02,V1, c("Pop","Ind"), sep = "_")

Add02 <- ggplot(AddGERP_02, aes(Pop, V2, color = Pop)) + geom_jitter() + 
  theme_bw() + labs(y = "GERP score", x = "Population", title = "0 < GERP <=2") + 
  theme(legend.position = "none") + geom_boxplot(width = 0.15)

AddGERP_24 <- as.data.frame(AddGERP_24)
AddGERP_24$V1 <- as.character(AddGERP_24$V1)
AddGERP_24$V2 <- as.numeric(as.character(AddGERP_24$V2))
AddGERP_24 <- separate(AddGERP_24,V1, c("Pop","Ind"), sep = "_")

Add24 <- ggplot(AddGERP_24, aes(Pop, V2, color = Pop)) + geom_jitter() + theme_bw() + 
  labs(y = "GERP score", x = "Population",title = "2 < GERP <=4") + 
  theme(legend.position = "none") + geom_boxplot(width = 0.15)


AddGERP_4 <- as.data.frame(AddGERP_4)
AddGERP_4$V1 <- as.character(AddGERP_4$V1)
AddGERP_4$V2 <- as.numeric(as.character(AddGERP_4$V2))
AddGERP_4 <- separate(AddGERP_4,V1, c("Pop","Ind"), sep = "_")

Add4 <- ggplot(AddGERP_4, aes(Pop, V2, color = Pop)) + geom_jitter() + theme_bw() + 
  labs(y = "GERP score", x = "Population",title = "GERP > 4") + 
  theme(legend.position = "none") + geom_boxplot(width = 0.15)



AddGERP <- as.data.frame(AddGERP)
AddGERP$V1 <- as.character(AddGERP$V1)
AddGERP$V2 <- as.numeric(as.character(AddGERP$V2))
AddGERP <- separate(AddGERP,V1, c("Pop","Ind"), sep = "_")


Add <- ggplot(AddGERP, aes(Pop, V2, color = Pop)) + geom_jitter() + theme_bw() + 
  labs(y = "GERP score", x = "Population", title = "All GERP > 0") + 
  theme(legend.position = "none") + geom_boxplot(width = 0.15)




plot_grid(Add02,Add24,Add4, Add, labels = "AUTO")


#### ANOVAS

aov.add02 <- aov(V2 ~ Pop, data = AddGERP_02)
TukeyHSD(aov.add02)


aov.add24 <- aov(V2 ~ Pop, data = AddGERP_24)
TukeyHSD(aov.add24)

aov.add4 <- aov(V2 ~ Pop, data = AddGERP_4)
TukeyHSD(aov.add4)

aov.add <- aov(V2 ~ Pop, data = AddGERP)
TukeyHSD(aov.add)

### Recessive GERP figures

RecGERP02 <- as.data.frame(RecGERP02)
RecGERP02$V1 <- as.character(RecGERP02$V1)
RecGERP02$V2 <- as.numeric(as.character(RecGERP02$V2))
RecGERP02 <- separate(RecGERP02,V1, c("Pop","Ind"), sep = "_")

Rec02 <- ggplot(RecGERP02, aes(Pop, V2, color = Pop)) + geom_jitter() + theme_bw() + 
  labs(y = "GERP score", x = "Population",title = "0 < GERP <= 2") + 
  theme(legend.position = "none") + geom_boxplot(width = 0.15)


RecGERP24 <- as.data.frame(RecGERP24)
RecGERP24$V1 <- as.character(RecGERP24$V1)
RecGERP24$V2 <- as.numeric(as.character(RecGERP24$V2))
RecGERP24 <- separate(RecGERP24,V1, c("Pop","Ind"), sep = "_")

Rec24 <- ggplot(RecGERP24, aes(Pop, V2, color = Pop))  + geom_jitter() + 
  theme_bw() + labs(y = "GERP score", x = "Population",title = "2 < GERP <= 4") + 
  theme(legend.position = "none") + geom_boxplot(width = 0.15)


RecGERP4 <- as.data.frame(RecGERP4)
RecGERP4$V1 <- as.character(RecGERP4$V1)
RecGERP4$V2 <- as.numeric(as.character(RecGERP4$V2))
RecGERP4 <- separate(RecGERP4,V1, c("Pop","Ind"), sep = "_")

Rec4 <- ggplot(RecGERP4, aes(Pop, V2, color = Pop)) + geom_jitter() + 
  theme_bw() + labs(y = "GERP score",x = "Population", title = "GERP > 4") + 
  theme(legend.position = "none") + geom_boxplot(width = 0.15)


RecGERP <- as.data.frame(RecGERP)
RecGERP$V1 <- as.character(RecGERP$V1)
RecGERP$V2 <- as.numeric(as.character(RecGERP$V2))
RecGERP <- separate(RecGERP,V1, c("Pop","Ind"), sep = "_")


Rec <- ggplot(RecGERP, aes(Pop, V2, color = Pop)) + geom_jitter() + 
  theme_bw() + labs(y = "GERP score", x = "Population",title = "All GERP > 0") + 
  theme(legend.position = "none") + geom_boxplot(width = 0.15)


plot_grid(Rec02,Rec24,Rec4, Rec, labels = "AUTO")

### ANOVAS

aov.rec02 <- aov(V2 ~ Pop, data = RecGERP02)
TukeyHSD(aov.rec02)


aov.rec24 <- aov(V2 ~ Pop, data = RecGERP24)
TukeyHSD(aov.rec24)

aov.rec4 <- aov(V2 ~ Pop, data = RecGERP4)
TukeyHSD(aov.rec4)

aov.rec <- aov(V2 ~ Pop, data = RecGERP)
TukeyHSD(aov.rec)





#### Count number of SNPs

Recessive4 <- filter(Additive, GERP > 4)
Inds <- Recessive4[,c(7:54)]
total <- matrix(NA,48,2)

for (j in 1:ncol(Inds)){
  
  A <- as.character(Inds[,j])
  
  for (i in 1:length(A)){
    score <- as.numeric(as.character(Recessive4[i,5]))
    derived <- as.character(Recessive4[i,4])
    if(A[i] == derived){
      A[i] <- "score"
    } else{
      A[i] <- 0
    }
    
  }
  print(table(A))
  #total[j,1] <- colnames(Inds[j])
  #total[j,2] <- sum(as.numeric(as.character((A))))
}
total

