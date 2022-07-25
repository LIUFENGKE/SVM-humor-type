## setting environment 
setwd("E:/UNDERGRADUATE COURSES/2022 spring semester/MULTIVARIATE STATISTICAL ANALYSIS/presentation/data and codes")
rm(list=ls())
## input packages
library(tidyverse)
library(ggcorrplot)
library(psych)
## input data
humor <- read.csv("data.csv",header=T)
head(humor)
humor.ques <- humor[,1:32]

## correlation
cor <- cor(humor.ques[,1:32])
p.mat <- cor_pmat(cor)
sig <- c(1,2,4,8,10,12,13,14,18,26,31,32)
humor.sign <- humor[,sig]
cor.sig <- cor(humor.sign)
ggcorrplot(cor.sig)
ggcorrplot(abs(cor),hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),p.mat = p.mat) +
  ggtitle("Figure 1 The visualization of correlation matrix of variables")

## scree plot 
pca_obj <- prcomp(humor.ques, scale. = TRUE)
var_explained_df <- data.frame(PC= 1:32,
                               var_explained=(pca_obj$sdev)^2/sum((pca_obj$sdev)^2))

head(var_explained_df)
var_explained_df %>%
  ggplot(aes(x=PC,y=var_explained))+
  geom_point(size=4)+
  geom_line()+
  ggtitle("Figure 2 The scree plot")+
  ylab("Variance") + xlab("Factors")+
  theme_bw()


## Factor analysis 
factor.1  <- factanal(humor.ques, factors = 4)
factor.2 <- factanal(humor.ques, factors = 4,rotation = "varimax")
factor.3 <- factanal(humor.ques, factors = 6,rotation = "varimax")
factor.3

factor.4 <- fa(abs(cor),nfactors = 6,rotate="varimax")
factor.5 <- factanal(factors = 6,rotation = "varimax",covmat = abs(cor(humor.ques[,1:32])))
factor.6 <- factanal(factors = 4,rotation = "varimax",covmat = abs(cor(humor.ques[,1:32])))
factor.7 <- factanal(factors = 6,rotation = "none",covmat = abs(cor(humor.ques[,1:32])))
