library(tidyverse)
library(readxl)
library(kableExtra)
library(meta)

# Dataset
df <- read_excel("thrombolysis_cochrane_2014.xlsx")
df 

# Forest plot
a <- metabin(event.e = d.trom,
             n.e = n.trom,
             event.c = d.cont,
             n.c = n.cont, studlab = paste(Study, Year),
             data = df)       
forest(a, overall = F)   


# Fixed effect meta-analysis
a <- metabin(event.e = d.trom,
             n.e = n.trom,
             event.c = d.cont,
             n.c = n.cont,
             sm = "OR",
             method = "Peto",
             studlab = paste(Study, Year),
             data = df)

summary(a,
        comb.random = FALSE)

forest(a, comb.random = FALSE)   


# Random effect meta-analysis
r <- metabin(event.e = d.trom,
             n.e = n.trom,
             event.c = d.cont,
             n.c = n.cont,
             sm = "OR",
             method = "Peto",
             method.tau = "REML",
             studlab = paste(Study, Year),
             comb.fixed = FALSE,
             comb.random = TRUE,
             data = df)

summary(r)

forest(r, comb.fixed = FALSE)   

forest(r, prediction = TRUE)

## Subgroups
a1 <- update(r, 
             byvar = Timing,
             bylab = "Time",
             comb.fixed = F,
             comb.rand = T)

forest(a1)

summary(a1)

## Meta-regression
mr <- update(r,
              byvar = Timing, tau.common = TRUE, comb.fixed = FALSE)

mr.fit <- metareg(mr)

summary(mr.fit)


## Publication bias: funnel plot
funnel(r, xlab = "Hedges' g")

## Publication bias: test
metabias(r)
