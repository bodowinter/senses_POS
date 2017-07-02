## Bodo Winter
## June 18, 2017
## POS by dataset

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load libraries:

library(tidyverse)
library(stringr)

## Set working directory:

setwd('/Users/winterb/Research/senses_sensory_modalities/POS/analysis/data/')

## Load data:

setwd('/Users/winterb/Research/senses_sensory_modalities/POS/analysis/data/')
adj <- read_csv('lynott_connell_2009_adj_norms.csv')
noun <- read_csv('lynott_connell_2013_noun_norms.csv')
verb <- read_csv('winter_2016_verb_norms.csv')
strik <- read_csv('strik_lievers_2015_words_all_POS.csv')
sens <- read_csv('sensicon.csv')

## Re-order noun column to match:

noun <- select(noun,
	Word:VisualStrengthMean, HapticStrengthMean, AuditoryStrengthMean,
	GustatoryStrengthMean:ModalityExclusivity)

## Combine:

allmod <- rbind(select(adj, -PropertyBritish),
	noun,
	select(verb, -RandomSet, -N))
allmod$POS <- c(rep('Adj', nrow(adj)),
	rep('Noun', nrow(noun)),
	rep('Verb', nrow(verb)))

## Exclude instruments from Strik Lievers (2015) dataset:

strik <- filter(strik, Instrument == 'no')

## Exclude adverbs from Sensicon:

sens <- filter(sens, POS != 'r')

## Change labels in Sensicon:

sens <- mutate(sens,
	POS = ifelse(POS == 'a', 'Adj', POS),
	POS = ifelse(POS == 'n', 'Noun', POS),
	POS = ifelse(POS == 'v', 'Verb', POS))




##------------------------------------------------------------------
## Switch in script for only considering high frequency words:
##------------------------------------------------------------------

## Specify this to be true to only get high frequency words:

high_freq <- T

## Load SUBTLEX POS usage data in:

SUBTL <- read_csv('SUBTLEX_US_with_POS.csv')

## Reduce to high frequency:

SUBTL <- filter(SUBTL, FREQcount > median(FREQcount))

## Before count:

nrow(allmod)	# 1,123
nrow(strik)		# 486
nrow(sens)		# 21,540

## Reduce other datasets to high frequency:

if (high_freq) {
	allmod <- filter(allmod, Word %in% SUBTL$Word)
	strik <- filter(strik, Word %in% SUBTL$Word)
	sens <- filter(sens, Word %in% SUBTL$Word)
	}

## Before count:

nrow(allmod)	# 1,047
nrow(strik)		# 372
nrow(sens)		# 16,424



##------------------------------------------------------------------
## Big table with everything in it:
##------------------------------------------------------------------

## Create crosstabulations:

norms.tab <- table(allmod$POS, allmod$DominantModality)
strik.tab <- table(strik$POS, strik$Modality)
sens.tab <- table(sens$POS, sens$Modality)

## Overall lexical differentiation for each of the sensory modalities:

round(colSums(norms.tab) / sum(norms.tab), 2)
round(colSums(strik.tab) / sum(strik.tab), 2)
round(colSums(sens.tab) / sum(sens.tab), 2)

## Write to file:

alltogether <- rbind(norms.tab, strik.tab, sens.tab)
write.table(alltogether, 'all_counts.csv', sep = ',', row.names = F)




##------------------------------------------------------------------
## Look at POS by modality:
##------------------------------------------------------------------

## Norms:

xsums <- allmod %>% group_by(DominantModality, POS) %>%
	count()
xtots <- xsums %>% group_by(DominantModality) %>% summarize(N = sum(n))
norms.sum <- left_join(xsums, xtots) %>% mutate(Prop = round(n / N, 2),
	Percentage = str_c(Prop * 100, '%'))

## Add % per POS:

norms.sum$NPOS <- c(423, 400, 300)
norms.sum <- mutate(norms.sum,
	PercentagePOS = round(n / NPOS, 2))

## Sensicon:

xsums <- sens %>% group_by(Modality, POS) %>%
	count()
xtots <- xsums %>% group_by(Modality) %>% summarize(N = sum(n))
sens.sum <- left_join(xsums, xtots) %>% mutate(Prop = round(n / N, 2),
	Percentage = str_c(Prop * 100, '%'))

## Strik Lievers (2015):

xsums <- strik %>% group_by(Modality, POS) %>% count()
xtots <- xsums %>% group_by(Modality) %>% summarize(N = sum(n))
strik.sum <- left_join(xsums, xtots) %>% mutate(Prop = round(n / N, 2),
	Percentage = str_c(Prop * 100, '%'))

## Check across datasets:

arrange(strik.sum, POS, Prop)
arrange(sens.sum, POS, Prop)
arrange(norms.sum, POS, Prop)

## Perform Chi-Square tests:

norms.tab <- select(norms.sum, DominantModality:n) %>%
	spread(DominantModality, n)
chisq.test(as.matrix(norms.tab[, 2:6]))
chisq.test(as.matrix(norms.tab[, 2:6]), simulate.p.value = T, B = 2000)
(norms.stdres <- round(chisq.test(as.matrix(norms.tab[, 2:6]))$stdres, 1))

sens.tab <- select(sens.sum, Modality:n) %>%
	spread(Modality, n)
chisq.test(as.matrix(sens.tab[, 2:6]))
chisq.test(as.matrix(sens.tab[, 2:6]), simulate.p.value = T, B = 2000)
(sens.stdres <- round(chisq.test(as.matrix(sens.tab[, 2:6]))$stdres, 1))

strik.tab <- select(strik.sum, Modality:n) %>%
	spread(Modality, n)
chisq.test(as.matrix(strik.tab[, 2:6]))
chisq.test(as.matrix(strik.tab[, 2:6]), simulate.p.value = T, B = 2000)
(strik.stdres <- round(chisq.test(as.matrix(strik.tab[, 2:6]))$stdres, 1))

## Average standardized residuals:

my.res <- norms.stdres + sens.stdres + strik.stdres
rownames(my.res) <- c('Adj', 'Noun', 'Verb')
print(my.res <- round((my.res) / 3, 1))


