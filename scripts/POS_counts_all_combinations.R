## Bodo Winter
## Dec 4, 2017
## POS by dataset, batch processing

## Explanation:
## This script performs the same as "POS_counts.R"
## But in batch; outcome is a list object
## With all the standardized residuals for all combinations
## of the "analysis switches" described in the paper
## No graphs are created here

## For more detailed explanations on each step,
## check the "POS_counts.R" file

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load libraries:

library(stringr)
library(tidyverse)

## Set working directory:

mainPath <- '/Users/winterb/Research/senses_sensory_modalities/POS/analysis/data'
setwd(mainPath)
adj <- read_csv('lynott_connell_2009_adj_norms.csv')
noun <- read_csv('lynott_connell_2013_noun_norms.csv')
verb <- read_csv('winter_2016_verb_norms.csv')
strik <- read_csv('strik_lievers_2015.csv')
sens <- read_csv('sensicon.csv')
SUBTL <- read_csv('SUBTLEX_US.csv')
francesca <- read_csv('francesca_perceptual.csv')

## Re-order to match and combine:

noun <- select(noun,
	Word:VisualStrengthMean, HapticStrengthMean, AuditoryStrengthMean,
	GustatoryStrengthMean:ModalityExclusivity)
allmod <- rbind(select(adj, -PropertyBritish),
	noun, select(verb, -RandomSet, -N))

## Exclusions (see other "POS_counts.R"):

strik <- filter(strik, Instrument == 'no')
sens <- filter(sens, POS != 'r')

## Change labels in Sensicon to more transparent labels:

sens <- mutate(sens,
	POS = ifelse(POS == 'a', 'Adj', POS),
	POS = ifelse(POS == 'n', 'Noun', POS),
	POS = ifelse(POS == 'v', 'Verb', POS))

## Merge with SUBTLEX:

SUBTL <- rename(SUBTL,
	POS = Dom_PoS_SUBTLEX)
allmod$SUBTL_POS <- SUBTL[match(allmod$Word, SUBTL$Word), ]$POS
strik$SUBTL_POS <- SUBTL[match(strik$Word, SUBTL$Word), ]$POS
sens$SUBTL_POS <- SUBTL[match(sens$Word, SUBTL$Word), ]$POS
matches <- match(allmod$Word, SUBTL$Word)
allmod$SUBTL_POS_prop <- as.numeric(SUBTL[matches, ]$Percentage_dom_PoS)
matches <- match(strik$Word, SUBTL$Word)
strik$SUBTL_POS_prop <- as.numeric(SUBTL[matches, ]$Percentage_dom_PoS)
matches <- match(sens$Word, SUBTL$Word)
sens$SUBTL_POS_prop <- as.numeric(SUBTL[matches, ]$Percentage_dom_PoS)

## Merge questions in there:

allmod$Perceptual <- francesca[match(allmod$Word, francesca$Word), ]$`perceptual?`

## Also add POS information:

allmod$FrancescaPOS <- francesca[match(allmod$Word, francesca$Word), ]$originPOS

## SUBTLEX high frequency sub corpus:

SUBTL_HF <- filter(SUBTL, FREQcount > median(FREQcount))
highfreqs <- SUBTL_HF$Word

## Calculate modality exclusivity for sensicon:

all_mins <- apply(select(sens, VisualStrength:TactileStrength),
	1, FUN = function(x) min(x))
min_min <- abs(min(all_mins))
all_diffs <- apply(select(sens, VisualStrength:TactileStrength),
	1, FUN = function(x) diff(range(x + min_min)))
all_sums <- apply(select(sens, VisualStrength:TactileStrength),
	1, FUN = function(x) sum(x + min_min))
sens$PerceptualSum <- all_sums
sens$ModalityExclusivity <- all_diffs / all_sums

## Exclude those that don't belong to the three major categories:

allmod <- filter(allmod,
	SUBTL_POS %in% c('Adjective', 'Noun', 'Verb'),
	SUBTL_POS_prop > 0.7)
strik <- filter(strik,
	SUBTL_POS %in% c('Adjective', 'Noun', 'Verb'),
	SUBTL_POS_prop > 0.7)
sens <- filter(sens,
	SUBTL_POS %in% c('Adjective', 'Noun', 'Verb'),
	SUBTL_POS_prop > 0.7)

## Perceptual strength sum:

allmod$PerceptualSum <- rowSums(select(allmod,
	VisualStrengthMean:OlfactoryStrengthMean))

## Get rid of dublets (words can no have only one POS):

strik <- filter(strik, !duplicated(Word))
sens <- filter(sens, !duplicated(Word))
allmod <- filter(allmod, !duplicated(Word))



##------------------------------------------------------------------
## Table of analysis 'switches':
##------------------------------------------------------------------

## Vectors of all combinations:

questionable <- c(rep(TRUE, 8), rep(FALSE, 8))
freq <- rep(c(rep(TRUE, 4), rep(FALSE, 4)), 2)
unimodal <- rep(c(TRUE, TRUE, FALSE, FALSE), 4)
perceptual <- rep(c(TRUE, FALSE), 8)

## Put this into a tibble:

switches <- tibble(questionable, freq, unimodal, perceptual)

## Append empty dataframes for the sound-verb and touch-verb columns:
# (to be filled with standardized residuals for that cell)

switches$SoundVerbAvg <- NA
switches$SoundVerbLC <- NA
switches$SoundVerbStrik <- NA
switches$SoundVerbSens <- NA
switches$TouchVerbAvg <- NA
switches$TouchVerbLC <- NA
switches$TouchVerbStrik <- NA
switches$TouchVerbSens <- NA

## To save the number of data points:

switches$LC_N <- NA
switches$Strik_N <- NA
switches$Sens_N <- NA

## Percent excluded:

switches$LC_Excluded <- NA
switches$Strik_Excluded <- NA
switches$Sens_Excluded <- NA

## Get number of data points to compute exclusions:

mod.N <- nrow(allmod)
strik.N <- nrow(strik)
sens.N <- nrow(sens)

## One for the folder names:

switches$FolderName <- NA

## Define function that performs addition and ignores NA's:
# https://stackoverflow.com/questions/13106645/using-in-data-table-to-sum-the-values-of-two-columns-in-r-ignoring-nas

`%+na%` <- function(x, y) {
	ifelse(is.na(x), y, ifelse(is.na(y), x, x+y))}



##------------------------------------------------------------------
## Create folder structure:
##------------------------------------------------------------------

## Main folder where all the individual results are put in:

if (!dir.exists('individual_runs')) dir.create('individual_runs')
batch_folder <- file.path(mainPath, 'individual_runs')
setwd(batch_folder)

## Extract switch names:

mynames <- colnames(switches)[1:4]

## Create all folders:

for (i in 1:nrow(switches)) {
	folder_name <- str_c(mynames[1], '_', switches[i, 1])
	folder_name <- str_c(folder_name, ':', mynames[2], '_', switches[i, 2])
	folder_name <- str_c(folder_name, ':', mynames[3], '_', switches[i, 3])
	folder_name <- str_c(folder_name, ':', mynames[4], '_', switches[i, 4])
	if (!dir.exists(folder_name)) dir.create(folder_name)
	switches[i, ]$FolderName <- folder_name
	}



##------------------------------------------------------------------
## The main loop:
##------------------------------------------------------------------

## These modalities:

modalities <- c('Auditory', 'Gustatory', 'Haptic', 'Olfactory', 'Visual')

## Loop through table and perform exclusions and then store table and results:

for (i in 1:nrow(switches)) {
	## Extract switches for this iteration / row:
	
	questionable <- switches$questionable[i]
	freq <- switches$freq[i]
	unimodal <- switches$unimodal[i]
	perceptual <- switches$perceptual[i]
	
	## Perform switches (the respective filters):
	
	allmod_red <- allmod
	strik_red <- strik
	sens_red <- sens
	
	if (questionable) {
		allmod_red <- filter(allmod_red, is.na(Perceptual))
		}

	if (freq) {
		allmod_red <- filter(allmod_red, Word %in% highfreqs)
		strik_red <- filter(strik_red, Word %in% highfreqs)
		sens_red <- filter(sens_red, Word %in% highfreqs)
		}
	
	if (unimodal) {
		allmod_red <- filter(allmod_red,
			ModalityExclusivity > quantile(ModalityExclusivity, 0.7))
		sens_red <- filter(sens_red,
			ModalityExclusivity > quantile(ModalityExclusivity, 0.7))
		}

	if (perceptual) {
		allmod_red <- filter(allmod_red,
			PerceptualSum > quantile(PerceptualSum, 0.7))
		sens_red <- filter(sens_red,
			PerceptualSum > quantile(PerceptualSum, 0.7))
		}

	## Create tables:

	norms.tab <- table(allmod_red$SUBTL_POS, allmod_red$DominantModality)
	strik.tab <- table(strik_red$SUBTL_POS, strik_red$Modality)
	sens.tab <- table(sens_red$SUBTL_POS, sens_red$Modality)
	
	## Add N to table:
	
	switches[i, 'LC_N'] <- sum(norms.tab)
	switches[i, 'Strik_N'] <- sum(strik.tab)
	switches[i, 'Sens_N'] <- sum(sens.tab)

	## Add exclusions to table:
	
	switches[i, 'LC_Excluded'] <- 1 - (sum(norms.tab) / mod.N)
	switches[i, 'Strik_Excluded'] <- 1 - (sum(strik.tab) / strik.N)
	switches[i, 'Sens_Excluded'] <- 1 - (sum(sens.tab) / sens.N)
	
	## Make into data frame matrix:
	
	norms.tab <- as.data.frame.matrix(norms.tab)
	strik.tab <- as.data.frame.matrix(strik.tab)
	sens.tab <- as.data.frame.matrix(sens.tab)
	
	## Get standardized residuals:
	
	suppressWarnings(norms.stdres <- chisq.test(norms.tab)$stdres)
	suppressWarnings(strik.stdres <- chisq.test(strik.tab)$stdres)
	suppressWarnings(sens.stdres <- chisq.test(sens.tab)$stdres)

	## If modality drops out, add empty NA column for tables to be of conform size:
	
	if (ncol(norms.stdres) < 5) {
		missing_mod <- setdiff(modalities, colnames(norms.stdres))
		norms.stdres <- cbind(norms.stdres, rep(NA, 3))
		colnames(norms.stdres)[colnames(norms.stdres) == ''] <- missing_mod
		norms.stdres <- norms.stdres[, modalities]
		}
	if (ncol(strik.stdres) < 5) {
		missing_mod <- setdiff(modalities, colnames(strik.stdres))
		strik.stdres <- cbind(strik.stdres, rep(NA, 3))
		colnames(strik.stdres)[colnames(strik.stdres) == ''] <- missing_mod
		strik.stdres <- strik.stdres[, modalities]
		}	
	if (ncol(sens.stdres) < 5) {
		missing_mod <- setdiff(modalities, colnames(sens.stdres))
		sens.stdres <- cbind(sens.stdres, rep(NA, 3))
		colnames(sens.stdres)[colnames(sens.stdres) == ''] <- missing_mod
		sens.stdres <- sens.stdres[, modalities]
		}

	## Write these tables to files:
	
	setwd(file.path(batch_folder, switches$FolderName[i]))
	write_csv(as.data.frame(norms.stdres), 'norms.csv')
	write_csv(as.data.frame(strik.stdres), 'strik.csv')
	write_csv(as.data.frame(sens.stdres), 'sens.csv')
	
	## Get the average:
	
	xres <- (norms.stdres %+na% strik.stdres %+na% sens.stdres) / 3
	
	## Save the most important residuals:
		
	write_csv(as.data.frame(xres), 'alltogether.csv')

	## Save the data:
	
	switches[i, ]$SoundVerbAvg <- round(xres['Verb', 'Auditory'], 2)
	switches[i, ]$TouchVerbAvg <- round(xres['Verb', 'Haptic'], 2)

	switches[i, ]$SoundVerbLC <- round(norms.stdres['Verb', 'Auditory'], 2)
	switches[i, ]$TouchVerbLC <- round(norms.stdres['Verb', 'Haptic'], 2)

	switches[i, ]$SoundVerbStrik <- round(strik.stdres['Verb', 'Auditory'], 2)
	switches[i, ]$TouchVerbStrik <- round(strik.stdres['Verb', 'Haptic'], 2)

	switches[i, ]$SoundVerbSens <- round(sens.stdres['Verb', 'Auditory'], 2)
	switches[i, ]$TouchVerbSens <- round(sens.stdres['Verb', 'Haptic'], 2)

	}


##------------------------------------------------------------------
## Export the main results table:
##------------------------------------------------------------------

setwd(mainPath)
write_csv(switches, 'allsummary.csv')




