## Bodo Winter
## June 18, 2017; Updated Dec 4, 2017
## POS by dataset

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load libraries:

library(png)
library(stringr)
library(tidyverse)

## Set working directory:

setwd('/Users/winterb/Research/senses_sensory_modalities/POS/analysis/data/')

## Load data:

setwd('/Users/winterb/Research/senses_sensory_modalities/POS/analysis/data/')
adj <- read_csv('lynott_connell_2009_adj_norms.csv')
noun <- read_csv('lynott_connell_2013_noun_norms.csv')
verb <- read_csv('winter_2016_verb_norms.csv')
strik <- read_csv('strik_lievers_2015.csv')
sens <- read_csv('sensicon.csv')

## SUBTLEX POS tags and frequencies:

SUBTL <- read_csv('SUBTLEX_US.csv')

## Load in hand classifications by Francesca:
## For modality norms only (infeasible for 20,000 Sensicon words):

francesca <- read_csv('francesca_perceptual.csv')

## Re-order noun column to match:

noun <- select(noun,
	Word:VisualStrengthMean, HapticStrengthMean, AuditoryStrengthMean,
	GustatoryStrengthMean:ModalityExclusivity)

## Combine:

allmod <- rbind(select(adj, -PropertyBritish),
	noun, select(verb, -RandomSet, -N))
# (We're getting rid of the sampling method info
# from Winter, 2016a — because we look at the full sample)

## Add POS tags as they are based on which norming study they come from:
# Lynott & Connell (2009): adjectives
# Lynott & Connell (2013): nouns
# Winter (2016a): verbs

allmod$POS <- c(rep('Adj', nrow(adj)),
	rep('Noun', nrow(noun)),
	rep('Verb', nrow(verb)))

## Exclude instruments from Strik Lievers (2015) dataset:

strik <- filter(strik, Instrument == 'no')

## Exclude adverbs from Sensicon:

sens <- filter(sens, POS != 'r')
# (we are only looking at adjectives, nouns and verbs
# and have no specific hypotheses for adverbs which are
# also often not particularly sensory in nature)

## Change labels in Sensicon to more transparent labels:
# (consistent with other datasets)

sens <- mutate(sens,
	POS = ifelse(POS == 'a', 'Adj', POS),
	POS = ifelse(POS == 'n', 'Noun', POS),
	POS = ifelse(POS == 'v', 'Verb', POS))


##------------------------------------------------------------------
## Check overall datasets statistics:
##------------------------------------------------------------------

## Check number of words:

nrow(allmod)	# 1,123
nrow(strik)		# 486
nrow(sens)		# 21,540

## Overlap to Sensicon:

sum(strik$Word %in% sens$Word) / nrow(strik)		# 73%
sum(allmod$Word %in% sens$Word) / nrow(allmod)	# 86%

## Overlap between Strik Lievers (2015) dataset and norm dataset:

sum(strik$Word %in% allmod$Word) / nrow(strik)	# 42%
sum(allmod$Word %in% strik$Word) / nrow(allmod)	# 13%

## What are the words that overlap?

strik$Word[strik$Word %in% sens$Word]
allmod$Word[allmod$Word %in% sens$Word]

strik$Word[strik$Word %in% allmod$Word]
allmod$Word[allmod$Word %in% strik$Word]

## Comment: There are definitely some overlapping words that
## pertain to our hypotheses, such as 'squeak', 'patter', 'thunder'
## This is unavoidable and in the nature of the lexicon
## What matters is that the three lists were constructed independently
## ... and that nonetheless similar words were included

## What are the words that don't overlap?

strik$Word[strik$Word %in% sens$Word]
allmod$Word[allmod$Word %in% sens$Word]

strik$Word[strik$Word %in% allmod$Word]
allmod$Word[allmod$Word %in% strik$Word]

## There are also relevaent non-overlapping words, such as 'screech'
## 'rattle' and 'howl'; so the overlap between the different datasets
## is far from complete when it comes to relevant words.



##------------------------------------------------------------------
## Put SUBTLEX in there:
##------------------------------------------------------------------

## Rename:

SUBTL <- rename(SUBTL,
	POS = Dom_PoS_SUBTLEX)

## Merge:

allmod$SUBTL_POS <- SUBTL[match(allmod$Word, SUBTL$Word), ]$POS
strik$SUBTL_POS <- SUBTL[match(strik$Word, SUBTL$Word), ]$POS
sens$SUBTL_POS <- SUBTL[match(sens$Word, SUBTL$Word), ]$POS
matches <- match(allmod$Word, SUBTL$Word)
allmod$SUBTL_POS_prop <- as.numeric(SUBTL[matches, ]$Percentage_dom_PoS)
matches <- match(strik$Word, SUBTL$Word)
strik$SUBTL_POS_prop <- as.numeric(SUBTL[matches, ]$Percentage_dom_PoS)
matches <- match(sens$Word, SUBTL$Word)
sens$SUBTL_POS_prop <- as.numeric(SUBTL[matches, ]$Percentage_dom_PoS)



##------------------------------------------------------------------
## Analysis 'switches':
##------------------------------------------------------------------

## What is reported in paper:
# no_questionable <- T
# high_freq <- F
# unimodal <- T
# perceptual <- F

## Specify this to be true to only get non-questionable ones:

no_questionable <- F

## Specify this to be true to exclude low frequency words:

high_freq <- F

## Specify this to be true to only get highly unimodal words:

unimodal <- F

## Switch for only getting the highly perceptual ones:

perceptual <- F

## The average standardized residual for the sound/verb cell
## (across all datasets) is > +2 for 14/16 combinations of
## analysis switches, which is 87.5% of all cases.
## It is 1.8 for T-T-F-T (unimodal = F) and
## F-T-F-T (questionables not excluded)
## The standardized residual for the sound/verb cell is never
## negative even for the individual datasets under all analysis switches.


##------------------------------------------------------------------
## Switch (1): Excluding those words coded as questionable:
##------------------------------------------------------------------

## Merge questions in there:

matches <- match(allmod$Word, francesca$Word)
allmod$Perceptual <- francesca[matches, ]$`perceptual?`

## Only take those that have NA for the "Perceptual" column (i.e., no "?"):

if (no_questionable) {
	allmod <- filter(allmod, is.na(Perceptual))
	}

## Also add POS information:

allmod$FrancescaPOS <- francesca[matches, ]$originPOS



##------------------------------------------------------------------
## Switch (2): only considering high frequency words:
##------------------------------------------------------------------

## Reduce to high frequency:

SUBTL_HF <- filter(SUBTL, FREQcount > median(FREQcount))

## Before count:

nrow(allmod)	# 1,123
nrow(strik)		# 486
nrow(sens)		# 21,540

## Reduce other datasets to high frequency:

if (high_freq) {
	allmod <- filter(allmod, Word %in% SUBTL_HF$Word)
	strik <- filter(strik, Word %in% SUBTL_HF$Word)
	sens <- filter(sens, Word %in% SUBTL_HF$Word)
	}


##------------------------------------------------------------------
## Siwtch (3): only consider unimodal words:
##------------------------------------------------------------------

## Before count:

nrow(allmod)	# 1,123
nrow(sens)	# 21,540

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

## Reduce to 70% most unimodal:

if (unimodal) {
	allmod <- filter(allmod,
		ModalityExclusivity > quantile(ModalityExclusivity, 0.7))
	sens <- filter(sens,
		ModalityExclusivity > quantile(ModalityExclusivity, 0.7))
	}

## New count:

nrow(allmod)	# 561 or 225 if 80%
nrow(sens)	# 4,308


##------------------------------------------------------------------
## Switch (4): Check perceptual words based on sum:
##------------------------------------------------------------------

## Perceptual strength sum:

allmod$PerceptualSum <- rowSums(select(allmod,
	VisualStrengthMean:OlfactoryStrengthMean))

## Get only the really perceptual ones based on 70th cut-off:

if (perceptual) {
	allmod <- filter(allmod,
		PerceptualSum > quantile(PerceptualSum, 0.7))
	sens <- filter(sens,
		PerceptualSum > quantile(PerceptualSum, 0.7))
	}


##------------------------------------------------------------------
## For all datasets incorporate POS tags based on SUBTLEX:
##------------------------------------------------------------------

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

## Get rid of dublets (words can no have only one POS):

strik <- filter(strik, !duplicated(Word))
sens <- filter(sens, !duplicated(Word))
allmod <- filter(allmod, !duplicated(Word))



##------------------------------------------------------------------
## MAIN ANALYSIS: Chi-square tests and standardized residuals:
##------------------------------------------------------------------

## Create crosstabulations:

norms.tab <- table(allmod$SUBTL_POS, allmod$DominantModality)
strik.tab <- table(strik$SUBTL_POS, strik$Modality)
sens.tab <- table(sens$SUBTL_POS, sens$Modality)

## Perform Chi-Square tests:

(norms.chisq <- chisq.test(norms.tab, simulate.p.value = T))
(strik.chisq <- chisq.test(strik.tab, simulate.p.value = T))
(sens.chisq <- chisq.test(sens.tab, simulate.p.value = T))
# (works also with non-simulated p-values)

## Look at standardized residuals:

norms.chisq$stdres
strik.chisq$stdres
sens.chisq$stdres

## Additional analysis without Lynott and Connell (2013) dataset for norms:

with(filter(allmod, POS != 'Noun'),
	norms.tab.nonouns <<- table(SUBTL_POS, DominantModality))
chisq.test(norms.tab.nonouns)$stdres

## Overall lexical differentiation for each of the sensory modalities:

round(colSums(norms.tab) / sum(norms.tab), 2)
round(colSums(strik.tab) / sum(strik.tab), 2)
round(colSums(sens.tab) / sum(sens.tab), 2)

## Write to file:

alltogether <- rbind(norms.tab, strik.tab, sens.tab)

## Look at averages across residuals:

sdtres.avg <- (norms.chisq$stdres +
	strik.chisq$stdres +
	sens.chisq$stdres) / 3


##------------------------------------------------------------------
## Additional sanity check:
##------------------------------------------------------------------

## Sanity-check, Francesca's morphology-based POS classification:
## (not the corpus-based SUBTLEX classification)

allmod$POS2 <- allmod$POS	# copy of Lynott & Connel POS
allmod[!is.na(allmod$FrancescaPOS), ]$POS2 <- allmod[!is.na(allmod$FrancescaPOS), ]$FrancescaPOS
allmod_franc <- filter(allmod,
	!POS2 %in% c('?', 'Verb?'))
with(allmod_franc,
	allmod_franc.tab <<- table(POS2, DominantModality))
chisq.test(allmod_franc.tab)$stdres

## Sanity-check: What if for the norms and Strik Lievers (2015),
## we use the non SUBTLEX-based POS classifications?





##------------------------------------------------------------------
## Make a graph of the Pearson residuals:
##------------------------------------------------------------------

## Load in images:

setwd('/Users/winterb/Research/senses_sensory_modalities/POS/analysis/figures/')
s1 <- readPNG('taste.png')
s2 <- readPNG('smell.png')
s3 <- readPNG('touch.png')
s4 <- readPNG('sight.png')
s5 <- readPNG('sound.png')

sight.png

## Define vector of names:

sense_names <- str_c('s', 1:5)

## Define colors:

mycols <- c('#425fac', '#30b77d', '#f79038', '#f37058', '#efbe1b')

## Resorted Pearson residuals for plotting:

norms.res <- t(norms.chisq$stdres)
strik.res <- t(strik.chisq$stdres)
sens.res <- t(sens.chisq$stdres)

norms.res <- norms.res[c('Gustatory', 'Olfactory', 'Haptic', 'Visual', 'Auditory'), ]
strik.res <- strik.res[c('Gustatory', 'Olfactory', 'Haptic', 'Visual', 'Auditory'), ]
sens.res <- sens.res[c('Gustatory', 'Olfactory', 'Haptic', 'Visual', 'Auditory'), ]

norms.res <- norms.res[, c('Noun', 'Adjective', 'Verb')]
strik.res <- strik.res[, c('Noun', 'Adjective', 'Verb')]
sens.res <- sens.res[, c('Noun', 'Adjective', 'Verb')]

xdata <- sens.res

## Plotting parameters:

x_fac <- 0.05
y_fac <- 0.58

## Make a plot:

quartz('', 11, 5)
par(mai = c(0.75, 1.75, 0.75, 0.5))
plot(1, 1,
	type = 'n',
	xlim = c(0.5, 3 * 5 + 4 + 0.5),
	ylim = c(-8, +8),
	xaxt = 'n', yaxt = 'n',
	xlab = '', ylab = '',
	bty = 'n')
mtext('Standardized Residual', side = 2, font = 2, line = 4.25, cex = 2)
axis(side = 2,
	at = seq(-8, 8, 2),
	font = 2, cex.axis = 1.25, las = 2,
	lwd = 2, lwd.ticks = 2)
for (i in 1:nrow(xdata)) {
	this_col <- mycols[i]
	
	for (j in 1:ncol(xdata)) {
	rect(xleft = -3 + i * 4 + j - 1 + x_fac,
		xright = -3 + i * 4 + j - x_fac,
		ybottom = 0, ytop = xdata[i, j], col = this_col)
	if (j == 3) {
		rect(xleft = -3 + i * 4 + j - 1 + x_fac,
			xright = -3 + i * 4 + j - x_fac,
			ybottom = 0, ytop = xdata[i, j], coll = this_col,
			border = 'black',
			density = 20)	
			}
		}
	}

for (i in 1:nrow(xdata)) {
	for (j in 1:ncol(xdata)) {	
		if (xdata[i, j] > 0) {
			text(x = -3 + i * 4 + j - 0.5,
				y = xdata[i, j] + y_fac,
				labels = c('N', 'A', 'V')[j],
				cex = 1.25, font = 2)
			}
		if (xdata[i, j] < 0) {
			text(x = -3 + i * 4 + j - 0.5,
				y = xdata[i, j] - y_fac,
				labels = c('N', 'A', 'V')[j],
				cex = 1.35, font = 2)
			}
		}
	}
	
for (i in seq_along(sense_names)) {
	this_img <- get(sense_names[i])
	rasterImage(this_img,
		xleft = -1.5 + i * 4 - 0.7,
		xright = -1.5 + i * 4 + 0.7,
		ybottom = -11, ytop = -8,
		xpd = NA)
	}
abline(h = 0, lwd = 2)
abline(h = c(-2, 2), lty = 2)




##------------------------------------------------------------------
## Look at POS by modality:
##------------------------------------------------------------------

## Load in the data:

s1 <- readPNG('taste_new.png')
s2 <- readPNG('smell_new.png')
s3 <- readPNG('touch_new.png')
s4 <- readPNG('sight_new.png')
s5 <- readPNG('sound_new.png')

## Define colors:

mycols <- gray.colors(5, start = 0.3, end = 0.9)
	## MAYBE MAKE THEM WITHIN THE MODALITY DIFFERENT

## Plotting parameters:

x_fac <- 0.15
y_fac <- 0.58

xdata <- sens.res

## Make a plot:

quartz('', 11, 5)
par(mai = c(0.75, 1.75, 0.75, 0.5))
plot(1, 1,
	type = 'n',
	xlim = c(0.5, 3 * 5 + 4 + 0.5),
	ylim = c(-8, +8),
	xaxt = 'n', yaxt = 'n',
	xlab = '', ylab = '',
	bty = 'n')
mtext('Standardized Residual', side = 2, font = 2, line = 4.25, cex = 2)
axis(side = 2,
	at = seq(-8, 8, 2),
	font = 2, cex.axis = 1.25, las = 2,
	lwd = 2, lwd.ticks = 2)
for (i in 1:nrow(xdata)) {
	this_col <- mycols[i]
	
	rect(xleft = -2.8 + i * 4 + 1 - 1 + x_fac,
		xright = -2.8 + i * 4 + 1 - x_fac,
		ybottom = 0, ytop = xdata[i, 1], col = this_col)
	rect(xleft = -3 + i * 4 + 2 - 1 + x_fac,
		xright = -3 + i * 4 + 2 - x_fac,
		ybottom = 0, ytop = xdata[i, 2], col = this_col)
	rect(xleft = -3.2 + i * 4 + 3 - 1 + x_fac,
		xright = -3.2 + i * 4 + 3 - x_fac,
		ybottom = 0, ytop = xdata[i, 3], col = this_col)
	rect(xleft = -3.2 + i * 4 + 3 - 1 + x_fac,
		xright = -3.2 + i * 4 + 3 - x_fac,
		ybottom = 0, ytop = xdata[i, 3], coll = this_col,
		border = 'black',
		density = 20)	
			}
		}
	}

for (i in 1:nrow(xdata)) {
	for (j in 1:ncol(xdata)) {	
		if(j == 1) {
			extra_fac <- +0.2
			}
		if(j == 2) {
			extra_fac <- 0
			}
		if(j == 3) {
			extra_fac <- -0.2
			}		
		if (xdata[i, j] > 0) {
			text(x = -3 + i * 4 + j - 0.5 + extra_fac,
				y = xdata[i, j] + y_fac,
				labels = c('N', 'A', 'V')[j],
				cex = 1.25, font = 2)
			}
		if (xdata[i, j] < 0) {
			text(x = -3 + i * 4 + j - 0.5 + extra_fac,
				y = xdata[i, j] - y_fac,
				labels = c('N', 'A', 'V')[j],
				cex = 1.35, font = 2)
			}
		}
	}
	
for (i in seq_along(sense_names)) {
	this_img <- get(sense_names[i])
	rasterImage(this_img,
		xleft = -1.5 + i * 4 - 0.7,
		xright = -1.5 + i * 4 + 0.7,
		ybottom = -11, ytop = -8,
		xpd = NA)
	}
abline(h = 0, lwd = 2)
abline(h = c(-2, 2), lty = 2)



