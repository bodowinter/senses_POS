## Bodo Winter
## June 18, 2017
## POS origin analysis (OED):

## Load libraries:

library(tidyverse)
library(stringr)

## Load etymology data:

setwd('/Users/winterb/Research/senses_sensory_modalities/POS/analysis/data/')
etym <- read_csv('all_words_OED_etymology.csv')
lyn <- read_csv('lynott_connell_2009_adj_norms.csv')

## Join etymology data with Lynott & Connell (2009):

etym <- inner_join(etym, lyn)

## Look at counts:

sumtab <- etym %>%
	group_by(DominantModality, OED_OriginPOS) %>%
	count()

## Look at this table:

sumtab %>% print(n = Inf)

## Take only those with clear POS categories:

sumtab <- filter(sumtab,
	OED_OriginPOS %in% c('adj', 'noun', 'verb'))

## Get total counts:

sumtab.N <- sumtab %>% group_by(DominantModality) %>%
	summarize(Total = sum(n))

## Merge:

sumtab <- left_join(sumtab, sumtab.N)

## Rename:

sumtab <- rename(sumtab,
	 OED_POS = OED_OriginPOS,
	 N = n,
	 Modality = DominantModality)

## Get proportions:

sumtab <- mutate(sumtab,
	Prop = round(N / Total, 2),
	Percentage = str_c(Prop * 100, '%'))

## Make a table out of this:

my.M <- matrix(numeric(3 * 5), nrow = 3)
row.names(my.M) <- c('adj', 'noun', 'verb')
colnames(my.M) <- unique(sumtab$Modality)
for (i in 1:nrow(my.M)) {
	for (j in 1:ncol(my.M)) {
		my.M[i, j] <- filter(sumtab, OED_POS == row.names(my.M)[i],
			Modality == colnames(my.M)[j])$N
		}
	}
write.table(my.M, 'summary_OED_counts.csv', sep = ',', row.names = F)
round(chisq.test(my.M)$stdres, 1)

## How much was undecided per category?

OED_present <- sumtab %>% group_by(Modality) %>% summarize(N = sum(N))
lyntab <- lyn %>% group_by(DominantModality) %>% summarize(Total = n())
OED_present <- left_join(OED_present, lyntab, by = c('Modality' = 'DominantModality'))
OED_present <- OED_present %>%
	mutate(Prop = round(N / Total, 2),
	Percentage = str_c(Prop * 100, '%'))

## Write to table:

setwd('/Users/winterb/Research/senses_sensory_modalities/POS/analysis/summary_tables/')
write_csv(sumtab, 'OED_adjective_etymology_summary.csv')



