## Bodo Winter
## June 18, 2017
## Wisconsin attribute norm results

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Packages:

library(tidyverse)
library(stringr)

## Load in data:

setwd('/Users/winterb/Data/')
wisc <- read_csv('wisconsin_norms.csv')

## Check few representative values:

arrange(wisc, desc(SoundMean))
arrange(wisc, SoundMean)
arrange(wisc, desc(ColorMean))
arrange(wisc, ColorMean)
arrange(wisc, desc(MotionMean))
arrange(wisc, MotionMean)

## Correlate:

with(wisc, cor.test(SoundMean, MotionMean))
with(wisc, cor.test(ColorMean, MotionMean))

## Get Color and Sound mean in long format:

soundcolor <- wisc %>% select(Word, SoundMean, ColorMean) %>%
	gather(Modality, PerceptualStrength, -Word) %>%
	mutate(Modality = str_replace(Modality, 'Mean', ''))
motion <- wisc %>% select(Word, MotionMean)	%>% rename(Motion = MotionMean)
wisc <- left_join(soundcolor, motion)
# wisc <- wisc %>% mutate(PerceptualStrength = PerceptualStrength - mean(PerceptualStrength))



##------------------------------------------------------------------
## Model this:
##------------------------------------------------------------------

## Make a model of this:

summary(xmdl <- lm(Motion ~ PerceptualStrength * Modality, data = wisc))

## R-squared comparison:

summary(xmdl.sound <- lm(Motion ~ PerceptualStrength,
	filter(wisc, Modality == 'Sound')))
summary(xmdl.color <- lm(Motion ~ PerceptualStrength,
	filter(wisc, Modality == 'Color')))



##------------------------------------------------------------------
## Make a plot of this:
##------------------------------------------------------------------

## Make empty data frame with predictions:

xdata <- tibble(PerceptualStrength = seq(0, 7, 0.01))

## Get predictions, sound:

xdata <- bind_cols(xdata,
	as.tibble(predict(xmdl.sound, newdata = xdata, se.fit = T)[1:2])) %>%
	rename(SoundFit = fit, SoundSE = se.fit)

## Get predictions, color:

xdata <- bind_cols(xdata,
	as.tibble(predict(xmdl.color, newdata = xdata, se.fit = T)[1:2])) %>%
	rename(ColorFit = fit, ColorSE = se.fit)

## Make a plot side to side:

quartz('', 13, 5)
par(mfrow = c(1,2 ), mai = rep(0.25, 4), omi = c(1, 1, 0.25, 0.25))
plot(1, 1, type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
	xlim = c(0, 6), ylim = c(0, 6))
axis(side = 2, at = 0:6, lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.5)
axis(side = 1, at = 0:6, lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.5)
mtext(side = 1, text = 'Sound Rating', line = 3.5, font = 2, cex = 2)
mtext(side = 2, text = 'Motion Rating', line = 3.8, font = 2, cex = 2)
polygon(x = c(xdata$PerceptualStrength, rev(xdata$PerceptualStrength)),
	y = c(xdata$SoundFit + 1.96 * xdata$SoundSE,
		rev(xdata$SoundFit - 1.96 * xdata$SoundSE)), border = NA, col = rgb(0, 0, 0, 0.2))
abline(xmdl.sound, lwd = 4)
with(filter(wisc, Modality == 'Sound'),
	points(PerceptualStrength, Motion, pch = 19, col = rgb(0, 0, 0, 0.4)))
box(lwd = 2)
plot(1, 1, type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
	xlim = c(0, 6), ylim = c(0, 6))
axis(side = 1, at = 0:6, lwd = 2, lwd.ticks = 2, font = 2, cex.axis = 1.5)
mtext(side = 1, text = 'Color Rating', line = 3.5, font = 2, cex = 2)
polygon(x = c(xdata$PerceptualStrength, rev(xdata$PerceptualStrength)),
	y = c(xdata$ColorFit + 1.96 * xdata$ColorSE,
		rev(xdata$ColorFit - 1.96 * xdata$ColorSE)), border = NA, col = rgb(0, 0, 0, 0.2))
abline(xmdl.color, lwd = 4)
with(filter(wisc, Modality == 'Color'),
	points(PerceptualStrength, Motion, pch = 19, col = rgb(0, 0, 0, 0.4)))
box(lwd = 2)

# ## Make a plot of this:

# wisc %>% ggplot(aes(x = SoundMean, y = MotionMean)) + geom_point() + geom_smooth(method = 'lm')
# wisc %>% ggplot(aes(x = ColorMean, y = MotionMean)) + geom_point() + geom_smooth(method = 'lm')

