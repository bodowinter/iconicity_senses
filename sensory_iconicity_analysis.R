## Bodo Winter
## September 17, 2015
## Analysis for Ch. 6.4, 'Testing the iconicity of sensory words'

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Set options:

options(stringsAsFactors = F)

## Load in packages:

library(dplyr)
library(xlsx)
library(lsr)			# for cohensD function
library(effsize)		# for cohens.d function
library(car)		# for vif function

## Define path to parent directory:

mainPath <- '/Users/winterb/Research/senses_sensory_modalities/iconicity/analysis/'

## Load in plotting and model prediction functions:

source(file.path(mainPath, 'functions/plotting_functions.R'))
source(file.path(mainPath, 'functions/model_prediction_functions.R'))

## Load in modality norms:

setwd(file.path(mainPath, 'data'))
l <- read.csv('lynott_connell_2009_adj_norms.csv')
n <- read.csv('lynott_connell_2013_noun_norms.csv')
v <- read.csv('winter_2016_verb_norms.csv')

## Order factors for plotting:

modalities <- c('Visual', 'Haptic', 'Auditory', 'Gustatory', 'Olfactory')
l$DominantModality <- factor(l$DominantModality, levels = modalities)
n$DominantModality <- factor(n$DominantModality, levels = modalities)
v$DominantModality <- factor(v$DominantModality, levels = modalities)

## Reduce verbs to random SUBTLset:

v <- filter(v, RandomSet == 'yes')

## Load in iconicity data and aggregate over that one duplicated value:

icon <- read.csv('iconicity_ratings.csv')
icon <- aggregate(Iconicity ~ Word, icon, mean)

## Load in sensory experience ratings and SUBTLTLEX POS data:

SER <- read.csv('juhasz_yap_2013_SER.csv')
conc <- read.csv('brysbaert_2013_concreteness.csv')
monagh <- read.csv('monaghan2014_systematicity.csv')
SUBTL <- read.csv('SUBTLEX_US.csv')
AOA <- read.csv('kuperman_2014_AOA.csv')
cortese <- read.csv('cortese_2004.csv')
paivio <- read.csv('MRC_paivio_imageability.csv')
clark <- read.csv('clark_paivio_2004_imageability.csv')

## Get rid of 0's in the Paivio norms (corresponds to NAs):

paivio <- filter(paivio, Imag != 0)
paivio <- aggregate(Imag ~ Word, paivio, mean)

## Make all Clark & Paivio (2004) words lowcaps:

clark <- mutate(clark, Word = tolower(Word))

## Merge sensory experience ratings, concreteness ratings, into there:

icon$SER <- SER[match(icon$Word, SER$Word), ]$SER
icon$PaivioImag <- paivio[match(icon$Word, paivio$Word), ]$Imag
icon$CorteseImag <- cortese[match(icon$Word, cortese$Word), ]$Imageability
icon$TogliaImag <- cortese[match(icon$Word, cortese$Word), ]$TogliaImageability
icon$ClarkImag1 <- clark[match(icon$Word, clark$Word), ]$Imageability
icon$ClarkImag2 <- clark[match(icon$Word, clark$Word), ]$Imageability2
icon$Conc <- conc[match(icon$Word, conc$Word), ]$Conc.M
icon$Syst <- monagh[match(icon$Word, monagh$word), ]$relativeIconicity

## Merge frequency into there:

icon$Freq <- SUBTL[match(icon$Word, SUBTL$Word),]$FREQcount

## Log transform frequency:

icon <- mutate(icon,
	LogFreq = log10(Freq))

## How many overlap? Create a table of this:

these_columns <- c('SER', 'PaivioImag', 'CorteseImag',
	'TogliaImag', 'ClarkImag1', 'ClarkImag2', 'Conc', 'Syst')
M <- data.frame(these_columns,
	N = numeric(length(these_columns)),
	Percent = numeric(length(these_columns)))
for (i in 1:length(these_columns)) {
	this_column <- these_columns[i]
	M[i, ]$N <- sum(!is.na(icon[, this_column]))
	M[i, ]$Percent <- round(M[i, ]$N  / 3001, 2)
	}
	# focus on Paivio / Clark / Brysbaert since N > 1000

## Rename part of speech column:

SUBTL <- rename(SUBTL, POS = Dom_PoS_SUBTLEX)

## With the part-of-speech tags of SUBTLTLEX, merge grammatical items:

gram_items <- c('Article', 'Conjunction', 'Determiner', 'Number', 'Preposition', 'Pronoun',
	'Not', 'Ex', '#N/A', 'To')
SUBTL[SUBTL$POS %in% gram_items,]$POS <- 'Grammatical'

## Merge interjections and unclassified to just 'interjection':

iconic_items <- c('Unclassified', 'Interjection')
SUBTL[SUBTL$POS %in% iconic_items,]$POS <- 'Interjection'

## Code the POS of the following words (which occur in the iconicity data frame) by hand:

SUBTL[SUBTL$Word == 'brown',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'kitty',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'walker',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'grr',]$POS <- 'Interjection'
SUBTL[SUBTL$Word == 'ash',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'beery',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'bray',]$POS <- 'Verb'	# according to Macmillan
SUBTL[SUBTL$Word == 'char',]$POS <- 'Verb'	# according to Macmillan
SUBTL[SUBTL$Word == 'curly',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'frost',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'god',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'grace',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'herby',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'jag',]$POS <- 'Noun'	# according to Macmillan
SUBTL[SUBTL$Word == 'jammy',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'march',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'pat',]$POS <- 'Verb'	# according to Macmillan
SUBTL[SUBTL$Word == 'rusty',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'shaggy',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'soundless',]$POS <- 'Adjective'

## Code the POS of the following words (which occur in the SER data frame) by hand:

nouns <- c('abbot', 'alto', 'arbor', 'arc', 'aspen', 'barb', 'basil', 'beck',
	'belle', 'bill', 'birch', 'bond', 'boulder', 'bud', 'burl', 'cam', 'cape',
	'carol', 'celeste', 'cheddar', 'cliff', 'cob', 'colt', 'coop', 'crick',
	'daisy', 'dale', 'dean', 'dell', 'dill', 'ensign', 'eve', 'fife',
	'finch', 'flax', 'flint', 'ford', 'fort', 'gable', 'gale', 'garland',
	'gene', 'gill', 'glen', 'gob', 'gong', 'gore', 'grant', 'hank', 'hart',
	'hawk', 'heath', 'helm', 'hutch', 'iris', 'jack', 'jasper', 'jay',
	'jersey', 'knight', 'lily', 'ma', 'mace', 'mandrake', 'mark', 'marsh',
	'mason', 'metro', 'mike', 'moll', 'moss', 'nick', 'pa', 'paddy', 'peacock',
	'pearl', 'peg', 'pelt', 'pip', 'poppy', 'prism', 'quill', 'ray', 'reed',
	'regent', 'reuben', 'robin', 'ruby', 'saint', 'sheen', 'shogun',
	'shore', 'sparrow', 'steed', 'stein', 'stork', 'swan', 'thorn',
	'trill', 'tulip', 'van', 'velcro', 'venus', 'ware', 'watt',
	'welch', 'wick', 'wren', 'yam')

SUBTL[SUBTL$Word %in% nouns, ]$POS <- 'Noun'

## Add verb tags:

verbs <- c('dodge', 'don', 'drew', 'fester', 'josh', 'leach',
	'leer', 'lynch', 'parry', 'peck', 'pierce', 'retch', 'revere',
	'rue', 'stoke', 'trek')

SUBTL[SUBTL$Word %in% verbs, ]$POS <- 'Verb'

## Add adjective tags:
	
adjectives <- c('butch', 'frank', 'haggard', 'hale', 'hardy', 'hazel',
	'ill', 'stark')

SUBTL[SUBTL$Word %in% adjectives, ]$POS <- 'Adjective'

## Exclude tags that are not in MacMillan (no POS information):

not_in_macmillan <- c('bunt', 'bosh', 'druthers', 'fritz', 'leary', 'lilly',
	'milch', 'tong')

SUBTL <- SUBTL[!(SUBTL$Word %in% not_in_macmillan), ]

## Exclude since it is listed as a name in MacMillan:

SUBTL <- SUBTL[SUBTL$Word != 'bob',]

## Merge POS into the iconicity dataset:

icon$POS <- SUBTL[match(icon$Word, SUBTL$Word),]$POS



##------------------------------------------------------------------
## One sample t-test against zero:
##------------------------------------------------------------------

## Descriptive stats:

mean(icon$Iconicity)
sd(icon$Iconicity)

## One sample t-test:

t.test(icon$Iconicity, mu = 0)

## One sample effect size:

cohensD(icon$Iconicity, mu = 0)




##------------------------------------------------------------------
## Look at parts of speech for the iconicity ratings:
##------------------------------------------------------------------

## Look at means:

arrange(icon_agr <- aggregate(Iconicity ~ POS, icon, mean), desc(Iconicity))
arrange(icon_agr_sd <- aggregate(Iconicity ~ POS, icon, sd), desc(Iconicity))

## For ease of reporting:

(mutate(icon_agr, Iconicity = round(Iconicity, 2)) %>% arrange(desc(Iconicity)) -> icon_agr)

## Test for significant differences between lexical categories:

summary(icon_POS <- lm(Iconicity ~ POS, icon))
anova(icon_POS)

## Slides for Cologne talk, POS differences:

xmeans <- aggregate(Iconicity ~ POS, icon, mean)
xmeans <- arrange(xmeans, desc(Iconicity))
xmeans <- mutate(xmeans, Icon = round(Iconicity, 1))

color.fnc <- colorRampPalette(c('goldenrod', 'steelblue'), space = 'rgb')
colors <- color.fnc(7)

xfac <- 0.1
yfac <- 0.13
i <- 5
quartz('', 10, 6.5)
par(mai = c(1.5, 0.8, 0.2, 0.8))
plot(1, 1, type = 'n', xlim = c(0, 8), ylim = c(0, 3),
	bty = 'n', xlab = '', ylab = '',
	xaxt = 'n', yaxt = 'n')
if(i >= 1){
rect(xleft = 0.5 + xfac, xright = 1.5 - xfac, ybottom = 0,
	ytop = xmeans[1, 2], col = colors[1], border = NA)}
if(i > 1 ){
rect(xleft = 1.5 + xfac, xright = 2.5 - xfac, ybottom = 0,
	ytop = xmeans[2, 2], col = colors[2], border = NA)}
if(i > 2){
rect(xleft = 2.5 + xfac, xright = 3.5 - xfac, ybottom = 0,
	ytop = xmeans[3, 2], col = colors[3], border = NA)}
if(i > 3){
rect(xleft = 3.5 + xfac, xright = 4.5 - xfac, ybottom = 0,
	ytop = xmeans[4, 2], col = colors[4], border = NA)}
if(i > 4){
rect(xleft = 4.5 + xfac, xright = 5.5 - xfac, ybottom = 0,
	ytop = xmeans[5, 2], col = colors[5], border = NA)}
if(i > 5){
rect(xleft = 5.5 + xfac, xright = 6.5 - xfac, ybottom = 0,
	ytop = xmeans[6, 2], col = colors[6], border = NA)}
if(i > 6){
rect(xleft = 6.5 + xfac, xright = 7.5 - xfac, ybottom = 0,
	ytop = xmeans[7, 2], col = colors[7], border = NA)}
text(x = (1:7)[1:i], y = (xmeans$Iconicity + yfac)[1:i],
	labels = xmeans$Icon[1:i], font = 2, cex = 2.3,
	col = colors[1:i])
axis(side = 1, at = (1:7)[1:i], labels = c('Inter', 'Verb', 'Adj', 'Adverb', 'Noun', 'Gram', 'Name')[1:i],
	las = 2, font = 2, cex.axis = 2.3, tick = F, line = -1.1)



##------------------------------------------------------------------
## Comparison to Monaghan et al. (2014) norms:
##------------------------------------------------------------------

## Linear model:

cor.test(icon$Syst, icon$Iconicity,
	method = 'pearson', use = 'complete.obs')
summary(lm(Syst ~ Iconicity, icon))



##------------------------------------------------------------------
## Analysis of SER ratings, simple:
##------------------------------------------------------------------

## Loop through imageability and concreteness variables and correlate + plot:

these_columns <- c('SER', 'PaivioImag', 'CorteseImag', 'Conc')
M <- data.frame(these_columns,
	r = numeric(length(these_columns)),
	df = numeric(length(these_columns)),
	t = numeric(length(these_columns)),
	p = numeric(length(these_columns)))

quartz(width = 11, height = 7)
par(mfrow = c(2, 2), mai = rep(0.35, 4))
for (i in 1:length(these_columns)) {
	this_column <- these_columns[i]
	M[i, ]$r <- cor(icon$Iconicity, icon[, this_column], use = 'complete.obs')
	fit <- cor.test(icon$Iconicity, icon[, this_column], use = 'complete.obs')
	M[i, ]$df <- fit$parameter
	M[i, ]$t <- fit$statistic
	M[i, ]$p <- round(fit$p.value, 3)
	plot(icon[, this_column], icon$Iconicity, xlab = '', ylab = this_column)
	mtext(this_column, side = 3, font = 2, line = 0.5)
	}

## Patterns look quadratic, so let's do quadratic fits:

summary(lm(Iconicity ~ SER, icon))
summary(lm(Iconicity ~ SER + I(SER^2), icon))

summary(lm(Iconicity ~ Conc, icon))
summary(lm(Iconicity ~ Conc + I(Conc^2), icon))

summary(lm(Iconicity ~ PaivioImag, icon))
summary(lm(Iconicity ~ PaivioImag + I(PaivioImag^2), icon))

summary(lm(Iconicity ~ CorteseImag, icon))
summary(lm(Iconicity ~ CorteseImag + I(CorteseImag^2), icon))

## Same thing for systematicity:

summary(lm(Syst ~ SER, icon))
summary(lm(Syst ~ SER + I(SER^2), icon))

summary(lm(Syst ~ Conc, icon))
summary(lm(Syst ~ Conc + I(Conc^2), icon))

summary(lm(Syst ~ PaivioImag, icon))
summary(lm(Syst ~ PaivioImag + I(PaivioImag^2), icon))

summary(lm(Syst ~ CorteseImag, icon))
summary(lm(Syst ~ CorteseImag + I(CorteseImag^2), icon))

## Make a plot of the sensory experience ratings:

summary(xmdl1 <- lm(Iconicity ~ SER + Conc + CorteseImag + Syst + LogFreq + POS, icon))
summary(xmdl2 <- lm(Iconicity ~ SER + Conc + PaivioImag + Syst + LogFreq + POS, icon))

## For comparability, let's z-score the predictors:

icon <- mutate(icon,
	Syst_z = (Syst - mean(Syst, na.rm = T)) / sd(Syst, na.rm = T),
	SER_z = (SER - mean(SER, na.rm = T)) / sd(SER, na.rm = T),
	Conc_z = (Conc - mean(Conc, na.rm = T)) / sd(Conc, na.rm = T),
	PaivioImag_z = (PaivioImag - mean(PaivioImag, na.rm = T)) / sd(PaivioImag, na.rm = T),
	CorteseImag_z = (CorteseImag - mean(CorteseImag, na.rm = T)) / sd(CorteseImag, na.rm = T),
	LogFreq_z = (LogFreq - mean(LogFreq, na.rm = T)) / sd(LogFreq, na.rm = T))

## Make models with z-scored variables:

summary(xmdl1_z_noPOS <- lm(Iconicity ~ SER_z + Conc_z + CorteseImag_z +
	Syst_z + LogFreq_z, icon))
summary(xmdl2_z_noPOS <- lm(Iconicity ~ SER_z + Conc_z + PaivioImag_z +
	Syst_z + LogFreq_z, icon))

## Make models with z-scored variables and POS:

summary(xmdl1_z <- lm(Iconicity ~ SER_z + Conc_z + CorteseImag_z +
	Syst_z + LogFreq_z + POS, icon))
summary(xmdl2_z <- lm(Iconicity ~ SER_z + Conc_z + PaivioImag_z +
	Syst_z + LogFreq_z + POS, icon))

## Test variance inflation factors:

vif(xmdl1_z_noPOS)
vif(xmdl1_z_noPOS)
vif(xmdl1_z)
vif(xmdl1_z)

## Create a model without Conc (due to collinearity and not significant):

summary(xmdl1_z <- lm(Iconicity ~ SER_z + CorteseImag_z +
	Syst_z + LogFreq_z + POS, icon))
summary(xmdl2_paivio <- lm(Iconicity ~ SER_z + PaivioImag_z +
	Syst_z + LogFreq_z + POS, icon))	
summary(xmdl1_z_noPOS <- lm(Iconicity ~ SER_z + CorteseImag_z +
	Syst_z + LogFreq_z, icon))
summary(xmdl1_z_noPOS_noSER <- lm(Iconicity ~ 1 + CorteseImag_z +
	Syst_z + LogFreq_z, icon))

## Make a marginal model without POS for plotting:

noNA <- !is.na(icon$CorteseImag) & !is.na(icon$Syst) & !is.na(icon$LogFreq) & !is.na(icon$SER)
icon_red <- icon[noNA, ]
summary(xmdl1_marg <- lm(Iconicity ~ CorteseImag + LogFreq, icon_red))
icon_red$Res <- residuals(xmdl1_marg)

## Regress iconicity onto SER:

summary(xmdl <- lm(Res ~ SER, icon_red))

## Make predictions for plot:

newdata <- data.frame(SER = seq(0, 7, 0.01))
newdata$fit <- predict(xmdl, newdata)
newdata$UB <- newdata$fit + 1.96 * predict(xmdl, newdata, se.fit = T)$se.fit
newdata$LB <- newdata$fit - 1.96 * predict(xmdl, newdata, se.fit = T)$se.fit

## Make a plot of this:

setup_plots(N = 1)
emptyplot(xlim = c(1, 5.5), ylim = c(-3.5, 4.5))
left_axis(text = 'Iconicity Ratings', at = seq(-4, 4, 2), type = 1)
mtext(side = 2, line = 2.3, text = '(Residuals)', cex = 1.5)
lower_axis(style = 'continuous', at = 1:7, lab = '', type = 1)
mtext(side = 1, text = 'Sensory Experience Ratings',
	line = 3.5, font = 2, cex = 2)
# text(icon_red$SER, icon_red$Res, labels = icon_red$Word, col = rgb(0, 0, 0, 0.4))
points(icon_red$SER, icon_red$Res, pch = 21,
	cex = 1.1, bg = rgb(0, 0, 0, 0.6), col = NA)
polygon(c(newdata$SER, rev(newdata$SER)),
	c(newdata$UB, rev(newdata$LB)), col = rgb(0, 0, 0, 0.4), border = F)
points(newdata$SER, newdata$fit, lwd = 2, type = 'l')

## Same thing with systematicity:

summary(xmdl1_Syst <- lm(Syst ~ SER_z + CorteseImag_z +
	LogFreq_z + POS, icon))
summary(xmdl1_Syst_noPOS <- lm(Syst ~ SER_z + CorteseImag_z +
	LogFreq_z, icon))
summary(xmdl1_Syst_noPOS_noSER <- lm(Syst ~ 1 + CorteseImag_z +
	LogFreq_z, icon))

## For Colonge plot, depict particular words:

lA <- c('pop')
lB <- c('pop', 'click')
lC <- c('pop', 'click', 'steak')
lD <- c('pop', 'click', 'steak', 'scale')
lE <- c('pop', 'click', 'steak', 'scale', 'node')
lF <- c('pop', 'click', 'steak', 'scale', 'node', 'quick')

i <- which(icon_red$Word %in% lA)

## Plot for Cologne presentation:

quartz('', 9.5, 6.25)
par(mai = c(1.25, 1.25, 0.25, 0.25))
plot(1, 1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n',
	xlab = '', ylab = '', xlim = c(0.5, 5.5), ylim = c(-4, 4.5), xaxs = 'i', yaxs = 'i')
points(icon_red$SER, icon_red$Res, font = 2,
	bg = rgb(0, 0, 0, 0.7), cex = 1.5, pch = 21,
	col = NA)
# text(icon_red[i, ]$SER, icon_red[i, ]$Res, labels = icon_red[i, ]$Word,
	# font = 2, col = 'black', cex = 2.5)
# points(icon_red[-i, ]$SER, icon_red[-i, ]$Res, font = 2,
	# bg = rgb(0, 0, 0, 0.7), cex = 1.5, pch = 21,
	# col = NA)
points(newdata$SER, newdata$fit, lwd = 4, type = 'l',
	col = rgb(0.4, 0.1, 0.4, 0.9))
polygon(c(newdata$SER, rev(newdata$SER)),
	c(newdata$UB, rev(newdata$LB)), col = rgb(0.4, 0.1, 0.4, 0.6), border = F)
arrows(x0 = 0.5, x1 = 5.5, y0 = -4, lwd = 6, xpd = NA)
mtext(side = 1, text = 'Sensory Experience Ratings', cex = 3, font = 2, line = 3)
arrows(x0 = 0.5, y0 = -4, y1 = 4.5, lwd = 6, xpd = NA)
mtext(side = 2, text = 'Iconicity Ratings', cex = 3, font = 2, line = 2.5)

## Plot for Cologne presentation of imageability:
## Make a marginal model without POS for plotting:

noNA <- !is.na(icon$CorteseImag) & !is.na(icon$Syst) & !is.na(icon$LogFreq) & !is.na(icon$SER)
icon_red <- icon[noNA, ]
summary(xmdl1_marg <- lm(Iconicity ~ LogFreq + SER, icon_red))
icon_red$Res2 <- residuals(xmdl1_marg)

## Regress iconicity onto SER:

summary(xmdl <- lm(Res2 ~ CorteseImag, icon_red))

## Make predictions for plot:

newdata <- data.frame(CorteseImag = seq(0, 7, 0.01))
newdata$fit <- predict(xmdl, newdata)
newdata$UB <- newdata$fit + 1.96 * predict(xmdl, newdata, se.fit = T)$se.fit
newdata$LB <- newdata$fit - 1.96 * predict(xmdl, newdata, se.fit = T)$se.fit

## The plot:

quartz('', 9.5, 6.25)
par(mai = c(1.25, 1.25, 0.25, 0.25))
plot(1, 1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n',
	xlab = '', ylab = '', xlim = c(0.5, 7.5), ylim = c(-3, 4.5), xaxs = 'i', yaxs = 'i')
points(icon_red$CorteseImag, icon_red$Res2, font = 2,
	bg = rgb(0, 0, 0, 0.7), cex = 1.5, pch = 21,
	col = NA)
# text(icon_red[i, ]$SER, icon_red[i, ]$Res, labels = icon_red[i, ]$Word,
	# font = 2, col = 'black', cex = 2.5)
# points(icon_red[-i, ]$SER, icon_red[-i, ]$Res, font = 2,
	# bg = rgb(0, 0, 0, 0.7), cex = 1.5, pch = 21,
	# col = NA)
points(newdata$CorteseImag, newdata$fit, lwd = 4, type = 'l',
	col = rgb(0.4, 0.1, 0.4, 0.9))
polygon(c(newdata$CorteseImag, rev(newdata$CorteseImag)),
	c(newdata$UB, rev(newdata$LB)), col = rgb(0.4, 0.1, 0.4, 0.6), border = F)
arrows(x0 = 0.5, x1 = 7.5, y0 = -3, lwd = 6, xpd = NA)
mtext(side = 1, text = 'Imageability Ratings', cex = 3, font = 2, line = 3)
arrows(x0 = 0.5, y0 = -3, y1 = 4.5, lwd = 6, xpd = NA)
mtext(side = 2, text = 'Iconicity Ratings', cex = 3, font = 2, line = 2.5)





##------------------------------------------------------------------
## Analysis of by-modality differences:
##------------------------------------------------------------------

## Take aggregate:

xagr <- rbind(dplyr::select(l, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity),
	dplyr::select(n, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity),
	dplyr::select(v, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity))

## Add iconicity and systematicity to this:

xagr$Iconicity  <- icon[match(xagr$Word, icon$Word),]$Iconicity
xagr$Syst  <- icon[match(xagr$Word, icon$Word),]$Syst

## Descriptive means:

arrange(aggregate(Iconicity ~ DominantModality, xagr, mean), desc(Iconicity))
aggregate(Iconicity ~ DominantModality, xagr, sd)

## Add POS column:

xagr$POS <- c(rep('adj', nrow(l)), rep('noun', nrow(n)), rep('verb', nrow(v)))

## Add frequency into this:

xagr$Freq <- log10(SUBTL[match(xagr$Word, SUBTL$Word),]$FREQcount + 1)

## Make a model of this:

summary(xmdl <- lm(Iconicity ~ DominantModality, xagr))
summary(xmdl.pos <- lm(Iconicity ~ DominantModality + POS + Freq, xagr))
anova(xmdl)
anova(xmdl.pos)

## R-squared:

xmdl.pos.null <- lm(Iconicity ~ 1 + POS + AOA + Freq, xagr)
summary(xmdl.pos)$adj.r.squared - summary(xmdl.pos.null)$adj.r.squared

## Make predictions:

mypreds <- my.predict.lm(xmdl)

## Make a plot of this:

quartz('', 8, 6)
par(mai = c(1.45, 1.25, 0.75, 0.75))
emptyplot(xlim = c(0.5, 5.5), ylim = c(0, 2))
draw_preds(mypreds)
axis(side = 1, at = 1:5,
	labels = c('', '', '', '', ''),
	font = 2, las = 2, cex.axis = 1.25, lwd.ticks = 2)
axis(side = 1, at = 1:5 - 0.1,
	labels = c('Visual', 'Tactile', 'Auditory', 'Gustatory', 'Olfactory'),
	font = 2, las = 2, cex.axis = 1.25, lwd.ticks = 2, tick = F)
axis(side = 1, at = 1:5 + 0.1,
	labels = paste('N', table(xagr$DominantModality), sep = ' = '),
	font = 2, las = 2, cex.axis = 1.05, lwd.ticks = 2, tick = F)
left_axis(text = 'Iconicity', at = seq(0, 2, 0.5), type = 1)
lower_axis(style = 'modality', lab = '', N = xagr$DominantModality, type = 2)

## Separate by-modality analysis for different parts of speech:

summary(xmdl.v <- lm(Iconicity ~ DominantModality, subset(xagr, POS == 'verb')))
anova(xmdl.v)
summary(xmdl.n <- lm(Iconicity ~ DominantModality, subset(xagr, POS == 'noun')))
anova(xmdl.n)
summary(xmdl.adj <- lm(Iconicity ~ DominantModality, subset(xagr, POS == 'adj')))
anova(xmdl.adj)

## Make a model of this:

summary(xmdl.syst <- lm(Syst ~ DominantModality, xagr))
summary(xmdl.pos.syst <- lm(Syst ~ DominantModality + POS + Freq, xagr))
anova(xmdl.syst)
anova(xmdl.pos.syst)

## Among the adjectives, test whether color terms have significantly lower iconicity:

semcodings <- read.csv('lynott_connell_2009_semantic_codings.csv')
l$Color <- semcodings[match(l$Word, semcodings$Word), ]$Color
l$Iconicity <- icon[match(l$Word, icon$Word), ]$Iconicity
visonly <- filter(l, DominantModality == 'Visual')
t.test(Iconicity ~ Color, visonly, paired = F, var.equal = T)

## Test continuous variables rather than categorical variables:

summary(lm(Iconicity ~ HapticStrengthMean, data = xagr))
summary(lm(Iconicity ~ AuditoryStrengthMean, data = xagr))
summary(lm(Iconicity ~ OlfactoryStrengthMean, data = xagr))
summary(lm(Iconicity ~ GustatoryStrengthMean, data = xagr))
summary(lm(Iconicity ~ VisualStrengthMean, data = xagr))




##------------------------------------------------------------------
## Analysis of whether sound association explains tactile iconicity:
##------------------------------------------------------------------

## Adjectives only:

library(lavaan)
myFormula <- '# direct effect:
	Iconicity ~ c * HapticStrengthMean
	# mediator:
	AuditoryStrengthMean ~ a * HapticStrengthMean
	Iconicity ~ b * AuditoryStrengthMean
	# indirect effect (a * b):
	ab := a * b
	# total effect:
	total := c + (a * b)'

## Path analysis for only haptic adjectives:

hap_adjs <- xagr[xagr$POS == 'adj' & xagr$DominantModality == 'Haptic',]
hap_adjs <- hap_adjs[!is.na(hap_adjs$Iconicity),]
hap_adjs.fit <- sem(myFormula, data = hap_adjs, se = 'boot')		# haptic subset
summary(hap_adjs.fit)

## Path analysis for adjectives (reported in chapter):

lred <- xagr[xagr$POS == 'adj',]
lred <- lred[!is.na(lred$Iconicity),]
lred.fit <- sem(myFormula, data = lred, se = 'boot')
summary(lred.fit)

## Path analysis for all data (not reported in main text):

xagr.red <- xagr[!is.na(xagr$Iconicity),]
xagr.fit <- sem(myFormula, data = xagr.red, se = 'boot')
summary(xagr.fit)



##------------------------------------------------------------------
## Phonestheme analysis:
##------------------------------------------------------------------

## Load in phonesthemes (hand-coded based on Hutchins, 1998 Appendix A):

phon <- read.xlsx('phonesthemes_hutchins_1998.xlsx', 1)

## Process the phonesthemes:

phonsplit <- strsplit(phon$Phonaestheme, ';')
nphon <- numeric(length(phonsplit))
for (i in 1:length(phonsplit)) {
	if (any(!is.na(phonsplit[[i]]))) {
		nphon[i] <- length(phonsplit[[i]])
		}
	}
phon$NPhonestheme <- nphon

## Separate final and initial phonesthemes:

phon$NInitialPhonestheme <- numeric(nrow(phon))
phon$NFinalPhonestheme <- numeric(nrow(phon))
for (i in 1:length(phonsplit)) {
	phon[i,]$NInitialPhonestheme <- length(grep('[a-z]+-', phonsplit[[i]]))
	phon[i,]$NFinalPhonestheme <- length(grep('-[a-z]+', phonsplit[[i]]))
	}

## Put this into Lynott & Connell (2009):

l <- cbind(l, phon[match(l$Word, phon$Word),-1])
l$Iconicity <- icon[match(l$Word, icon$Word),]$Iconicity

## Make a table:

xtab <- table(l$DominantModality, ifelse(l$NPhonestheme == 0, 0, 1))
props <- round(xtab[,2] / rowSums(xtab[,1:2]),2)

## Make a chi-square test:

chisq.test(xtab)


##------------------------------------------------------------------
## OED analysis:
##------------------------------------------------------------------

## Load in etymologies:

OED <- read.csv('all_words_OED_etymology.csv')

## Match:

l <- cbind(l, OED[match(l$Word, OED$Word),-1])

## Which are not in OED?

l[is.na(l$Year),]$Word		# 'coconutty'

## Get rid of 'coconutty' for this analysis:

l <- l[l$Word != 'coconutty',]

## Fix some origin POS:

l[l$OED_OriginPOS == 'noun/adj same form',]$OED_OriginPOS <- 'adj/noun'
l[l$OED_OriginPOS == 'noun or verb',]$OED_OriginPOS <- 'noun/verb'

## Suspected to be imitative:

l$Imitative <- 'not'
l[l$Origin %in% c('possibly imitative', 'Germanic/probably imitative', 'Germanic, perhaps ultimatively imitative', 'probably imitative'),]$Imitative <- 'possibly'
l[l$Origin %in% 'imitative',]$Imitative <- 'yes'
l[l$Origin %in% c('unclear', 'origin unknown', 'unclear, probably Scandinavian'),]$Imitative <- 'unclear'

## Make a table:

(xtab <- table(l$DominantModality, l$Imitative))
arrange(aggregate(Iconicity ~ Imitative, l, mean), desc(Iconicity))

## Chi-square test of this:

chisq.test(xtab)

## Test iconicity rating difference between 'unclear' and 'not iconic':

t.test(Iconicity ~ Imitative, filter(l, Imitative %in% c('unclear', 'not')),
	paired = F, var.equal = T)

## Effect size of this:

cohen.d(Iconicity ~ Imitative, filter(l, Imitative %in% c('unclear', 'not')))

