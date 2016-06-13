## Bodo Winter
## September 17, 2015
## Plotting functions for dissertation

## Function for setting up empty plotting window:

setup_plots <- function (N = 1) {
	if ( N == 1 ) {
		quartz('', 8, 6)
		par(mai = c(1.15, 1.25, 0.75, 0.75))	
		}
	if ( N == 2 ) {
		quartz('', 11, 5)
		par(mfrow = c(1, 2), omi = c(1, 1.1, 0.85, 0.25), mai = c(0, 0.25, 0, 0))
		}
	if ( N == 3 ) {
		quartz('', 9, 8)
		par(omi = c(0.8, 1, 0, 0),
			mai = c(0.35, 0.25, 0.75, 0.5))
		layout(matrix(c(1,1,2,2,0,3,3,0), 2, 4, byrow=TRUE), respect=FALSE)		
		}
	if ( N == 5 ) {
		quartz('', 10, 7)
		par(omi = c(0.6, 1, 0, 0),
			mai = c(0.45, 0.15, 0.5, 0.25))
		layout(matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), 2, 6, byrow=TRUE), respect=FALSE)		
		}
	}

## Function for setting up an empty plot:

emptyplot <- function(xlim = c(0.5, 5.5), ylim = c(0, 10), AB = '', xfactor = 0.03, yfactor = 0.04, ...) {
	plot(1, 1, xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', type = 'n', xlim = xlim, ylim = ylim, ...)
	box(lwd = 2)
	if (AB != '' & length(AB) > 0) {
		xrange <- abs(xlim[2] - xlim[1])
		yrange <- abs(ylim[2] - ylim[1])
		xpos <- xlim[1] + (xrange * xfactor)
		ypos <- ylim[2] - (yrange * yfactor)
		text(xpos, ypos, labels = AB, font = 2, cex = 1.5)
		}
	}

## Function for computing density for a specific value, given a bandwidth:
## Code from 'user10525':
## http://stats.stackexchange.com/questions/32093/how-to-draw-mean-median-and-mode-lines-in-r-that-end-at-density

kg <- function(x, vals) return(mean(dnorm((x - vals) / bw.nrd0(vals))) / bw.nrd0(vals))

## Function for plotting density:

plot_density <- function(x, mean = T, powerpoint = F) {
	## Compute density:
	
	dens_obj <- density(x)

	## Extract the density:

	dens <- as.data.frame(dens_obj[1:2])

	## Restrict densities to observed range:

	dens <- dens[dens$x > min(x) & dens$x < max(x),]

	this_color <- ifelse(powerpoint, 'wheat3', 'lightgray')

	polygon(x = c(dens$x, rev(dens$x)),
		y = c(dens$y, rep(0, length(dens$x))),
		col = this_color, border = F)
	
	if(mean) {
		if (!powerpoint) {
		segments(x0 = mean(x), x1 = mean(x),
			y0 = 0, y1 = kg(mean(x), x) - 0.003, lwd = 3, col = 'gray38')
			} else {
		segments(x0 = mean(x), x1 = mean(x),
			y0 = 0, y1 = kg(mean(x), x) - 0.004, lwd = 5, col = 'darkred')				
				}
		}
	
	segments(x0 = dens$x[1], x1 = dens$x[1],
		y0 = 0, y1 = dens$y[1], lty = 2)
	segments(x0 = dens$x[length(dens$x)], x1 = dens$x[length(dens$x)],
		y0 = 0, y1 = dens$y[length(dens$x)], lty = 2)
	points(dens$x, dens$y, lwd = 2, type = 'l')
	if (powerpoint) {
		points(dens$x, dens$y, lwd = 3, type = 'l')
		}
	}

## Function for drawing segments with 95% confidence intervals:

draw_preds <- function(df) {
	arrows(x0 = 1:nrow(df), x1 = 1:nrow(df),
		y0 = df$UB,
		y1 = df$LB,
		lwd = 2, length = 0.05, angle = 90, code = 3)
	points(1:nrow(df), y = df$fit, pch = 19, cex = 1.5)
	}

## Function for drawing continuous predictor with 95% confidence intervals:

draw_cont_pred <- function(df) {
	predictor_name <- setdiff(colnames(df), c('fit', 'UB', 'LB'))
	xvals <- df[,predictor_name]
	yvals <- df$fit
	polygon(x = c(xvals, rev(xvals)), y = c(df$UB, rev(df$LB)), col = rgb(0, 0, 0, 0.3), border = NA)	
	points(xvals, yvals, type = 'l', lwd = 2)
	}

## Function for plotting model predictions:

plot_lines <- function(newdata,
	x_column = 'Strength',
	y_column = 'LynFit',
	special_modalities = 'VisualStrength',
	text_x = 5.3) {
		for (i in 1:5) {
			this_modality <- levels(newdata$WhichModality)[i]
			this_subset <- newdata[newdata$WhichModality == this_modality, ]
			
			line_type <- ifelse(this_modality %in% special_modalities, 1, 3)
			line_width <- ifelse(this_modality %in% special_modalities, 3, 1)
			
			points(this_subset[, x_column], this_subset[, y_column], type = 'l',
				lwd = line_width, lty = line_type)
			text(x = text_x, this_subset[nrow(this_subset), y_column],
				labels = substr(this_modality, 1, 3), font = 2, cex = 1.15)
			}
	}

## Function for common bottom axis styles:

lower_axis <- function(style = 'modality', lab = 'Modality', N = NULL, type = 3, ...) {
	if (style == 'continuous') {
		if (type == 1) {
			axis(side = 1, cex.axis = 1.25, lwd.ticks = 2, font = 2, ...)
			mtext(lab, side = 1, line = 2.5, font = 2, cex = 2)
			}
		if (type == 5) {
			axis(side = 1, cex.axis = 1.15, lwd.ticks = 2, font = 2, ...)
			mtext(lab, side = 1, line = 2.8, font = 2, cex = 1.35)
			}			
		}
	if (style == 'modality') {
		if (type == 2) cex_point <- 1.6
		if (type == 3) cex_point <- 1.8
		axis(side = 1, at = 1:5, labels = F, lwd.ticks = 2)
		axis(side = 1, at = 1:5, labels = c('Vis', 'Tac', 'Aud', 'Gus', 'Olf'),
			font = 2, cex.axis = cex_point, line = 0.6, tick = F)
		if ( length(N) > 0 ) {
			axis(side = 1, at = 1:5,
				labels = paste('N', table(N), sep = '='),
				font = 2, cex.axis = 1.3, line = 2, tick = F)
			}
		}
	}

## Function for common left axis styles:

left_axis <- function(text = '', at = seq(0, 5, 1), type = 3, ...) {
	axis(side = 2, at = at, lwd = 2, font = 2, cex.axis = 1.5, las = 2, ...)
	if (type == 3) mtext(side = 2, text = text, line = 4.2, font = 2, cex = 1.4)
	if (type == 2) mtext(side = 2, text = text, line = 3.3, font = 2, cex = 1.9)
	if (type == 1) mtext(side = 2, text = text, line = 4.1, font = 2, cex = 2)
	}

## Function for common headers:

top_labels <- function(first_text = 'Adjectives', second_text = 'Lynott & Connell (2009)',
	type = 3) {
	if (type == 3) {
		line_point <- 2.5
		cex_point <- 1.8
		}
	if (type == 2) {
		line_point <- 2
		cex_point <- 2
		}
     if (second_text != '') {
          mtext(side = 3, text = first_text, line = line_point, font = 2, cex = cex_point)
          mtext(side = 3, text = second_text, line = 0.5, font = 2, cex = 1.25)
     }
	mtext(side = 3, text = first_text, line = line_point - 1.4, font = 2, cex = cex_point)
	}


