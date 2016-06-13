## Bodo Winter
## October 13, 2015
## Model prediction functions for dissertation

## Function for getting regular predictions:

my.predict.lm <- function(mdl, link = NULL) {
     df <- data.frame(mdl$xlevels[[1]])
	colnames(df) <- names(mdl$xlevels)
	df <- cbind(df, as.data.frame(predict(mdl, newdata = df, se.fit = T)[1:2]))
     df$UB <- df$fit + 1.96 * df$se.fit
     df$LB <- df$fit - 1.96 * df$se.fit
     
     if (length(link) > 0){
	     if (link == 'log') {
    	 	df$fit <- exp(df$fit)
     		df$UB <- exp(df$UB)
	     	df$LB <- exp(df$LB)
     	}
     	}
     
     return(df)
     }
	
## Function for getting mixed model predictions:

predict.glmm <- function(fit, levels,
     pred = 'WhichModality', resp = 'Strength') {
     newdata <- cbind(data.frame(levels, rep(0, length(levels))))
     colnames(newdata) <- c(pred, resp)
     mm <- model.matrix(terms(fit), newdata)
     newdata[,resp] <- predict(fit, newdata, re.form = NA)
     pvar1 <- diag(mm %*% tcrossprod(vcov(fit), mm))
     newdata$UB <- newdata[,resp] + 1.96 * sqrt(pvar1)
     newdata$LB <- newdata[,resp] - 1.96 * sqrt(pvar1)
     colnames(newdata)[colnames(newdata) == resp] <- 'fit'

     return(newdata)
     }


