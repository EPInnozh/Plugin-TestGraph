############################################################################
#                 FONCTIONS GRAPHIQUES                                     #
############################################################################
# Nuage de points avec droite de regression
graphCorrelation<-function (formula, factor, data = parent.frame(), boxplots = c("without","x", "y", "xy"), xlim = c(min(varx), max(varx)),ylim = c(min(vary),max(vary)),
xlab = NULL,ylab = NULL,legend.lab = NULL, IC.confidence = FALSE,IC.conf.value = 0.95, IC.predict = FALSE, IC.pred.value = 0.95,
axes = TRUE, width.title = 4, main = "Plot of regression", pch = NULL,pointsize=1,col = palette()) 
{
require(car)
op <- par(no.readonly = TRUE)
#extraction des parametres
eval(parse(text=paste0("ex<-Extract.fact(",deparse(substitute(formula)),",covariate=",deparse(substitute(factor)),",data=",deparse(substitute(data)),")")))
test.formula <- deparse(substitute(formula), 400)
test.formula <- unlist(strsplit(test.formula, "~"))
formula.left <- paste("~", test.formula[1])
formula.right <- paste("~", test.formula[2])
listvar.left <- as.character(attr(terms(formula(formula.left)),"variables"))[-1]
listvar.right <- as.character(attr(terms(formula(formula.right)),"variables"))[-1]
n.row <- ex$length.response
n.col <- ex$length.factor
position <- c("bottomleft", "bottomright", "topleft","topright")
boxplots <- match.arg(boxplots)
boxes <- switch(boxplots, without = 0, x = 1, y = 2, xy = 3)
#Sans groupe
    if (ex$length.covariate==0) {
	#Simple
        if (n.row == 1 & n.col == 1) {
		layout(matrix(c(1, 1, 1, 2, 6, 5, 3, 4, 5), 3, 3,byrow = TRUE), c(.7, 8, 1.3), c(1.3, 8, .7), TRUE)
        par(mar = c(0, 0, 0, 0))
        plot(c(0, 10), c(0, 10), type = "n", axe = F)
        varx <- listvar.right
        vary <- listvar.left
            if (is.null(xlab)) 
            xlab <- paste(listvar.right)
            if (is.null(ylab)) 
            ylab <- paste(listvar.left)
        valid <- complete.cases(with(data, eval(parse(text = varx))),with(data, eval(parse(text = vary))))
        varx <- with(data, eval(parse(text = varx)))[valid]
        vary <- with(data, eval(parse(text = vary)))[valid]
        coef.cor <- cor.test(varx, vary)
        coef.cor <- round(as.numeric(coef.cor[[4]]), digit = 3)
        predict.seq <- data.frame(varx = seq(min(varx), max(varx),length.out = length(varx)))
        pred.w.plim <- predict(lm(vary ~ varx), predict.seq,interval = "prediction", level = IC.pred.value)
        pred.w.clim <- predict(lm(vary ~ varx), predict.seq,interval = "confidence", level = IC.conf.value)
        text(5, 5, main, cex = 1.5)
        par(mar = c(5, 0, 0, 0))
            if (boxes == 2 | boxes == 3) 
            boxplot(vary, axes = F)
            if (boxes == 0 | boxes == 1) 
            plot(0, type = "n", axes = F, xlab = "",ylab = "")
        par(mar = c(0, 0, 0, 0))
        plot(c(0, 10), c(0, 10), type = "n", axe = F)
        par(mar = c(0, 5, 0, 0))
            if (boxes == 1 | boxes == 3) 
            boxplot(varx, axes = FALSE, horizontal = TRUE)
            if (boxes == 0 | boxes == 2) 
            plot(0, type = "n", axes = FALSE, xlab = "",ylab = "")
			if (is.null(pch)) pch<-1
        par(mar = c(0, 0, 0, 0))
        plot(c(0, 10), c(0, 10), type = "n", axe = FALSE)
        legend(0, 10, coef.cor, pch = pch, col = col, lwd = 1, title = expression(rho), merge = TRUE)
        par(mar = c(5, 5, 0, 0))
       plot(varx, vary, xlim = xlim, ylim = ylim, xlab = xlab,ylab = ylab,type="n" )
	   points(varx, vary,pch = pch,cex=pointsize, col = col[1])
       abline(lm(formula, data = data), col = col[1])
            if (IC.confidence) {
            lines(predict.seq$varx, pred.w.clim[, 2], lty = 2,col = col[1], lwd = 2)
            lines(predict.seq$varx, pred.w.clim[, 3], lty = 2,col = col[1], lwd = 2)
            }
            if (IC.predict) {
            lines(predict.seq$varx, pred.w.plim[, 2], lty = 3, col = col[1], lwd = 2)
            lines(predict.seq$varx, pred.w.plim[, 3], lty = 3, col = col[1], lwd = 2)
            }
        }
		#multiple       
	   else {
        layout(matrix(c(rep(1, n.col + 3), seq(2, (n.row +3) * (n.col + 3) + 1)), n.row + 4, n.col + 3,byrow = TRUE), c(4, 4, rep(28, n.col), 4), c(4,4, rep(28, n.row), 4, 4), TRUE)
        par(mar = c(0, 0, 0, 0))
		if (is.null(pch)) pch<-1
        plot(c(0, 1), c(0, 1), type = "n", axes = FALSE)
        text(0.5, 0.5, main, cex = 1.5)
            for (i in 1:2) plot(c(0, 1), c(0, 1), type = "n",axes = FALSE)
            for (i in 1:(n.col)) {
            plot(c(0, 1), c(0, 1), type = "n", axes = FALSE)
			mtext(listvar.right[i], 1, line = -1.2)
            }
            plot(c(0, 1), c(0, 1), type = "n", axes = FALSE)
            if (n.row > 1) {
				for (i in 1:(n.row - 1)) {
                valid <- complete.cases(with(data, eval(parse(text = listvar.right[1]))),with(data, eval(parse(text = listvar.left[i]))))
                varx <- with(data, eval(parse(text = listvar.left[i])))[valid]
                vary <- with(data, eval(parse(text = listvar.right[1])))[valid]
                coef.cor <- cor.test(varx, vary)
                coef.cor <- round(as.numeric(coef.cor[[4]]),digit = 3)
                predict.seq <- data.frame(varx = seq(min(varx),max(varx), length.out = length(varx)))
                pred.w.plim <- predict(lm(vary ~ varx), predict.seq,interval = "prediction", level = IC.pred.value)
                pred.w.clim <- predict(lm(vary ~ varx), predict.seq,interval = "confidence", level = IC.conf.value)
					if (boxes == 2 | boxes == 3) 
					boxplot(vary, axes = F)
					if (boxes == 0 | boxes == 1) 
					plot(0, type = "n", axes = F, xlab = "",ylab = "")
					plot(c(0, 1), c(0, 1), type = "n", axes = F,ylab = "", xlab = "")
					if (listvar.left[i] != listvar.right[1]) {
                    plot(varx, vary, xlim = xlim, ylim = ylim,axes = F,type="n")
					points(varx, vary,pch = pch,cex=pointsize, col = col[1])
                    abline(lm(vary ~ varx), col = col[1])
                    if (IC.confidence) {
                    lines(predict.seq$varx, pred.w.clim[, 2],lty = 2, col = col[1], lwd = 2)
                    lines(predict.seq$varx, pred.w.clim[, 3],lty = 2, col = col[1], lwd = 2)
                    }
                    if (IC.predict) {
                    lines(predict.seq$varx, pred.w.plim[, 2],lty = 3, col = col[1], lwd = 2)
                    lines(predict.seq$varx, pred.w.plim[, 3],lty = 3, col = col[1], lwd = 2)
                    }
                angles <- expand.grid(par()$usr[1:2], par()$usr[3:4])
                X <- matrix(c(varx, vary), ncol = 2)
                distances <- apply(angles, 1, function(w) min(rowSums(sweep(X,2, w, "-")^2)))
                position.leg <- position[max(distances) == distances]
                eval(parse(text = paste("legend('",position.leg, "','", coef.cor, "',title=expression(rho))", sep = "")))
                    if (axes) 
                    axis(2)
					}
					else {
                    plot(varx, vary, type = "n", axes = F, ylab = "", xlab = "")
                    axis(2)
					}
                box()
					if (n.col > 1) {
						for (j in 2:n.col) {
						valid <- complete.cases(with(data, eval(parse(text = listvar.right[j]))),with(data, eval(parse(text = listvar.left[i]))))
						varx <- with(data, eval(parse(text = listvar.left[i])))[valid]
						vary <- with(data, eval(parse(text = listvar.right[j])))[valid]
						coef.cor <- cor.test(varx, vary)
						coef.cor <- round(as.numeric(coef.cor[[4]]), digit = 3)
							if (listvar.left[i] != listvar.right[j]) {
							plot(varx, vary, axes = F, type="n")
							points(varx, vary,pch = pch,cex=pointsize, col = col[1])
							abline(lm(vary ~ varx), col = col[1])
								if (IC.confidence) {
								lines(predict.seq$varx, pred.w.clim[,2], lty = 2, col = col[1], lwd = 2)
								lines(predict.seq$varx, pred.w.clim[,3], lty = 2, col = col[1], lwd = 2)
								}
								if (IC.predict) {
								lines(predict.seq$varx, pred.w.plim[,2], lty = 3, col = col[1], lwd = 2)
								lines(predict.seq$varx, pred.w.plim[,3], lty = 3, col = col[1], lwd = 2)
								}
							angles <- expand.grid(par()$usr[1:2],par()$usr[3:4])
							X <- matrix(c(varx, vary), ncol = 2)
							distances <- apply(angles, 1, function(w) min(rowSums(sweep(X,2, w, "-")^2)))
							position.leg <- position[max(distances) == distances]
							eval(parse(text = paste("legend('", position.leg, "','", coef.cor, "',title=expression(rho))",sep = "")))
							}
							else {
							plot(c(0, 1), c(0, 1), type = "n",axes = F, ylab = "", xlab = "")
							}
						box()
						}
					}
				plot(c(0, 1), c(0, 1), type = "n", axes = F,ylab = "", xlab = "")
				mtext(listvar.left[i], 2, line = -1.3)
                }
            }
        valid <- complete.cases(with(data, eval(parse(text = listvar.right[1]))),with(data, eval(parse(text = listvar.left[n.row]))))
        varx <- with(data, eval(parse(text = listvar.left[n.row])))[valid]
        vary <- with(data, eval(parse(text = listvar.right[1])))[valid]
        coef.cor <- cor.test(varx, vary)
        coef.cor <- round(as.numeric(coef.cor[[4]]), digit = 3)
        predict.seq <- data.frame(varx = seq(min(varx), max(varx),length.out = length(varx)))
        pred.w.plim <- predict(lm(vary ~ varx), predict.seq,interval = "prediction", level = IC.pred.value)
        pred.w.clim <- predict(lm(vary ~ varx), predict.seq,interval = "confidence", level = IC.conf.value)
            if (boxes == 2 | boxes == 3) 
            boxplot(vary, axes = F)
            if (boxes == 0 | boxes == 1) 
            plot(0, type = "n", axes = F, xlab = "", ylab = "")
            plot(c(0, 1), c(0, 1), type = "n", axes = F, ylab = "", xlab = "")
            if (listvar.left[n.row] != listvar.right[1]) {
            plot(varx, vary, axes = F, type="n")
			points(varx, vary,pch = pch,cex=pointsize, col = col[1])
            abline(lm(vary ~ varx), col = col[1])
                if (IC.confidence) {
                lines(predict.seq$varx, pred.w.clim[, 2], lty = 2,col = col[1], lwd = 2)
                lines(predict.seq$varx, pred.w.clim[, 3], lty = 2, col = col[1], lwd = 2)
                }
                if (IC.predict) {
                lines(predict.seq$varx, pred.w.plim[, 2], lty = 3,col = col[1], lwd = 2)
                lines(predict.seq$varx, pred.w.plim[, 3], lty = 3, col = col[1], lwd = 2)
                }
            angles <- expand.grid(par()$usr[1:2], par()$usr[3:4])
            X <- matrix(c(varx, vary), ncol = 2)
            distances <- apply(angles, 1, function(w) min(rowSums(sweep(X,2, w, "-")^2)))
            position.leg <- position[max(distances) == distances]
            eval(parse(text = paste("legend('", position.leg,"','", coef.cor, "',title=expression(rho))", sep = "")))
                if (axes) {
                axis(2)
                axis(1)
                }
            }
            else {
                plot(varx, vary, type = "n", axes = F,ylab = "", xlab = "")
                if (axes) {
                axis(2)
                axis(1)
                }
            }
        box()
            if (n.col > 1) {
                for (j in 2:n.col) {
                valid <- complete.cases(with(data, eval(parse(text = listvar.right[j]))),with(data, eval(parse(text = listvar.left[n.row]))))
                varx <- with(data, eval(parse(text = listvar.left[n.row])))[valid]
                vary <- with(data, eval(parse(text = listvar.right[j])))[valid]
                coef.cor <- cor.test(varx, vary)
                coef.cor <- round(as.numeric(coef.cor[[4]]),digit = 3)
                predict.seq <- data.frame(varx = seq(min(varx),max(varx), length.out = length(varx)))
                pred.w.plim <- predict(lm(vary ~ varx), predict.seq, interval = "prediction", level = IC.pred.value)
                pred.w.clim <- predict(lm(vary ~ varx), predict.seq, interval = "confidence", level = IC.conf.value)
					if (listvar.left[n.row] != listvar.right[j]) {
                    plot(varx, vary, axes = F, type="n")
					points(varx, vary,pch = pch,cex=pointsize, col = col[1])
                    abline(lm(vary ~ varx), col = col[1])
						if (IC.confidence) {
						lines(predict.seq$varx, pred.w.clim[, 2],lty = 2, col = col[1], lwd = 2)
						lines(predict.seq$varx, pred.w.clim[, 3], lty = 2, col = col[1], lwd = 2)
						}
						if (IC.predict) {
						lines(predict.seq$varx, pred.w.plim[, 2],lty = 3, col = col[1], lwd = 2)
						lines(predict.seq$varx, pred.w.plim[, 3], lty = 3, col = col[1], lwd = 2)
						}
                    angles <- expand.grid(par()$usr[1:2], par()$usr[3:4])
                    X <- matrix(c(varx, vary), ncol = 2)
                    distances <- apply(angles, 1, function(w) min(rowSums(sweep(X,2, w, "-")^2)))
                    position.leg <- position[max(distances) == distances]
                    eval(parse(text = paste("legend('",position.leg, "','", coef.cor, "',title=expression(rho))", sep = "")))
						if (axes) 
						axis(1)
					}
					else {
                    plot(varx, vary, type = "n", axes = F,ylab = "", xlab = "")
						if (axes) 
						axis(1)
					}
                box()
                }
            }
        plot(c(0, 1), c(0, 1), type = "n", axes = F,ylab = "", xlab = "")
        mtext(listvar.left[n.row], 2, line = -1.3)
            for (i in 1:(n.col + 5)) plot(c(0, 1), c(0, 1), type = "n",axes = F, ylab = "", xlab = "")
            for (j in 1:n.col) {
            valid <- complete.cases(with(data, eval(parse(text = listvar.right[j]))),with(data,eval(parse(text = listvar.left[n.row]))))
            varx <- with(data, eval(parse(text = listvar.left[n.row])))[valid]
                if (boxes == 1 | boxes == 3) 
                boxplot(varx, axes = FALSE, horizontal = TRUE)
                if (boxes == 0 | boxes == 2) 
                plot(0, type = "n", axes = FALSE, xlab = "",ylab = "")
            }
        }
    }
	# Avec un groupe
    if (ex$length.covariate!=0) {
	#simple
        if (n.row == 1 & n.col == 1) {
        layout(matrix(c(1, 1, 1, 2, 6, 5, 3, 4, 5), 3, 3,byrow = TRUE), c(.7, 8, 1.3), c(1.3, 8, .7), TRUE)
        par(mar = c(0, 0, 0, 0))
        plot(c(0, 10), c(0, 10), type = "n", axe = FALSE)
        varx <- listvar.right
        vary <- listvar.left
            if (length(xlab) == 0) 
            xlab <- paste(listvar.right)
			if (length(ylab) == 0) 
            ylab <- paste(listvar.left)
        factor <- ex$list.covariate
		if (is.null(legend.lab)) legend.lab<-factor
        valid <- complete.cases(with(data, eval(parse(text = varx))),with(data, eval(parse(text = vary))), with(data,eval(parse(text = factor))))
        varx <- with(data, eval(parse(text = varx)))[valid]
        vary <- with(data, eval(parse(text = vary)))[valid]
        factor <- factor(with(data, eval(parse(text = factor)))[valid])
        levs <- levels(factor)
        n.levs <- length(levs)
			if(is.null(pch))
			pch <- 1:n.levs
			else {
			if (length(pch)<n.levs) pch<-c(pch,1:n.levs)
			}		
        text(5, 5, main, cex = 1.5)
        par(mar = c(5, 0, 0, 0))
            if (boxes == 2 | boxes == 3) 
            boxplot(vary, axes = F)
            if (boxes == 0 | boxes == 1) 
            plot(0, type = "n", axes = F, xlab = "",ylab = "")
        par(mar = c(0, 0, 0, 0))
        plot(c(0, 10), c(0, 10), type = "n", axe = F)
        par(mar = c(0, 5, 0, 0))
			if (boxes == 1 | boxes == 3) 
            boxplot(varx, axes = FALSE, horizontal = TRUE)
            if (boxes == 0 | boxes == 2) 
            plot(0, type = "n", axes = FALSE, xlab = "",ylab = "")
        par(mar = c(0, 0, 0, 0))
        plot(c(0, 10), c(0, 10), type = "n", axe = FALSE)
        coef.cor <- NULL
        i <- 1
            for (variable in levs) {
            coef.cor[i] <- round(as.numeric(cor.test(with(data, subset(varx, factor == variable)), with(data,subset(vary, factor == variable)))[[4]]), digit = 3)
            i = i + 1
            }
        legend(0, 10, coef.cor, pch = pch, col = col, lwd = 1, title = expression(rho), merge = TRUE)
        legend(0, 5, levs, pch = pch, col = col, lwd = 1, title = legend.lab, merge = TRUE)
        par(mar = c(5, 5, 0, 0))
        i <- 1
        plot(varx, vary, xlab = xlab, ylab = ylab, type = "n")
            for (variable in levs) {
            points(subset(varx, factor == variable), subset(vary, factor == variable), col = col[i], pch = pch[i])
            abline(lm(subset(vary, factor == variable) ~ subset(varx, factor == variable)), col = col[i])
            varxi <- subset(varx, factor == variable)
            varyi <- subset(vary, factor == variable)
            predict.seq <- data.frame(varxi = seq(min(varxi),max(varxi), length.out = length(varxi)))
				if (IC.confidence) {
                pred.w.clim <- predict(lm(varyi ~ varxi), predict.seq,interval = "confidence", level = IC.conf.value)
                lines(predict.seq$varx, pred.w.clim[, 2], lty = 2, col = col[i], lwd = 2)
                lines(predict.seq$varx, pred.w.clim[, 3], lty = 2, col = col[i], lwd = 2)
                }
                if (IC.predict) {
                pred.w.plim <- predict(lm(varyi ~ varxi), predict.seq, interval = "prediction", level = IC.pred.value)
                lines(predict.seq$varx, pred.w.plim[, 2], lty = 3, col = col[i], lwd = 2)
                lines(predict.seq$varx, pred.w.plim[, 3], lty = 3, col = col[i], lwd = 2)
                }
            i = i + 1
            }
        }
        #multiple
		else {
        layout(matrix(c(rep(1, n.col + 3), seq(2, (n.row + 3) * (n.col + 3) + 1)), n.row + 4, n.col + 3, byrow = TRUE), c(4, 4, rep(28, n.col), 4), c(width.title,4, rep(28, n.row), 4, 4), TRUE)
        factor <- ex$list.covariate
        fact <- factor(with(data, eval(parse(text = factor))))
        levs <- levels(fact)
        n.levs <- length(levs)
			if(is.null(pch))
			pch <- 1:n.levs
			else {
			if (length(pch)<n.levs) pch<-c(pch,1:n.levs)
			}			
        position <- c("bottomleft", "bottomright","topleft", "topright")
        par(mar = c(0, 0, 0, 0))
        plot(c(0, 1), c(0, 1), type = "n", axes = F)
        dim.leg <- legend("topleft", levs, pch = pch, title = paste(main, "by", factor), col = col, lwd = 1, ncol = length(levs), plot = FALSE, merge = TRUE)
        legend(0.5 + dim.leg$rect$left - dim.leg$rect$w/2, dim.leg$rect$top, levs, pch = pch, col = col,lwd = 1, title = paste(main, "by", factor), bty = "n", ncol = length(levs), merge = TRUE)
			for (i in 1:2) plot(c(0, 1), c(0, 1), type = "n", axes = F)
            for (i in 1:(n.col)) {
            plot(c(0, 1), c(0, 1), type = "n", axes = F)
            mtext(listvar.right[i], 1, line = -1.2)
            }
            plot(c(0, 1), c(0, 1), type = "n", axes = F)
            if (n.row > 1) {
                for (i in 1:(n.row - 1)) {
                valid <- complete.cases(with(data, eval(parse(text = listvar.right[1]))),with(data, eval(parse(text = listvar.left[i]))),with(data, eval(parse(text = factor))))
                varx <- with(data, eval(parse(text = listvar.left[i])))[valid]
                vary <- with(data, eval(parse(text = listvar.right[1])))[valid]
                fact <- factor(with(data, eval(parse(text = factor)))[valid])
                levs <- levels(fact)
                n.levs <- length(levs)
					if (boxes == 2 | boxes == 3) 
                    boxplot(vary, axes = F)
					if (boxes == 0 | boxes == 1) 
                    plot(0, type = "n", axes = F, xlab = "",ylab = "")
                plot(c(0, 1), c(0, 1), type = "n", axes = F, ylab = "", xlab = "")
                coef.cor <- NULL
                k <- 1
					if (listvar.left[i] != listvar.right[1]) {
                    plot(varx, vary, axes = F, type = "n")
					angles <- expand.grid(par()$usr[1:2], par()$usr[3:4])
					X <- matrix(c(varx, vary), ncol = 2)
					distances <- apply(angles, 1, function(w) min(rowSums(sweep(X,2, w, "-")^2)))
					position.leg <- position[max(distances) == distances]
						for (variable in levs) {
						coef.cor[k] <- round(as.numeric(cor.test(with(data,subset(varx, fact == variable)), with(data,subset(vary, fact == variable)))[[4]]), digit = 3)
						points(with(data, subset(varx, fact == variable)), with(data, subset(vary, fact == variable)), col = col[k], pch = pch[k])
						abline(lm(subset(vary, fact == variable) ~ subset(varx, fact == variable)), col = col[k])
						varxi <- subset(varx, fact == variable)
						varyi <- subset(vary, fact == variable)
						predict.seq <- data.frame(varxi = seq(min(varxi),max(varxi), length.out = length(varxi)))
							if (IC.confidence) {
							pred.w.clim <- predict(lm(varyi ~ varxi),predict.seq, interval = "confidence", level = IC.conf.value)
							lines(predict.seq$varx, pred.w.clim[,2], lty = 2, col = col[k], lwd = 2)
							lines(predict.seq$varx, pred.w.clim[,3], lty = 2, col = col[k], lwd = 2)
							}
						if (IC.predict) {
                        pred.w.plim <- predict(lm(varyi ~ varxi),predict.seq, interval = "prediction", level = IC.pred.value)
                        lines(predict.seq$varx, pred.w.plim[,2], lty = 3, col = col[k], lwd = 2)
                        lines(predict.seq$varx, pred.w.plim[, 3], lty = 3, col = col[k], lwd = 2)
						}
                    k = k + 1
                    }
                eval(parse(text = paste("legend('",position.leg, "',c(", paste("'", coef.cor, "'", collapse = ",",sep = ""), "),pch= pch,col=col,lwd=1,title=expression(rho),merge=TRUE)", sep = "")))
                    if (axes) 
                    axis(2)
					}
					else {
                    plot(varx, vary, type = "n", axes = F,ylab = "", xlab = "")
						if (axes) 
						axis(2)
					}
                box()
					if (n.col > 1) {
						for (j in 2:n.col) {
						valid <- complete.cases(with(data, eval(parse(text = listvar.right[j]))),with(data, eval(parse(text = listvar.left[i]))), with(data, eval(parse(text = factor))))
						varx <- with(data, eval(parse(text = listvar.left[i])))[valid]
						vary <- with(data, eval(parse(text = listvar.right[j])))[valid]
						fact <- factor(with(data, eval(parse(text = factor)))[valid])
						levs <- levels(fact)
						n.levs <- length(levs)
						coef.cor <- NULL
						k <- 1
							if (listvar.left[i] != listvar.right[j]) {
							plot(varx, vary, axes = F, type = "n")
							angles <- expand.grid(par()$usr[1:2],par()$usr[3:4])
							X <- matrix(c(varx, vary), ncol = 2)
							distances <- apply(angles, 1, function(w) min(rowSums(sweep(X, 2, w, "-")^2)))
							position.leg <- position[max(distances) ==distances]
								for (variable in levs) {
								coef.cor[k] <- round(as.numeric(cor.test(with(data, subset(varx, fact == variable)), with(data, subset(vary, fact == variable)))[[4]]),digit = 3)
								points(with(data, subset(varx, fact ==variable)), with(data, subset(vary, fact == variable)), col = col[k], pch = pch[k])
								abline(lm(subset(vary, fact == variable) ~ subset(varx, fact == variable)),col = col[k])
								k = k + 1
								}
							eval(parse(text = paste("legend('",position.leg, "',c(", paste("'",coef.cor, "'", collapse = ",",sep = ""), "),pch= pch,col=col,lwd=1,title=expression(rho),merge=TRUE)",sep = "")))
							}
							else {
							plot(c(0, 1), c(0, 1), type = "n",axes = F, ylab = "", xlab = "")
							}
						box()
						}
					}
                plot(c(0, 1), c(0, 1), type = "n", axes = F,ylab = "", xlab = "")
                mtext(listvar.left[i], 2, line = -1.3)
                }
            }
        valid <- complete.cases(with(data, eval(parse(text = listvar.right[1]))), with(data, eval(parse(text = listvar.left[n.row]))), with(data, eval(parse(text = factor))))
        varx <- with(data, eval(parse(text = listvar.left[n.row])))[valid]
        vary <- with(data, eval(parse(text = listvar.right[1])))[valid]
        fact <- factor(with(data, eval(parse(text = factor)))[valid])
        levs <- levels(fact)
        n.levs <- length(levs)
            if (boxes == 2 | boxes == 3) 
            boxplot(vary, axes = F)
			if (boxes == 0 | boxes == 1) 
            plot(0, type = "n", axes = F, xlab = "",ylab = "")
        plot(c(0, 1), c(0, 1), type = "n", axes = F,ylab = "", xlab = "")
        coef.cor <- NULL
        k <- 1
            if (listvar.left[n.row] != listvar.right[1]) {
            plot(varx, vary, axes = F, type = "n")
            angles <- expand.grid(par()$usr[1:2], par()$usr[3:4])
            X <- matrix(c(varx, vary), ncol = 2)
            distances <- apply(angles, 1, function(w) min(rowSums(sweep(X, 2, w, "-")^2)))
            position.leg <- position[max(distances) == distances]
                for (variable in levs) {
                coef.cor[k] <- round(as.numeric(cor.test(with(data,subset(varx, fact == variable)), with(data, subset(vary, fact == variable)))[[4]]), digit = 3)
                points(with(data, subset(varx, fact == variable)), with(data, subset(vary, fact == variable)), col = col[k], pch = pch[k])
                abline(lm(subset(vary, fact == variable) ~ subset(varx, fact == variable)), col = col[k])
                varxi <- subset(varx, fact == variable)
                varyi <- subset(vary, fact == variable)
                predict.seq <- data.frame(varxi = seq(min(varxi), max(varxi), length.out = length(varxi)))
					if (IC.confidence) {
                    pred.w.clim <- predict(lm(varyi ~ varxi),predict.seq, interval = "confidence",level = IC.conf.value)
                    lines(predict.seq$varx, pred.w.clim[, 2],lty = 2, col = col[k], lwd = 2)
                    lines(predict.seq$varx, pred.w.clim[, 3],lty = 2, col = col[k], lwd = 2)
					}
					if (IC.predict) {
                    pred.w.plim <- predict(lm(varyi ~ varxi),predict.seq, interval = "prediction",level = IC.pred.value)
                    lines(predict.seq$varx, pred.w.plim[, 2],lty = 3, col = col[k], lwd = 2)
                    lines(predict.seq$varx, pred.w.plim[, 3],lty = 3, col = col[k], lwd = 2)
					}
                k = k + 1
                }
            eval(parse(text = paste("legend('", position.leg,"',c(", paste("'", coef.cor, "'",collapse = ",", sep = ""), "),pch= pch,col=col,lwd=1,title=expression(rho),merge=TRUE)", sep = "")))
                if (axes) {
                axis(2)
                axis(1)
                }
            }
            else {
            plot(varx, vary, type = "n", axes = F,ylab = "", xlab = "")
                if (axes) {
                axis(2)
                axis(1)
                }
            }
        box()
            if (n.col > 1) {
                for (j in 2:n.col) {
                valid <- complete.cases(with(data, eval(parse(text = listvar.right[j]))),with(data, eval(parse(text = listvar.left[n.row]))),with(data, eval(parse(text = factor))))
                varx <- with(data, eval(parse(text = listvar.left[n.row])))[valid]
                vary <- with(data, eval(parse(text = listvar.right[j])))[valid]
                fact <- factor(with(data, eval(parse(text = factor)))[valid])
                levs <- levels(fact)
                n.levs <- length(levs)
                coef.cor <- NULL
                k <- 1
					if (listvar.left[n.row] != listvar.right[j]) {
                    plot(varx, vary, axes = F, type = "n")
                    angles <- expand.grid(par()$usr[1:2], par()$usr[3:4])
                    X <- matrix(c(varx, vary), ncol = 2)
                    distances <- apply(angles, 1, function(w) min(rowSums(sweep(X,2, w, "-")^2)))
                    position.leg <- position[max(distances) == distances]
						for (variable in levs) {
						coef.cor[k] <- round(as.numeric(cor.test(with(data,subset(varx, fact == variable)), with(data,subset(vary, fact == variable)))[[4]]),digit = 3)
						points(with(data, subset(varx, fact == variable)), with(data, subset(vary, fact ==variable)), col = col[k], pch = pch[k])
						abline(lm(subset(vary, fact == variable) ~ subset(varx, fact == variable)), col = col[k])
						varxi <- subset(varx, fact == variable)
						varyi <- subset(vary, fact == variable)
						predict.seq <- data.frame(varxi = seq(min(varxi),max(varxi), length.out = length(varxi)))
							if (IC.confidence) {
							pred.w.clim <- predict(lm(varyi ~ varxi),predict.seq, interval = "confidence",level = IC.conf.value)
							lines(predict.seq$varxi, pred.w.clim[,2], lty = 2, col = col[k], lwd = 2)
							lines(predict.seq$varxi, pred.w.clim[,3], lty = 2, col = col[k], lwd = 2)
							}
							if (IC.predict) {
							pred.w.plim <- predict(lm(varyi ~ varxi),predict.seq, interval = "prediction", level = IC.pred.value)
							lines(predict.seq$varxi, pred.w.plim[,2], lty = 3, col = col[k], lwd = 2)
							lines(predict.seq$varxi, pred.w.plim[,3], lty = 3, col = col[k], lwd = 2)
							}
						k = k + 1
						}
                    eval(parse(text=paste("legend('",position.leg,"',c(",paste("'",coef.cor,"'",collapse=",",sep = ""), "),pch= pch,col=col,lwd=1,title=expression(rho),merge=TRUE)", sep = "")))
						if (axes) 
						axis(1)
					}
					else {
                    plot(varx, vary, type = "n", axes = F,ylab = "", xlab = "")
                    axis(1)
					}
                box()
                }
            }
        plot(c(0, 1), c(0, 1), type = "n", axes = F,ylab = "", xlab = "")
        mtext(listvar.left[n.row], 2, line = -1.3)
            for (i in 1:(n.col + 5)) plot(c(0, 1), c(0, 1), type = "n",axes = F, ylab = "", xlab = "")
            for (j in 1:n.col) {
            valid <- complete.cases(with(data, eval(parse(text = listvar.right[j]))),with(data, eval(parse(text = listvar.left[n.row]))))
            varx <- with(data, eval(parse(text = listvar.left[n.row])))[valid]
                if (boxes == 1 | boxes == 3) 
                boxplot(varx, axes = FALSE, horizontal = TRUE)
				if (boxes == 0 | boxes == 2) 
                plot(0, type = "n", axes = FALSE, xlab = "",ylab = "")
            }
        }
    }
par(op)
invisible(NULL)
}
#############################################################################
# Histogramme des frequences
graphBar<-function (formula,nbre=NULL,strata=FALSE,name.repeated=NULL,data=parent.frame(),xlab = NULL,ylab=NULL,dim=c("2D","3D"),cex.names=1,xlab.axis = 0,legend.lab = NULL,
legend.pos="topright",cex.legend=1, columns = c("juxtaposed","stacked"),frequency=c("count","percent"),col = palette(),main=NULL)
{
require(stringr)
op <- par(no.readonly = TRUE)
columns <- match.arg(columns)
frequency <- match.arg(frequency)
dim <- match.arg(dim)
if(dim=="2D") dim<-NULL
if(columns=="juxtaposed") text.column<-",beside=TRUE"
if(columns=="stacked") text.column<-",beside=FALSE"
#extraction des parametres
eval(parse(text=paste0("ex<-Extract.fact(",paste(deparse(substitute(formula)),collapse=""),",data=",deparse(substitute(data)),")")))	
#Graphiques
	#Sans groupe
	if (ex$length.factor==0) {
		#Simple
		if (ex$length.response==1) {
		tab<-xtabs(~with(data,eval(parse(text=ex$list.response))))
			if (!is.null(nbre))
			tab<-xtabs(nbre~with(data,eval(parse(text=ex$list.response)))) 
		n.levs<-length(rownames(tab))
		pourcentage <- apply(tab, 1,function(x) x/sum(tab))
			if(is.null(ylab))
			ylab<-str_to_title(frequency)
			if(is.null(xlab))
			xlab <- ex$list.response
			if(is.null(main))
			main<-paste(xlab,"Distribution")
			if (frequency=="percent")
			tab<-pourcentage
			if(columns=="juxtaposed")
			eval(parse(text=paste0("barplot",dim,"(tab,space=0,xlab='",xlab,"'",",main='",main,"',ylab='",ylab,"',col=c(",paste0("'",col[1:n.levs],"'",collapse=","),"))")))
			if(columns=="stacked"){
			tab<-cbind(tab,rep(0,n.levs))
			colnames(tab)<-c("","")
			xlab <-""
			col.level<-paste0("c(",paste0("'",col[1:n.levs],"'",collapse=","),")")
			eval(parse(text=paste0("barplot",dim,"(tab,space=0,xlab='",xlab,"'",",main='",main,"',ylab='",ylab,"',col=",col.level,")")))
			axis1<-par("usr")[2]-par("usr")[1]
			axis2<-par("usr")[4]-par("usr")[3]
			scalex<-axis2/axis1
				if(is.null(legend.lab))
				legend.lab<-ex$list.response
			legend(legend.pos, rownames(tab),fill = col[1:n.levs], cex = cex.legend, bty = "n", title = legend.lab)	
			polygon(c(1,1+0.2,2+0.2,2), c(0,0.2*scalex*tan(37/180*pi),0.2*scalex*tan(37/180*pi),0), col = "white", border = "white")
			}
		}
		#Repetee
		if (ex$length.response) {
		#Sans strate
			if(!strata){
			eval(parse(text=paste0("tab",1:ex$length.response,"<-xtabs(~data$",ex$list.response,")")))
			eval(parse(text=paste0("tab.test",1:ex$length.response,"<-paste0('",ex$list.response,"',names(tab",1:ex$length.response,"))")))
			eval(parse(text=paste0("df<-data.frame(fact=c(",paste0("names(tab",1:ex$length.response,")",collapse=","),"))")))
			eval(parse(text=paste0("df.test<-data.frame(fact=c(",paste0("tab.test",1:ex$length.response,collapse=","),"))")))
			eval(parse(text=paste0("lev",1:ex$length.response,"<-names(tab",1:ex$length.response,")")))
			eval(parse(text=paste0("df$",ex$list.response,"<-0")))
			eval(parse(text=paste0("df.test$",ex$list.response,"<-0")))
				for (i in 1:ex$length.response){
				test.fact<-NULL
					for (j in 1:length(df.test$fact)){
					eval(parse(text=paste0("test<-tab.test",i)))
						if (sum(df.test$fact[j]==test)>0) test.fact<-c(test.fact,TRUE)
						else test.fact<-c(test.fact,FALSE)
					}
				eval(parse(text=paste0("df.test$",ex$list.response[i],"[c(",paste0(test.fact,collapse=","),")]<-tab",i)))
				}
			df[,-1]<-df.test[,-1]
			tab<-as.matrix(df[2:(length(ex$list.response)+1)])
			rownames(tab)<-df$fact
			n.levs.1<-length(rownames(tab))
			n.levs.2<-length(colnames(tab))
			tabp<-aperm(tab)
			sums <- apply(tabp, 1, sum)
			per <- apply(tabp, 2, function(x) x/sums)
			pourcentage<-aperm(per)
				if (frequency=="percent")
				tab<-pourcentage
				if(is.null(ylab))
				ylab<-str_to_title(frequency)
				if(is.null(legend.lab))
				legend.lab<-ex$list.response
				if(is.null(main))
				main<-paste(paste(ex$list.response,collapse=" and "),"Distribution")
			tab<-cbind(tab,rep(0,n.levs.1))
			col.level<-c(rep(col[1:n.levs.1],n.levs.2),rep("white",n.levs.1))
			col.legend<-col.level[1:length(rownames(tab))]
			bord.level<-c(rep(rep("black",n.levs.1),n.levs.2),rep("white",n.levs.1))
			col.side<-c(rep(rep("white",n.levs.1),n.levs.2),rep("white",n.levs.1))
			bord.side<-c(rep(rep("white",n.levs.1),n.levs.2),rep("white",n.levs.1))
			indice<-1
			offset<-0
				for (i in 1:ex$length.response){
				eval(parse(text=paste0("length.indice<-length(names(tab",i,"))")))
				col.side[(indice+offset):(indice+offset+length.indice-1)]<-col.level[(indice+offset):(indice+offset+length.indice-1)]
				bord.side[(indice+offset):(indice+offset+length.indice-1)]<-rep("black",length.indice)
				indice<-indice+length(rownames(tab))
				offset<-offset+length.indice
				}
				#juxtaposee
				if (columns=="juxtaposed"){
					if (is.null(dim)){
						if (xlab.axis==0)
						barplot(tab, beside = TRUE, col = col.side,border=bord.side, ylab = ylab, xlab = xlab, cex.names = cex.names)
						else{
						mp <-barplot(tab, beside = TRUE, col = col.level,border=bord.level,main=main, ylab = ylab, axisnames = FALSE)
						pos.x<-mp[1,1:n.levs.2]+.5
						text(pos.x, par("usr")[3], labels = ex$list.response, srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.names)
						pos.xlab<-(n.levs.1*(n.levs.2+1)+n.levs.2)/2
						mtext(xlab,1,at=pos.xlab,line=4)									
						}
					ymax<-par("usr")[4]
					x.pos<-length(colnames(tab))*length(rownames(tab))-1
					indice<-1
					leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+length(names(tab1))-1)],fill=col.legend[indice:(indice+length(names(tab1))-1)],title=ex$list.response[1])
					indice<-indice+length(names(tab1))
					ymax<-ymax-leg$rect$h
						for (i in 2:ex$length.response){
						eval(parse(text=paste0("offset<-length(names(tab",i,"))")))
						leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+offset-1)],fill=col.legend[indice:(indice+offset-1)],title=ex$list.response[i])
						indice<-indice+length(names(tab1))
						ymax<-ymax-leg$rect$h
						}			
					}
					else if (dim=="3D"){
						if (xlab.axis==0)
						mp<-barplot3D(tab, beside = TRUE, col = col.side,border=bord.side, ylab = ylab, xlab = xlab, cex.names = cex.names)
						else{
						mp <-barplot3D(tab, beside = TRUE, col = col.level,border=bord.level,main=main, ylab = ylab, axisnames = FALSE)
						pos.x<-mp$bar.draw[,4]
						pos.x<-pos.x[seq(1,length(pos.x),n.levs.1)]
						pos.x<-pos.x[1:n.levs.2]
						text(pos.x, par("usr")[3], labels = ex$list.response, srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.names)
						pos.xlab<-(n.levs.1*(n.levs.2+1)+n.levs.2)/2
						mtext(xlab,1,at=pos.xlab,line=4)					
						}
					bar.draw<-mp$bar.draw
					loz.upper<-mp$loz.upper
					loz.side<-mp$loz.side
					polygon(loz.side[,1], loz.side[,2], col = col.side, border =bord.side)
					polygon(loz.upper[,1], loz.upper[,2], col = col.side, border =bord.side)
					rect(bar.draw[,2], bar.draw[,1], bar.draw[,4], bar.draw[,3],  col = col.side, border =bord.side)
					indice<-1
					offset<-0
					line.side<-NULL
						for (i in 1:ex$length.response){
						eval(parse(text=paste0("length.indice<-length(names(tab",i,"))")))
						line.side<-rbind(line.side,loz.side[(indice+offset+(length.indice-1)*5):(indice+offset+(length.indice-1)*5+1),])
						line.side<-rbind(line.side,c(NA,NA))
						indice<-indice+length(rownames(tab))*5
						offset<-offset+length.indice*5
						}
					lines(line.side[,1],line.side[,2],col="black")
					ymax<-par("usr")[4]
					x.pos<-length(colnames(tab))*length(rownames(tab))-0.2
					indice<-1
					leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+length(names(tab1))-1)],fill=col.legend[indice:(indice+length(names(tab1))-1)],title=ex$list.response[1])
					indice<-indice+length(names(tab1))
					ymax<-ymax-leg$rect$h
						for (i in 2:length(ex$list.response)){
						eval(parse(text=paste0("offset<-length(names(tab",i,"))")))
						leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+offset-1)],fill=col.legend[indice:(indice+offset-1)],title=ex$list.response[i])
						indice<-indice+length(names(tab1))
						ymax<-ymax-leg$rect$h
						}
					}
				}
				#empilee
				if (columns=="stacked") {
					if (is.null(dim)){
						if (xlab.axis==0)
						barplot(tab, beside = FALSE, col = col.level,border=bord.level, ylab = ylab, xlab = xlab, cex.names = cex.names)
						else{
						mp <-barplot(tab, beside = FALSE,space=0.1, col = col.level,border=bord.level,main=main, ylab = ylab, axisnames = FALSE)
						pos.x<-mp[1:n.levs.2]
						text(pos.x, par("usr")[3], labels = ex$list.response, srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.names)
						pos.xlab<-(n.levs.2+1)/2
						mtext(xlab,1,at=pos.xlab,line=4)					
						}
						ymax<-par("usr")[4]
						x.pos<-length(colnames(tab))-0.2
						indice<-1
						leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+length(names(tab1))-1)],fill=col.legend[indice:(indice+length(names(tab1))-1)],title=ex$list.response[1])
						indice<-indice+length(names(tab1))
						ymax<-ymax-leg$rect$h
							for (i in 2:ex$length.response){
							eval(parse(text=paste0("offset<-length(names(tab",i,"))")))
							leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+offset-1)],fill=col.legend[indice:(indice+offset-1)],title=ex$list.response[i])
							indice<-indice+length(names(tab1))
							ymax<-ymax-leg$rect$h
							}
						axis1<-par("usr")[2]-par("usr")[1]
						axis2<-par("usr")[4]-par("usr")[3]
						scalex<-axis2/axis1	
						offset<-0
						for (i in 1:ex$length.response){
						eval(parse(text=paste0("y.coord<-sum(tab",i,")")))
						polygon(c(i-0.8+offset,i-0.6+offset,i+0.4+offset,i+0.2+offset), c(y.coord,y.coord+0.2*scalex*tan(37/180*pi),y.coord+0.2*scalex*tan(37/180*pi),y.coord), border = "black")
						offset<-offset+0.2
						}
					polygon(c(length(colnames(tab))-0.8+offset,length(colnames(tab))-0.6+offset,length(colnames(tab))+0.4+offset,length(colnames(tab))+0.2+offset), c(0,0.2*scalex*tan(37/180*pi),0.2*scalex*tan(37/180*pi),0), col = "white", border = "white")									
					}
					else if (dim=="3D"){
					mp <-barplot(tab, beside = FALSE,space=0.1, col = col.level,border=bord.level,main=main, ylab = ylab, axisnames = FALSE)
						#abscisse inclinee
						if (xlab.axis==0)
						barplot3D(tab, beside = FALSE, col = col.level,border=bord.level, ylab = ylab, xlab = xlab, cex.names = cex.names)
						else {
						barplot3D(tab, beside = FALSE,space=0.1, col = col.level,border=bord.level,main=main, ylab = ylab, xlab = xlab, axisnames = FALSE)
						pos.x<-mp[1:n.levs.2]
						text(pos.x, par("usr")[3], labels = ex$list.response, srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.names)
						pos.xlab<-(n.levs.2+1)/2
						mtext(xlab,1,at=pos.xlab,line=4)					
						}
					#affichage de la legende						
					ymax<-par("usr")[4]
					x.pos<-length(colnames(tab))
					indice<-1
					leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+length(names(tab1))-1)],fill=col.legend[indice:(indice+length(names(tab1))-1)],title=ex$list.response[1])
					indice<-indice+length(names(tab1))
					last.col<-indice-1
					ymax<-ymax-leg$rect$h
						for (i in 2:ex$length.response){
						eval(parse(text=paste0("offset<-length(names(tab",i,"))")))
						last.col<-c(last.col,indice+offset-1)
						leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+offset-1)],fill=col.legend[indice:(indice+offset-1)],title=ex$list.response[i])
						indice<-indice+length(names(tab1))
						ymax<-ymax-leg$rect$h
						}	
					axis1<-par("usr")[2]-par("usr")[1]
					axis2<-par("usr")[4]-par("usr")[3]
					scalex<-axis2/axis1	
					init<-mp[length(mp)]+0.1*ex$length.response-0.4
					polygon(c(init,init+0.2,init+1.2,init+1), c(0,0.2*scalex*tan(37/180*pi),0.2*scalex*tan(37/180*pi),0), col = "white", border = "white")				
					}
				}			
			}
			#avec strate
			else{
			eval(parse(text=paste0("tab",1:ex$length.response,"<-xtabs(~data$",ex$list.response,")")))
				if (!missing(nbre))
				eval(parse(text=paste("tab",1:ex$length.response,"<-xtabs(",nbre,"[",1:ex$length.response,"]~",ex$list.response,", data=", deparse(substitute(data)),",drop.unused.levels = TRUE)", sep="")))
			n.row<-ex$length.response
				if (is.null(name.repeated))
				name.repeated<-"Variable"
				if(is.null(main))
				main<-paste(name.repeated,"Distribution")
				if(is.null(ylab))
				ylab<-str_to_title(frequency)		
			layout(matrix(c(1,seq(2,n.row+1)),n.row+1,1,byrow=TRUE),widths=28*n.row+4,heights=c(4,rep(28,n.row)),TRUE)
			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
			eval(parse(text=paste0("text(.5,.5,bquote(bold('",main,"')),cex=1.5)")))
			par(mar=c(4,4,0,2))				
				for (i in 1:ex$length.response){
				eval(parse(text=paste0("tab<-tab",i)))
				n.levs<-length(rownames(tab))
				levs.1<-rownames(tab)
				pourcentage <- apply(tab, 1,function(x) x/sum(tab))
					if (frequency=="percent")
					tab<-pourcentage
					if(is.null(legend.lab))
					legend.lab<-name.repeated
				tab.stacked<-cbind(tab,0)
				tab<-c(tab,0)
					if(columns=="juxtaposed"){
						if (is.null(dim))
						barplot(tab, beside = TRUE, col = c(col[1:n.levs],"white"),border=c(rep("black",n.levs),"white"), ylab = ylab, cex.names = cex.names)
						else if (dim=="3D")
						barplot3D(tab, beside = TRUE, col = c(col[1:n.levs],"white"),border=c(rep("black",n.levs),"white"), ylab = ylab,  cex.names = cex.names)
					x.pos<-3
					pos.xlab<-(par("usr")[2]-par("usr")[1])/2+par("usr")[1]
					mtext(ex$list.response[i],1,at=pos.xlab,line=2.5,cex=0.8)	
					}
					if (columns=="stacked"){
					colnames(tab.stacked)<-c(ex$list.response[i],"")
						if (is.null(dim))
						barplot(tab.stacked, beside = FALSE, col = c(col[1:n.levs],"white"),border=c(rep("black",n.levs),"white"), ylab = ylab, cex.names = cex.names)
						else if (dim=="3D")
						barplot3D(tab.stacked, beside = FALSE, col = c(col[1:n.levs],"white"),border=c(rep("black",n.levs),"white"), ylab = ylab, cex.names = cex.names)
					axis1<-par("usr")[2]-par("usr")[1]
					axis2<-par("usr")[4]-par("usr")[3]
					scalex<-axis2/axis1	
					polygon(c(1.4,1.6,2.6,2.4), c(0,0.2*scalex*tan(37/180*pi),0.2*scalex*tan(37/180*pi),0), col = "white", border = "white")	
					x.pos<-2
					}
				legend(x.pos,par("usr")[4], levs.1,fill = col[1:n.levs], cex = cex.legend*1.1, bty = "n", title = legend.lab)		
				}				
			}
		}
	}
	#Avec un groupe
	if (ex$length.factor>0) {
	#avec un seul groupe
		if (ex$length.factor==1){
		#Simple
			if (ex$length.response==1) {
				#sans strate
				if(!strata){
				factor1 <- with(data,eval(parse(text=ex$list.factor)))
				factor2 <- with(data,eval(parse(text=ex$list.response)))
				tab<-xtabs(~factor1+factor2)
					if (!is.null(nbre))
					tab<-xtabs(nbre~factor1+factor2)
				n.levs.1<-length(rownames(tab))
				n.levs.2<-length(colnames(tab))
				tab1<-aperm(tab)
				sums <- apply(tab1, 1, sum)
				per1 <- apply(tab1, 2, function(x) x/sums)
				pourcentage<-aperm(per1)
					if (frequency=="percent")
					tab<-pourcentage
					if(is.null(ylab))
					ylab<-str_to_title(frequency)
					if(is.null(xlab))
					xlab <- ex$list.response
					if(is.null(legend.lab))
					legend.lab<-ex$list.factor
					if(is.null(main))
					main<-paste(legend.lab,"and",xlab,"Distribution")
				tab<-cbind(tab,rep(0,n.levs.1))
				col.level<-c(rep(col[1:n.levs.1],n.levs.2),rep("white",n.levs.1))
				bord.level<-c(rep(rep("black",n.levs.1),n.levs.2),rep("white",n.levs.1))
					#juxtaposee				
					if (columns=="juxtaposed"){
						if (is.null(dim)){
							if (xlab.axis==0)
							barplot(tab, beside = TRUE, col = col.level,border=bord.level,main=main, ylab = ylab, xlab = xlab, cex.names = cex.names)
							else{
							mp <-barplot(tab, beside = TRUE, col = col.level,border=bord.level,main=main, ylab = ylab, axisnames = FALSE)
							pos.x<-mp[1,1:n.levs.2]+.5
							text(pos.x, par("usr")[3], labels = levels(factor2), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.names)
							pos.xlab<-(n.levs.1*(n.levs.2+1)+n.levs.2)/2
							mtext(xlab,1,at=pos.xlab,line=4)							
							}
						}
						else if (dim=="3D"){
							if (xlab.axis==0)
							barplot3D(tab, beside = TRUE, col = col.level,border=bord.level,main=main, ylab = ylab, xlab = xlab, cex.names = cex.names)
							else {
							mp <-barplot3D(tab, beside = TRUE, col = col.level,border=bord.level,main=main, ylab = ylab, axisnames = FALSE)
							pos.x<-mp$bar.draw[,4]
							pos.x<-pos.x[seq(1,length(pos.x),n.levs.1)]
							pos.x<-pos.x[1:n.levs.2]
							text(pos.x, par("usr")[3], labels = levels(factor2), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.names)
							pos.xlab<-(n.levs.1*(n.levs.2+1)+n.levs.2)/2
							mtext(xlab,1,at=pos.xlab,line=4)
							}
						}
					}
					#empilee
					if (columns=="stacked") {
						if (is.null(dim)){
							if (xlab.axis==0)
							barplot(tab, beside = FALSE,space=0.1, col = col.level,border=bord.level,main=main, ylab = ylab, xlab = xlab, cex.names = cex.names)
							else {
							mp <-barplot(tab, beside = FALSE,space=0.1, col = col.level,border=bord.level,main=main, ylab = ylab, axisnames = FALSE)
							pos.x<-mp[1:n.levs.2]
							text(pos.x, par("usr")[3], labels = levels(factor2), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.names)
							pos.xlab<-(n.levs.2+1)/2
							mtext(xlab,1,at=pos.xlab,line=4)	
							}
						}
						else if (dim=="3D"){
							if (xlab.axis==0)
							barplot3D(tab, beside = FALSE,space=0.1, col = col.level,border=bord.level,main=main, ylab = ylab, xlab = xlab, cex.names = cex.names)
							else {
							mp <-barplot(tab, beside = FALSE,space=0.1, col = col.level,border=bord.level,main=main, ylab = ylab, axisnames = FALSE)
							barplot3D(tab, beside = FALSE,space=0.1, col = col.level,border=bord.level,main=main, ylab = ylab, xlab = xlab, axisnames = FALSE)
							pos.x<-mp[1:n.levs.2]
							text(pos.x, par("usr")[3], labels = levels(factor2), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.names)
							pos.xlab<-(n.levs.2+1)/2
							mtext(xlab,1,at=pos.xlab,line=4)
							}
						}
						
					axis1<-par("usr")[2]-par("usr")[1]
					axis2<-par("usr")[4]-par("usr")[3]
					scalex<-axis2/axis1
					polygon(c(0.1+n.levs.2+0.1*n.levs.2,n.levs.2+0.1*n.levs.2+0.3,n.levs.2+0.1*n.levs.2+1.4,n.levs.2+0.1*n.levs.2+1.1), c(0,0.2*scalex*tan(37/180*pi),0.2*scalex*tan(37/180*pi),0), col = "white", border = "white")				
					}
				legend(legend.pos,rownames(tab),cex=cex.legend,fill=col[1:n.levs.1],title=legend.lab)			
				}
			#Avec strate	
				else{
				factor1 <- with(data,eval(parse(text=ex$list.factor)))
				levs.1<-levels(factor1)
				factor2 <- with(data,eval(parse(text=ex$list.response)))
				levs.2<-levels(factor2)
				eval(parse(text=paste0("data",1:length(levs.1),"<-subset(",deparse(substitute(data)),",",ex$list.factor,"=='",levs.1,"')")))
				n.row<-length(levs.1)		
				if(is.null(main))
				main<-paste(ex$list.response,"Distribution")
				if(is.null(ylab))
				ylab<-str_to_title(frequency)			
				layout(matrix(c(1,seq(2,n.row+1)),n.row+1,1,byrow=TRUE),widths=28*n.row+4,heights=c(4,rep(28,n.row)),TRUE)
				par(mar=c(0,0,0,0))
				plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
				eval(parse(text=paste0("text(.5,.5,bquote(bold('",main,"')),cex=1.5)")))
				par(mar=c(4,4,0,2))
					for (i in 1:length(levs.1)){
					eval(parse(text=paste0("tab<-xtabs(~with(data",i,",eval(parse(text=ex$list.response))))")))
						if (!missing(nbre))
						eval(parse(text=paste0("tab<-xtabs(nbre~with(data",i,",eval(parse(text=ex$list.response))))")))
					n.levs<-length(rownames(tab))
					pourcentage <- apply(tab, 1,function(x) x/sum(tab))
						if (frequency=="percent")
						tab<-pourcentage
						if(is.null(legend.lab))
						legend.lab<-ex$list.response
					tab.stacked<-cbind(tab,0)
					tab<-c(tab,0)
						if(columns=="juxtaposed"){
							if (is.null(dim))
							barplot(tab, beside = TRUE, col = c(col[1:n.levs],"white"),border=c(rep("black",n.levs),"white"), ylab = ylab, cex.names = cex.names)
							else if (dim=="3D")
							barplot3D(tab, beside = TRUE, col = c(col[1:n.levs],"white"),border=c(rep("black",n.levs),"white"), ylab = ylab,  cex.names = cex.names)
						x.pos<-3
						pos.xlab<-(par("usr")[2]-par("usr")[1])/2+par("usr")[1]
						mtext(paste("For",ex$list.factor,"=",levs.1[i]),1,at=pos.xlab,line=2.5,cex=0.8)	
						}
						if (columns=="stacked"){
						colnames(tab.stacked)<-c(paste("For",ex$list.factor,"=",levs.1[i]),"")
							if (is.null(dim))
							barplot(tab.stacked, beside = FALSE, col = c(col[1:n.levs],"white"),border=c(rep("black",n.levs),"white"), ylab = ylab, cex.names = cex.names)
							else if (dim=="3D")
							barplot3D(tab.stacked, beside = FALSE, col = c(col[1:n.levs],"white"),border=c(rep("black",n.levs),"white"), ylab = ylab, cex.names = cex.names)
						axis1<-par("usr")[2]-par("usr")[1]
						axis2<-par("usr")[4]-par("usr")[3]
						scalex<-axis2/axis1	
						polygon(c(1.4,1.6,2.6,2.4), c(0,0.2*scalex*tan(37/180*pi),0.2*scalex*tan(37/180*pi),0), col = "white", border = "white")	
						x.pos<-2
						}
					legend(x.pos,par("usr")[4], levs.2,fill = col[1:n.levs], cex = cex.legend*1.1, bty = "n", title = legend.lab)		
					}				
				}
			}	
		#Repetee
			if (ex$length.response>1) {
			#Sans strate
				if(!strata){
				factor1 <- with(data,eval(parse(text=ex$list.factor)))
					for (i in 1:ex$length.response){
					factor2 <- with(data,eval(parse(text=ex$list.response[i])))
					eval(parse(text=paste0("tab",i,"<-xtabs(~factor1+factor2)")))
						if (!is.null(nbre))
						eval(parse(text=paste0("tab",i,"<-xtabs(nbre[",i,"]~factor1+factor2)")))
					eval(parse(text=paste0("n.levs.2.",i,"<-length(colnames(tab",i,"))")))
					}
				eval(parse(text=paste0("tab<-cbind(",paste0("tab",1:ex$length.response,collapse=","),")")))
				sums <- apply(tab, 2, sum)
				tabp<-aperm(tab)
				sums <- apply(tabp, 1, sum)
				per <- apply(tabp, 2, function(x) x/sums)
				pourcentage<-aperm(per)
				n.levs.1<-length(rownames(tab))
				eval(parse(text=paste0("n.levs.2<-",paste0("n.levs.2.",1:ex$length.response,collapse="+"))))
				col.level<-c(rep(col[1:n.levs.1],n.levs.2),rep("white",n.levs.1))
				bord.level<-c(rep(rep("black",n.levs.1),n.levs.2),rep("white",n.levs.1))	
					if (frequency=="percent")
					tab<-pourcentage
					if(is.null(main))
					main<-paste(paste(ex$list.response,collapse=","),"and",ex$list.factor,"Distribution")
					if(is.null(ylab))
					ylab<-str_to_title(frequency)		
					if(is.null(legend.lab))
					legend.lab<-ex$list.factor		
				tab<-cbind(tab,rep(0,n.levs.1))
				num.line<-3				
					if (columns=="juxtaposed"){
						if (is.null(dim)){
							if (xlab.axis==0)
							barplot(tab, beside = TRUE, col = col.level,border=bord.level,main=main, ylab = ylab, xlab = xlab, cex.names = cex.names)
							else{
							num.line<-4
							mp <-barplot(tab, beside = TRUE, col = col.level,border=bord.level,main=main, ylab = ylab, axisnames = FALSE)
							pos.x<-mp[1,1:n.levs.2]+.5
							text(pos.x, par("usr")[3], labels = levels(factor2), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.names)
							}
						}
						else if (dim=="3D"){
							if (xlab.axis==0)
							barplot3D(tab, beside = TRUE, col = col.level,border=bord.level,main=main, ylab = ylab, xlab = xlab, cex.names = cex.names)
							else{
							num.line<-4
							mp <-barplot3D(tab, beside = TRUE, col = col.level,border=bord.level,main=main, ylab = ylab, axisnames = FALSE)
							pos.x<-mp$bar.draw[,4]
							pos.x<-pos.x[seq(1,length(pos.x),n.levs.1)]
							pos.x<-pos.x[1:n.levs.2]
							text(pos.x, par("usr")[3], labels = levels(factor2), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.names)					
							}
						}
					x.inf<-1
						for (i in 1:ex$length.response){
						eval(parse(text=paste0("x.coord<-n.levs.2.",i)))
						pos.xlab<-x.inf+(x.coord*n.levs.1+x.coord-1)/2
						mtext(ex$list.response[i],1,at=pos.xlab,line=num.line)
						x.inf<-x.inf+x.coord*n.levs.1+x.coord
						}	
					}
					if (columns=="stacked") {
						if (is.null(dim)){
							if (xlab.axis==0)
							barplot(tab, beside = FALSE,space=0.1, col = col.level,main=main,border=bord.level, ylab = ylab, xlab = xlab, cex.names = cex.names)
							else{
							num.line<-4
							mp <-barplot(tab, beside = FALSE,space=0.1, col = col.level,border=bord.level,main=main, ylab = ylab, axisnames = FALSE)
							pos.x<-mp[1:n.levs.2]
							text(pos.x, par("usr")[3], labels = levels(factor2), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.names)							
							}
						}
						else if (dim=="3D"){
							if (xlab.axis==0)
							barplot3D(tab, beside = FALSE,space=0.1, col = col.level,main=main,border=bord.level, ylab = ylab, xlab = xlab, cex.names = cex.names)		
							else {
							num.line<-4
							mp <-barplot(tab, beside = FALSE,space=0.1, col = col.level,border=bord.level,main=main, ylab = ylab, axisnames = FALSE)
							barplot3D(tab, beside = FALSE,space=0.1, col = col.level,border=bord.level,main=main, ylab = ylab, xlab = xlab, axisnames = FALSE)
							pos.x<-mp[1:n.levs.2]
							text(pos.x, par("usr")[3], labels = levels(factor2), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.names)							
							}
						}
					axis1<-par("usr")[2]-par("usr")[1]
					axis2<-par("usr")[4]-par("usr")[3]
					scalex<-axis2/axis1
					polygon(c(0.1+n.levs.2+0.1*n.levs.2,n.levs.2+0.1*n.levs.2+0.3,n.levs.2+0.1*n.levs.2+1.4,n.levs.2+0.1*n.levs.2+1.1), c(0,0.2*scalex*tan(37/180*pi),0.2*scalex*tan(37/180*pi),0), col = "white", border = "white")				
					x.inf<-0
						for (i in 1:ex$length.response){
						eval(parse(text=paste0("x.coord<-n.levs.2.",i)))
						pos.xlab<-x.inf+(x.coord*.1+x.coord)/2
						mtext(ex$list.response[i],1,at=pos.xlab,line=num.line)
						x.inf<-x.inf+x.coord*.1+x.coord
						}
					}
				legend(legend.pos,rownames(tab),cex=cex.legend,fill=col[1:n.levs.1],title=legend.lab)				
				}
			#Avec strate	
				else {
				factor1 <- with(data,eval(parse(text=ex$list.factor)))
				levs.1<-levels(factor1)
				eval(parse(text=paste0("data",1:length(levs.1),"<-subset(",deparse(substitute(data)),",",ex$list.factor,"=='",levs.1,"')")))
				n.row<-length(levs.1)
				if(is.null(main))
				main<-paste(paste(ex$list.response,collapse=" and "),"Distribution")
				layout(matrix(c(1,seq(2,n.row+1)),n.row+1,1,byrow=TRUE),widths=28*n.row+4,heights=c(4,rep(28,n.row)),TRUE)
				par(mar=c(0,0,0,0))
				plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
				eval(parse(text=paste0("text(.5,.5,bquote(bold('",main,"')),cex=1.5)")))
				par(mar=c(4,4,0,2))
					for (i in 1:length(levs.1)){
					eval(parse(text=paste0("tab",1:ex$length.response,"<-xtabs(~data",i,"$",ex$list.response,")")))
					eval(parse(text=paste0("tab.test",1:ex$length.response,"<-paste0('",ex$list.response,"',names(tab",1:ex$length.response,"))")))
					eval(parse(text=paste0("df<-data.frame(fact=c(",paste0("names(tab",1:ex$length.response,")",collapse=","),"))")))
					eval(parse(text=paste0("df.test<-data.frame(fact=c(",paste0("tab.test",1:ex$length.response,collapse=","),"))")))
					eval(parse(text=paste0("lev",1:ex$length.response,"<-names(tab",1:ex$length.response,")")))
					eval(parse(text=paste0("df$",ex$list.response,"<-0")))
					eval(parse(text=paste0("df.test$",ex$list.response,"<-0")))				
						for (j in 1:ex$length.response){
						test.fact<-NULL
							for (k in 1:length(df.test$fact)){
							eval(parse(text=paste0("test<-tab.test",j)))
								if (sum(df.test$fact[k]==test)>0) test.fact<-c(test.fact,TRUE)
								else test.fact<-c(test.fact,FALSE)
							}
						eval(parse(text=paste0("df.test$",ex$list.response[j],"[c(",paste0(test.fact,collapse=","),")]<-tab",j)))
						}
					df[,-1]<-df.test[,-1]
					tab<-as.matrix(df[2:(ex$length.response+1)])
					rownames(tab)<-df$fact
					n.levs.1<-length(rownames(tab))
					n.levs.2<-length(colnames(tab))
					tabp<-aperm(tab)
					sums <- apply(tabp, 1, sum)
					per <- apply(tabp, 2, function(x) x/sums)
					pourcentage<-aperm(per)
						if (frequency=="percent")
						tab<-pourcentage
						if(is.null(ylab))
						ylab<-str_to_title(frequency)
						if(is.null(legend.lab))
						legend.lab<-ex$list.response
					tab<-cbind(tab,rep(0,n.levs.1))
					col.level<-c(rep(col[1:n.levs.1],n.levs.2),rep("white",n.levs.1))
					col.legend<-col.level[1:length(rownames(tab))]
					bord.level<-c(rep(rep("black",n.levs.1),n.levs.2),rep("white",n.levs.1))
					col.side<-c(rep(rep("white",n.levs.1),n.levs.2),rep("white",n.levs.1))
					bord.side<-c(rep(rep("white",n.levs.1),n.levs.2),rep("white",n.levs.1))
					indice<-1
					offset<-0
						for (j in 1:ex$length.response){
						eval(parse(text=paste0("length.indice<-length(names(tab",j,"))")))
						col.side[(indice+offset):(indice+offset+length.indice-1)]<-col.level[(indice+offset):(indice+offset+length.indice-1)]
						bord.side[(indice+offset):(indice+offset+length.indice-1)]<-rep("black",length.indice)
						indice<-indice+length(rownames(tab))
						offset<-offset+length.indice
						}	
						if (columns=="juxtaposed"){
							if (is.null(dim)){
							barplot(tab, beside = TRUE, col = col.side,border=bord.side, ylab = ylab, xlab = xlab, cex.names = cex.names)
							ymax<-par("usr")[4]
							x.pos<-length(colnames(tab))*length(rownames(tab))-1
							indice<-1
							leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+length(names(tab1))-1)],fill=col.legend[indice:(indice+length(names(tab1))-1)],title=ex$list.response[1])
							indice<-indice+length(names(tab1))
							ymax<-ymax-leg$rect$h
								for (j in 2:ex$length.response){
								eval(parse(text=paste0("offset<-length(names(tab",j,"))")))
								leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+offset-1)],fill=col.legend[indice:(indice+offset-1)],title=ex$list.response[j])
								indice<-indice+length(names(tab1))
								ymax<-ymax-leg$rect$h
								}			
							}
							else if (dim=="3D"){
							mp<-barplot3D(tab, beside = TRUE, col = col.side,border=bord.side, ylab = ylab, xlab = xlab, cex.names = cex.names)	
							bar.draw<-mp$bar.draw
							loz.upper<-mp$loz.upper
							loz.side<-mp$loz.side
							polygon(loz.side[,1], loz.side[,2], col = col.side, border =bord.side)
							polygon(loz.upper[,1], loz.upper[,2], col = col.side, border =bord.side)
							rect(bar.draw[,2], bar.draw[,1], bar.draw[,4], bar.draw[,3],  col = col.side, border =bord.side)
							ymax<-par("usr")[4]
							x.pos<-length(colnames(tab))*length(rownames(tab))-0.2
							indice<-1
							offset<-0
							line.side<-NULL
								for (j in 1:ex$length.response){
								eval(parse(text=paste0("length.indice<-length(names(tab",j,"))")))
								line.side<-rbind(line.side,loz.side[(indice+offset+(length.indice-1)*5):(indice+offset+(length.indice-1)*5+1),])
								line.side<-rbind(line.side,c(NA,NA))
								indice<-indice+length(rownames(tab))*5
								offset<-offset+length.indice*5
								}
							lines(line.side[,1],line.side[,2],col="black")
							indice<-1
							leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+length(names(tab1))-1)],fill=col.legend[indice:(indice+length(names(tab1))-1)],title=ex$list.response[1])
							indice<-indice+length(names(tab1))
							ymax<-ymax-leg$rect$h
								for (j in 2:ex$length.response){
								eval(parse(text=paste0("offset<-length(names(tab",j,"))")))
								leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+offset-1)],fill=col.legend[indice:(indice+offset-1)],title=ex$list.response[j])
								indice<-indice+length(names(tab1))
								ymax<-ymax-leg$rect$h
								}
							}
							axis1<-par("usr")[2]-par("usr")[1]
							if(is.null(xlab)){
							pos.xlab<-axis1/2+par("usr")[1]
							mtext(paste("For",ex$list.factor,"=",levs.1[i]),1,at=pos.xlab,line=2.5,cex=0.8)					
							}						
						}					
						if (columns=="stacked") {
							if (is.null(dim)){
							barplot(tab, beside = FALSE, col = col.level,border=bord.level, ylab = ylab, xlab = xlab, cex.names = cex.names)
							ymax<-par("usr")[4]
							x.pos<-length(colnames(tab))-0.2
							indice<-1
							leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+length(names(tab1))-1)],fill=col.legend[indice:(indice+length(names(tab1))-1)],title=ex$list.response[1])
							indice<-indice+length(names(tab1))
							ymax<-ymax-leg$rect$h
								for (j in 2:ex$length.response){
								eval(parse(text=paste0("offset<-length(names(tab",j,"))")))
								leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+offset-1)],fill=col.legend[indice:(indice+offset-1)],title=ex$list.response[j])
								indice<-indice+length(names(tab1))
								ymax<-ymax-leg$rect$h
								}
							axis1<-par("usr")[2]-par("usr")[1]
							axis2<-par("usr")[4]-par("usr")[3]
							scalex<-axis2/axis1	
							offset<-0
								for (j in 1:ex$length.response){
								eval(parse(text=paste0("y.coord<-sum(tab",j,")")))
								polygon(c(j-0.8+offset,j-0.6+offset,j+0.4+offset,j+0.2+offset), c(y.coord,y.coord+0.2*scalex*tan(37/180*pi),y.coord+0.2*scalex*tan(37/180*pi),y.coord), border = "black")
								offset<-offset+0.2
								}
							polygon(c(length(colnames(tab))-0.8+offset,length(colnames(tab))-0.6+offset,length(colnames(tab))+0.4+offset,length(colnames(tab))+0.2+offset), c(0,0.2*scalex*tan(37/180*pi),0.2*scalex*tan(37/180*pi),0), col = "white", border = "white")									
							}
							else if (dim=="3D"){
							barplot3D(tab, beside = FALSE, col = col.level,border=bord.level, ylab = ylab, xlab = xlab, cex.names = cex.names)
							ymax<-par("usr")[4]
							x.pos<-length(colnames(tab))
							indice<-1
							leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+length(names(tab1))-1)],fill=col.legend[indice:(indice+length(names(tab1))-1)],title=ex$list.response[1])
							indice<-indice+length(names(tab1))
							last.col<-indice-1
							ymax<-ymax-leg$rect$h
								for (j in 2:ex$length.response){
								eval(parse(text=paste0("offset<-length(names(tab",j,"))")))
								last.col<-c(last.col,indice+offset-1)
								leg<-legend(x.pos,ymax,rownames(tab)[indice:(indice+offset-1)],fill=col.legend[indice:(indice+offset-1)],title=ex$list.response[j])
								indice<-indice+length(names(tab1))
								ymax<-ymax-leg$rect$h
								}	
							axis1<-par("usr")[2]-par("usr")[1]
							axis2<-par("usr")[4]-par("usr")[3]
							scalex<-axis2/axis1	
							offset<-0
								for (j in 1:length(last.col)){
								eval(parse(text=paste0("y.coord<-sum(tab",j,")")))
									if (frequency=="percent") y.coord<-1					
								polygon(c(j-0.8+offset,j-0.6+offset,j+0.4+offset,j+0.2+offset), c(y.coord,y.coord+0.2*scalex*tan(37/180*pi),y.coord+0.2*scalex*tan(37/180*pi),y.coord), col = last.col[j], border = "black")
								offset<-offset+0.2
								}
							polygon(c(length(colnames(tab))-0.8+offset,length(colnames(tab))-0.6+offset,length(colnames(tab))+0.4+offset,length(colnames(tab))+0.2+offset), c(0,0.2*scalex*tan(37/180*pi),0.2*scalex*tan(37/180*pi),0), col = "white", border = "white")				
							}
							if(is.null(xlab)){
							pos.xlab<-axis1/2+par("usr")[1]
							mtext(paste("For",ex$list.factor,"=",levs.1[i]),1,at=pos.xlab,line=2.5,cex=0.8)					
							}
						}
					}
				}
			}			
		}
		#Groupe (factor1) et sous-groupe (strate factor1)
		if(ex$length.factor==2){
		#Simple
			if (length(ex$list.response)==1) {
			factor1 <- with(data,eval(parse(text=ex$list.factor[2])))
			levs.1<-levels(factor1)
			eval(parse(text=paste0("data",1:length(levs.1),"<-subset(",deparse(substitute(data)),",",ex$list.factor[2],"=='",levs.1,"')")))
			n.row<-length(levs.1)		
				if(is.null(main))
				main<-paste(ex$list.response,"and",ex$list.factor[1],"Distribution")
				if(is.null(ylab))
				ylab<-str_to_title(frequency)			
			layout(matrix(c(1,seq(2,n.row+1)),n.row+1,1,byrow=TRUE),widths=28*n.row+4,heights=c(4,rep(28,n.row)),TRUE)
			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
			text(.5,.5,main,cex=1.5)
			par(mar=c(4,4,0,2))
				for (i in 1:length(levs.1)){
				eval(parse(text=paste0("tab<-xtabs(~data",i,"$",ex$list.response,"+data",i,"$",ex$list.factor[1],")")))
					if (!is.null(nbre))
					eval(parse(text=paste0("tab<-xtabs(nbre~data",i,"$",ex$list.response,"+data",i,"$",ex$list.factor[1],")")))
				n.levs.1<-length(rownames(tab))
				n.levs.2<-length(colnames(tab))
				tab1<-aperm(tab)
				sums <- apply(tab1, 1, sum)
				per1 <- apply(tab1, 2, function(x) x/sums)
				pourcentage<-aperm(per1)
					if (frequency=="percent")
					tab<-pourcentage
					if(is.null(ylab))
					ylab<-str_to_title(frequency)
					if(is.null(legend.lab))
					legend.lab<-ex$list.response
				tab<-cbind(tab,rep(0,n.levs.1))	
				col.level<-c(rep(col[1:n.levs.1],n.levs.2),rep("white",n.levs.1))
				bord.level<-c(rep(rep("black",n.levs.1),n.levs.2),rep("white",n.levs.1))	
					#if(is.null(xlab))		
					text.xlab<-paste("For",ex$list.factor[2],"=",levs.1[i])
					if (columns=="juxtaposed"){
						if (is.null(dim))
						barplot(tab, beside = TRUE, col = col.level,border=bord.level, ylab = ylab, xlab =text.xlab , cex.names = cex.names)
						else if (dim=="3D"){
						barplot3D(tab, beside = TRUE, col = col.level,border=bord.level, ylab = ylab, xlab = text.xlab, cex.names = cex.names)
						}
					}
					if (columns=="stacked") {
						if (is.null(dim))
						barplot(tab, beside = FALSE,space=0.1, col = col.level,border=bord.level, ylab = ylab, xlab = text.xlab, cex.names = cex.names)
						else if (dim=="3D")
						barplot3D(tab, beside = FALSE,space=0.1, col = col.level,border=bord.level, ylab = ylab, xlab = text.xlab, cex.names = cex.names)		
					axis1<-par("usr")[2]-par("usr")[1]
					axis2<-par("usr")[4]-par("usr")[3]
					scalex<-axis2/axis1
					polygon(c(0.1+n.levs.2+0.1*n.levs.2,n.levs.2+0.1*n.levs.2+0.3,n.levs.2+0.1*n.levs.2+1.4,n.levs.2+0.1*n.levs.2+1.1), c(0,0.2*scalex*tan(37/180*pi),0.2*scalex*tan(37/180*pi),0), col = "white", border = "white")				
					}
				legend(legend.pos,rownames(tab),cex=cex.legend,fill=col[1:n.levs.1],title=legend.lab)
				axis1<-par("usr")[2]-par("usr")[1]
				pos.xlab<-n.levs.1*axis1/(2*(n.levs.1+1))+par("usr")[1]
					if(is.null(xlab))
					mtext(ex$list.factor[1],1,at=pos.xlab,line=2,cex=0.7)	
					else
					mtext(xlab,1,at=pos.xlab,line=2,cex=0.7)
				}
			}
			#Repetee
			if (ex$length.response>1){
			factor1 <- with(data,eval(parse(text=ex$list.factor[2])))
			levs.1<-levels(factor1)
			eval(parse(text=paste0("data",1:length(levs.1),"<-subset(",deparse(substitute(data)),",",ex$list.factor[2],"=='",levs.1,"')")))
			n.row<-length(levs.1)		
				if(is.null(main))
				main<-paste(paste(ex$list.response,collapse=","),"and",ex$list.factor[1],"Distribution")
				if(is.null(ylab))
				ylab<-str_to_title(frequency)			
			layout(matrix(c(1,seq(2,n.row+1)),n.row+1,1,byrow=TRUE),widths=28*n.row+4,heights=c(4,rep(28,n.row)),TRUE)
			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
			eval(parse(text=paste0("text(.5,.5,bquote(bold('",main,"')),cex=1.5)")))
			par(mar=c(4,4,0,2))
				for (i in 1:length(levs.1)){
				eval(parse(text=paste0("factor3 <- data",i,"$",ex$list.factor[1])))
					for (j in 1:ex$length.response){
					eval(parse(text=paste0("factor2 <- data",i,"$",ex$list.response[j])))
					eval(parse(text=paste0("tab",i,".",j,"<-xtabs(~factor3+factor2)")))
						if (!is.null(nbre))
						eval(parse(text=paste0("tab",i,".",j,"<-xtabs(nbre[",j,"]~factor3+factor2)")))
					eval(parse(text=paste0("n.levs.2.",j,"<-length(colnames(tab",i,".",j,"))")))
					}
				eval(parse(text=paste0("tab<-cbind(",paste0("tab",i,".",1:ex$length.response,collapse=","),")")))
				sums <- apply(tab, 2, sum)
				tabp<-aperm(tab)
				sums <- apply(tabp, 1, sum)
				per <- apply(tabp, 2, function(x) x/sums)
				pourcentage<-aperm(per)
				n.levs.1<-length(rownames(tab))
				eval(parse(text=paste0("n.levs.2<-",paste0("n.levs.2.",1:ex$length.response,collapse="+"))))
				col.level<-c(rep(col[1:n.levs.1],n.levs.2),rep("white",n.levs.1))
				bord.level<-c(rep(rep("black",n.levs.1),n.levs.2),rep("white",n.levs.1))	
					if (frequency=="percent")
					tab<-pourcentage
					if(is.null(main))
					main<-paste(paste(ex$list.response,collapse=","),"and",ex$list.factor[1],"Distribution")
					if(is.null(ylab))
					ylab<-str_to_title(frequency)		
					if(is.null(legend.lab))
					legend.lab<-ex$list.factor[1]		
				tab<-cbind(tab,rep(0,n.levs.1))
					if(is.null(xlab))		
					text.xlab<-paste("For",ex$list.factor[2],"=",levs.1[i])
				if (columns=="juxtaposed"){
						if (is.null(dim))
						barplot(tab, beside = TRUE, col = col.level,border=bord.level,main="", ylab = ylab, xlab = text.xlab, cex.names = cex.names)
						else if (dim=="3D")
						barplot3D(tab, beside = TRUE, col = col.level,border=bord.level,main="", ylab = ylab, xlab = text.xlab, cex.names = cex.names)
					x.inf<-1
						for (j in 1:ex$length.response){
						eval(parse(text=paste0("x.coord<-n.levs.2.",j)))
						pos.xlab<-x.inf+(x.coord*n.levs.1+x.coord-1)/2
						mtext(ex$list.response[j],1,at=pos.xlab,line=2,,cex=0.7)
						x.inf<-x.inf+x.coord*n.levs.1+x.coord
						}	
					}
					if (columns=="stacked") {
						if (is.null(dim))
						barplot(tab, beside = FALSE,space=0.1, col = col.level,main="",border=bord.level, ylab = ylab, xlab = text.xlab, cex.names = cex.names)
						else if (dim=="3D")
						barplot3D(tab, beside = FALSE,space=0.1, col = col.level,main="",border=bord.level, ylab = ylab, xlab = text.xlab, cex.names = cex.names)		
					axis1<-par("usr")[2]-par("usr")[1]
					axis2<-par("usr")[4]-par("usr")[3]
					scalex<-axis2/axis1
					polygon(c(0.1+n.levs.2+0.1*n.levs.2,n.levs.2+0.1*n.levs.2+0.3,n.levs.2+0.1*n.levs.2+1.4,n.levs.2+0.1*n.levs.2+1.1), c(0,0.2*scalex*tan(37/180*pi),0.2*scalex*tan(37/180*pi),0), col = "white", border = "white")				
					x.inf<-0
						for (j in 1:ex$length.response){
						eval(parse(text=paste0("x.coord<-n.levs.2.",j)))
						pos.xlab<-x.inf+(x.coord*.1+x.coord)/2
						mtext(ex$list.response[j],1,at=pos.xlab,line=2,cex=0.7)
						x.inf<-x.inf+x.coord*.1+x.coord
						}
					}
				legend(legend.pos,rownames(tab),cex=cex.legend,fill=col[1:n.levs.1],title=legend.lab)
				}
			}			
		}
	}
par(op)	
}

##########################################################################
# Boites de dispersion
graphBox<-function (formula,strata=FALSE,data=parent.frame(),boxwex=0.8,xlab = NULL,cex.axis=1,hack=FALSE,dim=c("2D","3D"),
xlab.axis=0,ylab = NULL,ylim=NULL,name.repeated="Variable", legend.lab = NULL,legend.pos="topright",cex.legend=1, main = NULL, col = palette())
{
op <- par(no.readonly = TRUE)
dim <- match.arg(dim)
	if(dim=="2D") dim<-NULL
	#extraction des parametres
f<-deparse(substitute(formula))
f<-paste(f,collapse="")
eval(parse(text=paste0("ex<-Extract.fact(",f,",data=",deparse(substitute(data)),")")))	

#Graphiques
#formule sans groupe
	if (ex$length.factor==0) {
		#simple
		if (ex$length.response==1) {
		n.levs<-1
			if (col[1]=="black") col<-"turquoise"
			if (is.null(ylab)) ylab<-ex$list.response
			if (is.null(main)) main <- paste(ex$list.response,"data dispersion")
			if (is.null(dim))
			boxplot(with(data,eval(parse(text=ex$list.response))),ylab=ylab,ylim=ylim,notch=hack,boxwex=boxwex,col=col,main=main)
			else if (dim=="3D")
			boxplot3D(with(data,eval(parse(text=ex$list.response))),ylab=ylab,ylim=ylim,notch=hack,boxwex=boxwex,col=col,main=main)
		}
		#formule sans groupe repetee
		if (ex$length.response>1)	{
		eval(parse(text=paste0("data<-Repeated.data.frame(",paste(ex$list.response,collapse="+",sep=""),",time.variable='var',data=",deparse(substitute(data)),")")))
		n.levs<-ex$length.response
		col.level<-col[1:n.levs]
		text.col<-paste0(",col=c(",paste0("'",col.level,"'",collapse=","),")")
			if (is.null(ylab)) text.ylab<-paste0(",ylab='",name.repeated,"'") else text.ylab<-paste0(",ylab='",ylab,"'")
			if (is.null(main)) text.main <- paste0(",main='",paste(ex$list.response,collapse=" and ")," data dispersion'") else text.main<-paste0(",main=\"",main,"\"")
			if (is.null(ylim)) text.ylim<-NULL else text.ylim<-paste(",ylim=c(",paste0(ylim,collapse=","),")")
			if (is.null(xlab)) text.xlab<-paste0(",xlab=''") else text.xlab<-paste0(",xlab='",xlab,"'")
		eval(parse(text=paste0("boxplot",dim,"(var.value~var, data=data",text.xlab,text.ylab,text.ylim,",notch=",hack,",boxwex=",boxwex,text.col,text.main,")")))
		}
	}
	#formule avec groupe
	if (ex$length.factor>0){
		#Un seul groupe	
		if (ex$length.factor==1){
			#simple
			if (ex$length.response==1) {
			fact <- with(data,eval(parse(text=ex$list.factor)))
			n.levs <- length(levels(fact))
			col.level<-col[1:n.levs]
			text.col<-paste0(",col=c(",paste0("'",col.level,"'",collapse=","),")")
				if (is.null(ylab)) text.ylab<-paste0(",ylab='",ex$list.response,"'") else text.ylab<-paste0(",ylab='",ylab,"'")
				if (is.null(main)) text.main <- paste0(",main='",ex$list.response," data dispersion'") else text.main<-paste0(",main=\"",main,"\"")
				if (is.null(xlab)) text.xlab<-paste0(",xlab='",ex$list.factor,"'") else text.xlab<-paste0(",xlab='",xlab,"'")
				if (is.null(ylim)) text.ylim<-NULL else text.ylim<-paste(",ylim=c(",paste0(ylim,collapse=","),")")
			eval(parse(text=paste0("boxplot",dim,"(",ex$list.response,"~",ex$list.factor,",data=",deparse(substitute(data)),text.xlab,text.ylab,text.ylim,",notch=",hack,",boxwex=",boxwex,text.col,text.main,")")))	
			}
			#variable repetee
			if (ex$length.response>1) {
			eval(parse(text=paste0("data<-Repeated.data.frame(",paste(ex$list.response,collapse="+",sep=""),",",ex$list.factor,",time.variable='var',data=",deparse(substitute(data)),")")))
				#facteur en legende
				if (!strata){
				levs.1<-ex$list.response
				n.levs.1<-ex$length.response
				fact <- eval(parse(text=paste0("data$",ex$list.factor)))
				#subfactor en legende
				levs.2<-levels(fact)
				n.levs.2 <- length(levs.2)
				}
				#variable repetees en legende
				if (strata){
				fact <- eval(parse(text=paste0("data$",ex$list.factor)))
				levs.1<-levels(fact)
				n.levs.1<-length(levs.1)
				#subfactor en legende
				levs.2<-ex$list.response
				n.levs.2 <- ex$length.response
				}
			nbre.names<-rep(1:(n.levs.2+1),n.levs.1)
			milieu<-ceiling((n.levs.2+1)/2)
			x.names<-rep("",length(nbre.names))
			j<-1
				for (i in 1:length(nbre.names)){
					if(nbre.names[i]==milieu){
					x.names[i]<-levs.1[j]
					j<-j+1
					}
				}
				if (is.null(ylab)) text.ylab<-paste0(",ylab='",name.repeated,"'") else text.ylab<-paste0(",ylab='",ylab,"'")
				if (is.null(main)) text.main <- paste0(",main='",name.repeated," data dispersion'") else text.main<-paste0(",main=\"",main,"\"")
				if (is.null(xlab)) {
					if (!strata) text.xlab<-paste0(",xlab='",name.repeated,"'") else text.xlab<-paste0(",xlab='",xlab,"'")
					if (strata) text.xlab<-paste0(",xlab='",ex$list.factor,"'") else text.xlab<-paste0(",xlab='",xlab,"'")
				}
				if (is.null(ylim)) text.ylim<-NULL else text.ylim<-paste(",ylim=c(",paste0(ylim,collapse=","),")")
			df<-data
			df<-rbind(df[1,],df)
				if (!strata) {
				eval(parse(text=paste0("levels(df$",ex$list.factor,")<-c(levels(df$",ex$list.factor,"),'')")))
				eval(parse(text=paste0("df$",ex$list.factor,"[1]<-''")))
				}
				if (strata) {
				levels(df$var)<-c(levels(df$var),'')
				df$var[1]<-''
				}
			eval(parse(text=paste0("df$var.value[1]<-NA")))
			col.level<-col[1:n.levs.2]
			text.col<-paste0(",col=c(",paste0("'",col.level,"'",collapse=","),")")
				if (is.null(dim)) {
				col.level<-c(col.level,"white")
				col.level<-rep(col.level,n.levs.2)	
				text.col<-paste0(",col=c(",paste0("'",col.level,"'",collapse=","),")")
				}
			abscisse<- rep('',length(x.names))
			textx.names<-paste0(",names=c(",paste0("'",abscisse,"'",collapse=","),")")
				if (!strata) {
				if (is.null(legend.lab)) legend.lab<-ex$list.factor
				eval(parse(text=paste0("boxplot",dim,"(var.value~",ex$list.factor,"+var,data=df",text.xlab,textx.names,text.ylab,text.ylim,",notch=",hack,",boxwex=",boxwex,text.col,text.main,")")))
					for (position in 1:length(x.names))
					eval(parse(text=paste0("mtext(x.names[",position,"],side=1,line=1,at=",position,")")))
				legend(legend.pos, levs.2, fill = col.level, title =legend.lab)
				}
				if(strata){
				if (is.null(legend.lab)) legend.lab<-name.repeated
				eval(parse(text=paste0("boxplot",dim,"(var.value~var+",ex$list.factor,",data=df",text.xlab,textx.names,text.ylab,text.ylim,",notch=",hack,",boxwex=",boxwex,text.col,text.main,")")))	
					for (position in 1:length(x.names))
					eval(parse(text=paste0("mtext(x.names[",position,"],side=1,line=1,at=",position,")")))
				legend(legend.pos, levs.2, fill = col.level, title =legend.lab)
				}
			}
		}
		#Un groupe et un sous-groupe
		if (ex$length.factor==2){
			#simple
			if (ex$length.response==1) {
			valid <- complete.cases(with(data,eval(parse(text=ex$list.response)),with(data,eval(parse(text=ex$list.factor[1]))),with(data,eval(parse(text=ex$list.factor[2])))))
			fact <- with(data,eval(parse(text=ex$list.factor[1])))[valid]
			subfactor <- with(data,eval(parse(text=ex$list.factor[2])))[valid]
			levs.1<-levels(fact)
			n.levs.1 <- length(levs.1)
			#subfactor en legende
			levs.2<-levels(subfactor)
			n.levs.2 <- length(levs.2)
			nbre.names<-rep(1:(n.levs.2+1),n.levs.1)
			milieu<-ceiling((n.levs.2+1)/2)
			x.names<-rep("",length(nbre.names))
			j<-1
				for (i in 1:length(nbre.names)){
					if(nbre.names[i]==milieu){
					x.names[i]<-levs.1[j]
					j<-j+1
					}
				}
				if (is.null(ylab)) text.ylab<-paste0(",ylab='",ex$list.response,"'") else text.ylab<-paste0(",ylab='",ylab,"'")
				if (is.null(main)) text.main <- paste0(",main='",ex$list.response," data dispersion'") else text.main<-paste0(",main=\"",main,"\"")
				if (is.null(xlab)) text.xlab<-paste0(",xlab='",ex$list.factor[1],"'") else text.xlab<-paste0(",xlab='",xlab,"'")
				if (is.null(ylim)) text.ylim<-NULL else text.ylim<-paste(",ylim=c(",paste0(ylim,collapse=","),")")
				if (is.null(legend.lab)) legend.lab<- ex$list.factor[2]
			df<-data
			df<-rbind(df[1,],df)
			eval(parse(text=paste0("levels(df$",ex$list.factor[2],")<-c(levels(df$",ex$list.factor[2],"),'')")))
			eval(parse(text=paste0("df$",ex$list.factor[2],"[1]<-''")))
			eval(parse(text=paste0("df$",ex$list.response,"[1]<-NA")))
			col.level<-col[1:n.levs.2]
			text.col<-paste0(",col=c(",paste0("'",col.level,"'",collapse=","),")")
				if (is.null(dim)) {
				col.level<-c(col.level,"white")
				col.level<-rep(col.level,n.levs.2)	
				text.col<-paste0(",col=c(",paste0("'",col.level,"'",collapse=","),")")
				}
			abscisse<- rep('',length(x.names))
			textx.names<-paste0(",names=c(",paste0("'",abscisse,"'",collapse=","),")")
			eval(parse(text=paste0("boxplot",dim,"(",ex$list.response,"~",ex$list.factor[2],"+",ex$list.factor[1],",data=df",textx.names,text.xlab,text.ylab,text.ylim,",notch=",hack,",boxwex=",boxwex,text.col,text.main,")")))	
				for (position in 1:length(x.names))
				eval(parse(text=paste0("mtext(x.names[",position,"],side=1,line=1,at=",position,")")))
			legend(legend.pos, levs.2, fill = col.level, title =legend.lab)
			}
			#variable repetee
			if (ex$length.response>1) {
			# sous groupe en strate
			eval(parse(text=paste0("data<-Repeated.data.frame(",paste(ex$list.response,collapse="+",sep=""),",",paste(ex$list.factor,collapse="+",sep=""),",time.variable='var',data=",deparse(substitute(data)),")")))
			strate <- eval(parse(text=paste0("data$",ex$list.factor[2])))
			levs.3<-levels(strate)
			n.row<-length(levs.3)
				if(is.null(main))
				main <- paste0(name.repeated," data dispersion")
				if(is.null(ylab)) text.ylab<-paste0(",ylab='",name.repeated,"'") else text.ylab<-paste0(",ylab='",ylab,"'")
			layout(matrix(c(1,seq(2,n.row+1)),n.row+1,1,byrow=TRUE),widths=28*n.row+4,heights=c(4,rep(28,n.row)),TRUE)
			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
			eval(parse(text=paste0("text(.5,.5,bquote(bold('",main,"')),cex=1.5)")))
			par(mar=c(4,4,0,2))	
				for (i in 1:length(levs.3)){
				eval(parse(text=paste0("df<-subset(data,",ex$list.factor[2],"=='",levs.3[i],"')")))
					#facteur en legende
					if (!strata){
					levs.1<-ex$list.response
					n.levs.1<-ex$length.response
					fact <- eval(parse(text=paste0("data$",ex$list.factor[1])))
					#subfactor en legende
					levs.2<-levels(fact)
					n.levs.2 <- length(levs.2)
					}
					#variable repetees en legende
					if (strata){
					fact <- eval(parse(text=paste0("data$",ex$list.factor[1])))
					levs.1<-levels(fact)
					n.levs.1<-length(levs.1)
					#subfactor en legende
					levs.2<-ex$list.response
					n.levs.2 <- ex$length.response
					}
				nbre.names<-rep(1:(n.levs.2+1),n.levs.1)
				milieu<-ceiling((n.levs.2+1)/2)
				x.names<-rep("",length(nbre.names))
				k<-1
					for (j in 1:length(nbre.names)){
						if(nbre.names[j]==milieu){
						x.names[j]<-levs.1[k]
						k<-k+1
						}
					}
					if (is.null(xlab)) text.xlab<-paste0("For ",ex$list.factor[2],"=",levs.3[i]) else text.xlab<-xlab
					if (is.null(ylim)) text.ylim<-NULL else text.ylim<-paste(",ylim=c(",paste0(ylim,collapse=","),")")
				df<-rbind(df[1,],df)
					if (!strata) {
					eval(parse(text=paste0("levels(df$",ex$list.factor[1],")<-c(levels(df$",ex$list.factor[1],"),'')")))
					eval(parse(text=paste0("df$",ex$list.factor[1],"[1]<-''")))
					}
					if (strata) {
					levels(df$var)<-c(levels(df$var),'')
					df$var[1]<-''
					}
				eval(parse(text=paste0("df$var.value[1]<-NA")))
				col.level<-col[1:n.levs.2]
				text.col<-paste0(",col=c(",paste0("'",col.level,"'",collapse=","),")")
					if (is.null(dim)) {
					col.level<-c(col.level,"white")
					col.level<-rep(col.level,n.levs.2)	
					text.col<-paste0(",col=c(",paste0("'",col.level,"'",collapse=","),")")
					}
				abscisse<- rep('',length(x.names))
				textx.names<-paste0(",names=c(",paste0("'",abscisse,"'",collapse=","),")")
					if (!strata) {
						if (is.null(legend.lab)) legend.lab<- ex$list.factor[1]
					eval(parse(text=paste0("boxplot",dim,"(var.value~",ex$list.factor[1],"+var,data=df,xlab=''",textx.names,text.ylab,text.ylim,",notch=",hack,",boxwex=",boxwex,text.col,")")))	
						for (position in 1:length(x.names))
						eval(parse(text=paste0("mtext(x.names[",position,"],side=1,cex=0.7,line=1,at=",position,")")))
					legend(legend.pos, levs.2, fill = col.level, title =legend.lab)
					}
					if(strata){
						if (is.null(legend.lab)) legend.lab<- name.repeated
					eval(parse(text=paste0("boxplot",dim,"(var.value~var+",ex$list.factor[1],",data=df,xlab=''",textx.names,text.ylab,text.ylim,",notch=",hack,",boxwex=",boxwex,text.col,")")))	
						for (position in 1:length(x.names))
						eval(parse(text=paste0("mtext(x.names[",position,"],side=1,line=1,cex=0.7,at=",position,")")))
					legend(legend.pos, levs.2, fill = col.level, title =legend.lab)
					}
				pos.xlab<-(par("usr")[2]-par("usr")[1])/2+par("usr")[1]
				mtext(text.xlab,1,at=pos.xlab,line=2.5,cex=0.8)				
				}
			}
		}
	}
par(op)
invisible(NULL)
}
##########################################################################
#Sortie boxplot par groupe
boxplot.graph<-function (formula,data=parent.frame(),boxwex=0.8,xlab = NULL,cex.axis=1,las=1,ylab = NULL,ylim = NULL,legend.lab = NULL, cex.legend=1,
 main = "Data dispersion", col = palette()[2:8])
{
f <- formula(formula)
listvar <- as.character(attr(terms(f), "variables"))[-1]
test.factor <- with(data,eval(parse(text=listvar[2])))
	if (is.numeric(test.factor)) {
	responsename<-listvar
	factorname<-NULL
	}
	else{
	responsename<-listvar[1]
	factorname<-listvar[2]
	}
	if (!is.null(factorname)){
	valid <- complete.cases(with(data,eval(parse(text=responsename))),with(data,eval(parse(text=factorname))))
	response <- with(data,eval(parse(text=responsename)))[valid]
	if (!is.numeric(response))
	stop(gettext("Variable1 must be numeric.",domain="R-RcmdrPlugin.TestGraph"))
	factor <- with(data,eval(parse(text=factorname)))[valid]
	factor<-factor(factor)
	}
	else{
	data$id<-1:length(rownames(data))
	dropname<-colnames(data)[colnames(data)!="id"]
	for (var in responsename) dropname<-dropname[dropname!=var]
	tmp.data<-reshape(data, idvar = "id", timevar="Parameter",varying = responsename, v.names = "Block", drop=dropname,direction = "long")
	tmp.data$Parameter<-factor(tmp.data$Parameter,labels=responsename)
	valid <- complete.cases(with(tmp.data,eval(parse(text="Block"))),with(tmp.data,eval(parse(text="Parameter"))))
	response <- with(tmp.data,eval(parse(text="Block")))[valid]
	factor <- with(tmp.data,eval(parse(text="Parameter")))[valid]
	factor<-factor(factor)
	}
#Graphiques
levs <- levels(factor)
n.levs <- length(levs)
	if (n.levs>6) col<-rainbow(n.levs)
	if (is.null(ylab)) {
	if (!is.null(factorname)) ylab<-responsename
	else ylab<-"Parameter"
	}
	if (is.null(xlab)) {
	if (!is.null(factorname)) xlab<-factorname
	else xlab<-"Block"
	}
boxplot(response~factor,boxwex=boxwex,col=col,show.names=F,data=data,xlab=xlab,ylab=ylab,ylim=ylim)
axis(1, at = 1:n.levs, cex.axis=cex.axis,las=las,labels = levs)
title(main = main, font.main = 4)
invisible(NULL)
}
############################################################################
#Graphique des moyennes
plotError<-function (formula,strata=FALSE, data=parent.frame(),error.bars = c("se", "sd","conf.int", "none"),level = 0.95,xlab=NULL,xlab.axis=0,cex.axis=1,ylab = NULL, ylim=NULL, 
legend.lab = NULL,name.variable=NULL,legend.pos="topright" ,scale=c("factor","numeric"),scale.levs=NULL,main = "Plot of Means", 
pch = NULL,pointsize=1.5, lty = rep(1,n.levs.2), lwd = rep(1,n.levs.2),col = palette())
{
op <- par(no.readonly = TRUE)
#extraction des facteurs
eval(parse(text=paste0("ex<-Extract.fact(",paste(deparse(substitute(formula)),collapse=""),",data=",deparse(substitute(data)),")")))
error.bars <- match.arg(error.bars)
if (length(scale)==2) scale<-"factor"

#formule sans groupe
    if (ex$length.factor == 0) {
	#Simple
        if (ex$length.response == 1)
		stop(gettext("At least one factor or two responses.",domain="R-RcmdrPlugin.TestGraph"))
		#Repetee
        if (ex$length.response > 1) {
		eval(parse(text=paste0("data<-Repeated.data.frame(",paste(ex$list.response,collapse="+",sep=""),",time.variable='var',data=",deparse(substitute(data)),")")))
		data<-na.omit(data)
		factor<-data$var
		responses <-data$var.value
		means <- tapply(responses, factor, mean)
		sds <- tapply(responses, factor, sd)
		ns <- tapply(responses, factor, length)
			if (error.bars == "se") sds <- sds/sqrt(ns)
			if (error.bars == "conf.int")
			sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) * sds/sqrt(ns)
		sds[is.na(sds)] <- 0
			if (error.bars == "none") 
			sds[1:length(sds)]<-0
			yrange <-if (is.null(ylim)) c(min(means - sds), max(means + sds)) else ylim
		levs <- levels(factor)
		n.levs <- length(levs)
		n.levs.2<-0	
			pch<-if (is.null(pch)) 16 else pch[1]
			if (length(xlab)==0) xlab<-name.variable
			if (length(ylab)==0) ylab<-paste(name.variable,"Mean")
			if (scale=="numeric"){
			scale.xticks<-sub("[A-Za-z./]*", "", levs) 
			scale.xticks<-as.numeric(scale.xticks)
				if (is.null(scale.levs)){
				scale.levs<-levs
				if (sum(is.na(scale.xticks))>0) scale<-"factor"
				}
				if (scale!="factor"){
				plot(c(min(scale.xticks), max(scale.xticks)),yrange,type="n",xlab=xlab,ylab=ylab,axes=FALSE,main=main)
				points(scale.xticks, means, type = "b", pch = pch, cex = pointsize,col=col[1])
				axis(1,at=scale.xticks,labels =scale.levs ,cex.axis=cex.axis)
				for (i in 1:n.levs) 
				if(sds[i]!=0) arrows(scale.xticks[i],means[i]-sds[i],scale.xticks[i],means[i]+sds[i],angle = 90,code=3,lty=1,length=0.125,col=col)
				}
			}
			if (scale=="factor"){
			scale.levs<-levs
			plot(c(1, n.levs),yrange,type = "n",xlab=xlab,ylab=ylab,axes=FALSE,main=main)
			points(1:n.levs, means, type = "b", pch = pch, cex = pointsize,col=col[1])
			axis(1, at = 1:n.levs, labels = scale.levs,cex.axis=cex.axis)
				for (i in 1:n.levs) 
				if(sds[i]!=0) arrows(i,means[i]-sds[i],i,means[i]+sds[i],angle = 90,code=3,lty=1,length=0.125,col=col)
			}
		box()
		axis(2)		
	  }
	}
	#Avec un groupe
    if (ex$length.factor != 0) {
		#Simple
        if (ex$length.response == 1) {
			#Un seul groupe
            if (ex$length.factor == 1) {
				valid <- complete.cases(with(data,eval(parse(text=ex$list.response))),with(data,eval(parse(text=ex$list.factor[1]))))
				response <- with(data,eval(parse(text=ex$list.response)))[valid]
				factor <- factor(with(data,eval(parse(text=ex$list.factor[1])))[valid])
				means <- tapply(response, factor, mean)
				sds <- tapply(response, factor, sd)
				ns <- tapply(response, factor, length)
					if (error.bars == "se") sds <- sds/sqrt(ns)
					if (error.bars == "conf.int")
				sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) * sds/sqrt(ns)
				sds[is.na(sds)] <- 0
					if (error.bars == "none") 
					sds[1:length(sds)]<-0
				yrange <-if (is.null(ylim)) c(min(means - sds), max(means + sds)) else ylim
				levs <- levels(factor)
				n.levs <- length(levs)
				n.levs.2<-0
					if (length(xlab)==0) xlab<-ex$list.factor[1]
					if (length(ylab)==0) ylab<-ex$list.response
					pch<-if (is.null(pch)) 16 else pch[1]
					if (scale=="numeric") {
					scale.xticks<-sub("[A-Za-z./]*", "", levs) 
					scale.xticks<-as.numeric(scale.xticks)
						if (is.null(scale.levs)){
						scale.levs<-levs
						if (sum(is.na(scale.xticks))>0) scale<-"factor"
						}
						if (scale!="factor"){
						plot(c(min(scale.xticks), max(scale.xticks)),yrange,type="n",xlab=xlab,ylab=ylab,axes=FALSE,main=main)
						points(scale.xticks, means, type = "b", pch = pch, cex = pointsize)
						axis(1,at=scale.xticks,labels =scale.levs ,cex.axis=cex.axis)
							for (i in 1:n.levs) 
							if(sds[i]!=0) arrows(scale.xticks[i],means[i]-sds[i],scale.xticks[i],means[i]+sds[i],angle = 90,code=3,lty=1,length=0.125,col=col)
						}
					}
					if (scale=="factor"){
					plot(c(1, n.levs),yrange,type="n",xlab=xlab,ylab=ylab,axes=FALSE,main=main)
					points(1:n.levs, means, type = "b", pch = pch, cex = pointsize)
					axis(1, at = 1:n.levs, labels = levs,cex.axis=cex.axis)
						for (i in 1:n.levs)
						if(sds[i]!=0) arrows(i,means[i]-sds[i],i,means[i]+sds[i],angle=90,code=3,lty=1,length=0.125,col=col)
					}
				box()
				axis(2)							
			}
			#Groupe et sous groupe
			if (ex$length.factor == 2) {
			valid <- complete.cases(with(data,eval(parse(text=ex$list.response))),with(data,eval(parse(text=ex$list.factor[1]))),with(data,eval(parse(text=ex$list.factor[2]))))
			response <- with(data,eval(parse(text=ex$list.response)))[valid]
			factor <- factor(with(data,eval(parse(text=ex$list.factor[1])))[valid])
			subfactor <- factor(with(data,eval(parse(text=ex$list.factor[2])))[valid])
			means <- tapply(response, list(factor,subfactor), mean)
			sds <- tapply(response, list(factor,subfactor), sd)
			ns <- tapply(response, list(factor,subfactor), length)			
				if (error.bars == "se") sds <- sds/sqrt(ns)
				if (error.bars == "conf.int")
				sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) * sds/sqrt(ns)
			sds[is.na(sds)] <- 0
				if (error.bars == "none") 
				sds[1:length(sds)]<-0
			yrange <-if (is.null(ylim)) c(min(means - sds), max(means + sds)) else ylim
			levs.1 <- levels(factor)
			levs.2 <- levels(subfactor)
			n.levs.2 <- length(levs.2)	
				if(is.null(pch))
				pch <- 1:n.levs.2
				else {
				if (length(pch)<n.levs.2) pch<-c(pch,1:n.levs.2)
				}
			if (length(xlab)==0) xlab<-ex$list.factor[1]
			if (length(ylab)==0) ylab<-ex$list.response
			if (is.null(legend.lab)) legend.lab<-ex$list.factor[2]
				if (scale=="numeric"){
				scale.xticks<-sub("[A-Za-z./]*", "", levs.1) 
				scale.xticks<-as.numeric(scale.xticks)				
					if (is.null(scale.levs)){
					scale.levs<-levs.1
					if (sum(is.na(scale.xticks))>0) scale<-"factor"
					}
					if (scale!="factor"){
					plot(c(min(scale.xticks), max(scale.xticks)),yrange,type="n",xlab=xlab,ylab=ylab,axes=FALSE,main=main)
						for (i in 1:n.levs.2){
							for (j in 1:length(levs.1)){
							points(scale.xticks[j], means[j,i],type="b",pch=pch[i],cex=pointsize,col=col[i])
							if(sds[j,i]!=0) arrows(scale.xticks[j],means[j,i]-sds[j,i],scale.xticks[j],means[j,i]+sds[j,i],angle=90,code=3,col=col[i],lty=1,lwd=lwd,length=0.125)
							}	
						segments(scale.xticks[1:(length(levs.1)-1)],means[1:(length(levs.1)-1),i],scale.xticks[2:length(levs.1)],means[2:length(levs.1),i],col=col[i],lty=lty[i],lwd=lwd[i])
						}
					axis(1, at = scale.xticks, labels = levs.1,cex.axis=cex.axis)
						for (i in 1:length(levs.1)) 
						if(sds[i]!=0) arrows(scale.xticks[i],means[i]-sds[i],scale.xticks[i],means[i]+sds[i],angle = 90,code=3,lty=1,length=0.125,col=col)
					}					
				}
				if (scale=="factor"){
				n.levs.1 <- length(levs.1)
				plot(c(1, n.levs.1 + 1),yrange,type="n",xlab=xlab,ylab=ylab,axes=FALSE,main=main)
				axis(1, at = 1:n.levs.1, labels = levs.1,cex.axis=cex.axis)
					for (i in 1:n.levs.2) {
						for (j in 1:length(levs.1)){
						points(j, means[j,i],type="b",pch=pch[i],cex=pointsize,col=col[i])
						if(sds[j,i]!=0) arrows(j,means[j,i]-sds[j,i],j,means[j,i]+sds[j,i],angle=90,code=3,col=col[i],lty=1,lwd=lwd,length=0.125)
						}
					segments(1:(n.levs.1-1),means[1:(length(levs.1)-1),i],2:n.levs.1,means[2:length(levs.1),i],col=col[i],lty=lty[i],lwd=lwd[i])
					}				
				}
			legend(legend.pos,c(legend.lab,levs.2),pch=c(0,pch),col=c(0,col),lty=c(0, lty),lwd=c(0,lwd))	
			box()
			axis(2)	
			}
		}
		#repetee
        if (ex$length.response > 1) {
		#Un seul groupe
            if (ex$length.factor == 1) {
				if (!strata){
				eval(parse(text=paste0("data<-Repeated.data.frame(",paste(ex$list.response,collapse="+",sep=""),",time.variable='var',factor=",ex$list.factor,",data=",deparse(substitute(data)),")")))
				data<-na.omit(data)
				factor<-data$var
				responses <-data$var.value
				eval(parse(text=paste0("subfactor<-data$",ex$list.factor)))
					if(is.null(legend.lab)) legend.lab<-ex$list.factor
				means <- tapply(responses, list(factor,subfactor), mean)
				sds <- tapply(responses, list(factor,subfactor), sd)
				ns <- tapply(responses, list(factor,subfactor), length)
					if (error.bars == "se") sds <- sds/sqrt(ns)
					if (error.bars == "conf.int")
					sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) * sds/sqrt(ns)
				sds[is.na(sds)] <- 0
					if (error.bars == "none") 
					sds[1:length(sds)]<-0
				yrange <-if (is.null(ylim)) c(min(means - sds), max(means + sds)) else ylim
				levs.1 <- levels(factor)
				levs.2 <- levels(subfactor)
				n.levs.1 <- length(levs.1)
				n.levs.2 <- length(levs.2)
					if(is.null(pch))
					pch <- 1:n.levs.2
					else {
					if (length(pch)<n.levs.2) pch<-c(pch,1:n.levs.2)
					}
					if (length(xlab)==0) xlab<-name.variable
					if (length(ylab)==0) ylab<-paste(name.variable,"Mean")	
					if (scale=="numeric"){
					scale.xticks<-sub("[A-Za-z./]*", "", levs.1) 
					scale.xticks<-as.numeric(scale.xticks)		
						if (is.null(scale.levs)){
						scale.levs<-levs.1
							if (sum(is.na(scale.xticks))>0) scale<-"factor"
						}
						if (scale!="factor"){
						plot(c(min(scale.xticks), max(scale.xticks)),yrange,type="n",xlab=xlab,ylab=ylab,axes=FALSE,main=main)
							for (i in 1:n.levs.2){
								for (j in 1:n.levs.1){
								points(scale.xticks[j], means[j,i],type="b",pch=pch[i],cex=pointsize,col=col[i])
								if(sds[j,i]!=0) arrows(scale.xticks[j],means[j,i]-sds[j,i],scale.xticks[j],means[j,i]+sds[j,i],angle=90,code=3,col=col[i],lty=1,lwd=lwd,length=0.125)
								}	
							segments(scale.xticks[1:(length(levs.1)-1)],means[1:(length(levs.1)-1),i],scale.xticks[2:length(levs.1)],means[2:length(levs.1),i],col=col[i],lty=lty[i],lwd=lwd[i])
							}
						axis(1, at = scale.xticks, labels = scale.levs,cex.axis=cex.axis)
							for (i in 1:length(levs.1)) 
							if(sds[i]!=0) arrows(scale.xticks[i],means[i]-sds[i],scale.xticks[i],means[i]+sds[i],angle = 90,code=3,lty=1,length=0.125,col=col)
						}					
					}
					if (scale=="factor"){
					plot(c(1, n.levs.1 + 1),yrange,type="n",xlab=xlab,ylab=ylab,axes=FALSE,main=main)
					axis(1, at = 1:n.levs.1, labels = levs.1,cex.axis=cex.axis)
						for (i in 1:n.levs.2) {
							for (j in 1:n.levs.1){
							points(j,means[j,i],type="b",pch=pch[i],cex=pointsize,col=col[i],lty=1)
								if(sds[j,i]!=0)
								arrows(j,means[j,i]-sds[j,i],j,means[j,i]+sds[j,i],angle=90,code=3,col=col[i],lty=1,length=0.125)
							}
						for (j in 1:(n.levs.1-1))
						segments(j,means[j,i],j+1,means[j+1,i],col=col[i],lty=1)
						}
					}
				legend(legend.pos,c(legend.lab,levs.2),pch=c(0,pch),col=c(0,col),lty =c(0, lty))
				box()
				axis(2)
				}	
				#Strate pour le facteur				
				if (strata){
				eval(parse(text=paste0("niveau<-levels(",deparse(substitute(data)),"$",ex$list.factor,")")))			
				eval(parse(text=paste0("resp.",1:length(niveau),"<-subset(",deparse(substitute(data)),",",ex$list.factor,"=='",niveau[1:length(niveau)],"')")))
				n.row<-length(niveau)
				layout(matrix(c(1,seq(2,n.row+1)),n.row+1,1,byrow=TRUE),widths=28*n.row+4,heights=c(4,rep(28,n.row)),TRUE)				
				par(mar=c(0,0,0,0))
				plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
				text(.5,.5,main,cex=1.5)
				par(mar=c(4,4,0,2))
					for (i in 1:length(niveau)){
					eval(parse(text=paste0("data<-Repeated.data.frame(",paste(ex$list.response,collapse="+",sep=""),",time.variable='var',factor=",ex$list.factor,",data=resp.",i,")")))
					data<-na.omit(data)
					factor<-data$var
					responses <-data$var.value
					means <- tapply(responses, factor, mean)
					sds <- tapply(responses, factor, sd)
					ns <- tapply(responses, factor, length)
						if (error.bars == "se") sds <- sds/sqrt(ns)
						if (error.bars == "conf.int")
						sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) * sds/sqrt(ns)
					sds[is.na(sds)] <- 0
						if (error.bars == "none") 
						sds[1:length(sds)]<-0
					yrange <-if (is.null(ylim)) c(min(means - sds), max(means + sds)) else ylim
					levs <- levels(factor)
					n.levs <- length(levs)		
					n.levs.2<-0	
						if(is.null(pch))
						pch <- 1:n.levs
						else {
						if (length(pch)<n.levs) pch<-c(pch,1:n.levs)
						}
						if (length(ylab)==0) ylab<-paste(name.variable,"Mean")
						if (scale=="numeric"){
						scale.xticks<-sub("[A-Za-z./]*", "", levs) 
						scale.xticks<-as.numeric(scale.xticks)		
							if (is.null(scale.levs)){
							scale.levs<-levs
							if (sum(is.na(scale.xticks))>0) scale<-"factor"
							}
							if (scale!="factor"){
							plot(c(min(scale.xticks), max(scale.xticks)),yrange,type="n",xlab="",ylab=ylab,axes=FALSE)
							points(scale.xticks, means, type = "b", pch = pch[i], cex = pointsize,col=col[i])
							axis(1, at = scale.xticks, labels = levs,cex.axis=cex.axis)
							for (j in 1:n.levs) 
							if(sds[j]!=0) arrows(scale.xticks[j],means[j]-sds[j],scale.xticks[j],means[j]+sds[j],angle = 90,code=3,lty=1,length=0.125,col=col[i])
							}
						}
						if (scale=="factor"){
						scale.levs<-levs
						plot(c(1, n.levs),yrange,type = "n",ylab=ylab,xlab="",axes=FALSE)
						points(1:n.levs, means, type = "b", pch = pch[i], cex = pointsize,col=col[i])
						axis(1, at = 1:n.levs, labels = scale.levs,cex.axis=cex.axis)
							for (j in 1:n.levs) 
							if(sds[j]!=0) arrows(j,means[j]-sds[j],j,means[j]+sds[j],angle = 90,code=3,lty=1,length=0.125,col=col[i])
						}
					axis1<-par("usr")[2]-par("usr")[1]
					pos.xlab<-axis1/2+par("usr")[1]
						if(is.null(xlab))
						mtext(paste(name.variable,"for",ex$list.factor,"=",niveau[i]),1,at=pos.xlab,line=2.5,cex=0.8)		
						else
						mtext(xlab[i],1,at=pos.xlab,line=2.5,cex=0.8)							
					box()
					axis(2)						
					}				
				}			
			}
			#groupe et sous groupe
			if (ex$length.factor == 2) {
			#Strate deuxieme facteur
			eval(parse(text=paste0("niveau<-levels(",deparse(substitute(data)),"$",ex$list.factor[2],")")))			
			#premier indice facteur2, deuxieme facteur1, troisieme reponse (page) strate=facteur2
			eval(parse(text=paste0("resp.",1:length(niveau),"<-subset(",deparse(substitute(data)),",",ex$list.factor[2],"=='",niveau[1:length(niveau)],"')")))
			n.row<-length(niveau)
			layout(matrix(c(1,seq(2,n.row+1)),n.row+1,1,byrow=TRUE),widths=28*n.row+4,heights=c(4,rep(28,n.row)),TRUE)				
			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
			text(.5,.5,main,cex=1.5)
			par(mar=c(4,4,0,2))
				for (i in 1:length(niveau)){
				eval(parse(text=paste0("data<-Repeated.data.frame(",paste(ex$list.response,collapse="+",sep=""),",time.variable='var',factor=",paste(ex$list.factor,collapse="+"),",data=resp.",i,")")))
				data<-na.omit(data)
				factor<-data$var
				responses <-data$var.value
				eval(parse(text=paste0("subfactor<-data$",ex$list.factor[1])))
					if(is.null(legend.lab)) legend.lab<-paste(ex$list.factor[1])
				means <- tapply(responses, list(factor,subfactor), mean)
				sds <- tapply(responses, list(factor,subfactor), sd)
				ns <- tapply(responses, list(factor,subfactor), length)
					if (error.bars == "se") sds <- sds/sqrt(ns)
					if (error.bars == "conf.int")
					sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) * sds/sqrt(ns)
				sds[is.na(sds)] <- 0
					if (error.bars == "none") 
					sds[1:length(sds)]<-0
				yrange <-if (is.null(ylim)) c(min(means - sds), max(means + sds)) else ylim
				levs.1 <- levels(factor)
				levs.2 <- levels(subfactor)
				n.levs.1 <- length(levs.1)
				n.levs.2 <- length(levs.2)
					if(is.null(pch))
					pch <- 1:n.levs.2
					else {
					if (length(pch)<n.levs.2) pch<-c(pch,1:n.levs.2)
					}				
					if (length(ylab)==0) ylab<-paste(name.variable,"Mean")					
					if (scale=="numeric"){
					scale.xticks<-sub("[A-Za-z./]*", "", levs.1) 
					scale.xticks<-as.numeric(scale.xticks)		
						if (is.null(scale.levs)){
						scale.levs<-levs.1
						if (sum(is.na(scale.xticks))>0) scale<-"factor"
						}
						if (scale!="factor"){
						plot(c(min(scale.xticks), max(scale.xticks)),yrange,type="n",xlab="",ylab=ylab,axes=FALSE)
							for (j in 1:n.levs.2){
								for (k in 1:n.levs.1){
								points(scale.xticks[k], means[k,j],type="b",pch=pch[j],cex=pointsize,col=col[j])
								if(sds[k,j]!=0) arrows(scale.xticks[k],means[k,j]-sds[k,j],scale.xticks[k],means[k,j]+sds[k,j],angle=90,code=3,col=col[j],lty=1,lwd=lwd,length=0.125)
								}	
							segments(scale.xticks[1:(length(levs.1)-1)],means[1:(length(levs.1)-1),j],scale.xticks[2:length(levs.1)],means[2:length(levs.1),j],col=col[j],lty=lty[j],lwd=lwd[j])
							}
						axis(1, at = scale.xticks, labels = levs.1,cex.axis=cex.axis)
							for (j in 1:length(levs.1)) 
								if(sds[j]!=0) arrows(scale.xticks[j],means[j]-sds[j],scale.xticks[j],means[j]+sds[j],angle = 90,code=3,lty=1,length=0.125,col=col)
						}					
					}
					if (scale=="factor"){
					plot(c(1, n.levs.1 + 1),yrange,type="n",xlab="",ylab=ylab,axes=FALSE)
					axis(1, at = 1:n.levs.1, labels = levs.1,cex.axis=cex.axis)
						for (j in 1:n.levs.2) {
							for (k in 1:n.levs.1){
							points(k,means[k,j],type="b",pch=pch[j],cex=pointsize,col=col[j],lty=1)
								if(sds[k,j]!=0)
								arrows(k,means[k,j]-sds[k,j],k,means[k,j]+sds[k,j],angle=90,code=3,col=col[j],lty=1,length=0.125)
							}
							for (k in 1:(n.levs.1-1))
							segments(k,means[k,j],k+1,means[k+1,j],col=col[j],lty=1)
						}
					}
				axis1<-par("usr")[2]-par("usr")[1]
				pos.xlab<-axis1/2+par("usr")[1]
					if(is.null(xlab))
					mtext(paste(name.variable,"for",ex$list.factor[2],"=",niveau[i]),1,at=pos.xlab,line=2.5,cex=0.8)		
					else
					mtext(xlab[i],1,at=pos.xlab,line=2.5,cex=0.8)		
				box()
				axis(2)	
				legend(legend.pos,c(legend.lab,levs.2),pch=c(0,pch),col=c(0,col),lty =c(0, lty))				
				}
			}
		}
	}
par(op)
invisible(NULL)
}


############################################################################
#Representation test t (simple, independant, apparie)
ttest.graph<-function (formula,mu=NULL,conf.level=0.95,data=parent.frame(),main=NULL,exp.lab=1,ylab=NULL,xlab=NULL,leg=TRUE,leg.title=NULL,exp.leg=1,color=NULL )
{
op <- par(no.readonly = TRUE)
	if (!is.null(mu)){
	listvar<-deparse(substitute(formula))
    response<-eval(parse(text=paste("with(data,",listvar,")",sep="")))
    valid<-complete.cases(response)
    response<-response[valid]
    stdeviation<-sd(response)
    estimated.mean<-mean(response)
    layout(matrix(c(1,2,3),3,1,byrow=TRUE), 3, c(.2,.6,2.2), TRUE)
    par(mar=c(0,0,0,0))
    plot(c(0,1),c(0,1),type="n",axes=F)
		if (is.null(main))
		main<-"t test representation"
    text(.5,.5,main,cex=2)
    par(mar=c(0,4,0,2))
    histo<-hist(response,breaks=18,plot=F)
    miny<-min(histo$breaks)
    maxy<-max(histo$breaks)
    supx<-max(histo$counts)
    scalex<-length(histo$counts)/(maxy-miny)
    seq.x<-seq(0,length(histo$counts),length=length(histo$breaks))
    seq.axis<-pretty(seq.x/scalex+miny)
    seq.x<-(seq.axis-miny)*scalex
	if (is.null(color)) color="turquoise"
    boxplot(response,axes=F,horizontal=TRUE,ylim=c(miny,maxy),xlim=c(0,3),col=color)
	test<-t.test(response,alternative ="two.sided",mu=mu,conf.level=conf.level)
	rect(test$conf.int[1],1.5,test$conf.int[2],2,col=color)
	text(estimated.mean,2.5,"Confidence interval")
    abline(v=par("usr")[2])
    abline(v=par("usr")[1])
    abline(h=par("usr")[4])
    abline(v=mu,lwd=2,lty=2)
    par(mar=c(5,4,0,2))
		if (is.null(xlab))
		xlab<-listvar
    barplot(histo$counts,space=0,xlab=xlab,ylab="Counts",col=color,cex.lab=exp.lab)
    axis(1,seq.x,seq.axis)
    abline(h=par("usr")[3])
    abline(v=par("usr")[2])
    abline(v=par("usr")[1])
    m.histo<-scalex*(estimated.mean-miny)
    sd.histo<-scalex*stdeviation
    normx<-seq(0,length(histo$breaks),length=500)
    densityx<-dnorm(normx,m.histo,sd.histo)
    densityx<-densityx/max(densityx)*max(histo$counts)
    lines(normx,densityx,lwd=2,col="black")
    abline(v=scalex*(mu-miny),lwd=2,lty=2)
		if(leg) legend("topright",paste("Estimated mean","=",round(estimated.mean,2)))
	}
	if (is.null(mu)){
	test.formula<-deparse(substitute(formula))
	test.formula<-unlist(strsplit(test.formula," "))[2]
		if (test.formula=="~"){
		f <- formula(formula)
		listvar <- as.character(attr(terms(f), "variables"))[-1]
		response<-listvar[1]
		factor<-listvar[2]
		valid <- complete.cases(with(data,eval(parse(text=response))),with(data,eval(parse(text=factor))))
		response <- with(data,eval(parse(text=response)))[valid]
			if (!is.numeric(response))
			stop(gettext("Variable1 must be numeric.",domain="R-RcmdrPlugin.TestGraph"))
		factor <- with(data,eval(parse(text=factor)))[valid]
			if (!is.factor(factor))
			stop(gettext("Variable2 must be factor.",domain="R-RcmdrPlugin.TestGraph"))
		factor<-factor(factor)
		levs<-levels(factor)
			if (length(levs)>2)
			stop(gettext("Maximum 2 levels for factor.",domain="R-RcmdrPlugin.TestGraph"))
		table <- tapply(response, factor, function(x) x)
		table.mean <- tapply(response, factor, mean)
		table.sd <- tapply(response, factor, sd)
		table.n<-c(length(table[[1]]),length(table[[2]]))
		factor1<-hist(table[[1]],breaks=25,plot=F)
		factor2<-hist(table[[2]],breaks=25,plot=F)
		length.width<-min(mean(diff(factor1$breaks)),mean(diff(factor2$breaks)))
		max.width<-max(factor1$breaks,factor2$breaks)
		min.width<-min(factor1$breaks,factor2$breaks)
		widths<-seq(min.width,max.width,by=length.width)
		factor1<-hist(table[[1]],breaks=widths,plot=F)
		factor2<-hist(table[[2]],breaks=widths,plot=F)
		miny<-min(factor1$breaks,factor2$breaks)
		maxy<-max(factor1$breaks,factor2$breaks)
		supx<-max(factor1$counts,factor2$counts)
		scaley<-length(widths)/(maxy-miny)
		m.histo1<-scaley*(table.mean[1]-miny)
		m.histo2<-scaley*(table.mean[2]-miny)
		sd.histo1<-scaley*table.sd[1]
		sd.histo2<-scaley*table.sd[2]
		normx1<-seq(0,length(widths),length=500)
		densityx1<-dnorm(normx1,m.histo1,sd.histo1)
		densityx1<-densityx1/max(densityx1)*max(factor1$counts)
		normx2<-seq(0,length(widths),length=500)
		densityx2<-dnorm(normx2,m.histo2,sd.histo2)
		densityx2<-densityx2/max(densityx2)*max(factor2$counts)
		layout(matrix(c(1,1,1,1,2,3,4,5,6,6,7,7),3,4,byrow=TRUE), c(.4,1.1,1.1,.4), c(.2,2.6,.2), TRUE)
		par(mar=c(0,0,0,0))
		plot(c(0,1),c(0,1),type="n",axes=F)
			if (is.null(main))
			main<-"t test representation"
		text(.5,.5,main,cex=2)
		par(mar=c(5,5,0,0))
		if (is.null(color)) colbox<-palette()[1]
		boxplot(table[[1]],ylim=c(miny,maxy),axes=F,ylab=ifelse(is.null(ylab),listvar[1],ylab),cex.lab=exp.lab,col=colbox)
		abline(h=par("usr")[3])
		abline(h=par("usr")[4])
		abline(v=par("usr")[1])
		axis(2)
		par(mar=c(5,0,0,0))
			if (is.null(color)) coldraw=palette()
		barplot(factor1$counts,space=0,xlim=c(supx,0),horiz=TRUE,xlab="Count",col=coldraw[1],cex.lab=exp.lab)
			if (leg) legend("topleft",paste("Mean =",round(table.mean[1],2)))
		lines(densityx1,normx1,lwd=2,col=coldraw[1])
		abline(h=par("usr")[3])
		abline(v=par("usr")[2])
		abline(h=par("usr")[4])
		barplot(factor2$counts,space=0,xlim=c(0,supx),horiz=TRUE,xlab="Count",col=coldraw[2],cex.lab=exp.lab)
			if(leg) legend("topright",paste("Mean =",round(table.mean[2],2)))
		lines(densityx2,normx2,lwd=2,col=coldraw[2])
		abline(h=par("usr")[3])
		abline(h=par("usr")[4])
		par(mar=c(5,0,0,4))
		if (is.null(color)) colbox<-palette()[2]
		boxplot(table[[2]],ylim=c(miny,maxy),axes=F,col=colbox)
		axis(4)
		abline(v=par("usr")[2])
		abline(h=par("usr")[3])
		abline(h=par("usr")[4])
		par(mar=c(0,0,0,0))
		plot(c(0,1),c(0,1),type="n",axes=F)
			if (!is.null(leg.title)) 
			legend("top",bty="n",title=leg.title,names(table)[1],fill=coldraw[1],cex=exp.leg)
			else 
			legend("top",bty="n",title=listvar[2],names(table)[1],fill=coldraw[1],cex=exp.leg)
		plot(c(0,1),c(0,1),type="n",axes=F)
			if (!is.null(leg.title)) 
			legend("top",title=leg.title,names(table)[2],bty="n",fill=coldraw[2],cex=exp.leg)    
			else 
			legend("top",title=listvar[2],names(table)[2],bty="n",fill=coldraw[2],cex=exp.leg)
		}
		if (test.formula=="+"){
		listvar<-deparse(substitute(formula))
		listvar<-unlist(strsplit(listvar," "))[c(1,3)]
		valid <- complete.cases(with(data,eval(parse(text=listvar[1]))),with(data,eval(parse(text=listvar[2]))))
		response1<-with(data,eval(parse(text=listvar[1])))[valid]
		response2<-with(data,eval(parse(text=listvar[2])))[valid]
		par(mar=c(5,4,4,4))
			if (is.null(xlab))
			xlab<-"Variable"
			if (is.null(ylab))
			ylab<-"Value"
		plot(c(0,1),c(min(response1,response2),max(response1,response2)),type="n",xlab=xlab,ylab=ylab,axes=FALSE,cex.lab=exp.lab)
		box()
		axis(2)
			if (is.null(color)) color<-"blue"
			axis(1,c(par("usr")[1],par("usr")[2]),c(listvar[1],listvar[2]))
			for (i in 1:length(response1))
			lines(c(par("usr")[1],par("usr")[2]),c(response1[i],response2[i]),col=color)
			if (is.null(main))
			main<-"Paired t test representation"
		title(main)
		}
	}
par(op)  
}
############################################################################
#Representation test t
power.graph<-function (formula,data=parent.frame(),true.mean=NULL,main="Mean distribution",alternative = c("two.sided", "less","greater"),alpha = 0.05)
{
op <- par(no.readonly = TRUE)
#formule avec groupe
    if (length(grep("~",deparse(substitute(formula))))!=0){
    f <- formula(formula)
    listvar <- as.character(attr(terms(f), "variables"))[-1]
    response<-listvar[1]
    listresponse<-" "
  #formule avec groupe repetee
	 if (length(grep(" +",deparse(substitute(response))))!=0){
stop("Just two variables or one variable and one factor")
	 }
     }
#formule sans groupe
    if (length(grep("~",deparse(substitute(formula))))==0) {
    formula<-paste(deparse(substitute(formula)),"~ factor1")
    f <- formula(formula)
    listvar <- as.character(attr(terms(f), "variables"))[-1]
    response<-listvar[1]
  #formule sans groupe repetee
      if (length(grep(" +",deparse(substitute(response))))!=0){
	response<-paste("~",response)
	response<-formula(response)
      listresponse <- as.character(attr(terms(response), "variables"))[-1]
 	}
      if (length(grep(" +",deparse(substitute(response))))==0)listresponse<-" "
    factor1<-NA
    }
if (!is.null(alpha) && !is.numeric(alpha) || any(0 >alpha | alpha > 1))
  stop(sQuote("alpha"), " must be numeric in [0, 1]")
    old.par <- par(mfrow = c(2, 1), oma = c(0, 0, 3.1, 0))
    on.exit(par(old.par))
    alternative <- match.arg(alternative)
    comparison <- switch(alternative,less="less than",two.sided="different than",greater="greater than")
    alpha <- as.numeric(alpha)
    sig.level<-switch(alternative,less=alpha,two.sided=alpha/2,greater=alpha)
    col.less<-switch(alternative,less="pink",two.sided="pink",greater="white")
    col.greater<-switch(alternative,less="white",two.sided="pink",greater="pink")
    legend.less<-switch(alternative,less="--> rejection\n      region",two.sided="--> rejection\n      region",greater="")
    legend.greater<-switch(alternative,less="",two.sided="rejection  \nregion <--",greater="rejection  \nregion <--")
    col1.less<-switch(alternative,less="lightblue",two.sided="lightblue",greater="white")
    col1.greater<-switch(alternative,less="white",two.sided="lightblue",greater="lightblue")
    axe.less<-switch(alternative,less="black",two.sided="black",greater="white")
    axe.greater<-switch(alternative,less="white",two.sided="black",greater="black")
    if (listvar[2]=="factor1") {
#Graphiques
  #formule sans groupe
type<-"one.sample"
	#Une variable
	  if (length(listresponse)==1) {
if (missing(true.mean)) stop("Choose true mean")
valid <- complete.cases(with(data,eval(parse(text=response))))
response <- with(data,eval(parse(text=response)))[valid]
n<-length(response)
mean<-mean(response)
delta<-mean-true.mean
sigma<-sd(response)
se<-sigma/sqrt(n)
mean1<-mean
mean2<-true.mean
title.graph1<-paste(listvar[1])
title.graph2<-true.mean
xlab.title1<-paste("Distribution for mean =",round(mean1,2))
xlab.title2<-paste("Distribution for mean =",true.mean)
test<-t.test(response, alternative=alternative, mu=true.mean)
		}
  #formule sans groupe et deux variables
	  if (length(listresponse)>1)	{
if (length(listresponse)>2) stop ("Just one or two variables.")
valid <- complete.cases(with(data,eval(parse(text=listresponse[1]))),with(data,eval(parse(text=listresponse[2]))))
response1<-with(data,eval(parse(text=listresponse[1])))[valid]
response2<-with(data,eval(parse(text=listresponse[2])))[valid]
n<-length(response1)
delta<-mean(response1-response2)
sigma<-sd(response1-response2)
se<-sigma/sqrt(n)
mean1<-delta
mean2<-0
title.graph1<-paste(listresponse[1]," - ",listresponse[2],sep="")
title.graph2<-"0"
xlab.title1<-paste("Distribution for mean =",round(delta,2))
xlab.title2<-"Distribution for mean = 0"
test<-t.test(response1,response2,alternative=alternative,paired=TRUE)
	}
options(warn=0)
power<-pwr.t.test(n=n,delta=delta,sigma=sigma,sig.level=alpha,type=type,alternative=alternative)
power<-power[[5]]
options(warn=1)
  }
#formule avec groupe
      if (listvar[2]!="factor1"){
if (length(listresponse)>1)
stop("Just two variable or one variable and one factor")
valid <- complete.cases(with(data,eval(parse(text=listvar[1]))),with(data,eval(parse(text=listvar[2]))))
response<- with(data,eval(parse(text=listvar[1])))[valid]
factor<- with(data,eval(parse(text=listvar[2])))[valid]
if (length(levels(factor))!=2)
stop ("Just one factor with two levels")
mean<-tapply(response,factor,mean)
n<-tapply(response,factor,function(x)length(!is.na(x)))
sd<-tapply(response,factor,sd)
delta<-mean[1]-mean[2]
sigma<-sqrt(((n[1]-1)*sd[1]^2+(n[2]-1)*sd[2]^2)/(n[1]+n[2]-2))
se<-sigma*sqrt(1/n[1]+1/n[2])
mean1<-mean[1]
mean2<-mean[2]
options(warn=0)
power<-pwr.t2n.test(n1=n[1],n2=n[2],delta=delta,sigma=sigma,sig.level=alpha,alternative=alternative)
power<-power[[6]]
options(warn=1)
title.graph1<-paste(levels(factor)[1])
title.graph2<-paste(levels(factor)[2])
xlab.title1<-paste("Distribution for mean =",round(mean1,2))
xlab.title2<-paste("Distribution for mean = ",round(mean2,2))
test<-t.test(formula,alternative=alternative,data=data)
  }
t<-test[[1]]
p<-test[[3]]
if (p<.001) p<-"p < 0.001"
else p<-paste("p =",round(p,3))
diff<-mean1-mean2
rangex<-c(qnorm(.00009,m=min(mean1,mean2),sd=se),qnorm(.9999,m=max(mean1,mean2),sd=se))
x <- seq(rangex[1],rangex[2], length = 200)
layout( matrix(c(1,1,2,3,4,5),ncol=2,byrow=TRUE),width=c(1,.2),heights=c(.2,1,1))
par(mar=c(0,0,0,0))
plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
text(.5,.9,paste(main," (t = ",round(t,2),", ",p,")",sep=""),cex=2)
if (length(n)==1)
text(.5,.3,paste("n =",n,"  sd =",round(sigma,3),"  se =", round(se, 3),"  difference =",round(diff,2)),cex=2)
if (length(n)==2)
text(.5,.3,paste("n1 =",n[1]," n2 =",n[2],"  sd =",round(sigma,3),"  se =",round(se,3),"  difference =",round(diff,2)),cex=2)
par(mar=c(4,4,4,0))
plot(x, dnorm(x, mean1, se), type = "n", ylim = c(0, max(dnorm(x,mean1, se))*7/6),
ylab = "",xlab=xlab.title1,main = paste("Null Distribution (",title.graph1," = ",title.graph2,")",sep=""))
r <- qnorm(1 - sig.level, mean1, se)
r1 <- qnorm(sig.level,mean1,se)
polygon(c(r, r, x[x > r]), c(0, dnorm(c(r, x[x > r]), mean1,se)),border=NA, col = col.less)
polygon(c(x[x < r1],r1,r1), c(dnorm(c(x[x < r1],r1),  mean1,se),0),border=NA, col = col.greater)
text(r, max(dnorm(x, mean1, se)*15/14) , legend.less,adj = 0)
text(r1, max(dnorm(x, mean1, se)*15/14) , legend.greater,pos=2,adj = 0)
abline(v=r,col=axe.less)
abline(v=r1,col=axe.greater)
lines(x, dnorm(x, mean1, se), col = "red")
abline(h = 0)
par(mar=c(0,0,0,0))
plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
legend(0,.5,bty = "n",fill="pink",legend =paste("Alpha =",signif(sig.level,3)))
par(mar=c(4,4,4,0))
plot(x, dnorm(x, mean1, se), type = "n", ylim = c(0, max(dnorm(x,mean1, se))*7/6),
 ylab = "",xlab=xlab.title2,main = paste("Alternative Distribution (",title.graph1," ",comparison," ",title.graph2,")",sep=""))
polygon(c(r, r, x[x > r], max(x)), c(0, dnorm(c(r, x[x >r]), mean2, se), 0), col = col1.less)
polygon(c(x[x < r1],r1,r1), c(dnorm(c(x[x < r1],r1),  mean2,se),0),border=NA, col = col1.greater)
text(r, max(dnorm(x, mean1, se)*15/14) , legend.less,adj = 0)
text(r1, max(dnorm(x, mean1, se)*15/14) , legend.greater,pos=2,adj = 0)
abline(v=r,col=axe.less)
abline(v=r1,col=axe.greater)
lines(x, dnorm(x, mean2, se), col = "blue")
abline(h = 0)
par(mar=c(0,0,0,0))
plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
legend(0,.5,bty = "n",fill="lightblue",legend =paste("Power =",round(power,3)))
par(op)
#FIN
}
############################################################################
#Representation test Chi deux
graphChisq<-function(tab,percent=c("row","column","total","OR"),test=c("chisq","fisher"),correct=FALSE,graph=c("pie","histogram"),res=FALSE,expect=FALSE,
main=Test,cex.main=2,cex.table=1,left=par("usr")[1],top=par("usr")[4],pages=1,diagnostic=c("Se","Sp"),pos.neg.ref=c("POS","NEG"),pos.neg.cont=c("POS","NEG"),color=NULL)
{
op <- par(no.readonly = TRUE)
if (length(dimnames(tab))==2) group<-FALSE
if (length(dimnames(tab))==3) group<-TRUE
graph <- match.arg(graph)
	if(!group){
	n.row<-dim(tab)[1]
	n.col<-dim(tab)[2]
	rowper <- aperm(apply(tab, 1, function(x) x/sum(x) * 100))
	oddratio <- rowper[, 2]/rowper[, 1]
	oddratio <- oddratio[2]/oddratio[1]
	sdoddratio <- sqrt(sum(1/tab))
	lowerlimit <- oddratio * exp(-qnorm(0.975) * sdoddratio)
	upperlimit <- oddratio * exp(qnorm(0.975) * sdoddratio)
	rowper<-round(rowper,digit=1)
	colper<-apply(tab,2, function(x) round(x/sum(x)*100,digit=1))
	total.rowcount<-apply(tab,1, function(x) sum(x))
	total.colcount<-apply(tab,2, function(x) sum(x))
	total.count<-sum(total.rowcount)
	sum<-apply(tab,1, function(x) (0*x+1)*sum(x))
	sum<-apply(sum,1, function(x) (0*x+1)*sum(x))
	totalper<-round(tab/sum*100,digit=1)
	total.rowtot<-apply(totalper,1, function(x) sum(x))
	test2<-chisq.test(tab,correct=correct)	
		if(dim(tab)[1]==2 & dim(tab)[2]==2){
		test.position<-1:2
		num.col.pos<-test.position[colnames(tab)==pos.neg.ref[1]]
		num.row.pos<-test.position[rownames(tab)==pos.neg.cont[1]]
		Se<-tab[num.row.pos,num.col.pos]/sum(tab[,num.col.pos])
		text.Se<-paste0("Se = ",round(Se,3)*100," %")
		num.col.neg<-test.position[colnames(tab)==pos.neg.ref[2]]
		num.row.neg<-test.position[rownames(tab)==pos.neg.cont[2]]
		Sp<-tab[num.row.neg,num.col.neg]/sum(tab[,num.col.neg])
		text.Sp<-paste0("Sp = ",round(Sp,3)*100," %")
		}
	Test<-NULL
		if (any(test=="chisq")){
			if (test2[[3]]<0.001) Test<-"'p < 0.001'"
			if (test2[[3]]>0.001) Test<-paste("expression(p==",round(test2[[3]],digits=3),")",sep="")
		text.chi2<-paste("expression(Chi^2==",round(test2[[1]],2),")")
		}
		else text.chi2<-NULL
		if (any(test=="fisher")){
		test1<-fisher.test(tab)
			if (test1[[1]]<0.001) text.fisher<-"'p < 0.001 (Fisher's Test)'"
			if (test1[[1]]>0.001) text.fisher<-paste("p=",round(test1[[1]],digits=3)," (Fisher's Test)",sep="")
		}
		else text.fisher<-NULL
		if (any(percent=="OR")){
		text.OR<-paste0("'Odds Ratio=",round(oddratio,digits=2)," ; Conf. interval=[",round(lowerlimit,1),",",round(upperlimit,1),"]'")
		}
		else text.OR<-NULL		
		if(expect){
		tab.freq<-round(test2$expected,1)
		tab.freq<-TotalTable(tab.freq)
		}
		if(res){
		tab.res<-round(test2$residuals,2)
		tab.res<-TotalTable(tab.res)
		}		
	draw<-seq(1:n.row)
	draw<-cut.page(draw,pages=pages)
		for(j in 1:pages){
			if (pages>1) x11()
		matrix<-c(paste(seq(1:length(draw[[j]]))),length(draw[[j]])+1,paste(seq(1:length(draw[[j]]))+length(draw[[j]])+1),length(draw[[j]])+1)
		layout(matrix(matrix,length(draw[[j]])+1,2,byrow=FALSE),c(1,1),c(rep(1,length(draw[[j]])),.3))
			if (graph=="pie"){
			par(mar = c(0,0,0,0))
				if(is.null(color)) textcol<-"rainbow(n.col)"
				else textcol<-paste0("c(",paste0("\"",color,"\"",collapse=","),")")
			eval(parse(text=paste("pie(tab[",draw[[j]],",],col=",textcol,")",sep="")))
			}
			if (graph=="histogram"){
			par(mar = c(4,4,1,0))
				for (k in 1: length(draw[[j]])){
					if(is.null(color)) textcol<-"rainbow(n.col)"
					else textcol<-paste0("c(",paste0("\"",color,"\"",collapse=","),")")				
				eval(parse(text=paste("barplot(cbind(tab[",draw[[j]][k],",]/sum(tab[",draw[[j]][k],",]),0),beside=F,axes=F,col=",textcol,",space=0)",sep="")))
				eval(parse(text=paste0("legend('topright',colnames(tab),fill=",textcol,")")))
				}
			}
			par(mar = c(0,0,0,0))
		plot(c(0,20),c(0,10),type='n',axe=FALSE)
			if (j==pages){
				if (!is.null(text.chi2)){
				ligne<-4.5
					if (is.null(text.OR)){
					eval(parse(text=paste("legend(5.5,10,",text.chi2,",cex=2,bty='n')")))
					eval(parse(text=paste("legend(9.5,10,",Test,",cex=2,bty='n')")))
					}
					else{
					eval(parse(text=paste("legend(-1.5,10,",text.chi2,",cex=2,bty='n')")))
					eval(parse(text=paste("legend(1.5,10,",Test,",cex=2,bty='n')")))
					eval(parse(text=paste("legend(5.5,10,",text.OR,",cex=2,bty='n')")))
					}
				}
				else ligne<-0
				if (!is.null(text.fisher)){
				coord<-legend('top',text.fisher,cex=2,bty='n',plot=F)
				legend(coord$rect$left,coord$rect$top-ligne,text.fisher,cex=2,bty="n")
				}
			} 
		eval(parse(text=paste("graphtab<-c(",paste(paste('"tab.', draw[[j]], '"', sep=""), collapse=","),")")))
		eval(parse(text=paste(graphtab,"<-rbind(Count=as.character(tab[",draw[[j]],",]))",sep="")))
		eval(parse(text=paste(graphtab,"<-cbind(",graphtab,",Total=as.character(",total.rowcount,"))",sep="")))
			if(expect)
			eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Expected=tab.freq[",draw[[j]],",])",sep="")))
			if(res)
			eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Residuals=tab.res[",draw[[j]],",])",sep="")))
			if (any(percent=="row"))
			eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Row.pct=c(rowper[",draw[[j]],",],'100'))",sep="")))
			if (any(percent=="column"))
			eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Column.pct=c(colper[",draw[[j]],",],''))",sep="")))
			if (any(percent=="total"))
			eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Total.pct=c(totalper[",draw[[j]],",],",total.rowtot,"))",sep="")))
		eval(parse(text=paste("colnames(",graphtab,")<-c(",paste0("'",colnames(tab),"'",collapse=","),",'Total')",sep="")))
		eval(parse(text=paste("names(dimnames(",graphtab,"))[2]<-'",names(dimnames(tab))[2],"'",sep="")))
		eval(parse(text=paste("names(dimnames(",graphtab,"))[1]<-paste(names(dimnames(tab))[1],'=',rownames(tab)[",draw[[j]],"],sep='')",sep="")))
			for (i in draw[[j]]){
			plot(c(0,10),c(0,10),type='n',axe=FALSE)
			graphTable(eval(parse(text=paste("tab.",i,sep=""))),title.pos="top",left=left,top=top,cex=cex.table)
				if (i==1 & any(diagnostic=="Se")){
				sizeFig<-max(diff(par("usr")))
				text(sizeFig/2,1,text.Se,cex=1.5)
				}
				if (i==2 & any(diagnostic=="Sp")){
				sizeFig<-max(diff(par("usr")))
				text(sizeFig/2,1,text.Sp,cex=1.5)
				}
			}
		}
	}
	if (group){
	strata<-dimnames(tab)[[3]]
	nbre.lev<-length(strata)
	n.row<-dim(tab)[1]
	n.col<-dim(tab)[2]
		for (k in 1:nbre.lev){
		subtab<-tab[,,k]
		rowper<-aperm(apply(subtab,1,function(x) x/sum(x)*100))
		colper<-apply(subtab,2, function(x) round(x/sum(x)*100,digit=1))
		oddratio <- rowper[, 2]/rowper[, 1]
		oddratio <- oddratio[2]/oddratio[1]
		sdoddratio <- sqrt(sum(1/tab))
		lowerlimit <- oddratio * exp(-qnorm(0.975) * sdoddratio)
		upperlimit <- oddratio * exp(qnorm(0.975) * sdoddratio)
		rowper<-round(rowper,digit=1)
		total.rowcount<-apply(subtab,1, function(x) sum(x))
		total.colcount<-apply(subtab,2, function(x) sum(x))
		total.count<-sum(total.rowcount)
		sum<-apply(subtab,1, function(x) (0*x+1)*sum(x))
		sum<-apply(sum,1, function(x) (0*x+1)*sum(x))
		totalper<-round(subtab/sum*100,digit=1)
		total.rowtot<-apply(totalper,1, function(x) sum(x))
		test2<-chisq.test(subtab,correct=correct)
		Test<-NULL
			if (any(test=="chisq")){
				if (test2[[3]]<0.001) Test<-"p < 0.001"
				if (test2[[3]]>0.001) Test<-paste("expression(p==",round(test2[[3]],digits=3),")",sep="")
			text.chi2<-paste("expression(Chi^2==",round(test2[[1]],2),")")
			}
			else text.chi2<-NULL
			if (any(test=="fisher")){
			test1<-fisher.test(subtab)
				if (test1[[1]]<0.001) text.fisher<-"p < 0.001 (Fisher's Test)"
				if (test1[[1]]>0.001) text.fisher<-paste("p=",round(test1[[1]],digits=3)," (Fisher's Test)",sep="")
			}
			else text.fisher<-NULL
			if (any(percent=="OR")){
			text.OR<-paste0("'Odds Ratio=",round(oddratio,digits=2)," ; Conf. interval=[",round(lowerlimit,1),",",round(upperlimit,1),"]'")
			}
			else text.OR<-NULL			
			if(expect){
			tab.freq<-round(test2$expected,1)
			tab.freq<-TotalTable(tab.freq)
			}
			if(res){
			tab.res<-round(test2$residuals,2)
			tab.res<-TotalTable(tab.res)
			}				
		draw<-seq(1:n.row)
		draw<-cut.page(draw,pages=pages)
			for(j in 1:pages){
				if (pages>1) x11()
			matrix<-c(paste(seq(1:length(draw[[j]]))),length(draw[[j]])+1,paste(seq(1:length(draw[[j]]))+length(draw[[j]])+1),length(draw[[j]])+1)
			layout(matrix(matrix,length(draw[[j]])+1,2,byrow=FALSE),c(1,1),c(rep(1,length(draw[[j]])),.3))
				if (graph=="pie"){
				par(mar = c(0,0,0,0))
					if(is.null(color)) textcol<-"rainbow(n.col)"
					else textcol<-paste0("c(",paste0("\"",color,"\"",collapse=","),")")					
				eval(parse(text=paste("pie(subtab[",draw[[j]],",],col=",textcol,")",sep="")))
				}
				if (graph=="histogram"){
				par(mar = c(4,4,1,0))
					for (n in 1: length(draw[[j]])){
						if(is.null(color)) textcol<-"rainbow(n.col)"
						else textcol<-paste0("c(",paste0("\"",color,"\"",collapse=","),")")	
					eval(parse(text=paste("barplot(cbind(subtab[",draw[[j]][n],",]/sum(subtab[",draw[[j]][n],",]),0),beside=F,axes=F,col=",textcol,",space=0)",sep="")))
					eval(parse(text=paste0("legend('topright',colnames(subtab),fill=",textcol,")")))
					}
				}
			par(mar = c(0,0,0,0))
			plot(c(0,20),c(0,10),type='n',axe=FALSE)
				if (j==pages){
					if (!is.null(text.chi2)){
					ligne<-4.5
						if (is.null(text.OR)){
						eval(parse(text=paste("legend(5.5,10,",text.chi2,",cex=2,bty='n')")))
						eval(parse(text=paste("legend(9.5,9.2,",Test,",cex=2,bty='n')")))
						}
						else{
						eval(parse(text=paste("legend(-1.5,10,",text.chi2,",cex=2,bty='n')")))
						eval(parse(text=paste("legend(1.5,9.2,",Test,",cex=2,bty='n')")))
						eval(parse(text=paste("legend(5.5,9.2,",text.OR,",cex=2,bty='n')")))
						}
					}
					else ligne<-0
					if (!is.null(text.fisher)){
					coord<-legend('top',text.fisher,cex=2,bty='n',plot=F)
					legend(coord$rect$left,coord$rect$top-ligne,text.fisher,cex=2,bty="n")
					}	
				} 
			eval(parse(text=paste("graphtab<-c(",paste(paste('"tab.', draw[[j]], '"', sep=""), collapse=","),")")))
			eval(parse(text=paste(graphtab,"<-rbind(Count=as.character(subtab[",draw[[j]],",]))",sep="")))
			eval(parse(text=paste(graphtab,"<-cbind(",graphtab,",Total=as.character(",total.rowcount,"))",sep="")))
				if(expect)
				eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Expected=tab.freq[",draw[[j]],",])",sep="")))
				if(res)
				eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Residuals=tab.res[",draw[[j]],",])",sep="")))			
				if (any(percent=="row"))
				eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Row.pct=c(rowper[",draw[[j]],",],'100'))",sep="")))
				if (any(percent=="column"))
				eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Column.pct=c(colper[",draw[[j]],",],''))",sep="")))
				if (any(percent=="total"))
				eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Total.pct=c(totalper[",draw[[j]],",],",total.rowtot,"))",sep="")))
			eval(parse(text=paste("colnames(",graphtab,")<-c(",paste0("'",colnames(subtab),"'",collapse=","),",'Total')",sep="")))
			eval(parse(text=paste("names(dimnames(",graphtab,"))[2]<-'",names(dimnames(subtab))[2]," for ",strata[k],"'",sep="")))
			eval(parse(text=paste("names(dimnames(",graphtab,"))[1]<-paste(names(dimnames(subtab))[1],'=',rownames(subtab)[",draw[[j]],"],sep='')",sep="")))
				for (i in draw[[j]]){
				plot(c(0,10),c(0,10),type='n',axe=FALSE)
				graphTable(eval(parse(text=paste("tab.",i,sep=""))),title.pos="top",left=left,top=top,cex=cex.table)
				}
			}
		if (k<nbre.lev) x11()
		}
	}
par(op)	
}
############################################################################
#Representation test mantel-Haenszel
graphMantelHaen<-function(tab,percent=c("row","column","OR"),test=c("chisq","mantelhaen"),graph=c("pie","histogram"),alternative = c("two.sided", "less", "greater"),
res=FALSE,expect=FALSE,correct=FALSE,cex.table=1,left=par("usr")[1],title.pos=c("top","left"),pages.chisq=1,pages.mantelhaen=1)
{
strata<-c(paste(names(dimnames(tab))[[3]],"=",dimnames(tab)[[3]],sep=""))
eval(parse(text=paste("subtab<-rbind(",paste(paste("tab[,,",sep=""),1:length(strata),"]",collapse=",",sep=""),")",sep="")))
rowper<-aperm(apply(subtab,1,function(x) round(x/sum(x)*100,digit=1)))
colper<-apply(subtab,2, function(x) round(x/sum(x)*100,digit=1))
graph <- match.arg(graph)
if (missing(title.pos)) title.pos<-"top"
Test<-NULL
tab.squared<-apply(tab,c(1,2),sum)
n.row<-dim(tab.squared)[1]
n.col<-dim(tab.squared)[2]
rowperx <- aperm(apply(tab.squared, 1, function(x) x/sum(x) * 100))
oddratio <- rowperx[, 2]/rowperx[, 1]
oddratio <- oddratio[2]/oddratio[1]
sdoddratio <- sqrt(sum(1/tab.squared))
lowerlimit <- oddratio * exp(-qnorm(0.975) * sdoddratio)
upperlimit <- oddratio * exp(qnorm(0.975) * sdoddratio)
rowperx<-round(rowperx,digit=1)
colperx<-apply(tab.squared,2, function(x) round(x/sum(x)*100,digit=1))
total.rowcount<-apply(tab.squared,1, function(x) sum(x))
total.colcount<-apply(tab.squared,2, function(x) sum(x))
total.count<-sum(total.rowcount)
sum<-apply(tab.squared,1, function(x) (0*x+1)*sum(x))
sum<-apply(sum,1, function(x) (0*x+1)*sum(x))
totalperx<-round(tab.squared/sum*100,digit=1)
total.rowtot<-apply(totalperx,1, function(x) sum(x))
test2<-chisq.test(tab.squared,correct=correct)
	if (any(test=="chisq")){
		if (test2[[3]]<0.001) Test<-"expression(p < 0.001)"
		if (test2[[3]]>0.001) Test<-paste("expression(p==",round(test2[[3]],digits=3),")",sep="")
	text.chi2<-paste("expression(Chi^2==",round(test2[[1]],2),")")
		if (any(percent=="OR")){
		text.OR<-paste0("'Odds Ratio=",round(oddratio,digits=2)," ; Conf. interval=[",round(lowerlimit,1),",",round(upperlimit,1),"]'")
		}
		else text.OR<-NULL	
		if(expect){
		tab.freq<-round(test2$expected,1)
		tab.freq<-TotalTable(tab.freq)
		}
		if(res){
		tab.res<-round(test2$residuals,2)
		tab.res<-TotalTable(tab.res)
		}		
	draw<-seq(1:n.row)
	draw<-cut.page(draw,pages=pages.chisq)
		for(j in 1:pages.chisq){
			if (pages.chisq>1) x11()
		matrix<-c(paste(seq(1:length(draw[[j]]))),length(draw[[j]])+1,paste(seq(1:length(draw[[j]]))+length(draw[[j]])+1),length(draw[[j]])+1)
		layout(matrix(matrix,length(draw[[j]])+1,2,byrow=FALSE),c(1,1),c(rep(1,length(draw[[j]])),.3))
			if (graph=="pie"){
			par(mar = c(0,0,0,0))
			eval(parse(text=paste("pie(tab.squared[",draw[[j]],",],col=rainbow(n.col))",sep="")))
			}
			if (graph=="histogram"){
			par(mar = c(4,4,1,0))
				for (k in 1: length(draw[[j]])){
				eval(parse(text=paste("barplot(cbind(tab.squared[",draw[[j]][k],",]/sum(tab.squared[",draw[[j]][k],",]),0),beside=F,axes=F,col=rainbow(n.col),space=0)",sep="")))
				eval(parse(text=paste0("legend('topright',colnames(tab.squared),fill=rainbow(n.col))")))
				}
			}
			par(mar = c(0,0,0,0))
		plot(c(0,20),c(0,10),type='n',axe=FALSE)
			if (j==pages.chisq){
				if (!is.null(text.chi2)){
					if (is.null(text.OR)){
					eval(parse(text=paste("legend(5.5,10,",text.chi2,",cex=2,bty='n')")))
					eval(parse(text=paste("legend(9.5,9.2,",Test,",cex=2,bty='n')")))
					}
					else{
					eval(parse(text=paste("legend(-1.5,10,",text.chi2,",cex=2,bty='n')")))
					eval(parse(text=paste("legend(1.5,9.2,",Test,",cex=2,bty='n')")))
					eval(parse(text=paste("legend(5.5,9.2,",text.OR,",cex=2,bty='n')")))
					}
				}
			} 
		eval(parse(text=paste("graphtab<-c(",paste(paste('"tab.squared.', draw[[j]], '"', sep=""), collapse=","),")")))
		eval(parse(text=paste(graphtab,"<-rbind(Count=as.character(tab.squared[",draw[[j]],",]))",sep="")))
		eval(parse(text=paste(graphtab,"<-cbind(",graphtab,",Total=as.character(",total.rowcount,"))",sep="")))
			if(expect)
			eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Expected=tab.freq[",draw[[j]],",])",sep="")))
			if(res)
			eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Residuals=tab.res[",draw[[j]],",])",sep="")))
			if (any(percent=="row"))
			eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Row.pct=c(rowperx[",draw[[j]],",],'100'))",sep="")))
			if (any(percent=="column"))
			eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Column.pct=c(colperx[",draw[[j]],",],''))",sep="")))
			if (any(percent=="total"))
			eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Total.pct=c(totalperx[",draw[[j]],",],",total.rowtot,"))",sep="")))
		eval(parse(text=paste("colnames(",graphtab,")<-c(",paste0("'",colnames(tab.squared),"'",collapse=","),",'Total')",sep="")))
		eval(parse(text=paste("names(dimnames(",graphtab,"))[2]<-'",names(dimnames(tab.squared))[2],"'",sep="")))
		eval(parse(text=paste("names(dimnames(",graphtab,"))[1]<-paste(names(dimnames(tab.squared))[1],'=',rownames(tab.squared)[",draw[[j]],"],sep='')",sep="")))
			for (i in draw[[j]]){
			plot(c(0,10),c(0,10),type='n',axe=FALSE)
			graphTable(eval(parse(text=paste("tab.squared.",i,sep=""))),title.pos="top",left=left,top=top,cex=cex.table)
			}
		}	
	if (pages.mantelhaen==1) x11()
	}
	else text.chi2<-NULL
	if (any(test=="mantelhaen")){	
		if (missing(alternative)) alternative<-"two.sided"
	test1<-mantelhaen.test(tab,correct=!correct,alternative=alternative)
		if (test1[[3]]<0.001) Test2<-"'p < 0.001 (Mantel-Haenszel Test)'"
		if (test1[[3]]>0.001) Test2<-paste("'p=",round(test1[[3]],digits=3)," (Mantel-Haenszel Test)'",sep="")	
	text2.chi2<-paste("expression(Chi^2==",round(test1[[1]],2),")")
		if (any(percent=="OR")){
		text.OR<-paste0("'Odds Ratio=",round(oddratio,digits=2)," ; Conf. interval=[",round(lowerlimit,1),",",round(upperlimit,1),"]'")
		}
		else text.OR<-NULL		
	}
	else text2.chi2<-NULL
n.col<-dim(subtab)[2]
n.row<-dim(subtab)[1]
row.tab<-seq(1:n.row)
n.subrow<-length(rownames(tab))
draw<-seq(1:length(strata))
draw<-cut.page(draw,pages=pages.mantelhaen)
	for(j in 1:pages.mantelhaen){
		if (pages.mantelhaen>1) x11()
	draw1<-rep(seq(1:length(draw[[j]])),each=n.subrow)
	matrix1<-c(draw1,length(draw[[j]])+1)
	draw2<-seq(1:(length(draw[[j]])*n.subrow))
	draw2<-draw2+length(draw[[j]])+1
	matrix<-c(draw2,length(draw[[j]])+1,draw2+length(draw2),length(draw[[j]])+1)
	matrix<-c(matrix1,matrix)
	plot.table<-(draw[[j]][1]*n.subrow-n.subrow+1):(draw[[j]][length(draw[[j]])]*n.subrow)
	layout(matrix(matrix,length(plot.table)+1,3,byrow=FALSE),c(.5,1,1),c(rep(1,length(plot.table)),.5))
	par(mar = c(0,0,0,0))
	command<-paste('plot(c(0,10),c(0,10),type="n",axe=F);text(5,5,"',strata[draw[[j]][1]:draw[[j]][length(draw[[j]])]],'");segments(10,1,10,10);segments(0,0,10,0,lwd=2)',sep="")
	eval(parse(text=command))
	plot(c(0,30),c(0,10),type='n',axe=FALSE)
		if (j==pages.mantelhaen){
			if (!is.null(text.chi2)){
				if (!is.null(text2.chi2)){
				eval(parse(text=paste("legend(4.5,10.5,",text2.chi2,",cex=1.5,bty='n')")))
				eval(parse(text=paste("legend(9.5,10.2,",Test2,",cex=1.5,bty='n')")))
				}
			}
			else if (!is.null(text2.chi2)){
				if (!is.null(text.OR)){
				eval(parse(text=paste("legend(4.5,9.5,",text2.chi2,",cex=1.5,bty='n')")))
				eval(parse(text=paste("legend(9.5,9.2,",Test2,",cex=1.5,bty='n')")))
				eval(parse(text=paste("legend(5.5,6,",text.OR,",cex=1.5,bty='n')")))
				}
				else {
				eval(parse(text=paste("legend(4.5,10.5,",text2.chi2,",cex=1.5,bty='n')")))
				eval(parse(text=paste("legend(9.5,10.2,",Test2,",cex=1.5,bty='n')")))
				}
			}
		}
	eval(parse(text=paste("graphtab<-c(",paste(paste('"tab.', plot.table, '"', sep=""), collapse=","),")")))
	eval(parse(text=paste(graphtab,"<-rbind(Count=as.character(subtab[",plot.table,",]))",sep="")))
		if (any(percent=="row"))
		eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Row.pct=rowper[",plot.table,",])",sep="")))
		if (any(percent=="column"))
		eval(parse(text=paste(graphtab,"<-rbind(",graphtab,",Column.pct=colper[",plot.table,",])",sep="")))
	eval(parse(text=paste("names(dimnames(",graphtab,"))[2]<-'",names(dimnames(tab))[2],"'",sep="")))
	eval(parse(text=paste("names(dimnames(",graphtab,"))[1]<-paste(names(dimnames(tab))[1],'=',rownames(subtab)[",plot.table,"],sep='')",sep="")))
		for (i in plot.table){
		plot(c(0,10),c(0,10),type='n',axe=FALSE)
		graphTable(eval(parse(text=paste("tab.",i,sep=""))),y.intersp=0.1,title.pos=title.pos,left=left,top=top,cex=cex.table)
		}
		test<-apply(subtab,1,sum)
		if (graph=="pie"){
		par(mar = c(0,0,0,0))
			for (k in 1: length(plot.table)){
				if (test[k]!=0)
				eval(parse(text=paste("pie(subtab[",plot.table[k],",],col=rainbow(n.col))",sep="")))
				else
				plot(c(0,10),c(0,10),type='n',axe=FALSE)
			}
		
		}
		if (graph=="histogram"){
		par(mar = c(0,0,1,0))
			for (k in 1: length(plot.table)){
				if (test[k]!=0){
				eval(parse(text=paste("barplot(cbind(subtab[",plot.table[k],",]/sum(subtab[",plot.table[k],",]),0),beside=F,axes=F,col=rainbow(n.col),space=0)",sep="")))
				eval(parse(text=paste0("legend('topright',colnames(subtab),fill=rainbow(n.col))")))
				}
				else
				plot(c(0,10),c(0,10),type='n',axe=FALSE)
			}
		}
	par(mar = c(0,0,0,0))
	}


}
############################################################################
#Representation test de Dunnet & Gent
graphDunnettGent<-function (tab, delta = 0.1, Odds = TRUE,Confidence=TRUE, percent = c("row", "column","total"), response=NULL,test.product=NULL, 
Expected = TRUE, cex.table = 1) 
{
n.row <- dim(tab)[1]
n.col <- dim(tab)[2]
	if (n.row != 2 || n.col != 2) 
    stop("You must have 2x2 table")
rowper <- aperm(apply(tab, 1, function(x) x/sum(x) * 100))
colper <- apply(tab, 2, function(x) x/sum(x) * 100)
totalper <- round(tab/sum(tab) * 100, digit = 1)
test3 <- DunnettGent.test(tab, delta = delta, response=response,test.product = test.product)
success <- rowper[, response]
	if (rownames(tab)[1] == test.product) 
    obs.diff <- -diff(success)
    else obs.diff <- diff(success)
p.CI <- sum(tab[, response])/sum(tab)
q.CI <- 1 - p.CI
CI.inf <- obs.diff/100 - qnorm(0.975) * sqrt(p.CI * q.CI/sum(tab[1,]) + p.CI * q.CI/sum(tab[2, ]))
CI.sup <- obs.diff/100 + qnorm(0.975) * sqrt(p.CI * q.CI/sum(tab[1,]) + p.CI * q.CI/sum(tab[2, ]))
CI <- cbind(CI.inf, CI.sup)
rownames(CI) <- test3$Title
rowper <- round(rowper, digits = 1)
colper <- round(colper, digits = 1)
CI <- round(CI * 100, digits = 1)
CI.mid <- CI[1] + (CI[2] - CI[1])/2
	if (colnames(tab)[2] == response) 
	Exp.per <- cbind(c("", ""), round(test3$Expected, 1))
	else
	Exp.per <- cbind(round(test3$Expected, 1),c("", ""))
oddratio <- rowper[, 2]/rowper[, 1]
    if (rownames(tab)[2] == test.product) 
    oddratio <- oddratio[2]/oddratio[1]
    else oddratio <- oddratio[1]/oddratio[2]
sdoddratio <- sqrt(sum(1/tab))
lowerlimit <- oddratio * exp(-qnorm(0.975) * sdoddratio)
lowereqlimit <- test3$Expected/(100 - test3$Expected)
lowereqlimit <- lowereqlimit[1]/lowereqlimit[2]
tabfreq <- aperm(rowper)
colnames(tabfreq) <- c("", "")
nf <- layout(matrix(c(1, 2, 3, 4, 1, 5, 6, 4), 4, 2, byrow = FALSE),c(1, 1), c(1, 1, 1, 0.3))
par(mar = c(0, 5, 0, 0))
barplot3D(tabfreq, col = rainbow(2), xlab = "", ylab = "Percent", beside = TRUE)
eval(parse(text = paste("graphtab<-c(", paste(paste("\"tab.",1:2, "\"", sep = ""), collapse = ","), ")")))
eval(parse(text = paste(graphtab, "<-rbind(Count=as.character(tab[",1:2, ",]))", sep = "")))
	if (any(percent == "row")) 
    eval(parse(text = paste(graphtab, "<-rbind(", graphtab,",Row.percent=rowper[", 1:2, ",])", sep = "")))
    if (Expected) 
    eval(parse(text = paste(graphtab, "<-rbind(", graphtab,",Expected=Exp.per[", 1:2, ",])", sep = "")))
    if (any(percent == "column")) 
    eval(parse(text = paste(graphtab, "<-rbind(", graphtab,",Column.percent=colper[", 1:2, ",])", sep = "")))
    if (any(percent == "total")) 
    eval(parse(text = paste(graphtab, "<-rbind(", graphtab,",Total.percent=totalper[", 1:2, ",])", sep = "")))
eval(parse(text = paste("names(dimnames(", graphtab, "))[2]<-'",names(dimnames(tab))[2], "'", sep = "")))
eval(parse(text = paste("names(dimnames(", graphtab, "))[1]<-paste(names(dimnames(tab))[1],'=',rownames(tab)[",1:2, "],sep='')", sep = "")))
par(mar = c(0, 0, 0, 0))
plot(c(0, 10), c(0, 10), type = "n", axe = FALSE)
graphTable(eval(parse(text = paste("tab.1"))), cex = cex.table)
    if(Confidence){
    par(mar = c(4, 5, 0, 0))
    xlimit <- c(min(-delta * 100 * 1.2, CI), max(delta * 100 * 1.2, CI))
    plot(xlimit, c(0, 6), xlab = "", ylab = names(dimnames(tab)[2]),type = "n", axes = FALSE)
    axis(1)
    box()
    axis(3, c(CI, CI.mid))
    abline(v = 0, lty = 2, col = "turquoise")
    rect(CI[1], 4.4, CI[2], 5.1, col = "turquoise")
    text(CI.mid, 4.8, test3$Title)
    text(CI.mid, 5.8, "Confidence interval for percentage difference")
    segments(CI.mid, 4.3, CI.mid, 4.5, lwd = 2)
    abline(v = c(-delta * 100, delta * 100), lty = 2)
    arrows(0, 3.2, -delta * 100, 3.2, length = 0.1)
    text(-delta * 100/2, 2.8, "Favours the Reference drug")
    arrows(0, 2.2, delta * 100 * 1.2, 2.2, length = 0.1)
    text(delta * 100/2.5, 1.8, "Favours the Test drug")
	}
	else
    plot(c(0, 10), c(0, 10), type = "n", axe = FALSE, xlab = "",ylab = "")
    if (Odds) {
    par(mar = c(0, 0, 0, 0))
    plot(c(0, 10), c(0, 10), type = "n", axe = FALSE)
    text(5, 8.5, test3$method, cex = 1.5)
    text(5, 5.5, paste( test3$Title, " against H1: > -", delta, sep = ""))
    text(1, 2, paste(test3$X.squared, sep = ""),pos = 4)
    text(5, 2, paste( test3$p.Value, " (one-tail)", sep = ""), pos = 4)
    }
    else
    plot(c(0, 10), c(0, 10), type = "n", axe = FALSE, xlab = "",ylab = "")
plot(c(0, 10), c(0, 10), type = "n", axe = FALSE, xlab = "",ylab = "")
graphTable(eval(parse(text = paste("tab.2"))), title.pos="top",cex = cex.table)
    if (Odds) {
    par(mar = c(4, 5, 0, 2))
    ylimit <- c(0, max(lowerlimit, lowereqlimit) * 1.2)
    plot(c(0, 3), ylimit, xlab = "", ylab = "Odds ratio value",type = "n", axes = FALSE)
    rect(par("usr")[1], lowereqlimit, par("usr")[2], par("usr")[4],density = 10, col = "turquoise")
    axis(2, round(lowerlimit, 2))
    axis(4, round(lowereqlimit, 2))
    arrows(1, 0, 1, lowerlimit, length = 0.15)
    arrows(2, 0, 2, lowereqlimit, length = 0.15)
    text(1, lowerlimit * 1.1, "One-sided lower limit")
    text(2, lowereqlimit * 1.1, "Lower equivalence limit")
    box()
    }
    else {
    par(mar = c(0, 0, 0, 0))
    plot(c(0, 10), c(0, 10), type = "n", axe = FALSE, xlab = "",ylab = "")
    text(5, 9.5, test3$method, cex = 1.2)
    text(5, 6, paste(test3$Title, " against H1: > -",delta, sep = ""), cex = 1.2)
    text(1, 4, paste( test3$X.squared, sep = ""),pos = 4, cex = 1.2)
    text(1, 2, paste( test3$p.Value, " (one-tail)",sep = ""), pos = 4, cex = 1.2)
    }
}
############################################################################
#Representation test d'equivalence
Equivalence.graph<-function (formula, data = parent.frame(), delta = NULL, alpha = 0.05,reference=NULL,main=NULL)
{

    f <- formula(formula)
    listvar <- as.character(attr(terms(f), "variables"))[-1]
    response <- listvar[1]
    factor <- listvar[2]
    valid <- complete.cases(with(data, eval(parse(text = response))),
        with(data, eval(parse(text = factor))))
    response <- with(data, eval(parse(text = response)))[valid]
    if (!is.numeric(response))
        stop(gettext("Variable1 must be numeric.",domain="R-RcmdrPlugin.TestGraph"))
    factor <- with(data, eval(parse(text = factor)))[valid]
    factor <- factor(factor)
    if (!is.factor(factor))
        stop(gettext("Variable2 must be factor.",domain="R-RcmdrPlugin.TestGraph"))
    levs <- levels(factor)
    if (length(levs)!=2)
        stop(gettext("Factor must have 2 levels.",domain="R-RcmdrPlugin.TestGraph"))
    if (!is.null(alpha) && !is.numeric(alpha) || any(0 > alpha |
        alpha > 1))
        stop(sQuote("alpha"), " must be numeric in [0, 1]")
    old.par <- par(mfrow = c(2, 1), oma = c(0, 0, 3.1, 0))
    on.exit(par(old.par))
    alpha <- as.numeric(alpha)
    sig.level <-alpha/2
    col.less <- "pink"
    col.greater <- "pink"
    legend.less <- "--> H02\n      test"
    legend.greater <-  "H01  \ntest <--"
  	reg<-lm(response~factor)
  	CI<-confint(reg,level=0.9)
 	  ifelse(levs[1]==reference,title.test<-paste("Test (",levs[2],")-reference (",levs[1],")",sep=""),
    title.test<-paste("Test (",levs[1],")-Reference (",levs[2],")",sep=""))
    means <- tapply(response, factor, mean)
    ifelse(levs[1]==reference,diffmean<-means[2]-means[1],diffmean<-means[1]-means[2])
    ns <- tapply(response, factor, function(x) length(!is.na(x)))
    vars<-tapply(response, factor, var)
	  s2n<-vars/ns
    s2n_1<-(1/(ns-1))*(vars/ns)^2
	  k<-sum(s2n)^2/sum(s2n_1)
	  s2<-sum((ns-1)*vars)/(sum(ns)-2)
	  se<-s2/k
    t1 <- (diffmean+delta)/sqrt(s2*sum(1/ns))
    t2<-(delta-diffmean)/sqrt(s2*sum(1/ns))
    proba1<-1-pt(abs(t1),k)
    proba2<-1-pt(abs(t2),k)
    ifelse(proba1 < 0.001,proba1 <- "p < 0.001",proba1 <- paste("p =", round(proba1, 3)))
    ifelse(proba2 < 0.001,proba2 <- "p < 0.001",proba2 <- paste("p =", round(proba2, 3)))
    xlab.title1 <- paste("H01: ", proba1,"            H02: ",proba2,sep="")
    rangex <- c(qnorm(9e-05, m = diffmean-delta, sd = se),qnorm(0.9999, m = diffmean+delta, sd = se))
    x <- seq(rangex[1], rangex[2], length = 200)
    layout(matrix(c(1, 1, 2, 3, 4, 5), ncol = 2, byrow = T),width = c(1, 0.2), heights = c(0.2, 1, 1))
    par(mar = c(0, 0, 0, 0))
    plot(c(0, 1), c(0, 1), type = "n", axes = F, xlab = "", ylab = "")
    if (is.null(main)) text(0.5, 0.85, paste("Equivalence test for ",title.test, sep = ""), cex = 2)
    else text(0.5, 0.85, main, cex = 2)
    text(0.2, 0.25, bquote(t[1] == .(round(t1,2))),cex=2)    
    text(0.5, 0.25, bquote(t[2] == .(round(t2,2))),cex=2)   
    text(0.8, 0.25, bquote(Delta[eq] == .(delta)),cex=2)   
    #Premier graphique
    par(mar = c(4, 4, 4, 0))
    plot(x, dnorm(x, diffmean, se), type = "n", ylim = c(0, max(dnorm(x,diffmean, se)) * 7/6), ylab = "",
    xlab = xlab.title1, main = paste("Mean difference = ",round(diffmean,1), sep = ""))
	  ifelse(levs[1]==reference,CI.inf<-CI[2,1],CI.inf<--CI[2,1])
	  ifelse(levs[1]==reference,CI.sup<-CI[2,2],CI.sup<--CI[2,2])
	  CI <- cbind(CI.inf, CI.sup)
    r.inf <- qnorm(0.9,diffmean, se)
    r.sup<- qnorm(0.1, diffmean, se)
    polygon(c(r.inf, r.inf, x[x > r.inf]), c(0, dnorm(c(r.inf, x[x > r.inf]), diffmean,se)), border = NA, col = "pink")
    polygon(c(x[x < r.sup], r.sup, r.sup), c(dnorm(c(x[x < r.sup], r.sup), diffmean,se), 0), border = NA, col = "pink")
    text(0,max(dnorm(x, diffmean, se)) * 16/14,"Equivalence interval")
    arrows(0, max(dnorm(x, diffmean, se)) * 15/14, -delta ,  max(dnorm(x, diffmean, se)) * 15/14, length = 0.1)
    arrows(0, max(dnorm(x, diffmean, se)) * 15/14, delta ,  max(dnorm(x, diffmean, se)) * 15/14, length = 0.1)
    abline(v = r.inf, col = "black")
    abline(v = r.sup, col = "black")
    abline(v = c(-delta, delta ), lty = 2,col="red")
    lines(x, dnorm(x,diffmean, se), col = "red")
    abline(h = 0)
    par(mar = c(0, 0, 0, 0))
    plot(c(0, 1), c(0, 1), type = "n", axes = F, xlab = "", ylab = "")
    # Deuxieme graphique
    par(mar = c(4, 4, 4, 0))
    plot(x, dnorm(x, diffmean+delta, se), type = "n", ylim = c(0, max(dnorm(x,diffmean+delta, se)) * 7/6),ylab = "",
    xlab = "", main = "Alternative Distribution")
    polygon(c(r.inf, r.inf, x[x > r.inf], max(x)), c(0, dnorm(c(r.inf, x[x > r.inf]), diffmean+delta, se), 0), col ="lightblue")
    polygon(c(x[x < r.sup], r.sup, r.sup), c(dnorm(c(x[x < r.sup], r.sup),diffmean-delta,se), 0), border = NA, col = "lightblue")
    rect(CI[1], max(dnorm(x, diffmean, se)) * 15/14, CI[2], max(dnorm(x, diffmean, se))* 17/14, col = "turquoise")
    text(CI.inf+(CI.sup-CI.inf)/2,max(dnorm(x, diffmean, se)) * 16/14,"Confidence Int.")
    text(r.inf, max(dnorm(x, diffmean+delta, se) * 15/14), legend.less, adj = 0)
    text(r.sup, max(dnorm(x, diffmean+delta, se) * 15/14), legend.greater,pos = 2, adj = 0)
    abline(v = r.inf, col = "black")
    abline(v = r.sup, col = "black")
    abline(v = c(-delta, delta ), lty = 2,col="red")
    lines(x, dnorm(x, delta+diffmean, se), col = "blue")
    lines(x, dnorm(x, diffmean-delta, se), col="blue")
    abline(h = 0)
    par(mar = c(0, 0, 0, 0))
    plot(c(0, 1), c(0, 1), type = "n", axes = F, xlab = "", ylab = "")

}

############################################################################
#cote 3D
lozenge<-function (xleft, ybottom, xright, ytop, density = NULL, side=c("upper","side")
,tilt=NULL,angle = 45,col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"),
    ...)
{
  axis1<-par("usr")[2]-par("usr")[1]
  axis2<-par("usr")[4]-par("usr")[3]
  scalex<-axis2/axis1
  scaley<-axis1/axis2
  n <- range(length(xleft), length(xright), length(ybottom),length(ytop))
        if (n[1] == 0)
            stop("invalid lozenge specification")
        n <- n[2]
    if (is.numeric(density) && all(is.na(density) | density < 0))
        density <- NULL
    if (!is.null(density) && !is.null(angle)) {
        if (is.logical(border) && !is.na(border)) {
            if (border)
            border <- col
            else border <- NA
        }

	if(side=="upper"){
	  d<-(ytop-ybottom)*scaley/tan((tilt/180)*pi)
        x <- rbind(rep.int(NA, n), xleft, xright-d, xright, xleft+d)[-1]
        y <- rbind(rep.int(NA, n), ybottom, ybottom, ytop, ytop)[-1]
		loz<-cbind(x=x,y=y)
		}
	if(side=="side"){
	  d<-(xright-xleft)*scalex*tan((tilt/180)*pi)
        x <- rbind(rep.int(NA, n), xleft, xright, xright, xleft)[-1]
        y <- rbind(rep.int(NA, n), ybottom, ybottom+d, ytop, ytop-d)[-1]
		loz<-cbind(x=x,y=y)
		}
	  polygon(x, y, col = col, border = border, lty = lty,lwd = lwd, density = density, angle = angle, ...)
    }
   else {
	if(side=="upper"){
	  d<-(ytop-ybottom)*scaley/tan((tilt/180)*pi)
        x <- rbind(rep.int(NA, n), xleft, xright-d, xright, xleft+d)[-1]
        y <- rbind(rep.int(NA, n), ybottom, ybottom, ytop, ytop)[-1]
		loz<-cbind(x=x,y=y)
		}
	if(side=="side"){
	  d<-(xright-xleft)*scalex*tan((tilt/180)*pi)
        x <- rbind(rep.int(NA, n), xleft, xright, xright, xleft)[-1]
        y <- rbind(rep.int(NA, n), ybottom, ybottom+d, ytop, ytop-d)[-1]
		loz<-cbind(x=x,y=y)
		}
        polygon(x, y, col = col, border = border, lty = lty,lwd = lwd, ...)
		}
invisible(loz)		
}
#################################################################
# histogramme en 3D
barplot3D<-function (height, width = 1, space = NULL, names.arg = NULL,legend.text = NULL, beside = FALSE, horiz = FALSE, density = NULL,angle = 45,col = NULL,
border = par("fg"), main = NULL,sub = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,xpd = TRUE, log = "", axes = TRUE, axisnames = TRUE,
cex.axis = par("cex.axis"),cex.names = par("cex.axis"), inside = TRUE, plot = TRUE,axis.lty = 0, offset = 0, add = FALSE, ...)
{
    if (!missing(inside))
        .NotYetUsed("inside", error = FALSE)
    if (is.null(space))
        space <- if (is.matrix(height) && beside) c(0, 1) else 0.2
    space <- space * mean(width)
    if (plot && axisnames && is.null(names.arg))
        names.arg <- if (is.matrix(height)) colnames(height) else names(height)
    if (is.vector(height) || (is.array(height) && (length(dim(height)) == 1))) {
        height <- cbind(height)
        beside <- TRUE
        if (is.null(col)) col <- "grey"
    }
    else if (is.matrix(height)) {
        if (is.null(col)) col <- grey.colors(nrow(height))
    }
    else stop("'height' must be a vector or a matrix")
    if (is.logical(legend.text))
        legend.text <- if (legend.text && is.matrix(height)) rownames(height)
    stopifnot(is.character(log))
    logx <- logy <- FALSE
    if (log != "") {
        logx <- any(grep("x", log))
        logy <- any(grep("y", log))
    }
    if ((logx || logy) && !is.null(density))
        stop("Cannot use shading lines in bars when log scale is used")
    NR <- nrow(height)
    NC <- ncol(height)
    if (beside) {
        if (length(space) == 2)
        space <- rep.int(c(space[2], rep.int(space[1], NR - 1)), NC)
    width <- rep(width, length.out = NR)
    }
    else {
    width <- rep(width, length.out = NC)
    }
    offset <- rep(as.vector(offset), length.out = length(width))
    delta <- width/2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    log.dat <- (logx && horiz) || (logy && !horiz)
    if (log.dat) {
        if (min(height + offset) <= 0)
            stop("log scale error: at least one 'height + offset' value <= 0")
        if (logx && !is.null(xlim) && min(xlim) <= 0)
            stop("log scale error: 'xlim' <= 0")
        if (logy && !is.null(ylim) && min(ylim) <= 0)
            stop("log scale error: 'ylim' <= 0")
        rectbase <- if (logy && !horiz && !is.null(ylim)) ylim[1]
        else if (logx && horiz && !is.null(xlim)) xlim[1]
        else 0.9 * min(height)
    }
    else rectbase <- 0
    if (!beside)
        height <- rbind(rectbase, apply(height, 2, cumsum))
    rAdj <- offset + (if (log.dat) 0.9 * height else -0.01 * height)
    delta <- width/2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    if (horiz) {
        if (is.null(xlim))
            xlim <- range(rAdj, (height + offset)*1.1, na.rm = TRUE)
        if (is.null(ylim))
            ylim <- c(min(w.l), max(w.r)*1.1)
    }
    else {
        if (is.null(xlim))
            xlim <- c(min(w.l), max(w.r)*1.1)
        if (is.null(ylim))
            ylim <- range(rAdj, (height + offset)*1.1, na.rm = TRUE)
    }
    if (beside)
        w.m <- matrix(w.m, ncol = NC)
    if (plot) {
        opar <- if (horiz)
            par(xaxs = "i", xpd = xpd)
        else par(yaxs = "i", xpd = xpd)
        on.exit(par(opar))
        if (!add) {
            plot.new()
            plot.window(xlim, ylim, log = log, ...)
  		axis1<-par("usr")[2]-par("usr")[1]
  		axis2<-par("usr")[4]-par("usr")[3]
  		scalex<-axis2/axis1
  		scaley<-axis1/axis2
        }
  		if(add){
      axis1<-par("usr")[2]-par("usr")[1]
  		axis2<-par("usr")[4]-par("usr")[3]
  		scalex<-axis2/axis1
  		scaley<-axis1/axis2
  		}
        xyrect <- function(x1, y1, x2, y2, horizontal = TRUE,...) {
            if (horizontal)
                rect(x1, y1, x2, y2, ...)
            else rect(y1, x1, y2, x2, ...)
        }

        if (beside){
 		if (horiz){
      if (xlim[1]>xlim[2]){
            lozenge(c(height) + offset-.3*scaley*tan(53/180*pi),w.l-.3,c(height)-c(height)+offset,w.l,
		    side="upper",tilt=37, angle = angle, density = density,col = col, border = border)
            lozenge(c(height) + offset,w.l, c(height) + offset-.3*scaley*tan(53/180*pi),w.r-.3,
            tilt=37,side="side", angle = angle, density = density,col = col, border = border)
        }
      if (xlim[2]>xlim[1]){
            lozenge(c(height)-c(height)+offset,w.r,c(height) + offset+.3*scaley*tan(53/180*pi),w.r+.3,
		    side="upper",tilt=37, angle = angle, density = density,col = col, border = border)
            lozenge(c(height) + offset,w.l, c(height) + offset+.3*scaley*tan(53/180*pi),w.r+.3,
            tilt=37,side="side", angle = angle, density = density,col = col, border = border)
        }
			}
		else {
     if (xlim[1]>xlim[2]){
         loz.side<-lozenge(w.l,c(height)-c(height) + offset,w.l-.3, c(height) + offset-.3*scalex*tan(37/180*pi),
            tilt=37,side="side", angle = angle, density = density,col = col, border = border)
         loz.upper<-lozenge(w.r,c(height)+offset,w.l-.3,  c(height) + offset-.3*scalex*tan(37/180*pi),
		    side="upper",tilt=37, angle = angle, density = density, col = col, border = border)
        }
      if (xlim[2]>xlim[1]){
         loz.side<-lozenge(w.r,c(height)-c(height) + offset,w.r+.3, c(height) + offset+.3*scalex*tan(37/180*pi), 
                 tilt=37,side="side", angle = angle, density = density,col = col, border = border)
         loz.upper<-lozenge(w.l,c(height)+offset,w.r+.3,  c(height) + offset+.3*scalex*tan(37/180*pi),
		    side="upper",tilt=37, angle = angle, density = density,col = col, border = border)
        }
      }
            xyrect(rectbase + offset, w.l, c(height) + offset, w.r, horizontal = horiz, angle = angle, density = density,
                col = col, border = border)
			rect.draw<-	cbind(y1=rectbase + offset, x1=w.l, y2=c(height) + offset, x2=w.r)
			}
        else {

            for (i in 1:NC) {
		if (horiz){
		lozenge(height[-1, i]+offset[i],rep(w.l[i],NR),height[-1,i] + offset[i]+.15*scaley*tan(53/180*pi),rep(w.r[i],NR)+.15,
		angle = angle, side="side",tilt=37, col = col,density=density, border = border)
		lozenge(height[1:NR, i]+offset[i],rep(w.r[i],NR),height[-1,i] + offset[i]+.15*scaley*tan(53/180*pi),rep(w.r[i],NR)+.15,
		angle = angle, side="upper",tilt=37, col = col,density=density, border = border)

		}
		else {
		lozenge( rep(w.l[i],NR),height[-1, i] + offset[i],rep(w.r[i],NR)+.2, height[-1,i] + offset[i]+.2*scalex*tan(37/180*pi),
		angle = angle, side="upper",tilt=37, col = col,density=density, border = border)
		lozenge( rep(w.r[i],NR),height[1:NR, i] + offset[i],rep(w.r[i],NR)+.2, height[-1,i] + offset[i]+.2*scalex*tan(37/180*pi),
		angle = angle, side="side",tilt=37, col = col,density=density, border = border)
		}
        xyrect(height[1:NR, i] + offset[i], w.l[i], height[-1,i] + offset[i], w.r[i], horizontal = horiz,
                  angle = angle, density = density, col = col, border = border)
		
            }
        rect.draw<-""
		loz.side<-""
		loz.upper<-""
		}
        if (axisnames && !is.null(names.arg)) {
            at.l <- if (length(names.arg) != length(w.m)) {
                if (length(names.arg) == NC)
                  colMeans(w.m)
                else stop("incorrect number of names")
            }
            else w.m
            axis(if (horiz)
                2
            else 1, at = at.l, labels = names.arg, lty = axis.lty,
                cex.axis = cex.names, ...)
        }
        if (!is.null(legend.text)) {
            legend.col <- rep(col, length.out = length(legend.text))
            if ((horiz & beside) || (!horiz & !beside)) {
                legend.text <- rev(legend.text)
                legend.col <- rev(legend.col)
                density <- rev(density)
                angle <- rev(angle)
            }
            xy <- par("usr")
            legend(xy[2] - xinch(0.1), xy[4] - yinch(0.1), legend = legend.text,
                angle = angle, density = density, fill = legend.col,
                xjust = 1, yjust = 1)
        }
        title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
        if (axes)
            axis(if (horiz)
                1
            else 2, cex.axis = cex.axis, ...)
        invisible(list(bar.draw=rect.draw,loz.side=loz.side,loz.upper=loz.upper))
    }
}
############################################################################
#Fonction Histogramme avec courbe normale
HistogramNormal<-function (x, breaks = "Sturges", col.plot="blue",col.hist="turquoise",Test.shapiro=FALSE,main = "Comparison to normal distribution",
xlab =paste("Normal quantiles of",xname), ylab=xname, axes = TRUE, plot = TRUE, nclass = NULL, ...)
{
  if (!is.numeric(x))    stop("'x' must be numeric")
  r <- hist(x,breaks=breaks,plot=F)
  ylimit<-max(r$density)
  xlimit=range(r$breaks)
  test<-shapiro.test(x[!is.na(x)])
  xname<-deparse(substitute(x))
  xname<-unlist(strsplit(xname,"\\$"))[2]
  if(test$p.value<0.001) test<-"p < 0.001"
  else test<-paste("p =",round(test$p.value,3))
  if (plot) {
    if(Test.shapiro) xlab<-paste (xlab,", ",test," (Shapiro-Wilk normality test)",sep="")
    layout( matrix(c(2,1,1,1),ncol=2),width=c(1,1),heights=c(1,1))
    qqPlot(x, dist= "norm", col=col.plot,xlab=xlab,ylab=ylab,main=main,...)    
    normx<-seq(min(na.omit(x)),max(na.omit(x)),length=500)
    densityx<-dnorm(normx,mean(na.omit(x)),sd(na.omit(x)))
    ylimit<-max(ylimit,densityx)
    axis1<-max(r$breaks)-min(r$breaks)
    axis2<-ylimit
    scalex<-axis2/axis1
    scaley<-axis1/axis2
    plot(r, freq = FALSE, col = col.hist,
         main = "", xlim = xlimit, ylim = c(0,ylimit),
         xlab = "", ylab = "", axes = FALSE,...)
    ytopupper<-r$density+.3*diff(r$breaks)*scalex*tan(37/180*pi)
    ybottomupper<-r$density
    xleftupper<-r$breaks[-length(r$breaks)]
    xrightupper<-r$breaks[-1]+.3*diff(r$breaks)
    xleftside<-r$breaks[-1]
    xrightside<-xleftside+.3*diff(r$breaks)
    d<-(xrightside-xleftside)*scalex*tan((37/180)*pi)
    x1 <- rbind(rep.int(NA, 1), xleftside, xrightside, xrightside, xleftside)[-1]
    y1 <- rbind(rep.int(NA, 1), 0, d, ytopupper, ytopupper-d)[-1]
    polygon(x1,y1,col=col.hist,...)
    d<-(ytopupper-ybottomupper)*scaley/tan((37/180)*pi)
    x1 <- rbind(rep.int(NA, 1), xleftupper, xrightupper-d, xrightupper, xleftupper+d)[-1]
    y1 <- rbind(rep.int(NA, 1), ybottomupper, ybottomupper, ytopupper, ytopupper)[-1]
    polygon(x1,y1,col=col.hist,...)
    plot(r, freq = FALSE, col = col.hist,
         main = "", xlim = xlimit, ylim = c(0,ylimit),
         xlab = "", ylab = "", axes = FALSE,add=TRUE,...)
    box()
    lines(normx,densityx,lwd=2)
    axis(1,cex.axis=1)
    invisible(r)
  }
  else {
    nf <- names(formals())
    nf <- nf[is.na(match(nf, c("x", "breaks", "nclass", "plot",
                               "include.lowest", "right")))]
    missE <- lapply(nf, function(n) substitute(missing(.),
                                               list(. = as.name(n))))
    not.miss <- !sapply(missE, eval, envir = environment())
    if (any(not.miss))
      warning(sprintf(ngettext(sum(not.miss), "argument %s is not made use of",
                               "arguments %s are not made use of"), paste(sQuote(nf[not.miss]),
                                                                          collapse = ", ")), domain = NA)
    r
  }
}
############################################################################
#Histogramme des moyennes
histogramMean<-function (formula,data=parent.frame(),error=FALSE,minimum=0,dim=c("2D","3D"),strata=c("repeated","factor"),ymax=NULL,xlab=NULL,cex.names=1,cex.axis=1,
xlab.axis=0,ylab=NULL,name.repeated=NULL,legend.lab=NULL,legend.pos="topright", cex.legend = 1,main = "Histogram of Means",col = palette()[2:8]) 
{
     if (length(grep("~", deparse(substitute(formula)))) != 0) {
        f <- formula(formula)
        listvar <- as.character(attr(terms(f), "variables"))[-1]
        response <- listvar[1]
        factor <- listvar[-1]
        factor <- chartr("|", "+", factor)
        if (length(grep(" +", deparse(substitute(response)))) != 
            0) {
            response <- paste("~", response)
            response <- formula(response)
            listresponse <- as.character(attr(terms(response), 
                "variables"))[-1]
        }
        else listresponse <- response
        if (length(grep(" +", deparse(substitute(factor)))) != 
            0) {
            factor <- paste("~", factor)
            factor <- formula(factor)
            listfactor <- as.character(attr(terms(factor), "variables"))[-1]
        }
        else listfactor <- factor
    }
    if (length(grep("~", deparse(substitute(formula)))) == 0) {
        formula <- paste(paste(deparse(substitute(formula)),collapse=""), "~ factor")
        f <- formula(formula)
        listvar <- as.character(attr(terms(f), "variables"))[-1]
        response <- listvar[1]
        listresponse <- response
        listfactor <- NA
        if (length(grep(" +", deparse(substitute(response)))) != 
            0) {
            response <- paste("~", response)
            response <- formula(response)
            listresponse <- as.character(attr(terms(response), 
                "variables"))[-1]
        }
    }
dim<-match.arg(dim)
strata<-match.arg(strata)	
	#Sans groupe
    if (is.na(listfactor[1]) == TRUE) {
		#Simple
        if (length(listresponse) == 1) {
        tab <- with(data, eval(parse(text = paste("mean(",listresponse, ",na.rm=TRUE)", sep = ""))))
        n.levs.2 <- 1
        stdeviation <- with(data, eval(parse(text = paste("sd(",listresponse, ",na.rm=TRUE)", sep = ""))))
        valid <- complete.cases(with(data, eval(parse(text = listresponse))))
        sample <- length(with(data, eval(parse(text = listresponse)))[valid])
        stdeviation <- stdeviation/sqrt(sample)
        marge <- (tab - minimum) * 0.3 * tan(37/180 * pi)
            if (error) 
            ymax <- tab + marge + stdeviation
            else ymax <- tab + marge
            if (is.null(ylab)) 
            ylab <- paste("Mean of", listresponse)
            if (dim=="2D") 
			barplotlim(tab, col = col, xlab = "", ylab = ylab, ymin = minimum, ymax = ymax)
			if (dim=="3D") 
			barplotlim3D(tab, col = col, xlab = "", ylab = ylab, ymin = minimum, ymax = ymax)
        title(main = paste(listresponse), font.main = 4)
            if (error && stdeviation != 0) 
            arrows(0.7, tab - stdeviation, 0.7, tab + stdeviation,angle = 90, code = 3, lty = 1, lwd = 2, length = 0.25)
        }
		#Repetee
        if (length(listresponse) > 1) {
            tab <- NULL
            stdeviation <- NULL
            sample <- NULL
            for (i in 1:length(listresponse)) {
                tab <- c(tab, with(data, eval(parse(text = paste("mean(", 
                  listresponse[i], ",na.rm=TRUE)", sep = "")))))
                stdeviation <- c(stdeviation, with(data, eval(parse(text = paste("sd(", 
                  listresponse[i], ",na.rm=TRUE)", sep = "")))))
                valid <- complete.cases(with(data, eval(parse(text = listresponse[i]))))
                sample <- c(sample, length(with(data, eval(parse(text = listresponse[i])))[valid]))
            }
            names(tab) <- listresponse
            stdeviation <- stdeviation/sqrt(sample)
            n.levs.2 = length(listresponse)
            marge <- 0.3 * tan(37/180 * pi) * ((max(tab) - minimum)/n.levs.2)
            if (error) {
                if (max(stdeviation) > marge) 
                  ymax <- max(tab) + max(stdeviation)
                else ymax <- max(tab) + marge
            }
            else ymax <- max(tab) + marge
            if (is.null(ylab)) 
                ylab <- "Mean"
			if (is.null(name.repeated)) 
                name.repeated <- "Variable"				
            if (is.null(xlab)) 
                xlab <- name.repeated
			if (dim=="2D"){
				if (xlab.axis==0)
				barplotlim(tab, col = col, ymin = minimum, ymax = ymax, xlab = xlab, ylab = ylab, cex.names = cex.names,cex.axis = cex.axis)
				else{
				mp <-barplotlim(tab, col = col, ymin = minimum, ymax = ymax,xlab = xlab, ylab = ylab, axisnames = FALSE)
				text(mp, par("usr")[3], labels = names(tab), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.axis)
				}
			}
			if (dim=="3D"){
				if (xlab.axis==0)
				barplotlim3D(tab, col = col, ymin = minimum, ymax = ymax, xlab = xlab, ylab = ylab, cex.names = cex.names,cex.axis = cex.axis)
				else{
				mp <-barplotlim3D(tab, col = col, ymin = minimum, ymax = ymax,xlab = xlab, ylab = ylab, axisnames = FALSE)
				text(mp, par("usr")[3], labels = names(tab), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.axis)
				}
			}			
            title(main = main, font.main = 4)
            width <- rep(1, length.out = length(tab))
            delta <- width/2
            w.r <- cumsum(0.2 + width)
            w.m <- w.r - delta
            if (error) 
                for (i in 1:length(tab)) if (stdeviation[i] != 
                  0) 
                  arrows(w.m[i], tab[i] - stdeviation[i], w.m[i], tab[i] + stdeviation[i], angle = 90, code = 3,lty = 1, lwd = 2, length = 0.125 * 3/length(tab))
        }
    }
	#Avec un groupe
    if (is.na(listfactor[1]) == FALSE) {
		#Simple
        if (length(listresponse) == 1) {
			#Un seul groupe
            if (length(listfactor) == 1) {
                factorname <- factor
                responsename <- response
                factor <- with(data, eval(parse(text = factor)))
                response <- with(data, eval(parse(text = response)))
                data <- cbind(as.character(factor), response)
                colnames(data) <- c(factorname, responsename)
                data <- as.data.frame(data)
                data[, 2] <- as.numeric(as.character(data[, 2]))
                tab <- tapply(data[, 2], factor(data[, 1], levels = levels(factor)[levels(factor) !=""]), function(x) mean(x, na.rm = TRUE))
                stdeviation <- tapply(data[, 2], factor(data[,1], levels = levels(factor)[levels(factor) != ""]), function(x) sd(x, na.rm = TRUE))
                sample <- tapply(data[, 2], factor(data[, 1],levels = levels(factor)[levels(factor) != ""]),function(x) length(x[!is.na(x)]))
                n.levs.2 <- length(rownames(tab))
                stdeviation <- stdeviation/sqrt(sample)
                marge <- 0.3 * tan(37/180 * pi) * ((max(tab) - minimum)/length(tab))
                if (error) {
					if (max(stdeviation) > marge) 
					ymax <- max(tab) + max(stdeviation)
					else ymax <- max(tab) + marge
                }
                else ymax <- max(tab) + marge
				if (is.null(ylab)) 
				ylab <- paste("Mean of", responsename)
				if (is.null(xlab)) 
				xlab <- factorname
				if (dim=="2D"){
					if (xlab.axis==0)
					barplotlim(tab, col = col, ymin = minimum, ymax = ymax, xlab = xlab, ylab = ylab, cex.names = cex.names,cex.axis = cex.axis)
					else{
					mp <-barplotlim(tab, col = col, ymin = minimum, ymax = ymax,xlab = xlab, ylab = ylab, axisnames = FALSE)
					nbre.leg<-dim(mp)[1]
					coord.lab<-(mp[nbre.leg,]-mp[1,])/2+mp[1,]						
					text(coord.lab, par("usr")[3], labels = names(tab), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.axis)
					}				
				}
				if (dim=="3D"){
					if (xlab.axis==0)
					barplotlim3D(tab, col = col, ymin = minimum, ymax = ymax, xlab = xlab, ylab = ylab, cex.names = cex.names,cex.axis = cex.axis)
					else{
					mp <-barplotlim3D(tab, col = col, ymin = minimum, ymax = ymax,xlab = xlab, ylab = ylab, axisnames = FALSE)
					nbre.leg<-dim(mp)[1]
					coord.lab<-(mp[nbre.leg,]-mp[1,])/2+mp[1,]						
					text(coord.lab, par("usr")[3], labels = names(tab), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.axis)
					}				
				}				
                title(main = main, font.main = 4)
                width <- rep(1, length.out = n.levs.2)
                delta <- width/2
                w.r <- cumsum(0.2 + width)
                w.m <- w.r - delta
                if (error) 
                  for (i in 1:length(tab)) if (stdeviation[i] != 0) 
                  arrows(w.m[i], tab[i] - stdeviation[i], w.m[i],tab[i] + stdeviation[i], angle = 90, code = 3, lty = 1, lwd = 2, length = 0.125 * 3/length(tab))
            }
			#Groupe et sous groupe
            if (length(listfactor) == 2) {
            factor <- listfactor[2]
            repetition <- listfactor[1]
            factorname <- factor
            responsename <- response
            repetitionname <- repetition
            factor <- with(data, eval(parse(text = factor)))
            response <- with(data, eval(parse(text = response)))
            repetition <- with(data, eval(parse(text = repetition)))
            data <- cbind(as.character(factor), as.character(repetition),response)
            colnames(data) <- c(factorname, repetitionname,responsename)
            data <- as.data.frame(data)
            data[, 3] <- as.numeric(as.character(data[, 3]))
            tab <- tapply(response, list(repetition = factor(repetition),factor = factor(factor)), mean, na.rm = TRUE)
            stdeviation <- tapply(response, list(repetition = factor(repetition),factor = factor(factor)), sd, na.rm = TRUE)
            sample <- tapply(response, list(repetition = factor(repetition),factor = factor(factor)), function(x) length(x[!is.na(x)]))
            stdeviation <- stdeviation/sqrt(sample)
			#Sortie graphique
			n.levs.2 <- length(levels(factor(repetition)))
			n.levs.1 <- length(levels(factor(factor)))
            marge <- 0.3 * tan(37/180 * pi) * ((max(tab) - minimum)/length(tab))
                if (error) {
                  if (max(stdeviation) > marge) 
                  ymax1 <- max(tab) + max(stdeviation)
                  else ymax1 <- max(tab) + marge
                }
                else ymax1 <- max(tab) + marge
                if (is.null(ylab)) 
                ylab <- paste("Mean", responsename)
                if (is.null(xlab)) 
                xlab <- factorname
				tab<-cbind(tab,rep(0,n.levs.2))
				col.level<-c(rep(col[1:n.levs.2],n.levs.1),rep("white",n.levs.2))
				bord.level<-c(rep(rep("black",n.levs.2),n.levs.1),rep("white",n.levs.2))
				if (dim=="2D"){
					if (xlab.axis==0)
					barplotlim(tab, beside = TRUE, col = col.level,border=bord.level, ymin = minimum,ymax = ymax1, ylab = ylab, xlab = xlab, cex.names = cex.names,cex.axis = cex.axis)
					else{
					mp <- barplotlim(tab, beside = TRUE, col = col.level,border=bord.level, ymin = minimum,ymax = ymax1, ylab = ylab, xlab = xlab, axisnames = FALSE)
					nbre.leg<-dim(mp)[1]
					coord.lab<-(mp[nbre.leg,]-mp[1,])/2+mp[1,]
					text(coord.lab, par("usr")[3], labels = colnames(tab), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.axis)
					}
				}
				if (dim=="3D"){
					if (xlab.axis==0)
					barplotlim3D(tab,beside = TRUE, col = col.level,border=bord.level, ymin = minimum,ymax = ymax1, ylab = ylab, xlab = xlab, cex.names = cex.names,cex.axis = cex.axis)
					else{
					mp <-barplotlim3D(tab,beside = TRUE, col = col.level,border=bord.level, ymin = minimum,ymax = ymax1, ylab = ylab, xlab = xlab, axisnames = FALSE)
					nbre.leg<-dim(mp)[1]
					coord.lab<-(mp[nbre.leg,]-mp[1,])/2+mp[1,]
					text(coord.lab, par("usr")[3], labels = colnames(tab), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.axis)
					}
				}				
            title(main = main, font.main = 4)
			#rajout legende
				if(is.null(legend.lab))
				legend.lab<-repetitionname
            legend(legend.pos, levels(factor(repetition)),fill = col[1:n.levs.2], cex = cex.legend, bty = "n", title = legend.lab)	
            space <- rep.int(c(1, rep.int(0, nrow(tab) - 1)), ncol(tab))
            width <- rep(1, length.out = nrow(tab))
            delta <- width/2
            w.r <- cumsum(space + width)
            w.m <- w.r - delta
                if (error) 
					for (i in 1:length(tab)) {
					if (stdeviation[i] != 0) 
						arrows(w.m[i], tab[i] - stdeviation[i], w.m[i],tab[i] + stdeviation[i], angle = 90, code = 3, lty = 1, lwd = 2, length = 0.125 * 3/length(tab))
					}
            }
        }
		#repetee
        if (length(listresponse) > 1) {
			#Un seul groupe
            if (length(listfactor) == 1) {
            factorname <- factor
            responsename <- listresponse
            factor <- with(data, eval(parse(text = factor)))
            datatab <- NULL
                for (i in 1:length(listresponse))
                datatab <- cbind(datatab, with(data, eval(parse(text = listresponse[i]))))
            colnames(datatab) <- responsename
            data <- as.data.frame(cbind(as.character(factor),datatab))
                for (i in 1:length(responsename) + 1) {
                  data[, i] <- as.numeric(as.character(data[,i]))
                }
            tabf <- NULL
            stdeviationf <- NULL
            samplef <- NULL
                for (i in 1:length(responsename) + 1) {
                tabf <- rbind(tabf, tapply(data[, i], factor(data[,1]), function(x) mean(x, na.rm = TRUE)))
                stdeviationf <- rbind(stdeviationf, tapply(data[,i], factor(data[, 1]), function(x) sd(x,na.rm = TRUE)))
                samplef <- rbind(samplef, tapply(data[, i], factor(data[,1]), function(x) length(x[!is.na(x)])))
                }
            stdeviationf <- stdeviationf/sqrt(samplef)
			n.levs.2 = length(listresponse)
			n.levs.1 <- length(levels(factor(factor)))
			stdeviationr<-t(stdeviationf)
			tabr<-t(tabf)
			colnames(tabr)<-listresponse
			tabf<-cbind(tabf,rep(0,n.levs.2))
			tabr<-cbind(tabr,rep(0,n.levs.1))
			stdeviationf<-cbind(stdeviationf,rep(0,n.levs.2))
			stdeviationr<-cbind(stdeviationr,rep(0,n.levs.1))			
				if(strata=="repeated") {
				tab<-tabf
				col.level<-c(rep(col[1:n.levs.2],n.levs.1),rep("white",n.levs.2))
				bord.level<-c(rep(rep("black",n.levs.2),n.levs.1),rep("white",n.levs.2))
				stdeviation<-stdeviationf
				text.legend<-listresponse
					if (is.null(name.repeated)) 
					name.repeated <- "Variable"		
					if (is.null(xlab)) 
					xlab <- factorname
					if (is.null(legend.lab))
					legend.lab <- name.repeated
				}
				if(strata=="factor"){
				tab<-tabr
				col.level<-c(rep(col[1:n.levs.1],n.levs.2),rep("white",n.levs.1))
				bord.level<-c(rep(rep("black",n.levs.1),n.levs.2),rep("white",n.levs.1))
				stdeviation<-stdeviationr
				text.legend<-levels(factor(factor))
					if (is.null(name.repeated)) 
					name.repeated <- "Variable"		
					if (is.null(xlab)) 
					xlab <- name.repeated
					if (is.null(legend.lab))
					legend.lab <- factorname
				} 
			marge <- 0.3 * tan(37/180 * pi) * ((max(tab) -minimum)/length(tab))
                if (error) {
                  if (max(stdeviation) > marge) 
                  ymax1 <- max(tab) + max(stdeviation)
                  else ymax1 <- max(tab) + marge
                }
                else ymax1 <- max(tab) + marge
                if (is.null(ylab)) 
                ylab <- paste("Mean", name.repeated)
				if (dim=="2D"){
					if (xlab.axis==0)
					barplotlim(tab, beside = TRUE, col = col.level,border=bord.level, ymin = minimum,ymax = ymax1, ylab = ylab, xlab = xlab, cex.names = cex.names,cex.axis = cex.axis)
					else{
					mp <- barplotlim(tab, beside = TRUE, col = col.level,border=bord.level, ymin = minimum,ymax = ymax1, ylab = ylab, xlab = xlab, axisnames = FALSE)
					nbre.leg<-dim(mp)[1]
					coord.lab<-(mp[nbre.leg,]-mp[1,])/2+mp[1,]					
					text(coord.lab, par("usr")[3], labels = colnames(tab), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.axis)
					}	
				}
				if (dim=="3D"){
					if (xlab.axis==0)
					barplotlim3D(tab, beside = TRUE, col = col.level,border=bord.level, ymin = minimum,ymax = ymax1, ylab = ylab, xlab = xlab, cex.names = cex.names,cex.axis = cex.axis)
					else{
					mp <- barplotlim3D(tab, beside = TRUE, col = col.level,border=bord.level, ymin = minimum,ymax = ymax1, ylab = ylab, xlab = xlab, axisnames = FALSE)
					nbre.leg<-dim(mp)[1]
					coord.lab<-(mp[nbre.leg,]-mp[1,])/2+mp[1,]					
					text(coord.lab, par("usr")[3], labels = colnames(tab), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.axis)
					}	
				}
            title(main = main, font.main = 4)
            legend(legend.pos, text.legend, fill = col.level,bty = "n", cex = cex.legend, title = legend.lab)
            space <- rep.int(c(1, rep.int(0, nrow(tab) -1)), ncol(tab))
            width <- rep(1, length.out = nrow(tab))
            delta <- width/2
            w.r <- cumsum(space + width)
            w.m <- w.r - delta
                if (error) 
                  for (i in 1:length(tab)) if (stdeviation[i] !=0) 
                  arrows(w.m[i], tab[i] - stdeviation[i], w.m[i], tab[i] + stdeviation[i], angle = 90, code = 3, lty = 1, lwd = 2, length = 0.125 * 3/length(tab))
            }
			#Groupe et sous groupe
            if (length(listfactor) == 2) {
            factorname <- listfactor[1]
            responsename <- listresponse
            repetitionname <- listfactor[2]
            factor <- with(data, eval(parse(text = listfactor[1])))
            repetition <- with(data, eval(parse(text = listfactor[2])))
            datatab <- NULL
                for (i in 1:length(listresponse))
                datatab <- cbind(datatab, with(data, eval(parse(text = listresponse[i]))))
            datatab <- as.data.frame(datatab)
            colnames(datatab) <- responsename
            data <- as.data.frame(cbind(as.character(factor),as.character(repetition), datatab))
                for (i in 1:length(responsename) + 2)
                data[, i] <- as.numeric(as.character(data[,i]))
            data <- reshape(data = data, varying = list(listresponse),direction = "long", v.name = "resp")
            data$time <- factor(data$time, labels = listresponse)
            data$id <- NULL
            colnames(data)[1:2] <- c("factor", "repetition")
            tabf <- tapply(data$resp, list(response = data$time,factor = data$factor, repetition = data$repetition), function(x) mean(x, na.rm = TRUE))
			tabr <- tapply(data$resp, list(factor = data$factor,response = data$time, repetition = data$repetition), function(x) mean(x, na.rm = TRUE))
            stdeviationf <- tapply(data$resp, list(response = data$time, factor = data$factor, repetition = data$repetition), function(x) sd(x, na.rm = TRUE))
			stdeviationr <- tapply(data$resp, list( factor = data$factor, response = data$time,repetition = data$repetition), function(x) sd(x, na.rm = TRUE))
            samplef <- tapply(data$resp, list(response = data$time, factor = data$factor, repetition = data$repetition),function(x) length(x[!is.na(x)]))
			sampler <- tapply(data$resp, list( factor = data$factor,response = data$time, repetition = data$repetition),function(x) length(x[!is.na(x)]))
            stdeviationf <- stdeviationf/sqrt(samplef)
			stdeviationr <- stdeviationr/sqrt(sampler)
			n.levs.2 = length(listresponse)
			n.levs.1 <- length(levels(factor(factor)))
			n.row<-length(levels(factor(repetition)))
			name.row<-levels(factor(repetition))
			#sortie du dessin
			layout(matrix(c(1,seq(2,n.row+1)),n.row+1,1,byrow=TRUE),widths=28*n.row+4,heights=c(4,rep(28,n.row)),TRUE)
			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
			text(.5,.5,main,cex=1.5)
			par(mar=c(4,4,0,2))
				for (i in 1:n.row){
					if(strata=="repeated") {
					stdeviation<-stdeviationf[,,i]
					tab<-tabf[,,i]
					tab<-cbind(tab,rep(0,n.levs.2))
					stdeviation<-cbind(stdeviation,rep(0,n.levs.2))
					col.level<-c(rep(col[1:n.levs.2],n.levs.1),rep("white",n.levs.2))
					bord.level<-c(rep(rep("black",n.levs.2),n.levs.1),rep("white",n.levs.2))
					text.legend<-listresponse
						if (is.null(name.repeated)) 
						name.repeated <- "Variable"		
						if (is.null(legend.lab))
						legend.lab <- name.repeated
					}
					if(strata=="factor"){
					tab<-tabr[,,i]
					stdeviation<-stdeviationr[,,i]
					tab<-cbind(tab,rep(0,n.levs.1))
					stdeviation<-cbind(stdeviation,rep(0,n.levs.1))
					col.level<-c(rep(col[1:n.levs.1],n.levs.2),rep("white",n.levs.1))
					bord.level<-c(rep(rep("black",n.levs.1),n.levs.2),rep("white",n.levs.1))
					text.legend<-levels(factor(factor))
						if (is.null(name.repeated)) 
						name.repeated <- "Variable"		
						if (is.null(legend.lab))
						legend.lab <- factorname
					} 
				marge <- 0.3 * tan(37/180 * pi) * ((max(tab) -minimum)/length(tab))
					if (is.null(xlab)) 
					x.label <- paste(listfactor[2],"=",name.row[i])
					else
					x.label<-xlab[i]
					if (error) {
						if (max(stdeviation) > marge) 
						ymax1 <- max(tab) + max(stdeviation)
						else ymax1 <- max(tab) + marge
					}
					else ymax1 <- max(tab) + marge
					if (is.null(ylab)) 
					ylab <- paste("Mean", name.repeated)	
					if (dim=="2D"){
						if (xlab.axis==0)
						barplotlim(tab, beside = TRUE, col = col.level,border=bord.level, ymin = minimum,ymax = ymax1, ylab = ylab, xlab = x.label, cex.names = cex.names,cex.axis = cex.axis)
						else{
						mp <- barplotlim(tab, beside = TRUE, col = col.level,border=bord.level, ymin = minimum,ymax = ymax1, ylab = ylab, xlab = x.label, axisnames = FALSE)
						nbre.leg<-dim(mp)[1]
						coord.lab<-(mp[nbre.leg,]-mp[1,])/2+mp[1,]								
						text(coord.lab, par("usr")[3], labels = colnames(tab), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.axis)
						}	
					}
					if (dim=="3D"){
						if (xlab.axis==0)
						barplotlim3D(tab, beside = TRUE, col = col.level,border=bord.level, ymin = minimum,ymax = ymax1, ylab = ylab, xlab = x.label, cex.names = cex.names,cex.axis = cex.axis)
						else{
						mp <- barplotlim3D(tab, beside = TRUE, col = col.level,border=bord.level, ymin = minimum,ymax = ymax1, ylab = ylab, xlab = x.label, axisnames = FALSE)
						nbre.leg<-dim(mp)[1]
						coord.lab<-(mp[nbre.leg,]-mp[1,])/2+mp[1,]								
						text(coord.lab, par("usr")[3], labels = colnames(tab), srt = xlab.axis, adj = c(1.1,1.1), xpd = TRUE, cex=cex.axis)
						}	
					}	
				legend(legend.pos, text.legend, fill = col.level,bty = "n", cex = cex.legend, title = legend.lab)
				space <- rep.int(c(1, rep.int(0, nrow(tab) -1)), ncol(tab))
				width <- rep(1, length.out = nrow(tab))
				delta <- width/2
				w.r <- cumsum(space + width)
				w.m <- w.r - delta
					if (error) 
						for (i in 1:length(tab)) if (stdeviation[i] !=0) 
						arrows(w.m[i], tab[i] - stdeviation[i], w.m[i], tab[i] + stdeviation[i], angle = 90, code = 3, lty = 1, lwd = 2, length = 0.125 * 3/length(tab))					
					}				
            }
        }
    }
}

#########################################################################
#barplot avec limite inf
barplotlim<-function (height, width = 1, space = NULL, names.arg = NULL,legend.text = NULL, beside = FALSE, horiz = FALSE, density = NULL,angle = 45, col = NULL,
border = par("fg"), main = NULL,sub = NULL, xlab = NULL, ylab = NULL, ymin = NULL, ymax=NULL,xpd = TRUE, log = "", axes = TRUE, axisnames = TRUE,
 cex.axis = par("cex.axis"),cex.names = par("cex.axis"), inside = TRUE, plot = TRUE,axis.lty = 0, offset = 0, add = FALSE, ...)
{
    if (!missing(inside))
        .NotYetUsed("inside", error = FALSE)
    if (is.null(space))
        space <- if (is.matrix(height) && beside)
            c(0, 1)
        else 0.2
    space <- space * mean(width)
    if (plot && axisnames && is.null(names.arg))
        names.arg <- if (is.matrix(height))
            colnames(height)
        else names(height)
    if (is.vector(height) || (is.array(height) && (length(dim(height)) ==
        1))) {
        height <- cbind(height)
        beside <- TRUE
        if (is.null(col))
            col <- "grey"
    }
    else if (is.matrix(height)) {
        if (is.null(col))
            col <- grey.colors(nrow(height))
    }
    else stop("'height' must be a vector or a matrix")
    if (is.logical(legend.text))
        legend.text <- if (legend.text && is.matrix(height))
            rownames(height)
    stopifnot(is.character(log))
    logx <- logy <- FALSE
    if (log != "") {
        logx <- any(grep("x", log))
        logy <- any(grep("y", log))
    }
    if ((logx || logy) && !is.null(density))
        stop("Cannot use shading lines in bars when log scale is used")
    NR <- nrow(height)
    NC <- ncol(height)
    if (beside) {
        if (length(space) == 2)
            space <- rep.int(c(space[2], rep.int(space[1], NR -
                1)), NC)
        width <- rep(width, length.out = NR)
    }
    else {
        width <- rep(width, length.out = NC)
    }
    offset <- rep(as.vector(offset), length.out = length(width))
    delta <- width/2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    log.dat <- (logx && horiz) || (logy && !horiz)
    if (log.dat) {
        if (min(height + offset) <= 0)
            stop("log scale error: at least one 'height + offset' value <= 0")
        if (logx && !is.null(xlim) && min(xlim) <= 0)
            stop("log scale error: 'xlim' <= 0")
        if (logy && !is.null(ylim) && min(ylim) <= 0)
            stop("log scale error: 'ylim' <= 0")
        rectbase <- if (logy && !horiz && !is.null(ylim))
            ylim[1]
        else if (logx && horiz && !is.null(xlim))
            xlim[1]
        else 0.9 * min(height)
    }
    else rectbase <- 0
    if (beside && !is.null(ymin)) rectbase<-ymin
    if (!beside)
        height <- rbind(rectbase, apply(height, 2, cumsum))
    rAdj <- offset + (if (log.dat)
        0.9 * height
    else -0.01 * height)
    delta <- width/2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    if (horiz) {
            xlim <- range(rAdj, height + offset, na.rm = TRUE)
    if (beside && !is.null(ymin)) {
    xlim<-c(ymin,max(height,na.rm=TRUE))
    if (beside && !is.null(ymax)) xlim<-c(ymin,ymax)
    }
            ylim <- c(min(w.l), max(w.r))

    }
    else {

            xlim <- c(min(w.l), max(w.r))
            ylim <- range(rAdj, height + offset, na.rm = TRUE)
    if (beside && !is.null(ymin)) {
    ylim<-c(ymin,max(height,na.rm=TRUE))
    if (beside && !is.null(ymax)) ylim<-c(ymin,ymax)
      }
    }
    if (beside)
        w.m <- matrix(w.m, ncol = NC)
    if (plot) {
        opar <- if (horiz)
            par(xaxs = "i", xpd = xpd)
        else par(yaxs = "i", xpd = xpd)
        on.exit(par(opar))
        if (!add) {
            plot.new()
            plot.window(xlim, ylim, log = log, ...)
        }
        xyrect <- function(x1, y1, x2, y2, horizontal = TRUE,
            ...) {
            if (horizontal)
                rect(x1, y1, x2, y2, ...)
            else rect(y1, x1, y2, x2, ...)
        }
        if (beside){
		recthight<-c(height)
		if (!is.null(ymin))
		for (i in 1:length(recthight)) if (recthight[i]<ymin) recthight[i]=ymin
            xyrect(rectbase + offset, w.l, recthight + offset,
                w.r, horizontal = horiz, angle = angle, density = density,
                col = col, border = border)
		}
        else {
            for (i in 1:NC) {
                xyrect(height[1:NR, i] + offset[i], w.l[i], height[-1,
                  i] + offset[i], w.r[i], horizontal = horiz,
                  angle = angle, density = density, col = col,
                  border = border)
            }
        }
        if (axisnames && !is.null(names.arg)) {
            at.l <- if (length(names.arg) != length(w.m)) {
                if (length(names.arg) == NC)
                  colMeans(w.m)
                else stop("incorrect number of names")
            }
            else w.m
            axis(if (horiz)
                2
            else 1, at = at.l, labels = names.arg, lty = axis.lty,
                cex.axis = cex.names, ...)
        }
        if (!is.null(legend.text)) {
            legend.col <- rep(col, length.out = length(legend.text))
            if ((horiz & beside) || (!horiz & !beside)) {
                legend.text <- rev(legend.text)
                legend.col <- rev(legend.col)
                density <- rev(density)
                angle <- rev(angle)
            }
            xy <- par("usr")
            legend(xy[2] - xinch(0.1), xy[4] - yinch(0.1), legend = legend.text,
                angle = angle, density = density, fill = legend.col,
                xjust = 1, yjust = 1)
        }
        title(main = main, sub = sub, xlab = xlab, ylab = ylab,
            ...)
        if (axes)
            axis(if (horiz)
                1
            else 2, cex.axis = cex.axis, ...)
        invisible(w.m)
    }
    else w.m
}

############################################################################
#barplot 3D avec limite inf
barplotlim3D<-function (height, width = 1, space = NULL, names.arg = NULL,legend.text = NULL, beside = FALSE, horiz = FALSE, density = NULL,angle = 45, col = NULL,
border = par("fg"), main = NULL,sub = NULL, xlab = NULL, ylab = NULL, ymin=NULL,ymax=NULL,xpd = TRUE, log = "", axes = TRUE, axisnames = TRUE,
cex.axis = par("cex.axis"),cex.names = par("cex.axis"), inside = TRUE, plot = TRUE,axis.lty = 0, offset = 0, add = FALSE, ...)
{
lozenge<-function (xleft, ybottom, xright, ytop, density = NULL, side=c("upper","side")
,tilt=NULL,angle = 45,col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"),
    ...)
{
  axis1<-par("usr")[2]-par("usr")[1]
  axis2<-par("usr")[4]-par("usr")[3]
  scalex<-axis2/axis1
  scaley<-axis1/axis2
  n <- range(length(xleft), length(xright), length(ybottom),
            length(ytop))
        if (n[1] == 0)
            stop("invalid lozenge specification")
        n <- n[2]
    if (is.numeric(density) && all(is.na(density) | density <
        0))
        density <- NULL
    if (!is.null(density) && !is.null(angle)) {
        if (is.logical(border) && !is.na(border)) {
            if (border)
                border <- col
            else border <- NA
        }

	if(side=="upper"){
	  d<-(ytop-ybottom)*scaley/tan((tilt/180)*pi)
        x <- rbind(rep.int(NA, n), xleft, xright-d, xright, xleft+d)[-1]
        y <- rbind(rep.int(NA, n), ybottom, ybottom, ytop, ytop)[-1]
		}
	if(side=="side"){
	  d<-(xright-xleft)*scalex*tan((tilt/180)*pi)
        x <- rbind(rep.int(NA, n), xleft, xright, xright, xleft)[-1]
        y <- rbind(rep.int(NA, n), ybottom, ybottom+d, ytop, ytop-d)[-1]
		}
	  polygon(x, y, col = col, border = border, lty = lty,
            lwd = lwd, density = density, angle = angle, ...)
    }
   else {
	if(side=="upper"){
	  d<-(ytop-ybottom)*scaley/tan((tilt/180)*pi)
        x <- rbind(rep.int(NA, n), xleft, xright-d, xright, xleft+d)[-1]
        y <- rbind(rep.int(NA, n), ybottom, ybottom, ytop, ytop)[-1]
		}
	if(side=="side"){
	  d<-(xright-xleft)*scalex*tan((tilt/180)*pi)
        x <- rbind(rep.int(NA, n), xleft, xright, xright, xleft)[-1]
        y <- rbind(rep.int(NA, n), ybottom, ybottom+d, ytop, ytop-d)[-1]
		}
        polygon(x, y, col = col, border = border, lty = lty,
            lwd = lwd, ...)
	}
}
    if (!missing(inside))
        .NotYetUsed("inside", error = FALSE)
    if (is.null(space))
        space <- if (is.matrix(height) && beside)
            c(0, 1)
        else 0.2
    space <- space * mean(width)
    if (plot && axisnames && is.null(names.arg))
        names.arg <- if (is.matrix(height))
            colnames(height)
        else names(height)
    if (is.vector(height) || (is.array(height) && (length(dim(height)) ==
        1))) {
        height <- cbind(height)
        beside <- TRUE
        if (is.null(col))
            col <- "grey"
    }
    else if (is.matrix(height)) {
        if (is.null(col))
            col <- grey.colors(nrow(height))
    }
    else stop("'height' must be a vector or a matrix")
    if (is.logical(legend.text))
        legend.text <- if (legend.text && is.matrix(height))
            rownames(height)
    stopifnot(is.character(log))
    logx <- logy <- FALSE
    if (log != "") {
        logx <- any(grep("x", log))
        logy <- any(grep("y", log))
    }
    if ((logx || logy) && !is.null(density))
        stop("Cannot use shading lines in bars when log scale is used")
    NR <- nrow(height)
    NC <- ncol(height)
    if (beside) {
        if (length(space) == 2)
            space <- rep.int(c(space[2], rep.int(space[1], NR -
                1)), NC)
        width <- rep(width, length.out = NR)
    }
    else {
        width <- rep(width, length.out = NC)
    }
    offset <- rep(as.vector(offset), length.out = length(width))
    delta <- width/2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    log.dat <- (logx && horiz) || (logy && !horiz)
    if (log.dat) {
        if (min(height + offset) <= 0)
            stop("log scale error: at least one 'height + offset' value <= 0")
        if (logx && !is.null(xlim) && min(xlim) <= 0)
            stop("log scale error: 'xlim' <= 0")
        if (logy && !is.null(ylim) && min(ylim) <= 0)
            stop("log scale error: 'ylim' <= 0")
        rectbase <- if (logy && !horiz && !is.null(ylim))
            ylim[1]
        else if (logx && horiz && !is.null(xlim))
            xlim[1]
        else 0.9 * min(height)
    }
    else rectbase <- 0
    if (beside && !is.null(ymin)) rectbase<-ymin
    if (!beside)
        height <- rbind(rectbase, apply(height, 2, cumsum))
    rAdj <- offset + (if (log.dat)
        0.9 * height
    else -0.01 * height)
    delta <- width/2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    if (horiz) {
            xlim <- range(rAdj, height + offset, na.rm = TRUE)
    if (beside && !is.null(ymin)) {
    xlim<-c(ymin,max(height,na.rm=TRUE))
    if (beside && !is.null(ymax)) xlim<-c(ymin,ymax)
    }
            ylim <- c(min(w.l), max(w.r)+.3)
    }
    else {
            xlim <- c(min(w.l), max(w.r)+.3)
            ylim <- range(rAdj, height + offset, na.rm = TRUE)
    if (beside && !is.null(ymin)) {
    ylim<-c(ymin,max(height,na.rm=TRUE))
    if (beside && !is.null(ymax)) ylim<-c(ymin,ymax)
    }
        }
    if (beside)
        w.m <- matrix(w.m, ncol = NC)
    if (plot) {
        opar <- if (horiz)
            par(xaxs = "i", xpd = xpd)
        else par(yaxs = "i", xpd = xpd)
        on.exit(par(opar))
        if (!add) {
            plot.new()
            plot.window(xlim, ylim, log = log, ...)
  		axis1<-par("usr")[2]-par("usr")[1]
  		axis2<-par("usr")[4]-par("usr")[3]
  		scalex<-axis2/axis1
  		scaley<-axis1/axis2
        }
        xyrect <- function(x1, y1, x2, y2, horizontal = TRUE,
            ...) {
            if (horizontal)
                rect(x1, y1, x2, y2, ...)
            else rect(y1, x1, y2, x2, ...)
        }

        if (beside){
		recthight<-c(height)
		if (!is.null(ymin))
		for (i in 1:length(recthight)) if (recthight[i]<ymin) recthight[i]=ymin
 		if (horiz){
            lozenge(recthight-recthight+ymin+offset,w.r,recthight + offset+.3*scaley*tan(53/180*pi),w.r+.3,
		    side="upper",tilt=37, angle = angle, density = density,
                col = col, border = border)
            lozenge(recthight + offset,w.l, recthight + offset+.3*scaley*tan(53/180*pi),w.r+.3,
                 tilt=37,side="side", angle = angle, density = density,
                col = col, border = border)
			}
		else {
           lozenge(w.r,recthight-recthight+ymin + offset,w.r+.3, recthight + offset+.3*scalex*tan(37/180*pi),
                 tilt=37,side="side", angle = angle, density = density,
                col = col, border = border)
            lozenge(w.l,recthight+offset,w.r+.3,  recthight + offset+.3*scalex*tan(37/180*pi),
		    side="upper",tilt=37, angle = angle, density = density,
                col = col, border = border)
			}
            xyrect(rectbase + offset, w.l, recthight + offset,
                w.r, horizontal = horiz, angle = angle, density = density,
                col = col, border = border)
			}
        else {

            for (i in 1:NC) {
		if (horiz){
lozenge(height[-1, i]+offset[i],rep(w.l[i],NR),height[-1,i] + offset[i]+.15*scaley*tan(53/180*pi),rep(w.r[i],NR)+.15,
angle = angle, side="side",tilt=37, col = col,density=density, border = border)
lozenge(height[1:NR, i]+offset[i],rep(w.r[i],NR),height[-1,i] + offset[i]+.15*scaley*tan(53/180*pi),rep(w.r[i],NR)+.15,
angle = angle, side="upper",tilt=37, col = col,density=density, border = border)

		}
		else {
lozenge( rep(w.l[i],NR),height[-1, i] + offset[i],rep(w.r[i],NR)+.2, height[-1,i] + offset[i]+.2*scalex*tan(37/180*pi),
 angle = angle, side="upper",tilt=37, col = col,density=density, border = border)
lozenge( rep(w.r[i],NR),height[1:NR, i] + offset[i],rep(w.r[i],NR)+.2, height[-1,i] + offset[i]+.2*scalex*tan(37/180*pi),
 angle = angle, side="side",tilt=37, col = col,density=density, border = border)
		}
                xyrect(height[1:NR, i] + offset[i], w.l[i], height[-1,
                  i] + offset[i], w.r[i], horizontal = horiz,
                  angle = angle, density = density, col = col,
                  border = border)
            }
        }
        if (axisnames && !is.null(names.arg)) {
            at.l <- if (length(names.arg) != length(w.m)) {
                if (length(names.arg) == NC)
                  colMeans(w.m)
                else stop("incorrect number of names")
            }
            else w.m
            axis(if (horiz)
                2
            else 1, at = at.l, labels = names.arg, lty = axis.lty,
                cex.axis = cex.names, ...)
        }
        if (!is.null(legend.text)) {
            legend.col <- rep(col, length.out = length(legend.text))
            if ((horiz & beside) || (!horiz & !beside)) {
                legend.text <- rev(legend.text)
                legend.col <- rev(legend.col)
                density <- rev(density)
                angle <- rev(angle)
            }
            xy <- par("usr")
            legend(xy[2] - xinch(0.1), xy[4] - yinch(0.1), legend = legend.text,
                angle = angle, density = density, fill = legend.col,
                xjust = 1, yjust = 1)
        }
        title(main = main, sub = sub, xlab = xlab, ylab = ylab,
            ...)
        if (axes)
            axis(if (horiz)
                1
            else 2, cex.axis = cex.axis, ...)
        invisible(w.m)
    }
    else w.m
}
########################################################################################################################################################
# Boites de dispersion 3D
boxplot3D<-function (formula,range = 1.5,width=NULL,varwidth=FALSE,ylim=NULL,main=NULL,data=NULL,boxwex=NULL,notch=FALSE,xlab=NULL,ylab=NULL,outline=TRUE,names=NULL,
plot = TRUE, border = par("fg"),col = NULL, log = "", pars = list(boxwex = 0.8, staplewex = 0.5,outwex = 0.5),axes=TRUE, horizontal = FALSE, add = FALSE, at = NULL)
{
	lozenge<-function (xleft, ybottom, xright, ytop,  side=c("upper","side"),tilt=NULL,col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"),...)
	{
	axis1<-par("usr")[2]-par("usr")[1]
	axis2<-par("usr")[4]-par("usr")[3]
	scalex<-axis2/axis1
	scaley<-axis1/axis2
	n <- range(length(xleft), length(xright), length(ybottom),length(ytop))
       if (n[1] == 0)
       stop("invalid lozenge specification")
    n <- n[2]
		if(side=="upper"){
		d<-(ytop-ybottom)*scaley/tan((tilt/180)*pi)
        x <- rbind(rep.int(NA, n), xleft, xright-d, xright, xleft+d)[-1]
        y <- rbind(rep.int(NA, n), ybottom, ybottom, ytop, ytop)[-1]
		}
		if(side=="side"){
		d<-(xright-xleft)*scalex*tan((tilt/180)*pi)
        x <- rbind(rep.int(NA, n), xleft, xright, xright, xleft)[-1]
        y <- rbind(rep.int(NA, n), ybottom, ybottom+d, ytop, ytop-d)[-1]
		}
    polygon(x, y, col = col, border = border, lty = lty,lwd = lwd, ...)
	}
	if (is.numeric(formula)==TRUE){
	graph<-boxplot(formula, range = range, width = width, varwidth = varwidth,axes=axes,notch = notch, outline = outline,plot = plot,xlab=xlab,main=main,
	ylab=ylab,ylim=ylim,border = border, col = col, log =log,pars = pars,horizontal = horizontal, add = add, at = at)
  	axis1<-par("usr")[2]-par("usr")[1]
  	axis2<-par("usr")[4]-par("usr")[3]
  	scalex<-axis2/axis1
  	scaley<-axis1/axis2
		if(!horizontal&&plot){
		lozenge(1.2,graph$stats[2],1.3,graph$stats[4]+.1*scalex*tan(37/180*pi),tilt=37,side="side",col = col, border = border)
		lines(c(1.2,1.3),c(graph$stats[3],graph$stats[3]+.1*scalex*tan(37/180*pi)),lwd=3)
		lozenge(.8,graph$stats[4],1.3,graph$stats[4]+.1*scalex*tan(37/180*pi),side="upper",tilt=37,col = col, border = border)
		lines(c(1,1),c(graph$stats[4]+.05*scalex*tan(37/180*pi),graph$stats[4]+.1*scalex*tan(37/180*pi)),lty=2)
		}
		if(horizontal&&plot){
		lozenge(graph$stats[4],.8,graph$stats[4]+.1*scaley*tan(53/180*pi),1.3,tilt=37,side="side",col = col, border = border)
		lozenge(graph$stats[2],1.2,graph$stats[4]+.1*scaley*tan(53/180*pi),1.3,side="upper",tilt=37,col = col, border = border)
		lines(c(graph$stats[3],graph$stats[3]+.1*scaley*tan(53/180*pi)),c(1.2,1.3),lwd=3)
		lines(c(graph$stats[4]+.05*scaley*tan(53/180*pi),graph$stats[4]+.1*scaley*tan(53/180*pi)),c(1,1),lty=2)
		}
	}
	if (is.numeric(formula)==FALSE){
    f <- formula(formula)
    listvar <- as.character(attr(terms(f), "variables"))[-1]
		if (length(listvar)>1) {
			if (is.null(data)){
			x <- eval(parse(text=listvar[1])) 
			y <- eval(parse(text=listvar[2]))
			}  
			else {
			x <- eval(parse(text=paste0("data$",listvar[1]))) 
			y <- eval(parse(text=paste0("data$",listvar[2])))
			}
		
			if (length(listvar)==2){
			graph<- boxplot(x~y, range = range, width = width, varwidth = varwidth,ylim=ylim,notch = notch,boxwex=boxwex, outline = outline,  plot = plot,xlab=xlab,main=main,
			ylab=ylab,axes=axes,border = border, col = col, log =log,pars = pars,horizontal = horizontal, add = add, at = at)
			axis1<-par("usr")[2]-par("usr")[1]
			axis2<-par("usr")[4]-par("usr")[3]
			scalex<-axis2/axis1
			scaley<-axis1/axis2
				if(!horizontal&&plot){
				lozenge(seq(1,length(graph$stats[2,]))+.4,graph$stats[2,],seq(1,length(graph$stats[2,]))+.5,graph$stats[4,]+.1*scalex*tan(37/180*pi),
				tilt=37,side="side",col = col, border = border)
					for (i in 1:length(graph$stats[2,]))
					lines(c(i+.4,i+.5),c(graph$stats[3,i],graph$stats[3,i]+.1*scalex*tan(37/180*pi)),lwd=3)
				lozenge(seq(1,length(graph$stats[2,]))-.4,graph$stats[4,],seq(1,length(graph$stats[2,]))+.5,graph$stats[4,]+.1*scalex*tan(37/180*pi),
				side="upper",tilt=37,col = col, border = border)
					for (i in 1:length(graph$stats[2,]))
					lines(c(i,i),c(graph$stats[4,i]+.05*scalex*tan(37/180*pi),graph$stats[4,i]+.1*scalex*tan(37/180*pi)),lty=2)
				}
				if(horizontal&&plot){
				lozenge(graph$stats[4,],seq(1,length(graph$stats[2,]))-.4,graph$stats[4,]+.1*scaley*tan(53/180*pi),seq(1,length(graph$stats[2,]))+.5,
				tilt=37,side="side",col = col, border = border)
				lozenge(graph$stats[2,],seq(1,length(graph$stats[2,]))+.4,graph$stats[4,]+.1*scaley*tan(53/180*pi),seq(1,length(graph$stats[2,]))+.5,
				side="upper",tilt=37,col = col, border = border)
					for (i in 1:length(graph$stats[2,]))
					lines(c(graph$stats[3,i],graph$stats[3,i]+.1*scaley*tan(53/180*pi)),c(i+.4,i+.5),lwd=3)
					for (i in 1:length(graph$stats[2,]))
					lines(c(graph$stats[4,i]+.05*scaley*tan(53/180*pi),graph$stats[4,i]+.1*scaley*tan(53/180*pi)),c(i,i),lty=2)
				}
			}
			if (length(listvar)==3){
				if (is.null(data))
				z <- eval(parse(text=listvar[3]))
				else
				z <- eval(parse(text=paste0("data$",listvar[3])))
			n.lev<-length(levels(z))
			col.level<- rep(col,n.lev)	
			col.box<-c(col,"white")
			col.box<-rep(col.box,n.lev)	
			graph<- boxplot(x~y+z, range = range, width = width, varwidth = varwidth,ylim=ylim,notch = notch, outline = outline, names=names,show.names=T,main=main,
			plot = plot,xlab=xlab,ylab=ylab,axes=axes,border = border, col = col.box, log =log, pars = pars,horizontal = horizontal, add = add, at = at)
			axis1<-par("usr")[2]-par("usr")[1]
			axis2<-par("usr")[4]-par("usr")[3]
			scalex<-axis2/axis1
			scaley<-axis1/axis2
				if(!horizontal&&plot){
				lozenge(seq(1,length(graph$stats[2,]))+.4,graph$stats[2,],seq(1,length(graph$stats[2,]))+.5,graph$stats[4,]+.1*scalex*tan(37/180*pi),
				tilt=37,side="side",col = col.level, border = border)
					for (i in 1:length(graph$stats[2,]))
					lines(c(i+.4,i+.5),c(graph$stats[3,i],graph$stats[3,i]+.1*scalex*tan(37/180*pi)),lwd=3)
				lozenge(seq(1,length(graph$stats[2,]))-.4,graph$stats[4,],seq(1,length(graph$stats[2,]))+.5,graph$stats[4,]+.1*scalex*tan(37/180*pi),
				side="upper",tilt=37,col = col.level, border = border)
					for (i in 1:length(graph$stats[2,]))
					lines(c(i,i),c(graph$stats[4,i]+.05*scalex*tan(37/180*pi),graph$stats[4,i]+.1*scalex*tan(37/180*pi)),lty=2)
				}
				if(horizontal&&plot){
				lozenge(graph$stats[4,],seq(1,length(graph$stats[2,]))-.4,graph$stats[4,]+.1*scaley*tan(53/180*pi),seq(1,length(graph$stats[2,]))+.5,tilt=37,
				side="side",col = col.level, border = border)
				lozenge(graph$stats[2,],seq(1,length(graph$stats[2,]))+.4,graph$stats[4,]+.1*scaley*tan(53/180*pi),seq(1,length(graph$stats[2,]))+.5,
				side="upper",tilt=37,col = col.level, border = border)
					for (i in 1:length(graph$stats[2,]))
					lines(c(graph$stats[3,i],graph$stats[3,i]+.1*scaley*tan(53/180*pi)),c(i+.4,i+.5),lwd=3)
					for (i in 1:length(graph$stats[2,]))
					lines(c(graph$stats[4,i]+.05*scaley*tan(53/180*pi),graph$stats[4,i]+.1*scaley*tan(53/180*pi)),c(i,i),lty=2)
				}
			}
		}
	}
}

############################################################################
# pour histogramme en 3D dans lattice
panel.histogram3D<-function (x, breaks, equal.widths = TRUE, type = "density", nint = round(log2(length(x)) +1), alpha = plot.polygon$alpha,
col = plot.polygon$col, border = plot.polygon$border,lty = plot.polygon$lty, lwd = plot.polygon$lwd, ...)
{
    plot.polygon <- trellis.par.get("plot.polygon")
    xscale <- current.panel.limits()$xlim
    panel.lines(x = xscale[1] + diff(xscale) * c(0.05, 0.95),
        y = c(0, 0), col = border, lty = lty, lwd = lwd, alpha = alpha)
    if (length(x) > 0) {
        if (is.null(breaks)) {
            breaks <- if (is.factor(x))
                seq_len(1 + nlevels(x)) - 0.5
            else if (equal.widths)
                do.breaks(range(x, finite = TRUE), nint)
            else quantile(x, 0:nint/nint, na.rm = TRUE)
        }
        h <- hist(x, breaks = breaks,plot=FALSE, ...)
        y <- if (type == "count")
            h$counts
        else if (type == "percent")
            100 * h$counts/length(x)
        else h$density
        breaks <- h$breaks
 	axis1<-max(breaks)-min(breaks)
	axis2<-max(y)-min(y)
      scalex<-axis2/axis1
      scaley<-axis1/axis2
        nb <- length(breaks)
        if (length(y) != nb - 1)
            warning("problem with 'hist' computations")
        if (nb > 1) {
	  ytopupper<-y+.3*diff(breaks)*scalex*tan(37/180*pi)
	  ybottomupper<-y
	  xleftupper<-breaks[-nb]
	  xrightupper<-breaks[-1]+.3*diff(breaks)
	  xleftside<-breaks[-1]
	  xrightside<-xleftside+.3*diff(breaks)
	  d<-(xrightside-xleftside)*scalex*tan((37/180)*pi)
        x1 <- rbind(rep.int(NA, 1), xleftside, xrightside, xrightside, xleftside)[-1]
        y1 <- rbind(rep.int(NA, 1), 0, d, ytopupper, ytopupper-d)[-1]
	  panel.polygon(x1,y1,col=col,...)
	  d<-(ytopupper-ybottomupper)*scaley/tan((37/180)*pi)
        x1 <- rbind(rep.int(NA, 1), xleftupper, xrightupper-d, xrightupper, xleftupper+d)[-1]
        y1 <- rbind(rep.int(NA, 1), ybottomupper, ybottomupper, ytopupper, ytopupper)[-1]
	  panel.polygon(x1,y1,col=col,...)
    panel.rect(x = breaks[-nb], y = 0, height = y, width = diff(breaks),
                col = col, alpha = alpha, border = border, lty = lty,
                lwd = lwd, just = c("left", "bottom"))
        }
    }
}
###############################################
#Pour une courbe normale
panel.norm<-function (x,breaks,type="density",lwd=2,...){
hy <- hist(x, breaks = breaks,plot=FALSE)
        yscale <- if (type == "count")
            hy$counts
        else if (type == "percent")
            100 * hy$counts/length(x)
        else hy$density
x <- do.breaks(endpoints = c(min(hy$breaks),max(hy$breaks)), nint = 49)
mean.x<-mean(x)
sd.x<-sd(x)/2
y<-dnorm(x, mean=mean.x, sd=sd.x)
mult<-max(yscale)/max(y)
y<-y*mult
panel.lines(x,y,col="black",lwd=lwd)
}
############################################################################
#pour test shapiro dans facteur (lattice)
strip.shapiro.1 <-function(which.given, var.name,which.panel,factor.levels,shapiro=TRUE,cex.strip=1, ...)
 {
	data<-trellis.panelArgs()
	test<-shapiro.test(data$x[!is.na(data$x)])
	if(test$p.value<0.001) test<-"< 0.001"
	else test<-paste("=",round(test$p.value,3))
	strip.args<-trellis.par.get("strip.background")
 if (which.given == 1) {
        panel.rect(0, 0, 1,1, col = strip.args$col, border = "black")
        if(shapiro) panel.text(x = 0.5, y = 0.5,  cex=cex.strip,lab = paste(factor.levels[which.panel[which.given]],"(p",test,")"))
        if(!shapiro) panel.text(x = 0.5, y = 0.5,  cex=cex.strip,lab = paste(factor.levels[which.panel[which.given]]))
    }
}
############################################################################
#pour test shapiro dans facteur (lattice)
strip.shapiro.2 <-function(which.given, var.name,which.panel,factor.levels,shapiro=TRUE,cex.strip=1, ...)
 {
	data<-trellis.panelArgs()
	test<-shapiro.test(data$x[!is.na(data$x)])
	if(test$p.value<0.001) test<-"< 0.001"
	else test<-paste("=",round(test$p.value,3))
	strip.args<-trellis.par.get("strip.background")
 if (which.given == 1) {
        panel.rect(0, 0, 1,1, col = strip.args$col[2], border = "black")
        panel.rect(0, 0.5, 1, 1, col = strip.args$col[1], border = "black")
        panel.text(x = 0.5, y = 0.75,  cex=cex.strip,
                   lab = factor.levels[which.panel[which.given]])
        if(shapiro) panel.text(x = 0.5, y = 0.25,  cex=cex.strip,lab = paste("(p",test,")"))
    }
}
############################################################################
#pour test shapiro dans facteur (lattice)
strip.shapiro.3 <-function(which.given, var.name,which.panel, shingle.intervals = 0,factor.levels,shapiro=TRUE,cex.strip=1, ...)
 {
	data<-trellis.panelArgs()
	test<-shapiro.test(data$x[!is.na(data$x)])
	if(test$p.value<0.001) test<-"< 0.001"
	else test<-paste("=",round(test$p.value,3))
	strip.args<-trellis.par.get("strip.background")
 if (which.given == 1) {
panel.rect(0, 0, 1, 1, col = strip.args$col[3], border = "black")
        panel.rect(0, 1/3, 1, 1, col = strip.args$col[2], border = "black")
      panel.rect(0, 2/3, 1, 1, col = strip.args$col[1], border = "black")
        panel.text(x = 0.5, y = .5,  cex=cex.strip, lab = factor.levels[which.panel[which.given]])
        if(shapiro) panel.text(x = 0.5, y = 1/6,  cex=cex.strip, lab = paste("(p",test,")"))

    }
 if (which.given == 2) {
		panel.text(x = 0.5, y = .8,  cex=cex.strip,lab = factor.levels[which.panel[which.given]])
    }
}
############################################################################
#Histogramme et normalite (affichage multiple)
histogramNormal.lattice<-function (formula,data=parent.frame(),dim=c("2D","3D"),normal=TRUE,breaks="sturges",col="turquoise",Test.shapiro=TRUE,cex.strip=1,
scales=c("same","free"),type="density",name.variable="Repeated.variable",x.label=NULL,main = NULL,xlim=NULL)
{
test.formula<-deparse(substitute(formula))
#formule avec groupe
    if (length(grep("~",test.formula))!=0){
    f <- formula(formula)
    listvar <- as.character(attr(terms(f), "variables"))[-1]
    response<-listvar[1]
	response<-paste("fact~",response)
	f<-formula(response)
	listresponse <- as.character(attr(terms(f), "variables"))[-1]
    listresponse<-listresponse[2:length(listresponse)]
	factor<-listvar[2:length(listvar)]
    factor<-chartr("|","+",factor)
	factor<-paste("fact~",factor)
	f<-formula(factor)
	listfactor <- as.character(attr(terms(f), "variables"))[-1]
    listfactor<-listfactor[2:length(listfactor)]
    }
#formule sans groupe
   else {
   listresponse<-unlist(strsplit(test.formula,"\\+"))
   listresponse<-unlist(strsplit(listresponse,"\\ "))
   listresponse<-listresponse[!listresponse==""]
    listfactor<-NA
   }
    if (missing(dim)) dim<-"3D"
    if (missing(scales)) scales<-"free"
if (dim=="2D") graphic.histo<-"panel.histogram"
if (dim=="3D") graphic.histo<-"panel.histogram3D"
#GRAPHIQUES
#######
if (!normal){
	#Sans groupe
	if (is.na(listfactor[1])==TRUE) {
		#Simple
		if (length(listresponse)==1){
		data.normal<-with(data,eval(parse(text=listresponse)))
		length.data<-length(data.normal[!is.na(data.normal)])
			if (length.data<51){
			test<-shapiro.test(data.normal[!is.na(data.normal)]) 
			text.main<-"Histogram with Shapiro-Wilk normality test"
			}
			else{
			test<-nortest::lillie.test(data.normal[!is.na(data.normal)])
			text.main<-"Histogram with Kolmogorov-Smirnov normality test"
			}
			
			if(test$p.value<0.001) test<-"< 0.001" else test<-paste("=",round(test$p.value,3))
			if (is.null(main)){
				if (Test.shapiro) main.lab  <-paste(",main='",text.main," (p",test,")'",sep="")
				else main.lab<-NULL
			}
			else main.lab<-paste(",main=\"",main,"\"",sep="")
			if (is.null(x.label)) x.label<-listresponse
			if(is.null(xlim)) text.xlim<-NULL
			else text.xlim<-paste(",xlim=c(",paste(xlim,collapse=","),")")
		eval(parse(text=paste0("dessin<-histogram(~",listresponse,",xlab='",x.label,"'",main.lab,text.xlim,",data=",deparse(substitute(data)),",type='",type,"',
		breaks='",breaks,"', col='",col,"',panel=function(x,...){",graphic.histo,"(x,...)})")))
		}
		#Repetee
		if (length(listresponse)>1){
		 	if (is.null(main)){
				if (Test.shapiro) main.lab  <-paste(",main='",text.main,"'",sep="")
				else main.lab<-NULL
			}
			else main.lab<-paste(",main='",main,"'",sep="")
			if (is.null(x.label)) x.label<-name.variable
		eval(parse(text=paste0("dessin<-histogram(~var.value|var,xlab='",x.label,"'",main.lab,",data=Repeated.data.frame(",paste(listresponse,collapse="+",sep=""),
		",time.variable='var',data=",deparse(substitute(data)),"),type='",type,"',breaks='",breaks,"',scales='",scales,"', col='",col,"',
		strip=function(...,cex.strip) 
		strip.shapiro.1(...,cex.strip=",cex.strip,",shapiro=",Test.shapiro,"), 
		panel=function(x,...){",graphic.histo,"(x,...)})")))
		}
	}
   #Avec groupe
   if (is.na(listfactor[1])==FALSE) {
		#Simple
		if (length(listresponse)==1) {
			#Avec un seul groupe
			if (length(listfactor)==1){
				if (is.null(main)){
					if (Test.shapiro) main.lab  <-paste(",main='",text.main,"'",sep="")
					else main.lab<-NULL
				}
				else main.lab<-paste(",main=\"",main,"\"",sep="")
				if (is.null(x.label)) x.label<-paste("Summarized variable ",listresponse," by ",listfactor,sep="")
			eval(parse(text=paste0("dessin<-histogram(~",listresponse, "|",listfactor,",xlab='",x.label,"'",main.lab,",data=",deparse(substitute(data)),",type='",type,"',
			breaks='",breaks,"',scales='",scales,"', col='",col,"',
			strip=function(...,cex.strip) 
			strip.shapiro.1(...,cex.strip=",cex.strip,",shapiro=",Test.shapiro,"),
			panel=function(x,...){",graphic.histo,"(x,...)})")))			
			}
			#Avec un sous-groupe
			if (length(listfactor)==2){
				if (is.null(main)){
					if (Test.shapiro) {
					main.lab  <-paste(",main='",text.main,"'",sep="")
					n.col<-length(levels(factor(with(data,eval(parse(text=listfactor[1]))))))
					n.row<-length(levels(factor(with(data,eval(parse(text=listfactor[2]))))))
					lab.factor<-paste0(listresponse,"|interaction(",listfactor[1],",",listfactor[2],",sep=' and ')")
					lab.strip<-paste0(",par.strip.text = list(lines = 2),layout=c(",n.col,",",n.row,")
					,strip=function(...,cex.strip) 
					strip.shapiro.2(...,cex.strip=",cex.strip,")")
					}
					else {
					main.lab<-NULL
					lab.factor<-paste0(listresponse,"|",listfactor[1],"+",listfactor[2])
					lab.strip<-NULL
					}
				}
				else main.lab<-paste(",main=\"",main,"\"",sep="")
				if (is.null(x.label)) x.label<-paste("Summarized variable ",listresponse," by ",listfactor[1]," and ",listfactor[2],sep="")
			eval(parse(text=paste("dessin<-histogram(~",lab.factor,",xlab='",x.label,"'",main.lab,",data=",deparse(substitute(data)),",type='",type,"',
			breaks='",breaks,"',scales='",scales,"',col='",col,"'",lab.strip,
			",panel=function(x,...){",graphic.histo,"(x,...)})", sep="")))	
			}
		}
		#Repetee
		if (length(listresponse)>1){
		#Avec un seul groupe
			if (length(listfactor)==1){
			n.col<-length(levels(factor(with(data,eval(parse(text=listfactor[1]))))))
			n.row<-length(listresponse)
				if (is.null(main)){
					if (Test.shapiro) {
					main.lab  <-paste(",main='",text.main,"'",sep="")
					lab.factor<-paste0("var.value|interaction(",listfactor,",var,sep=' and ')")
					lab.strip<-paste0(",par.strip.text = list(lines = 2),layout=c(",n.col,",",n.row,")
					,strip=function(...,cex.strip) 
					strip.shapiro.2(...,cex.strip=",cex.strip,")")
					}
					else {
					main.lab<-NULL
					lab.factor<-paste0("var.value|",listfactor,"+var")
					lab.strip<-NULL
					}
				}
				else main.lab<-paste(",main=\"",main,"\"",sep="")
				if (is.null(x.label)) x.label<-paste("Summarized variable ",name.variable," by ",listfactor,sep="")
			eval(parse(text=paste0("dessin<-histogram(~",lab.factor,",xlab='",x.label,"'",main.lab,",data=Repeated.data.frame(",paste(listresponse,collapse="+",sep=""),
			",factors=",paste(listfactor,collapse="+",sep=""),",time.variable='var',data=",deparse(substitute(data)),"),type='",type,
			"',breaks='",breaks,"',scales='",scales,"', col='",col,"'",lab.strip,
			",panel=function(x,...){",graphic.histo,"(x,...)})")))
			}
			#Avec un sous-groupe
			if (length(listfactor)==2){
			lev1<-length(listresponse)
			lev2<-length(levels(factor(with(data,eval(parse(text=listfactor[1]))))))
			lev3<-length(levels(factor(with(data,eval(parse(text=listfactor[2]))))))
			lev<-c(lev1,lev2,lev3)
			n.col<-max(lev)
			n.row<-lev1*lev2*lev3/n.col
			eval(parse(text=paste0("rep.data<-Repeated.data.frame(",paste(listresponse,collapse="+",sep=""),",factors=",
			paste(listfactor,collapse="+",sep=""),",time.variable='var',data=",deparse(substitute(data)),")")))
				if (is.null(main)){
					if (Test.shapiro) {
					main.lab  <-paste(",main='",text.main,"'",sep="")
					lab.factor<-paste0("var.value|interaction(",listfactor[2],",",listfactor[1],",sep=' and ')+var")
					lab.strip<-paste0(",par.strip.text = list(lines = 1.8),
					strip=function(...,cex.strip)
					strip.shapiro.3(...,cex.strip=",cex.strip,"), ")
					}
					else {
					main.lab<-NULL
					lab.factor<-paste0("var.value|",listfactor[2],"+",listfactor[1],"+var")
					lab.strip<-paste0(",strip=strip.custom(par.strip.text=list(cex=",cex.strip,"))")
					}
				}
				else main.lab<-paste(",main=\"",main,"\"",sep="")
				if (is.null(x.label)) x.label<-paste("Summarized variable ",name.variable," by ",listfactor[1]," and ",listfactor[2],sep="")
			eval(parse(text=paste0("dessin<-histogram(~",lab.factor,",layout=c(",n.col,",",n.row,"),xlab='",x.label,"'",main.lab,",
			data=rep.data,type='",type,"',breaks='",breaks,"',scales='",scales,"',col='",col,"'",lab.strip,
			",panel=function(x,...){",graphic.histo,"(x,...)})")))				
			}
		}
   }
}

if (normal){
   #Sans groupe
  if (is.na(listfactor[1])==TRUE) {
	#Simple
    if (length(listresponse)==1) {
    data.normal<-with(data,eval(parse(text=listresponse)))
	length.data<-length(data.normal[!is.na(data.normal)])
    			if (length.data<51){
			test<-shapiro.test(data.normal[!is.na(data.normal)]) 
			text.main<-"Histogram with Shapiro-Wilk normality test"
			}
			else{
			test<-nortest::lillie.test(data.normal[!is.na(data.normal)])
			text.main<-"Histogram with Kolmogorov-Smirnov normality test"
			}
      if(test$p.value<0.001) test<-"< 0.001" else test<-paste("=",round(test$p.value,3))
			if (is.null(main)){
				if (Test.shapiro) main.lab  <-paste(",main='",text.main," (p",test,")'",sep="")
				else main.lab<-",main='Histogram with normality curve'"
			}
			else main.lab<-paste(",main=\"",main,"\"",sep="")
			if (is.null(x.label)) x.label<-listresponse	  
    eval(parse(text=paste("dessin<-histogram(~",listresponse,",data=",deparse(substitute(data)),
    ",xlab='",x.label,"',type='",type,"',breaks='",breaks,"',col='",col,"'",main.lab,",
    panel=function(x,...){
    ",graphic.histo,"(x,...) 
    panel.norm(x,...) 
    })", sep="")))
    }
	#Repetee
    if (length(listresponse)>1) {
		if (is.null(main)){
			if (Test.shapiro) main.lab  <-paste(",main='",text.main,"'",sep="")
			else main.lab<-",main='Histogram with normality curve'"
		}
		else main.lab<-paste(",main=\"",main,"\"",sep="")
    eval(parse(text=paste("dessin<-histogram(~",name.variable,".value|",name.variable,",xlab='",x.label,"'",main.lab,",data=Repeated.data.frame(variables=",paste(listresponse,collapse="+",sep=""),
    ",time.variable='",name.variable,"',data=",deparse(substitute(data)),"),type='",type,"',breaks='",breaks,"',scales='",scales,"', col='",col,"', 
    strip=function(...,cex.strip) 
    strip.shapiro.1(...,cex.strip=",cex.strip,",shapiro=",Test.shapiro,"), 
    panel=function(x,...){
	",  graphic.histo,"(x,...)  
    panel.norm(x,...)}
	)", sep="")))
    }
  }
   #Avec groupe
  if (is.na(listfactor[1])==FALSE) {
	#Simple
    if (length(listresponse)==1) {
		#Avec un seul groupe
		if (length(listfactor)==1){
		  	if (is.null(main)){
				if (Test.shapiro) main.lab  <-paste(",main='",text.main,"'",sep="")
				else main.lab<-",main='Histogram with normality curve'"
			}
			else main.lab<-paste(",main=\"",main,"\"",sep="")
			if (is.null(x.label)) x.label<-paste("Summarized variable ",listresponse," by ",listfactor,sep="")
		eval(parse(text=paste("dessin<-histogram(~",listresponse, "|",listfactor,",xlab='",x.label,"'",main.lab,",data=",deparse(substitute(data)),",type='",type,"',
		breaks='",breaks,"',scales='",scales,"', col='",col,"',	
		strip=function(...,cex.strip) 
		strip.shapiro.1(...,cex.strip=",cex.strip,",shapiro=",Test.shapiro,"),
		panel=function(x,...){
		",graphic.histo,"(x,...)  
		panel.norm(x,...)}
		)", sep="")))
		}
		#Avec un sous-groupe
		if (length(listfactor)==2){
			if (is.null(main)){
				if (Test.shapiro) {
				main.lab  <-paste(",main='",text.main,"'",sep="")
				n.col<-length(levels(factor(with(data,eval(parse(text=listfactor[1]))))))
				n.row<-length(levels(factor(with(data,eval(parse(text=listfactor[2]))))))
				lab.factor<-paste0(listresponse,"|interaction(",listfactor[1],",",listfactor[2],",sep=' and ')")
				lab.strip<-paste0(",par.strip.text = list(lines = 2),layout=c(",n.col,",",n.row,")
				,strip=function(...,cex.strip) 
				strip.shapiro.2(...,cex.strip=",cex.strip,")")
				}
				else {
				main.lab<-NULL
				lab.factor<-paste0(listresponse,"|",listfactor[1],"+",listfactor[2])
				lab.strip<-NULL
				}
			}
			else main.lab<-paste(",main=\"",main,"\"",sep="")
			if (is.null(x.label)) x.label<-paste("Summarized variable ",listresponse," by ",listfactor[1]," and ",listfactor[2],sep="")
		eval(parse(text=paste("dessin<-histogram(~",lab.factor,",xlab='",x.label,"'",main.lab,",data=",deparse(substitute(data)),",type='",type,"',
		breaks='",breaks,"',scales='",scales,"',col='",col,"'",lab.strip,
		",panel=function(x,...){",graphic.histo,"(x,...)
		panel.norm(x,...)}
		)", sep="")))					
		}
	}
	  #Repetee
	  if (length(listresponse)>1) {
		#avec un seul groupe
			if (length(listfactor)==1){
			n.col<-length(levels(factor(with(data,eval(parse(text=listfactor[1]))))))
			n.row<-length(listresponse)
				if (is.null(main)){
					if (Test.shapiro) {
					main.lab  <-paste(",main='",text.main,"'",sep="")
					lab.factor<-paste0(name.variable,".value|interaction(",listfactor,",",name.variable,",sep=' and ')")
					lab.strip<-paste0(",par.strip.text = list(lines = 2),layout=c(",n.col,",",n.row,")
					,strip=function(...,cex.strip) 
					strip.shapiro.2(...,cex.strip=",cex.strip,")")
					}
					else {
					main.lab<-NULL
					lab.factor<-paste0(name.variable,".value|",listfactor,"+",name.variable)
					lab.strip<-NULL
					}
				}
				else main.lab<-paste(",main=\"",main,"\"",sep="")
				if (is.null(x.label)) x.label<-paste("Summarized variable ",name.variable," by ",listfactor,sep="")
			eval(parse(text=paste0("dessin<-histogram(~",lab.factor,",xlab='",x.label,"'",main.lab,",data=Repeated.data.frame(",paste(listresponse,collapse="+",sep=""),
			",factors=",paste(listfactor,collapse="+",sep=""),",time.variable='",name.variable,"',data=",deparse(substitute(data)),"),type='",type,
			"',breaks='",breaks,"',scales='",scales,"', col='",col,"'",lab.strip,
			",panel=function(x,...){",graphic.histo,"(x,...)
			panel.norm(x,...)
			})")))
			}
			#Avec un sous-groupe
			if (length(listfactor)==2){
			lev1<-length(listresponse)
			lev2<-length(levels(factor(with(data,eval(parse(text=listfactor[1]))))))
			lev3<-length(levels(factor(with(data,eval(parse(text=listfactor[2]))))))
			lev<-c(lev1,lev2,lev3)
			n.col<-max(lev)
			n.row<-lev1*lev2*lev3/n.col
			eval(parse(text=paste0("rep.data<-Repeated.data.frame(",paste(listresponse,collapse="+",sep=""),",factors=",
			paste(listfactor,collapse="+",sep=""),",time.variable='var',data=",deparse(substitute(data)),")")))
				if (is.null(main)){
					if (Test.shapiro) {
					main.lab  <-paste(",main='",text.main,"'",sep="")
					lab.factor<-paste0("var.value|interaction(",listfactor[2],",",listfactor[1],",sep=' and ')+var")
					lab.strip<-paste0(",par.strip.text = list(lines = 1.8),
					strip=function(...,cex.strip)
					strip.shapiro.3(...,cex.strip=",cex.strip,"), ")
					}
					else {
					main.lab<-NULL
					lab.factor<-paste0("var.value|",listfactor[2],"+",listfactor[1],"+var")
					lab.strip<-paste0(",strip=strip.custom(par.strip.text=list(cex=",cex.strip,"))")
					}
				}
				else main.lab<-paste(",main=\"",main,"\"",sep="")
				if (is.null(x.label)) x.label<-paste("Summarized variable ",name.variable," by ",listfactor[1]," and ",listfactor[2],sep="")
			eval(parse(text=paste0("dessin<-histogram(~",lab.factor,",layout=c(",n.col,",",n.row,"),xlab='",x.label,"'",main.lab,",
			data=rep.data,type='",type,"',breaks='",breaks,"',scales='",scales,"',col='",col,"'",lab.strip,
			",panel=function(x,...){",graphic.histo,"(x,...)
			panel.norm(x,...)
			})")))				
			}			
    }
  }
 }
dessin  
}

#######################################################################################
#Histogrammes multiples (meme dessin)
multiple.histogram<-function (formula,data=parent.frame(),Test.shapiro=TRUE,strata=c("repeated","factor"),x.label=NULL,y.label=NULL,main="Multiple histogram",
name.repeated=NULL,dim=c("2D","3D"),legend.lab=NULL,type=c("count","percent","density"),normal=TRUE,leg="topright" ,breaks="sturges",col=palette(),alpha=.5)
{
require(stringr)
#extraction des parametres
eval(parse(text=paste0("ex<-Extract.fact(",deparse(substitute(formula)),",data=",deparse(substitute(data)),")")))
col<-col2rgb(col)
color<-rgb(col[1,],col[2,],col[3,],alpha=255*alpha,max=255)   
type<-match.arg(type)
strata<-match.arg(strata)
dim<-match.arg(dim)
if (dim=="2D") dimension<-NULL else dimension<-dim
#Sans groupe
	if (ex$length.factor==0) {
		#repetee
		if (ex$length.response>1){
		eval(parse(text=paste0("resp.m",1:ex$length.response,"<-mean(",deparse(substitute(data)),"$",ex$list.response,",na.rm=TRUE)")))
		eval(parse(text=paste0("resp.s",1:ex$length.response,"<-sd(",deparse(substitute(data)),"$",ex$list.response,",na.rm=TRUE)")))
		eval(parse(text=paste0("test",1:ex$length.response,"<-shapiro.test(",deparse(substitute(data)),"$",ex$list.response,"[!is.na(",deparse(substitute(data)),"$",ex$list.response,")])")))
		eval(parse(text=paste0("resp.",1:ex$length.response,"<-na.omit(",deparse(substitute(data)),"$",ex$list.response,")")))
		eval(parse(text=paste0("resp<-c(",paste0("resp.",1:ex$length.response,collapse=","),")")))
		graph<-hist(resp,plot=F,breaks=breaks)
		min.width<-min(graph$breaks)
		max.width<-max(graph$breaks)
		length.width<-max(mean(diff(graph$breaks)))
		widths<-seq(min.width,max.width,by=length.width)
		eval(parse(text=paste("graph",1:ex$length.response,"<-hist(resp.",1:ex$length.response,",breaks=graph$breaks,plot=F)",sep="")))
			if (type=="density")
			eval(parse(text=paste("graph",1:ex$length.response,"$counts<-graph",1:ex$length.response,"$density",sep="")))
			if (type=="percent")
			eval(parse(text=paste("graph",1:ex$length.response,"$counts<-graph",1:ex$length.response,"$counts/sum(graph",1:ex$length.response,"$counts)*100",sep="")))
		eval(parse(text=paste("supy<-max(",paste("graph",1:ex$length.response,"$counts",collapse=",",sep=""),")",sep="")))
		scalex<-length(widths)/(max.width-min.width)
		seq.x<-seq(0,length(widths),by=length(length.width))
		seq.axis<-pretty(seq.x/scalex+min.width)
		seq.x<-scalex*(seq.axis-min.width)
		normx<-seq(0,length(widths),length=500)
		eval(parse(text=paste("m.histo",1:ex$length.response,"<-scalex*(resp.m",1:ex$length.response,"-min.width)",sep="")))
		eval(parse(text=paste("sd.histo",1:ex$length.response,"<-scalex*(resp.s",1:ex$length.response,")",sep="")))
		eval(parse(text=paste("densityx",1:ex$length.response,"<-dnorm(normx,m.histo",1:ex$length.response,",sd.histo",1:ex$length.response,")",sep="")))
		eval(parse(text=paste("densityx",1:ex$length.response,"<-densityx",1:ex$length.response,"/max(densityx",1:ex$length.response,
		")*max(graph",1:ex$length.response,"$counts)",sep="")))
		# definition des legendes
		eval(parse(text=paste0("test<-c(",paste0("test",1:ex$length.response,"$p.value",collapse=","),")")))
			for (i in 1:ex$length.response){
				if(test[i]<0.001) test[i]<-"< 0.001" 
				else test[i]<-paste("=",round(as.numeric(test[i]),3))
			}
			text.legend<-if (Test.shapiro) paste(ex$list.response," (p",test,")",sep="") else ex$list.response
			if(is.null(name.repeated)) name.repeated<-"Variable"
			if (main=="Multiple histogram") main<-paste(main,"for",name.repeated)
			if(is.null(x.label)) x.label<-name.repeated
			if(is.null(y.label)) y.label<-str_to_title(type)
		#sortie du dessin
		if (is.null(legend.lab)) legend.lab<-name.repeated
		eval(parse(text=paste0("barplot",dimension,"(graph1$counts,space=0,ylim=c(0,supy*1.1),xlab=\"",x.label,"\",main=\"",main,"\",ylab=\"",y.label,"\",col=color[1])")))
			for (i in 2:ex$length.response)
			eval(parse(text=paste("barplot",dimension,"(graph",i,"$counts,space=0,ylim=c(0,supy*1.1),col=color[",i,"],add=TRUE)",sep="")))
			if (normal){
			eval(parse(text=paste("lines(normx,densityx",1:ex$length.response,",lwd=2,col=",1:ex$length.response,")",sep="")))
			legend(leg,text.legend,lty=1,title=legend.lab,col=palette()[1:ex$length.response],fill=color[1:ex$length.response])
			}
			else legend(leg,text.legend,title=legend.lab,fill=color[1:ex$length.response]) 
		axis(1,seq.x,seq.axis)
		}
	}
#Avec groupe
	if (ex$length.factor!=0) {
	#Avec un seul groupe
		if (ex$length.factor==1) {
		#Simple
			if (ex$length.response==1){
			eval(parse(text=paste0("resp<-na.omit(",deparse(substitute(data)),"$",ex$list.response,")")))
			eval(parse(text=paste0("niveaux<-levels(",deparse(substitute(data)),"$",ex$list.factor,")")))
			eval(parse(text=paste0("resp.",1:length(niveaux),"<-subset(",deparse(substitute(data)),",",ex$list.factor,"=='",niveaux,"')$",ex$list.response)))
			eval(parse(text=paste0("resp.m",1:length(niveaux),"<-mean(resp.",1:length(niveaux),",na.rm=TRUE)")))
			eval(parse(text=paste0("resp.s",1:length(niveaux),"<-sd(resp.",1:length(niveaux),",na.rm=TRUE)")))
			eval(parse(text=paste0("test",1:length(niveaux),"<-shapiro.test(resp.",1:length(niveaux),"[!is.na(resp.",1:length(niveaux),")])")))
			graph<-hist(resp,plot=F,breaks=breaks)
			min.width<-min(graph$breaks)
			max.width<-max(graph$breaks)
			length.width<-max(mean(diff(graph$breaks)))
			widths<-seq(min.width,max.width,by=length.width)			
			eval(parse(text=paste("graph",1:length(niveaux),"<-hist(resp.",1:length(niveaux),",breaks=graph$breaks,plot=F)",sep="")))
				eval(parse(text=paste("graph",1:length(niveaux),"<-hist(resp.",1:length(niveaux),",breaks=widths,plot=F)",sep="")))
				if (type=="density")
				eval(parse(text=paste("graph",1:length(niveaux),"$counts<-graph",1:length(niveaux),"$density",sep="")))
				if (type=="percent")
				eval(parse(text=paste("graph",1:length(niveaux),"$counts<-graph",1:length(niveaux),"$counts/sum(graph",1:length(niveaux),"$counts)*100",sep="")))
			eval(parse(text=paste("supy<-max(",paste("graph",1:length(niveaux),"$counts",collapse=",",sep=""),")",sep="")))
			scalex<-length(widths)/(max.width-min.width)
			seq.x<-seq(0,length(widths),by=length(length.width))
			seq.axis<-pretty(seq.x/scalex+min.width)
			seq.x<-scalex*(seq.axis-min.width)
			normx<-seq(0,length(widths),length=500)
			eval(parse(text=paste("m.histo",1:length(niveaux),"<-scalex*(resp.m",1:length(niveaux),"-min.width)",sep="")))
			eval(parse(text=paste("sd.histo",1:length(niveaux),"<-scalex*(resp.s",1:length(niveaux),")",sep="")))
			eval(parse(text=paste("densityx",1:length(niveaux),"<-dnorm(normx,m.histo",1:length(niveaux),",sd.histo",1:length(niveaux),")",sep="")))
			eval(parse(text=paste("densityx",1:length(niveaux),"<-densityx",1:length(niveaux),"/max(densityx",1:length(niveaux),
			")*max(graph",1:length(niveaux),"$counts)",sep="")))
			# definition des legendes
			eval(parse(text=paste0("test<-c(",paste0("test",1:length(niveaux),"$p.value",collapse=","),")")))
			for (i in 1:length(niveaux)){
				if(test[i]<0.001) test[i]<-"< 0.001" 
				else test[i]<-paste("=",round(as.numeric(test[i]),3))
			}
			text.legend<-if (Test.shapiro) paste(niveaux," (p",test,")",sep="") else niveaux
			if(is.null(name.repeated)) name.repeated<-ex$list.factor
			if (main=="Multiple histogram") main<-paste(main,"for",ex$list.factor)
			if(is.null(x.label)) x.label<-ex$list.response
			if(is.null(y.label)) y.label<-str_to_title(type)
			#sortie du dessin
			if (is.null(legend.lab)) legend.lab<-name.repeated
			eval(parse(text=paste0("barplot",dimension,"(graph1$counts,space=0,ylim=c(0,supy*1.1),xlab=\"",x.label,"\",main=\"",main,"\",ylab=\"",y.label,"\",col=color[1])")))
				for (i in 2:length(niveaux))
				eval(parse(text=paste("barplot",dimension,"(graph",i,"$counts,space=0,ylim=c(0,supy*1.1),col=color[",i,"],add=TRUE)",sep="")))
				if (normal){
				eval(parse(text=paste("lines(normx,densityx",1:length(niveaux),",lwd=2,col=",1:length(niveaux),")",sep="")))
				legend(leg,text.legend,lty=1,title=legend.lab,col=palette()[1:length(niveaux)],fill=color[1:length(niveaux)])
				}
				else legend(leg,text.legend,title=legend.lab,fill=color[1:length(niveaux)]) 
			axis(1,seq.x,seq.axis)
			}
			#Repetee
			if (ex$length.response>1){
			eval(parse(text=paste0("niveaux<-levels(",deparse(substitute(data)),"$",ex$list.factor,")")))
				if (strata=="repeated"){
				niveaux1<-niveaux
				niveaux2<-ex$list.response
				eval(parse(text=paste0("resp.",1:ex$length.response,"<-",deparse(substitute(data)),"$",ex$list.response[1:ex$length.response])))
				eval(parse(text=paste0("resp.",1:ex$length.response,"<-as.data.frame(cbind(",ex$list.factor,"=as.character(",deparse(substitute(data)),"$",ex$list.factor,"),as.numeric(resp.",1:ex$length.response,")))")))
					for (i in 1:length(niveaux2)){
					eval(parse(text=paste0("resp.",i,".",1:length(niveaux1),"<-na.omit(as.numeric(subset(resp.",i,",",ex$list.factor,"=='",niveaux1[1:length(niveaux1)],"')$V2))")))
					eval(parse(text=paste0("resp.m",i,".",1:length(niveaux1),"<-mean(resp.",i,".",1:length(niveaux1),",na.rm=TRUE)")))
					eval(parse(text=paste0("resp.s",i,".",1:length(niveaux1),"<-sd(resp.",i,".",1:length(niveaux1),",na.rm=TRUE)")))
					eval(parse(text=paste0("test",i,".",1:length(niveaux1),"<-shapiro.test(resp.",i,".",1:length(niveaux1),"[!is.na(resp.",i,".",1:length(niveaux1),")])")))
					eval(parse(text=paste0("graph",i,"<-hist(na.omit(as.numeric(resp.",i,"$V2)),breaks=breaks,plot=F)")))
					eval(parse(text=paste0("min.width",i,"<-min(graph",i,"$breaks)")))
					eval(parse(text=paste0("max.width",i,"<-max(graph",i,"$breaks)")))
					eval(parse(text=paste0("length.width",i,"<-max(mean(diff(graph",i,"$breaks)))")))
					eval(parse(text=paste0("widths",i,"<-seq(min.width",i,",max.width",i,",by=length.width",i,")")))
					}
				}
				else {
				niveaux2<-niveaux
				niveaux1<-ex$list.response
				eval(parse(text=paste0("resp.",1:length(niveaux),"<-subset(",deparse(substitute(data)),",",ex$list.factor,"=='",niveaux,"')")))
					for (i in 1:length(niveaux1)){
					eval(parse(text=paste0("resp.",1:length(niveaux2),".",i,"<-na.omit(resp.",1:length(niveaux2),"$",ex$list.response[i],")")))
					eval(parse(text=paste0("resp.m",1:length(niveaux2),".",i,"<-mean(resp.",1:length(niveaux2),".",i,")")))
					eval(parse(text=paste0("resp.s",1:length(niveaux2),".",i,"<-sd(resp.",1:length(niveaux2),".",i,")")))
					eval(parse(text=paste0("test",1:length(niveaux2),".",i,"<-shapiro.test(resp.",1:length(niveaux2),".",i,")")))
					}
					for (i in 1:length(niveaux2)){
					eval(parse(text=paste0("resp.",i,"<-c(",paste0("resp.",i,".",1:length(niveaux1),collapse=","),")")))
					eval(parse(text=paste0("graph",i,"<-hist(na.omit(resp.",i,"),breaks=breaks,plot=F)")))
					eval(parse(text=paste0("min.width",i,"<-min(graph",i,"$breaks)")))
					eval(parse(text=paste0("max.width",i,"<-max(graph",i,"$breaks)")))
					eval(parse(text=paste0("length.width",i,"<-max(mean(diff(graph",i,"$breaks)))")))
					eval(parse(text=paste0("widths",i,"<-seq(min.width",i,",max.width",i,",by=length.width",i,")")))
					}
				}
			
			#premier indice niveaux2(strate), deuxieme niveaux1
				for (i in 1:length(niveaux2)){
				eval(parse(text=paste("graph",i,".",1:length(niveaux1),"<-hist(resp.",i,".",1:length(niveaux1),",breaks=graph",i,"$breaks,plot=F)",sep="")))
					if (type=="density")
					eval(parse(text=paste("graph",i,".",1:length(niveaux1),"$counts<-graph",i,".",1:length(niveaux1),"$density",sep="")))
					if (type=="percent")
					eval(parse(text=paste("graph",i,".",1:length(niveaux1),"$counts<-graph",i,".",1:length(niveaux1),"$counts/sum(graph",i,".",1:length(niveaux1),"$counts)*100",sep="")))
				eval(parse(text=paste("supy_",i,"<-max(",paste("graph",i,".",1:length(niveaux1),"$counts",collapse=",",sep=""),")",sep="")))
				eval(parse(text=paste0("scalex_",i,"<-length(widths",i,")/(max.width",i,"-min.width",i,")")))
				eval(parse(text=paste0("seq.x_",i,"<-seq(0,length(widths",i,"),by=length(length.width",i,"))")))
				eval(parse(text=paste0("seq.axis_",i,"<-pretty(seq.x_",i,"/scalex_",i,"+min.width",i,")")))
				eval(parse(text=paste0("seq.x_",i,"<-scalex_",i,"*(seq.axis_",i,"-min.width",i,")")))
				eval(parse(text=paste0("normx_",i,"<-seq(0,length(widths",i,"),length=500)")))
				eval(parse(text=paste("m.histo",i,".",1:length(niveaux1),"<-scalex_",i,"*(resp.m",i,".",1:length(niveaux1),"-min.width",i,")",sep="")))
				eval(parse(text=paste("sd.histo",i,".",1:length(niveaux1),"<-scalex_",i,"*(resp.s",i,".",1:length(niveaux1),")",sep="")))
				eval(parse(text=paste0("test<-c(",paste0("test",i,".",1:length(niveaux1),"$p.value",collapse=","),")")))
					for (j in 1:length(niveaux1)){
						if(test[j]<0.001) test[j]<-"< 0.001" 
						else test[j]<-paste("=",round(as.numeric(test[j]),3))
					eval(parse(text=paste0("test",j,"<-test")))						
					}
				eval(parse(text=paste0("test",i,".",1:length(niveaux1),"<-test[",1:length(niveaux1),"]")))
				if (Test.shapiro) eval(parse(text=paste0("text.legend",i,".",1:length(niveaux1),"<-paste0('",niveaux1,"',' (p ',test",i,".",1:length(niveaux1),",')')")))
				else eval(parse(text=paste0("text.legend",i,".",1:length(niveaux1),"<-paste0('",niveaux1,"')")))
				eval(parse(text=paste("densityx",i,".",1:length(niveaux1),"<-dnorm(normx_",i,",m.histo",i,".",1:length(niveaux1),",sd.histo",i,".",1:length(niveaux1),")",sep="")))
				eval(parse(text=paste("densityx",i,".",1:length(niveaux1),"<-densityx",i,".",1:length(niveaux1),"/max(densityx",i,".",1:length(niveaux1),
				")*max(graph",i,".",1:length(niveaux1),"$counts)",sep="")))
				}
			n.row<-length(niveaux2)
			# definition des legendes
			if(is.null(name.repeated)){
				if(strata=="repeated"){
				name.repeated<-ex$list.factor
				name.title<-"Variable"
				} 
				if(strata=="factor"){
				name.repeated<-"Variable"
				name.title<-ex$list.factor
				}
			if(is.null(x.label)) x.label<-paste("For",name.title,"=")	
			}
			if(is.null(y.label)) y.label<-str_to_title(type)
			
			#sortie du dessin
			layout(matrix(c(1,seq(2,n.row+1)),n.row+1,1,byrow=TRUE),widths=28*n.row+4,heights=c(4,rep(28,n.row)),TRUE)
			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
				if (main=="Multiple histogram")
				text(.5,.5,paste(main,"for",name.repeated),cex=1.5)
				else
				text(.5,.5,main,cex=1.5)
			par(mar=c(4,4,0,2))
				for (i in 1:length(niveaux2)){
				eval(parse(text=paste0("barplot",dimension,"(graph",i,".",1,"$counts,space=0,ylim=c(0,supy_",i,"*1.1),xlab=\"",x.label,niveaux2[i],"\",main='',ylab=\"",y.label,"\",col=color[1])")))
				eval(parse(text=paste0("text.legend<-c(",paste0("text.legend",i,".",1:length(niveaux1),collapse=","),")")))
					for (j in 2:length(niveaux1)){
					if (is.null(legend.lab)){
					if(strata=="repeated") legend.lab<-ex$list.factor
					if(strata=="factor") legend.lab<-name.repeated
					} 
					
					eval(parse(text=paste("barplot",dimension,"(graph",i,".",j,"$counts,space=0,ylim=c(0,supy_",i,"*1.1),col=color[",j,"],add=TRUE)",sep="")))
						if (normal){
						eval(parse(text=paste("lines(normx_",i,",densityx",i,".",1:length(niveaux1),",lwd=2,col=",1:j,")",sep="")))
						legend(leg,text.legend,lty=1,title=legend.lab,col=palette()[1:j],fill=color[1:length(niveaux1)])
						}
						else legend(leg,text.legend,title=legend.lab,fill=color[1:j]) 
					}
				eval(parse(text=paste0("axis(1,seq.x_",i,",seq.axis_",i,")")))
				}
			}	
		}
		#Groupe et sous-groupe
		if (ex$length.factor==2) {
		#Simple
			if (ex$length.response==1){
			eval(parse(text=paste0("niveaux1<-levels(",deparse(substitute(data)),"$",ex$list.factor[1],")")))
			eval(parse(text=paste0("niveaux2<-levels(",deparse(substitute(data)),"$",ex$list.factor[2],")")))
			#premier indice facteur2, deuxieme facteur1
			eval(parse(text=paste0("resp.",1:length(niveaux2),"<-subset(",deparse(substitute(data)),",",ex$list.factor[2],"=='",niveaux2[1:length(niveaux2)],"')")))
				for (i in 1:length(niveaux2))
				eval(parse(text=paste0("resp.",i,".",1:length(niveaux1),"<-subset(resp.",i,",",ex$list.factor[1],"=='",niveaux1[1:length(niveaux1)],"')$",ex$list.response)))
				#premier indice facteur2(strate), deuxieme facteur1
				for (i in 1:length(niveaux1)){
				eval(parse(text=paste0("resp.",1:length(niveaux2),".",i,"<-na.omit(resp.",1:length(niveaux2),".",i,")")))
				eval(parse(text=paste0("resp.m",1:length(niveaux2),".",i,"<-mean(resp.",1:length(niveaux2),".",i,")")))
				eval(parse(text=paste0("resp.s",1:length(niveaux2),".",i,"<-sd(resp.",1:length(niveaux2),".",i,")")))
				eval(parse(text=paste0("test",1:length(niveaux2),".",i,"<-shapiro.test(resp.",1:length(niveaux2),".",i,")")))
				}
				for (i in 1:length(niveaux2)){
				eval(parse(text=paste0("resp.",i,"<-c(",paste0("resp.",i,".",1:length(niveaux1),collapse=","),")")))
				eval(parse(text=paste0("graph",i,"<-hist(na.omit(resp.",i,"),breaks=breaks,plot=F)")))
				eval(parse(text=paste0("min.width",i,"<-min(graph",i,"$breaks)")))
				eval(parse(text=paste0("max.width",i,"<-max(graph",i,"$breaks)")))
				eval(parse(text=paste0("length.width",i,"<-max(mean(diff(graph",i,"$breaks)))")))
				eval(parse(text=paste0("widths",i,"<-seq(min.width",i,",max.width",i,",by=length.width",i,")")))				
				eval(parse(text=paste("graph",i,".",1:length(niveaux1),"<-hist(resp.",i,".",1:length(niveaux1),",breaks=graph",i,"$breaks,plot=F)",sep="")))
					if (type=="density")
					eval(parse(text=paste("graph",i,".",1:length(niveaux1),"$counts<-graph",i,".",1:length(niveaux1),"$density",sep="")))
					if (type=="percent")
					eval(parse(text=paste("graph",i,".",1:length(niveaux1),"$counts<-graph",i,".",1:length(niveaux1),"$counts/sum(graph",i,".",1:length(niveaux1),"$counts)*100",sep="")))
				eval(parse(text=paste("supy_",i,"<-max(",paste("graph",i,".",1:length(niveaux1),"$counts",collapse=",",sep=""),")",sep="")))
				eval(parse(text=paste0("scalex_",i,"<-length(widths",i,")/(max.width",i,"-min.width",i,")")))
				eval(parse(text=paste0("seq.x_",i,"<-seq(0,length(widths",i,"),by=length(length.width",i,"))")))
				eval(parse(text=paste0("seq.axis_",i,"<-pretty(seq.x_",i,"/scalex_",i,"+min.width",i,")")))
				eval(parse(text=paste0("seq.x_",i,"<-scalex_",i,"*(seq.axis_",i,"-min.width",i,")")))
				eval(parse(text=paste0("normx_",i,"<-seq(0,length(widths",i,"),length=500)")))
				eval(parse(text=paste("m.histo",i,".",1:length(niveaux1),"<-scalex_",i,"*(resp.m",i,".",1:length(niveaux1),"-min.width",i,")",sep="")))
				eval(parse(text=paste("sd.histo",i,".",1:length(niveaux1),"<-scalex_",i,"*(resp.s",i,".",1:length(niveaux1),")",sep="")))
				eval(parse(text=paste0("test<-c(",paste0("test",i,".",1:length(niveaux1),"$p.value",collapse=","),")")))
					for (j in 1:length(niveaux1)){
						if(test[j]<0.001) test[j]<-"< 0.001" 
						else test[j]<-paste("=",round(as.numeric(test[j]),3))
					eval(parse(text=paste0("test",j,"<-test")))						
					}
				eval(parse(text=paste0("test",i,".",1:length(niveaux1),"<-test[",1:length(niveaux1),"]")))
				if (Test.shapiro) eval(parse(text=paste0("text.legend",i,".",1:length(niveaux1),"<-paste0('",niveaux1,"',' (p ',test",i,".",1:length(niveaux1),",')')")))
				else eval(parse(text=paste0("text.legend",i,".",1:length(niveaux1),"<-paste0('",niveaux1,"')")))				
				eval(parse(text=paste("densityx",i,".",1:length(niveaux1),"<-dnorm(normx_",i,",m.histo",i,".",1:length(niveaux1),",sd.histo",i,".",1:length(niveaux1),")",sep="")))
				eval(parse(text=paste("densityx",i,".",1:length(niveaux1),"<-densityx",i,".",1:length(niveaux1),"/max(densityx",i,".",1:length(niveaux1),
				")*max(graph",i,".",1:length(niveaux1),"$counts)",sep="")))
				}
			n.row<-length(niveaux2)
			# definition des legendes
			if(is.null(name.repeated)) name.repeated<-ex$list.factor[1]
			if(is.null(x.label)) x.label<-paste(ex$list.response,"for",ex$list.factor[2],"=")
			if(is.null(y.label)) y.label<-str_to_title(type)
		#sortie du dessin
			layout(matrix(c(1,seq(2,n.row+1)),n.row+1,1,byrow=TRUE),widths=28*n.row+4,heights=c(4,rep(28,n.row)),TRUE)
			par(mar=c(0,0,0,0))
			plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
			if (main=="Multiple histogram")
				text(.5,.5,paste(main,"for",name.repeated),cex=1.5)
				else
				text(.5,.5,main,cex=1.5)
			par(mar=c(4,4,0,2))
				for (i in 1:length(niveaux2)){
				eval(parse(text=paste0("barplot",dimension,"(graph",i,".",1,"$counts,space=0,ylim=c(0,supy_",i,"*1.1),xlab=\"",x.label,niveaux2[i],"\",main='',ylab=\"",y.label,"\",col=color[1])")))
				eval(parse(text=paste0("text.legend<-c(",paste0("text.legend",i,".",1:length(niveaux1),collapse=","),")")))
					for (j in 2:length(niveaux1)){
					if (is.null(legend.lab)) legend.lab<-name.repeated
					eval(parse(text=paste("barplot",dimension,"(graph",i,".",j,"$counts,space=0,ylim=c(0,supy_",i,"*1.1),col=color[",j,"],add=TRUE)",sep="")))
						if (normal){
						eval(parse(text=paste("lines(normx_",i,",densityx",i,".",1:length(niveaux1),",lwd=2,col=",1:j,")",sep="")))
						legend(leg,text.legend,lty=1,title=legend.lab,col=palette()[1:j],fill=color[1:length(niveaux1)])
						}
						else legend(leg,text.legend,title=legend.lab,fill=color[1:j]) 
					}
				eval(parse(text=paste0("axis(1,seq.x_",i,",seq.axis_",i,")")))
				}			
			}
		#Repetees
			if (ex$length.response>1){
			eval(parse(text=paste0("niveaux1<-levels(",deparse(substitute(data)),"$",ex$list.factor[1],")")))
			eval(parse(text=paste0("niveaux2<-levels(",deparse(substitute(data)),"$",ex$list.factor[2],")")))			
			#premier indice facteur2, deuxieme facteur1, troisieme reponse (page) strate=facteur2
			eval(parse(text=paste0("resp.",1:length(niveaux2),"<-subset(",deparse(substitute(data)),",",ex$list.factor[2],"=='",niveaux2[1:length(niveaux2)],"')")))
				for (i in 1:length(niveaux2))
				eval(parse(text=paste0("resp.",i,".",1:length(niveaux1),"<-subset(resp.",i,",",ex$list.factor[1],"=='",niveaux1[1:length(niveaux1)],"')")))
				for (i in 1:length(niveaux2))
					for (j in 1:length(niveaux1))
					eval(parse(text=paste0("resp.",i,".",j,".",1:ex$length.response,"<-resp.",i,".",j,"$",ex$list.response[1:ex$length.response])))
				#calcul pour chaque reponse
				for (k in 1:ex$length.response){
					for (i in 1:length(niveaux1)){
					eval(parse(text=paste0("resp.",1:length(niveaux2),".",i,".",k,"<-na.omit(resp.",1:length(niveaux2),".",i,".",k,")")))
					eval(parse(text=paste0("resp.m",1:length(niveaux2),".",i,".",k,"<-mean(resp.",1:length(niveaux2),".",i,".",k,")")))
					eval(parse(text=paste0("resp.s",1:length(niveaux2),".",i,".",k,"<-sd(resp.",1:length(niveaux2),".",i,".",k,")")))
					eval(parse(text=paste0("test",1:length(niveaux2),".",i,".",k,"<-shapiro.test(resp.",1:length(niveaux2),".",i,".",k,")")))
					}
					for (i in 1:length(niveaux2)){
					eval(parse(text=paste0("resp.",i,".",k,"<-c(",paste0("resp.",i,".",1:length(niveaux1),".",k,collapse=","),")")))
					eval(parse(text=paste0("graph",i,".",k,"<-hist(na.omit(resp.",i,".",k,"),breaks=breaks,plot=F)")))
					eval(parse(text=paste0("min.width",i,".",k,"<-min(graph",i,".",k,"$breaks)")))
					eval(parse(text=paste0("max.width",i,".",k,"<-max(graph",i,".",k,"$breaks)")))
					eval(parse(text=paste0("length.width",i,".",k,"<-max(mean(diff(graph",i,".",k,"$breaks)))")))
					eval(parse(text=paste0("widths",i,".",k,"<-seq(min.width",i,".",k,",max.width",i,".",k,",by=length.width",i,".",k,")")))	
					eval(parse(text=paste("graph",i,".",1:length(niveaux1),".",k,"<-hist(resp.",i,".",1:length(niveaux1),".",k,",breaks=graph",i,".",k,"$breaks,plot=F)",sep="")))
						if (type=="density")
						eval(parse(text=paste("graph",i,".",1:length(niveaux1),".",k,"$counts<-graph",i,".",1:length(niveaux1),".",k,"$density",sep="")))
						if (type=="percent")
						eval(parse(text=paste("graph",i,".",1:length(niveaux1),".",k,"$counts<-graph",i,".",1:length(niveaux1),".",k,"$counts/sum(graph",i,".",1:length(niveaux1),".",k,"$counts)*100",sep="")))
					eval(parse(text=paste("supy_",i,".",k,"<-max(",paste("graph",i,".",1:length(niveaux1),".",k,"$counts",collapse=",",sep=""),")",sep="")))
					eval(parse(text=paste0("scalex_",i,".",k,"<-length(widths",i,".",k,")/(max.width",i,".",k,"-min.width",i,".",k,")")))
					eval(parse(text=paste0("seq.x_",i,".",k,"<-seq(0,length(widths",i,".",k,"),by=length(length.width",i,".",k,"))")))
					eval(parse(text=paste0("seq.axis_",i,".",k,"<-pretty(seq.x_",i,".",k,"/scalex_",i,".",k,"+min.width",i,".",k,")")))
					eval(parse(text=paste0("seq.x_",i,".",k,"<-scalex_",i,".",k,"*(seq.axis_",i,".",k,"-min.width",i,".",k,")")))
					eval(parse(text=paste0("normx_",i,".",k,"<-seq(0,length(widths",i,".",k,"),length=500)")))
					eval(parse(text=paste("m.histo",i,".",1:length(niveaux1),".",k,"<-scalex_",i,".",k,"*(resp.m",i,".",1:length(niveaux1),".",k,"-min.width",i,".",k,")",sep="")))
					eval(parse(text=paste("sd.histo",i,".",1:length(niveaux1),".",k,"<-scalex_",i,".",k,"*(resp.s",i,".",1:length(niveaux1),".",k,")",sep="")))
					eval(parse(text=paste0("test<-c(",paste0("test",i,".",1:length(niveaux1),".",k,"$p.value",collapse=","),")")))
						for (j in 1:length(niveaux1)){
							if(test[j]<0.001) test[j]<-"< 0.001" 
							else test[j]<-paste("=",round(as.numeric(test[j]),3))
						eval(parse(text=paste0("test",j,"<-test")))						
						}
					eval(parse(text=paste0("test",i,".",1:length(niveaux1),"<-test[",1:length(niveaux1),"]")))
						if (Test.shapiro) eval(parse(text=paste0("text.legend",i,".",1:length(niveaux1),"<-paste0('",niveaux1,"',' (p ',test",i,".",1:length(niveaux1),",')')")))
						else eval(parse(text=paste0("text.legend",i,".",1:length(niveaux1),"<-paste0('",niveaux1,"')")))
					eval(parse(text=paste("densityx",i,".",1:length(niveaux1),".",k,"<-dnorm(normx_",i,".",k,",m.histo",i,".",1:length(niveaux1),".",k,",sd.histo",i,".",1:length(niveaux1),".",k,")",sep="")))
					eval(parse(text=paste("densityx",i,".",1:length(niveaux1),".",k,"<-densityx",i,".",1:length(niveaux1),".",k,"/max(densityx",i,".",1:length(niveaux1),".",k,
					")*max(graph",i,".",1:length(niveaux1),".",k,"$counts)",sep="")))
					}
				n.row<-length(niveaux2)
				# definition des legendes
					if(is.null(name.repeated)) name.repeated<-ex$list.factor[1]
					if(is.null(x.label)) x.lab<-paste(ex$list.response[k],"for",ex$list.factor[2],"=")
					if(is.null(y.label)) y.label<-str_to_title(type)
				#sortie du dessin
				x11()
				layout(matrix(c(1,seq(2,n.row+1)),n.row+1,1,byrow=TRUE),widths=28*n.row+4,heights=c(4,rep(28,n.row)),TRUE)				
				par(mar=c(0,0,0,0))
				plot(c(0,1),c(0,1),type="n",axes=F,xlab="",ylab="")
					if (main=="Multiple histogram")
					text(.5,.5,paste(main,"for",name.repeated,"and",ex$list.response[k]),cex=1.5)
					else
					text(.5,.5,main,cex=1.5)
				par(mar=c(4,4,0,2))
					for (i in 1:length(niveaux2)){
					eval(parse(text=paste0("barplot",dimension,"(graph",i,".",1,".",k,"$counts,space=0,ylim=c(0,supy_",i,".",k,"*1.1),xlab=\"",x.lab,niveaux2[i],"\",main='',ylab=\"",y.label,"\",col=color[1])")))
					eval(parse(text=paste0("text.legend<-c(",paste0("text.legend",i,".",1:length(niveaux1),collapse=","),")")))
						for (j in 2:length(niveaux1)){
						if (is.null(legend.lab)) legend.lab<-name.repeated
						eval(parse(text=paste("barplot",dimension,"(graph",i,".",j,".",k,"$counts,space=0,ylim=c(0,supy_",i,".",k,"*1.1),col=color[",j,"],add=TRUE)",sep="")))
							if (normal){
							eval(parse(text=paste("lines(normx_",i,".",k,",densityx",i,".",1:length(niveaux1),".",k,",lwd=2,col=",1:j,")",sep="")))
							legend(leg,text.legend,lty=1,title=legend.lab,col=palette()[1:j],fill=color[1:length(niveaux1)])
							}
							else legend(leg,text.legend,title=legend.lab,fill=color[1:j]) 
						}
					eval(parse(text=paste0("axis(1,seq.x_",i,".",k,",seq.axis_",i,".",k,")")))
					}
				}
	
			}
		}
	}
}

#######################################################################################
#Representation frequences cumulees
cum.frequency.graph<-function (formula,data=parent.frame(),xlab=NULL,ylab=NULL,frequency=c("counts","percents"),xscale=c("normal","inverse"),
legend.pos="bottomright",main="Cumulative representation",colour=palette())
{
f <- formula(formula)
listvar <- as.character(attr(terms(f), "variables"))[-1]
response<-listvar[1]
factor<-listvar[2]
valid <- complete.cases(with(data,eval(parse(text=response))),with(data,eval(parse(text=factor))))
response <- with(data,eval(parse(text=response)))[valid]
	if (!is.numeric(response))
	stop(gettext("Variable1 must be numeric.",domain="R-RcmdrPlugin.TestGraph"))
factor <- with(data,eval(parse(text=factor)))[valid]
	if (!is.factor(factor))
	stop(gettext("Variable2 must be factor.",domain="R-RcmdrPlugin.TestGraph"))
factor<-factor(factor)
levs<-levels(factor)
frequency<-match.arg(frequency)
xscale<-match.arg(xscale)
freq<-switch(frequency, counts = 1, percents = 2)
scale<-switch(xscale, normal = 1, inverse = 2)
table <- tapply(response, factor, function(x) x)
supx<-0;infx<-min(table[[1]]);supy<-0
	for (i in 1:length(table)){
	name<-paste("table.",i,sep="")
		if (scale==1) assign(name,cum.freq(table(table[i]),order=TRUE))
		if (scale==2) assign(name,cum.freq(table(table[i]),order=FALSE))
		if (freq==2)
		eval(parse(text=paste("table.",i,"<-table.",i,"/table.",i,"[length(table.",i,")]*100",sep="")))
	supx<-max(supx,eval(parse(text=paste("as.numeric(names(table.",i,"))",sep=""))))
	infx<-min(infx,eval(parse(text=paste("as.numeric(names(table.",i,"))",sep=""))))
	supy<-max(supy,eval(parse(text=name)))
		if (freq==2) supy<-100
	}
	if (scale==1) xlim<-c(infx,supx)
	if (scale==2) xlim<-c(supx,infx)
	if (is.null(ylab)){
		if (freq==2) yaxis<-"Cumulative percents"
		else yaxis<-"Cumulative counts"
	}
	else yaxis<-ylab
	x.label<-if (is.null(xlab)) listvar[1] else xlab
plot(xlim,c(0, supy),xlim=xlim,xlab=x.label,ylab=yaxis,type="n", main = main)
for (i in 1:length(table))
eval(parse(text=paste("lines(as.numeric(names(table.",i,")),table.",i,",lwd=2,col='",colour[i],"')",sep="")))
legend(legend.pos,col=colour[1:length(table)],lwd=2,paste(listvar[2],"=",levs))
}
############################################################################
#Representation Anova un facteur ploterror
plot.Anova.one<-function (formula,covariate=NULL,num.variable=1,data=parent.frame(),xlab = NULL,ylab = NULL,ylim=NULL,main=NULL,las=1,lab.comp=NULL,cex.axis=1, col = palette())
{
#extraction des facteurs
eval(parse(text=paste0("ex<-Extract.fact(",deparse(substitute(formula)),",covariate=",deparse(substitute(covariate)),",data=",deparse(substitute(data)),")")))
	if (!is.null(ex$list.covariate)){
	data.complete<-eval(parse(text=paste("na.omit(with(",deparse(substitute(data)),",as.data.frame(cbind(",ex$list.response,",",
	paste(ex$list.factor,"=as.character(",ex$list.factor,")",collapse=",",sep=""),",",paste(ex$list.covariate,"=as.character(",ex$list.covariate,")",collapse=",",sep=""),"))))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.covariate,"<-as.numeric(as.character(data.complete$",ex$list.covariate,"))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.response,"<-as.numeric(as.character(data.complete$",ex$list.response,"))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.factor,"<-factor(data.complete$",ex$list.factor,")",sep="")))
	formula.cov<-paste(deparse(substitute(formula)),"+",paste(ex$list.covariate,collapse="+"))
	}
	else{
	data.complete<-eval(parse(text=paste("na.omit(with(",deparse(substitute(data)),",as.data.frame(cbind(",ex$list.response,",",
	paste(ex$list.factor,"=as.character(",ex$list.factor,")",sep="",collapse=","),"))))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.response,"<-as.numeric(as.character(data.complete$",ex$list.response,"))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.factor,"<-factor(data.complete$",ex$list.factor,")",sep="")))
	}
.Anova<-lm(formula, data=data.complete)
formula.cov<-paste(deparse(substitute(formula)))
.Anova.cov<-lm(formula.cov,data=data)
emm<-eval(parse(text=paste("marg.true.Means(~",ex$list.factor[num.variable],",fm=.Anova.cov)",sep="")))
	if (length(emm$Adj.mean)!=0) emm$mean<-emm$Adj.mean
	if (is.null(ylim)) ylim<-c(min(emm$mean - emm$se), max(emm$mean + emm$se))
yrange <-  ylim
levs <- levels(emm[,1])
n.levs <- length(levs)
if (is.null(xlab)) xlab <- ex$list.factor[num.variable]
if (is.null(ylab)) ylab <- paste(ex$list.response,"Mean")
if(!is.null(main)) textmain<-main else textmain<-""
plot(c(1, n.levs),yrange,type = "n",xlab=xlab,ylab=ylab,main=textmain,axes=FALSE)
points(1:n.levs, emm$mean,col=col, type = "l",  cex = 2)
if(!is.null(lab.comp)){
mini.x<-par("usr")[1]
decalage<-(1-mini.x)/2
text((1:n.levs)-decalage, emm$mean,pos=3,lab.comp)
 }
box()
axis(2)
axis(1, at = 1:n.levs, labels = levs,las=las,cex.axis=cex.axis)
      for (i in 1:n.levs) {
      arrows(i,emm$mean[i]-emm$se[i],i,emm$mean[i]+emm$se[i],angle = 90,col=col,code=3,lty=1,length=0.125)
      }

   invisible(NULL)
}
############################################################################
#Representation Anova un facteur boxplot
box.Anova.one<-function (formula,data=parent.frame(),num.variable=1,xlab = NULL,ylab = NULL,ylim=NULL,main=NULL,lab.comp=NULL,cex.axis=1,col = "black")
{
f <- formula(formula)
listvar <- as.character(attr(terms(f), "variables"))[-1]
responsename<-listvar[1]
factorname<-listvar[2:length(listvar)]
f<-paste(responsename,"~",factorname[num.variable])
if (is.null(xlab)) xlab <- factorname[num.variable]
if (is.null(ylab)) ylab <- paste(responsename,"Mean")
if(!is.null(main)) textmain<-main else textmain<-""
dessin<-boxplot(formula(f),data=data,col="white",outcol=col,medcol=col,ylab=ylab,main=textmain,xlab=xlab,ylim=ylim)
lines(1:length(dessin$names), dessin$stats[3,],col=col,lwd=2)
if (!is.null(lab.comp)) text(1:length(dessin$names), dessin$stats[3,],pos=3,adj=1,lab.comp)
}
########################################################################
#Representation Anova un facteurs avec variable temporelle ploterror
plot.Anova.one.rep<-function (formula,covariate=NULL,name.variable="Response",num.variable=1,data=parent.frame(),xlab = NULL,
ylab = NULL,ylim=NULL,las=1,cex.axis=1,legend.lab=NULL, col = palette())
{
#extraction des facteurs
eval(parse(text=paste0("ex<-Extract.fact(",paste0(deparse(substitute(formula)),collapse=""),",covariate=",deparse(substitute(covariate)),",data=",deparse(substitute(data)),")")))
	if( !is.null(ex$list.covariate))
	model.2<-paste0("~",paste(ex$list.factor,collapse="*"),"+",paste(ex$list.covariate,collapse="+"))
	else
	model.2<-paste0("~",paste(ex$list.factor,collapse="*"))
#calul moyennes marginales
	for (l in 1:ex$length.response){
	modele<-paste(ex$list.response[l],model.2,sep="")
	.Anova<-eval(parse(text=paste("lm(",modele,",data=",deparse(substitute(data)),")",sep="")))
	eval(parse(text=paste("emm_",l,"<-marg.true.Means(~",ex$list.factor[num.variable],",fm=.Anova)",sep="")))
	eval(parse(text=paste("n.row1_",l,"<-nrow(emm_",l,")",sep="")))
	eval(parse(text=paste("n.col1_",l,"<-ncol(emm_",l,")",sep="")))
	name.col1<-eval(parse(text=paste("colnames(emm_",l,")[1]",sep="")))
	name.response<-ex$list.response[l]
	eval(parse(text=paste("emm_",l,"<-cbind(",name.col1,"=emm_",l,"[,1],",name.variable,"=rep('",name.response,"',n.row1_",l,"),emm_",l,"[,2:n.col1_",l,"])",sep="")))
	}
n.row<-nrow(emm_1)
emm_rep<-NULL
  	for (k in 1:n.row){
       	for (j in 1:ex$length.response){
       	eval(parse(text=paste("emm_rep<-rbind(emm_rep,emm_",j,"[",k,",])",sep="")))
    	}
  	}
emm_rep[,2]<-as.factor(emm_rep[,2])
levs.1<-levels(emm_rep[,2])
tri<-sub("[A-Za-z./]*", "", levs.1) 
levs.1<-levs.1[order(as.numeric(tri))]
levs.2<-levels(emm_rep[,1])
n.levs.1 <- length(levs.1)
n.levs.2 <- length(levs.2)
	if (ex$length.factor>1) 
	adj.means<-tapply(emm_rep$Adj.mean,emm_rep[2:1],function(x) x)
	else 
	adj.means<-tapply(emm_rep$mean,emm_rep[2:1],function(x) x)
adj.se<-tapply(emm_rep$se,emm_rep[2:1],function(x) x)
adj.se<-adj.se[order(as.numeric(tri)),]
adj.means<-adj.means[order(as.numeric(tri)),]
	if (is.null(ylim)) ylim<-c(min(adj.means - adj.se), max(adj.means - adj.se))
yrange <-  ylim
	if (is.null(xlab)) xlab <- name.variable
	if (is.null(ylab)) ylab <- paste(name.variable,"Mean")
	if (is.null(legend.lab)) legend.lab<-ex$list.factor[num.variable]
par(mar=c(5,4,0,0))
layout(matrix(c(1,2,1,2),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)  
plot(c(1, n.levs.1),yrange,type = "n",xlab=xlab,ylab=ylab,axes=FALSE)
box()
axis(2)
axis(1, at = 1:n.levs.1, labels = levs.1,cex.axis=cex.axis)
	for (i in 1:n.levs.2) {
		for (j in 1:n.levs.1){
		points(j,adj.means[j,i],type="l",cex=2,col=col[i],lty=1)
		arrows(j,adj.means[j,i]-adj.se[j,i],j,adj.means[j,i]+adj.se[j,i],angle=90,code=3,col=col[i],lty=1,length=0.125)
		}
    for (j in 1:(n.levs.1-1))
    segments(j,adj.means[j,i],j+1,adj.means[j+1,i],col=col[i],lty=1)	
	}
par(mar=c(0,0,0,0))
plot(c(0,1),type="n",axes=F)
legend("center",c(legend.lab,levs.2),bty="n",col=c(0,col),lty =c(1,1))
par(mar=c(5,4,4,2))
invisible(NULL)
}
############################################################################
#Representation Anova deux facteurs boxplot avec variable temporelle
box.Anova.one.rep<-function (formula,name.variable="Response",data=parent.frame(),num.variable=1,xlab = NULL,ylab = NULL,ylim=NULL,main=NULL,legend.lab=NULL,
cex.axis=1,col = palette())
{
#extraction des facteurs
eval(parse(text=paste0("ex<-Extract.fact(",deparse(substitute(formula)),",data=",deparse(substitute(data)),")")))
data.complete<-eval(parse(text=paste("na.omit(with(",deparse(substitute(data)),",as.data.frame(cbind(",
paste(ex$list.response,"=as.character(",ex$list.response,")",collapse=",",sep=""),",",
paste(ex$list.factor,"=as.character(",ex$list.factor,")",collapse=",",sep=""),"))))",sep="")))
Repeated.data<-eval(parse(text=paste("Repeated.data.frame(",paste(ex$list.response,collapse="+"),",factors=",paste(ex$list.factor,collapse="+"),
",time.variable='",name.variable,"',data=data.complete)",sep="")))
eval(parse(text=paste0("Repeated.data$",name.variable,".value<-as.numeric(Repeated.data$",name.variable,".value)")))
f<-paste0(name.variable,".value~",name.variable,"+",ex$list.factor[num.variable])
dessin<-boxplot(formula(f),data=Repeated.data,col="white",ylab=ylab,main=main,xlab=xlab,ylim=ylim,plot=F)
if (is.null(ylim)) yrange<-c(min(dessin$stats,dessin$out),max(dessin$stats,dessin$out))
else yrange<-ylim
eval(parse(text=paste0("lev2<-levels(Repeated.data$",ex$list.factor[num.variable],")")))
eval(parse(text=paste0("lev1<-levels(Repeated.data$",name.variable,")")))
if (is.null(xlab)) xlab <- name.variable
if (is.null(ylab)) ylab <- paste(name.variable,"Mean")
outliers<-data.frame(x=dessin$group,y=dessin$out)
TotalBox<-dessin$stats
num.box<-1
	for (i in 1:length(lev2)){
	eval(parse(text=paste0("Box",num.box,"<-TotalBox[,1:length(lev1)]")))
	eval(parse(text=paste0("TotalBox<-TotalBox[,-(1:length(lev1))]")))
	num.box<-num.box+1
	}
left.box<-(1:length(lev1))-.2
right.box<-(1:length(lev1))	
med.box<-(left.box+right.box)/2
x.sup<-0.25*(length(lev2)-1)
par(mar=c(5,4,0,0))
layout(matrix(c(1,2,1,2),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)  
plot(c(0.8,length(lev1)+x.sup),yrange,type = "n",xlab=xlab,ylab=ylab,axes=FALSE)
box()
axis(2)
axis(1, 1:length(lev1), lev1)
	for (i in 1:length(lev2)){
	eval(parse(text=paste0("DrawBow<-Box",i)))
	rect(left.box,DrawBow[2,],right.box,DrawBow[4,],border="black")
	lines(left.box,DrawBow[3,],col=col[i],lwd=2)
		for (j in 1:length(lev1)){
		lines(c(left.box[j],right.box[j]),c(DrawBow[3,j],DrawBow[3,j]),lwd=2,col=col[i])
		lines(c(med.box[j],med.box[j]),c(DrawBow[1,j],DrawBow[2,j]),col="black",lty=2)
		lines(c(med.box[j],med.box[j]),c(DrawBow[4,j],DrawBow[5,j]),col="black",lty=2)
		lines(c(left.box[j]+.05,right.box[j]-.05),c(DrawBow[1,j],DrawBow[1,j]),col="black")
		lines(c(left.box[j]+.05,right.box[j]-.05),c(DrawBow[5,j],DrawBow[5,j]),col="black")
		}
	left.box<-left.box+.25
	right.box<-right.box+.25
	med.box<-med.box+.25
	}
	if(is.null(legend.lab)) legend.lab<- ex$list.factor[num.variable]
par(mar=c(0,0,0,0))
plot(c(0,1),type="n",axes=F)
legend("center",c(legend.lab,lev2),bty="n",col=c(0,col),lwd =c(2,2))
par(mar=c(5,4,4,2))
invisible(NULL)	
}
#############################################################
#Representation Anova un facteurs facteur niche
plot.Anova.one.nested<-function(formula,covariate=NULL,nested.factor=NULL,nested.in=NULL,num.variable=1, plot.nested=FALSE,data=parent.frame(),xlab = NULL,
ylab = NULL,ylim=NULL,main=NULL,lab.comp=NULL,legend.lab=NULL,col = palette()){
require(stringi)
#extraction des facteurs
eval(parse(text=paste0("ex<-Extract.fact(",deparse(substitute(formula)),",covariate=",deparse(substitute(covariate)),",data=",deparse(substitute(data)),")")))
#donnees completes
	if (ex$length.covariate!=0){
	data.complete<-eval(parse(text=paste("na.omit(with(",deparse(substitute(data)),",as.data.frame(cbind(",ex$list.response,",",
	paste(ex$list.factor,"=as.character(",ex$list.factor,")",collapse=",",sep=""),",",paste(ex$list.covariate,"=as.character(",ex$list.covariate,")",collapse=",",sep=""),
	",",deparse(substitute(nested.in)),"=as.character(",deparse(substitute(nested.in)),")))))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.covariate,"<-as.numeric(as.character(data.complete$",ex$list.covariate,"))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.response,"<-as.numeric(as.character(data.complete$",ex$list.response,"))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.factor,"<-factor(data.complete$",ex$list.factor,")",sep="")))
	formula.cov<-paste(deparse(substitute(formula)),"+",paste(ex$list.covariate,collapse="+"))
	}
	else {
	data.complete<-eval(parse(text=paste("na.omit(with(",deparse(substitute(data)),",as.data.frame(cbind(",ex$list.response,",",
	paste(ex$list.factor,"=as.character(",ex$list.factor,")",sep="",collapse=","),",",deparse(substitute(nested.in)),"=as.character(",deparse(substitute(nested.in)),")))))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.response,"<-as.numeric(as.character(data.complete$",ex$list.response,"))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.factor,"<-factor(data.complete$",ex$list.factor,")",sep="")))
	}
eval(parse(text=paste("data.complete$",deparse(substitute(nested.in)),"<-factor(data.complete$",deparse(substitute(nested.in)),")",sep="")))
	for (i in 1:ex$length.factor){
	eval(parse(text=paste("levs",i,"<-levels(data.complete$",ex$list.factor[i],")",sep="")))  
	eval(parse(text=paste("levs",i,"<-levs",i,"[levs",i,"!='']",sep="")))
	}
max.levs<-NULL
	for (i in 1:ex$length.factor)
  	eval(parse(text=paste("max.levs<-max(max.levs,length(levs",i,"))",sep="")))
formula.cov<-paste(deparse(substitute(formula)))
formula.nested.in<-paste(ex$list.response,"~",deparse(substitute(nested.in)),"*",deparse(substitute(nested.factor)))
	if (ex$length.covariate!=0){
	formula.cov<-paste(deparse(substitute(formula)),"+",paste(listcov,collapse="+"))
	formula.nested.in<-paste(formula.nested.in,"+",paste(listcov,collapse="+"))
	}
	else
	formula.nested<-paste(formula.cov,"+",deparse(substitute(nested.in)),"%in%",deparse(substitute(nested.factor)))
.Anova.nested<-lm(formula.nested,data=data.complete)
levs<-matrix("",nrow=length(ex$list.factor),ncol=max.levs)
	for (i in 1:length(ex$list.factor)){
	eval(parse(text=paste("levs[",i,",1:length(levs",i,")]<-levs",i,sep="")))
	eval(parse(text=paste("emm_",i,"<-marg.true.Means.nested(~",ex$list.factor[i],",fm=.Anova.nested,nested.factor=",deparse(substitute(nested.factor)),
	",nested.in=",deparse(substitute(nested.in)),")",sep="")))
	}
	if (!plot.nested){
	eval(parse(text=paste0("emm<-emm_",num.variable)))
	emm$se<-emm$sd/sqrt(emm$N)
		if (length(emm$Adj.mean)!=0) emm$mean<-emm$Adj.mean
		if (is.null(ylim)) ylim<-c(min(emm$mean - emm$se), max(emm$mean + emm$se))
	yrange <-  ylim
	levs <- levels(emm[,1])
	n.levs <- length(levs)
		if (is.null(xlab)) xlab <- ex$list.factor[num.variable]
		if (is.null(ylab)) ylab <- paste(ex$list.response,"Mean")
		if(!is.null(main)) textmain<-main else textmain<-""
	plot(c(1, n.levs),yrange,type = "n",xlab=xlab,ylab=ylab,main=textmain,axes=FALSE)
	points(1:n.levs, emm$mean,col=col, type = "l",  cex = 2)
		if(!is.null(lab.comp)) text((1:n.levs)-0.1, emm$mean,pos=3,adj=1,lab.comp)
	box()
	axis(2)
	axis(1, at = 1:n.levs, labels = levs)
		for (i in 1:n.levs) {
		arrows(i,emm$mean[i]-emm$se[i],i,emm$mean[i]+emm$se[i],angle = 90,col=col,code=3,lty=1,length=0.125)
		}	
	}
	else{
		#sortie facteur niche
	eval(parse(text=paste("emm_n<-marg.true.Means.nested(~",deparse(substitute(nested.in)),",fm=lm(",formula.nested,",data=data.complete),nested.factor=",deparse(substitute(nested.factor)),",nested.in=",deparse(substitute(nested.in)),")",sep="")))
	nested.factor<-unlist(strsplit(deparse(substitute(nested.factor)),":"))
		if (length(nested.factor)>1)
		eval(parse(text=paste0("data.complete$",nested.factor[1:(length(nested.factor)-1)],"<-paste0(",paste0("data.complete$",nested.factor[1:(length(nested.factor)-1)],",'.')"))))
	eval(parse(text=paste0("factor.name<-paste0(",paste0("data.complete$",nested.factor,collapse=","),")")))
	factor.name<-levels(as.factor(factor.name))
	eval(parse(text=paste0("emm_n$'",paste(nested.factor,collapse=":"),"'<-NA")))
		for (i in 1:length(emm_n[,1]))
		eval(parse(text=paste0("emm_n$'",paste(nested.factor,collapse=":"),"'[",i,"]<-factor.name[stri_detect_fixed(emm_n[",i,",1], factor.name)]")))
	eval(parse(text=paste0("levs.1<-levels(data.complete$",deparse(substitute(nested.in)),")")))
	levs.2 <- factor.name
	n.levs.1 <- length(levs.1)
	n.levs.2 <- length(levs.2)
	response <- emm_n$mean
	factor <-emm_n[,deparse(substitute(nested.in))]
	subfactor <-emm_n[,ncol(emm_n)]
	std.error<- emm_n$se
	adj.means<-tapply(response, list(factor,subfactor), function(x) x)
	adj.se<-tapply(std.error, list(factor,subfactor), function(x) x)
	eval(parse(text=paste0("adj.values<-cbind(",paste0("'mean.",colnames(adj.means),"'=adj.means[,",1:n.levs.2,"],'se.",colnames(adj.se),"'=adj.se[,",1:n.levs.2,"]",collapse=","),")")))
	valeur.min<-adj.means-adj.se
	valeur.max<-adj.means+adj.se
	eval(parse(text=paste0("min.means<-min(",paste0("na.omit(valeur.min[,",1:n.levs.2,"])",collapse=","),")")))
	eval(parse(text=paste0("max.means<-max(",paste0("na.omit(valeur.max[,",1:n.levs.2,"])",collapse=","),")")))
		if (is.null(ylim)) ylim<-c(min.means,max.means)
	yrange <-  ylim
	eval(parse(text=paste0("adj.mse<-adj.values[order(",paste0("adj.means[,",1:n.levs.2,"]",collapse=","),"),]")))
	eval(parse(text=paste0("adj.means<-adj.mse[,c(",paste0("'mean.",colnames(adj.means),"'",collapse=","),")]")))
	eval(parse(text=paste0("adj.se<-adj.mse[,c(",paste0("'se.",colnames(adj.se),"'",collapse=","),")]")))
		if (is.null(xlab)) xlab <- deparse(substitute(nested.in))
		if (is.null(ylab)) ylab <- paste(ex$list.response,"Mean")
		if(!is.null(main)) textmain<-main else textmain<-""
		if (is.null(legend.lab)) legend.lab<-paste(nested.factor,collapse=":")
	par(mar=c(5,4,0,0))
	layout(matrix(c(1,2,1,2),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)  
	plot(c(1, n.levs.1),yrange,type = "n",xlab=xlab,ylab=ylab,axes=FALSE)
	box()
	axis(2)
	axis(1, at = 1:n.levs.1, labels =  names(levs.1))
		for (i in 1:n.levs.2) {
			for (j in 1:n.levs.1){
			points(j,adj.means[j,i],type="l",cex=2,col=col[i],lty=1)
			arrows(j,adj.means[j,i]-adj.se[j,i],j,adj.means[j,i]+adj.se[j,i],angle=90,code=3,col=col[i],lty=1,length=0.125)
			}
			for (j in 1:(n.levs.1-1))
			segments(j,adj.means[j,i],j+1,adj.means[j+1,i],col=col[i],lty=1)	
		}
	par(mar=c(0,0,0,0))
	plot(c(0,1),type="n",axes=F)
	legend("center",c(legend.lab,levs.2),bty="n",col=c(0,col),lty =c(1,1))
	par(mar=c(5,4,4,2))		
	}
}
############################################################################
#Representation Anova deux facteurs
plot.Anova.two<-function (formula,covariate=NULL,num.variable=c(1,2),data=parent.frame(),xlab = NULL,ylab = NULL,ylim=NULL,cex.axis=1,legend.lab=NULL, col = palette())
{
#extraction des facteurs
eval(parse(text=paste0("ex<-Extract.fact(",deparse(substitute(formula)),",covariate=",deparse(substitute(covariate)),",data=",deparse(substitute(data)),")")))
	if (!is.null(ex$list.covariate)){
	data.complete<-eval(parse(text=paste("na.omit(with(",deparse(substitute(data)),",as.data.frame(cbind(",ex$list.response,",",
	paste(ex$list.factor,"=as.character(",ex$list.factor,")",collapse=",",sep=""),",",paste(ex$list.covariate,"=as.character(",ex$list.covariate,")",collapse=",",sep=""),"))))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.covariate,"<-as.numeric(as.character(data.complete$",ex$list.covariate,"))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.response,"<-as.numeric(as.character(data.complete$",ex$list.response,"))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.factor,"<-factor(data.complete$",ex$list.factor,")",sep="")))
	formula.cov<-paste(deparse(substitute(formula)),"+",paste(ex$list.covariate,collapse="+"))
	}
	else{
	data.complete<-eval(parse(text=paste("na.omit(with(",deparse(substitute(data)),",as.data.frame(cbind(",ex$list.response,",",
	paste(ex$list.factor,"=as.character(",ex$list.factor,")",sep="",collapse=","),"))))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.response,"<-as.numeric(as.character(data.complete$",ex$list.response,"))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.factor,"<-factor(data.complete$",ex$list.factor,")",sep="")))
	}
.Anova<-lm(formula, data=data.complete)
formula.cov<-paste(deparse(substitute(formula)))
	if (ex$length.covariate!=0)
	formula.cov<-paste(deparse(substitute(formula)),"+",paste(ex$list.covariate,collapse="+"))
.Anova.cov<-lm(formula.cov,data=data)
emm<-eval(parse(text=paste("marg.true.Means(~",ex$list.factor[num.variable[1]],"*",ex$list.factor[num.variable[2]],",fm=.Anova.cov)",sep="")))
	if (is.null(ylim)) yrange<-c(min(emm$Adj.mean - emm$se), max(emm$Adj.mean + emm$se))
	else yrange <-  ylim
levs.1 <- levels(emm[,ex$list.factor[num.variable[1]]])
levs.2 <- levels(emm[,ex$list.factor[num.variable[2]]])
n.levs.1 <- length(levs.1)
n.levs.2 <- length(levs.2)
response <- emm$Adj.mean
factor <-emm[,ex$list.factor[num.variable[1]]]
subfactor <-emm[,ex$list.factor[num.variable[2]]]
std.error<- emm$se
adj.means<-tapply(response, list(factor,subfactor), function(x) x)
adj.se<-tapply(std.error, list(factor,subfactor), function(x) x)
	if (is.null(xlab)) xlab<-ex$list.factor[num.variable[1]]
	if (is.null(ylab)) ylab<-paste(ex$list.response,"Mean")
	if (is.null(legend.lab)) legend.lab<-ex$list.factor[num.variable[2]]
par(mar=c(5,4,0,0))
layout(matrix(c(1,2,1,2),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)  
plot(c(1, n.levs.1),yrange,type = "n",xlab=xlab,ylab=ylab,axes=FALSE)
box()
axis(2)
axis(1, at = 1:n.levs.1, labels = levs.1,cex.axis=cex.axis)
	for (i in 1:n.levs.2) {
		for (j in 1:n.levs.1){
		points(j,adj.means[j,i],type="l",cex=2,col=col[i],lty=1)
		arrows(j,adj.means[j,i]-adj.se[j,i],j,adj.means[j,i]+adj.se[j,i],angle=90,code=3,col=col[i],lty=1,length=0.125)
		}
		for (j in 1:(n.levs.1-1))
		segments(j,adj.means[j,i],j+1,adj.means[j+1,i],col=col[i],lty=1)	
	}
par(mar=c(0,0,0,0))
plot(c(0,1),type="n",axes=F)
legend("center",c(legend.lab,levs.2),bty="n",col=c(0,col),lty =c(1,1))
par(mar=c(5,4,4,2))
invisible(NULL)
}
############################################################################
#Representation Anova deux facteurs boxplot
box.Anova.two<-function (formula,data=parent.frame(),num.variable=c(1,2),xlab = NULL,ylab = NULL,ylim=NULL,main=NULL,legend.lab=NULL,cex.axis=1,col = palette())
{
f <- formula(formula)
listvar <- as.character(attr(terms(f), "variables"))[-1]
responsename<-listvar[1]
factorname<-listvar[2:length(listvar)]
f<-paste(responsename,"~",paste(factorname[num.variable],collapse="+"))
dessin<-boxplot(formula(f),data=data,col="white",ylab=ylab,main=main,xlab=xlab,ylim=ylim,plot=F)
if (is.null(ylim)) yrange<-c(min(dessin$stats,dessin$out),max(dessin$stats,dessin$out))
else yrange<-ylim
eval(parse(text=paste0("lev1<-levels(",substitute(data),"$",factorname[num.variable[1]],")")))
eval(parse(text=paste0("lev2<-levels(",substitute(data),"$",factorname[num.variable[2]],")")))
if (is.null(xlab)) xlab <- factorname[num.variable[1]]
if (is.null(ylab)) ylab <- paste(responsename,"Mean")
outliers<-data.frame(x=dessin$group,y=dessin$out)
TotalBox<-dessin$stats
num.box<-1
	for (i in 1:length(lev2)){
	eval(parse(text=paste0("Box",num.box,"<-TotalBox[,1:length(lev1)]")))
	eval(parse(text=paste0("TotalBox<-TotalBox[,-(1:length(lev1))]")))
	num.box<-num.box+1
	}
left.box<-(1:length(lev1))-.2
right.box<-(1:length(lev1))	
med.box<-(left.box+right.box)/2
x.sup<-0.25*(length(lev2)-1)
par(mar=c(5,4,0,0))
layout(matrix(c(1,2,1,2),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)  
plot(c(0.8,length(lev1)+x.sup),yrange,type = "n",xlab=xlab,ylab=ylab,axes=FALSE)
box()
axis(2)
axis(1, 1:length(lev1), lev1)
	for (i in 1:length(lev2)){
	eval(parse(text=paste0("DrawBow<-Box",i)))
	rect(left.box,DrawBow[2,],right.box,DrawBow[4,],border="black")
	lines(left.box,DrawBow[3,],col=col[i],lwd=2)
		for (j in 1:length(lev1)){
		lines(c(left.box[j],right.box[j]),c(DrawBow[3,j],DrawBow[3,j]),lwd=2,col=col[i])
		lines(c(med.box[j],med.box[j]),c(DrawBow[1,j],DrawBow[2,j]),col="black",lty=2)
		lines(c(med.box[j],med.box[j]),c(DrawBow[4,j],DrawBow[5,j]),col="black",lty=2)
		lines(c(left.box[j]+.05,right.box[j]-.05),c(DrawBow[1,j],DrawBow[1,j]),col="black")
		lines(c(left.box[j]+.05,right.box[j]-.05),c(DrawBow[5,j],DrawBow[5,j]),col="black")
		}
	left.box<-left.box+.25
	right.box<-right.box+.25
	med.box<-med.box+.25
	}
	if(is.null(legend.lab)) legend.lab<-factorname[num.variable[2]]
par(mar=c(0,0,0,0))
plot(c(0,1),type="n",axes=F)
legend("center",c(legend.lab,lev2),bty="n",col=c(0,col),lwd =c(2,2))
par(mar=c(5,4,4,2))
invisible(NULL)	
}
############################################################################
#Representation Anova deux facteurs plotError avec variable temporelle
plot.Anova.two.rep<-function (formula,covariate=NULL,name.variable="Response",num.variable=c(1,2),data=parent.frame(),xlab = NULL,
sub=NULL,ylab = NULL,ylim=NULL,legend.pos=NULL,cex.axis=1,legend.lab=NULL, col = palette())
{
#extraction des facteurs
eval(parse(text=paste0("ex<-Extract.fact(",deparse(substitute(formula)),",covariate=",deparse(substitute(covariate)),",data=",deparse(substitute(data)),")")))
	if( !is.null(ex$list.covariate))
	model.2<-paste0("~",paste(ex$list.factor,collapse="*"),"+",paste(ex$list.covariate,collapse="+"))
	else
	model.2<-paste0("~",paste(ex$list.factor,collapse="*"))
#Moyennes marginales des reponses repetees
	for (l in 1:ex$length.response){
	modele<-paste(ex$list.response[l],model.2,sep="")
	.Anova<-eval(parse(text=paste("lm(",modele,",data=",deparse(substitute(data)),")",sep="")))
	eval(parse(text=paste("emm_",l,"<-marg.true.Means(~",ex$list.factor[num.variable[1]],"*",paste(ex$list.factor[num.variable[2]]),",fm=.Anova)",sep="")))
	eval(parse(text=paste("n.row_",l,"<-nrow(emm_",l,")",sep="")))
	eval(parse(text=paste("n.col_",l,"<-ncol(emm_",l,")",sep="")))
	name.col1<-eval(parse(text=paste("colnames(emm_",l,")[1]",sep="")))
	name.col2<-eval(parse(text=paste("colnames(emm_",l,")[2]",sep="")))	
	name.response<-ex$list.response[l]
	eval(parse(text=paste("emm_",l,"<-cbind(",name.col1,"=emm_",l,"[,1],",name.col2,"=emm_",l,"[,2],",name.variable,"=rep('",name.response,"',n.row_",l,"),emm_",l,"[,3:n.col_",l,"])",sep="")))
	}
n.row<-nrow(emm_1)
emm_rep<-NULL
  	for (k in 1:n.row){
       	for (j in 1:ex$length.response){
       	eval(parse(text=paste("emm_rep<-rbind(emm_rep,emm_",j,"[",k,",])",sep="")))
    	}
  	}
emm_rep[,3]<-as.factor(emm_rep[,3])
levs.1<-levels(emm_rep[,3])
tri<-sub("[A-Za-z./]*", "", levs.1) 
levs.1<-levs.1[order(as.numeric(tri))]
levs.2<-levels(emm_rep[,2])
levs.3<-levels(emm_rep[,1])
n.levs.1 <- length(levs.1)
n.levs.2 <- length(levs.2)
n.levs.3 <- length(levs.3)
adj.means<-tapply(emm_rep$Adj.mean,emm_rep[3:1],function(x) x)
adj.se<-tapply(emm_rep$se,emm_rep[3:1],function(x) x)
adj.se<-adj.se[order(as.numeric(tri)),,]
adj.means<-adj.means[order(as.numeric(tri)),,]
	if (is.null(ylim)) ylim<-c(min(adj.means - adj.se), max(adj.means - adj.se))
yrange <-  ylim
	if (is.null(xlab)) xlab <- paste(name.variable,"\n")
	if (is.null(ylab)) ylab <- paste(name.variable,"Mean")
	if (is.null(sub)) sub <- paste(ex$list.factor[num.variable[2]],"=",levs.3)
	if (is.null(legend.lab)) legend.lab<-ex$list.factor[num.variable[1]]
	if (is.null(legend.pos)) legend.pos<-"topright"
matrix<-c(1:(n.levs.3+1)) 
layout(matrix(matrix, ncol = n.levs.3 + 1), width = c(0.1 * n.levs.3, rep(1, n.levs.3)))
par(mar=c(7,4,0,0))
plot(yrange,type="n",ylab=ylab,axes=F,xlab="")
abline(h=par("usr")[3])
abline(h=par("usr")[4])
abline(v=par("usr")[1])
axis(2)
par(mar=c(7,0,0,0))
k<-1
	for (var in 1:n.levs.3){
	plot(c(1,n.levs.1+.5),yrange,type="n",xlab=xlab,sub=sub[k],axes=FALSE)
	axis(1, at = 1:n.levs.1, labels = levs.1,cex.axis=cex.axis)
		for (i in 1:n.levs.2) {
			for (j in 1:n.levs.1){
			points(j,adj.means[j,i,k],type="l",cex=2,col=col[i],lty=1)
			arrows(j,adj.means[j,i,k]-adj.se[j,i,k],j,adj.means[j,i,k]+adj.se[j,i,k],angle=90,code=3,col=col[i],lty=1,length=0.125)
			}
			for (j in 1:(n.levs.1-1))
			segments(j,adj.means[j,i,k],j+1,adj.means[j+1,i,k],col=col[i],lty=1)
		abline(h=par("usr")[3])
		abline(h=par("usr")[4])   	
		}
	k=k+1
	}
legend(legend.pos,c(legend.lab,levs.2),col=c(0,col),lty =c(1,1))
 par(mar=c(5,4,4,2))
 invisible(NULL)
}
############################################################################
#Representation Anova deux facteurs boxplotavec variable temporelle
box.Anova.two.rep<-function (formula,name.variable="Response",num.variable=c(1,2),data=parent.frame(),xlab = NULL,
sub=NULL,ylab = NULL,ylim=NULL,legend.pos=NULL,cex.axis=1,legend.lab=NULL, col = palette())
{
#extraction des facteurs
eval(parse(text=paste0("ex<-Extract.fact(",deparse(substitute(formula)),",data=",deparse(substitute(data)),")")))
data.complete<-eval(parse(text=paste("na.omit(with(",deparse(substitute(data)),",as.data.frame(cbind(",
paste(ex$list.response,"=as.character(",ex$list.response,")",collapse=",",sep=""),",",
paste(ex$list.factor,"=as.character(",ex$list.factor,")",collapse=",",sep=""),"))))",sep="")))
Repeated.data<-eval(parse(text=paste("Repeated.data.frame(",paste(ex$list.response,collapse="+"),",factors=",paste(ex$list.factor,collapse="+"),
",time.variable='",name.variable,"',data=data.complete)",sep="")))
eval(parse(text=paste0("Repeated.data$",name.variable,".value<-as.numeric(Repeated.data$",name.variable,".value)")))
eval(parse(text=paste0("lev3<-levels(Repeated.data$",ex$list.factor[num.variable[2]],")")))
eval(parse(text=paste0("lev2<-levels(Repeated.data$",ex$list.factor[num.variable[1]],")")))
eval(parse(text=paste0("lev1<-levels(Repeated.data$",name.variable,")")))
n.levs.3 <- length(lev3)
eval(parse(text=paste0("Repeated.data",1:n.levs.3,"<-subset(Repeated.data,Repeated.data$",ex$list.factor[num.variable[2]],"=='",lev3,"')")))
eval(parse(text=paste0("Repeated.data",1:n.levs.3,"$",name.variable,".value<-as.numeric(Repeated.data",1:n.levs.3,"$",name.variable,".value)")))
f<-paste0(name.variable,".value~",name.variable,"+",ex$list.factor[num.variable[1]])
eval(parse(text=paste0("dessin",1:n.levs.3,"<-boxplot(formula(f),data=Repeated.data",1:n.levs.3,",col='white',ylab=ylab,xlab=xlab,ylim=ylim,plot=F)")))
	if (is.null(ylim)) eval(parse(text=paste0("yrange<-c(min(",paste0("dessin",1:n.levs.3,"$stats",collapse=","),",",paste0("dessin",1:n.levs.3,"$out",collapse=","),
	"),max(",paste0("dessin",1:n.levs.3,"$stats",collapse=","),",",paste0("dessin",1:n.levs.3,"$out",collapse=","),"))")))
	else yrange<-ylim
	if (is.null(xlab)) xlab <- paste(name.variable,"\n")
	if (is.null(ylab)) ylab <- paste(name.variable,"Mean")
	if (is.null(sub)) sub <- paste(ex$list.factor[num.variable[2]],"=",lev3)
	if (is.null(legend.lab)) legend.lab<-ex$list.factor[num.variable[1]]
	if (is.null(legend.pos)) legend.pos<-"topright"	
matrix<-c(1:(n.levs.3+1)) 
layout(matrix(matrix, ncol = n.levs.3 + 1), width = c(0.1 * n.levs.3, rep(1, n.levs.3)))
par(mar=c(7,4,0,0))
plot(yrange,type="n",ylab=ylab,axes=F,xlab="")
abline(h=par("usr")[3])
abline(h=par("usr")[4])
abline(v=par("usr")[1])
axis(2)
par(mar=c(7,0,0,0))
k<-1
	for (var in 1:n.levs.3){
	eval(parse(text=paste0("outliers<-data.frame(x=dessin",var,"$group,y=dessin",var,"$out)")))
	eval(parse(text=paste0("TotalBox<-dessin",var,"$stats")))
	num.box<-1
		for (i in 1:length(lev2)){
		eval(parse(text=paste0("Box",num.box,"<-TotalBox[,1:length(lev1)]")))
		eval(parse(text=paste0("TotalBox<-TotalBox[,-(1:length(lev1))]")))
		num.box<-num.box+1
		}	
	left.box<-(1:length(lev1))-.2
	right.box<-(1:length(lev1))	
	med.box<-(left.box+right.box)/2
	x.sup<-0.25*(length(lev2)-1)
	plot(c(0.8,length(lev1)+x.sup),yrange,type = "n",xlab=xlab,sub=sub[var],ylab=ylab,axes=FALSE)
	axis(1, 1:length(lev1), lev1)
		for (i in 1:length(lev2)){
		eval(parse(text=paste0("DrawBow<-Box",i)))
		rect(left.box,DrawBow[2,],right.box,DrawBow[4,],border="black")
		lines(left.box,DrawBow[3,],col=col[i],lwd=2)
			for (j in 1:length(lev1)){
			lines(c(left.box[j],right.box[j]),c(DrawBow[3,j],DrawBow[3,j]),lwd=2,col=col[i])
			lines(c(med.box[j],med.box[j]),c(DrawBow[1,j],DrawBow[2,j]),col="black",lty=2)
			lines(c(med.box[j],med.box[j]),c(DrawBow[4,j],DrawBow[5,j]),col="black",lty=2)
			lines(c(left.box[j]+.05,right.box[j]-.05),c(DrawBow[1,j],DrawBow[1,j]),col="black")
			lines(c(left.box[j]+.05,right.box[j]-.05),c(DrawBow[5,j],DrawBow[5,j]),col="black")
			}
		left.box<-left.box+.25
		right.box<-right.box+.25
		med.box<-med.box+.25
		}
	abline(h=par("usr")[3])
	abline(h=par("usr")[4])  		
	}
legend(legend.pos,c(legend.lab,lev2),col=c(0,col),lty =c(1,1))
 par(mar=c(5,4,4,2))
 invisible(NULL)
}
#############################################################################
#Representation Anova deux facteurs facteur niche
plot.Anova.two.nested<-function (formula,covariate=NULL,nested.factor=NULL,nested.in=NULL,num.variable=c(1,2), plot.nested=FALSE,data=parent.frame(),xlab = NULL,
ylab =NULL,ylim=NULL,main=NULL,legend.lab=NULL,legend.pos=NULL, col = palette())
{
#extraction des facteurs
eval(parse(text=paste0("ex<-Extract.fact(",deparse(substitute(formula)),",covariate=",deparse(substitute(covariate)),",data=",deparse(substitute(data)),")")))
#donnees completes
	if (ex$length.covariate!=0){
	data.complete<-eval(parse(text=paste("na.omit(with(",deparse(substitute(data)),",as.data.frame(cbind(",ex$list.response,",",
	paste(ex$list.factor,"=as.character(",ex$list.factor,")",collapse=",",sep=""),",",paste(ex$list.covariate,"=as.character(",ex$list.covariate,")",collapse=",",sep=""),
	",",deparse(substitute(nested.in)),"=as.character(",deparse(substitute(nested.in)),")))))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.covariate,"<-as.numeric(as.character(data.complete$",ex$list.covariate,"))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.response,"<-as.numeric(as.character(data.complete$",ex$list.response,"))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.factor,"<-factor(data.complete$",ex$list.factor,")",sep="")))
	formula.cov<-paste(deparse(substitute(formula)),"+",paste(ex$list.covariate,collapse="+"))
	}
	else {
	data.complete<-eval(parse(text=paste("na.omit(with(",deparse(substitute(data)),",as.data.frame(cbind(",ex$list.response,",",
	paste(ex$list.factor,"=as.character(",ex$list.factor,")",sep="",collapse=","),",",deparse(substitute(nested.in)),"=as.character(",deparse(substitute(nested.in)),")))))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.response,"<-as.numeric(as.character(data.complete$",ex$list.response,"))",sep="")))
	eval(parse(text=paste("data.complete$",ex$list.factor,"<-factor(data.complete$",ex$list.factor,")",sep="")))
	}
eval(parse(text=paste("data.complete$",deparse(substitute(nested.in)),"<-factor(data.complete$",deparse(substitute(nested.in)),")",sep="")))
missing.data<-length(rownames(data))-length(rownames(data.complete))
	for (i in 1:ex$length.factor){
	eval(parse(text=paste("levs",i,"<-levels(data.complete$",ex$list.factor[i],")",sep="")))  
	eval(parse(text=paste("levs",i,"<-levs",i,"[levs",i,"!='']",sep="")))
	}
max.levs<-NULL
	for (i in 1:length(ex$list.factor))
  	eval(parse(text=paste("max.levs<-max(max.levs,length(levs",i,"))",sep="")))
formula.cov<-paste(deparse(substitute(formula)))
	if (ex$length.covariate!=0){
	formula.cov<-paste(deparse(substitute(formula)),"+",paste(ex$list.covariate,collapse="+"))
	formula.nested.in<-paste(formula.nested.in,"+",paste(ex$list.covariate,collapse="+"))
	}
formula.nested<-paste(formula.cov,"+",deparse(substitute(nested.in)),"%in%",deparse(substitute(nested.factor)))
.Anova.nested<-lm(formula.nested,data=data.complete)
eval(parse(text=paste("emm<-marg.true.Means.nested(~",ex$list.factor[num.variable[1]],"*",ex$list.factor[num.variable[2]],
",fm=.Anova.nested,nested.factor=",deparse(substitute(nested.factor)),",nested.in=",deparse(substitute(nested.in)),")",sep="")))
	if (is.null(ylim)) yrange<-c(min(emm$Adj.mean - emm$se), max(emm$Adj.mean + emm$se))
	else yrange <-  ylim
levs.1 <- levels(emm[,ex$list.factor[num.variable[1]]])
levs.2 <- levels(emm[,ex$list.factor[num.variable[2]]])
n.levs.1 <- length(levs.1)
n.levs.2 <- length(levs.2)
response <- emm$Adj.mean
factor <-emm[,ex$list.factor[num.variable[1]]]
subfactor <-emm[,ex$list.factor[num.variable[2]]]
std.error<- emm$se
adj.means<-tapply(response, list(factor,subfactor), function(x) x)
adj.se<-tapply(std.error, list(factor,subfactor), function(x) x)	
	if (!plot.nested){
		if (is.null(xlab)) xlab<-ex$list.factor[num.variable[1]]
		if (is.null(ylab)) ylab<-paste(ex$list.response,"Mean")
		if (is.null(legend.lab)) legend.lab<-ex$list.factor[num.variable[2]]
	par(mar=c(5,4,0,0))
	layout(matrix(c(1,2,1,2),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)  
	plot(c(1, n.levs.1),yrange,type = "n",xlab=xlab,ylab=ylab,axes=FALSE)
	box()
	axis(2)
	axis(1, at = 1:n.levs.1, labels = levs.1)
		for (i in 1:n.levs.2) {
			for (j in 1:n.levs.1){
			points(j,adj.means[j,i],type="l",cex=2,col=col[i],lty=1)
			arrows(j,adj.means[j,i]-adj.se[j,i],j,adj.means[j,i]+adj.se[j,i],angle=90,code=3,col=col[i],lty=1,length=0.125)
			}
			for (j in 1:(n.levs.1-1))
			segments(j,adj.means[j,i],j+1,adj.means[j+1,i],col=col[i],lty=1)	
		}
	par(mar=c(0,0,0,0))
	plot(c(0,1),type="n",axes=F)
	legend("center",c(legend.lab,levs.2),bty="n",col=c(0,col),lty =c(1,1))
	par(mar=c(5,4,4,2))	
	}
	else{
	eval(parse(text=paste("emm_n<-marg.true.Means.nested(~",deparse(substitute(nested.in)),",fm=lm(",formula.nested,",data=data.complete),nested.factor=",deparse(substitute(nested.factor)),",nested.in=",deparse(substitute(nested.in)),")",sep="")))
	nested.factor<-unlist(strsplit(deparse(substitute(nested.factor)),":"))
		if (length(nested.factor)>1)
		eval(parse(text=paste0("data.complete$",nested.factor[1:(length(nested.factor)-1)],"<-paste0(",paste0("data.complete$",nested.factor[1:(length(nested.factor)-1)],",'.')"))))
	eval(parse(text=paste0("factor.name<-paste0(",paste0("data.complete$",nested.factor,collapse=","),")")))
	factor.name<-levels(as.factor(factor.name))
	eval(parse(text=paste0("emm_n$'",paste(nested.factor,collapse=":"),"'<-NA")))
		for (i in 1:length(emm_n[,1]))
		eval(parse(text=paste0("emm_n$'",paste(nested.factor,collapse=":"),"'[",i,"]<-factor.name[stri_detect_fixed(emm_n[",i,",1], factor.name)]")))
	eval(parse(text=paste0("levs.1<-levels(data.complete$",deparse(substitute(nested.in)),")")))
	levs.2 <- factor.name
	n.levs.1 <- length(levs.1)
	n.levs.2 <- length(levs.2)
	response <- emm_n$mean
	factor <-emm_n[,deparse(substitute(nested.in))]
	subfactor <-emm_n[,ncol(emm_n)]
	std.error<- emm_n$se
	adj.means<-tapply(response, list(factor,subfactor), function(x) x)
	adj.se<-tapply(std.error, list(factor,subfactor), function(x) x)
	eval(parse(text=paste0("adj.values<-cbind(",paste0("'mean.",colnames(adj.means),"'=adj.means[,",1:n.levs.2,"],'se.",colnames(adj.se),"'=adj.se[,",1:n.levs.2,"]",collapse=","),")")))
	valeur.min<-adj.means-adj.se
	valeur.max<-adj.means+adj.se
	eval(parse(text=paste0("min.means<-min(",paste0("na.omit(valeur.min[,",1:n.levs.2,"])",collapse=","),")")))
	eval(parse(text=paste0("max.means<-max(",paste0("na.omit(valeur.max[,",1:n.levs.2,"])",collapse=","),")")))
		if (is.null(ylim)) ylim<-c(min.means,max.means)
	yrange <-  ylim
	eval(parse(text=paste0("adj.mse<-adj.values[order(",paste0("adj.means[,",1:n.levs.2,"]",collapse=","),"),]")))
	eval(parse(text=paste0("adj.means<-adj.mse[,c(",paste0("'mean.",colnames(adj.means),"'",collapse=","),")]")))
	eval(parse(text=paste0("adj.se<-adj.mse[,c(",paste0("'se.",colnames(adj.se),"'",collapse=","),")]")))
		if (is.null(xlab)) xlab <- deparse(substitute(nested.in))
		if (is.null(ylab)) ylab <- paste(ex$list.response,"Mean")
		if(!is.null(main)) textmain<-main else textmain<-""
		if (is.null(legend.lab)) legend.lab<-paste(nested.factor,collapse=":")
	par(mar=c(5,4,0,0))
	layout(matrix(c(1,2,1,2),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)  
	plot(c(1, n.levs.1),yrange,type = "n",xlab=xlab,ylab=ylab,axes=FALSE)
	box()
	axis(2)
	axis(1, at = 1:n.levs.1, labels =  names(levs.1))
		for (i in 1:n.levs.2) {
			for (j in 1:n.levs.1){
			points(j,adj.means[j,i],type="l",cex=2,col=col[i],lty=1)
			arrows(j,adj.means[j,i]-adj.se[j,i],j,adj.means[j,i]+adj.se[j,i],angle=90,code=3,col=col[i],lty=1,length=0.125)
			}
			for (j in 1:(n.levs.1-1))
			segments(j,adj.means[j,i],j+1,adj.means[j+1,i],col=col[i],lty=1)	
		}
	par(mar=c(0,0,0,0))
	plot(c(0,1),type="n",axes=F)
	legend("center",c(legend.lab,levs.2),bty="n",col=c(0,col),lty =c(1,1))
	par(mar=c(5,4,4,2))		
	}

  invisible(NULL)
}

############################################################################
#Representation Anova trois facteurs
plot.Anova.three<-function (formula,covariate=NULL,num.variable=c(1,2,3),data=parent.frame(),xlab = NULL,sub=NULL,ylab = NULL,ylim=NULL,las=1,cex.axis=1,
legend.lab=NULL,legend.pos=NULL, col = palette())
{
 f <- formula(formula)
 listvar <- as.character(attr(terms(f), "variables"))[-1]
 covariate<-paste("factor~",deparse(substitute(covariate)))
	if (covariate!="factor~ NULL"){
    cov<-formula(covariate)
    listcov <- as.character(attr(terms(cov), "variables"))[-1]
    listcov<-listcov[2:length(listcov)]
	}
	if (covariate=="factor~ NULL") listcov<-NULL
factorname<-listvar[2:length(listvar)]
responsename<-listvar[1]
data.plot<-data
data.complete<-eval(parse(text=paste("na.omit(with(data.plot,as.data.frame(cbind(",responsename,",",paste(factorname,"=as.character(",factorname,")",sep="",collapse=","),"))))",sep="")))
eval(parse(text=paste("data.complete$",responsename,"<-as.numeric(as.character(data.complete$",responsename,"))",sep="")))
eval(parse(text=paste("data.complete$",factorname,"<-factor(data.complete$",factorname,")",sep="")))
.Anova<-lm(formula, data=data)
formula.cov<-paste(deparse(substitute(formula)))
	if (covariate!="factor~ NULL"){
	data.complete<-eval(parse(text=paste("na.omit(with(data.plot,as.data.frame(cbind(",responsename,",",
	paste(factorname,"=as.character(",factorname,")",collapse=",",sep=""),",",paste(listcov,"=as.character(",listcov,")",collapse=",",sep=""),"))))",sep="")))
	eval(parse(text=paste("data.complete$",listcov,"<-as.numeric(as.character(data.complete$",listcov,"))",sep="")))
	eval(parse(text=paste("data.complete$",responsename,"<-as.numeric(as.character(data.complete$",responsename,"))",sep="")))
	eval(parse(text=paste("data.complete$",factorname,"<-factor(data.complete$",factorname,")",sep="")))
	formula.cov<-paste(deparse(substitute(formula)),"+",paste(listcov,collapse="+"))
	}
.Anova.cov<-lm(formula.cov,data=data)
emm<-eval(parse(text=paste("marg.true.Means(~",factorname[num.variable[1]],"*",factorname[num.variable[2]],"*",factorname[num.variable[3]],",fm=.Anova.cov)",sep="")))
	if (is.null(ylim)) yrange<-c(min(emm$Adj.mean - emm$se), max(emm$Adj.mean + emm$se))
	else yrange <-  ylim
levs.1 <- levels(emm[,factorname[num.variable[1]]])
levs.2 <- levels(emm[,factorname[num.variable[2]]])
levs.3 <- levels(emm[,factorname[num.variable[3]]])
n.levs.1 <- length(levs.1)
n.levs.2 <- length(levs.2)
n.levs.3 <- length(levs.3)
eval(parse(text=paste0("emm.",1:n.levs.3,"<-subset(emm,emm$",factorname[num.variable[3]],"=='",levs.3,"')")))
	for (i in 1:n.levs.3){
	eval(parse(text=paste0("response <- emm.",i,"$Adj.mean")))
	eval(parse(text=paste0("factor <-emm.",i,"[,'",factorname[num.variable[1]],"']")))
	eval(parse(text=paste0("subfactor <-emm.",i,"[,'",factorname[num.variable[2]],"']")))
	eval(parse(text=paste0("std.error<- emm.",i,"$se")))
	eval(parse(text=paste0("adj.means",i,"<-tapply(response, list(factor,subfactor), function(x) x)")))
	eval(parse(text=paste0("adj.se",i,"<-tapply(std.error, list(factor,subfactor), function(x) x)")))
	}
	if(is.null(xlab)) xlab<-paste(factorname[num.variable[1]],"\n")
	if(is.null(sub)) sub<-paste(factorname[num.variable[3]],"=",levs.3)
	if(is.null(ylab)) ylab<-paste(responsename,"Mean")
	if(is.null(ylim)) yrange<-c(min(emm$Adj.mean - emm$se), max(emm$Adj.mean + emm$se))
	else yrange<-ylim
	if(is.null(legend.lab)) legend.lab<-factorname[num.variable[2]]
matrix<-c(1:(n.levs.3+1)) 
layout(matrix(matrix, ncol = n.levs.3 + 1), width = c(0.1 * n.levs.3, rep(1, n.levs.3)))
par(mar=c(7,4,0,0))
plot(yrange,type="n",ylab=ylab,axes=F,xlab="")
abline(h=par("usr")[3])
abline(h=par("usr")[4])
abline(v=par("usr")[1])
axis(2)
par(mar=c(7,0,0,0))
	for (k in 1:n.levs.3){
	plot(c(1,n.levs.1+.5),yrange,type="n",xlab=xlab,sub=sub[k],axes=FALSE)
	axis(1, at = 1:n.levs.1, labels = levs.1,las=las,cex.axis=cex.axis)
	eval(parse(text=paste0("adj.means<-adj.means",k)))
	eval(parse(text=paste0("adj.se<-adj.se",k)))
		for (i in 1:n.levs.2) {
			for (j in 1:n.levs.1){
			points(j,adj.means[j,i],type="l",cex=2,col=col[i],lty=1)
			arrows(j,adj.means[j,i]-adj.se[j,i],j,adj.means[j,i]+adj.se[j,i],angle=90,code=3,col=col[i],lty=1,length=0.125)
			}
		for (j in 1:(n.levs.1-1))
		segments(j,adj.means[j,i],j+1,adj.means[j+1,i],col=col[i],lty=1)
    abline(h=par("usr")[3])
    abline(h=par("usr")[4])   	
		}
	}
	if (is.null(legend.pos)) legend.pos<-"topright"
legend(legend.pos,c(legend.lab,levs.2),col=c(0,col),lty =c(1,1))
par(mar=c(5,4,4,2))
invisible(NULL)
}

############################################################################
#Representation moyennes marginales
plot.emm<-function (emm,num.variable=c(1,2),responsename=NULL,xlab = factorname[num.variable[1]],ylab = paste(responsename,"Mean"),ylim=c(min(emm$Adj.mean - emm$se),
max(emm$Adj.mean + emm$se)),las=1,cex.axis=1,legend.lab=factorname[num.variable[2]], col = palette())
{
factorname<-colnames(emm[1:2])
  yrange <-  ylim
  levs.1 <- levels(emm[,num.variable[1]])
  levs.2 <- levels(emm[,num.variable[2]])
  n.levs.1 <- length(levs.1)
  n.levs.2 <- length(levs.2)
  adj.means<-matrix(0,nrow=length(levels(emm[,num.variable[1]])),ncol=length(levels(emm[,num.variable[2]])),dimnames=list(levels(emm[,num.variable[1]]),levels(emm[,num.variable[2]])))
  adj.se<-matrix(0,nrow=length(levels(emm[,num.variable[1]])),ncol=length(levels(emm[,num.variable[2]])),dimnames=list(levels(emm[,num.variable[1]]),levels(emm[,num.variable[2]])))
	for (i in 1:length(rownames(adj.means))){
  		for (j in 1:length(colnames(adj.means))){
    	index<-paste(rownames(adj.means)[i],colnames(adj.means)[j])==paste(emm[,num.variable[1]],emm[,num.variable[2]])
    	adj.means[i,j]<-emm[index,3]
	adj.se[i,j]<-emm[index,4]
   	 }
	}  

  par(mar=c(5,4,0,0))
  layout(matrix(c(1,2,1,2),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)  
  plot(c(1, n.levs.1),yrange,type = "n",xlab=xlab,ylab=ylab,axes=FALSE)
  box()
  axis(2)
  axis(1, at = 1:n.levs.1, labels = levs.1,las=las,cex.axis=cex.axis)
  for (i in 1:n.levs.2) {
    for (j in 1:n.levs.1){
      points(j,adj.means[j,i],type="l",cex=2,col=col[i],lty=1)
      arrows(j,adj.means[j,i]-adj.se[j,i],j,adj.means[j,i]+adj.se[j,i],angle=90,code=3,col=col[i],lty=1,length=0.125)
    }
    for (j in 1:(n.levs.1-1))
      segments(j,adj.means[j,i],j+1,adj.means[j+1,i],col=col[i],lty=1)	
  }
  par(mar=c(0,0,0,0))
  plot(c(0,1),type="n",axes=F)
  legend("center",c(legend.lab,levs.2),bty="n",col=c(0,col),lty =c(1,1))
  par(mar=c(5,4,4,2))
  invisible(NULL)

}
############################################################################
# Representation erreur standard
Dispersion.graph<-function (formula,data=parent.frame(),xlim=NULL,xlab=NULL,ylab="Density",deviation=c("se","sd"), colour=c("fill","hatched"),col=palette(),
legend.pos="topright",main=NULL,legend.text=NULL)
{
#extraction des facteurs
eval(parse(text=paste0("ex<-Extract.fact(",deparse(substitute(formula)),",data=",paste0(deparse(substitute(data)),collapse=""),")")))
	#Un seul groupe
	if(ex$length.factor==1){
	valid <- complete.cases(with(data,eval(parse(text=ex$list.response))),with(data,eval(parse(text=ex$list.factor))))
	resp <- with(data,eval(parse(text=ex$list.response)))[valid]
		if (!is.numeric(resp))
		stop(gettext("Variable1 must be numeric.",domain="R-RcmdrPlugin.TestGraph"))
	fact <- with(data,eval(parse(text=ex$list.factor)))[valid]
	fact<-factor(fact)
		if (!is.factor(fact))
		stop(gettext("Variable2 must be factor.",domain="R-RcmdrPlugin.TestGraph"))
	levs<-levels(fact)
	col.line<-col
		if(colour=="fill"){
		col<-col2rgb(col)
		color<-rgb(col[1,],col[2,],col[3,],alpha=255*.4,max=255)
		}
		if(colour=="hatched") color=col
	mean <- tapply(resp, fact, mean)
	n<-tapply(resp,fact,function(x) length(x))
	sd <- tapply(resp, fact, sd)
		if (deviation=="se") dev<-sd/sqrt(n)
		if (deviation=="sd") dev<-sd
	supx<-max(mean)
	infx<-min(mean)
	supy<-max(dnorm(mean,m=mean,sd=dev))
	xmin <- qnorm(.00009,m=infx,sd=max(dev))
	xmax <- qnorm(.9999,m=supx,sd=max(dev))
	ylim<-c(0,supy)
		if(is.null(xlim)) xlim<-c(xmin,xmax)
		if(is.null(xlab)) xlab<-ex$list.response
	x <- seq(qnorm(.00009,m=infx,sd=max(dev)),qnorm(.9999,m=supx,sd=max(dev)),by=qnorm(.51,m=supx,sd=max(dev))-qnorm(.5,m=supx,sd=max(dev)))
		if (is.null(main)){
			if (deviation=="se")  main <- "Standard error representation"
			if (deviation=="sd")  main <- "Standard deviation representation"
		}
		plot(xlim,c(0, supy),xlab=xlab,ylab=ylab,type="n", axes=FALSE, main = main)
		for (i in 1:length(n)){
			if(colour=="fill") 
			polygon(c(qnorm(.00009,m=mean[i],sd=dev[i]),qnorm(.00009,m=mean[i],sd=dev[i]),x,qnorm(.9999,m=mean[i],sd=dev[i])),
			c(0,dnorm(qnorm(.00009,m=mean[i],sd=dev[i]),m=mean[i],sd=dev[i]),dnorm(x,m=mean[i],sd=dev[i]),0),col=color[i],border=NA)
			if(colour=="hatched") 
			polygon(c(qnorm(.00009,m=mean[i],sd=dev[i]),qnorm(.00009,m=mean[i],sd=dev[i]),x,qnorm(.9999,m=mean[i],sd=dev[i])),
			c(0,dnorm(qnorm(.00009,m=mean[i],sd=dev[i]),m=mean[i],sd=dev[i]),dnorm(x,m=mean[i],sd=dev[i]),0),col=color[i],border=NA,density=c(10,20),angle=45*i,lwd=1.5)
		}
		for (i in 1:length(n)){
		points(x,dnorm(x,mean=mean[i],sd=dev[i]),type="l",col="black")
		arrows(mean[i],dnorm(mean[i]+dev[i],m=mean[i],sd=dev[i]),mean[i]+dev[i],dnorm(mean[i]+dev[i],m=mean[i],sd=dev[i]), col=col.line[i],lwd=2,length=0.1)
		arrows(mean[i],dnorm(mean[i]+dev[i],m=mean[i],sd=dev[i]),mean[i]-dev[i],dnorm(mean[i]-dev[i],m=mean[i],sd=dev[i]), col=col.line[i],lwd=2,length=0.1)
		segments(mean[i],dnorm(mean[i],m=mean[i],sd=dev[i]),mean[i],0,col=col.line[i],lty=2,lwd=2)
		}
	abline(h=0,col="gray")
	box()
	axis(1)
	axis(2)
		if(is.null(legend.text)) legend.text<- paste(ex$list.factor,"=",levs)
	legend(legend.pos,fill=color,legend.text)
	}
	#groupe et sous-groupe
	if (ex$length.factor==2){
	valid <- complete.cases(with(data,eval(parse(text=ex$list.response))),with(data,eval(parse(text=ex$list.factor[1]))),with(data,eval(parse(text=ex$list.factor[2]))))
	resp <- with(data,eval(parse(text=ex$list.response)))[valid]
		if (!is.numeric(resp))
		stop(gettext("Variable1 must be numeric.",domain="R-RcmdrPlugin.TestGraph"))
	fact <- with(data,eval(parse(text=ex$list.factor[1])))[valid]
	fact<-factor(fact)
		if (!is.factor(fact))
		stop(gettext("Variable2 must be factor.",domain="R-RcmdrPlugin.TestGraph"))
	subfact <- with(data,eval(parse(text=ex$list.factor[2])))[valid]
	subfact<-factor(subfact)
		if (!is.factor(subfact))
		stop(gettext("Variable3 must be factor.",domain="R-RcmdrPlugin.TestGraph"))
	levs<-levels(fact)
	sublevs<-levels(subfact)
	col.line<-col
		if(colour=="fill"){
		col<-col2rgb(col)
		color<-rgb(col[1,],col[2,],col[3,],alpha=255*.4,max=255)
		}
		if(colour=="hatched") color=col
	mean <- tapply(resp, list(fact,subfact), mean)
	n<-tapply(resp,list(fact,subfact),function(x) length(x))
	sd <- tapply(resp, list(fact,subfact), sd)
	supx<-max(mean)
	infx<-min(mean)
		if (deviation=="se") dev<-sd/sqrt(n)
		if (deviation=="sd") dev<-sd
	supy<-max(dnorm(mean,m=mean,sd=dev))
	xmin <- qnorm(.00009,m=infx,sd=max(dev))
	xmax <- qnorm(.9999,m=supx,sd=max(dev))
	ylim<-c(0,supy)
		if(is.null(xlim)) xlim<-c(xmin,xmax)
		if(is.null(xlab)) xlab<-paste0(ex$list.response," - ",ex$list.factor[2],"=",sublevs)
	x <- seq(qnorm(.00009,m=infx,sd=max(dev)),qnorm(.9999,m=supx,sd=max(dev)),by=qnorm(.51,m=supx,sd=max(dev))-qnorm(.5,m=supx,sd=max(dev)))
	n.row<-length(sublevs)
	layout(matrix(c(1,seq(2,n.row+1)),n.row+1,1,byrow=TRUE),widths=28*n.row+4,heights=c(4,rep(28,n.row)),TRUE)
	par(mar=c(0,0,0,0))	
	plot(c(0,1),c(0,1),type="n",axes=F)
		if (is.null(main)){
			if (deviation=="se")  text(.5,.5,"Standard error representation",cex =1.5)
			if (deviation=="sd")  text(.5,.5,"Standard deviation representation",cex=1.5)
		}
		else
		text(.5,.5,main,cex=1.5)
	par(mar=c(5,4,0,2))
		for (i in 1:length(sublevs)){
		plot(xlim,c(0, supy),xlab=xlab[i],ylab=ylab,type="n", axes=FALSE, main = "")
			for (j in 1:length(n[,i])){
				if(colour=="fill")
				polygon(c(qnorm(.00009,m=mean[j,i],sd=dev[j,i]),qnorm(.00009,m=mean[j,i],sd=dev[j,i]),x,qnorm(.9999,m=mean[j,i],sd=dev[j,i])),
				c(0,dnorm(qnorm(.00009,m=mean[j,i],sd=dev[j,i]),m=mean[j,i],sd=dev[j,i]),dnorm(x,m=mean[j,i],sd=dev[j,i]),0),col=color[j],border=NA)
				if(colour=="hatched") 
				polygon(c(qnorm(.00009,m=mean[j,i],sd=dev[j,i]),qnorm(.00009,m=mean[j,i],sd=dev[j,i]),x,qnorm(.9999,m=mean[j,i],sd=dev[j,i])),
				c(0,dnorm(qnorm(.00009,m=mean[j,i],sd=dev[j,i]),m=mean[j,i],sd=dev[j,i]),dnorm(x,m=mean[j,i],sd=dev[j,i]),0),col=color[j],border=NA,density=c(10,20),angle=45*i,lwd=1.5)
			}	
			for (j in 1:length(n[,i])){
			points(x,dnorm(x,mean=mean[j,i],sd=dev[j,i]),type="l",col="black")
			arrows(mean[j,i],dnorm(mean[j,i]+dev[j,i],m=mean[j,i],sd=dev[j,i]),mean[j,i]+dev[j,i],dnorm(mean[j,i]+dev[j,i],m=mean[j,i],sd=dev[j,i]), col=col.line[j],lwd=2,length=0.1)
			arrows(mean[j,i],dnorm(mean[j,i]+dev[j,i],m=mean[j,i],sd=dev[j,i]),mean[j,i]-dev[j,i],dnorm(mean[j,i]-dev[j,i],m=mean[j,i],sd=dev[j,i]), col=col.line[j],lwd=2,length=0.1)
			segments(mean[j,i],dnorm(mean[j,i],m=mean[j,i],sd=dev[j,i]),mean[j,i],0,col=col.line[j],lty=2,lwd=2)
			}	
			abline(h=0,col="gray")
		box()
		axis(1)
		axis(2)
			if(is.null(legend.text)) 
			legend.text<- paste(ex$list.factor[i],"=",levs)
		legend(legend.pos,fill=color,legend.text) 
		}
	}
}

############################################################################
#Representation mediane################ a revoir---------------------------------------
median.graph<-function (formula,data=parent.frame(),name.variable=NULL,breaks="sturges",col=palette()[2:8],xlab = NULL,ylab=NULL,main = NULL,median.digit=3,
mean.digit=3)
{
#extraction des parametres
eval(parse(text=paste0("ex<-Extract.fact(",deparse(substitute(formula)),",data=",deparse(substitute(data)),")")))
	if (ex$length.factor==0) {
		#repetee sans groupe
		if (ex$length.response>1){
		eval(parse(text=paste0("resp.",1:ex$length.response,"<-na.omit(",deparse(substitute(data)),"$",ex$list.response,")")))
		eval(parse(text=paste0("resp<-c(",paste0("resp.",1:ex$length.response,collapse=","),")")))
		graph<-hist(resp,plot=F,breaks=breaks)
		new.breaks<-length(graph$breaks)*ex$length.response
		graph<-hist(resp,plot=F,breaks=new.breaks)
		eval(parse(text=paste("graph",1:ex$length.response,"<-hist(resp.",1:ex$length.response,",breaks=graph$breaks,plot=F)",sep="")))	
		eval(parse(text=paste0("resp.m",1:ex$length.response,"<-round(mean(",deparse(substitute(data)),"$",ex$list.response,",na.rm=TRUE),",mean.digit,")")))
		eval(parse(text=paste0("resp.d",1:ex$length.response,"<-round(median(",deparse(substitute(data)),"$",ex$list.response,",na.rm=TRUE),",median.digit,")")))
		eval(parse(text=paste0("resp.m<-c(",paste0("resp.m",1:ex$length.response,collapse=","),")")))
		eval(parse(text=paste0("resp.d<-c(",paste0("resp.d",1:ex$length.response,collapse=","),")")))
		eval(parse(text=paste0("tick.x<-c(",paste0("'mean=",resp.m,"\nmedian=",resp.d,"'",collapse=","),")")))
		lab<-pretty(graph$breaks)
		scale<-seq(0,length(graph$count),length.out =length(lab))
			if (is.null(xlab))
			xlab<-""
			if (is.null(ylab))
			ylab<-name.variable
			if (is.null(main))
			main<-""
		ratio<-(graph$breaks[length(graph$breaks)]-graph$breaks[1])
		coord.m<-(resp.d-graph$breaks[1])/ratio*length(graph$count)
		plot(c(0,ex$length.response),c(0,length(graph$count)),type='n',axe=F,ylab=ylab,xlab="",main=main)
		axis(2,scale,lab)
		axis(1,seq(0,ex$length.response-1)+0.5,ex$list.response)
		mtext(tick.x,1,line=3, at=seq(0,ex$length.response-1)+0.5,cex=0.8)
			for (i in 1:ex$length.response){
			eval(parse(text=paste0("x1<-rep(",i-1,",length(graph",i,"$count))")))
			eval(parse(text=paste0("y1<-0:(length(graph",i,"$count)-1)")))
			eval(parse(text=paste0("max.x2<-max(graph",i,"$count)")))
			eval(parse(text=paste0("x2<-0.9*graph",i,"$count/max.x2")))
			x2<-x2+i-1
			y2<-y1+1
			rect(x1,y1,x2,y2,col=col[i])
			}
		lines(1:ex$length.response-0.75,coord.m,lwd=2)
		mtext(xlab,1,line=4,at=(par("usr")[2]-par("usr")[1])/2)
		box()
		}
		else{
		
		}
	}
	if (ex$length.factor==1) {
		#Simple
		if (ex$length.response==1){
		eval(parse(text=paste0("resp<-na.omit(",deparse(substitute(data)),"$",ex$list.response,")")))
		eval(parse(text=paste0("niveaux<-levels(",deparse(substitute(data)),"$",ex$list.factor,")")))
		eval(parse(text=paste0("resp.",1:length(niveaux),"<-subset(",deparse(substitute(data)),",",ex$list.factor,"=='",niveaux,"')$",ex$list.response)))
		eval(parse(text=paste0("resp.m",1:length(niveaux),"<-round(mean(resp.",1:length(niveaux),",na.rm=TRUE),",mean.digit,")")))
		eval(parse(text=paste0("resp.d",1:length(niveaux),"<-round(median(resp.",1:length(niveaux),",na.rm=TRUE),",median.digit,")")))
		graph<-hist(resp,plot=F,breaks=breaks)
			#new.breaks<-length(graph$breaks)*ex$length.factor
			#graph<-hist(resp,plot=F,breaks=new.breaks)
		eval(parse(text=paste("graph",1:length(niveaux),"<-hist(resp.",1:length(niveaux),",breaks=graph$breaks,plot=F)",sep="")))
		eval(parse(text=paste0("resp.m<-c(",paste0("resp.m",1:length(niveaux),collapse=","),")")))
		eval(parse(text=paste0("resp.d<-c(",paste0("resp.d",1:length(niveaux),collapse=","),")")))
		eval(parse(text=paste0("tick.x<-c(",paste0("'mean=",resp.m,"\nmedian=",resp.d,"'",collapse=","),")")))
		lab<-pretty(graph$breaks)
		scale<-seq(0,length(graph$count),length.out =length(lab))
			if (is.null(xlab))
			xlab<-ex$list.factor
			if (is.null(ylab))
			ylab<-ex$list.response
			if (is.null(main))
			main<-""
		ratio<-(graph$breaks[length(graph$breaks)]-graph$breaks[1])
		coord.m<-(resp.d-graph$breaks[1])/ratio*length(graph$count)
		plot(c(0,length(niveaux)),c(0,length(graph$count)),type='n',axe=F,ylab=ylab,xlab="",main=main)
		axis(2,scale,lab)
		axis(1,seq(0,length(niveaux)-1)+0.5,niveaux)
		mtext(tick.x,1,line=3, at=seq(0,length(niveaux)-1)+0.5,cex=0.8)
			for (i in 1:length(niveaux)){
			eval(parse(text=paste0("x1<-rep(",i-1,",length(graph",i,"$count))")))
			eval(parse(text=paste0("y1<-0:(length(graph",i,"$count)-1)")))
			eval(parse(text=paste0("max.x2<-max(graph",i,"$count)")))
			eval(parse(text=paste0("x2<-0.9*graph",i,"$count/max.x2")))
			x2<-x2+i-1
			y2<-y1+1
			rect(x1,y1,x2,y2,col=col[i])
			}
		lines(1:length(niveaux)-0.75,coord.m,lwd=2)
		mtext(xlab,1,line=4,at=(par("usr")[2]-par("usr")[1])/2)
		box()
		}	
	}
invisible(NULL)
}
##################################################################################
#Representation mediane  3D
median.graph3D<-function (formula,data=parent.frame(),name.variable=NULL,breaks=11,col="turquoise",xlab = factorname,main = responsename,cex.main=1.2,cex.sub=.7,
median.digit=3,mean.digit=3)
{
f <- formula(formula)
listvar <- as.character(attr(terms(f), "variables"))[-1]
test.factor <- with(data,eval(parse(text=listvar[2])))
	#Pas de groupes
	if (is.numeric(test.factor)) {
	responsename<-listvar
	factorname<-NULL
	}
	#Un groupe
	else{
	responsename<-listvar[1]
	factorname<-listvar[2]
	}
	 #stockage donnees avec groupe
	if (!is.null(factorname)){
	valid <- complete.cases(with(data,eval(parse(text=responsename))),with(data,eval(parse(text=factorname))))
	response <- with(data,eval(parse(text=responsename)))[valid]
	if (!is.numeric(response))
	stop(gettext("Variable1 must be numeric.",domain="R-RcmdrPlugin.TestGraph"))
	factor <- with(data,eval(parse(text=factorname)))[valid]
	factor<-factor(factor)
	}
	 #Stockage des donnees sans groupe
	else{
	data$id<-1:length(rownames(data))
	dropname<-colnames(data)[colnames(data)!="id"]
	for (var in responsename) dropname<-dropname[dropname!=var]
	tmp.data<-reshape(data, idvar = "id", timevar="Parameter",varying = responsename, v.names = "Block", drop=dropname,direction = "long")
	tmp.data$Parameter<-factor(tmp.data$Parameter,labels=responsename)
	valid <- complete.cases(with(tmp.data,eval(parse(text="Block"))),with(tmp.data,eval(parse(text="Parameter"))))
	response <- with(tmp.data,eval(parse(text="Block")))[valid]
	factor <- with(tmp.data,eval(parse(text="Parameter")))[valid]
	factor<-factor(factor)
	factorname<-"Parameter"
	responsename<-"Block"
	}
  cex.main<-as.numeric(cex.main)
  means <- round(tapply(response, factor, mean),digits=mean.digit)
  medians <- round(tapply(response, factor, median),digits=median.digit)
  table<-hist(response,breaks=breaks,plot=F)
  miny<-min(table$breaks)
  maxy<-max(table$breaks)
  supx<-max(table$counts)
  scalex<-c(-2,supx+1)
  levs<-levels(factor)
  minbr<-NULL;maxbr<-NULL;lengthbr<-NULL;br<-NULL
	matrix<-c(1,1,1,2)
    for (i in 1:length(levs)){
    minbr[i]<-min(hist(subset(response,factor==levs[i]),breaks=breaks,plot=F)$breaks)
    maxbr[i]<-max(hist(subset(response,factor==levs[i]),breaks=breaks,plot=F)$breaks)
    lengthbr[i]<-length(hist(subset(response,factor==levs[i]),breaks=breaks,plot=F)$breaks)
    if (maxbr[i]>.00005)br[i]<-(maxbr[i]-minbr[i])/lengthbr[i]
    else br[i]<-maxy
    matrix<-c(matrix,rep(i+2,3),2)
		}
  barres<-(seq(miny,maxy+min(br),min(br)))
  zero<-rep(0,length(barres))
  ticks<-pretty(barres)
  scaley<-length(barres)*(ticks-min(barres))/(max(barres)-min(barres))
	draw1<-NULL;draw2<-NULL
	for (i in 1:(length(levs)-1)){
  draw1[i]<-length(zero)*(medians[i]-min(barres))/(max(barres)-min(barres))
  draw2[i]<-length(zero)*(medians[i+1]-min(barres))/(max(barres)-min(barres))
	}
	draw1<-c(draw1,0);draw2<-c(draw2,0)
  height<-c(5,rep(1,length(levs)))
  supscale<-c(rep(supx,length(levs)-1),0)
  n.col<-length(levs)+1
	layout(matrix(matrix,ncol=n.col),widths=c(.1*length(levs),1,1,1),heights=height)
	par(mar=c(5,3,3,0))
	barplot(zero,axe=F,horiz=TRUE,space=0,border="white")
	axis(2,scaley,ticks)
	axis(1,scalex)
	par(mar=c(0,0,0,0))
	plot(c(0,10),c(0,1),type='n',axe=F)
	text(5,.5,xlab,cex=1.2)
	par(mar=c(5,0,3,0))
	for (i in 1:length(levs)){
	barplot3D(hist(subset(response,factor==levs[i]),breaks=breaks,plot=F)[[2]],
	space=0,horiz=TRUE,axe=FALSE,ylim=c(0,length(zero)),
	xlim=c(0,supx),xlab=paste("median =",medians[i]),col=col,
	sub=paste("mean =",means[i]))
	title(main=main,line=1,cex.main=cex.main)
      if (factorname!="") mtext(paste(factorname,"=",levs[i]),cex=cex.sub)
	else mtext(levs[i],cex=cex.sub)
	segments(0,draw1[i],supscale[i],draw2[i])
	axis(1,scalex)
	}
    invisible(NULL)
}

############################################################################
#Ellipse pour dessin lattice
panel.ellipse<-function (x,y,alpha = plot.line$alpha, col = plot.line$col,lty = plot.line$lty,
lwd = plot.line$lwd,level=0.683,segments=51, ...)
{
    plot.line <- trellis.par.get("plot.line")
    valid<-complete.cases(x,y)
    varx<-x[valid]
    vary<-y[valid]
    shape <- var(cbind(varx, vary))
    center <- c(mean(varx), mean(vary))
    dfd <- length(varx) - 1
    radius <- sqrt(2 * qf(level, 2, dfd))
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    ellipse <- t(center + radius * t(unit.circle %*% chol(shape)))
    panel.xyplot(x,y,col = col, lty = lty, lwd = lwd, alpha = alpha)
    panel.lines(ellipse, col = col, lty = lty, lwd = lwd, alpha = alpha)
}
####################################################################################
#Regression pour dessin lattice
panel.regression<-function (x,y,alpha = plot.line$alpha, col = plot.line$col,lty = plot.line$lty,
lwd = plot.line$lwd,level=0.683,segments=51, ...)
{
    plot.line <- trellis.par.get("plot.line")
    valid<-complete.cases(x,y)
    varx<-x[valid]
    vary<-y[valid]
    xlim<-range(varx)
    coef<-lm(vary~varx)
    coef<-coef$coefficients
    xx<-seq(xlim[1],xlim[2],length.out=200)
    yy<-coef[2]*xx+coef[1]
    panel.xyplot(x,y,col = col, lty = lty, lwd = lwd, alpha = alpha)
    panel.lines(xx,yy, col = col, lty = lty, lwd = 2, alpha = alpha)
}
############################################################################
# Affichage puissance
power.display<-function(n1=NULL,p1=NULL,p2=NULL,delta=NULL,sigma=NULL,sig.level=0.05,
type = c("two.sample", "one.sample", "paired")
,alternative = c("two.sided", "less", "greater")){
if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > sig.level | sig.level > 1)) 
stop(sQuote("sig.level"), " must be numeric in [0, 1]")
alternative <- match.arg(alternative)
tside <- switch(alternative, less = 1, two.sided = 2, greater = 1)
zalpha<-qnorm(1-sig.level/tside)
type <- match.arg(type)
tsample <- switch(type, one.sample = 1, two.sample = 2, paired = 3) 
type.test<-1
if (is.null(p1) & is.null(p2)) type.test<-2
if (type.test==1){
eval(parse(text=paste("title<-expression(paste('For ',p[1],' = ',",p1,",' and ',p[2],' = ',",p2,"))",sep="")))
difference<- 4*(asin(sqrt(p1)) - asin(sqrt(p2)))^2
zbeta<-qnorm(seq(.95,.01,by=-.001))
frac1<-(zalpha-zbeta)^2
power<-1-pnorm(zbeta)
z80<-qnorm(.2)
frac80<-(zalpha-z80)^2
if (!is.null(n1)){
k<-difference/frac1*n1-1
k80<-difference/frac80*n1-1
n80<-n1/k80
n2<-n1/k
power<-power[n2>0 & n2<1500]
n2<-n2[n2>0 & n2<1500]
length<-n2>min(n2)
ini<-grep(FALSE,length)
sample<-n2[ini:length(n2)]
power<-power[ini:length(n2)]
xlab<-paste("Two samples with size 1 =",n1)

}
if (is.null(n1)){
n2<-2*frac1/difference
n80<-2*frac80/difference
length<-n2>min(n2)
ini<-grep(FALSE,length)
sample<-n2[ini:length(n2)]
power<-power[ini:length(n2)]
xlab<-paste("Two samples with same size")
}
plot(sample,power,type="l",lwd=2,xlab=xlab,ylab="Power",col="blue",main=title)
if(n80>0){
segments(par("usr")[1],.8,n80,.8,lty=2)
text(n80,.8,paste("n =",ceiling(n80)),pos=4)
}
op<-par(new=TRUE,fig=c(0.50,1, 0,.5))
barplot(c(p1,p2),axes=FALSE,col="turquoise")
text(.7,p1/2,expression(p[1]))
text(1.9,p2/2,expression(p[2]))
par(op)
}
if (type.test==2){
eval(parse(text=paste("title<-expression(paste('For ',Delta,' = ',",delta,",' and ',sigma,' = ',",sigma,"))",sep="")))
difference<- (sigma/delta)^2
zbeta<-qnorm(seq(.95,.01,by=-.001))
frac1<-(zalpha-zbeta)^2
power<-1-pnorm(zbeta)
z80<-qnorm(.2)
frac80<-(zalpha-z80)^2
if (!is.null(n1)){
k<-difference*frac1/(n1-difference*frac1)
k80<-difference*frac80/(n1-difference*frac80)
n80<-n1*k80
n2<-n1*k
xlab<-paste("Two samples with size 1 =",n1)
power<-power[n2>0 & n2<1500]
n2<-n2[n2>0 & n2<1500]
length<-n2>min(n2)
ini<-grep(FALSE,length)
sample<-n2[ini:length(n2)]
power<-power[ini:length(n2)]

}
if (is.null(n1)){
n2<-2*frac1*difference
n80<-2*frac80*difference
xlab<-paste("Two samples with same size")
if (tsample==1) xlab<-"One sample, comparison with a mean"
if (tsample==3) xlab<-"Sample size for paired sample"
sample<-n2
if (tsample==1 | tsample==3) {
sample<-n2/2
n80<-n80/2
}
}
plot(sample,power,type="l",lwd=2,xlab=xlab,ylab="Power",col="blue",main=title)
if(n80>0){
segments(par("usr")[1],.8,n80,.8,lty=2)
text(n80,.8,paste("n =",ceiling(n80)),pos=4)
}
op<-par(new=TRUE,fig=c(0.50,1, 0,.5))
x1<-qnorm(seq(.001,.999,by=.001),sd=sigma)
x2<-qnorm(seq(.001,.999,by=.001),mean=delta,sd=sigma)
y1<-dnorm(x1,sd=sigma)
y2<-dnorm(x2,mean=delta,sd=sigma)
plot(x1,y1,type="l",col="blue",axes=FALSE,xlab="",ylab="",xlim=c(min(x1),max(x2)))
box()
lines(x2,y2,col="red")
segments(qnorm(.5,sd=sigma),0,qnorm(.5,sd=sigma),dnorm(qnorm(.5,sd=sigma),sd=sigma),col="blue")
segments(qnorm(.5,mean=delta,sd=sigma),0,qnorm(.5,mean=delta,sd=sigma),dnorm(qnorm(.5,mean=delta,sd=sigma),mean=delta,sd=sigma),col="red")
length.arrow<-.4*par()$pin[1]*delta/diff(par()$usr[1:2])
arrows(0,max(y1)/2,delta,max(y2)/2,code=3,length=length.arrow)
text(delta/2,max(y1)/2,expression(Delta),pos=1)
par(op)
}
} 
############################################################################
#fonction supplementaire panel.qqp (envelope)
panel.qqp <-function(x, distribution = qnorm, density = dnorm,envelope = 0.95,line = c("quartiles", "robust", "none"),...)
{
line <- match.arg(line)
good <- is.finite(x)
ord <- order(x[good])
ord.x <- x[good][ord]
q.function <- distribution
d.function <- density
n <- length(ord.x)
P <- ppoints(n)
z <- q.function(P)
panel.points(z, ord.x, ...)
if (line == "quartiles") {
Q.x <- quantile(ord.x, c(0.25, 0.75))
Q.z <- q.function(c(0.25, 0.75))
b <- (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1])
a <- Q.x[1] - b * Q.z[1]
panel.abline(a, b, ...)
}
if (line == "robust") {
if (!require("MASS"))
stop("MASS package not available")
coef <- coefficients(rlm(ord.x ~ z))
a <- coef[1]
b <- coef[2]
panel.abline(a, b, ...)
}
if (line != "none" & envelope != FALSE) {
zz <- qnorm(1 - (1 - envelope)/2)
SE <- (b/d.function(z)) * sqrt(P * (1 - P)/n)
fit.value <- a + b * z
upper <- fit.value + zz * SE
lower <- fit.value - zz * SE
panel.lines(z, upper, ...)
panel.lines(z, lower, ...)
}
}
############################################################################
# Dessin de tableau
graphTable<-function(tab,title.pos=c("top","left"),cex=1,x.intersp=0,y.intersp=1,left=par("usr")[1],top=par("usr")[4]){
n.row<-dim(tab)[1]
n.col<-dim(tab)[2]
	if (length(title.pos)==2) title.pos<-"top"
	if (title.pos=="top"){
	title.row<-legend('top',names(dimnames(tab))[1],x.intersp=x.intersp,y.intersp=y.intersp,bty="n",cex=cex)
	title.col<-legend('top',names(dimnames(tab))[2],x.intersp=x.intersp,y.intersp=y.intersp,bty="n",plot=F,cex=cex)
	ligne<-title.row$rect$top-title.row$rect$h
	legend(title.col$rect$left,ligne,names(dimnames(tab))[2],x.intersp=x.intersp,y.intersp=y.intersp,bty="n",cex=cex)
	ligne<-ligne-title.col$rect$h
	tableau<-rbind(colnames(tab),tab)
	tableau<-cbind(c("",rownames(tab)),tableau)
	coord<-legend("top",tableau,x.intersp=x.intersp,y.intersp=y.intersp,bty="n",plot=F,ncol=n.col+1)
	gauche<-coord$rect$left
	top.row<-ligne
	#Titre des lignes
	max.width<-0
		for (i in 1:(n.row+1)){
		width.row<-legend(gauche,top.row,rownames(tableau)[i],bty="n",x.intersp=x.intersp,y.intersp=y.intersp,cex=cex)
		top.row<-top.row-width.row$rect$h
		max.width<-max(max.width,width.row$rect$w)
		}
	#titre des colonnes et remplissage tableau
	top.row<-ligne
	gauche<-coord$rect$left+max.width
		for (i in 1:n.col){
		width.col<-legend(gauche,top.row,c(colnames(tab)[i],tab[,i]),bty="n",x.intersp=x.intersp,y.intersp=y.intersp,plot=F,cex=cex)
		legend(gauche,top.row,colnames(tab)[i],bty="n",x.intersp=x.intersp,y.intersp=y.intersp,cex=cex)
		top.row<-top.row-width.row$rect$h
			for (j in 1:n.row){
			text(gauche+width.col$rect$w/2,top.row-width.row$rect$h/2,tab[j,i],cex=cex)
			rect(gauche,top.row-width.row$rect$h,gauche+width.col$rect$w,top.row)
			top.row<-top.row-width.row$rect$h
			}
		gauche<-gauche+width.col$rect$w
		top.row<-ligne
		}
	}
	if (title.pos=="left"){
	milieu<-(par("usr")[2]-par("usr")[1])/2
	title.width<-legend(par("usr")[1],milieu,names(dimnames(tab))[1],x.intersp=x.intersp,y.intersp=y.intersp,plot=F,bty="n",cex=cex)
	title.width<-title.width$rect$left+title.width$rect$w
	text(par("usr")[1],milieu,names(dimnames(tab))[1],pos=4,cex=cex)
	title.col<-legend('top',names(dimnames(tab))[2],x.intersp=x.intersp,y.intersp=y.intersp,bty="n",plot=F,cex=cex)
	ligne<-par("usr")[2]-title.col$rect$h/2
	text((par("usr")[2]-title.width)/2+title.width,par("usr")[2],adj=0.5,pos=1,names(dimnames(tab))[2],cex=cex)
	tableau<-rbind(colnames(tab),tab)
	tableau<-cbind(c("",rownames(tab)),tableau)
	gauche<-title.width
	top.row<-ligne
	#titre des lignes
		max.width<-0
		for (i in 1:(n.row+1)){
		width.row<-legend(gauche,top.row,rownames(tableau)[i],bty="n",x.intersp=x.intersp,y.intersp=y.intersp,cex=cex)
		top.row<-top.row-width.row$rect$h
		max.width<-max(max.width,width.row$rect$w)
		}
		#titre des colonnes et remplissage tableau
	top.row<-ligne
	gauche<-title.width+max.width
		for (i in 1:n.col){
		width.col<-legend(gauche,top.row,c(colnames(tab)[i],tab[,i]),bty="n",x.intersp=x.intersp,y.intersp=y.intersp,plot=F,cex=cex)
		legend(gauche,top.row,colnames(tab)[i],bty="n",x.intersp=x.intersp,y.intersp=y.intersp,cex=cex)
		top.row<-top.row-width.row$rect$h
			for (j in 1:n.row){
			text(gauche+width.col$rect$w/2,top.row-width.row$rect$h/2,tab[j,i],cex=cex)
			rect(gauche,top.row-width.row$rect$h,gauche+width.col$rect$w,top.row)
			top.row<-top.row-width.row$rect$h
			}
		gauche<-gauche+width.col$rect$w
		top.row<-ligne
		}
	
	}

}
########################################################################
#Affichage data frame
plot.data.frame<-function(frame,top=par("usr")[4],cex=1,mar=c(0,0,0,0)){
nbre.col<-ncol(frame)
par(mar=mar)
plot(c(0,10),c(0,10),axes=FALSE,type="n",xlab="",ylab="")
left<-par("usr")[1]
left.good<-NULL;cex.good<-NULL;length.row<-NULL
for (cex.test in seq(cex,.5,by=-.05)){
pos<-paste("legend(left,top,title=expression(bold('",names(frame)[c(1)],"')),",paste("c(",paste("'",frame[[1]],"'",collapse=",",sep=""),")"),",x.intersp=0,
bty='n',cex=cex.test,plot=F)",sep="")
pos<-eval(parse(text=pos))
for (i in 2:length(frame)){
left<-left+pos$rect$w
pos<-paste("legend(left,top,title=expression(bold('",names(frame)[c(i)],"')),",paste("c(",paste("'",frame[[i]],"'",collapse=",",sep=""),")"),",x.intersp=0,
bty='n',cex=cex.test,plot=F)",sep="")
pos<-eval(parse(text=pos))
}
left<-left+pos$rect$w
left<-par("usr")[1]+(par("usr")[2]-left)/2
	if (left>0){
	left.good<-c(left.good,left)
	cex.good<-c(cex.good,cex.test)
	length.row<-c(length.row,length(pos$text$y[pos$text$y>0]))
	}
left<-par("usr")[1]
}
left<-min(left.good)
left1<-min(left.good)
cex.good<-max(cex.good)
page<-ceiling(length(frame[[1]])/length.row[left.good==left])
length.frame<-length.row[left.good==left]-1
row1<-1
row2<-length.frame
row.end<-length(frame[[1]])
pos<-paste("legend(left,top,title=expression(bold('",names(frame)[c(1)],"')),",paste("c(",paste("'",frame[[1]]
,"'",collapse=",",sep=""),")"),",x.intersp=0,
bty='n',cex=cex.good)",sep="")
pos<-eval(parse(text=pos))
	for (i in 2:length(frame)){
	left<-left+pos$rect$w
	pos<-paste("legend(left,top,title=expression(bold('",names(frame)[c(i)],"')),",paste("c(",paste("'",frame[[i]]
,"'",collapse=",",sep=""),")"),",x.intersp=0,
	bty='n',cex=cex.good)",sep="")
	pos<-eval(parse(text=pos))
	}
if (page>1){
	left<-left1
	row1<-row2+1
	row2<-min(row1+length.frame,row.end)
for (row in 2:page){
	x11()
	par(mar=mar)
	plot(c(0,10),c(0,10),axes=FALSE,type="n",xlab="",ylab="")
pos<-paste("legend(left,top,title=expression(bold('",names(frame)[c(1)],"')),",paste("c(",paste("'",frame[[1]][row1:row2]
,"'",collapse=",",sep=""),")"),",x.intersp=0,
bty='n',cex=cex.good)",sep="")
pos<-eval(parse(text=pos))
	for (i in 2:length(frame)){
	left<-left+pos$rect$w
	pos<-paste("legend(left,top,title=expression(bold('",names(frame)[c(i)],"')),",paste("c(",paste("'",frame[[i]][row1:row2]
,"'",collapse=",",sep=""),")"),",x.intersp=0,
	bty='n',cex=cex.good)",sep="")
	pos<-eval(parse(text=pos))
	}
	left<-left1
	row1<-row2+1
	row2<-min(row1+length.frame,row.end)
}
}
}
##################################################################################
#Correlations et histogrammes
graphCorHisto<-function (formula,data=parent.frame(),legend=NULL,cex.legend=1, shapiro=TRUE,cex.shapiro=1,normale=TRUE,cex.calc=1,confidence=0.95,pearson=TRUE,rho=TRUE,R2=TRUE,
col.point=palette()[1],col.line=palette()[2],col.hist=palette()[3])
{
require(car)
test.formula<-deparse(substitute(formula))
test.formula<-unlist(strsplit(test.formula,"\\+"))
test.formula<-unlist(strsplit(test.formula,"\\ "))
listvar<-NULL
	for (i in 1:length(test.formula))
		if (nchar(test.formula[i])!=0) listvar<-c(listvar,test.formula[i])
		n.row<-length(listvar)
n.col<-n.row
nf<-layout(matrix(seq(1,(n.row+1)*(n.col+1)),n.row+1,n.col+1,byrow=T),c(4/28,rep(1,n.col)),c(rep(1,n.row),4/28),TRUE)
par(mar=c(0,0,0,0))
	for (i in 1:n.row){
	plot(c(0,1),c(0,1),type="n",axes=F,ylab="",xlab="")
		if(length(legend)==0) mtext(listvar[i],4,line=-1.5,cex=cex.legend)
			if (length(legend)!=0) mtext(legend[i],4,line=-1.5,cex=cex.legend)
				for (j in 1:n.col){
				valid <- complete.cases(with(data,eval(parse(text=listvar[j]))),with(data,eval(parse(text=listvar[i]))))
				varx <- with(data,eval(parse(text=listvar[i])))[valid]
				vary <- with(data,eval(parse(text=listvar[j])))[valid]
					if (i==j){
					test<-shapiro.test(varx[!is.na(varx)])
						if(test$p.value<0.001) test<-"p<0.001"
						else test<-paste("p=",round(test$p.value,3),sep="")
					normx<-seq(min(na.omit(varx)),max(na.omit(varx)),length=500)
					densityx<-dnorm(normx,mean(na.omit(varx)),sd(na.omit(varx)))
					hist(varx,freq=FALSE,xlab="",ylab="",col=col.hist,main="",axes=FALSE,ylim=c(0,max(densityx)))
						if(normale) lines(normx,densityx,lwd=2)
						coord<-par("usr")
						if(shapiro){
						text(coord[1],.9*coord[4],"Wilk-Shapiro",pos=4,cex=cex.shapiro)
						text(coord[1],.8*coord[4],test,pos=4,cex=cex.shapiro)
						}
					}
					if (i>j){
					plot(varx,vary,axes=FALSE)
					dataEllipse(varx, vary, xlab="",ylab="",add=T,center.pch=F,levels = confidence, col=c(col.point,col.line)) 
					}
					if (i<j) {
					plot(c(0,1),c(0,1),type="n",axes=F,ylab="",xlab="")
					coef.cor<-cor.test(varx,vary)
					coef.calc<-round(coef.cor$estimate,digit=3)
					coefficient<-paste("=",coef.calc)
					proba<-round(coef.cor$p.value,digit=3)
						if (pearson){
						proba<-ifelse(proba<.001,"p < 0.001",paste("p =",proba))
						t.value<-paste("t =",round(coef.cor$statistic,digit=1))
						ddl<-paste("df =",coef.cor$parameter)
						text.calcul<-paste(t.value," ",ddl)
						proba<-c(proba,"")
						}
						else{
						proba<-NULL
						text.calcul<-NULL
						}
						if (rho){
						eval(parse(text=paste0("ex.rho<-expression(rho==",coef.calc,")") ))
						ex.rho<-c(ex.rho,"")
						}
						else
						ex.rho<-NULL
						if (R2){
						val.R2<-round(coef.cor$estimate*coef.cor$estimate*100,digit=1)
						det <- as.expression(substitute(R^2 ~ "=" ~ rcarre ~ pour ,list(rcarre =val.R2,pour="%")))
						}
						else det<-NULL
					text.leg<-c(text.calcul,proba,ex.rho,det)
					legend("topleft",text.leg,cex=cex.calc,bty="n")
					}
				box()  
				}
	}
plot(c(0,1),c(0,1),type="n",axes=F)
	for (i in 1:(n.col)){
	plot(c(0,1),c(0,1),type="n",axes=F)
		if(length(legend)==0) mtext(listvar[i],3,line=-1.5,cex=cex.legend)
		if (length(legend)!=0) mtext(legend[i],3,line=-1.5,cex=cex.legend)
	}
invisible(NULL)
}
#####################################################################################
#Affichage liste de data frame
plot.list.frame<-function(list,top=par("usr")[4],cex=1,mar=c(0,0,0,0)){
nbre.col<-NULL
for (j in 1:length(list))
nbre.col<-max(nbre.col,ncol(list[[j]]))
par(mar=mar)
plot(c(0,10),c(0,10),axes=FALSE,type="n",xlab="",ylab="")
left<-par("usr")[1]
top1<-top
left.good<-NULL;cex.good<-NULL;length.list<-NULL
for (cex.test in seq(cex,.5,by=-.05)){
  for (j in 1:length(list)){
  pos<-paste("legend(left,top1,title=expression(bold('",names(list)[j],"')),'',x.intersp=0,
  bty='n',cex=cex.test,plot=F)",sep="")
  pos<-eval(parse(text=pos))
  top1<-pos$rect$top-pos$rect$h/2.5
  pos<-paste("legend(left,top1,title=expression(bold('",names(list[[j]])[c(1)],"')),",paste("c(",paste("'",list[[j]][[1]],"'",collapse=",",sep=""),")"),",x.intersp=0,
	bty='n',cex=cex.test,plot=F)",sep="")
	pos<-eval(parse(text=pos))
	for (i in 2:length(list[[j]])){
	left<-left+pos$rect$w
	pos<-paste("legend(left,top1,title=expression(bold('",names(list[[j]])[c(i)],"')),",paste("c(",paste("'",list[[j]][[i]],"'",collapse=",",sep=""),")"),",x.intersp=0,
	bty='n',cex=cex.test,plot=F)",sep="")
	pos<-eval(parse(text=pos))
	}
left<-left+pos$rect$w
left<-par("usr")[1]+(par("usr")[2]-left)/2
	if (left>0){
	left.good<-c(left.good,left)
	cex.good<-c(cex.good,cex.test)
	}
left<-par("usr")[1]
top1<-top1-pos$rect$h
 }
  if (pos$text$y[length(pos$text$y)]<0) {
  left.good<-NULL
  cex.good<-NULL
  }
left<-par("usr")[1]
top1<-top
}
left<-min(left.good)
cex.good<-max(cex.good)
top1<-top
for (j in 1:length(list)){
pos<-paste("legend(par('usr')[1]+(left-par('usr')[1])/2,top1,title=expression(bold('",names(list)[j],"')),'',x.intersp=0,
bty='n',cex=cex.good)",sep="")
pos<-eval(parse(text=pos))
top1<-pos$rect$top-pos$rect$h/2.5
pos<-paste("legend(left,top1,title=expression(bold('",names(list[[j]])[c(1)],"')),",paste("c(",paste("'",list[[j]][[1]]
,"'",collapse=",",sep=""),")"),",x.intersp=0,
bty='n',cex=cex.good)",sep="")
pos<-eval(parse(text=pos))
	for (i in 2:length(list[[j]])){
	left<-left+pos$rect$w
	pos<-paste("legend(left,top1,title=expression(bold('",names(list[[j]])[c(i)],"')),",paste("c(",paste("'",list[[j]][[i]]
,"'",collapse=",",sep=""),")"),",x.intersp=0,
	bty='n',cex=cex.good)",sep="")
	pos<-eval(parse(text=pos))
	}
top1<-top1-pos$rect$h
left<-min(left.good)
	}
}
############################################################################################
#Affichage vecteur
plot.vector<-function (object, halign = c("center", "left", "right"), valign = c("center","top", "bottom"), cex, fixed.width = TRUE, cspace = 1, lspace = 1,
    mar = c(0, 0, 3, 0) + 0.1, tab.width = 8, ...)
{
    object <- paste(object, collapse = "\n", sep = "")
    halign = match.arg(halign)
    valign = match.arg(valign)
    plot.new()
    opar <- par()[c("mar", "xpd", "cex", "family")]
    on.exit(par(opar))
    par(mar = mar, xpd = FALSE)
    if (fixed.width)
        par(family = "mono")
    plot.window(xlim = c(0, 1), ylim = c(0, 1), log = "", asp = NA)
    slist <- unlist(lapply(object, function(x) strsplit(x, "\n")))
    slist <- lapply(slist, function(x) unlist(strsplit(x, "")))
    slen <- sapply(slist, length)
    slines <- length(slist)
    if (missing(cex)) {
        lastloop <- FALSE
        cex <- 1
    }
    else lastloop <- TRUE
    for (i in 1:20) {
        oldcex <- cex
        cwidth <- max(sapply(unlist(slist), strwidth, cex = cex)) *
            cspace
        cheight <- max(sapply(unlist(slist), strheight, cex = cex)) *
            (lspace + 0.5)
        width <- strwidth(object, cex = cex)
        height <- strheight(object, cex = cex)
        if (lastloop)
            break
        cex <- cex/max(width, height)
        if (abs(oldcex - cex) < 0.001) {
            lastloop <- TRUE
        }
    }
    if (halign == "left")
        xpos <- 0
    else if (halign == "center")
        xpos <- 0 + (1 - width)/2
    else xpos <- 0 + (1 - width)
    if (valign == "top")
        ypos <- 1
    else if (valign == "center")
        ypos <- 1 - (1 - height)/2
    else ypos <- 1 - (1 - height)
    text(x = xpos, y = ypos, labels = object, adj = c(0, 1),
        cex = cex, ...)
    par(opar)
    invisible(cex)
}
############################################################################
#Affichage texte
textplot <- function(object, halign="center", valign="center", cex, ... )
UseMethod('textplot')
textplot.default <- function(object,halign=c("center","left","right"),valign=c("center","top","bottom"),cex, ... )
{
	if (is.matrix(object) || (is.vector(object) && length(object)>1) )
    return(textplot.matrix(object, halign, valign, cex, ... ))
halign <- match.arg(halign)
valign <- match.arg(valign)
textplot.character(object, halign,  valign, cex, ...)
}

####################################################################
textplot.data.frame <- function(object,halign=c("center","left","right"),valign=c("center","top","bottom"),cex, ... )
textplot.matrix(object, halign, valign, cex, ... )
textplot.matrix <- function(object,halign=c("center","left","right"),valign=c("center","top","bottom"),cex, cmar=2, rmar=0.5,show.rownames=TRUE, show.colnames=TRUE,
hadj=1,vadj=1,mar= c(1,1,4,1)+0.1,col.data=par("col"),col.rownames=par("col"),col.colnames=par("col"),... )
{

  if(is.vector(object))
    object <- t(as.matrix(object))
  else
    object <- as.matrix(object)

  # check dimensions of col.data, col.rownames, col.colnames
  if(length(col.data)==1)
    col.data <- matrix(col.data, nrow=nrow(object), ncol=ncol(object))
  else
    if( nrow(col.data)!=nrow(object) || ncol(col.data)!=ncol(object) )
      stop("Dimensions of 'col.data' do not match dimensions of 'object'.")

  if(length(col.rownames)==1)
      col.rownames <- rep(col.rownames, nrow(object))      

  if(length(col.colnames)==1)
    if(show.rownames)
      col.colnames <- rep(col.colnames, ncol(object)+1)
    else
      col.colnames <- rep(col.colnames, ncol(object))
  
  halign=match.arg(halign)
  valign=match.arg(valign)

  opar <- par()[c("mar","xpd","cex")]
  on.exit( par(opar) )
  par(mar=mar, xpd=FALSE )

  # setup plot area
  plot.new()
  plot.window(xlim=c(0,1),ylim=c(0,1), log = "", asp=NA)



  # add 'r-style' row and column labels if not present
  if( is.null(colnames(object) ) )
    colnames(object) <- paste( "[,", 1:ncol(object), "]", sep="" )
  if( is.null(rownames(object)) )
    rownames(object) <- paste( "[", 1:nrow(object), ",]", sep="")


  # extend the matrix to include row and column labels
  if( show.rownames )
    {
      object <- cbind( rownames(object), object )
      col.data <- cbind( col.rownames, col.data )
      
    }
  if( show.colnames )
    {
      object <- rbind( colnames(object), object )
      col.data <- rbind( col.colnames, col.data )
    }

  # set the character size
  if( missing(cex) )
    {
      cex <- 1.0
      lastloop <- FALSE
    }
  else
    {
      lastloop <- TRUE
    }

  for (i in 1:20)
    {
      oldcex <- cex

      width  <- sum(
                    apply( object, 2,
                          function(x) max(strwidth(x,cex=cex) ) )
                    ) +
                      strwidth('M', cex=cex) * cmar * ncol(object)

      height <- strheight('M', cex=cex) * nrow(object) * (1 + rmar)

      if(lastloop) break

      cex <- cex / max(width,height)

      if (abs(oldcex - cex) < 0.001)
        {
          lastloop <- TRUE
        }
    }


  # compute the individual row and column heights
  rowheight<-strheight("W",cex=cex) * (1 + rmar)
  colwidth<- apply( object, 2, function(XX) max(strwidth(XX, cex=cex)) ) +
               strwidth("W")*cmar


  width  <- sum(colwidth)
  height <- rowheight*nrow(object)

  # setup x alignment
  if(halign=="left")
    xpos <- 0
  else if(halign=="center")
    xpos <- 0 + (1-width)/2
  else #if(halign=="right")
    xpos <- 0 + (1-width)

  # setup y alignment
  if(valign=="top")
    ypos <- 1
  else if (valign=="center")
    ypos <- 1 - (1-height)/2
  else #if (valign=="bottom")
    ypos <- 0 + height

  x <- xpos
  y <- ypos

  # iterate across elements, plotting them
  xpos<-x
  for(i in 1:ncol(object)) {
    xpos <- xpos + colwidth[i]
    for(j in 1:nrow(object)) {
      ypos<-y-(j-1)*rowheight
      if( (show.rownames && i==1) || (show.colnames && j==1) )
        text(xpos, ypos, object[j,i], adj=c(hadj,vadj), cex=cex, font=2,
             col=col.data[j,i], ... )
      else
        text(xpos, ypos, object[j,i], adj=c(hadj,vadj), cex=cex, font=1,
             col=col.data[j,i], ... )
    }
  }

  par(opar)
}
############################################################################
textplot.character <- function (object,halign = c("center", "left", "right"),valign = c("center", "top", "bottom"),cex, fixed.width=TRUE,cspace=1,lspace=1,
mar=c(0,0,3,0)+0.1,tab.width=8,...)
  {
    object <- paste(object,collapse="\n",sep="")
    object <- replaceTabs(object, width=tab.width)

    halign = match.arg(halign)
    valign = match.arg(valign)
    plot.new()

    opar <- par()[c("mar","xpd","cex","family")]
    on.exit( par(opar) )

    par(mar=mar,xpd=FALSE )
    if(fixed.width)
        par(family="mono")

    plot.window(xlim = c(0, 1), ylim = c(0, 1), log = "", asp = NA)

    slist   <- unlist(lapply(object, function(x) strsplit(x,'\n')))
    slist   <- lapply(slist, function(x) unlist(strsplit(x,'')))

    slen    <- sapply(slist, length)
    slines  <- length(slist)

    if (missing(cex))
      {
        lastloop <- FALSE
        cex <- 1
      }
    else
      lastloop <- TRUE


    for (i in 1:20)
      {
        oldcex <- cex
 
        cwidth  <- max(sapply(unlist(slist), strwidth,  cex=cex)) * cspace
         cheight <- max(sapply(unlist(slist), strheight, cex=cex)) * ( lspace + 0.5 )
        width <- strwidth(object, cex=cex)
        height <- strheight(object, cex=cex)

        if(lastloop) break

        cex <- cex  / max(width, height)

        if (abs(oldcex - cex) < 0.001)
          {
            lastloop <- TRUE
          }

      }

    if (halign == "left")
        xpos <- 0
    else if (halign == "center")
        xpos <- 0 + (1 - width)/2
    else xpos <- 0 + (1 - width)

    if (valign == "top")
        ypos <- 1
    else if (valign == "center")
        ypos <- 1 - (1 - height)/2
    else ypos <- 1 - (1 - height)

    text(x=xpos, y=ypos, labels=object, adj=c(0,1),
             cex=cex, ...)

    par(opar)
    invisible(cex)
}
############################################################################
#Representation graphique anova avec legende adaptee
plot.anova<-function(fact,mean,sdev,y.scale,name.fact,name.y){
fact<-as.factor(fact)
y.sup<-(y.scale[1]-y.scale[2])*.04/1.08+y.scale[2]
y.inf<-y.sup+(y.scale[1]-y.scale[2])/1.08
plot(c(0,length(fact)+1),c(y.inf,y.sup),type="n",axes=FALSE,xlab=name.fact,ylab=name.y,main="")
cex.ticks<-ce.label(fact,1)
axis(1,1:length(fact),fact,cex.axis=cex.ticks,tck=.025)
mar.y<-((par("usr")[2]-par("usr")[1])/(par("plt")[2]-par("plt")[1])-(par("usr")[2]-par("usr")[1]))
y.ticks<-strwidth(axTicks(2,axp=c(y.scale,5)))
cex.ticks.y<-ce.label(y.ticks,mar.y)
axis(2,yaxp=c(y.scale,5),las=1,cex.axis=cex.ticks.y,tck=.025)
axis(3,1:length(fact),rep("",length(fact)),tck=.025)
axis(4,yaxp=c(y.scale,5),col.axis="white",tck=.025)
box()
n.group<-1:length(fact)
x0<-n.group[-length(n.group)]
y0<-as.numeric(mean[-length(mean)])
x1<-n.group[-1]
y1<-as.numeric(mean[-1])
segments(x0,y0,x1,y1,col="blue")
points(n.group,as.numeric(mean),col="blue",pch=19)
p <- par()$pin[1]
w <- diff(par()$usr[1:2])
inc <- p/w
arrows(n.group,as.numeric(mean)-as.numeric(sdev),n.group,as.numeric(mean)+as.numeric(sdev),angle=90,code=3,col="blue",lty=1,length=inc/16)
}
############################################################################
#Correlations et histogrammes
correlationHisto.graph<-function (formula,data=parent.frame(),legend=NULL,main="Correlation matrix", cex.calc=1,confidence=0.75)
{
    require(car)
test.formula<-deparse(substitute(formula))
listvar <- as.character(attr(terms(formula(formula)), "variables"))[-1]
n.row<-length(listvar)
n.col<-n.row
nf<-layout(matrix(c(rep(1,n.row+1),seq(2,(n.row+1)*(n.col+1)+1)),n.row+2,n.col+1,byrow=T),c(8/28,rep(1,n.col)),c(4/28,rep(1,n.row),4/28),TRUE)
#nf<-layout(matrix(seq(1,(n.row+1)*(n.col+1)),n.row+1,n.col+1,byrow=T),c(4/28,rep(1,n.col)),c(rep(1,n.row),4/28),TRUE)
par(mar=c(0,0,0,0))
plot(c(0,1),c(0,1),type="n",axes=F,ylab="",xlab="")
text(0.5,0.5,main,cex=2)
for (i in 1:n.row){
plot(c(0,1),c(0,1),type="n",axes=F,ylab="",xlab="")
if(length(legend)==0) mtext(listvar[i],4,line=-1.5)
if (length(legend)!=0) mtext(legend[i],4,line=-1.5)
  for (j in 1:n.col){
        valid <- complete.cases(with(data,eval(parse(text=listvar[j]))),with(data,eval(parse(text=listvar[i]))))
        varx <- with(data,eval(parse(text=listvar[i])))[valid]
        vary <- with(data,eval(parse(text=listvar[j])))[valid]
if (i==j){
hist(varx,xlab="",ylab="",border="blue",main="",axes=FALSE)
}
if (i>j){
	plot(varx,vary,axes=FALSE,col="blue")
	dataEllipse(varx, vary, xlab="",ylab="",add=T,center.pch=F,levels = confidence, col="blue") 
	}
if (i<j) {
plot(c(0,1),c(0,1),type="n",axes=F,ylab="",xlab="")
	}
	box()  
	}
  }
plot(c(0,1),c(0,1),type="n",axes=F)
for (i in 1:(n.col)){
plot(c(0,1),c(0,1),type="n",axes=F)
if(length(legend)==0) mtext(listvar[i],3,line=-1.5)
if (length(legend)!=0) mtext(legend[i],3,line=-1.5)
}

    invisible(NULL)
}
############################################################################
#Representation de dessins multiples
split.postscript<-function(file,ndraw){
	if(ndraw==1){
	postscript(width=7,height=8.45,file=file,horizontal=FALSE)
	layout(matrix(c(1,2),2,1,byrow=TRUE),heights=c(.1,1.1),respect=TRUE)
	par(mar=c(0,0,0,0))
	plot(c(0,1),c(0,1),axes=FALSE,type="n")
	text(.5,.5,"Least Square Means",cex=1.5)
	par(mar=c(5,4,4,2))
	}
	if(ndraw==2){
	postscript(width=7,height=4.55,,file=file,horizontal=FALSE)
	layout(matrix(c(1,1,2,3), 2, 2, byrow=TRUE), heights=c(.2,1.1),respect=TRUE)
	par(mar=c(0,0,0,0))
	plot(c(0,1),c(0,1),axes=FALSE,type="n")
	text(.5,.5,"Least Square Means",cex=2)
	par(mar=c(5,4,4,2))
	}
	if (ndraw>2&&ndraw<5){
	postscript(width=7,height=8.38,file=file,horizontal=FALSE)
	layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow=TRUE), heights=c(.2,1.1,1.1),respect=TRUE)
	par(mar=c(0,0,0,0))
	plot(c(0,1),c(0,1),axes=FALSE,type="n")
	text(.5,.5,"Least Square Means",cex=2)
	par(mar=c(5,4,4,2))
	}
	if (ndraw>4&&ndraw<7){
	postscript(width=7,height=5.5,file=file,horizontal=FALSE)
	layout(matrix(c(1,1,1,2,3,4,5,6,7),3,3,byrow=TRUE),heights=c(.2,1.1,1.1),respect=TRUE)
	par(mar=c(0,0,0,0))
	plot(c(0,1),c(0,1),axes=FALSE,type="n")
	text(.5,.5,"Least Square Means",cex=2)
	par(mar=c(5,4,4,2))

	}
	if (ndraw>6&&ndraw<10){
	postscript(width=7,height=8.17,file=file,horizontal=FALSE)
	layout(matrix(c(1,1,1,2,3,4,5,6,7,8,9,10),4,3,byrow=TRUE),heights=c(.2,1.1,1.1,1.1),respect=TRUE)
	par(mar=c(0,0,0,0))
	plot(c(0,1),c(0,1),axes=FALSE,type="n")
	text(.5,.5,"Least Square Means",cex=2)
	par(mar=c(5,4,4,2))
	}
	if (ndraw>10&&ndraw<13){
	postscript(width=7,height=8.17,file=file,horizontal=FALSE)
	layout(matrix(c(1,1,1,2,3,4,5,6,7,8,9,10,11,12,13),5,3,byrow=TRUE),heights=c(.2,1.1,1.1,1.1,1.1),respect=TRUE)
	par(mar=c(0,0,0,0))
	plot(c(0,1),c(0,1),axes=FALSE,type="n")
	text(.5,.5,"Least Square Means",cex=2)
	par(mar=c(5,4,4,2))
	}
}
##############################################################################
#Representation residus
graphResidual<-function (formula,factor,data=parent.frame(),xlab = paste(listvar[2]),ylab = paste(listvar[1]), legend.lab = deparse(substitute(factor)),
main = "", pch = 1:n.levs,col = palette())
{
    f <- formula(formula)
    listvar <- as.character(attr(terms(f), "variables"))[-1]
    vary<-listvar[1]
    varx<-listvar[2]
   	model<-lm(formula,data=data)
    limy<-max(abs(model$residuals))
    xlab
    ylab
	legend.lab
	if (!missing(factor)) layout(matrix(c(1,1,2,3),2,2,byrow=T),c(1.6,.4),c(.2,1.8),TRUE)
	else layout(matrix(c(1,2),2,1,byrow=T),2,c(.2,1.8),TRUE)
	par(mar=c(0,0,0,0));plot(c(0,10),c(0,10),type='n',axe=F)
      if (missing(factor)) {
  text(5,5,paste(main,"(x =",listvar[2],"; y = ",listvar[1],")"),cex=1.5)
	par(mar=c(5,4,0,2))
  plot(model$fitted.value,model$residuals,xlab="Estimate",ylab="Residuals",ylim=c(-limy,limy),col="blue")
  abline(h=0,col="gray")
  	  }
      if (!missing(factor)){
	factor<-deparse(substitute(factor))
	valid <- complete.cases(with(data,eval(parse(text=varx))),with(data,eval(parse(text=vary))),with(data,eval(parse(text=factor))))
        varx <- with(data,eval(parse(text=varx)))[valid]
        vary <- with(data,eval(parse(text=vary)))[valid]
       factor <- factor(with(data,eval(parse(text=factor)))[valid])
        model<-lm(vary~varx)
        levs <- levels(factor)
        n.levs <- length(levs)
	i<-1
	text(5,5,paste(main,"(x =",listvar[2],"; y = ",listvar[1],")"),cex=2)
	par(mar=c(5,5,0,0))
	 plot(model$fitted.value,model$residuals,xlab="Estimate",ylab="Residuals",type="n",ylim=c(-limy,limy),col="blue")
  abline(h=0,col="gray")
		for (variable in levs){
   data.group<-data.frame(varx=model$fitted.values,vary=model$residuals,factor)
   model.group<-subset(data.group,factor==variable)
		points(model.group$varx,model.group$vary,col=col[i],pch=pch[i])
   	i=i+1

		}
      par(mar=c(0,0,0,0));plot(c(0,10),c(0,10),type='n',axe=F)
	legend("topright",levs,pch=pch,col=col,lwd=1,title=legend.lab,merge=T)
	}
    invisible(NULL)
}
 #########################################################################################
 # Droite de regression
correlation.graph<-function (formula,factor,data=parent.frame(),xlab =NULL ,xlim=NULL,IC.confidence=FALSE,IC.conf.value=0.95,IC.predict=FALSE,IC.pred.value=0.95,
ylab = NULL, ylim=NULL,legend.lab = deparse(substitute(factor)),axes=TRUE,width.title=4, main = "Plot of regression", pch = 1:n.levs, col = palette())
{
require(car)
f <- formula(formula)
listvar <- as.character(attr(terms(f), "variables"))[-1]
y<-listvar[1]
x<-listvar[2]
	if (is.null(xlab)) xlab<- x
	if (is.null(ylab)) ylab<- y	
	#Pas de groupe
    if (missing(factor)) {
  	par(mar=c(5,4,2,0))
  	layout(matrix(c(1,1,2,2),2,2,byrow = TRUE), c(1,4), c(4,1), TRUE)  
    valid <- complete.cases(with(data,eval(parse(text=x))),with(data,eval(parse(text=y))))
    varx <- with(data,eval(parse(text=x)))[valid]
    vary <- with(data,eval(parse(text=y)))[valid]
 	coef.cor<-cor.test(varx,vary)
	coef.cor<-round(as.numeric(coef.cor[[4]]),digit=3)
	predict.seq<-data.frame(varx=seq(min(varx),max(varx),length.out=length(varx)))
	pred.w.plim<-predict(lm(vary~varx),predict.seq,interval="prediction",level=IC.pred.value)
	pred.w.clim<-predict(lm(vary~varx),predict.seq,interval="confidence",level=IC.conf.value)
	plot(varx,vary,xlab=xlab,ylab=ylab,main=main,pch=1,col=col[1],xlim=xlim,ylim=ylim)
	abline(lm(formula,data=data),col=col[2])
		if(IC.confidence){
		lines(predict.seq$varx,pred.w.clim[,2],lty=2,col=col[3],lwd=2)
		lines(predict.seq$varx,pred.w.clim[,3],lty=2,col=col[3],lwd=2)
		}
		if(IC.predict){
		lines(predict.seq$varx,pred.w.plim[,2],lty=3,col=col[3],lwd=2)
		lines(predict.seq$varx,pred.w.plim[,3],lty=3,col=col[3],lwd=2)
		}
  	par(mar=c(0,0,0,0))
  	plot(c(0,1),type="n",axes=F)
		if (legend.lab == deparse(substitute(factor)))
		legend("center",c(ifelse(IC.confidence==TRUE,"Confidence interval",""),ifelse(IC.predict==TRUE,"Prediction interval","")),
		lty=c(ifelse(IC.confidence==TRUE,2,0),ifelse(IC.predict==TRUE,3,0)),lwd=c(ifelse(IC.confidence==TRUE,2,0),ifelse(IC.predict==TRUE,2,0)),bty="n",ncol=2)
		else
		legend("center",c(ifelse(IC.confidence==TRUE,legend.lab[1],""),ifelse(IC.predict==TRUE,legend.lab[2],"")),lty=c(ifelse(IC.confidence==TRUE,2,0),
		ifelse(IC.predict==TRUE,3,0)),lwd=c(ifelse(IC.confidence==TRUE,2,0),ifelse(IC.predict==TRUE,2,0)),bty="n",ncol=2)
	}
	#Avec un groupe
	else {
	factorname<-deparse(substitute(factor))
	valid <- complete.cases(with(data,eval(parse(text=x))),with(data,eval(parse(text=y))),with(data,eval(parse(text=factorname))))
    varx <- with(data,eval(parse(text=x)))[valid]
    vary <- with(data,eval(parse(text=y)))[valid]
    factor <- with(data,eval(parse(text=factorname)))[valid]
    levs <- levels(factor)
    n.levs <- length(levs)
	i<-1
	par(mar=c(5,4,2,0))
	layout(matrix(c(1,2,3,3),2,2,byrow = TRUE), c(4,1), c(4,1), TRUE)  
	plot(varx,vary,xlab=xlab,ylab=ylab,main=main,type="n",xlim=xlim,ylim=ylim)
		for (variable in levs){
		varxi<-subset(varx,factor==variable)
		varyi<-subset(vary,factor==variable)		
		points(varxi,varyi,col=col[i],pch=pch[i])
		abline(lm(varyi~varxi),col=col[i])
		predict.seq<-data.frame(varxi=seq(min(varxi),max(varxi),length.out=length(varxi)))
			if(IC.confidence){
			pred.w.clim<-predict(lm(varyi~varxi),predict.seq,interval="confidence",level=IC.conf.value)
			lines(predict.seq$varxi,pred.w.clim[,2],lty=2,col=col[i],lwd=2)
			lines(predict.seq$varxi,pred.w.clim[,3],lty=2,col=col[i],lwd=2)
			}
			if(IC.predict){
			pred.w.plim<-predict(lm(varyi~varxi),predict.seq,interval="prediction",level=IC.pred.value)
			lines(predict.seq$varxi,pred.w.plim[,2],lty=3,col=col[i],lwd=2)
			lines(predict.seq$varxi,pred.w.plim[,3],lty=3,col=col[i],lwd=2)
			}
		i=i+1
		}
  	par(mar=c(0,0,0,0))
  	plot(c(0,1),type="n",axes=F)
  	legend("center",levs,pch=pch[1:length(levs)],levs,col=col[1:length(levs)],title=factorname)
  	plot(c(0,1),type="n",axes=F)
  	legend("center",c(ifelse(IC.confidence==TRUE,"Confidence interval",""),ifelse(IC.predict==TRUE,"Prediction interval","")),lty=c(ifelse(IC.confidence==TRUE,2,0),ifelse(IC.predict==TRUE,3,0)),lwd=c(ifelse(IC.confidence==TRUE,2,0),ifelse(IC.predict==TRUE,2,0)),bty="n",ncol=2)
	}
par(mar=c(5,4,4,2))
invisible(NULL)
}
#########################################################################################
#Amelioration DMFA
plot.DMFA2<-function (x, axes = c(1, 2), choix = "ind", label = "all", lim.cos2.var = 0, 
    xlim = NULL, ylim = NULL, title = NULL,  new.plot = FALSE, 
    autoLab = c("auto", "yes", "no"), ...) 
{
    res.dmfa = x
    autoLab <- match.arg(autoLab, c("auto", "yes", "no"))
    if (autoLab == "yes") 
        autoLab = TRUE
    if (autoLab == "no") 
        autoLab = FALSE
    class(res.dmfa) <- c("PCA", "list ")
    if (choix == "ind") {
        if (is.null(title)) 
            titre = "Individuals factor map (PCA)"
        else titre = title
        plot.PCA(res.dmfa, habillage = 1, palette=palette(),axes = axes, label = label, 
            xlim = xlim, ylim = ylim, autoLab = autoLab, title = titre, 
            ...)
    }
    if (choix == "quali") {
        if (length(res.dmfa$call$quali.sup$modalite) == 1) 
            stop("There is no supplementary qualitative variable")
        lev = levels(res.dmfa$call$X[, 1])
        ng = length(lev)
        nb.quali = (length(res.dmfa$call$quali.sup$modalite) - 
            1)/2
        xlim = 1.1 * c(min(res.dmfa$quali.sup$coord[, axes[1]]), 
            max(res.dmfa$quali.sup$coord[, axes[1]]))
        ylim = 1.1 * c(min(res.dmfa$quali.sup$coord[, axes[2]]), 
            max(res.dmfa$quali.sup$coord[, axes[2]]))
        lab.x <- paste("Dim ", axes[1], " (", signif(res.dmfa$eig[axes[1], 
            2], 4), " %)", sep = "")
        lab.y <- paste("Dim ", axes[2], " (", signif(res.dmfa$eig[axes[2], 
            2], 4), " %)", sep = "")
        if (is.null(title)) 
            titre = "Qualitative representation"
        else titre = title
        if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) 
            dev.new()
        plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, 
            xlim = xlim, ylim = ylim, col = "white", asp = 1, 
            ...)
        abline(v = 0, lty = 2, ...)
        abline(h = 0, lty = 2, ...)
        for (i in 1:sum(res.dmfa$call$quali.sup$modalite[2:(1 + 
            nb.quali)])) {
            points(res.dmfa$quali.sup$coord[i, axes[1]], res.dmfa$quali.sup$coord[i, 
                axes[2]], pch = 15, ...)
            text(res.dmfa$quali.sup$coord[i, axes[1]], res.dmfa$quali.sup$coord[i, 
                axes[2]], rownames(res.dmfa$quali.sup$coord)[i], 
                pos = 3, ...)
            for (j in 1:ng) {
                points(res.dmfa$quali.sup$coord[sum(res.dmfa$call$quali.sup$modalite[2:(1 + 
                  nb.quali)]) + ng * (i - 1) + j, axes[1]], res.dmfa$quali.sup$coord[sum(res.dmfa$call$quali.sup$modalite[2:(1 + 
                  nb.quali)]) + ng * (i - 1) + j, axes[2]], col = j + 
                  1, pch = 20, ...)
                lines(c(res.dmfa$quali.sup$coord[i, axes[1]], 
                  res.dmfa$quali.sup$coord[sum(res.dmfa$call$quali.sup$modalite[2:(1 + 
                    nb.quali)]) + ng * (i - 1) + j, axes[1]]), 
                  c(res.dmfa$quali.sup$coord[i, axes[2]], res.dmfa$quali.sup$coord[sum(res.dmfa$call$quali.sup$modalite[2:(1 + 
                    nb.quali)]) + ng * (i - 1) + j, axes[2]]), 
                  col = j + 1, ...)
            }
        }
        legend("topleft", legend = rownames(res.dmfa$group$coord), 
            text.col = 2:(1 + ng), cex = 0.8, bg = "white")
    }
    if (choix == "var") {
        lev = levels(res.dmfa$call$X[, 1])
        ng = length(lev)
        if (is.null(title)) 
            titre = "Variables factor map (PCA)"
        else titre = title
        plot.PCA(res.dmfa, choix = "var", axes = axes, col.var = ng + 
            1, lim.cos2.var = lim.cos2.var, label = label, autoLab = autoLab, 
            title = titre, ...)
        for (j in 1:ng) {
            cor.partiel = res.dmfa$var.partiel[[j]][, axes]
            cor.cos2 = res.dmfa$cor.dim.gr[[j]][axes[1], axes[2]]
            for (v in 1:nrow(cor.partiel)) {
                qualite = (cor.partiel[v, 1]^2 + cor.partiel[v, 
                  2]^2)/sqrt(cor.partiel[v, 1]^2 + cor.partiel[v, 
                  2]^2 + 2 * cos(cor.cos2) * (cor.partiel[v, 
                  1] * cor.partiel[v, 2]))
                arrows(0, 0, cor.partiel[v, 1], cor.partiel[v, 
                  2], length = 0.1 * qualite, angle = 15, code = 2, 
                  col = j, ...)
                if (abs(cor.partiel[v, 1]) > abs(cor.partiel[v, 
                  2])) {
                  if (cor.partiel[v, 1] >= 0) 
                    pos <- 4
                  else pos <- 2
                }
                else {
                  if (cor.partiel[v, 2] >= 0) 
                    pos <- 3
                  else pos <- 1
                }
                if (label == "all") 
                  text(cor.partiel[v, 1], y = cor.partiel[v, 
                    2], labels = rownames(cor.partiel)[v], pos = pos, 
                    col = j, ...)
            }
        }
        legend("bottomleft", legend = c(lev, "var"), text.col = 1:(ng + 
            1), cex = 0.8, bg = "white")
        Xc = res.dmfa$Xc
        for (j in 1:ng) {
            auxil = res.dmfa$ind$coord[res.dmfa$call$X[, 1] == 
                lev[j], axes]
            prefpls(cbind(auxil, Xc[[j]][rownames(auxil), ]), 
                title = paste("Biplot between axes ", axes[1], 
                  " and ", axes[2], " for group ", lev[j], sep = ""))
        }
    }
    if (choix == "group") {
        if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) 
            dev.new()
        coord.gr = res.dmfa$group$coord.n
        lev = levels(res.dmfa$call$X[, 1])
        ng = length(lev)
        xlim = 1.1 * c(0, max(1, max(coord.gr[, axes[1]])))
        ylim = 1.1 * c(0, max(1, max(coord.gr[, axes[2]])))
        if (is.null(title)) 
            titre = "Projection of the groups"
        else titre = title
        plot(0, 0, xlab = paste("Dim", axes[1]), ylab = paste("Dim", 
            axes[2]), xlim = xlim, ylim = ylim, col = "white", 
            asp = 1, main = titre, ...)
        for (j in 1:ng) {
            points(coord.gr[j, axes[1]], coord.gr[j, axes[2]], 
                col = j, pch = 15, ...)
            if (label == "all") 
                text(coord.gr[j, axes[1]], coord.gr[j, axes[2]], 
                  labels = lev[j], pos = 3, ...)
        }
        abline(v = 0, lty = 2, ...)
        abline(h = 0, lty = 2, ...)
    }
}
###############################################################################
#Amelioration MFA
plot.MFA2<-function (x, axes = c(1, 2), choix = c("ind", "var", "group", 
    "axes", "freq"), ellipse = NULL, ellipse.par = NULL, lab.grpe = TRUE, 
    lab.var = TRUE, lab.ind = TRUE, lab.par = FALSE, lab.col = TRUE, 
    habillage = "ind", col.hab = NULL, invisible = c("none", 
        "ind", "ind.sup", "quanti", "quanti.sup", "quali", "quali.sup", 
        "row", "row.sup", "col", "col.sup"), partial = NULL, 
    lim.cos2.var = 0, chrono = FALSE, xlim = NULL, ylim = NULL, 
    title = NULL, palette = palette(), autoLab = c("auto", "yes", 
        "no"), new.plot = FALSE, select = NULL, unselect = 0.7, 
    shadowtext = FALSE, ...) 
{
    res.mfa <- x
    if (!inherits(res.mfa, "MFA")) 
        stop("non convenient data")
    if (is.numeric(unselect)) 
        if ((unselect > 1) | (unselect < 0)) 
            stop("unselect should be betwwen 0 and 1")
    autoLab <- match.arg(autoLab, c("auto", "yes", "no"))
    if (autoLab == "yes") 
        autoLab = TRUE
    if (autoLab == "no") 
        autoLab = FALSE
    choix <- match.arg(choix, c("ind", "var", "group", "axes", 
        "freq"))
    invisible <- match.arg(invisible, c("none", "ind", "ind.sup", 
        "quanti", "quanti.sup", "quali", "row", "row.sup", "col", 
        "col.sup"), several.ok = TRUE)
    if ("none" %in% invisible) 
        invisible = NULL
    lab.x <- paste("Dim ", axes[1], " (", format(res.mfa$eig[axes[1], 
        2], nsmall = 2, digits = 2), "%)", sep = "")
    lab.y <- paste("Dim ", axes[2], " (", format(res.mfa$eig[axes[2], 
        2], nsmall = 2, digits = 2), "%)", sep = "")
    group <- res.mfa$call$group
    nbre.grpe <- length(group)
    type <- res.mfa$call$type
    num.group.sup = NULL
    if (!is.null(res.mfa$call$num.group.sup)) {
        num.group.sup <- res.mfa$call$num.group.sup
        nbre.grpe.sup <- length(num.group.sup)
        type.sup <- type[num.group.sup]
        type.act <- type[-num.group.sup]
        nbre.grpe <- nbre.grpe - length(num.group.sup)
    }
    if (choix == "axes") {
        if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) 
            dev.new()
        if (is.null(title)) 
            title <- "Partial axes"
        plot(0, 0, xlab = lab.x, ylab = lab.y, xlim = c(-1.1, 
            1.1), ylim = c(-1.1, 1.1), col = "white", asp = 1, 
            main = title, ...)
        x.cercle <- seq(-1, 1, by = 0.01)
        y.cercle <- sqrt(1 - x.cercle^2)
        lines(x.cercle, y = y.cercle, ...)
        lines(x.cercle, y = -y.cercle, ...)
        abline(v = 0, lty = 2, ...)
        abline(h = 0, lty = 2, ...)
        coord.axes <- res.mfa$partial.axes$coord[, axes, drop = FALSE]
        if (!is.null(select)) {
            if (mode(select) == "numeric") 
                selection <- select
            else {
                if (sum(rownames(res.mfa$partial.axes$coord) %in% 
                  select) != 0) 
                  selection <- which(rownames(res.mfa$partial.axes$coord) %in% 
                    select)
                else {
                  if (grepl("contrib", select)) 
                    selection <- (rev(order(res.mfa$partial.axes$contrib[, 
                      axes[1], drop = FALSE] * res.mfa$eig[axes[1], 
                      1] + res.mfa$partial.axes$contrib[, axes[2], 
                      drop = FALSE] * res.mfa$eig[axes[2], 1])))[1:min(nrow(res.mfa$partial.axes$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "contrib"))), na.rm = T))]
                  if (grepl("coord", select)) 
                    selection <- (rev(order(apply(res.mfa$partial.axes$coord[, 
                      axes]^2, 1, sum))))[1:min(nrow(res.mfa$partial.axes$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "coord"))), na.rm = T))]
                  if (is.integer(select)) 
                    selection <- select
                }
            }
        }
        if (habillage == "group") {
            if (is.null(col.hab) | length(col.hab) < length(group)) {
                if (is.null(res.mfa$call$num.group.sup)) 
                  col.hab <- 2:(length(group) + 1)
                else {
                  col.hab[which(!(1:length(group)) %in% (res.mfa$call$num.group.sup))] <- 2:(1 + 
                    length(group) - length(res.mfa$call$num.group.sup))
                  col.hab[res.mfa$call$num.group.sup] <- length(group) - 
                    length(res.mfa$call$num.group.sup) + 1 + 
                    (1:length(res.mfa$call$num.group.sup))
                }
            }
            i = 1
            couleur.axes <- col.hab[i]
            auxil = strsplit(rownames(res.mfa$partial.axes$coord)[1], 
                ".", fixed = TRUE)[[1]]
            auxil2 = auxil[length(auxil)]
            for (j in 2:nrow(res.mfa$partial.axes$coord)) {
                auxil = strsplit(rownames(res.mfa$partial.axes$coord)[j], 
                  ".", fixed = TRUE)[[1]]
                if (auxil2 != auxil[length(auxil)]) {
                  i = i + 1
                  auxil2 = auxil[length(auxil)]
                }
                couleur.axes <- c(couleur.axes, col.hab[i])
            }
        }
        else {
            couleur.axes <- NULL
            for (i in 1:length(group)) couleur.axes <- c(couleur.axes, 
                rep("black", ncol(res.mfa$partial.axes$coord)))
        }
        posi <- coll <- NULL
        if (!is.null(select)) {
            coord.axes <- coord.axes[selection, , drop = FALSE]
            couleur.axes <- couleur.axes[selection]
        }
        for (v in 1:nrow(coord.axes)) {
            arrows(0, 0, coord.axes[v, 1], coord.axes[v, 2], 
                length = 0.1, angle = 15, code = 2, col = couleur.axes[v], 
                ...)
            if (abs(coord.axes[v, 1]) > abs(coord.axes[v, 2])) {
                if (coord.axes[v, 1] >= 0) 
                  posi <- c(posi, 4)
                else posi <- c(posi, 2)
            }
            else {
                if (coord.axes[v, 2] >= 0) 
                  posi <- c(posi, 3)
                else posi <- c(posi, 1)
            }
            labe <- rownames(coord.axes)
        }
        if (autoLab == "auto") 
            autoLab = (length(labe) < 50)
        if (autoLab == FALSE) 
            text(coord.axes[, 1], y = coord.axes[, 2], labels = labe, 
                pos = posi, col = couleur.axes, ...)
        if (autoLab == TRUE) 
            autoLab(coord.axes[, 1], y = coord.axes[, 2], labels = labe, 
                col = couleur.axes, shadotext = shadowtext, ...)
        if (habillage == "group") 
            legend("topleft", legend = rownames(res.mfa$group$Lg)[-length(rownames(res.mfa$group$Lg))], 
                text.col = unique(couleur.axes), ...)
    }
    if (choix == "group") {
        coord.actif <- res.mfa$group$coord[, axes, drop = FALSE]
        if (!is.null(res.mfa$group$coord.sup)) 
            coord.illu <- res.mfa$group$coord.sup[, axes, drop = FALSE]
        if (length(col.hab) == 1) 
            col.hab = rep(col.hab, length(group))
        if (is.null(col.hab)) {
            col.hab = rep("darkred", nrow(coord.actif))
            if (!is.null(res.mfa$group$coord.sup)) 
                col.hab = c(col.hab, rep("darkolivegreen", nrow(coord.illu)))
        }
        if (habillage == "group") 
            col.hab <- (2:(length(group) + 1))
        if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) 
            dev.new()
        coo <- labe <- coll <- ipch <- fonte <- NULL
        if (is.null(xlim)) 
            xlim <- c(0, 1)
        if (is.null(ylim)) 
            ylim <- c(0, 1)
        if (is.null(title)) 
            title <- "Groups representation"
        plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, 
            xlim = xlim, ylim = ylim, asp = 1, col = "white", 
            ...)
        abline(v = 0, lty = 2, ...)
        abline(h = 0, lty = 2, ...)
        coo <- rbind(coo, coord.actif)
        if (lab.grpe) {
            labe <- c(labe, rownames(coord.actif))
        }
        else labe <- c(labe, rep("", nrow(coord.actif)))
        coll <- c(coll, col.hab[1:nrow(coord.actif)])
        ipch <- c(ipch, rep(17, nrow(coord.actif)))
        fonte <- c(fonte, rep(1, nrow(coord.actif)))
        if (!is.null(res.mfa$group$coord.sup)) {
            coo <- rbind(coo, coord.illu)
            if (lab.grpe) {
                labe <- c(labe, rownames(coord.illu))
            }
            else labe <- c(labe, rep("", nrow(coord.illu)))
            coll <- c(coll, col.hab[(nrow(coord.actif) + 1):(nrow(coord.actif) + 
                nrow(coord.illu))])
            ipch <- c(ipch, rep(2, nrow(coord.illu)))
            fonte <- c(fonte, rep(3, nrow(coord.illu)))
        }
        if (shadowtext) 
            points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, 
                ...)
        if (autoLab == "auto") 
            autoLab = (length(labe) < 50)
        if (autoLab == TRUE) 
            autoLab(coo[, 1], y = coo[, 2], labels = labe, col = coll, 
                font = fonte, shadotext = shadowtext, ...)
        if (autoLab == FALSE) 
            text(coo[, 1], y = coo[, 2], labels = labe, col = coll, 
                font = fonte, pos = 3, ...)
        if (!shadowtext) 
            points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, 
                ...)
    }
    if (choix == "var") {
        if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) 
            dev.new()
        test.invisible <- vector(length = 2)
        if (!is.null(invisible)) {
            test.invisible[1] <- match("quanti", invisible)
            test.invisible[2] <- match("quanti.sup", invisible)
        }
        else test.invisible <- rep(NA, 2)
        col <- NULL
        if (habillage == "group") {
            if (is.null(col.hab) | length(col.hab) < length(group[type == 
                "c"])) {
                if (!is.null(res.mfa$call$num.group.sup)) {
                  col.hab[which(!(1:length(group)) %in% (res.mfa$call$num.group.sup))] <- 2:(1 + 
                    length(group) - length(res.mfa$call$num.group.sup))
                  col.hab[res.mfa$call$num.group.sup] <- length(group) - 
                    length(res.mfa$call$num.group.sup) + 1 + 
                    (1:length(res.mfa$call$num.group.sup))
                  col <- c(1 + rep(which(res.mfa$call$nature.group[-res.mfa$call$num.group.sup] == 
                    "quanti"), times = group[which(res.mfa$call$nature.group == 
                    "quanti")]), length(group) - length(res.mfa$call$num.group.sup) + 
                    1 + rep(which((res.mfa$call$nature.group[res.mfa$call$num.group.sup]) == 
                    "quanti.sup"), times = group[which(res.mfa$call$nature.group == 
                    "quanti.sup")]))
                }
                else {
                  col.hab <- 2:(length(group) + 1)
                  col <- 1 + rep(which(type == "c"), times = group[type == 
                    "c"])
                }
            }
        }
        else {
            if (is.null(col.hab) | length(col.hab) < sum(group[type == 
                "c"])) 
                col <- rep(1, sum(group[type == "c"]))
            else col <- col.hab
        }
        if (is.null(title)) 
            title <- "Correlation circle"
        plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, 
            xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), col = "white", 
            asp = 1, ...)
        x.cercle <- seq(-1, 1, by = 0.01)
        y.cercle <- sqrt(1 - x.cercle^2)
        lines(x.cercle, y = y.cercle, ...)
        lines(x.cercle, y = -y.cercle, ...)
        abline(v = 0, lty = 2, ...)
        abline(h = 0, lty = 2, ...)
        if ((!is.null(select)) & (!is.null(res.mfa["quanti.var"]$quanti.var))) {
            if (mode(select) == "numeric") 
                selection <- select
            else {
                if (sum(rownames(res.mfa$quanti.var$coord) %in% 
                  select) + sum(rownames(res.mfa$quanti.var.sup$coord) %in% 
                  select) != 0) 
                  selection <- which(rownames(res.mfa$quanti.var$coord) %in% 
                    select)
                else {
                  if (grepl("contrib", select)) 
                    selection <- (rev(order(res.mfa$quanti.var$contrib[, 
                      axes[1], drop = FALSE] * res.mfa$eig[axes[1], 
                      1] + res.mfa$quanti.var$contrib[, axes[2], 
                      drop = FALSE] * res.mfa$eig[axes[2], 1])))[1:min(nrow(res.mfa$quanti.var$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "contrib"))), na.rm = T))]
                  if (grepl("coord", select)) 
                    selection <- (rev(order(apply(res.mfa$quanti.var$coord[, 
                      axes]^2, 1, sum))))[1:min(nrow(res.mfa$quanti.var$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "coord"))), na.rm = T))]
                  if (grepl("cos2", select)) {
                    if (sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T) >= 1) 
                      selection <- (rev(order(apply(res.mfa$quanti.var$cos2[, 
                        axes], 1, sum))))[1:min(nrow(res.mfa$quanti.var$coord), 
                        sum(as.numeric(unlist(strsplit(select, 
                          "cos2"))), na.rm = T))]
                    else selection <- which(apply(res.mfa$quanti.var$cos2[, 
                      axes], 1, sum) > sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T))
                  }
                  if (is.integer(select)) 
                    selection <- select
                }
            }
        }
        if ((!is.null(select)) & (!is.null(res.mfa$quanti.var.sup))) {
            if (mode(select) == "numeric") 
                selectionS <- select
            else {
                if (sum(rownames(res.mfa$quanti.var$coord) %in% 
                  select) + sum(rownames(res.mfa$quanti.var.sup$coord) %in% 
                  select) != 0) 
                  selectionS <- which(rownames(res.mfa$quanti.var.sup$coord) %in% 
                    select)
                else {
                  if (grepl("contrib", select)) 
                    selectionS <- NULL
                  if (grepl("coord", select)) 
                    selectionS <- (rev(order(apply(res.mfa$quanti.var.sup$coord[, 
                      axes]^2, 1, sum))))[1:min(nrow(res.mfa$quanti.var.sup$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "coord"))), na.rm = T))]
                  if (grepl("cos2", select)) {
                    if (sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T) >= 1) 
                      selectionS <- (rev(order(apply(res.mfa$quanti.var.sup$cos2[, 
                        axes], 1, sum))))[1:min(nrow(res.mfa$quanti.var.sup$coord), 
                        sum(as.numeric(unlist(strsplit(select, 
                          "cos2"))), na.rm = T))]
                    else selectionS <- which(apply(res.mfa$quanti.var.sup$cos2[, 
                      axes], 1, sum) > sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T))
                  }
                  if (is.integer(select)) 
                    selectionS <- select
                }
            }
        }
        labe <- labe2 <- coll <- coll2 <- NULL
        if (!is.null(res.mfa["quanti.var"]$quanti.var) & is.na(test.invisible[1])) {
            coll <- col[1:nrow(res.mfa["quanti.var"]$quanti.var$coord)]
            if (lab.var) 
                labe <- rownames(res.mfa["quanti.var"]$quanti.var$coord)
            else labe <- rep("", nrow(res.mfa["quanti.var"]$quanti.var$coord))
        }
        if (!is.null(res.mfa$quanti.var.sup) & is.na(test.invisible[2])) {
            if (lab.var) 
                labe2 <- rownames(res.mfa$quanti.var.sup$coord)
            else labe2 <- rep("", nrow(res.mfa$quanti.var.sup$coord))
            coll2 <- col[(length(coll) + 1):length(col)]
        }
        if (!is.null(select)) {
            if (!is.null(res.mfa["quanti.var"]$quanti.var) & 
                is.na(test.invisible[1])) {
                if (is.numeric(unselect)) 
                  coll[!((1:length(coll)) %in% selection)] = rgb(t(col2rgb(coll[!((1:length(coll)) %in% 
                    selection)])), alpha = 255 * (1 - unselect), 
                    maxColorValue = 255)
                else coll[!((1:length(coll)) %in% selection)] = unselect
                labe[!((1:length(coll)) %in% selection)] <- ""
            }
            if (!is.null(res.mfa$quanti.var.sup) & is.na(test.invisible[2])) {
                if (is.numeric(unselect)) 
                  coll2[!((1:length(coll2)) %in% selectionS)] = rgb(t(col2rgb(coll2[!((1:length(coll2)) %in% 
                    selectionS)])), alpha = 255 * (1 - unselect), 
                    maxColorValue = 255)
                else coll2[!((1:length(coll2)) %in% selectionS)] = unselect
                labe2[!((1:length(coll2)) %in% selectionS)] <- ""
            }
        }
        col <- c(coll, coll2)
        labe <- c(labe, labe2)
        if (habillage == "group" & is.na(test.invisible[1]) & 
            is.na(test.invisible[2])) 
            legend("topleft", legend = rownames(res.mfa$group$Lg[-nrow(res.mfa$group$Lg), 
                ])[type == "c"], text.col = col.hab[type == "c"], 
                cex = 0.8 * par("cex"))
        if (habillage == "group" & is.na(test.invisible[1]) & 
            !is.na(test.invisible[2])) {
            if ("quanti.sup" %in% res.mfa$call$nature.var) 
                legend("topleft", legend = rownames(res.mfa$group$Lg[-c(num.group.sup, 
                  nrow(res.mfa$group$Lg)), ])[type.act == "c"], 
                  text.col = col.hab[which(!((1:length(group)) %in% 
                    res.mfa$call$num.group.sup))[type.act == 
                    "c"]], cex = 0.8 * par("cex"))
            else legend("topleft", legend = rownames(res.mfa$group$Lg[-nrow(res.mfa$group$Lg), 
                ])[type == "c"], text.col = col.hab[type == "c"], 
                cex = 0.8 * par("cex"))
        }
        if (habillage == "group" & !is.na(test.invisible[1]) & 
            is.na(test.invisible[2])) {
            if ("quanti" %in% res.mfa$call$nature.var) 
                legend("topleft", legend = rownames(res.mfa$group$Lg[num.group.sup, 
                  ])[type.sup == "c"], text.col = col.hab[res.mfa$call$num.group.sup[type.sup == 
                  "c"]], cex = 0.8 * par("cex"))
            else legend("topleft", legend = rownames(res.mfa$group$Lg[num.group.sup, 
                ])[type.sup == "c"], text.col = col.hab[res.mfa$call$num.group.sup[type.sup == 
                "c"]], cex = 0.8 * par("cex"))
        }
        nrow.coord.var <- 0
        coo <- posi <- NULL
        if ((!is.null(res.mfa["quanti.var"]$quanti.var)) & (is.na(test.invisible[1]))) {
            if (length(apply(res.mfa["quanti.var"]$quanti.var$cos2[, 
                axes, drop = FALSE], 1, sum, na.rm = TRUE) >= 
                lim.cos2.var) > 0) {
                coord.var <- res.mfa$quanti.var$cor[which(apply(res.mfa$quanti.var$cos2[, 
                  axes, drop = FALSE], 1, sum, na.rm = TRUE) >= 
                  lim.cos2.var), axes, drop = FALSE]
                coo <- coord.var
                nrow.coord.var <- nrow.coord.var + nrow(coord.var)
                for (v in 1:nrow(coord.var)) {
                  arrows(0, 0, coord.var[v, 1], coord.var[v, 
                    2], length = 0.1, angle = 15, code = 2, col = col[v])
                  if (lab.var) {
                    if (abs(coord.var[v, 1]) > abs(coord.var[v, 
                      2])) {
                      if (coord.var[v, 1] >= 0) 
                        posi <- c(posi, 4)
                      else posi <- c(posi, 2)
                    }
                    else {
                      if (coord.var[v, 2] >= 0) 
                        posi <- c(posi, 3)
                      else posi <- c(posi, 1)
                    }
                  }
                }
            }
        }
        if ((!is.null(res.mfa$quanti.var.sup$coord)) & (is.na(test.invisible[2]))) {
            if (!is.null(res.mfa$quanti.var.sup$coord[which(apply(res.mfa$quanti.var.sup$cos2[, 
                axes, drop = FALSE], 1, sum, na.rm = TRUE) >= 
                lim.cos2.var), ])) {
                coord.quanti <- res.mfa$quanti.var.sup$cor[which(apply(res.mfa$quanti.var.sup$cos2[, 
                  axes, drop = FALSE], 1, sum, na.rm = TRUE) >= 
                  lim.cos2.var), axes, drop = FALSE]
                coo <- rbind(coo, coord.quanti)
                for (q in 1:nrow(coord.quanti)) {
                  arrows(0, 0, coord.quanti[q, 1], coord.quanti[q, 
                    2], length = 0.1, angle = 15, code = 2, lty = 2, 
                    col = col[nrow.coord.var + q], ...)
                  if (lab.var) {
                    if (abs(coord.quanti[q, 1]) > abs(coord.quanti[q, 
                      2])) {
                      if (coord.quanti[q, 1] >= 0) 
                        posi <- c(posi, 4)
                      else posi <- c(posi, 2)
                    }
                    else {
                      if (coord.quanti[q, 2] >= 0) 
                        posi <- c(posi, 3)
                      else posi <- c(posi, 1)
                    }
                  }
                }
            }
        }
        if (autoLab == "auto") 
            autoLab = (length(labe) < 50)
        if (autoLab == FALSE) 
            text(coo[, 1], y = coo[, 2], labels = labe, pos = posi, 
                col = col, ...)
        if (autoLab == TRUE) 
            autoLab(coo[, 1], y = coo[, 2], labels = labe, col = col, 
                shadotext = shadowtext, ...)
        par(mar = c(5, 4, 4, 2) + 0.1)
    }
    if (choix == "freq") {
        if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) 
            dev.new()
        if (is.null(col.hab)) 
            col.hab = c("black", "grey60", "darkblue", "blue")
        col.row = col.hab[1]
        col.row.sup = col.hab[2]
        col.col = col.hab[3]
        col.col.sup = col.hab[4]
        coord.col <- res.mfa$freq$coord[, axes, drop = FALSE]
        coord.row <- res.mfa$ind$coord[, axes]
        coord.row.sup <- coord.col.sup <- NULL
        if (!is.null(res.mfa$ind.sup)) 
            coord.row.sup <- res.mfa$ind.sup$coord[, axes, drop = FALSE]
        if (!is.null(res.mfa$freq.sup)) 
            coord.col.sup <- res.mfa$freq.sup$coord[, axes, drop = FALSE]
        test.invisible <- vector(length = 4)
        if (!is.null(invisible)) {
            test.invisible[1] <- match("row", invisible)
            test.invisible[2] <- match("col", invisible)
            test.invisible[3] <- match("row.sup", invisible)
            test.invisible[4] <- match("col.sup", invisible)
        }
        else test.invisible <- rep(NA, 4)
        if (is.null(xlim)) {
            xmin <- xmax <- 0
            if (is.na(test.invisible[1])) 
                xmin <- min(xmin, coord.row[, 1])
            if (is.na(test.invisible[1])) 
                xmax <- max(xmax, coord.row[, 1])
            if (is.na(test.invisible[2])) 
                xmin <- min(xmin, coord.col[, 1])
            if (is.na(test.invisible[2])) 
                xmax <- max(xmax, coord.col[, 1])
            if (is.na(test.invisible[3])) 
                xmin <- min(xmin, coord.row.sup[, 1])
            if (is.na(test.invisible[3])) 
                xmax <- max(xmax, coord.row.sup[, 1])
            if (is.na(test.invisible[4])) 
                xmin <- min(xmin, coord.col.sup[, 1])
            if (is.na(test.invisible[4])) 
                xmax <- max(xmax, coord.col.sup[, 1])
            xlim <- c(xmin, xmax) * 1.2
        }
        else {
            xmin = xlim[1]
            xmax = xlim[2]
        }
        if (is.null(ylim)) {
            ymin <- ymax <- 0
            if (is.na(test.invisible[1])) 
                ymin <- min(ymin, coord.row[, 2])
            if (is.na(test.invisible[1])) 
                ymax <- max(ymax, coord.row[, 2])
            if (is.na(test.invisible[2])) 
                ymin <- min(ymin, coord.col[, 2])
            if (is.na(test.invisible[2])) 
                ymax <- max(ymax, coord.col[, 2])
            if (is.na(test.invisible[3])) 
                ymin <- min(ymin, coord.row.sup[, 2])
            if (is.na(test.invisible[3])) 
                ymax <- max(ymax, coord.row.sup[, 2])
            if (is.na(test.invisible[4])) 
                ymin <- min(ymin, coord.col.sup[, 2])
            if (is.na(test.invisible[4])) 
                ymax <- max(ymax, coord.col.sup[, 2])
            ylim <- c(ymin, ymax) * 1.2
        }
        else {
            ymin = ylim[1]
            ymax = ylim[2]
        }
        col <- NULL
        if (habillage == "group") {
            if (is.null(col.hab) | length(col.hab) < length(group[type == 
                "f"])) 
                col.hab <- 2:(length(group[type == "f"]) + 1)
            for (i in 1:length(group[type == "f"])) col <- c(col, 
                rep(col.hab[i], group[type == "f"][i]))
        }
        else {
            if (is.null(col.hab) | length(col.hab) < sum(group[type == 
                "f"])) 
                col <- rep(1, sum(group[type == "f"]))
            else col <- col.hab
        }
        if (is.null(title)) 
            titre <- "Factor map for the contingency table(s)"
        else titre <- title
        plot(0, 0, main = titre, xlab = lab.x, ylab = lab.y, 
            xlim = xlim, ylim = ylim, col = "white", asp = 1, 
            ...)
        abline(h = 0, lty = 2, ...)
        abline(v = 0, lty = 2, ...)
        selection <- selectionC <- selectionS <- selectionCS <- NULL
        if (!is.null(select)) {
            if (mode(select) == "numeric") 
                selection <- select
            else {
                if (sum(rownames(res.mfa$freq.sup$coord) %in% 
                  select) + sum(rownames(res.mfa$freq$coord) %in% 
                  select) + sum(rownames(res.mfa$ind$coord) %in% 
                  select) + sum(rownames(res.mfa$ind.sup$coord) %in% 
                  select) != 0) 
                  selection <- which(rownames(res.mfa$ind$coord) %in% 
                    select)
                else {
                  if (grepl("contrib", select)) 
                    selection <- (rev(order(res.mfa$ind$contrib[, 
                      axes[1], drop = FALSE] * res.mfa$eig[axes[1], 
                      1] + res.mfa$ind$contrib[, axes[2], drop = FALSE] * 
                      res.mfa$eig[axes[2], 1])))[1:min(nrow(res.mfa$ind$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "contrib"))), na.rm = T))]
                  if (grepl("inertia", select)) 
                    selection <- (rev(order(apply(res.mfa$ind$within.inertia[, 
                      axes], 1, sum))))[1:min(nrow(res.mfa$ind$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "inertia"))), na.rm = T))]
                  if (grepl("coord", select)) 
                    selection <- (rev(order(apply(res.mfa$ind$coord[, 
                      axes]^2, 1, sum))))[1:min(nrow(res.mfa$ind$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "coord"))), na.rm = T))]
                  if (grepl("cos2", select)) {
                    if (sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T) >= 1) 
                      selection <- (rev(order(apply(res.mfa$ind$cos2[, 
                        axes], 1, sum))))[1:min(nrow(res.mfa$ind$coord), 
                        sum(as.numeric(unlist(strsplit(select, 
                          "cos2"))), na.rm = T))]
                    else selection <- which(apply(res.mfa$ind$cos2[, 
                      axes], 1, sum) > sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T))
                  }
                  if (is.integer(select)) 
                    selection <- select
                }
            }
        }
        if ((!is.null(select)) & (!is.null(res.mfa$ind.sup$coord))) {
            if (mode(select) == "numeric") 
                selectionS <- select
            else {
                if (sum(rownames(res.mfa$freq.sup$coord) %in% 
                  select) + sum(rownames(res.mfa$freq$coord) %in% 
                  select) + sum(rownames(res.mfa$ind$coord) %in% 
                  select) + sum(rownames(res.mfa$ind.sup$coord) %in% 
                  select) != 0) 
                  selectionS <- which(rownames(res.mfa$ind.sup$coord) %in% 
                    select)
                else {
                  if (grepl("contrib", select)) 
                    selectionS <- NULL
                  if (grepl("inertia", select)) 
                    selectionS <- (rev(order(apply(res.mfa$ind.sup$within.inertia[, 
                      axes]^2, 1, sum))))[1:min(nrow(res.mfa$ind.sup$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "inertia"))), na.rm = T))]
                  if (grepl("coord", select)) 
                    selectionS <- (rev(order(apply(res.mfa$ind.sup$coord[, 
                      axes]^2, 1, sum))))[1:min(nrow(res.mfa$ind.sup$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "coord"))), na.rm = T))]
                  if (grepl("cos2", select)) {
                    if (sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T) >= 1) 
                      selectionS <- (rev(order(apply(res.mfa$ind.sup$cos2[, 
                        axes], 1, sum))))[1:min(nrow(res.mfa$ind.sup$coord), 
                        sum(as.numeric(unlist(strsplit(select, 
                          "cos2"))), na.rm = T))]
                    else selectionS <- which(apply(res.mfa$ind.sup$cos2[, 
                      axes], 1, sum) > sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T))
                  }
                  if (is.integer(select)) 
                    selectionS <- select
                }
            }
        }
        if ((!is.null(select)) & (!is.null(res.mfa$freq$coord))) {
            if (mode(select) == "numeric") 
                selectionC <- select
            else {
                if (sum(rownames(res.mfa$freq.sup$coord) %in% 
                  select) + sum(rownames(res.mfa$freq$coord) %in% 
                  select) + sum(rownames(res.mfa$ind$coord) %in% 
                  select) + sum(rownames(res.mfa$ind.sup$coord) %in% 
                  select) != 0) 
                  selectionC <- which(rownames(res.mfa$freq$coord) %in% 
                    select)
                else {
                  if (grepl("contrib", select)) 
                    selectionC <- (rev(order(res.mfa$freq$contrib[, 
                      axes[1], drop = FALSE] * res.mfa$eig[axes[1], 
                      1] + res.mfa$freq$contrib[, axes[2], drop = FALSE] * 
                      res.mfa$eig[axes[2], 1])))[1:min(nrow(res.mfa$freq$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "contrib"))), na.rm = T))]
                  if (grepl("coord", select)) 
                    selectionC <- (rev(order(apply(res.mfa$freq$coord[, 
                      axes]^2, 1, sum))))[1:min(nrow(res.mfa$freq$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "coord"))), na.rm = T))]
                  if (grepl("cos2", select)) {
                    if (sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T) >= 1) 
                      selectionC <- (rev(order(apply(res.mfa$freq$cos2[, 
                        axes], 1, sum))))[1:min(nrow(res.mfa$freq$coord), 
                        sum(as.numeric(unlist(strsplit(select, 
                          "cos2"))), na.rm = T))]
                    else selectionC <- which(apply(res.mfa$freq$cos2[, 
                      axes], 1, sum) > sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T))
                  }
                  if (is.integer(select)) 
                    selectionC <- select
                }
            }
        }
        if ((!is.null(select)) & (!is.null(res.mfa$freq.sup$coord))) {
            if (mode(select) == "numeric") 
                selectionCS <- select
            else {
                if (sum(rownames(res.mfa$freq.sup$coord) %in% 
                  select) + sum(rownames(res.mfa$freq$coord) %in% 
                  select) + sum(rownames(res.mfa$ind$coord) %in% 
                  select) + sum(rownames(res.mfa$ind.sup$coord) %in% 
                  select) != 0) 
                  selectionCS <- which(rownames(res.mfa$freq.sup$coord) %in% 
                    select)
                else {
                  if (grepl("contrib", select)) 
                    selectionCS <- NULL
                  if (grepl("coord", select)) 
                    selectionCS <- (rev(order(apply(res.mfa$freq.sup$coord[, 
                      axes]^2, 1, sum))))[1:min(nrow(res.mfa$freq.sup$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "coord"))), na.rm = T))]
                  if (grepl("cos2", select)) {
                    if (sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T) >= 1) 
                      selectionCS <- (rev(order(apply(res.mfa$freq.sup$cos2[, 
                        axes], 1, sum))))[1:min(nrow(res.mfa$freq.sup$coord), 
                        sum(as.numeric(unlist(strsplit(select, 
                          "cos2"))), na.rm = T))]
                    else selectionCS <- which(apply(res.mfa$freq.sup$cos2[, 
                      axes], 1, sum) > sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T))
                  }
                  if (is.integer(select)) 
                    selectionCS <- select
                }
            }
        }
        coo <- labe <- coll <- ipch <- fonte <- NULL
        if (is.na(test.invisible[1])) {
            coo <- rbind(coo, coord.row)
            if (lab.ind) {
                labe <- rownames(coord.row)
            }
            else labe <- rep("", nrow(coord.row))
            coll <- rep(col.row, nrow(coord.row))
            ipch <- c(ipch, rep(20, nrow(coord.row)))
            fonte <- c(fonte, rep(1, nrow(coord.row)))
            if (!is.null(selection)) {
                if (is.numeric(unselect)) 
                  coll[!((1:length(coll)) %in% selection)] = rgb(t(col2rgb(coll[!((1:length(coll)) %in% 
                    selection)])), alpha = 255 * (1 - unselect), 
                    maxColorValue = 255)
                else coll[!((1:length(coll)) %in% selection)] = unselect
                labe[!((1:length(coll)) %in% selection)] <- ""
            }
        }
        if (is.na(test.invisible[2])) {
            coo <- rbind(coo, coord.col)
            if (lab.ind) {
                labe2 <- rownames(coord.col)
            }
            else labe2 <- rep("", nrow(coord.col))
            coll2 <- rep(col.col, nrow(coord.col))
            ipch <- c(ipch, rep(17, nrow(coord.col)))
            fonte <- c(fonte, rep(1, nrow(coord.col)))
            if (!is.null(selectionC)) {
                if (is.numeric(unselect)) 
                  coll2[!((1:length(coll2)) %in% selectionC)] = rgb(t(col2rgb(coll2[!((1:length(coll2)) %in% 
                    selectionC)])), alpha = 255 * (1 - unselect), 
                    maxColorValue = 255)
                else coll2[!((1:length(coll2)) %in% selectionC)] = unselect
                labe2[!((1:length(coll2)) %in% selectionC)] <- ""
            }
            coll <- c(coll, coll2)
            labe <- c(labe, labe2)
        }
        if (!is.null(res.mfa$freq.sup) & is.na(test.invisible[4])) {
            coo <- rbind(coo, coord.col.sup)
            if (lab.ind) {
                labe2 <- rownames(coord.col.sup)
            }
            else labe2 <- rep("", nrow(coord.col.sup))
            coll2 <- rep(col.col.sup, nrow(coord.col.sup))
            ipch <- c(ipch, rep(17, nrow(coord.col.sup)))
            fonte <- c(fonte, rep(1, nrow(coord.col.sup)))
            if (!is.null(selectionCS)) {
                if (is.numeric(unselect)) 
                  coll2[!((1:length(coll2)) %in% selectionCS)] = rgb(t(col2rgb(coll2[!((1:length(coll2)) %in% 
                    selectionCS)])), alpha = 255 * (1 - unselect), 
                    maxColorValue = 255)
                else coll2[!((1:length(coll2)) %in% selectionCS)] = unselect
                labe2[!((1:length(coll2)) %in% selectionCS)] <- ""
            }
            coll <- c(coll, coll2)
            labe <- c(labe, labe2)
        }
        if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[3])) {
            coo <- rbind(coo, coord.row.sup)
            if (lab.ind) {
                labe2 <- rownames(coord.row.sup)
            }
            else labe2 <- rep("", nrow(coord.row.sup))
            coll2 <- rep(col.row.sup, nrow(coord.row.sup))
            ipch <- c(ipch, rep(17, nrow(coord.row.sup)))
            fonte <- c(fonte, rep(1, nrow(coord.row.sup)))
            if (!is.null(selectionS)) {
                if (is.numeric(unselect)) 
                  coll2[!((1:length(coll2)) %in% selectionS)] = rgb(t(col2rgb(coll2[!((1:length(coll2)) %in% 
                    selectionS)])), alpha = 255 * (1 - unselect), 
                    maxColorValue = 255)
                else coll2[!((1:length(coll2)) %in% selectionS)] = unselect
                labe2[!((1:length(coll2)) %in% selectionS)] <- ""
            }
            if (length(select) == 1) {
                if (grepl("contrib", select)) {
                  if (is.numeric(unselect)) 
                    coll2[1:length(coll2)] = rgb(t(col2rgb(coll2[1:length(coll2)])), 
                      alpha = 255 * (1 - unselect), maxColorValue = 255)
                  else coll2[1:length(coll2)] = unselect
                  labe2[1:length(coll2)] <- ""
                }
            }
            coll <- c(coll, coll2)
            labe <- c(labe, labe2)
        }
        if (shadowtext) 
            points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, 
                ...)
        if (any(labe != "")) {
            if (autoLab == "auto") 
                autoLab = (length(which(labe != "")) < 50)
            if (autoLab == TRUE) 
                autoLab(coo[labe != "", 1], y = coo[labe != "", 
                  2], labels = labe[labe != ""], col = coll[labe != 
                  ""], font = fonte[labe != ""], shadotext = shadowtext, 
                  ...)
            if (autoLab == FALSE) 
                text(coo[labe != "", 1], y = coo[labe != "", 
                  2], labels = labe[labe != ""], col = coll[labe != 
                  ""], font = fonte[labe != ""], pos = 3, ...)
        }
        if (!shadowtext) 
            points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, 
                ...)
        if (habillage == "group") 
            legend("topleft", legend = rownames(res.mfa$group$Lg[-nrow(res.mfa$group$Lg), 
                ])[type == "f"], text.col = col.hab, cex = 0.8 * 
                par("cex"))
    }
    if (choix == "ind") {
        test.invisible <- vector(length = 3)
        if (!is.null(invisible)) {
            test.invisible[1] <- match("ind", invisible)
            test.invisible[2] <- match("ind.sup", invisible)
            test.invisible[3] <- match("quali", invisible)
        }
        else test.invisible <- rep(NA, 3)
        nb.ind.actif <- nrow(res.mfa$ind$coord)
        nb.ind.illu <- 0
        if (!is.null(res.mfa$ind.sup)) 
            nb.ind.illu <- nrow(res.mfa$ind.sup$coord)
        nb.ind <- nb.ind.actif + nb.ind.illu
        coord.ind <- res.mfa$ind$coord[, axes, drop = FALSE]
        coord.ind.partiel <- res.mfa$ind$coord.partiel[, axes, 
            drop = FALSE]
        coord.ind.sup <- NULL
        if (!is.null(res.mfa$ind.sup)) {
            coord.ind.sup <- res.mfa$ind.sup$coord[, axes, drop = FALSE]
            coord.ind.partiel.sup <- res.mfa$ind.sup$coord.partiel[, 
                axes, drop = FALSE]
        }
        coord.quali <- coord.quali.sup <- coord.quali.partiel <- coord.quali.sup.partiel <- NULL
        nrow.coord.quali <- 0
        if (!is.null(res.mfa["quali.var"]$quali.var)) {
            coord.quali <- res.mfa$quali.var$coord[, axes, drop = FALSE]
            coord.quali.partiel <- res.mfa$quali.var$coord.partiel[, 
                axes, drop = FALSE]
            nrow.coord.quali <- nrow(coord.quali)
        }
        if (!is.null(res.mfa["quali.var.sup"])) {
            coord.quali.sup <- res.mfa$quali.var.sup$coord[, 
                axes, drop = FALSE]
            coord.quali.partiel.sup <- res.mfa$quali.var.sup$coord.partiel[, 
                axes, drop = FALSE]
        }
        group.ind.actif <- group.ind.sup <- group.quali <- group.quali.sup <- NULL
        if (!is.null(partial)) {
            if (length(partial) == 1) {
                if (partial == "all") {
                  group.ind.actif <- 1:nrow(coord.ind)
                  if (!is.null(res.mfa$ind.sup)) 
                    group.ind.sup <- 1:nrow(coord.ind.sup)
                  if (!is.null(res.mfa["quali.var"]$quali.var)) 
                    group.quali <- 1:nrow(coord.quali)
                  if (!is.null(res.mfa["quali.var.sup"]$quali.var.sup)) 
                    group.quali.sup <- 1:nrow(coord.quali.sup)
                }
                else {
                  for (i in 1:length(partial)) {
                    if (partial[i] %in% rownames(coord.ind)) 
                      group.ind.actif <- c(group.ind.actif, match(partial[i], 
                        rownames(coord.ind)))
                    if (partial[i] %in% rownames(coord.ind.sup)) 
                      group.ind.sup <- c(group.ind.sup, match(partial[i], 
                        rownames(coord.ind.sup)))
                    if (partial[i] %in% rownames(coord.quali)) 
                      group.quali <- c(group.quali, match(partial[i], 
                        rownames(coord.quali)))
                    if (partial[i] %in% rownames(coord.quali.sup)) 
                      group.quali.sup <- c(group.quali.sup, match(partial[i], 
                        rownames(coord.quali.sup)))
                  }
                }
            }
            else {
                for (i in 1:length(partial)) {
                  if (partial[i] %in% rownames(coord.ind)) 
                    group.ind.actif <- c(group.ind.actif, match(partial[i], 
                      rownames(coord.ind)))
                  if (partial[i] %in% rownames(coord.ind.sup)) 
                    group.ind.sup <- c(group.ind.sup, match(partial[i], 
                      rownames(coord.ind.sup)))
                  if (partial[i] %in% rownames(coord.quali)) 
                    group.quali <- c(group.quali, match(partial[i], 
                      rownames(coord.quali)))
                  if (partial[i] %in% rownames(coord.quali.sup)) 
                    group.quali.sup <- c(group.quali.sup, match(partial[i], 
                      rownames(coord.quali.sup)))
                }
            }
        }
        if (!is.null(ellipse)) {
            coord.ellipse <- ellipse$res
            npoint.ellipse <- ellipse$call
        }
        else coord.ellipse <- NULL
        if (!is.null(ellipse.par)) {
            coord.ellipse.par <- ellipse.par$res
            npoint.ellipse.par <- ellipse.par$call
        }
        else coord.ellipse.par <- NULL
        if (is.null(xlim)) {
            xmin <- xmax <- 0
            if (is.na(test.invisible[1])) 
                xmin <- min(xmin, coord.ind[, 1])
            if (is.na(test.invisible[1])) 
                xmax <- max(xmax, coord.ind[, 1])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                xmin <- min(xmin, coord.ind.sup[, 1])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                xmax <- max(xmax, coord.ind.sup[, 1])
            if (is.na(test.invisible[1])) 
                xmin <- min(xmin, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (is.na(test.invisible[1])) 
                xmax <- max(xmax, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                xmin <- min(xmin, coord.ind.partiel.sup[unlist(lapply(group.ind.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                xmax <- max(xmax, coord.ind.partiel.sup[unlist(lapply(group.ind.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                xmin <- min(xmin, coord.quali[, 1])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                xmax <- max(xmax, coord.quali[, 1])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                xmin <- min(xmin, coord.quali.partiel[unlist(lapply(group.quali, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                xmax <- max(xmax, coord.quali.partiel[unlist(lapply(group.quali, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                xmin <- min(xmin, coord.quali[, 1], coord.quali.sup[, 
                  1])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                xmax <- max(xmax, coord.quali[, 1], coord.quali.sup[, 
                  1])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                xmin <- min(xmin, coord.quali.partiel.sup[unlist(lapply(group.quali.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                xmax <- max(xmax, coord.quali.partiel.sup[unlist(lapply(group.quali.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  1])
            xlim <- c(xmin, xmax) * 1.1
        }
        else {
            xmin = xlim[1]
            xmax = xlim[2]
        }
        if (is.null(ylim)) {
            ymin <- ymax <- 0
            if (is.na(test.invisible[1])) 
                ymin <- min(ymin, coord.ind[, 2])
            if (is.na(test.invisible[1])) 
                ymax <- max(ymax, coord.ind[, 2])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                ymin <- min(ymin, coord.ind.sup[, 2])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                ymax <- max(ymax, coord.ind.sup[, 2])
            if (is.na(test.invisible[1])) 
                ymin <- min(ymin, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (is.na(test.invisible[1])) 
                ymax <- max(ymax, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                ymin <- min(ymin, coord.ind.partiel.sup[unlist(lapply(group.ind.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) 
                ymax <- max(ymax, coord.ind.partiel.sup[unlist(lapply(group.ind.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                ymin <- min(ymin, coord.quali[, 2])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                ymax <- max(ymax, coord.quali[, 2])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                ymin <- min(ymin, coord.quali.partiel[unlist(lapply(group.quali, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
                ymax <- max(ymax, coord.quali.partiel[unlist(lapply(group.quali, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                ymin <- min(ymin, coord.quali[, 1], coord.quali.sup[, 
                  2])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                ymax <- max(ymax, coord.quali[, 1], coord.quali.sup[, 
                  2])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                ymin <- min(ymin, coord.quali.partiel.sup[unlist(lapply(group.quali.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            if (!is.null(res.mfa$quali.var.sup) & is.na(test.invisible[3])) 
                ymax <- max(ymax, coord.quali.partiel.sup[unlist(lapply(group.quali.sup, 
                  function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                  2])
            ylim <- c(ymin, ymax) * 1.1
        }
        else {
            ymin = ylim[1]
            ymax = ylim[2]
        }
        selection <- NULL
        if (!is.null(select)) {
            if (mode(select) == "numeric") 
                selection <- select
            else {
                if (sum(rownames(res.mfa$ind$coord) %in% select) != 
                  0) 
                  selection <- which(rownames(res.mfa$ind$coord) %in% 
                    select)
                else {
                  if (grepl("contrib", select)) 
                    selection <- (rev(order(res.mfa$ind$contrib[, 
                      axes[1], drop = FALSE] * res.mfa$eig[axes[1], 
                      1] + res.mfa$ind$contrib[, axes[2], drop = FALSE] * 
                      res.mfa$eig[axes[2], 1])))[1:min(nrow(res.mfa$ind$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "contrib"))), na.rm = T))]
                  if (grepl("dist", select)) 
                    selection <- (rev(order(res.mfa$ind$dist)))[1:min(nrow(res.mfa$ind$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "dist"))), na.rm = T))]
                  if (grepl("coord", select)) 
                    selection <- (rev(order(apply(res.mfa$ind$coord[, 
                      axes]^2, 1, sum))))[1:min(nrow(res.mfa$ind$coord), 
                      sum(as.integer(unlist(strsplit(select, 
                        "coord"))), na.rm = T))]
                  if (grepl("cos2", select)) {
                    if (sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T) >= 1) 
                      selection <- (rev(order(apply(res.mfa$ind$cos2[, 
                        axes], 1, sum))))[1:min(nrow(res.mfa$ind$coord), 
                        sum(as.numeric(unlist(strsplit(select, 
                          "cos2"))), na.rm = T))]
                    else selection <- which(apply(res.mfa$ind$cos2[, 
                      axes], 1, sum) > sum(as.numeric(unlist(strsplit(select, 
                      "cos2"))), na.rm = T))
                  }
                  if (is.integer(select)) 
                    selection <- select
                }
            }
        }
        if (habillage == "group") {
            if (is.null(col.hab) | length(col.hab) != (nbre.grpe)) 
                col.hab <- 2:(nbre.grpe + 1)
            col.ind <- c(rep(1, nb.ind.actif), rep(col.hab, nb.ind.actif))
            if (!is.null(res.mfa$ind.sup)) 
                col.ind.sup <- c(rep(1, nb.ind - nb.ind.actif), 
                  rep(col.hab, nb.ind - nb.ind.actif))
            if (length(group[type == "n"]) != 0) 
                col.quali <- c(rep(1, sum(res.mfa$call$group.mod[type == 
                  "n"])), rep(col.hab, sum(res.mfa$call$group.mod[type == 
                  "n"])))
            if (!is.null(res.mfa$quali.var.sup)) 
                col.quali.sup <- c(rep(1, sum(res.mfa$call$group.mod[num.group.sup][type.sup == 
                  "n"])), rep(col.hab, sum(res.mfa$call$group.mod[num.group.sup][type.sup == 
                  "n"])))
            if (!is.null(ellipse)) 
                col.ellipse <- rep(1, nb.ind.actif)
            if (!is.null(ellipse.par)) 
                col.ellipse.par <- rep(col.hab, nb.ind.actif)
        }
        if (habillage == "ind") {
            if (is.null(col.hab) | length(col.hab) != nb.ind) 
                col.hab <- 1:nb.ind
            col.ind <- c(col.hab[1:nb.ind.actif], rep(col.hab[1:nb.ind.actif], 
                each = nbre.grpe))
            if (!is.null(res.mfa$ind.sup)) 
                col.ind.sup <- c(col.hab[(nb.ind.actif + 1):nb.ind], 
                  rep(col.hab[(nb.ind.actif + 1):nb.ind], each = nbre.grpe))
            if (length(group[type == "n"]) != 0) 
                col.quali <- col.quali.sup <- rep("black", (1 + 
                  nbre.grpe) * sum(res.mfa$call$group.mod[type == 
                  "n"]))
            if (!is.null(ellipse)) 
                col.ellipse <- col.hab[1:nb.ind.actif]
            if (!is.null(ellipse.par)) 
                col.ellipse.par <- rep(col.hab[1:nb.ind.actif], 
                  each = nbre.grpe)
        }
        if ((habillage != "none") & (habillage != "ind") & (habillage != 
            "group")) {
            group.act <- (1:length(group))
            if (!is.null(num.group.sup)) 
                group.act <- group.act[-num.group.sup]
            nbre.modalite <- nbre.modalite.sup <- NULL
            liste.quali <- liste.quali.sup <- NULL
            for (i in group.act) {
                if (type[i] == "n") {
                  for (k in 1:ncol(res.mfa$separate.analyses[[i]]$call$X)) nbre.modalite <- c(nbre.modalite, 
                    nlevels(res.mfa$separate.analyses[[i]]$call$X[, 
                      k]))
                  if (i == 1) 
                    liste.quali <- c(liste.quali, colnames(res.mfa$call$X[1:group[1]]))
                  else liste.quali <- c(liste.quali, colnames(res.mfa$call$X[(sum(group[1:(i - 
                    1)]) + 1):sum(group[1:i])]))
                }
            }
            if (!is.null(num.group.sup)) {
                for (i in num.group.sup) {
                  if (type[i] == "n") {
                    if (i == 1) 
                      liste.quali.sup <- c(liste.quali.sup, colnames(res.mfa$call$X[1:group[1]]))
                    else liste.quali.sup <- c(liste.quali.sup, 
                      colnames(res.mfa$call$X[(sum(group[1:(i - 
                        1)]) + 1):sum(group[1:i])]))
                    for (k in 1:ncol(res.mfa$separate.analyses[[i]]$call$X)) nbre.modalite.sup <- c(nbre.modalite.sup, 
                      nlevels(res.mfa$separate.analyses[[i]]$call$X[, 
                        k]))
                  }
                }
            }
            if (is.double(habillage)) 
                nom.quali <- colnames(res.mfa$call$X)[habillage]
            else nom.quali = habillage
            if (!(nom.quali %in% c(liste.quali, liste.quali.sup))) 
                stop("The variable ", habillage, " is not qualitative")
            modalite <- levels(as.factor(res.mfa$call$X[, nom.quali]))
            col.ind <- as.numeric(as.factor(res.mfa$call$X[, 
                nom.quali]))
            if (is.null(col.hab) | length(col.hab) != length(modalite)) 
                col.hab <- 2:(1 + length(modalite))
            col.ind <- col.hab[col.ind]
            if (!is.null(res.mfa$call$ind.sup)) {
                col.ind.sup <- col.ind[res.mfa$call$ind.sup]
                col.ind <- col.ind[-res.mfa$call$ind.sup]
                col.ind.sup <- c(col.ind.sup, rep(col.ind.sup, 
                  each = nbre.grpe))
            }
            col.ind <- c(col.ind, rep(col.ind, each = nbre.grpe))
            col.ellipse <- col.ind[1:nb.ind.actif]
            col.ellipse.par <- col.ind[-c(1:nb.ind.actif)]
            col.quali <- rep("black", sum(res.mfa$call$group.mod[type == 
                "n"]))
            if (nom.quali %in% liste.quali) {
                indice.inf <- sum(nbre.modalite[0:(match(nom.quali, 
                  liste.quali) - 1)]) + 1
                indice.sup <- indice.inf + length(modalite) - 
                  1
                if (length(group[type == "n"]) != 0) {
                  for (i in 1:length(liste.quali)) {
                    if (liste.quali[i] == nom.quali) 
                      col.quali[indice.inf:indice.sup] <- col.hab
                  }
                }
            }
            col.quali <- c(col.quali, rep(col.quali, each = nbre.grpe))
            col.quali.sup <- rep("black", sum(res.mfa$call$group.mod[(type == 
                "n") %in% num.group.sup]))
            if (nom.quali %in% liste.quali.sup) {
                indice.inf.sup <- sum(nbre.modalite.sup[0:(match(nom.quali, 
                  liste.quali.sup) - 1)]) + 1
                indice.sup.sup <- indice.inf.sup + length(modalite) - 
                  1
                if (length(group[type == "n"]) != 0) {
                  for (i in 1:length(liste.quali.sup)) {
                    if (liste.quali.sup[i] == nom.quali) 
                      col.quali.sup[indice.inf.sup:indice.sup.sup] <- col.hab
                  }
                }
            }
            col.quali.sup <- c(col.quali.sup, rep(col.quali.sup, 
                each = nbre.grpe))
        }
        if (habillage == "none") 
            col.ind <- col.ind.sup <- col.quali.sup <- col.quali <- col.ellipse <- col.ellipse.par <- rep("black", 
                nb.ind * (nbre.grpe + 1))
        if ((new.plot) & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) 
            dev.new(width = min(14, max(8, 8 * (xmax - xmin)/(ymax - 
                ymin))), height = 8)
        if (is.null(title)) 
            title <- "Individual factor map"
        plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, 
            xlim = xlim, ylim = ylim, col = "white", asp = 1, 
            ...)
        abline(v = 0, lty = 2, ...)
        abline(h = 0, lty = 2, ...)
        coo <- labe <- coll <- ipch <- fonte <- NULL
        if (is.na(test.invisible[1])) {
            coo <- rbind(coo, coord.ind)
            if (lab.ind) {
                labe <- c(labe, rownames(coord.ind))
            }
            else labe <- c(labe, rep("", nrow(coord.ind)))
            coll <- c(coll, col.ind[1:nb.ind.actif])
            ipch <- c(ipch, rep(20, nrow(coord.ind)))
            fonte <- c(fonte, rep(1, nrow(coord.ind)))
            if (!is.null(selection)) {
                if (is.numeric(unselect)) 
                  coll[!((1:length(coll)) %in% selection)] = rgb(t(col2rgb(coll[!((1:length(coll)) %in% 
                    selection)])), alpha = 255 * (1 - unselect), 
                    maxColorValue = 255)
                else coll[!((1:length(coll)) %in% selection)] = unselect
                labe[!((1:length(coll)) %in% selection)] <- ""
            }
            for (i in group.ind.actif) {
                if (col2rgb(coll[i], alpha = TRUE)[4] == 255) {
                  for (j in 1:nbre.grpe) {
                    points(coord.ind.partiel[(i - 1) * nbre.grpe + 
                      j, ], cex = 0.8 * par("cex"), col = col.ind[nb.ind.actif + 
                      (i - 1) * nbre.grpe + j], pch = 20)
                    if (lab.par) 
                      text(coord.ind.partiel[(i - 1) * nbre.grpe + 
                        j, 1], y = coord.ind.partiel[(i - 1) * 
                        nbre.grpe + j, 2], labels = rownames(coord.ind.partiel)[(i - 
                        1) * nbre.grpe + j], pos = 3, col = col.ind[nb.ind.actif + 
                        (i - 1) * nbre.grpe + j], ...)
                    if (chrono) {
                      if (j > 1) 
                        lines(c(coord.ind.partiel[(i - 1) * nbre.grpe + 
                          (j - 1), 1], coord.ind.partiel[(i - 
                          1) * nbre.grpe + j, 1]), c(coord.ind.partiel[(i - 
                          1) * nbre.grpe + (j - 1), 2], coord.ind.partiel[(i - 
                          1) * nbre.grpe + j, 2]), col = col.ind[i], 
                          ...)
                    }
                    else lines(c(coord.ind[i, 1], coord.ind.partiel[(i - 
                      1) * nbre.grpe + j, 1]), c(coord.ind[i, 
                      2], coord.ind.partiel[(i - 1) * nbre.grpe + 
                      j, 2]), col = col.ind[nb.ind.actif + (i - 
                      1) * nbre.grpe + j], lty = j, ...)
                  }
                }
            }
        }
        if (!is.null(res.mfa$ind.sup) & is.na(test.invisible[2])) {
            coo <- rbind(coo, coord.ind.sup)
            if (lab.ind) {
                labe <- c(labe, rownames(coord.ind.sup))
            }
            else labe <- c(labe, rep("", nrow(coord.ind.sup)))
            coll <- c(coll, col.ind.sup[1:(nb.ind - nb.ind.actif)])
            ipch <- c(ipch, rep(21, nrow(coord.ind.sup)))
            fonte <- c(fonte, rep(3, nrow(coord.ind.sup)))
            for (i in group.ind.sup) {
                for (j in 1:nbre.grpe) {
                  points(coord.ind.partiel.sup[(i - 1) * nbre.grpe + 
                    j, ], cex = 0.8 * par("cex"), col = col.ind.sup[nb.ind - 
                    nb.ind.actif + (i - 1) * nbre.grpe + j], 
                    pch = 21)
                  if (lab.par) 
                    text(coord.ind.partiel.sup[(i - 1) * nbre.grpe + 
                      j, 1], y = coord.ind.partiel.sup[nb.ind + 
                      (i - 1) * nbre.grpe + j, 2], labels = rownames(coord.ind.partiel.sup)[(i - 
                      1) * nbre.grpe + j], pos = 3, col = col.ind.sup[nb.ind - 
                      nb.ind.actif + (i - 1) * nbre.grpe + j], 
                      cex = par("cex") * 0.8)
                  if (chrono) {
                    if (j > 1) 
                      lines(c(coord.ind.partiel.sup[(i - 1) * 
                        nbre.grpe + (j - 1), 1], coord.ind.partiel.sup[(i - 
                        1) * nbre.grpe + j, 1]), c(coord.ind.partiel.sup[(i - 
                        1) * nbre.grpe + (j - 1), 2], coord.ind.partiel.sup[(i - 
                        1) * nbre.grpe + j, 2]), col = col.ind[nb.ind.actif + 
                        i])
                  }
                  else lines(c(coord.ind.sup[i, 1], coord.ind.partiel.sup[(i - 
                    1) * nbre.grpe + j, 1]), c(coord.ind.sup[i, 
                    2], coord.ind.partiel.sup[(i - 1) * nbre.grpe + 
                    j, 2]), col = col.ind.sup[nb.ind - nb.ind.actif + 
                    (i - 1) * nbre.grpe + j], lty = j)
                }
            }
        }
        if (!is.null(coord.quali) & is.na(test.invisible[3])) {
            coo <- rbind(coo, coord.quali)
            if (lab.var) {
                labe <- c(labe, rownames(coord.quali))
            }
            else labe <- c(labe, rep("", nrow(coord.quali)))
            coll <- c(coll, col.quali[1:nrow.coord.quali])
            ipch <- c(ipch, rep(15, nrow(coord.quali)))
            fonte <- c(fonte, rep(2, nrow(coord.quali)))
            for (i in group.quali) {
                for (j in 1:nbre.grpe) {
                  points(coord.quali.partiel[(i - 1) * nbre.grpe + 
                    j, ], pch = 15, col = col.quali[nrow.coord.quali + 
                    (i - 1) * nbre.grpe + j], cex = par("cex") * 
                    0.8)
                  if (lab.var & lab.par) 
                    text(coord.quali.partiel[(i - 1) * nbre.grpe + 
                      j, 1], y = coord.quali.partiel[(i - 1) * 
                      nbre.grpe + j, 2], labels = rownames(coord.quali.partiel)[(i - 
                      1) * nbre.grpe + j], pos = 3, col = col.quali[nrow.coord.quali + 
                      (i - 1) * nbre.grpe + j], ...)
                  if (chrono) {
                    if (j > 1) 
                      lines(c(coord.quali.partiel[(i - 1) * nbre.grpe + 
                        (j - 1), 1], coord.quali.partiel[(i - 
                        1) * nbre.grpe + j, 1]), c(coord.quali.partiel[(i - 
                        1) * nbre.grpe + (j - 1), 2], coord.quali.partiel[(i - 
                        1) * nbre.grpe + j, 2]), col = col.quali[i])
                  }
                  else lines(c(coord.quali[i, 1], coord.quali.partiel[(i - 
                    1) * nbre.grpe + j, 1]), c(coord.quali[i, 
                    2], coord.quali.partiel[(i - 1) * nbre.grpe + 
                    j, 2]), col = col.quali[nrow.coord.quali + 
                    (i - 1) * nbre.grpe + j], lty = j)
                }
            }
        }
        if (!is.null(coord.quali.sup) & is.na(test.invisible[3])) {
            coo <- rbind(coo, coord.quali.sup)
            if (lab.var) {
                labe <- c(labe, rownames(coord.quali.sup))
            }
            else labe <- c(labe, rep("", nrow(coord.quali.sup)))
            coll <- c(coll, col.quali.sup[1:nrow(coord.quali.sup)])
            ipch <- c(ipch, rep(22, nrow(coord.quali.sup)))
            fonte <- c(fonte, rep(4, nrow(coord.quali.sup)))
            for (i in group.quali.sup) {
                for (j in 1:nbre.grpe) {
                  points(coord.quali.partiel.sup[(i - 1) * nbre.grpe + 
                    j, ], pch = 22, col = col.quali.sup[nrow(coord.quali.sup) + 
                    (i - 1) * nbre.grpe + j], cex = par("cex") * 
                    0.8)
                  if (lab.var & lab.par) 
                    text(coord.quali.partiel.sup[(i - 1) * nbre.grpe + 
                      j, 1], y = coord.quali.partiel.sup[(i - 
                      1) * nbre.grpe + j, 2], labels = rownames(coord.quali.partiel.sup)[(i - 
                      1) * nbre.grpe + j], pos = 3, col = col.quali.sup[nrow(coord.quali.sup) + 
                      (i - 1) * nbre.grpe + j], ...)
                  if (chrono) {
                    if (j > 1) 
                      lines(c(coord.quali.partiel.sup[(i - 1) * 
                        nbre.grpe + (j - 1), 1], coord.quali.partiel.sup[(i - 
                        1) * nbre.grpe + j, 1]), c(coord.quali.partiel.sup[(i - 
                        1) * nbre.grpe + (j - 1), 2], coord.quali.partiel.sup[(i - 
                        1) * nbre.grpe + j, 2]), col = col.quali[nrow.coord.quali + 
                        i])
                  }
                  else lines(c(coord.quali.sup[i, 1], coord.quali.partiel.sup[(i - 
                    1) * nbre.grpe + j, 1]), c(coord.quali.sup[i, 
                    2], coord.quali.partiel.sup[(i - 1) * nbre.grpe + 
                    j, 2]), col = col.quali.sup[nrow(coord.quali.sup) + 
                    (i - 1) * nbre.grpe + j], lty = j)
                }
            }
        }
        if (shadowtext) 
            points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, 
                ...)
        if (any(labe != "")) {
            if (autoLab == "auto") 
                autoLab = (length(which(labe != "")) < 50)
            if (autoLab == TRUE) 
                autoLab(coo[labe != "", 1], y = coo[labe != "", 
                  2], labels = labe[labe != ""], col = coll[labe != 
                  ""], font = fonte[labe != ""], shadotext = shadowtext, 
                  ...)
            if (autoLab == FALSE) 
                text(coo[labe != "", 1], y = coo[labe != "", 
                  2], labels = labe[labe != ""], col = coll[labe != 
                  ""], font = fonte[labe != ""], pos = 3, ...)
        }
        if (!shadowtext) 
            points(coo[, 1], y = coo[, 2], pch = ipch, col = coll, 
                ...)
        if ((!is.null(partial)) & (habillage == "group")) 
            legend("topright", legend = rownames(res.mfa$group$Lg)[-c(num.group.sup, 
                length(rownames(res.mfa$group$Lg)))], lty = 1:length(rownames(res.mfa$group$Lg)[-c(num.group.sup, 
                length(rownames(res.mfa$group$Lg)))]), text.col = col.hab, 
                col = col.hab, cex = par("cex") * 0.8)
        if ((!is.null(partial)) & (habillage != "group")) 
            legend("topright", legend = rownames(res.mfa$group$Lg)[-c(num.group.sup, 
                length(rownames(res.mfa$group$Lg)))], lty = 1:length(rownames(res.mfa$group$Lg)[-c(num.group.sup, 
                length(rownames(res.mfa$group$Lg)))]), cex = par("cex") * 
                0.8)
        if ((habillage != "none") & (habillage != "ind") & (habillage != "group")) 
            legend("topleft", legend = levels(res.mfa$call$X[, 
                habillage]), text.col = col.hab, cex = par("cex") * 
                0.8)
        if (!is.null(coord.ellipse) & is.na(test.invisible[2])) {
            for (e in 1:nb.ind.actif) {
                debut <- ((nb.ind.actif - 1) * npoint.ellipse) + 
                  1
                fin <- debut + npoint.ellipse - 1
                data.elli <- coord.ellipse[debut:fin, -1]
                lines(data.elli[, 1], y = data.elli[, 2], col = col.ellipse[e])
            }
        }
        if (!is.null(coord.ellipse)) {
            for (e in 1:nlevels(coord.ellipse[, 1])) {
                data.elli <- coord.ellipse[(npoint.ellipse * 
                  (e - 1) + 1):(npoint.ellipse * e), -1]
                lines(data.elli[, 1], y = data.elli[, 2], col = col.ellipse[e])
            }
        }
        if (!is.null(coord.ellipse.par)) {
            for (i in group.ind.actif) {
                for (j in 1:nbre.grpe) {
                  ind.e <- (i - 1) * nbre.grpe + j
                  data.elli <- coord.ellipse.par[(npoint.ellipse.par * 
                    (ind.e - 1) + 1):(npoint.ellipse.par * ind.e), 
                    -1]
                  lines(data.elli[, 1], y = data.elli[, 2], col = col.ellipse.par[ind.e], 
                    lty = 2)
                }
            }
        }
    }
}
###############################
plotMFApartial2<-function (x, axes = c(1, 2), lab.ind = TRUE, lab.par = FALSE, 
    habillage = "ind", chrono = FALSE, col.hab = NULL, invisible = NULL, 
    draw.partial = NULL, xlim = NULL, ylim = NULL, cex = 1, title = NULL, 
    palette = NULL, ...) 
{
    res.mfa <- x
    if (!inherits(res.mfa, "MFA")) 
        stop("non convenient data")
    if (is.null(palette)) 
        palette(c("black", "red", "green3", "blue", "cyan", "magenta", 
            "darkgray", "darkgoldenrod", "darkgreen", "violet", 
            "turquoise", "orange", "lightpink", "lavender", "yellow", 
            "lightgreen", "lightgrey", "lightblue", "darkkhaki", 
            "darkmagenta", "darkolivegreen", "lightcyan", "darkorange", 
            "darkorchid", "darkred", "darksalmon", "darkseagreen", 
            "darkslateblue", "darkslategray", "darkslategrey", 
            "darkturquoise", "darkviolet", "lightgray", "lightsalmon", 
            "lightyellow", "maroon"))
    if (is.null(title)) 
        title <- "Partial points graph"
    tab <- res.mfa$ind$coord[, axes]
    if (!is.null(res.mfa$ind.sup)) 
        tab <- rbind.data.frame(tab, res.mfa$ind.sup$coord[, 
            axes])
    if (!is.null(res.mfa["quali.var"]$quali.var)) 
        tab <- rbind.data.frame(tab, res.mfa$quali.var$coord[, 
            axes])
    if (!is.null(res.mfa$quali.var.sup)) 
        tab <- rbind.data.frame(tab, res.mfa$quali.var.sup$coord[, 
            axes])
    if (is.null(draw.partial)) {
        draw.partial <- rep(FALSE, nrow(tab))
        names(draw.partial) <- rownames(tab)
    }
    else draw.partial <- draw.partial[, 1]
    partial <- rownames(tab[draw.partial, ])
    if (length(partial) == 0) 
        partial <- NULL
    disto <- matrix(0, nrow(tab), 1)
    rownames(disto) <- rownames(tab)
    plot.MFA2(res.mfa, axes = axes, lab.ind = lab.ind, lab.par = lab.par, 
        habillage = habillage, col.hab = col.hab, invisible = invisible, 
        xlim = xlim, ylim = ylim, chrono = chrono, cex = cex, 
        title = title, partial = partial, palette = palette)
    point.haut <- max(tab[, 2]) * 1.2
    if (!is.null(ylim)) 
        point.haut <- ylim[2]
    nbpoint <- 0
    while (nbpoint < 1000) {
        pos <- locator(n = 1)
        if (is.null(pos$y)) 
            nbpoint = 1000
        else {
            for (i in 1:nrow(tab)) disto[i] <- (tab[i, 1] - pos$x)^2 + 
                (tab[i, 2] - pos$y)^2
            draw.partial[order(disto)[1]] <- !draw.partial[order(disto)[1]]
            partial <- rownames(tab)[draw.partial]
            if (length(partial) == 0) 
                partial <- NULL
            dev.off()
            plot.MFA(res.mfa, axes = axes, lab.ind = lab.ind, 
                lab.par = lab.par, habillage = habillage, col.hab = col.hab, 
                invisible = invisible, xlim = xlim, ylim = ylim, 
                chrono = chrono, cex = cex, title = title, partial = partial, 
                palette = palette)
            nbpoint = nbpoint + 1
        }
    }
    return(as.data.frame(draw.partial))
}
########################################################################
#Anova 1 facteur bayes
plotSimulAnova1<-function(bug,main="Plot of means",x.lab=NULL,y.lab=NULL,levels=NULL,lab.levels=NULL,las=1,cex.axis=1,error.bar=c("sd","se","CI"),
CI.lim=c(0.025,0.0975),ylim=NULL)
{
    bug <- as.mcmc(bug)
	data<-summary(bug,quantiles = c(CI.lim,0.5))
	if (!is.null(levels)) {
	n.levs<-length(levels)
	levs<-levels
	}
	else{
	n.levs<-length(data$statistics[, "Mean"])
	levs<-rownames(data)
	}
	if (length(error.bar)==3) error.bar="se"
	if (error.bar=="se"){
	data<-data$statistics[, c("Mean", "Naive SE")]
	sds<-data[levs,"Naive SE"]
	means<-data[levs,"Mean"]
	if (is.null(x.lab)) x.lab<-"factor"
	if (is.null(y.lab)) y.lab<-"mean"
	if (main=="Plot of means") main<-paste(main,"(with standard deviation of parameter)")
	if (is.null(ylim)) ylim<-c(min(means-sds),max(means+sds))
	plot(c(1, n.levs),ylim,type="n",xlab=x.lab,ylab=y.lab,axes=FALSE,main=main)
	points(1:n.levs, means, type = "b", pch = 16, cex = 2)
	box()
	axis(2)
	axis(1, at = 1:n.levs, labels = if(is.null(lab.levels)) levs else lab.levels,las=las,cex.axis=cex.axis)
        for (i in 1:n.levs) {
            if(sds[i]!=0)
		arrows(i,means[i]-sds[i],i,means[i]+sds[i],angle=90,code=3,lty=1,length=0.125)
        }
	}
		if (error.bar=="sd"){
	data<-data$statistics[, c("Mean", "SD")]
	sds<-data[levs,"SD"]
	means<-data[levs,"Mean"]
	if (is.null(x.lab)) x.lab<-"factor"
	if (is.null(y.lab)) y.lab<-"mean"
	if (main=="Plot of means") main<-paste(main,"(with standard deviation of parameter)")
	if (is.null(ylim)) ylim<-c(min(means-sds),max(means+sds))
	plot(c(1, n.levs),ylim,type="n",xlab=x.lab,ylab=y.lab,axes=FALSE,main=main)
	points(1:n.levs, means, type = "b", pch = 16, cex = 2)
	box()
	axis(2)
	axis(1, at = 1:n.levs, labels = if(is.null(lab.levels)) levs else lab.levels,las=las,cex.axis=cex.axis)
        for (i in 1:n.levs) {
            if(sds[i]!=0)
		arrows(i,means[i]-sds[i],i,means[i]+sds[i],angle=90,code=3,lty=1,length=0.125)
        }
	}
	if (error.bar=="CI"){
	data <- data$quantiles
	means <- data[levs,3]
	sds1 <- data[levs,1]
	sds2 <- data[levs, 2]
	if (is.null(x.lab)) x.lab<-"factor"
	if (is.null(y.lab)) y.lab<-"mean"
	if (main=="Plot of means") main<-paste(main,"(with credibility 95% of parameter)")
	if (is.null(ylim)) ylim<-c(min(sds1),max(sds2))
	plot(c(1, n.levs),ylim,type="n",xlab=x.lab,ylab=y.lab,axes=FALSE,main=main)
	points(1:n.levs, means, type = "b", pch = 16, cex = 2)
	box()
	axis(2)
	axis(1, at = 1:n.levs, labels = if(is.null(lab.levels)) levs else lab.levels,las=las,cex.axis=cex.axis)
        for (i in 1:n.levs) {
         arrows(i,sds1[i],i,sds2[i],angle=90,code=3,lty=1,length=0.125)
        }
	}
}
##########################################################
#Anova 2 facteurs bayes
plotSimulAnova2<-function (bug, main = "Plot of means", x.lab = NULL, y.lab = NULL, 
    levels1 = NULL, levels2 = NULL,lab.levels1 = NULL,lab.levels2 = NULL, las = 1, cex.axis = 1, 
    error.bar = c("sd","se", "CI"),CI.lim=c(0.025,0.0975), ylim = NULL,legend.pos="topright",legend.lab=NULL, col = palette()) 
{
    bug <- as.mcmc(bug)
	data<-summary(bug,quantiles = c(CI.lim,0.5))
	lab.main<-paste("(with credibility ",diff(CI.lim)*100,"%"," of parameter)",sep="")
    if (!is.null(levels1) & !is.null(levels2)) {
        n.levs1 <- length(levels1)
		n.levs2 <- length(levels2)
        levs1 <- levels1
		levs2 <- levels2
    }
    else {
        n.levs <- length(data$statistics[, "Mean"])
        levs <- rownames(data)
    }
    if (length(error.bar) == 3) 
        error.bar = "se"
    if (error.bar == "se") {
        data <- data$statistics[, c("Mean", "Naive SE")]
		means <- cbind(data[levs1,"Mean"],data[levs2,"Mean"])
		sds <- cbind(data[levs1,"Naive SE"],data[levs2,"Naive SE"])
        if (is.null(x.lab)) 
            x.lab <- "factor1"
        if (is.null(y.lab)) 
            y.lab <- "mean"
		if(is.null(legend.lab))
			legend.lab <- "factor2"
        if (main == "Plot of means") 
            main <- paste(main, "(with standard error of parameter)")
        if (is.null(ylim)) 
            ylim <- c(min(means - sds), max(means + sds))
	plot(c(1, n.levs1 + 1),ylim,type="n",xlab=x.lab,ylab=y.lab,axes=FALSE,main=main)
	box()
	axis(2)
	axis(1, at = 1:n.levs1, labels = lab.levels1,las=1,cex.axis=1)
       for (i in 1:n.levs2) {
   		    for (j in 1:n.levs1){
			points(j, means[j,i],type="b",pch=".",col=col[i],lty=1)
	          if(sds[j,i]!=0)
			  arrows(j,means[j,i]-sds[j,i],j,means[j,i]+sds[j,i],angle=90,code=3,col=col[i],lty=1,length=0.125)
		      }
		      for (j in 1:(n.levs1-1)){
			  segments(j,means[j,i],j+1,means[j+1,i],col=col[i],lty=1)
		      }
        }
	legend(legend.pos,c(legend.lab,lab.levels2),col=c(0,col),lty=c(0,rep(1,length(col))))      
    }
	    if (error.bar == "sd") {
        data <- data$statistics[, c("Mean", "SD")]
		means <- cbind(data[levs1,"Mean"],data[levs2,"Mean"])
		sds <- cbind(data[levs1,"SD"],data[levs2,"SD"])
        if (is.null(x.lab)) 
            x.lab <- "factor1"
        if (is.null(y.lab)) 
            y.lab <- "mean"
		if(is.null(legend.lab))
			legend.lab <- "factor2"
        if (main == "Plot of means") 
            main <- main <- paste(main, lab.main)
        if (is.null(ylim)) 
            ylim <- c(min(means - sds), max(means + sds))
	plot(c(1, n.levs1 + 1),ylim,type="n",xlab=x.lab,ylab=y.lab,axes=FALSE,main=main)
	box()
	axis(2)
	axis(1, at = 1:n.levs1, labels = lab.levels1,las=1,cex.axis=1)
       for (i in 1:n.levs2) {
   		    for (j in 1:n.levs1){
			points(j, means[j,i],type="b",pch=".",col=col[i],lty=1)
	          if(sds[j,i]!=0)
			  arrows(j,means[j,i]-sds[j,i],j,means[j,i]+sds[j,i],angle=90,code=3,col=col[i],lty=1,length=0.125)
		      }
		      for (j in 1:(n.levs1-1)){
			  segments(j,means[j,i],j+1,means[j+1,i],col=col[i],lty=1)
		      }
        }
	legend(legend.pos,c(legend.lab,lab.levels2),col=c(0,col),lty=c(0,rep(1,length(col))))      
    }
    if (error.bar == "CI") {
        data <- data$quantiles
		means <- cbind(data[levs1,3],data[levs2,3])
		sds1 <- cbind(data[levs1,1],data[levs2,1])
		sds2 <- cbind(data[levs1, 2],data[levs2,2])
         if (is.null(x.lab)) 
            x.lab <- "factor1"
        if (is.null(y.lab)) 
            y.lab <- "mean"
		if(is.null(legend.lab))
			legend.lab <- "factor2"
        if (main == "Plot of means") 
            main <- paste(main, lab.main)
        if (is.null(ylim)) 
            ylim <- c(min(sds1), max(sds2))
	plot(c(1, n.levs1 + 1),ylim,type="n",xlab=x.lab,ylab=y.lab,axes=FALSE,main=main)
	box()
	axis(2)
	axis(1, at = 1:n.levs1, labels = lab.levels1,las=1,cex.axis=1)
       for (i in 1:n.levs2) {
   		    for (j in 1:n.levs1){
			points(j, means[j,i],type="b",pch=".",col=col[i],lty=1)
	          if(sds[j,i]!=0)
			  arrows(j,sds1[j,i],j,sds2[j,i],angle=90,code=3,col=col[i],lty=1,length=0.125)
		      }
		      for (j in 1:(n.levs1-1)){
			  segments(j,means[j,i],j+1,means[j+1,i],col=col[i],lty=1)
		      }
        }
	legend(legend.pos,c(legend.lab,lab.levels2),col=c(0,col),lty=c(0,rep(1,length(col))))   
    }
}
#Nombre de sujets----------
graphe.nbre.sujet<-function(percent=FALSE,min.moyenne=NA,max.moyenne=min.moyenne,min.et=NA,max.et=min.et,titre=NA,complete.titre=paste0("Calculation for ",titre," (",test,")"),
xlab="% difference",ylab=NA,legend=c(NA,"Mini","Maxi"),power=NA,N=NA,xlim=c(5,30),nbre.lot=NA,test=c("independant","paired","anova","chi2")){
require(pwr)
	if(!percent){
	pct.moyenne<-seq(min.moyenne*xlim[2]/100,min.moyenne*xlim[1]/100,-min.moyenne*.01)
	cohen1<-pct.moyenne/max.et
	}
	else{
	cohen1<-seq((min.moyenne+xlim[2])/100,(min.moyenne+xlim[1])/100,-.01)
	cohen1[cohen1>1]<-1
	max.et<-min.moyenne
	}
nbre1<-NULL
	for (cohen in cohen1){
		if(test=="independant"){
			if(is.na(N))
			nbre<-pwr.t.test(d=cohen,power=power/100,sig.level=0.05,type="two.sample",alternative="two.sided")
			if (is.na(power))
			nbre<-pwr.t.test(d=cohen,n=N,sig.level=0.05,type="two.sample",alternative="two.sided")
		}
		if(test=="paired"){
			if(is.na(N))
			nbre<-pwr.t.test(d=cohen,power=power/100,sig.level=0.05,type="paired",alternative="two.sided")
			if (is.na(power))
			nbre<-pwr.t.test(d=cohen,n=N,sig.level=0.05,type="paired",alternative="two.sided")		
		}
		if(test=="anova"){
			if(is.na(N))
			nbre<-pwr.anova.test(k =nbre.lot,  f = cohen, sig.level = 0.05, power = power/100)
			if (is.na(power))
			nbre<-pwr.anova.test(k =nbre.lot,n=N,  f = cohen, sig.level = 0.05)
		}
		if(test=="chi2"){
			if(is.na(N))
			nbre<-pwr.proportion(p1=min.moyenne/100,p2= cohen,sig.level=0.05,power = power/100,alternative ="two.sided") 
			if (is.na(power))
			nbre<-pwr.proportion(p1=min.moyenne/100,p2= cohen,sig.level=0.05,n=N,alternative ="two.sided")
		}		
		if(is.na(N))
		nbre1<-c(ceiling(nbre$n),nbre1)
		if (is.na(power))
		nbre1<-c(round(nbre$power*100,1),nbre1)
	}
	if(!percent){
	pct.moyenne<-seq(max.moyenne*xlim[2]/100,max.moyenne*xlim[1]/100,-max.moyenne*.01)
	cohen2<-pct.moyenne/min.et
	}
	else{
	cohen2<-seq((max.moyenne+xlim[2])/100,(max.moyenne+xlim[1])/100,-.01)
	cohen2[cohen2>1]<-1
	min.et<-max.moyenne
	}
nbre2<-NULL
	for (cohen in cohen2){
		if(test=="independant"){
			if(is.na(N))
			nbre<-pwr.t.test(d=cohen,power=power/100,sig.level=0.05,type="two.sample",alternative="two.sided")
			if (is.na(power))
			nbre<-pwr.t.test(d=cohen,n=N,sig.level=0.05,type="two.sample",alternative="two.sided")
		}
		if(test=="paired"){
			if(is.na(N))
			nbre<-pwr.t.test(d=cohen,power=power/100,sig.level=0.05,type="paired",alternative="two.sided")
			if (is.na(power))
			nbre<-pwr.t.test(d=cohen,n=N,sig.level=0.05,type="paired",alternative="two.sided")		
		}
		if(test=="anova"){
			if(is.na(N))
			nbre<-pwr.anova.test(k =nbre.lot,  f = cohen, sig.level = 0.05, power = power/100)
			if (is.na(power))
			nbre<-pwr.anova.test(k =nbre.lot,n=N,  f = cohen, sig.level = 0.05)
		}
		if(test=="chi2"){
			if(is.na(N))
			nbre<-pwr.proportion(p1=max.moyenne/100,p2= cohen,sig.level=0.05,power = power/100,alternative ="two.sided") 
			if (is.na(power))
			nbre<-pwr.proportion(p1=max.moyenne/100,p2= cohen,sig.level=0.05,n=N,alternative ="two.sided")
		}				
		if(is.na(N))
		nbre2<-c(ceiling(nbre$n),nbre2)
		if (is.na(power))
		nbre2<-c(round(nbre$power*100,1),nbre2)
	}	
	if (is.na(ylab)){
		if(is.na(N)) 
		ylab<-"Sample size/group"
		if (is.na(power)) 
		ylab<-"Power"
	}
	if(is.na(N)) {
	posLegend<-"topright"
		if (is.na(legend[1]))
		legTitre<-paste0("Sample (",power,"% power)")
		else
		legTitre<-legend[1]
	coordLeg<-c(legend[2],legend[3])
	}
	if (is.na(power)) {
	posLegend<-"bottomright"
		if (is.na(legend[1]))
		legTitre<-paste0("Power (",N,"/group)")
		else
		legTitre<-legend[1]
	coordLeg<-c(legend[2],legend[3])
	}
abscisse<-seq(xlim[1],xlim[2],1)
	if(!percent)
	complete.titre1<-paste0(complete.titre,"\nm = ",min.moyenne)
	else
	complete.titre1<-paste0(complete.titre,"\nprevalence = ",min.moyenne,"%")
	if(min.moyenne!=max.moyenne | min.et!=max.et){
		if(!percent)
		complete.titre1<-paste0(complete.titre,"\nm = ",min.moyenne," / ",max.moyenne)
		else
		complete.titre1<-paste0(complete.titre,"\nprevalence = ",min.moyenne,"% / ",max.moyenne,"%")
	}
	if(is.na(N))
	plot(c(xlim[1]-1,xlim[2]),c(max(nbre1,nbre2)*1.1,min(nbre1,nbre2)),ylab=ylab,xlab=xlab,type="n",main=complete.titre1)
	if (is.na(power))
	plot(c(xlim[1]-1,xlim[2]),c(max(nbre1,nbre2),min(nbre1,nbre2)*.9),ylab=ylab,xlab=xlab,type="n",main=complete.titre1)
lines(abscisse,nbre1,col="red",lwd=2)
	if(min.moyenne!=max.moyenne | min.et!=max.et)
	lines(abscisse,nbre2,col="blue",lwd=2)
difference<-seq(xlim[1],xlim[2],5)
donnees<-data.frame(abscisse,nbre1,nbre2)
label.N<-NULL
	for (i in 1:length(difference)){
	label.N<-rbind(label.N,donnees[donnees$abscisse==difference[i],])
	}
	if(is.na(N)){
	text(label.N[,1],label.N[,2],label.N[,2],col="red",pos=3)
		if(min.moyenne!=max.moyenne | min.et!=max.et)
		text(label.N[,1],label.N[,3],label.N[,3],col="blue",pos=3)
	}
	if (is.na(power)){
	text(label.N[,1],label.N[,2],label.N[,2],col="red",pos=1)
		if(is.na(percent)){
			if(min.moyenne!=max.moyenne | min.et!=max.et)
			text(label.N[,1],label.N[,3],label.N[,3],col="blue",pos=1)
		}
	}
	if(min.moyenne!=max.moyenne | min.et!=max.et)
	legend(posLegend,coordLeg,lwd=2,col=c("blue","red"),title=legTitre)
	else
	legend(posLegend,"",title=legTitre)
}



