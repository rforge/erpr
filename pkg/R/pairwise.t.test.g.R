pairwise.t.test.g <-
function (x1, g.list, id, agg=T, p.adjust.method = p.adjust.methods, pool.sd = !paired, 
    paired = FALSE, alternative = c("two.sided", "less", "greater"), 
    ...) 
{
	##### PARTE GIORGIO
	
	g.list.names=deparse(substitute(g.list))
	g.list.names=gsub("\\(", "",g.list.names)
	g.list.names=gsub("\\)", "", g.list.names)
	g.list.names=gsub("^list", "", g.list.names)
	g.list.names=gsub(" ", "", g.list.names)	
	
	g.list[[length(g.list)+1]]=id
	newdata=aggregate(x1, g.list, mean)
	newdata$newfactor=apply(newdata[1:(length(newdata)-2)], 1, function(x){paste(x, collapse="_")})#
	x=newdata$x
	g=newdata$newfactor
    
    #### FINE PARTE GIORGIO
    
    if (paired & pool.sd) 
        stop("Pooling of SD is incompatible with paired tests")
    DNAME <- paste(deparse(substitute(x1)), "and", g.list.names[1])
    g <- factor(g)
    p.adjust.method <- match.arg(p.adjust.method)
    alternative <- match.arg(alternative)
    if (pool.sd) {
        METHOD <- "t tests with pooled SD"
        xbar <- tapply(x, g, mean, na.rm = TRUE)
        s <- tapply(x, g, sd, na.rm = TRUE)
        n <- tapply(!is.na(x), g, sum)
        degf <- n - 1
        total.degf <- sum(degf)
        pooled.sd <- sqrt(sum(s^2 * degf)/total.degf)
        compare.levels <- function(i, j) {
            dif <- xbar[i] - xbar[j]
            se.dif <- pooled.sd * sqrt(1/n[i] + 1/n[j])
            t.val <- dif/se.dif
            if (alternative == "two.sided") 
                2 * pt(-abs(t.val), total.degf)
            else pt(t.val, total.degf, lower.tail = (alternative == 
                "less"))
        }
    }
    else {
        METHOD <- if (paired) 
            "paired t tests"
        else "t tests with non-pooled SD"
        compare.levels <- function(i, j) {
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            t.test(xi, xj, paired = paired, alternative = alternative, 
                ...)$p.value
        }
        ### AGGIUNTA GIORGIO #
        compare.levels.t <- function(i, j) {
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            t.test(xi, xj, paired = paired, alternative = alternative, 
                ...)$statistic
        }
        compare.levels.df <- function(i, j) {
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            t.test(xi, xj, paired = paired, alternative = alternative, 
                ...)$parameter
        }
        #######################
        #######################
        
    }
    PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
    TVAL <- pairwise.table(compare.levels.t, levels(g), "none")
    DF	<- pairwise.table(compare.levels.df, levels(g), "none")
    ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL, t.value=TVAL, df=DF, ### ALTRE AGGIUNTE GIORGIO
        p.adjust.method = p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}
