library(ggplot2)
stopifnot(packageVersion("ggplot2")>="2.2.0")
library(RColorBrewer)
require(dplyr)  ## for mutate(), filter()

months.name = c("Jan", "Apr", "Jul", "Oct")
days <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)


theme.fit <- (theme_bw() + ## black and white rather than grey default background
           theme(panel.spacing=grid::unit(0,"lines"), ## squash panels together
                 axis.text.y = element_text(size=6),  ## smaller y axis tick labels
                 strip.text.y = element_text(angle=0), ## horizontal strip labels
                 ## get rid of grid lines
                 ## panel.grid.major.x=element_blank(),
                 panel.grid.minor.x=element_line(linetype="dotted",colour=grey(0.8)),
                 panel.grid.major.y=element_blank(),
                 panel.grid.minor.y=element_blank()
           )
)

#' figure for death fit
#' @param fit fitted object
#' @importFrom ggplot2 ggplot aes facet_grid scale_x_continuous scale_y_continuous geom_line geom_point
#' @export
figure.deaths.fit <- function(fit,fake_data=TRUE) {
    ## only if more than one type plotted on the figure
    bp3 <- brewer.pal(3,"Set1")
    myblue <- bp3[2]
    myred <- bp3[1]
    ## set point defaults so that single-class version will work correctly
    update_geom_defaults("point", list(fill=myred, shape=21, colour="black" ))
    fitted_plus_obs <- length(unique(fit$type))>1
    if (fitted_plus_obs) {
        fit$type <- factor(fit$type,levels=c("raw","fitted"),
                           labels=c("observed","fitted"))
        figure <- ggplot(data = fit, aes(x=time, y=deaths, fill=type,
                                         shape=type, color=type)) +
            scale_fill_manual(name="",values=c(myred,NA))+
            scale_color_manual(values=c("black","blue"),name="") +
            scale_shape_manual(name="",values=c(21,NA))
    } else {
        ## single type (no colour, shape aesthetics)
        figure <- ggplot(data = fit, aes(x=time, y=deaths))
    }
    if (fake_data) {
        ## ugly, ugly, ugly
        ## need to add points at y-values large enough to extend limits, but not
        ##  large enough to add another major break/axis tick label ...
        fakedata <- data.frame(time=min(fit$time),
                               epidemic=c("1593","1603","Harbin"),
                               deaths=c(1100,3500,260)) %>%
            filter(epidemic %in% unique(fit$epidemic))
        figure <- figure + geom_point(data=fakedata,colour=NA,shape=NA,fill=NA)
    }
    figure <-  figure + geom_point() + geom_line()
    ## hack to get correct lines/points overlay:
    ## http://stackoverflow.com/questions/42983499/plot-points-in-front-of-lines-for-each-group-ggplot2-equivalent-of-type-o
    if (fitted_plus_obs) {
        figure <- figure +
            geom_point(data=subset(fit,type=="observed"))+
            geom_line(data=subset(fit,type=="fitted"),show.legend=FALSE)
    } else {
        figure <- figure + geom_point()
    }
    ## now add facets
    figure <- figure +
        facet_grid(epidemic ~ ., scales="free") +
        labs(y="deaths or testaments") +
        scale_x_continuous(name="",
                           limits=c(-7/12,1), # x axis in units of years
                           breaks=c(days[seq(1, 12, 3)]-365,
                                    days[seq(1, 12, 3)],
                                    days[seq(1, 12, 3)]+365)/365,
                           minor_breaks=c(days[1:12]-365,
                                          days[1:12],
                                          days[1:12]+365)/365,
                           labels=rep(months.name, 3))
        ## scale_y_continuous(expand=c(0.25,0)) + ## more vertical space
        ## geom_line(na.rm=TRUE,size=0.25)+##,data=subset(fit,type=="observed")) +
        ## geom_point(na.rm=TRUE,size=1,color="black",data=fit) +
        ## geom_line(na.rm=TRUE,show.legend=FALSE,data=subset(fit,type=="fitted"),color=myblue,size=1,alpha=0.5)
    figure <- figure + theme.fit +
        theme(
            ## expand plot margins
            ## plot.margin = grid::unit(c(0.2,0.2,1.5,0.2), "cm"),
            legend.key.size = unit(0.75, 'lines'),
            legend.position=c(0.15,0.08),
            legend.background=element_rect(fill=NA)
        )
    
    return(figure)
}

#' figure for fitting to sampled deaths
#' @param fit fitted object
#' @importFrom ggplot2 ggplot aes facet_grid scale_x_continuous scale_y_continuous geom_line geom_point
#' @export
figure.sampled.fit <- function(fit) {
  figure = ggplot(data = fit, aes(x=time, ymin=deaths.min, ymax=deaths.max, 
                                  color=type, fill=type)) + 
    facet_grid(epidemic ~ ., scales="free") +
    scale_y_continuous(name="deaths") + 
    scale_x_continuous(name="time",
                       limits=c(0,1.5),
                       breaks=c(days[seq(1, 12, 3)], days[seq(1, 12, 3)+365])/365,
                       minor_breaks=c(days[seq(1, 12, 1)],days)/365,
                       labels=c(months.name, months.name)) +
    geom_ribbon(na.rm=TRUE, alpha=0.5)
}

#' figure to compare fitting results to raw data.
#' @param fit the $data component from fit.epidemics (in analysis/fits/epidemics.R) or sample.epidemics (in analysis/fits/sample.R)
#' @export
figure.fit <- function(fit) {
  if ("deaths.max" %in% colnames(fit)) {
    figure.sampled.fit(fit)
  } else figure.deaths.fit(fit)
}

#' plot fitted results
#' @param fit fitted model
#' @param plot.file name of plot file
#' @param show.fit ?
#' @export plot.fit
plot.fit <- function(fit, plot.file = "", width=4, height=4) {
	figure = figure.fit(fit)
	plot.setup(plot.file, width=width, height=height)
	print(figure)
	plot.close(plot.file)
}

##' draw a figure summarizing growth rate statistics
##' (e.g. growth rate, doubling time, R0, etc.)
##' @param data data frame with columns epidemic, value, lower and upper.
##' @param style plot style
##' @param ylab y-axis label
##' @param xlab_size character size for x-axis tick labels
##' @param log_y log-scale y axis?
##' @importFrom ggplot2 geom_crossbar geom_pointrange
figure.growth <- function(data,
                          style=c("pointrange","crossbar"),
                          colour=c("epoch","severity","none"),
                          ylab="initial epidemic growth rate $r$ (per year)",
                          xlab_size=10, log_y=FALSE,
                          do_panels=FALSE) {

    if (do_panels) {
        data <- transform(data,
                panel=ifelse(epoch=="$<1400$ CE","14th c.","16th--17th c."))
    }

    style <- match.arg(style) ## argument-processing (allows partial matching etc.)
    colour <- match.arg(colour)
    map <- if ("type" %in% colnames(data)) aes(color=type) else NULL
    fig <- (ggplot(data = data, 
                 aes(x=epidemic, y=value, ymin=lower, ymax=upper))
        + labs(y=ylab)
        + theme(axis.text.x = element_text(size=xlab_size, angle=45, hjust=1))
    )
    if (style=="crossbar") {
        fig <- fig + geom_crossbar(mapping=map)
    } else {
        ## pointrange
        if (colour=="epoch") {
            fig <- fig +
                geom_linerange(aes(colour=epoch),size=12) + ## error bars
                scale_colour_manual(values=grey(c(0.95,0.85)))
        } else if (colour=="severity") {
            cols <- RColorBrewer::brewer.pal(n=3,"Set1")[1:2]
            cols <- adjustcolor(cols,alpha=0.5)
            fig <- fig +
                geom_linerange(aes(colour=severity),size=12) +
                scale_colour_manual(values=cols)
        } else if (colour=="none") {
            fig <- fig + geom_linerange(colour="gray",size=12)
        }
        fig <- fig + 
            geom_point(aes(colour=epoch),colour="black",alpha=1, size=5) + ## data points
            geom_point(colour="red", size=3) ## data points again
    }
    if (log_y) fig <- fig + scale_y_log10()
    if (do_panels) {
        fig <- fig + facet_grid(.~panel,scale="free_x",space="free") +
            theme(panel.spacing=grid::unit(0,"lines"))
    }
    return(fig)
}

#' plot the growth rate figure
#' @param growth the $growth component from fit.epidemics (in analysis/fits/epidemics.R) or sample.epidemics (in analysis/fits/sample.R). It is a dataframe with columns epidemic, r, lower and upper.
#' @param plot.file the file name for the figure to be generated
#' @param width the width of the figure (in inches)
#' @param height the height of the figure (in inches)
#' @export
plot.growth <- function(growth, plot.file = "", width=4, height=4, ...) {
	plot.setup(plot.file, width=width, height=height)
	print(figure.growth(growth, ...))
	plot.close(plot.file)
}

#' plot a figure with the growth rates on the left panel and the fitted time series on the right panel.
#' @param fit the $data component from fit.epidemics (in analysis/fits/epidemics.R) or sample.epidemics (in analysis/fits/sample.R)
#' @param growth the $growth component from fit.epidemics (in analysis/fits/epidemics.R) or sample.epidemics (in analysis/fits/sample.R). It is a dataframe with columns epidemic, r, lower and upper.
#' @param plot.file the file name for the figure to be generated
#' @export
plot.growth.fit <- function(growth, fit, plot.file = "") {
	if (plot.file == "") separate = FALSE
	
	fig.growth <- figure.growth(growth)
	fig.fit <- figure.fit(fit)

	plot.setup(plot.file, width=8, height=6)
	multiplot(fig.growth, fig.fit, layout=matrix(c(1,1,2,2,2), nrow=1))
	plot.close(plot.file)
}


replace_val <- function(x,oldval,newval) {
    replace(x,which(x==oldval),newval)
}

plot.pred <- function(res,epi_frame) {
    ## assumes 'plague' available, i.e. epigrowthfit loaded
    pmajor <- (plague
        %>% filter(severity=="major" & place=="London")
        %>% mutate(date=as.Date(paste(year,month,day,sep="-")))
        %>% select(time,date,plague.deaths,outbreak.year)
    )

    pfun <- function(res) if (is.null(res)) NULL else predict(res)
    allpred <- (bind_rows(lapply(res,pfun),.id="id")
        %>% mutate(id=as.numeric(id))
        %>% full_join(epi_frame,by=c("id"="ind"))
        %>% select(epi,model,distrib,time,date,deaths)
        %>% rename(outbreak.year=epi,plague.deaths=deaths)
        %>% mutate(outbreak.year=as.numeric(outbreak.year))
        %>% right_join(pmajor %>% select(outbreak.year),by="outbreak.year")
        %>% filter(model %in% c("logistic","richards_s01"))
        %>% mutate(model=replace_val(model,"richards_s01",
                                     "richards ($s \\geq 0.1$)"))
    )
    fig <- ggplot(pmajor,aes(date,plague.deaths))+geom_point()+
        geom_line(data=allpred,aes(colour=model,linetype=distrib),size=2)+
        facet_wrap(~outbreak.year,scale="free")+
        scale_colour_brewer(palette="Set1")+
        scale_x_date(date_label="%b")+
        scale_linetype(name="distribution")+
        labs(x="",y="deaths or testaments")

    return(fig)
}


##' summarize nested lists of fits
##' level=0 returns the summary from a single fit
##' level=1 returns the summary from a list of fits
##' level=2 returns the summary from a nested list of fits ...
sumfun <- function(fitlist,level=1,keys=c("outbreak.year","source"),
                   type="sum") {
    maxLevel <- length(keys)+1
    b <- if (level==1) fitlist else fitlist[[rep(1,level)]]
    template <- as(summary(b),"data.frame")
    template[] <- NA ## replace *contents* of template with NA
    funList <- list()
    funList[[1]] <- function(x,type="sum") {
        if (is.null(x) || inherits(x,"try-error")) {
            if (type=="sum") ss <- template else ss <- NULL
        } else {
            ss <- switch(type,
                         sum=as(summary(x),"data.frame"),
                         pred=fitted(x),
                         deaths=data.frame(time=x@time,deaths=x@deaths))
        }
        return(ss)
    }
    for (i in 2:maxLevel) {
        funList[[i]] <- function(x,type="sum") {
            dplyr::bind_rows(lapply(x,funList[[i-1]],type=type),
                             .id=keys[i-1])
        }
        ## force evaluation of environment
        environment(funList[[i]]) <- list2env(list(i=i,keys=keys))
    }
    ## funList[[1]](fitlist[[1]][[1]],type="pred")
    ## funList[[2]](fitlist[[1]],type="pred")
    ## funList[[3]](fitlist,type="pred")
    return(funList[[level+1]](fitlist,type=type))
}

## helper function to translate outbreak years to epochs
add_epoch <- . %>%
    mutate(epoch=ifelse(outbreak.year<1500,"14th c.","16th-17th c."))
## data sets considered too bad to bother with
drop_bad <- . %>%
    filter(!(source=="Canterbury wills" & outbreak.year %in% c("1563","1593")),
           !(source=="London bills" & outbreak.year=="1593"))
