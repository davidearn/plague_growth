require(xtable)

#' translate column names to latex text
#' @param col the column name in the dataframe
col.translate <- function(col) {
  switch(col, 
         source="Source",
         epidemic="Epidemic",
         growth.rate = "Growth rate",
         R0="$\\mathcal{R}_0$",
         doubling.time="Doubling time",
         finalsize="Final size")
}

#' create the text for a table column from fitting results
#' @param variable a variable in the fitting results, containing columns value, lower and upper
column <- function(variable, digits=2, percent=FALSE) {
    value <- paste("%1.", digits, "f", if (percent) "\\%%" else "", sep="")
    if (all(is.na(variable$lower))) {
        format <- value
    } else {
        format <- paste(value, " \\quad (", value, ", ", value, ")", sep="")
    }
    no_limits <- all(is.na(variable$lower)) && all(is.na(variable$upper))
    fmtfun <- function(v, l, u){
        if (no_limits) {
            values = v
        } else {
            values = c(v, l, u)
        }
        if (is.na(v)) return("NA")
        if (round(v,digits)<10) format <- paste0("\\ \\,",format)
        if (percent) values <- values * 100
        do.call(sprintf, c(list(format), values))
    }
    mapply(fmtfun, variable$value, variable$lower, variable$upper)
}

#' print a latex table for the fitting results, with selected columns
#' @param fit the fitting results
#' @param columns the components of fits that will be printed as colummns
#' @param colnames the column names in the latex table
#' @param digits a vector of number of digits for each column
#' @param percent a vector of logical indicating if each column is a percentage
#' @param comment the latex comment to be added
#' @param file the file to print
#' @return NULL
latexTable <- function(fit, columns, colnames, digits=1, percent=FALSE, comment=NULL, file) {
  n <- length(columns)
  if (length(colnames) != n)
    stop("the columns and colnames must have the same length.")
  if (length(digits) == 1) digits <- rep(digits, n)
  if (length(percent) == 1) percent <- rep(percent, n)
  if (length(digits) != n)
    stop("the columns and digits must have the same length.")
  if (length(percent) != n)
    stop("the columns and percent must have the same length.")
  ##data = data.frame(epidemic=fit[[1]]$epidemic)
  data = data.frame(source=fit[[1]]$source, epidemic=fit[[1]]$epidemic)
  for (i in 1:n) {
    var = columns[[i]]
    if (var %in% names(fit)) {
      data[[var]] <- column(fit[[var]], digits[[i]], percent[[i]])
    } else stop("column ", var, " is not in the fitting result.", call.=FALSE)
  }
  colnames(data) = c("Source", "Epidemic", colnames)

  ## ssn = 'simplify source names':
  ## FIX: why does x get altered to numeric by the first ifelse ?!?
  ## colours should be exactly the same as in plots [DONE, 29 Aug 2020]
  ssn <- function(x) {
    x <- ifelse(grepl("wills",x), "Wills", x)
    x <- ifelse(grepl("bills",x), "{\\color{LBoMred}LBoM}", x)
    x <- ifelse(grepl("parish",x),"{\\color{parishblue}Parish}", x)
    return(x)
  }
  data <- data %>% mutate(Source=ssn(as.character(Source)))

  if (!is.null(comment)) cat("%", comment, "\n", file=file)
  table = xtable(data)
  ## Hack to deal with different tables for which this script
  ## is used.  table tab:params has 7 columns, two of which
  ## need to be specificied with p{} rather than l:
  if (ncol(data) != 7) {
    a = paste(rep("|", ncol(data)+2), collapse="l")
  } else {
    a <- "l|l|l|l|l|p{3.5cm}|p{3.5cm}|l|"
    ## FIX: It would probably be better to save  data  in an RData
    ##      file and construct the table using tabularx as I do for
    ##      tab:datasources.  So, I'm saving this:
    save(table2=data, file="table2.RData")
  }
  align(table) = a
  print(table,
        append = TRUE,
        floating = FALSE,
        include.rownames = FALSE, 
        comment = TRUE, 
        sanitize.text.function = function(x) {x},
        file=file)
}
