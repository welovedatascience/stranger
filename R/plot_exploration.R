#' Data Exploration plots
#'
#' This is a simple function that produced plots that may help you to understand the shape of your dataset.
#' Today, 3 plots are implemented;(1) a small histogram for each features, (2) a correlation matrix and
#' (3) a 2-Dimension Hexbin Frequency.
#'
#' @param data is the data frame containing the observations. Each row represents an observation and each variable is stored in one column.
#' @param keep character vector: names of columns to keep (filter)
#' @param drop character vector: names of columns to drop (filter)
#' @param type one of "histogram", "correlation"
#' @param \dots is additional arguments to be passed to internal
#'    functions. Currently only col.fill and bins for histograms.
#'
#' @examples
#' \dontrun{
#' explore(BudgetUK, type ="hist")
#' explore(BudgetUK, type ="cor",drop="children")
#' }
#'
explore <- function(data, type="histogram",keep=NULL,drop=NULL,...){
  type <- match.arg(type, c("histogram","correlation"))
  ## Load library and data
  Vectorize(require)(package = c("dplyr"),character.only = TRUE)
  ## Check basic required data properties
  if(!is.data.frame(data)){
    data <- as.data.frame(data)
    message("Your data has been converted to a dataframe to be compatible with ggplot function.")
  }

  if (any(keep%in% drop)) stop("Can't assign variables  both in  keep and drop arguments")
  if (!is.null(keep)) data <- data[, (colnames(data)%in% keep),drop=FALSE]
  if (!is.null(drop)) data <- data[,!(colnames(data)%in% drop),drop=FALSE]

  ## anyway remove any potential .id column
  if (".id" %in% colnames(data)){
    data <- data[,!(colnames(data)%in% ".id"),drop=FALSE]
    message(".id column removed.")
  }

  assertthat::assert_that(ncol(data)>0,msg="No more column remaining for exploration; please change keep/drop.")
  get(paste("explore",type,sep="_"))(data,...)
}

explore_histogram <- function(data,col.fill="steelblue",bins=50,...){
  #assertthat::assert_that(require(ggplot2),msg="ggplot2 package required.")
  ## Take colomns name of numeric versus factor features

  cn <- colnames(data)
  colclasses <- sapply(data,class)
  numerical_feature <- cn[colclasses %in% c("integer","numeric")]
  factor_feature    <- cn[colclasses %in% c("character","factor")]
  # ensure we deal with thoses classes (for instance drop date/time variables)

  temp <- data[,cn %in% c(numerical_feature,factor_feature),drop=FALSE]

  nc <- ncol(temp)
  assertthat::assert_that(nc>0,msg="No more column in data")
  column_number <- round(sqrt(nc),0)

  plots <- vector(mode="list",length=nc)

  cn <- colnames(temp)
  names(plots) <- cn

  for (col in cn ){

    if (col %in% numerical_feature){
      # numeric: use histogram
      pl <- ggplot(temp) + geom_histogram(
        aes_string(x=col),fill = col.fill,bins = bins)
    } else  {
      # factor: barplot
      pl  <- ggplot(temp) + geom_bar(
        aes_string(x=col),stat = "count",fill=col.fill)
    }

    pl <- pl +
      labs(x=col) +
      theme_bw() +
      theme(strip.background = element_rect(fill = "gray80", colour = "black",size = 0.5, linetype = "solid"),
            strip.text = element_text(face = "bold"),
            axis.title.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank())

    plots[[col]] <- pl
  }

  multiplot(plotlist = plots, cols=column_number)
}

# 2) Helper function to plot a matrix of correlation
explore_correlation <- function(data,...){

  cn <- colnames(data)
  colclasses <- sapply(data,class)
  numerical_feature <- cn[colclasses %in% c("integer","numeric")]
  assertthat::assert_that(length(numerical_feature)>0,msg="No more numeric variable")
  data <- data[,numerical_feature,drop=FALSE]

  # MAIN PART: Prepare data and plot
  data %>%
    ## Only numerical
    select(one_of(numerical_feature))  %>%
    ## Compute Correlation
    cor(...) %>%
    round(digits = 2) %>%
    as.data.frame() %>%

    ## Reorder correlation
    #reorder_cormat() %>%

    ## Add rownames
    mutate(var1 = rownames(.)) %>%

    ## Remove upper triangle
    #get_upper_tri() %>%

    ## Reshape
    gather(key = var2, value = value, - var1, na.rm = TRUE ) -> tmp

  # put missing for diagonal
  tmp[tmp[,1]==tmp[,2],"value"] <- NA


  ## Basic histogram plot

  ggplot(tmp,aes(x = var2, y= var1 , fill = value)) +
    geom_tile(color = "gray", size = 1) +
    scale_fill_gradient2(low = "red", high = "green", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Pearson\nCorrelation") +
    ## Theme
    theme_bw() +
    geom_text(aes(x = var2, y = var1, label = value), color = "black", size = 4) +
    theme(
      panel.border = element_rect(size = 2),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank())

}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# From: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # library(grid)
  requireNamespace(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

