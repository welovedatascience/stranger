## CODE -- Set of visualization functions --------------
## THIS FILE CONTAINS FUNCTIONS TO MAKE SOME VISUALIZATIONS
# TO UNDERSTAND SCORE LOCALLY AROUND A SINGLE RECORD           # Author : WeLoveDataScience                                   # License: xxxxx


# Helper (sub) functions-----------

investigate_cluster <- function(data, id, score, anomaly_id, n.anom = 100, n.cluster = 4, ...){

  # Step1: Select records
  ## Take top n-anomaly
  data <- data[order(data[, score], decreasing = TRUE), ]
  nbre_observation <- min(n.anom, nrow(data))
  top_score <- data[1: nbre_observation, ]

  ## Add the selected Id (if not yet in) in the Top n-anomly selection
  if(!(anomaly_id %in% top_score[, id])){
    top_score <- rbind(top_score, data[data[, id] == anomaly_id,])

  }

  rownames(top_score) <- top_score[[id]]
  # Step 2: Compute hierachical clustering
  distance_matrix <- dist(scale(top_score[, !colnames(top_score) == id]), method = "euclidean")
  hc <- hclust(distance_matrix, method = "ward.D2")

  # Step 3: Plot Dendrogram
  ## Convert hclust into a dendrogram and plot
  hcd <- as.dendrogram(hc)

  ## Makes plots (Full tree + Zooming-in cutted trees )
  par(mfrow = c(2, 1), mar = c(1,1,1,1))

  ### Plot Full Tree
  plot(hcd,
       main = "Full Tree",
       type = "rectangle",
       ylab = "",
       nodePar = list(lab.cex = 0.001, pch = c(NA, 5),cex = 0.2, col = "steelblue"),
       edgePar = list(col = "steelblue"),
       axes = FALSE,
       cex.main = 1
  )
  rect.hclust(hc, k= n.cluster)

  ### Plot Tree with the cluster of the specific ID

  #### find the height corresponding to the number of cluster
  cutting_height = (max(hc$height) - min(hc$height))  / 2
  for(i in 1:nbre_observation){
    clust <- length(cut(hcd, h = cutting_height)$lower )
    if (clust > n.cluster){
      cutting_height = cutting_height + 0.01 * (max(hc$height) - min(hc$height))
    }else if (clust < n.cluster){
      cutting_height = cutting_height - 0.01 * (max(hc$height) - min(hc$height))
    }else{ break }
  }

  #### Find the cluster of the specific anomaly_id
  #https://stackoverflow.com/questions/25452472/extract-labels-membership-classification-from-a-cut-dendrogram-in-r-i-e-a-c

  cut_hcd <- cut(hcd, h = cutting_height)$lower

  for(i in 1:length(cut_hcd)){
    member_list <- unlist(dendrapply(cut_hcd[[i]],function(n) {
      labels<-c()
      if (is.leaf(n)) {
        a <- attributes(n)
        labels<-c(labels,a$label)
      }
      labels
    }))
    anomaly_id_cluster <- i
    if(as.character(anomaly_id) %in% member_list) {break}
  }

  # cf. https://rpubs.com/gaston/dendrograms



  chcd <- cut(hcd, h = cutting_height)$lower[[anomaly_id_cluster]]
  colLab <- function(n) {
    if (is.leaf(n)) {
      a <- attributes(n)
      labCol <- ifelse(a$label %in% as.character(anomaly_id),"orange","darkgray")
      attr(n, "nodePar") <- c(a$nodePar, list(lab.col = labCol,lab.cex = 0.6, pch = c(NA, 5),cex = 0.2))
    }
    n
  }
  # using dendrapply
  chcd = dendrapply(chcd, colLab)

  #### Make the plot
  plot(chcd,
       main = paste("Zooming-in Cluster ",anomaly_id_cluster,sep =""),
       type = "rectangle",
       ylab = "",
       edgePar = list(col = "steelblue"),
       axes = FALSE,
       cex.main = 0.8)
}


investigate_neighbours <- function(data, id, score, anomaly_id, k = NULL, ...){


  if (is.null(k)) k <- min(1000,nrow(data)-1)
  ## Load library and data
  Vectorize(require)(package = c("FNN","tidyr"),character.only = TRUE)

  if (inherits(data,"data.table")){ data=as.data.frame(data) }

    knn <- get.knn(data[, !colnames(data) %in% c(id,score)], k, algorithm="cover_tree")



  ## Filter data to keep only the K neighbours

  rownames(knn$nn.index) <- data[[id]]
  knn_id <- data[[id]][as.vector(knn$nn.index[anomaly_id, ])]
  # print(anomaly_id)
  knn_data <- data[data[, id] %in% c(anomaly_id, knn_id), ]

  ## Compute the number of columns for the faced_wrap to obtain a almost square plots
  x <- dim(knn_data)[2]
  column_number <- round(sqrt(x),0)

  ## Reshape
  knn_data <- gather(knn_data, key = indicator, value = val, - one_of(c(id,score)))

  ## plot
  ggplot(data=knn_data, aes(x = val, y = get(score))) +
    geom_smooth(color = "steelblue", method="loess", se=FALSE, size = 0.3) +
    geom_point(color = "gray", alpha = 0.7, size = 1) +
    geom_point(data = knn_data[knn_data[, id] == anomaly_id, ],aes(x = val, y = get(score)), color = "orange", alpha = 1, size = 3) +
    labs(y="Anomaly Score") +
    facet_wrap( ~indicator, nrow = column_number, scales = "free_x") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "gray80", colour = "black",size = 0.5, linetype = "solid"),
          strip.text = element_text(face = "bold"),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position="none")
}


investigate_feature_importance <- function(data, id, score, anomaly_id, num.trees=NULL, ...){
  if (is.null(num.trees)) num.trees <- min(300,2*sqrt(ncol(data)))
  Vectorize(require)(package = c("ranger"),character.only = TRUE)

  data$anomalyRF <- as.factor(data[[id]] %in% anomaly_id)
  remove=c(score,id)
  dataRF <- data[,!colnames(data) %in% remove,drop=FALSE]
  rf <- ranger::ranger(formula=anomalyRF~. , data=dataRF, importance="impurity", num.trees=num.trees, write.forest = FALSE)
  varimp <- data.frame(Importance=ranger::importance(rf))
  varimp[,"Variable"] <- rownames(varimp)


  # create a theme for dot plots, which can be reused
  theme_dotplot <- theme_bw(14) +
    theme(axis.text.y = element_text(size = rel(.75)),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = rel(.75)),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.5),
          panel.grid.minor.x = element_blank())


  ggplot(varimp, aes(x = Importance, y = reorder(Variable, Importance))) +
    geom_point(color = "orange") +
   theme_dotplot +
    xlab("Importance") +
    ylab("Variable") + ggtitle(paste("Variable importance for anomalies with IDs:", paste(anomaly_id, collapse=",")))
}


#
## Mimic LIME approach - difficult in our case to retrace all the workflow...
# investigate_feature_importance <- function(data, id, score, anomaly_id, k = NULL, ...){
#
#   if (is.null(k)) k <- min(1000,nrow(data)-1)
#
#   # Load library and data
#   Vectorize(require)(package = c("FNN"),character.only = TRUE)
#
#   ## Filter data to keep only the K neighbours
#   knn <- get.knn(data[, !colnames(data) %in% c(id,score)], k, algorithm="cover_tree")
#   knn_id <- knn$nn.index[anomaly_id, ]
#   knn_data <- data[data[, id] %in% c(anomaly_id, knn_id), ]
#
#
#   # Step 1: Compute the score sensitivity for each variable
#
#   ## Prepare an empty dataframe to collect resuts
#   feature <- as.character(colnames(knn_data[, !colnames(knn_data) %in% c(id, score)]))
#   result <- as.data.frame(feature)
#   result$var_imp <- 0
#   result$gap_with_mean<- 0
#
#   ## Compute sensitivity for each feature
#   j <- 1
#   for (feature_i in feature){
#     temp <- as.data.frame(knn_data)
#     ## Keep information of the gap between actual value and mean
#     result[j, "gap_with_mean"] <- round((temp[temp[,id] == anomaly_id, feature_i] - mean(temp[, feature_i]) ) / mean(temp[, feature_i]), 2)
#     ## Set the input to its population mean
#     temp[temp[,id] == anomaly_id , feature_i] <- mean(temp[, feature_i])
#     ## Recompute the score using same method
#     temp <- crazyfy(temp)   #### SHOULD NOT USE THAT STEP BUT NO CHOOSE!!!!!!!!!!!!!!
#     temp <- strange(temp)
#     temp <- as.data.frame(temp)
#     ## Save the differential in score
#     importance <- (temp[temp[,id] == anomaly_id, score] - knn_data[knn_data[, id] == anomaly_id, score]) / knn_data[knn_data[,id] == anomaly_id, score]
#     result[j, "var_imp"] <- round(importance, 2)
#     j <- j +1
#   }
#
#   # Step 2: reshape data
#   result <- gather(result, key = indicator, value = val,-feature)
#   result <- mutate(result, sign = sign(val))
#   result <- mutate(result, value = ifelse(indicator == "var_imp", abs(val), -abs(val)))
#   result <- arrange(result,desc(val))
#   result <- mutate(result, sign = as.character(sign))
#
#   # Step 3: Plot
#   ggplot(result, aes(group = indicator)) +   # Fill column
#     geom_bar(aes(x = feature, y = value,  fill = sign), stat = "identity", width = .6, alpha = 0.8) +
#     scale_fill_manual(values = c("#F64D27", "#3F8219")) +
#     geom_hline(yintercept=0, linetype="dashed", size=0.7) +
#     geom_text(aes(label = val, x = feature, y = 0.2*val), size = 7,color = "white") +
#     geom_text(label="Variable Importance (%)", x=0.5, y=0.2, hjust=0, size=4) +
#     geom_text(label="Distance from mean (%)", x=0.5, y=-1, hjust=0, size=4) +
#     coord_flip() +  # Flip axes
#     theme(panel.background = element_blank(),
#           panel.grid = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text.x = element_blank(),
#           panel.border = element_blank(),
#           axis.title.x=element_blank(),
#           axis.title.y=element_blank(),
#           legend.position="none")
# }


investigate_scores_decline <- function(data, id, score, anomaly_id, k = NULL, n_label = 15, ...){
  if (is.null(k)) k <- min(1000,nrow(data)-1)
  #Load library and data
  Vectorize(require)(package = c("FNN"),character.only = TRUE)

    # step 0: Filter data to keep only the K-neighbours
  knn <- suppressMessages(get.knn(data[, !colnames(data) %in% c(id,score)], k, algorithm="cover_tree"))

  rownames(knn$nn.index) <- data[[id]]
  knn_id <- data[[id]][as.vector(knn$nn.index[anomaly_id, ])]
  # print(anomaly_id)
  knn_data <- data[data[, id] %in% c(anomaly_id, knn_id), ]

  # Step 1: Decreasing order
  knn_data <- knn_data[order(knn_data[, score], decreasing = TRUE), ]

  # Plot
  ggplot(data = knn_data, aes(x = 1:(nrow(knn_data)), y = get(score))) +
    geom_point(color = "steelblue", size = 0.7, alpha = 0.8) +
    labs( x = "k-closest neighbours", y="Anomaly Score") +
    geom_text(data = knn_data[1:n_label, ], aes(label = get(id), y = get(score), x = 1:n_label),
              hjust=-1.1, size=2.5, color = "orange") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "gray80", colour = "black",size = 0.5, linetype = "solid"),
          strip.text = element_text(face = "bold"),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank())
}


investigate_regression_tree <- function(data, id, score, anomaly_id, k=NULL, ...){

  # Load library and data
  Vectorize(require)(package = c("FNN", "rpart"),character.only = TRUE)
  if (is.null(k)) k <- min(1000,round(nrow(data)*2/3))
  # step 0: Filter data to keep only the K-neighbours
  knn <- suppressMessages(get.knn(data[, !colnames(data) %in% c(id,score)], k, algorithm="cover_tree"))

  rownames(knn$nn.index) <- data[[id]]
  knn_id <- data[[id]][as.vector(knn$nn.index[anomaly_id, ])]
  # print(anomaly_id)
  knn_data <- data[data[, id] %in% c(anomaly_id, knn_id), ]

    # Step1: Build a large regression tree (with a small cp).
    tree <- rpart::rpart(knn_data[,score] ~ ., data=knn_data[, !colnames(knn_data) %in% c(score, id)] , method = "anova", control = rpart.control(cp = 0.0001))

  # Step2: Pick the tree size that minimizes prediction error.
  # Prediction error rate in cross-validation = Root node error * xerror * 100%
  # Hence we want the cp value (with a simpler tree) that minimizes the xerror.
  best_cp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

  # Step3: Prune the tree using the best cp.
  tree_pruned <- prune(tree, cp = best_cp)

  plot(tree_pruned, cex = .8, compress = FALSE, minbranch = 10, nspace = 0.01)
  text(tree_pruned,
       use.n=TRUE, # use.n = TRUE adds number of observations at each node
       xpd = TRUE,  # xpd = TRUE keeps the labels from exteding outside the plot
       #minlength = 1, # length of label
       all=FALSE,
       pretty = TRUE,
       cex=.7,
       digits = 2,
       fancy = FALSE)
}



#' Data visualizations of anomaly score locally around a specific data point
#' @details
#' Function that produces visualizations to understand the anomaly score locally around a specific data point.
#' We believe this should help people to trust scores a made by models even if they don’t fully understand them.
#' Today, 5 visualisazions are implemented;
#' (1) A hierarchical clustering, named "cluster", showing among the top n-anomaly which records belongs to the same cluster
#' a specific record. Finding the commun pattern amoung the cluster may lead to the orign of of the specifi record score.
#' (2) A dots plot, named "neighbours", showing the relationship between the anomly score and each feature for the
#' k nearest neighbours of a specific record.
#' (3) A bar chart, named "feature_importance", showing how sensitive is the anomaly score of a specific record to each of feature.
#' This may help to identify the features behind the score.
#' (4) A dots plot, names "score_decline", showing the decrease in anomaly score among the k nearest neighbours of a specific record.
#' The shape indicates how extrem and how frequent is the anomaly score of a speicif record amoung its neighbours.
#' (5) A Regression tree, named "regression_tree", showing the roots to high score around a specific record.
#'
#' @param x is either of class dataframe, stranger or anomaly. It contains the observations; each row represents an observation
#'  and each variable is stored in one column. It must have at least one column with IDs and one column with the anomaly
#'  score for each ID.
#' @param type is the name of the visualization;
#' (1) A hierarchical clustering, named "cluster", showing among the top n-anomaly which records belongs to the same cluster
#' a specific record. Finding the commun pattern amoung the cluster may lead to the orign of of the specifi record score.
#' (2) A dots plot, named "neighbours", showing the relationship between the anomly score and each feature for the
#' k nearest neighbours of a specific record.
#' (3) A bar chart, named "feature_importance", showing how sensitive is the anomaly score of a specific record to each of feature.
#' This may help to identify the features behind the score.
#' (4) A dots plot, names "score_decline", showing the decrease in anomaly score among the k nearest neighbours of a specific record.
#' The shape indicates how extrem and how frequent is the anomaly score of a speicif record amoung its neighbours.
#' (5) A Regression tree, named "regression_tree", showing the roots to high score around a specific record.
#' @param id is the colname with records IDs
#' @param score is the colname which contains the anomaly score
#' @param anomaly_id is the record ID you want to investigate
#' @param \dots Additional parameters to pass 
#'
#' @details 
#' Extra parameters that can be used in \dots:
#' \itemize{
#' \item check logical indicating if object data should be checked for validity. The default is TRUE, this check is not necessary
#' when data is known to be valid such as when it is the direct result of stranger().
#' \item  keep character vector: names of columns to keep (filter)
#' \item  drop character vector: names of columns to drop (filter)
#' \item  n.cluster is the number of cluster groups to emphasis.
#' This parameter must only be specified with type ="cluster".
#' \item  n.anom is the number of top anomalies to be considered.
#' This parameter must only be specified with type ="cluster".
#' \item  k is the number of neighbours to be considered. This parameter must always be specified,
#' except with type = "cluster".
#' \item  n_label specifies the number of data point to be labelled in the plot.
#' This parameter must only be specified with type ="scores_decline".
#' }
#' 
#' @return A plot
#' @rdname plot
plot.stranger <- function(x,
                          type="cluster",
                          id = ".id",
                          score = NULL,
                          anomaly_id = NULL,...){

  s <- singularize(x,...)


  plot.singular(s, type=type,id=id,score=score,anomaly_id=anomaly_id,...)
}


#' @rdname plot
plot.fortifiedanomaly <- function(x,
                          type="feature_importance",
                          id = ".id",
                          anomaly_id = NULL,score=NULL,
                          ...){

  type <- match.arg(type, c( "feature_importance","neighbours", "regression_tree") ) #"neighbours"

  # anoflag <- attr(x,"meta")$anomalyflag
  # score <- anoflag
  # if (type=="neighbours"){
  #   assertthat::assert_that(!is.null(anomaly_id),msg="Anomaly ID required")
  #   return(investigate_neighbours(data=x, id=id,
  #                   anomaly_id=anomaly_id,
  #                   score=x$meta$anomalyflag))
  # }

  if (type=="feature_importance" & is.null(score)) {
    score  <- names(x)[1]

    if (is.null(anomaly_id)) warning(paste0("No score given to determine anomaly; data have been sorted by ",score,". You should explicitely provide a score (anomaly metric) or will have unexpected results."))
  }



  ## Well-defined ID
  if (is.null(id))    stop("The function required an ID to be defined")
  if  ( anyDuplicated(x$id) > 0 )   stop("The function required an ID to be defined")


  id = match.arg(id, colnames(x))


  ## Well-defined score
  if (is.null(score) | is.numeric(x$score)) {
    stop("You need to specify a numeric column populated with the anomaly score")
  }
  else {
    score = match.arg(score, colnames(x))
  }

  # Select desired feature
  if (hasArg("keep") & hasArg("drop")){
    if (any(keep%in% drop)) stop("Can't assign variables  both in keep and drop arguments")
  }

  if (hasArg("keep")) {
    if (any(keep%in% c(id, score))) stop("Can't remove ID or anomaly score")
    if (!is.null(keep)) {x <- x[, (colnames(x)%in% keep),drop=FALSE]}
  }

  if (hasArg("keep") & hasArg("drop")){
    if (!is.null(drop)) x <- x[,!(colnames(x)%in% drop),drop=FALSE]
  }
  assertthat::assert_that(ncol(x)>0,msg="No more column remaining for investigation; please change keep/drop.")


  # select top anomaly if argument is NULL
  if (is.null(anomaly_id)){
    anodist <- x[[score]]
    sortdorder <- attr(anodist,"sort")
    anomaly_id <- x[[id]][order(sortdorder*x[[score]])][1]
    print(anomaly_id)
  }

data <- as.data.frame(x)

  # Call the plot function
  function_arguments <- list(data = data, id = id, score = score, anomaly_id = anomaly_id, ... )
  do.call(paste("investigate",type , sep="_"), function_arguments)


}


#' @rdname plot
plot.anomalies <- function(x,
                                  type="feature_importance",
                                  id = ".id",
                                  anomaly_id = NULL,
                                  ...){

  if (is.null(anomaly_id)) anomaly_id <- x[1]
  x <- fortify.anomalies(x,colname="flag_anomaly")
  plot.fortifiedanomaly(x,type=type,id=id,anomaly_id=anomaly_id,score="flag_anomaly",...)
}


#' @rdname plot
plot.singular <- function(x,
                          type="cluster",
                          id = ".id",
                          score = NULL,
                          anomaly_id = NULL,
                          ...){

  # Check parameters for validity
  ## Type is one of the options


  type <- match.arg(type, c("cluster","neighbours", "feature_importance", "scores_decline", "regression_tree") )


  if (!(type %in% c( "feature_importance", "regression_tree") )
      & (length(anomaly_id)>1)){
    anomaly_id <- anomaly_id[1]
    warning(paste("Method",type,"works with only one anomaly; first one considered."))
  }


  if (type=="feature_importance" & is.null(score)) {
    score  <- names(x)[1]

    if (is.null(anomaly_id)) warning(paste0("No score given to determine anomaly; data have been sorted by ",score,". You should explicitely provide a score (anomaly metric) or will have unexpected results."))
  }



  ## Well-defined ID
  if (is.null(id))    stop("The function required an ID to be defined")
  if  ( anyDuplicated(x$id) > 0 )   stop("The function required an ID to be defined")


  id = match.arg(id, colnames(x))


  ## Well-defined score
  if (is.null(score) | is.numeric(x$score)) {
    stop("You need to specify a numeric column populated with the anomaly score")
  }
  else {
    score = match.arg(score, colnames(x))
  }

  # Select desired feature
  if (hasArg("keep") & hasArg("drop")){
    if (any(keep%in% drop)) stop("Can't assign variables  both in keep and drop arguments")
  }

  if (hasArg("keep")) {
    if (any(keep%in% c(id, score))) stop("Can't remove ID or anomaly score")
    if (!is.null(keep)) {x <- x[, (colnames(x)%in% keep),drop=FALSE]}
  }

  if (hasArg("keep") & hasArg("drop")){
    if (!is.null(drop)) x <- x[,!(colnames(x)%in% drop),drop=FALSE]
  }
  assertthat::assert_that(ncol(x)>0,msg="No more column remaining for investigation; please change keep/drop.")


  # select top anomaly if argument is NULL
  if (is.null(anomaly_id)){
    anodist <- x[[score]]
    sortdorder <- attr(anodist,"sort")
    anomaly_id <- x[[id]][order(sortdorder*x[[score]])][1]
    print(anomaly_id)
  }



  # keep id and score
  # todo: checks presence of id (+manage) and score
  x <- x[,c(id,score),with=FALSE]
  data <- fortify.singular(x)


  ## Data is Dataframe
  if(!inherits(data, "data.frame")){
    data <- as.data.frame(data)
    message("Your data has been converted to a dataframe to be compatible with ggplot function.")}
  if(inherits(x, "data.table")){
    data <- as.data.frame(data)
    message("Your data has been converted to a dataframe to be compatible with ggplot function.")}


  # Call the plot function
  function_arguments <- list(data = data, id = id, score = score, anomaly_id = anomaly_id, ... )
  do.call(paste("investigate",type , sep="_"), function_arguments)
  #
  # investigate(
  #   data,
  #   type=type,
  #   id=id,
  #   score=score,
  #   anomaly_id=anomaly_id,...)
}
