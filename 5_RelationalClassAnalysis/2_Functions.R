### Inputting Functions

f <- function(m) {
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  m
}

changeX <- function(string){
  changemat <- outer(string,string,"-")
  diag(changemat) <- NA
  return(changemat)
}

gen_change_list <- function(df){
  changeX_list <- vector("list", nrow(df))
  for ( i in 1:nrow(df)){
    changeX_list[[i]] <- changeX(df[i,])
  }
  return(changeX_list)
}

# Function for calculating relationality
relationality1 <- function(Xi, Xj){

  kx = ncol(Xi)
  ky = ncol(Xj)

  lambda <- ifelse(Xi*Xj >= 0, 1, -1)

  dist <- 1-abs(abs(Xi) - abs(Xj))
  dist = lambda * dist

  to_keep = which(colSums(is.na(dist)) < kx)
  if(length(to_keep) > 0){
    dist = dist[to_keep, to_keep]
  }

  kx = ncol(dist)

  relationalities = sum(dist, na.rm = TRUE)

  relationalities = (relationalities)/(kx*(kx-1))

  return(relationalities)
}

# Function for constructing distance matrix
rca.dist <- function(data){

  data <- as.matrix(data)
  change_x_list <- gen_change_list(data)
  distmat <- matrix(nrow = nrow(data), ncol = nrow(data))
  for (i in 1:(nrow(distmat)-1)){
    for ( j in (i+1):nrow(distmat)){
      distmat[i,j] <- relationality1(change_x_list[[i]], change_x_list[[j]])
    }
  }

  distmat <- f(distmat)

  return(distmat)
}

cor.all <- function(df){

  cordata <- matrix(NA, nrow = length(df), ncol = length(df))
  ps <- matrix(NA, nrow = length(df), ncol = length(df))

  for (i in 1: length(df) ){
    for ( j in 1 : length(df) ){
      corres <- try(cor.test(df[,i], df[,j]))
      if (is.na(corres$estimate) == TRUE){
        cordata[i,j] <- 0
        ps[i,j] <- 1
      } else{
        cordata[i,j] <- corres$estimate
        ps[i,j] <- corres$p.value
      }
    }
  }

  diag(cordata) <- NA
  diag(ps) <- NA

  results <- list(correlations = cordata, pvalues = ps)
  return(results)
}

giant.component <- function(graph){
  cl <- clusters(graph)
  graph <- induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
  return(graph)
}

# Updated RCA algorithm
rca.full.3 <- function(data, community_detection = "leading.eigenvector", bootstrap = FALSE, plot_structure = "net", sig.level = 0.05, amirsd = T){

  require(igraph)
  require(boot)
  require(lsa)
  require(scales)

  data <- sapply(as.data.frame(data), rescale)

  distmat <- rca.dist(data)

  distmat = distmat - median(distmat, na.rm = T)

  diag(distmat) <- 0

  raw_distmat <- distmat

  distmat = abs(distmat)
  #distmat[distmat < 0.05] = 0

  net <- graph.adjacency(distmat, mode = "undirected", weighted = TRUE)

  cluster_out <- cluster_louvain(net,  weights = E(net)$weight)

  mod_out <- modularity(cluster_out)

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data$group <- as.vector(membership(cluster_out))

  results <- list(Data = data, Raw_Distance_Matrix = raw_distmat, Distance_Matrix = distmat, Modularity_Score = mod_out, membership = data$group)

  return(results)
}

plot.groups <- function(data, variables = c("V1", "V2", "V3", "V4", "V5"), group.ids = "group", sig.level = .05, type = "net", bw = T){
  require(igraph)
  require(corrplot)
  require(scales)

  plots <- vector("list", length(unique(data[,group.ids])))
  count = 0
  group_vec <- unique(data[,group.ids])
  group_vec <- group_vec[order(group_vec)]
  for ( i in group_vec ){

    count = count + 1
    subdata <- data[data[,group.ids] == i,]
    subdata <- subset(subdata, select = variables)

    cor.subdata <- cor.all(subdata)
    pval.subdata <- cor.subdata$pvalues
    cor.subdata <- cor.subdata$correlations

    diag(cor.subdata) <- 0
    diag(pval.subdata) <- 1

    cor.subdata <- ifelse(pval.subdata > sig.level, 0, cor.subdata)
    cor.subdata <- ifelse(is.na(cor.subdata), 0,cor.subdata)

    if(type == "net"){
      cor.g <- graph.adjacency(cor.subdata, mode = c("lower"), weighted = TRUE)

      if(bw == F){
        colfunc_neg <- colorRampPalette(c("#FFCCCC", "#E60000"))
        colfunc_pos <- colorRampPalette(c("#d2d2ff", "#0000E6"))
      } else if (bw){
        colfunc_neg <- colorRampPalette(c("grey90", "black"))
        colfunc_pos <- colorRampPalette(c("grey90", "black"))
      }

      strength_neg <- colfunc_neg(11)
      strength_pos <- colfunc_pos(12)
      E(cor.g)$less_than_zero = ifelse(E(cor.g)$weight < 0, 1, 0)
      if(bw) E(cor.g)$lty <- ifelse(E(cor.g)$less_than_zero == 1, 2, 1)

      E(cor.g)$color <- NA
      for (j in E(cor.g)){
        if(E(cor.g)$weight[j] >= 0){
          E(cor.g)$color[j] <- strength_pos[round(E(cor.g)$weight[j]*10)+1]
        } else {
          E(cor.g)$color[j] <- strength_neg[round(abs(E(cor.g)$weight[j]*10))+1]
        }
      }
      V(cor.g)$name <- colnames(subdata)
      cor.g <- giant.component(cor.g)
    } else if (type == "matrix") {
      colnames(cor.subdata) <- colnames(subdata)
      rownames(cor.subdata) <- colnames(subdata)
      colfunc <- colorRampPalette(c("grey20", "white", "grey20"))
      cor.g <- corrplot(cor.subdata, diag = F, method = "square", order = "hclust", col = colfunc(100), title = paste0("Class ", i), mar=c(0,0,2,0), tl.col = "black")
    } else {
      stop("Argument plot_structure must be set to either 'net' or 'matrix'.")
    }
    plots[[count]] <- cor.g
  }
  return(plots)
}

pole_heatmaps <- function(data, variables = c("V1", "V2", "V3", "V4", "V5"), pole.ids = "poles", class.ids = "RCA", sig.level = .05, bw = T, file_name = "pole_heatmaps_sig"){

  require(igraph)
  require(corrplot)
  require(scales)
  require(reshape2)
  require(viridis)
  require(ggplot2)
  require(ggthemes)
  require(gridExtra)
  require(grid)

  dfs <- vector("list", length(na.omit(unique(data[,pole.ids]))))
  count = 0
  group_vec <- sort(unique(data[,pole.ids]))
  class_vec <- sort(unique(data[,class.ids]))
  for ( i in group_vec ){

    count = count + 1
    subdata <- data[data[,pole.ids] == i,]
    subdata <- subset(subdata, select = variables)

    cor.subdata <- cor.all(subdata)
    pval.subdata <- cor.subdata$pvalues
    cor.subdata <- cor.subdata$correlations

    diag(cor.subdata) <- 0
    diag(pval.subdata) <- 1

    cor.subdata <- ifelse(pval.subdata > sig.level, 0, cor.subdata)
    cor.subdata <- ifelse(is.na(cor.subdata), 0, cor.subdata)

    colnames(cor.subdata) <- colnames(subdata)
    rownames(cor.subdata) <- colnames(subdata)

    diag(cor.subdata) <- 1
    row.order <- hclust(dist(cor.subdata, method = "manhattan"), method="ward.D2")$order # clustering
    diag(cor.subdata) <- 0

    var_order <- rownames(cor.subdata)[row.order]
    cor.subdata <- cor.subdata[var_order, var_order]

    cor_el <- melt(cor.subdata)
    colnames(cor_el) <- c("Var1", "Var2", "corr")
    cor_el$order1 <- match(cor_el$Var1, var_order) + ((count-1) * length(var_order))
    cor_el$order2 <- match(cor_el$Var2, rev(var_order)) + ((count-1) * length(var_order))
    cor_el$pole <- i
    dfs[[count]] <- cor_el
  }

  cor_df <- do.call("rbind", dfs)
  cor_df$class <- as.numeric(unlist(lapply(strsplit(cor_df$pole, "_"), function(x) x[1])))
  cor_df$pole_id <- as.numeric(unlist(lapply(strsplit(cor_df$pole, "_"), function(x) x[2])))
  cor_df <- cor_df %>% group_by(class) %>% mutate(pole_id = toupper(letters)[rank_poles(pole_id)]) %>% ungroup()

  letter_set <- sort(unique(cor_df$pole_id))
  p <- list()
  count = 0
  for(i in class_vec){
    for(j in letter_set){
      count = count + 1
      p[[count]] <- ggplot(cor_df[cor_df$class==i & cor_df$pole_id == j,], aes(order1,order2,fill=corr))+
        geom_tile(color= "white",size=0.1) +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-1, 1))+
        #scale_fill_viridis(name="Correlation",option ="B", limits = c(-1, 1)) +
        labs(x=NULL, y=NULL, title=unique(cor_df[cor_df$class==i & cor_df$pole_id == j,]$pole)) +
        theme_tufte(base_family="Helvetica") +
        theme(plot.title=element_text(hjust=0)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
        scale_x_continuous(
          breaks = cor_df[cor_df$class==i & cor_df$pole_id == j,]$order1,
          labels = cor_df[cor_df$class==i & cor_df$pole_id == j,]$Var1,
          position = "top"
        ) +
        scale_y_continuous(
          breaks = cor_df[cor_df$class==i & cor_df$pole_id == j,]$order2,
          labels = cor_df[cor_df$class==i & cor_df$pole_id == j,]$Var2
        )
    }
  }
  nCol = length(letter_set)

  return(p)
}

# RCA class heatmap function
class_cormats <- function(data,
                           variables = c("V1", "V2", "V3", "V4", "V5"),
                           class.ids = "RCA",
                           sig_level = .05,
                           bw = T,
                           file_name = "class_heatmaps_sig",
                           addition = "",
                           label_type = "directions")
{

  require(igraph)
  require(corrplot)
  require(scales)
  require(reshape2)
  require(viridis)
  require(ggplot2)
  require(ggthemes)
  require(gridExtra)
  require(grid)

  dfs <- vector("list", length(na.omit(unique(data[,class.ids]))))
  count = 0
  class_vec <- sort(unique(data[,class.ids]))
  for (i in class_vec) {

    count = count + 1
    subdata <- data[data[,class.ids] == i,]
    subdata <- subset(subdata, select = variables)

    cor.subdata <- cor.all(subdata)
    pval.subdata <- cor.subdata$pvalues
    cor.subdata <- cor.subdata$correlations

    star.subdata <- ifelse(pval.subdata  <= 0.05, "*", "")
    star.subdata <- ifelse(pval.subdata  <= 0.01, "**", star.subdata)
    star.subdata <- ifelse(pval.subdata  <= 0.001, "***", star.subdata)
    diag(star.subdata) = ""

    diag(cor.subdata) <- 0

    colnames(cor.subdata) <- colnames(subdata)
    rownames(cor.subdata) <- colnames(subdata)

    diag(cor.subdata) <- 1
    row.order <- hclust(dist(cor.subdata, method = "manhattan"), method="ward.D2")$order # clustering
    diag(cor.subdata) <- 0

    cor.subdata.sig <- matrix(paste0(round(cor.subdata, 2), star.subdata), length(variables), length(variables))
    colnames(cor.subdata.sig) <- colnames(subdata)
    rownames(cor.subdata.sig) <- colnames(subdata)

    var_order <- rownames(cor.subdata.sig)[row.order]
    cor.subdata.sig <- cor.subdata.sig[var_order, var_order]

    dfs[[count]] <- cor.subdata.sig
  }

  return(dfs)
}


# RCA class heatmap function
class_heatmaps <- function(data,
                           variables = c("V1", "V2", "V3", "V4", "V5"),
                           class.ids = "RCA",
                           sig_level = .05,
                           bw = T,
                           file_name = "class_heatmaps_sig",
                           addition = "",
                           label_type = "directions")
{

  require(igraph)
  require(corrplot)
  require(scales)
  require(reshape2)
  require(viridis)
  require(ggplot2)
  require(ggthemes)
  require(gridExtra)
  require(grid)

  dfs <- vector("list", length(na.omit(unique(data[,class.ids]))))
  count = 0
  class_vec <- sort(unique(data[,class.ids]))
  for (i in class_vec) {

    count = count + 1
    subdata <- data[data[,class.ids] == i,]
    subdata <- subset(subdata, select = variables)

    cor.subdata <- cor.all(subdata)
    pval.subdata <- cor.subdata$pvalues
    cor.subdata <- cor.subdata$correlations

    diag(cor.subdata) <- 0
    diag(pval.subdata) <- 1

    cor.subdata <- ifelse(pval.subdata > sig_level, 0, cor.subdata)
    cor.subdata <- ifelse(is.na(cor.subdata), 0, cor.subdata)

    colnames(cor.subdata) <- colnames(subdata)
    rownames(cor.subdata) <- colnames(subdata)

    diag(cor.subdata) <- 1
    row.order <- hclust(dist(cor.subdata, method = "manhattan"), method="ward.D2")$order # clustering
    diag(cor.subdata) <- 0

    var_order <- rownames(cor.subdata)[row.order]
    cor.subdata <- cor.subdata[var_order, var_order]

    cor_el <- melt(cor.subdata)
    colnames(cor_el) <- c("Var1", "Var2", "corr")
    cor_el$order1 <- match(cor_el$Var1, var_order) + ((count-1) * length(var_order))
    cor_el$order2 <- match(cor_el$Var2, rev(var_order)) + ((count-1) * length(var_order))
    cor_el$class <- i
    dfs[[count]] <- cor_el
  }

  cor_df <- do.call("rbind", dfs)
  cor_df$direction = ifelse(cor_df$corr < 0, "-", "+")

  if(bw){
    low_col = "black"
    high_col = "black"
  } else {
    low_col = "red"
    high_col = "blue"
  }

  if (label_type == "correlations") {
    label_vals <- round(cor_df$corr, 1)
  } else if (label_type == "directions") {
    label_vals <- cor_df$direction
  }
  cor_df$label_vals = label_vals

  p <- list()
  count = 0
  for(i in class_vec){
    count = count + 1
    p[[count]] <- ggplot(cor_df[cor_df$class==i,], aes(order1,order2,fill=corr))+
      geom_tile(color= "white",size=0.1) +
      geom_text(aes(label = label_vals), color = "white", size=3) +
      scale_fill_gradient2(low = low_col, high = high_col, mid = "white", midpoint = 0, limits = c(-1, 1))+
      labs(x=NULL, y=NULL, title=unique(cor_df[cor_df$class==i,]$class)) +
      theme_tufte(base_family="Helvetica") +
      theme(plot.title=element_text(hjust=0)) +
      theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 0),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size=16)) +
      scale_x_continuous(
        breaks = cor_df[cor_df$class==i,]$order1,
        labels = cor_df[cor_df$class==i,]$Var1,
        position = "top"
      ) +
      scale_y_continuous(
        breaks = cor_df[cor_df$class==i,]$order2,
        labels = cor_df[cor_df$class==i,]$Var2
      ) +
      guides(fill=guide_legend(title="Correlation"))

    p[[count]]
    ggsave(paste0(paste(file_name, sig_level, "class", i, addition, sep = "_"), ".tiff"), units = "in", height = 8, width = 9, device = "tiff")
  }
  nCol = length(class_vec)

  return(p)
}


# RCA class heatmap function
class_heatmaps_adjusted <- function(data, variables = c("V1", "V2", "V3", "V4", "V5"), class.ids = "RCA", sig.level = .05, bw = T, file_name = "class_heatmaps_sig", addition = ""){

  require(igraph)
  require(corrplot)
  require(scales)
  require(reshape2)
  require(viridis)
  require(ggplot2)
  require(ggthemes)
  require(gridExtra)
  require(grid)

  dfs <- vector("list", length(na.omit(unique(data[,class.ids]))))
  count = 0
  class_vec <- sort(unique(data[,class.ids]))
  for ( i in class_vec ){

    count = count + 1
    subdata <- data[data[,class.ids] == i,]
    subdata <- subset(subdata, select = variables)
    data_all <- subset(data, select = variables)
    cor.subdata <- cor.all(subdata)$correlations
    cor.data <- cor.all(data_all)$correlations
    adj.cors <- cor.subdata-cor.data
    
    colnames(adj.cors) <- colnames(subdata)
    rownames(adj.cors) <- colnames(subdata)

    diag(adj.cors) <- 1
    row.order <- hclust(dist(adj.cors, method = "manhattan"), method="ward.D2")$order # clustering
    diag(adj.cors) <- 0

    var_order <- rownames(adj.cors)[row.order]
    adj.cors <- adj.cors[var_order, var_order]

    cor_el <- melt(adj.cors)
    colnames(cor_el) <- c("Var1", "Var2", "corr")
    cor_el$order1 <- match(cor_el$Var1, var_order) + ((count-1) * length(var_order))
    cor_el$order2 <- match(cor_el$Var2, rev(var_order)) + ((count-1) * length(var_order))
    cor_el$class <- i
    dfs[[count]] <- cor_el
  }

  cor_df <- do.call("rbind", dfs)
  cor_df$direction = ifelse(cor_df$corr < 0, "-", "+")

  p <- list()
  count = 0
  for(i in class_vec){
    p[[i]] <- ggplot(cor_df[cor_df$class==i,], aes(order1,order2,fill=corr))+
      geom_tile(color= "white",size=0.1) +
      geom_text(aes(label = direction), color = "white") +
      scale_fill_gradient2(low = "black", high = "black", mid = "white", midpoint = 0, limits = c(-1, 1))+
      labs(x=NULL, y=NULL, title=unique(cor_df[cor_df$class==i,]$class)) +
      theme_tufte(base_family="Helvetica") +
      theme(plot.title=element_text(hjust=0)) +
      theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 0),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size=16)) +
      scale_x_continuous(
        breaks = cor_df[cor_df$class==i,]$order1,
        labels = cor_df[cor_df$class==i,]$Var1,
        position = "top"
      ) +
      scale_y_continuous(
        breaks = cor_df[cor_df$class==i,]$order2,
        labels = cor_df[cor_df$class==i,]$Var2
      ) +
      guides(fill=guide_legend(title="Correlation"))

    p[[i]]
    ggsave(paste0(paste(file_name, "class", i, "adjusted", sep = "_"), ".tiff"), units = "in", height = 8, width = 9, device = "tiff")
  }
  nCol = length(class_vec)

  return(p)
}

# Rescaling and converting feeling thermometers from ANES to ordinal

# original function
# rescale_cat = function(x){
# 
#   require(scales)
#   require(purrr)
# 
#   # de-center all rows
#   for(i in 1:nrow(x)){
#     rowvals = as.numeric(x[i,])
#     x[i,] = rowvals - mean(rowvals, na.rm = T)
#   }
# 
#   # rescale back to 0 to 100 by variable
#   x = data.frame(sapply(x, FUN = function(x) rescale(x, to = c(0, 100))), stringsAsFactors = F)
# 
#   # convert to ordinal
#   x[,1:length(x)] <- purrr::map(x[,1:length(x)], cut,
#                          breaks = c(0, 15, 30, 40, 50, 51, 61, 71, 86, 100),
#                          labels = c(1:9), include.lowest = TRUE, right = FALSE, ordered_result = TRUE)
# 
#   return(x)
# }
rescale_cat = function(x){
  
  require(scales)
  require(purrr)
  
  # de-center all rows
  # for(i in 1:nrow(x)){
  #   rowvals = as.numeric(x[i,])
  #   x[i,] = rowvals - mean(rowvals, na.rm = T)
  # }
  # 
  # # rescale back to 0 to 100 by variable
  # x = data.frame(sapply(x, FUN = function(x) rescale(x, to = c(0, 100))), stringsAsFactors = F)
  
  # convert to ordinal
  # x[,1:length(x)] <- purrr::map(x[,1:length(x)], cut,
  #                               breaks = c(0, 15, 30, 40, 50, 51, 61, 71, 86, 100),
  #                               labels = c(1:9), include.lowest = TRUE, right = FALSE, ordered_result = TRUE)
  x[,1:length(x)] <- purrr::map(x[,1:length(x)], as.factor)
  
  return(x)
}

mean_response_pattern <- function(poles, df, min_size = 5, zscore = F, byclass = F){

  df <- as.data.frame(df, stringsAsFactors = F)
  pole_tab <- table(poles)
  poles_to_drop <- names(pole_tab[which(pole_tab < min_size)])
  df$poles <- poles
  df$class <- unlist(lapply(strsplit(df$poles, "_"), function(x) x[1]))
  df <- subset(df, !poles %in% poles_to_drop)

  if(zscore){
    if(byclass){
      to_rbind <- list()
      for(i in 1:max(as.numeric(df$class), na.rm = T)){
        sub_df <- subset(df, df$class == i)
        sub_df[,1:(ncol(sub_df) - 2)] <- sapply(sub_df[,1:(ncol(sub_df) - 2)], scale)
        to_rbind <- append(to_rbind, list(sub_df))
      }
      df <- do.call("rbind", to_rbind)
    } else {
      df[,1:(ncol(df) - 2)] <- sapply(df[,1:(ncol(df)-2)], scale)
    }
  }

  mean_response_patterns <- aggregate(. ~ poles + class, data = df, FUN = function(x) mean(x, na.rm = T), na.action = na.pass)
  return(mean_response_patterns)
}

rank_poles <- function(x){
  unique_set <- sort(unique(x))
  x <- match(x, unique_set)
  return(x)
}

grid_arrange_shared_legend <- function(..., COLNUM) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots, function(x)
      x + theme(legend.position="none")), ncol = COLNUM)),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight),
    newpage = F)
}

add_raw_distance_matrix <- function(rca_obj){
  require(igraph)
  require(boot)
  require(lsa)
  require(scales)

  data <- sapply(as.data.frame(rca_obj$Data), rescale)

  distmat <- rca.dist(data)

  distmat = distmat - median(distmat, na.rm = T)

  diag(distmat) <- 0
  #distmat[] <- ifelse(distmat[] > -0.05 & distmat[] < 0.05, 0, distmat[])

  rca_obj$Raw_Distance_Matrix <- distmat
  return(rca_obj)
}

reduceLabelOverlap <- function(g, labeldist_coef=1.5, cex_from_device=F, label.attr='label', labelsize.attr='label.cex'){
  layout_matrix = layout.norm(g$layout)

  vnames = names(vertex.attributes(g))
  if(!label.attr %in% vnames) {
    stop('"', label.attr, '" is not a valid vertex attribute)')
  } else {
    label = get.vertex.attribute(g, label.attr)
  }

  if(labelsize.attr %in% vnames){
    label.cex = get.vertex.attribute(g, labelsize.attr)
  } else {
    message('"', labelsize.attr, '" is not a valid vertex attribute). Labelsize is set to 1')
    label.cex = 1
  }

  ord = order(-degree(g)) ## reorder so that words with the least connections are rearranged first
  layout_matrix = layout_matrix[ord,]
  label.cex = label.cex[ord]
  label = label[ord]

  plot(range(layout_matrix), axes = F, frame.plot = F, xlab='', ylab='', type='n')
  newlayout = wordlayout(layout_matrix[,1], layout_matrix[,2], label, cex=label.cex*(labeldist_coef), rstep=0.05)

  ## calculate new cex based on percentual difference old and new word width
  #oldwidth = strwidth(label, cex=label.cex*(labeldist_coef))
  #shrinkcoef = newlayout[,'width'] / oldwidth
  #newlayout = cbind(newlayout, newcex=label.cex*shrinkcoef)

  newlayout = newlayout[match(1:nrow(newlayout), ord),] # return original order

  #g = set.vertex.attribute(g, labelsize.attr, value=newlayout[,'newcex'])
  g$layout = as.matrix(newlayout[,1:2])
  g
}

get_eigenvalue <- function(pcs){
  # equation for calculating eigenvals
  ev <- pcs$sdev^2
  return(ev)
}

# Function for scaling according to class memberships using PCA
rca_scale <- function(df, pca){
  # grab eigenvals
  rca_ev <- get_eigenvalue(pca)
  # which is max?
  dominant_factor <- which.max(rca_ev)
  # grab dominant factor
  dominant_factor_loading <- pca$rotation[,dominant_factor]
  # rescale data, mean = 0, sd = 1
  df <- sapply(df, scale)
  # scale the data using dominant factor
  scale_vals <- df %*% dominant_factor_loading
  # return scale
  return(scale_vals)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Function for plotting histogram of PCA scales
pca_plot_hist_version <- function(df, class = 1, title = "Partisan"){
  require(reshape2)
  covars_df <- df[,c(paste0("Scale", class), "RCA_new")]

  covars_df_melted <- melt(covars_df, id.vars = "RCA_new")
  covars_df_melted$RCA <- ifelse(covars_df_melted$RCA_new == class, paste0("Class ", class), paste0("Not in Class ", class))
  in_class <- subset(covars_df_melted, !grepl("Not in Class", covars_df_melted$RCA))
  not_in_class <- subset(covars_df_melted, grepl("Not in Class", covars_df_melted$RCA))

  cnames <- c("grey20", "grey70")
  names(cnames) <- c("Respondents Assigned to this Class", "Respondents Assigned to a Different Class")

  ggplot() +
    geom_histogram(bins = 50, data = in_class, aes(x = value, y = stat(count / sum(count)), fill = names(cnames)[1]), alpha=0.5) +
    geom_histogram(bins = 50, data = not_in_class, aes(x = value, y = stat(count / sum(count)), fill = names(cnames)[2]), alpha=0.3) +
    theme_classic(base_size = 14) +
    xlab(paste(title, "Scale")) +
    ylab("Proportion") +
    ggtitle("") +
    scale_fill_manual(name = "", values = cnames, breaks = names(cnames)) +
    theme(plot.title = element_text(hjust = 0.5))
}

grid_arrange_shared_legend <- function(..., COLNUM) {
  require(gridExtra)
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots, function(x)
      x + theme(legend.position="none")), ncol = COLNUM)),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight),
    newpage = F)
}

# For rand
adjRand_test=function(A, B, perm=999) {
  if (length(A)!=length(B)) { stop("A and B should have the same length") }
  # Make sure that the two groups of partitions have the same length

  ARIo=mclust::adjustedRandIndex(A, B)
  # Observed adjusted Rand index

  Aperm=lapply(seq_len(perm), function(X) sample(A, length(A), replace=FALSE))
  Bperm=lapply(seq_len(perm), function(X) sample(B, length(B), replace=FALSE))
  # Generate permuted samples

  ARIperm=unlist(lapply(seq_len(perm),
                        function(i) mclust::adjustedRandIndex(Aperm[[i]], Bperm[[i]])))
  # Compute adjusted Rand index for the permuted samples

  m=mean(ARIperm); v=var(ARIperm)
  # compute mean and variance of the permuted samples

  NARI=(ARIperm-m)/sqrt(v)
  # compute NARI (normalized ARI)

  NARIo=(ARIo-m)/sqrt(v)
  # compute observed NARI

  p_value=(length(which(NARI>NARIo))+1)/(perm+1)
  # Compute p value as proportion of permuted NARI larger than the observed

  Results=c(ARIo, p_value)
  names(Results)=c("Adjusted_Rand_index", "p_value")
  return(Results)
}

rand.index <- function (group1, group2) 
{
    x <- abs(sapply(group1, function(x) x - group1))
    x[x > 1] <- 1
    y <- abs(sapply(group2, function(x) x - group2))
    y[y > 1] <- 1
    sg <- sum(abs(x - y))/2
    bc <- choose(dim(x)[1], 2)
    ri <- 1 - sg/bc
    return(ri)
}

Rand_test=function(A, B, perm=999) {
  if (length(A)!=length(B)) { stop("A and B should have the same length") }
  # Make sure that the two groups of partitions have the same length

  RIo=fossil::rand.index(A, B)
  # Observed Rand index

  Aperm=lapply(seq_len(perm), function(X) sample(A, length(A), replace=FALSE))
  Bperm=lapply(seq_len(perm), function(X) sample(B, length(B), replace=FALSE))
  # Generate permuted samples

  RIperm=unlist(lapply(seq_len(perm),
                        function(i) fossil::rand.index(Aperm[[i]], Bperm[[i]])))
  # Compute Rand index for the permuted samples

  m=mean(RIperm); v=var(RIperm)
  # compute mean and vRIance of the permuted samples

  NRI=(RIperm-m)/sqrt(v)
  # compute NRI (normalized RI)

  NRIo=(RIo-m)/sqrt(v)
  # compute observed NRI

  p_value=(length(which(NRI>NRIo))+1)/(perm+1)
  # Compute p value as proportion of permuted NRI larger than the observed

  Results=c(RIo, p_value)
  names(Results)=c("Rand_index", "p_value")
  return(Results)
}