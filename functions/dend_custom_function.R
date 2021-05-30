fviz_dend_custom <- function (x, k = NULL, h = NULL, k_colors = NULL, palette = NULL, 
          show_labels = TRUE, color_labels_by_k = TRUE, label_cols = NULL, 
          labels_track_height = NULL, repel = FALSE, lwd = 0.7, type = c("rectangle", 
                                                                         "circular", "phylogenic"), phylo_layout = "layout.auto", 
          rect = FALSE, rect_border = "gray", rect_lty = 2, rect_fill = FALSE, 
          lower_rect, horiz = FALSE, cex = 0.8, main = "Cluster Dendrogram", 
          xlab = "", ylab = "Height", sub = NULL, ggtheme = theme_classic(), 
          offset_labels, ...) 
{
  if (missing(k_colors) & !is.null(palette)) {
    k_colors <- palette
    palette <- NULL
  }
  if (!color_labels_by_k & is.null(label_cols)) 
    label_cols <- "black"
  type <- match.arg(type)
  circular <- type == "circular"
  phylogenic <- type == "phylogenic"
  rectangle <- type == "rectangle"
  if (inherits(x, "HCPC")) {
    k <- length(unique(x$data.clust$clust))
    x <- x$call$t$tree
  }
  if (inherits(x, "hcut")) {
    k <- x$nbclust
    dend <- as.dendrogram(x)
    method <- x$method
  }
  else if (inherits(x, "hkmeans")) {
    k <- length(unique(x$cluster))
    dend <- as.dendrogram(x$hclust)
    method <- x$hclust$method
  }
  else if (inherits(x, c("hclust", "agnes", "diana"))) {
    dend <- as.dendrogram(x)
    method <- x$method
  }
  else if (inherits(x, "dendrogram")) {
    dend <- x
    method <- ""
  }
  else stop("Can't handle an object of class ", paste(class(x), 
                                                      collapse = ", "))
  if (is.null(method)) 
    method <- ""
  else if (is.na(method)) 
    method <- ""
  if (is.null(sub) & method != "") 
    sub = paste0("Method: ", method)
  if (!is.null(dendextend::labels_cex(dend))) 
    cex <- dendextend::labels_cex(dend)
  dend <- dendextend::set(dend, "labels_cex", cex)
  dend <- dendextend::set(dend, "branches_lwd", lwd)
  k <- factoextra:::.get_k(dend, k, h)
  if (!is.null(k)) {
    if (ggpubr:::.is_col_palette(k_colors)) 
      k_colors <- ggpubr:::.get_pal(k_colors, k = k)
    else if (is.null(k_colors)) 
      k_colors <- ggpubr:::.get_pal("default", k = k)
    dend <- dendextend::set(dend, what = "branches_k_color", 
                            k = k, value = k_colors)
    if (color_labels_by_k) 
      dend <- dendextend::set(dend, "labels_col", k = k, 
                              value = k_colors)
  }
  if (!is.null(label_cols)) {
    dend <- dendextend::set(dend, "labels_col", label_cols)
  }
  leaflab <- ifelse(show_labels, "perpendicular", "none")
  if (xlab == "") 
    xlab <- NULL
  if (ylab == "") 
    ylab <- NULL
  max_height <- max(dendextend::get_branches_heights(dend))
  if (missing(labels_track_height)) 
    labels_track_height <- max_height/8
  # if (max_height < 1) 
  #   offset_labels <- -max_height/100
  # else offset_labels <- -0.1
  if (rectangle | circular) {
    p <- factoextra:::.ggplot_dend(dend, type = "rectangle", offset_labels = offset_labels, 
                      nodes = FALSE, ggtheme = ggtheme, horiz = horiz, 
                      circular = circular, palette = palette, labels = show_labels, 
                      label_cols = label_cols, labels_track_height = labels_track_height, ...)
    if (!circular) 
      p <- p + labs(title = main, x = xlab, y = ylab)
  }
  else if (phylogenic) {
    p <- .phylogenic_tree(dend, labels = show_labels, label_cols = label_cols, 
                          palette = palette, repel = repel, ggtheme = ggtheme, 
                          phylo_layout = phylo_layout, ...)
  }
  if (circular | phylogenic | is.null(k)) 
    rect <- FALSE
  if (rect_fill & missing(rect_lty)) 
    rect_lty = "blank"
  if (missing(lower_rect)) 
    lower_rect = -(labels_track_height + 0.5)
  if (rect) {
    p <- p + factoextra:::.rect_dendrogram(dend, k = k, palette = rect_border, 
                              rect_fill = rect_fill, rect_lty = rect_lty, size = lwd, 
                              lower_rect = lower_rect)
  }
  attr(p, "dendrogram") <- dend
  structure(p, class = c(class(p), "fviz_dend"))
  return(p)
}
