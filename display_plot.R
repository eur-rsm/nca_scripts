library(NCA)

# Default values copied from p_constants
line_colors <- list(ols="green",        lh="red3",            cols="darkgreen",
                    qr="blue",          ce_vrs="orchid4",     cr_vrs="violet",
                    ce_fdh="red",       cr_fdh="orange",      sfa="darkgoldenrod",
                    c_lp="blue")
line_types  <- list(ols=1,              lh=2,                 cols=3,
                    qr=4,               ce_vrs=5,             cr_vrs=1,
                    ce_fdh=6,           cr_fdh=1,             sfa=7,
                    c_lp=2)
line_width  <- 1.5
point_type  <- 21
point_color <- 'red'

display_plot <-
function (plot) {
  flip.x <- plot$flip.x
  flip.y <- plot$flip.y

  # Determine the bounds of the plot based on the scope
  xlim <- c(plot$scope.theo[1 + flip.x], plot$scope.theo[2 - flip.x])
  ylim <- c(plot$scope.theo[3 + flip.y], plot$scope.theo[4 - flip.y])

  # Reset/append colors etc. if needed
  for (method in names(line_types)) {
    line.types[[method]] <- line_types[[method]]
  }
  if (is.numeric(line_width)) {
    line.width <- line_width
  }
  if (is.numeric(point_type)) {
    point.type <- point_type
  }
  if (point_color %in% colors()) {
    point.color <- point_color
  }
  # Only needed until the next release (3.0.2)
  if (!exists("point.color")) {
    point.color <- "blue"
  }

  # Plot the data points
  plot (plot$x, plot$y, pch=point.type, col=point.color, bg=point.color,
        xlim=xlim, ylim=ylim, xlab=colnames(plot$x), ylab=tail(plot$names, n=1))

  # Plot the scope outline
  abline(v=plot$scope.theo[1], lty=2, col="grey")
  abline(v=plot$scope.theo[2], lty=2, col="grey")
  abline(h=plot$scope.theo[3], lty=2, col="grey")
  abline(h=plot$scope.theo[4], lty=2, col="grey")

  # Plot the legend before adding the clipping area
  legendParams = list()
  for (method in plot$methods) {
    line.color <- line.colors[[method]]
    line.type  <- line.types[[method]]

    name <- gsub("_", "-", toupper(method))
    legendParams$names  = append(legendParams$names, name)
    legendParams$types  = append(legendParams$types, line.type)
    legendParams$colors = append(legendParams$colors, line.color)
  }
  if (length(legendParams) > 0) {
    legend("topleft", cex=0.7, legendParams$names,
           lty=legendParams$types, col=legendParams$colors, bg=NA)
  }

  # Apply clipping to the lines
  clip(xlim[1], xlim[2], ylim[1], ylim[2])

  # Plot the lines
  for (method in plot$methods) {
    line <- plot$lines[[method]]
    line.color <- line.colors[[method]]
    line.type  <- line.types[[method]]

    if (method %in% c("lh", "ce_vrs", "ce_fdh")) {
      lines(line[[1]], line[[2]], type="l",
            lty=line.type, col=line.color, lwd=line.width)
    } else {
      abline(line, lty=line.type, col=line.color, lwd=line.width)
    }
  }

  # Plot the title
  title(paste0("NCA Plot : ", plot$title), cex.main=1)
}
