########################################################################
### Nested loop plot, comparing analysis methods, separate by DE & rand
### Modified from rsimsum package by Gasparini & White
########################################################################
## Check and install required packages
req_packages <- c("ggplot2","ggpubr","grid","gridExtra","cowplot","egg")
installed_pack <- req_packages %in% installed.packages()
if(!all(installed_pack)){install.packages(req_packages[installed_pack==F], repos="http://cran.us.r-project.org")}
#sapply(req_packages,require,character.only=T) #must be all TRUE

#' @keywords internal
.nlp2deref <- function(data, methodvar, by, stats, target, reflow, refhigh, top, linewidth=1,
                    pcol=NULL, ylab=stats, mlab=NULL, pheight=c(7,3), y.lim=c(NA,NA),
                    pgridwidth=0.2, pytickwidth=0.2, pborderwidth=0.3) {
  ### Compute internal data
  opts <- lapply(X = by, FUN = function(x) levels(data[[x]]))
  names(opts) <- by
  dgms <- do.call(expand.grid, opts)
  dgms[[".scenario"]] <- seq(nrow(dgms))
  data <- merge(x = data, y = dgms)
  data <- data[order(data[[".scenario"]]), ]
  
  # bit of a hack to include a horizontal segment at end of step plot
  # duplicates the last three rows of data (one for each method) and makes them an additional scenario
  data = rbind(data, tail(data, 3))
  data[(nrow(data)-2):nrow(data), ".scenario"] = data[nrow(data)-3, ".scenario"] + 1
  
  ### Compute limits and placement of nested loop plot labels
  limits <- range(data[["est"]], na.rm = TRUE)
  delta <- diff(range(data[["est"]])) / 10
  placement <- vector(mode = "list", length = length(by))
  for (i in seq_along(placement)) {
    if (i == 1) {
      if (top) {
        placement[[i]] <- c(round(limits[2], digits = 2) + delta, round(limits[2], digits = 2) + 2 * delta)
      } else {
        placement[[i]] <- c(round(limits[1], digits = 2) - 2 * delta, round(limits[1], digits = 2) - delta)
      }
    } else {
      if (top) {
        placement[[i]] <- c(placement[[i - 1]][2] + delta, placement[[i - 1]][2] + 2 * delta)
      } else {
        placement[[i]] <- c(placement[[i - 1]][1] - 2 * delta, placement[[i - 1]][1] - delta)
      }
    }
  }
  
  ### Rescale variables included in the nested loop plot
  for (i in seq_along(by)) {
    data[[paste0(".", by[i])]] <- scales::rescale(x = as.numeric(data[[by[i]]]), to = placement[[i]])
  }
  
  ### Build basic plot
  if (!is.null(methodvar)) {
    methodvar <- rlang::sym(methodvar)
    gg <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .scenario, y = est, group = !!methodvar)) +
      ggplot2::geom_hline(yintercept = target, linetype = "solid", col = "grey50", linewidth = linewidth-0.1)+
      ggplot2::geom_hline(yintercept = reflow, linetype = "dotted", linewidth = linewidth-0.1) +
      ggplot2::geom_hline(yintercept = refhigh, linetype = "dotted", linewidth = linewidth-0.1) +
      ggplot2::geom_step(mapping = ggplot2::aes(color = !!methodvar),linewidth=linewidth)
    gg2 <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .scenario, y = est, group = !!methodvar))
  } else {
    gg <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .scenario, y = est)) +
      ggplot2::geom_hline(yintercept = target, linetype = "solid", col = "grey50", linewidth = linewidth-0.1) +
      ggplot2::geom_hline(yintercept = reflow, linetype = "dotted", linewidth = linewidth-0.1) +
      ggplot2::geom_hline(yintercept = refhigh, linetype = "dotted", linewidth = linewidth-0.1) +
      ggplot2::geom_step(linewidth=linewidth)
    gg2 <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = .scenario, y = est))
  }
  gg <- gg +
    #ggplot2::labs(x = "Simulation scenarios", y = stats) + theme_bw() +
    ggplot2::labs(x = NULL, y = ylab) + theme_bw() +
    theme(axis.line.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          legend.position="bottom", plot.margin=unit(c(0.1,0.1,0,0.1),'lines'),
          axis.ticks.y = element_line(linewidth = pytickwidth), panel.grid=element_line(linewidth = pgridwidth),
          panel.border = element_rect(linewidth = pborderwidth),
          text=element_text(size=4)) +
    scale_x_continuous(breaks=seq(1, 72, by=2)) +
    scale_y_continuous(limits=c(y.lim[1],y.lim[2]))
  if(!is.null(pcol)){gg <- gg +
    scale_color_manual(name = "Randomisation method", labels = mlab, values=pcol)
  }
  gg2 <- gg2 +
    ggplot2::labs(x = "Simulation scenarios", y = ylab)
  
  ### Build and add legends of nested loop plot
  for (i in seq_along(by)) {
    .tmp <- rlang::sym(paste0(".", by[i]))
    gg2 <- gg2 + #ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = .scenario, y = est)) +
      ggplot2::geom_step(mapping = ggplot2::aes(y = !!.tmp), linewidth = linewidth-0.15) +
      ggplot2::annotate(geom = "text", x = 1, y = placement[[i]][2] + delta / 2,
                        label = paste0(by[i], ": ", paste(levels(data[[by[i]]]), collapse = ", ")),
                        hjust = 0, vjust = 0.5, size = 1.2) +
      theme_bw() +
      theme(panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank(),
            axis.line.y=element_blank(),axis.title.y=element_text(colour="white"),
            axis.text.y=element_text(colour="white"), axis.ticks.y=element_blank(),
            axis.text.x=element_blank(),axis.ticks.x=element_blank(),
            panel.grid = element_line(linewidth=pgridwidth), panel.border=element_rect(linewidth = pborderwidth),
            legend.position="bottom", plot.margin=unit(c(0,0.1,0.2,0.1),'lines'),
            text = element_text(size = 4)) +
      scale_x_continuous(breaks=seq(1, 72, by=2))
  }
  
  ### Put legend at bottom (for combined plots)
  top_legend <- get_legend(gg) #grab legend of top plot
  gg <- gg + theme(legend.position="none", text = element_text(size = 4)) #remove legend from top plot
  
  #gg_out <- ggarrange(gg,gg2,nrow=2,heights=pheight,common.legend=T,legend="bottom",align="h")
  gg_out <- egg::ggarrange(gg,gg2,plot_grid(top_legend),nrow=3,heights=c(pheight-0.025,0.5))
  
  ### Return plot
  #return(gg2)
  #return(gg)
  return(gg_out)
}