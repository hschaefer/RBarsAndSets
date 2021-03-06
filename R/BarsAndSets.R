#' Plot omics data along with functional classification
#'
#' This function plots a dataset (e.g. gene expression ratios) as a bar plot along with
#' one or multiple tracks indicating the membership in functional or regulatory categories.
#'
#' Expression data is given in the first argument as a vector of ratios. Note that the data
#' should be log transformed and ordered ascending for best results.
#'
#' Functional categories are given as a dataframe a column of TRUE/FALSE values for each set.
#' Column names of the data frame are used as labels in the plot.
#'
#'
#' @param bar_values log transformed, ordered vector of ratios.
#' @param categories Dataframe with T/F values indicating the membership in categories
#' and column names as labels.
#' @export
BarsAndSetsPlot <- function(bar_values, categories, title="") {
  values = bar_values
  nmeasures = length(values)
  sets = categories
  nsets = dim(sets)[2]
  set_names = colnames(sets)
  colors = RColorBrewer::brewer.pal(n=nsets, name="Set1")
  ylabel = "log2 fold change"

  font_size = 12
  grid::gpar(fontsize=font_size)

  left_margin = max(grid::unit(graphics::strwidth(set_names, font = font_size, units = 'in'),"inches")) + grid::unit(2,"lines")
  right_margin = grid::unit(1,"lines")
  yscale = c(min(values)*1.1,max(values)*1.1)
  frac_width = 1/nmeasures
  set_height = grid::unit(1,"lines")
  set_spacing = grid::unit(0.1,"lines")
  set_margin = grid::unit(0.5,"lines")
  set_area_height = nsets * set_height + (nsets+1)*set_spacing + 2*set_margin



  # Setup plot layout
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 1, heights = grid::unit.c(grid::unit(1,"null"), set_area_height))))
  grid::pushViewport(grid::viewport(layout.pos.row = 1))

  # upper part of the plot
  #pushViewport(plotViewport(c(0, 5, 1, 1), yscale = yscale)) # bottom, left, top, right
  grid::pushViewport(grid::viewport(x = left_margin,
                                    y = grid::unit(0, "line"),
                                    width = grid::unit(1, "native") - left_margin -right_margin,
                                    height = grid::unit(1, "native") - grid::unit(1, "line"),
                                    just = c("left", "bottom"),
                                    yscale = yscale))

  grid::grid.yaxis()
  grid::grid.text(title, x=unit(0.5,"npc"), y=unit(1,"npc"),hjust="center",gp = grid::gpar(fontsize=font_size))

  grid::grid.text(ylabel, x = grid::unit(-3, "lines"), rot = 90, gp = grid::gpar(fontsize=font_size))
  grid::grid.rect(x = grid::unit(c(0:(nmeasures-1))/nmeasures, "native"),
                  y = rep(grid::unit(0, "native"),nmeasures),
                  height = grid::unit(values, "native"),
                  width = grid::unit(frac_width, "native"),
                  gp = grid::gpar(fill = "#808080", lty=0),
                  just = c(0,0))

  grid::popViewport()
  grid::popViewport()
  # lower part of the plot
  grid::pushViewport(grid::viewport(layout.pos.row = 2))
  # draw a bar
  for(i in 1:nsets){
    y_offset = grid::unit(1, "native") - (i*set_height+i*set_spacing +set_margin)
    # draw text
    grid::grid.text(colnames(sets)[i],x = grid::unit(1,"lines"),y= y_offset, just = c("left", "bottom"), gp = grid::gpar(fontsize=font_size))
    # draw bar
    grid::pushViewport(grid::viewport(x = left_margin,
                                      y = y_offset,
                                      width = grid::unit(1, "npc") - sum(left_margin, right_margin),
                                      height=set_height,
                                      just = c("left", "bottom")
    ))

    grid::grid.rect(x = grid::unit(c(0:(nmeasures-1))/nmeasures, "native"),
                    y = rep(grid::unit(0, "native"),nmeasures),
                    height = grid::unit(1, "native"),
                    width = grid::unit(frac_width, "native"),
                    gp = grid::gpar(fill = colors[i], lty=0, alpha = as.numeric(t(sets[,i]))),
                    just = c(0,0))

    grid::popViewport()
  }


  grid::popViewport() #lower viewport
  grid::popViewport() # main

}
