#' ---
#' title: "A small library containing a plot helper function"
#' author: "Richard Reeve"
#' date: "November 2019"
#' output: html_document
#' ---

#' File: 0204-plot-helper.r
#' ========================
#'
#' ### Function: plot_populations()
#'
#' Plot all of the populations in a data frame against time. One column must
#' be called "time" and will be used as the x-axis of the plot. The rest will
#' be used as different lines on the y-axis, with a legend denoting their
#' column names.
#'
#' #### Arguments:
#'
#' - **populations**: dataframe with columns corresponding to different
#'   population segments and a 'time' column
#' - (optionally) **new.graph**: whether to start a new graph - *default TRUE*
#' - (optionally) **ylim**: the limits of the y axis for new graphs only
#' - (optionally) **lty**: the line type for the graph, optionally one for each line - *default 1*
#' - (optionally) **col**: the colour for all lines, optionally one for all lines together - *default 1:num.populations*
#'  - **with.legend** (optionally) -- whether to include the legend (TRUE or FALSE), *default TRUE*
#' - **...**: any other (optional) arguments that plot and lines will both accept
#'
#' Note: you can set lty and col to c(susceptibles="green", ...) to change the
#' colour and line type of the populations by name.
plot_populations <- function(populations, new.graph=TRUE,
                             ylim=NA, lty=1, col=NA, with.legend=TRUE, ...)
{
  if (any(colnames(populations)=="time"))
  {
    time <- populations$time
    populations$time = NULL
  }
  else
    stop("No time available")
  
  # Sort out the line colours for the populations
  labels <- colnames(populations)
  if (is.na(col[1]))
    line.cols <- 1:length(labels)
  else
  {
    if (all(labels(col) == 1:length(col)))
      line.cols <- rep(col, length.out = length(labels))
    else
    {
      line.cols <- c()
      for (name in labels)
        line.cols <- c(line.cols, col[name])
    }
  }
  
  # Sort out the line types for the populations
  if (is.na(lty[1]))
    line.ltys <- 1:length(labels)
  else
  {
    if (all(labels(lty)==1:length(lty)))
      line.ltys <- rep(lty, length.out = length(labels))
    else
    {
      line.ltys <- c()
      for (name in labels)
        line.ltys <- c(line.ltys, lty[name])
    }
  }
  
  # Sort out the y limits on the graph if need be
  if (is.na(ylim[1]))
    ylim <- c(0, max(rowSums(populations)))
  
  # And now plot the graphs
  for (index in 1:length(labels))
  {
    label <- labels[index]
    this.pop <- populations[[label]]
    if (new.graph)
    { # When it's a new plot, do labels and legends, etc.
      plot(time, this.pop,
           ylim=ylim, xlab='time', ylab='population size',
           type='l', col=line.cols[index], lty=line.ltys[index], ...)
      if (with.legend) # Plot the legend if desired
        legend("topright", legend=labels, lty=line.ltys, col=line.cols)
      new.graph <- FALSE
    }
    else # Otherwise just draw the lines
      lines(time, this.pop, col=line.cols[index], lty=line.ltys[index], ...)
  }
}

#' ### Function: plot_simple()
#'
#' A simple plot all of the populations in a data frame against time. One column
#' must be called "time" and will be used as the x-axis of the plot. The rest
#' will be used as different lines on the y-axis, with a legend denoting their
#' column names. See plot_populations() above for a more sophisticated plotting
#' function.
#'
#' #### Arguments:
#'
#'  - **populations** -- data frame with columns corresponding to different population
#'                   segments and a 'time' column
#'  - **new.graph** (optionally) -- whether to start a new graph, *default TRUE*
#'  - **xlim** (optionally, for new graphs) -- the limits of the x axis, *default min to max time*
#'  - **ylim** (optionally, for new graphs) -- the limits of the y axis, *default min to max pop size*
#'  - **lty** (optionally) -- the line type for all lines on the graph, *default 1*
plot_simple <- function(populations, new.graph=TRUE, xlim=NA, ylim=NA, lty=1)
{
  # First make sure there's a time column in the data frame being plotted
  if (any(colnames(populations)=="time"))
  {
    time <- populations$time
    populations$time <- NULL
    # If need be, set x-limits on plot from it
    if (is.na(xlim[1]))
      xlim <- c(min(time), max(time))
  }
  else # Otherwise complain and stop...
    stop("No time info available - data frame must have a column called 'time'")
  
  # Get the column names of the data frame to use as labels
  labels <- colnames(populations)
  # Create our own standard set of colours
  line.cols <- 1:length(labels)
  
  # Get y-limits on graph from input if we haven't set our own
  if (is.na(ylim[1]))
    ylim <- c(0, max(rowSums(populations)))
  
  # And plot the individual columns against time
  for (index in 1:length(labels))
  {
    label <- labels[index]
    this.pop <- populations[[label]]
    if (new.graph)
    { # Either in a new graph if appropriate
      plot(time, this.pop,
           xlim=xlim, ylim=ylim,
           xlab='time', ylab='population size',
           type='l', col=line.cols[index], lty=lty[index])
      legend("topright", legend=labels, lty=lty, col=line.cols)
      new.graph <- FALSE
    }
    else # Or as a new line on an existing graph
      lines(time, this.pop, col=line.cols[index], lty=lty[index])
  }
}

#' #### Do the functions works without any external (global) information?

library(codetools)
globals <- findGlobals(plot_simple, merge=FALSE)$variables
if (length(globals) != 0)
{
  stop("Function plot_simple() may not use global variable(s): ",
       globals)
}
globals <- findGlobals(plot_populations, merge=FALSE)$variables
if (length(globals) != 0)
{
  stop("Function plot_populations() may not use global variable(s): ",
       globals)
}