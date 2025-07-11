% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_vpc_exactbins.R
\name{plot_vpc_exactbins}
\alias{plot_vpc_exactbins}
\title{Plot a visual predictive check (VPC) with exact time bins}
\usage{
plot_vpc_exactbins(
  sim,
  pcvpc = FALSE,
  time_vars = c(TIME = "TIME", NTIME = "NTIME"),
  output_vars = c(PRED = "PRED", IPRED = "IPRED", SIMDV = "SIMDV", OBSDV = "OBSDV"),
  loq = NULL,
  strat_var = NULL,
  irep_name = "SIM",
  min_bin_count = 1,
  show_rep = TRUE,
  lower_bound = 0,
  shown = NULL,
  theme = NULL,
  timeu = "hours",
  n_breaks = 8,
  ...
)
}
\arguments{
\item{sim}{Input dataset. Must contain the following variables: \code{"ID"}, \code{"TIME"}}

\item{pcvpc}{logical for prediction correction. Default is \code{FALSE}.}

\item{time_vars}{Names of actual and nominal time variables. Must be named character vector.
Defaults is: c(\code{TIME}=\code{"TIME"}, \code{NTIME}=\code{"NTIME"}).}

\item{output_vars}{Names of model outputs from \code{model}. Must be named character vector.
Defaults is: c(\code{PRED}= \code{"PRED"}, \code{IPRED} = \code{"IPRED"}, \code{DV}= \code{"DV"}).}

\item{loq}{Numeric value of the lower limit of quantification (LLOQ) for the assay. Passed to \code{lloq} argument
of \code{\link[vpc:vpc]{vpc::vpc()}}. Specifying this argument implies that \code{OBSDV} is missing in \code{sim} where < LLOQ.}

\item{strat_var}{Character string of stratification variable passed to \code{stratify} argument
of \code{\link[vpc:vpc]{vpc::vpc()}}. Currently, only a single stratifying variable is supported.}

\item{irep_name}{Name of replicate variable in \code{data}. Must be a string. Default is \code{"SIM"}.}

\item{min_bin_count}{Minimum number of quantifiable observations in exact bin for inclusion
in binned plot layers. This argument drops small bins from summary statistic calculation
but retains these observations in the observed data points.}

\item{show_rep}{Display number of replicates as a plot caption. Default is \code{TRUE}.}

\item{lower_bound}{Lower bound of the dependent variable for prediction correction. Default is \code{0}.}

\item{shown}{Named list of logicals specifying which layers to include on the plot. Passed to \code{show} argument of \code{\link[vpc:vpc]{vpc::vpc()}}.

Defaults are:
\itemize{
\item Observed points: \code{obs_dv} = TRUE.
\item Observed quantiles: \code{obs_ci} = TRUE
\item Simulated inter-quantile range:\code{pi} = FALSE
\item Simulated inter-quantile area: \code{pi_as_area} = FALSE
\item Simulated Quantile CI: \code{pi_ci} = TRUE
\item Observed Median: \code{obs_median} = TRUE
\item Simulated Median: \code{sim_median} = FALSE
\item Simulated Median CI: \code{sim_median_ci} = TRUE
}}

\item{theme}{Named list of aesthetic parameters for the plot.Passed to \code{vpc_theme} arumgent of \code{\link[vpc:vpc]{vpc::vpc()}}.
Defaults can be obtained by running \code{\link[vpc:new_vpc_theme]{vpc::new_vpc_theme()}} with no arguments.}

\item{timeu}{Character string specifying units for the time variable.
Passed to \code{breaks_time} and assigned to default x-axis label.
Options include:
\itemize{
\item "hours" (default)
\item "days"
\item "weeks"
\item "months"
}}

\item{n_breaks}{Number of breaks requested for x-axis. Default is 5.}

\item{...}{Other arguments passed to \code{\link[vpc:vpc]{vpc::vpc()}}.}
}
\value{
A list containing calculated VPC information (when \code{vpcdb=TRUE}), or a ggplot2 object (default)
}
\description{
\code{plot_vpc_exactbins()} is a wrapper function for \code{\link[vpc:vpc]{vpc::vpc()}}
that returns a \code{ggplot2} object.
}
\examples{
model <- model_mread_load(model = "model")
simout <- df_mrgsim_replicate(data = data_sad, model = model, replicates = 100,
dv_var = "ODV",
num_vars = c("CMT", "EVID", "MDV", "NTIME", "LLOQ", "WTBL", "FOOD"),
char_vars = c("USUBJID", "PART"),
irep_name = "SIM")

vpc_plot <- plot_vpc_exactbins(
sim = simout,
pcvpc = TRUE,
pi = c(0.05, 0.95),
ci = c(0.05, 0.95),
loq = 1)
}
