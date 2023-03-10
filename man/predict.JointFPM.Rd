% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.JointFPM.R
\name{predict.JointFPM}
\alias{predict.JointFPM}
\title{Post-estimation function for JointFPMs}
\usage{
\method{predict}{JointFPM}(
  JointFPM,
  type = "mean_no",
  newdata,
  t,
  exposed,
  gauss_init_nodes = 50,
  gauss_max_nodes = 5
)
}
\arguments{
\item{JointFPM}{A joint flexible parametric model of class \code{JointFPM}.}

\item{type}{A character vector defining the estimate of interest. Currently available
options are:
\itemize{
\item{\code{mean_no}: }{Estimates the mean number of events at time(s) \code{t}.}
\item{\code{diff}: }{Estimates the difference in mean number of events
between exposed and unexposed at time(s) \code{t}.}
}}

\item{newdata}{A \code{data.frame} including the variable parameters for the prediction. One
value for each variable used in either the recurrent or competing event
model is requierd}

\item{t}{A vector defining the time points for the prediction.}

\item{exposed}{A function that takes \code{newdata} as an argument and creates a new dataset
for the exposed group. This argument is required if \code{type = 'diff'}.
Please see details for more information.}

\item{gauss_init_nodes}{Number of nodes used for the initial Gaussian quadrature approximation
of the integral (default = 50):
\deqn{E[N(t)] = \int_{0}^{t} S(t)\lambda(t)}}

\item{gauss_max_iter}{The maximum number of iterations for the Gaussian quadrature
(default = 5).}
}
\value{
A \code{data.frame} with the following columns:
\itemize{
\item{\code{t}: }{The time for the prediction,}
\item{\code{fit}: }{The point estimate of the prediction,}
\item{\code{lci}: }{The lower confidence interval limit,}
\item{\code{uci}: }{The upper confidence interval limit.}
}
}
\description{
Predicts different estimates from a joint flexible parametric model.
Currently only the estimation of the mean number of events at different
time points is supported.
}
\details{
The function required for the \code{exposed} argument must take the \code{newdata}
dataset as argument and transform it to a new dataset that defines the
exposed group. If we assume that we have a model with one variable \code{trt},
which is a 0/1 coded treatment indicator. If we would like to obtain
the difference in mean number of events comparing untreated to treated
we could use the following function assuming that
\code{newdata = data.frame(trt = 0)}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{function(x)\{transform(x, trt = 1)\}
}\if{html}{\out{</div>}}
}
