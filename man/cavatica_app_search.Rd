% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SyntacticSugar.R
\name{cavatica_app_search}
\alias{cavatica_app_search}
\title{Non-exact app searching by name}
\usage{
cavatica_app_search(project, name)
}
\arguments{
\item{project}{a sevenbridges 'Project' object. See readme for instructions on how to create this object}

\item{name}{Name of app. Does not need to be complete or exact.}
}
\value{
Applists
}
\description{
Non-exact app searching by name. Typically used to find an apps ID for  \code{cavatica_app_get}
}
