#' Tools for creating social choice models.
#'
#' This package lets a researcher create, explore and analyze different social choice models. There are four main components of the package are generating voters, spatial voting models, voting rules and modeling tools.
#' 
#' Researcher interested in analyzing electoral and policy making systems (i.e. legislatures) should look at the related legislatuR package.
#'
#' 1) Generating Voters
#' A set of tools for generating sets of voters (see ?genVoter).
#'
#' 2) Multidimensional Spatial Models
#' A set of tools for exploring multidimensional spatial voting models (see ?spatialModel).
#' 
#' 3) Voting Rules
#' A large set of voting rules/social choice rules/preference aggregation mechanisms (see ?votingRule)
#' 
#' 4) Modeling
#'A set of modeling tools that allow a researcher to combine (1), (2) and/or (3) in varying combinanations, to analyzie a wide range of social choice phenomona. A user may also use any of the four components in combination with their own code. (?)
#'
#'Some possible ways to use the tools provided in this package are: Exploring simple one-period voting models. Creating static models of overlaping voting rules. Exploring dynamic simulations ar agent-based models. (see ?spatialModel ?dynamicModel) 
#'
#' 
#' 
#' @references Ragan, Robi (2017). "Social Choice Simulations". Working Paper, \url{http://userwww.service.emory.edu/~rragan/software.htm/}.
#' @import ggplot2
#' @import truncdist
#' @import deldir
#' @docType package
#' @name voteR
#' @useDynLib voteR
NULL