#' Write dataset list
#'
#' Exports the results of the featuring network function function under 2 csv file in your working directory (!! May not yet work with windows)
#'
#' @param dataframes_list The result of the featuring network function. A variable consisting in a list of two dataframes : one for the nodes of the network and the other for the edges
#'
#' @return  The csv files are exported in the working directory and can be then used in a network analysing software like Gephi
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Get a network of featuring between artists :
#' network <- ExportFeaturingNetworkData(base)
#' # Export the network to your computer and remove the first column (rownames)
#' write_featuring_dataset_list<-(network)
#'
#' }
#'
#' @export

ExportFeaturingNetworkData <- function(dataframes_list) {
  path_1<-getwd()
  name<-c("edges_table","nodes_table")
  compteur<-1
  for (df in dataframes_list) {
    write.csv(df,paste0(path_1,"/",name[compteur],".csv"),row.names = FALSE)
    compteur=compteur+1
  }
}