#' Featuring network from artists songs
#'
#' Create a network of featuring collaboration between artists
#'
#' @param x A dataset coming from scraping function of genius API
#'
#' @return 2 dataframes : One with a Source node and Target node, the other containing infos about artists. Those dataframe can be exported in csv and used in a network analysing software like Gephi
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Get a network of the following df :
#' feat_net<-create_featuring_network(dataframe)
#' # Export the network to your computer and remove the first column (rownames)
#' write.csv(feat_net, "/PATH_TO_DIRECTORY/users_network.csv", rownames = FALSE)
#'
#' }
#'
#' @export

# Création de la table des liens/noeud
create_featuring_network <- function(x) {
  base<-x[,c("artist_name.x","song_name")]
  require(stringr)
  base$featuring<-lapply(base$song_name,str_extract,pattern="\\(Ft\\.\\D[A-Za-z0-9\\D&éèàç@â]+")
  base<-base[-which(is.na(base$featuring)),]
  base$featuring<-as.character(base$featuring)
  base$featuring<-str_replace_all(base$featuring,"\\D\\(Fra\\)|\\D\\(ES\\)|\\D\\(FRA\\)|\\D\\(FR\\)","")
  base$featuring<-str_replace_all(base$featuring,"\\(Ft.\\D","")
  base$featuring<-str_replace_all(base$featuring,"[\\)]$","")
  liste_artistes<-base$artist_name.x[which(duplicated(base$artist_name.x)==FALSE)]
  featuring_network<-as.data.frame(matrix(0,ncol=2,nrow=0))
  colnames(featuring_network)<-c("Source","Target")
  for (artiste in liste_artistes) {
    feat_par_artiste<-base$featuring[which(base$artist_name.x == artiste)]
    feat_par_artiste_mix<-paste0(feat_par_artiste,collapse = "@@@")
    feat_par_artiste_mix<-str_replace_all(feat_par_artiste_mix," & ","@@@")
    feat_par_artiste_mix<-str_replace_all(feat_par_artiste_mix,", ","@@@")
    feat_fly<-as.data.frame(matrix(0,ncol = 2,nrow = str_count(feat_par_artiste_mix,"@@@")+1))
    dim(feat_fly)
    colnames(feat_fly)<-c("Source","Target")
    liste_feat_par_artiste<-str_split(feat_par_artiste_mix,"@@@")
    liste_feat_par_artiste<-as.data.frame(liste_feat_par_artiste)
    feat_fly$Source<-artiste
    feat_fly$Target<-liste_feat_par_artiste[,1]
    featuring_network<-rbind(featuring_network,feat_fly)
  }
  nodes_table<-x[which(x$artist_name.x %in% liste_artistes),c("artist_name.x","followers_count","annotation_count_artist","nombre_songs_by_artist","views_artists","Pays","Genre","Groupe_Solo")]
  edges_table<-featuring_network
  return(list(edges_table,nodes_table))
}
