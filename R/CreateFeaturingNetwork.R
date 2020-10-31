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
#' feat_net<-CreateFeaturingNetwork(dataframe)
#' # Export the network to your computer and remove the first column (rownames)
#' write.csv(feat_net, "/PATH_TO_DIRECTORY/users_network.csv", rownames = FALSE)
#'
#' }
#'
#' @export

# Création de la table des liens/noeud
CreateFeaturingNetwork <- function(x) {
  base<-x[,c("artist_name.x","song_name")]
  require(stringr)
  base$featuring<-lapply(base$song_name,str_extract,pattern="\\(Ft\\.\\D[A-Za-z0-9\\D&éèàç@â]+")
  base<-base[-which(is.na(base$featuring)),]
  base$featuring<-as.character(base$featuring)
  # base$featuring<-str_replace_all(base$featuring,"\\D\\(Fra\\)|\\D\\(ES\\)|\\D\\(FRA\\)|\\D\\(FR\\)","")
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
  # j'ajoute cette nouvelle partie du code à la suite du premier :
  liste_chansons<-base$song_name[which(duplicated(base$song_name)==FALSE)]
  lenchanson<-length(liste_chansons)
  for (song in liste_chansons) {
    print(lenchanson)
    feat_par_chanson<-base$featuring[which(base$song_name == song)]
    feat_par_chanson_mix<-str_replace_all(feat_par_chanson," & ","@@@")
    feat_par_chanson_mix<-str_replace_all(feat_par_chanson_mix,", ","@@@")
    liste_feat_par_chanson<-as.data.frame.list((str_split(feat_par_chanson_mix,"@@@")))
    colnames(liste_feat_par_chanson)<-"X"
    if (nrow(liste_feat_par_chanson) > 1) {
      feat_fly<-as.data.frame(matrix(0,ncol = 2,nrow = (nrow(liste_feat_par_chanson))^2))
      colnames(feat_fly)<-c("Source","Target")
      a=1
      b=nrow(liste_feat_par_chanson)
      for (artiste in liste_feat_par_chanson$X) {
        feat_fly$Source[a:b]<-artiste
        feat_fly$Target[a:b]<-paste0(liste_feat_par_chanson$X)
        a=a+nrow(liste_feat_par_chanson)
        b=b+nrow(liste_feat_par_chanson)
      }
      # feat_fly<-feat_fly[-which(feat_fly$Source == feat_fly$Target),]
      # Je dois ajouter un morceau ici pour virer les doublons
      require(rlist)
      liste_remove<-c()
      a=as.numeric(length(unique(feat_fly$Source)))
      counter=1
      start=2
      range=a-2
      while (counter != a) {
        liste_remove<-list.append(liste_remove,c(start:(start+range)))
        start=start+a+1
        range=range-1
        counter=counter+1
      }
      feat_fly<-feat_fly[liste_remove,]
    } else {
      feat_fly<-as.data.frame(matrix(0,ncol = 2,nrow = 0))
      colnames(feat_fly)<-c("Source","Target")
    }
    featuring_network<-rbind(featuring_network,feat_fly)
    lenchanson=lenchanson-1
  }
  nodes_table<-x[which(x$artist_name.x %in% liste_artistes),c("artist_name.x","followers_count","annotation_count_artist","nombre_songs_by_artist","views_artists","Pays","Genre","Groupe_Solo","count_feat","moyenne_date","moyenne_date_par_mois","moyenne_date_par_annee","mediane_date","mediane_date_par_mois","mediane_date_par_annee")]
  nodes_table<-nodes_table[-which(duplicated(nodes_table$artist_name.x)==TRUE),]
  colnames(nodes_table)<-c("Id","followers_count","annotation_count_artist","nombre_songs_by_artist","views_artists","Pays","Genre","Groupe_Solo","count_feat","moyenne_date","moyenne_date_par_mois","moyenne_date_par_annee","mediane_date","mediane_date_par_mois","mediane_date_par_annee")
  edges_table<-featuring_network
  nodes_table$Id<-str_replace_all(nodes_table$Id,"\\s","-")
  nodes_table$Id<-str_replace_all(nodes_table$Id,"\\'","")
  nodes_table$Id<-str_replace_all(nodes_table$Id,"\\’","")
  edges_table$Source<-as.character(edges_table$Source)
  edges_table$Target<-as.character(edges_table$Target)
  edges_table$Source<-str_replace_all(edges_table$Source,"\\s","-")
  edges_table$Target<-str_replace_all(edges_table$Target,"\\s","-")
  edges_table$Source<-str_replace_all(edges_table$Source,"\\'","")
  edges_table$Target<-str_replace_all(edges_table$Target,"\\'","")
  edges_table$Source<-str_replace_all(edges_table$Source,"\\’","")
  edges_table$Target<-str_replace_all(edges_table$Target,"\\’","")
  
  return(list(edges_table,nodes_table))
}
