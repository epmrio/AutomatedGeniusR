#' Scrap Artists and songs information
#'
#' This function allow you to get the information about artists and songs from Genius API. You need to have package geniusr installed
#'
#' @param x A list of artists names
#'
#' @return A dataframe
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Get a dataset from the following list :
#' artists_list <- c("Bruce Springsteen", "Pink Floyd")
#' # Use the Function to retrieve information
#' artists_info <- GetSongsArtistsName(artists_list)
#'
#' }
#'
#' @export

GetSongsArtistsName <- function(x) {
  require(geniusr)
  require(stringr)
  artistes_total<-as.data.frame(matrix(0, ncol = 3, nrow = 0))
  colnames(artistes_total)<-c("artist_id","artist_name","artist_url")
  len_artistes<-length(x)
  for (artiste in x) {
    artiste<-as.character(artiste)
    artiste2<-str_to_lower(artiste)
    artiste2<-str_replace_all(artiste2," ","-")
    artiste2<-str_replace_all(artiste2,"\\s","-")
    artiste2<-str_replace_all(artiste2,"\\(","")
    artiste2<-str_replace_all(artiste2,"\\)","")
    artiste2<-str_replace_all(artiste2,"\\’","")
    artiste2<-str_replace_all(artiste2,"é","e")
    artiste2<-str_replace_all(artiste2,"è","e")
    artiste2<-str_replace_all(artiste2,"ê","e")
    print(paste0("Il reste ",len_artistes," artistes à récupérer avant de scrapper les chansons"))
    try(data<-search_artist(artiste2,n_results = 200))
    data<-data[-which(data$artist_name!=artiste),]
    artistes_total<-rbind(artistes_total,data)
    len_artistes=len_artistes-1
  }
  liste_id<-artistes_total$artist_id
  len_id<-length(liste_id)
  songs_total<-as.data.frame(matrix(0,ncol = 7,nrow = 0))
  colnames(songs_total)<-c("song_id","song_name","song_lyrics_url","song_annotation_count","artist_id","artist_name","artist_url")
  # création du total des annotations, etc
  id_annot_total<-as.data.frame(matrix(0,ncol = 3,nrow = 0))
  colnames(id_annot_total)<-c("artist_id","annotation_count_artist","nombre_songs_by_artist")
  for (id in liste_id) {
    print(paste0("Récupération des chansons de ",len_id," artistes"))
    print(1)
    try(songs_artistes<-get_artist_songs_df(id))
    print(2)
    try(songs_artistes$nombre_songs_by_artist<-nrow(songs_artistes))
    print(3)
    try(songs_total<-rbind(songs_total,songs_artistes))
    print(4)
    #on rempli l'id_annot_total
    try(songs_artistes$annotation_count_artist<-sum(songs_artistes$song_annotation_count))
    print(5)
    try(id_annot_brouillon<-songs_artistes[,c("artist_id","annotation_count_artist","nombre_songs_by_artist")])
    print(6)
    try(id_annot_total<-rbind(id_annot_total,id_annot_brouillon))
    print(7)
    len_id<-len_id-1
  }
  id_songs<-songs_total$song_id
  len_songs_meta<-length(id_songs)
  songs_meta_total<-as.data.frame(matrix(0,ncol = 13,nrow = 0))
  colnames(songs_meta_total)<-c("song_id","song_name","song_lyrics_url","song_art_image_url","song_release_date","song_pageviews","song_annotation_count","artist_id","artist_name","artist_url","album_id","album_name","album_url")
  for (song_id_ in id_songs) {
    print(paste0("Recuperation des metadata pour ",len_songs_meta," chansons..."))
    try(song_meta<-get_song_meta(song_id_))
    songs_meta_total<-rbind(songs_meta_total,song_meta)
    len_songs_meta<-len_songs_meta-1
  }
  len_infoscomp<-length(liste_id)
  info_comp_total<-as.data.frame(matrix(0,ncol = 5,nrow = 0))
  colnames(info_comp_total)<-c("artist_id","artist_name","artist_url","artist_image_url","followers_count")
  for (art_id in liste_id) {
    print(paste0("Recuperation d'informations complémentaires pour ",len_infoscomp," artistes. C'est presque fini"))
    try(art_meta<-get_artist_df(art_id))
    info_comp_total<-rbind(info_comp_total,art_meta)
    len_infoscomp<-len_infoscomp-1
  }
  songs_meta_total<-merge(songs_meta_total,info_comp_total,by="artist_id")
  songs_meta_total<-merge(songs_meta_total,id_annot_total,by="artist_id")
  songs_meta_total<-songs_meta_total[-which(duplicated(songs_meta_total$song_id)==TRUE),]
  # colonne des views total
  songs_meta_total$song_pageviews[is.na(songs_meta_total$song_pageviews)]<-0
  songs_meta_total$views_artists<-NA
  for (artist_identifiant in liste_id) {
    songs_meta_total$views_artists[which(songs_meta_total$artist_id == artist_identifiant)]<-sum(songs_meta_total$song_pageviews[which(songs_meta_total$artist_id == artist_identifiant)])
  }
  print("Le processus de récupération est terminé")
  songs_meta_total$artist_name.y<-NULL
  songs_meta_total$artist_url.y<-NULL
  names(songs_meta_total)[match("artist_name.x",names(songs_meta_total))] <- "artist_name"
  names(songs_meta_total)[match("artist_url.x",names(songs_meta_total))] <- "artist_url"
  return(songs_meta_total)
}