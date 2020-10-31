#' Scrap Artists and songs information from ID
#'
#' This function allow you to get the information aboute artists and songs from an artist ID from Genius API. You need to have package geniusr installed
#'
#' @param x A list of artists IDs
#'
#' @return A dataframe
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Get a dataset from the following list :
#' ID_list <- c("ID-1", "ID-2")
#' # Use the Function to retrieve information
#' artists_info <- GetSongsArtistsID(ID_list)
#'
#' }
#'
#' @export

GetSongsArtistsID <- function(x) {
  require(geniusr)
  require(stringr)
  len_id<-length(x)
  songs_total<-as.data.frame(matrix(0,ncol = 7,nrow = 0))
  colnames(songs_total)<-c("song_id","song_name","song_lyrics_url","song_annotation_count","artist_id","artist_name","artist_url")
  # création du total des annotations, etc
  id_annot_total<-as.data.frame(matrix(0,ncol = 3,nrow = 0))
  colnames(id_annot_total)<-c("artist_id","annotation_count_artist","number_songs_by_artist")
  for (id in x) {
    print(paste0("Récupération des chansons de ",len_id," artistes"))
    print(1)
    try(songs_artistes<-get_artist_songs_df(id))
    print(2)
    try(songs_artistes$number_songs_by_artist<-nrow(songs_artistes))
    print(3)
    try(songs_total<-rbind(songs_total,songs_artistes))
    print(4)
    #on rempli l'id_annot_total
    try(songs_artistes$annotation_count_artist<-sum(songs_artistes$song_annotation_count))
    print(5)
    try(id_annot_brouillon<-songs_artistes[,c("artist_id","annotation_count_artist","number_songs_by_artist")])
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
  len_infoscomp<-length(x)
  info_comp_total<-as.data.frame(matrix(0,ncol = 5,nrow = 0))
  colnames(info_comp_total)<-c("artist_id","artist_name","artist_url","artist_image_url","followers_count")
  for (art_id in x) {
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
  for (artist_identifiant in x) {
    songs_meta_total$views_artists[which(songs_meta_total$artist_id == artist_identifiant)]<-sum(songs_meta_total$song_pageviews[which(songs_meta_total$artist_id == artist_identifiant)])
  }
  print("Le processus de récupération est terminé")
  return(songs_meta_total)
}