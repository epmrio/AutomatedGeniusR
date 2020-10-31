#' Featuring network from artists songs
#'
#' Create a network of featuring collaboration between artists. The recuperation can be pretty long if there is a lot of song. For instance, to get all the lyrics from 10 artists it would take approximatly 1 hour
#'
#' @param x A dataset containing at least 3 columns : artist_id, song_name, and song_id (you can use the output of GetSongsArtistName or GetSongsArtistID). If you don't, make sure your x input is a dataframe containing at least those 3 columns
#' @param y A list of Artists IDS from which you want to scrap all the lyrics
#' @param feat_exclude if False, the function will get all the songs of the artist even the featuring with other artists. Default is set to True (for personnal purpose) but it could be pertinent to set it to false
#'
#' @return a dataframes with all the lyrics, with columns about artists, song etc...
#'
#' @examples
#'
#' \dontrun{
#'
#' ## all the lyrics of multiple artists :
#' lyrics<-RetreiveSongs(corpus,list_of_artists_id,feat_exclude=F)
#'
#' }
#'
#' @export

# X correspond à un data frame récupéré par les fonction de AutomatedGeniusR qui comporte une colonne id d'artiste, 
# une colonne Chanson et une colone id chanson. Y correspond à une liste d'ID d'artistes et la fonction feat_exclude exclu les feat de la récup
# (par défaut == TRUE)
RetrieveSongs <- function(x,y,feat_exclude=T) {
  require(geniusr)
  df_final<-as.data.frame(matrix(0,nrow = 0,ncol = 6))
  colnames(df_final)<-c("line","section_name","section_artist","song_name","artist_name","song_id")
  chansons_a_scraper<-x[which(x$artist_id %in% y),c("song_id","song_name")]
  require(stringr)
  if (feat_exclude==TRUE) {
    chansons_a_scraper<-chansons_a_scraper[-which(str_detect(chansons_a_scraper$song_name,"Ft.")),]
  } else{
    chansons_a_scraper<-chansons_a_scraper
  }
  nb_songs<-nrow(chansons_a_scraper)
  for (chanson_id in chansons_a_scraper$song_id) {
    estimated_time<-nb_songs*3
    print(paste0("Il reste ",nb_songs," chansons à scrapper. Le temps restant estimé est de ",estimated_time/60," minutes"))
    try(song_fly<-get_lyrics_id(chanson_id))
    try(df_final<-rbind(df_final,song_fly))
    nb_songs=nb_songs-1
  }
  return(df_final)
}
