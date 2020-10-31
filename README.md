# AutomatedGeniusR
Using GeniusR pacakge function to scrap more data

This package contains functions that proceed to an iteration of some of the GeniusR package.  
For instance, it allows, from a list of Genius Artists ID's to automaticly retrieve and put in a dataset a lot of informations about those artists.  
The functions in this package relies on Genius API and the R package GeniusR. The objective was to automise some of the already existing functions and try to give some build in analysis tools of API data  
This package was originally developped for a personal use so the function may not be suited for specific analysis  
For now, there is 5 functions that should most of the time be used in that order :  

- GetSongsArtistsName() = Retrieve a Dataframe from a list of ArtistsName. The dataframe contains most of Genius information about songs of inputed list of artists. It also includes number of songs of artists, artist Id's, informations about artists, etc.
- GetSongsArtistsID() = The output is the same than ScrapSongsArtistName but is more reliable because with the Id's there is now risk of the API not finding the artists (that issue can happen when using directly the artist name). However this is more difficult to organise because it requires to get all the ID's of artists
- RetrieveSongs() = Retrieve a dataframe with all the lyrics of songs of one or more artists
- CreateFeaturingNetwork() = Creates formatted data to build a network representation of featuring relations between artists. Input data must be the output of RetrieveSongs function
- ExportFeaturingNetworkData() = Function to export the result of CreateFeaturingNetwork function. It writes 2 files on your local machine. One file contains Nodes information and the other Edges information. Both files can then be open in a network analysis software such as Gephi


Personal notes on future functions : Pour la prochaine modification, il faut que j'ajoute dans le script de récupération des artistes par les noms et les ids, des fonctions permettant d'avoir directement le nombre de feat que la personne a fait et aussi la moyenne et médiane des dates pour estimer "l'ancienneté". De plus, éventuellement ajouter un scrap de la date du premier album ?
