# Lage Der Nation 2020 Podcast Visualisieren
# Meta Daten werden aus RSS-Feed gesammelt




# Libraries Laden ---------------------------------------------------------


library(tidyverse)
library(lubridate)
library(XML)
library(xml2)
library(RCurl)
library(feedeR)
library(methods)
library(here)
library(xslt)
library(plotly)
library(tidytext)

theme_set(theme_light())

rm(list = ls())  #Loescht alle Vorherigen Variablen und Daten aus R


# RSS - Feed laden --------------------------------------------------------

url = "https://feeds.lagedernation.org/feeds/ldn-mp3.xml"

# Rohdaten laden
download.file(url,destfile = "data/LDN_RSS_FEED.xml")

LDN_xml <- xmlParse(here("data/LDN_RSS_FEED.xml"))
LDN_list <- xmlToList(LDN_xml)



# Metadaten aus LDN_list in ein tibble schreiben --------------------------

# Daten die aus dem XML / Liste geholt werden sollen. 
meta_episonden_lst <- c("title","subtitle","link","pubDate","duration")
Var_Namen <- c(meta_episonden_lst,"Kapitel_Start","Kapitel_Titel")

meta_episooden_kapitel_lst <- c("chapter.start","chapter.title") #Löschen ? 

# Leere df's für die for schleife
LDN_df2 <- Var_Namen %>% 
  purrr::map_dfc(setNames, object = list(c("")))


LDN_df <- Var_Namen %>% 
  purrr::map_dfc(setNames, object = list(logical()))

for (folge in 22:240) {
  LDN_df2 <-
    Var_Namen %>%
    purrr::map_dfc(setNames, object = list(c("")))
  
  for (spalte in 1:length(meta_episonden_lst)) {
    j = meta_episonden_lst[spalte]
    
    if (is.null(LDN_list$channel[[folge]][[j]][[1]])) {
      LDN_df2[1, spalte] = NA # Meta Daten schreiben
    } else{
      
      if (!is.null(LDN_list$channel[[folge]][["chapters"]][[1]])) {
        for (kapitel in 1:(length(LDN_list$channel[[folge]][["chapters"]]) - 1)) {
          LDN_df2[0 + kapitel, spalte] = LDN_list$channel[[folge]][[j]][[1]][[1]] # Meta Daten schreiben
          
          
          LDN_df2[0 + kapitel, 6] = LDN_list$channel[[folge]][["chapters"]][[kapitel]][[1]] # Kapitel schreiben
          LDN_df2[0 + kapitel, 7] = LDN_list$channel[[folge]][["chapters"]][[kapitel]][[2]] # Kapitel schreiben
        }
      } else{
        LDN_df2[1, spalte] = LDN_list$channel[[folge]][[j]][[1]][[1]] # Meta Daten schreiben
        
        LDN_df2[1, 6] = NA # Kapitel schreiben
        LDN_df2[1, 7] = NA # Kapitel schreiben
        
      }
    }
  }
  LDN_df = rbind(LDN_df, LDN_df2)
}






# fehlende Daten manuel ergänzen ------------------------------------------

# Länge der Folegen

LDN_df_angepasst <- LDN_df %>% 
  mutate(duration = ifelse(title == "LdN183 Corona-Update, Bundestag remote, AfD-Streit, UN-Klimakonferenz" ,
                           "01:13:05",
                           duration),
         duration = ifelse(title == "LdN178 COVID-19, Nazi-Terror in Hanau, Wahl in Hamburg, Kandidatensuche Union, Professionelle Sterbehilfe, Windenergie, Assange im Auslieferungsverfahren" ,
                           "01:55:36",
                           duration),
         duration = ifelse(title == "LdN160 Terror in Halle, Syrien, Brexit, Klimapaket, Feedback: Pendlerpauschale, Extinction Rebellion" ,
                           "01:40:13",
                           duration),
         duration = ifelse(title == "LdN139 Mitte-Studie, Banken-Fusion (Interview Gerhard Schick) Lobbyismus, Terror Sri Lanka, Schuldenbremse, Korrektur: Mobilfunk" ,
                           "01:30:38",
                           duration),
         duration = ifelse(title == "LdN122 Brexit-Chaos, Attentat in Straßburg, Abtreibungen, Ermittlungen gegen correctiv, Deutsche Bahn Analyse" ,
                           "01:49:42",
                           duration),
         duration = ifelse(title == "LdN083 Regierungsbildung, SPD-Parteitag, Bayern-Plan, Asylzahlen, Post, US-Shutdown" ,
                           "00:39:48",
                           duration),
         duration = ifelse(title == "LdN030 Steinmeier for BuPrä, Merkel tritt an, Trump´s Team, private Autobahnen",
                           "01:38:37",
                           duration),
         duration = ifelse(title == "LdN027 CETA verschoben, Rentenbericht, Steuer-Mios für Startups, Populismus, NSU-DNA",
                           "01:04:14",
                           duration),
         duration = ifelse(title == "LdN Sommerinterview 2: Peter Altmaier",
                           "00:39:48",
                           duration),
         duration = ifelse(title == "LdN025 Failed Freistaat, Trumps Absturz, CETA, NSA-Selektoren, Schul-WLAN",
                           "01:51:40",
                           duration),
         duration = ifelse(title == "LdN022 Wahlen in Berlin, Gabriel pro CETA, “Snowden”, BND-Reform",
                           "01:32:32",
                           duration),
         duration = ifelse(title == "LdN019 Kein Nizza, Brexit, Ulf in Brüssel, Teilhabegesetz & News",
                           "01:30:27",
                           duration))

# Testen, ob Folgenlängen fehlen
LDN_df_angepasst %>% 
  group_by(title) %>% 
  summarize(duration = last(duration)) %>% 
  filter(duration == "00:00:00") 



# Sichern des neuen Datensatzes -----------------------------------------------


write.csv(LDN_df_angepasst,here("data/LDN_Datensatz.csv"),
          row.names = F)



# Quellenangaben aus XML lesen und aufbereiten --------------------------------------------------------------

# Einzelne daten aus XML lesen
Shownotes <- xpathApply(LDN_xml,"//channel/item/content:encoded",xmlValue)
title <- xpathApply(LDN_xml,"//channel/item/title",xmlValue)

# Tibble / Data Frame erstellen
LDN_xml_df <- data.frame(cbind(title,Shownotes))


LDN_xml_df <- as_tibble(LDN_xml_df) %>% 
  mutate(title = as.character(title))


LDN_Full <- left_join(LDN_df_angepasst,LDN_xml_df, by = "title")

LDN_Full%>% 
  mutate(Shownotes =  str_replace_all(string = Shownotes, " ", ""))

write.csv(LDN_Full,here("data/LDN_Datensatz_mit_Quelle.csv"),
          row.names = F)





#b = LDN_xml_df %>% 
b = LDN_Full %>% 
  filter(title >="LdN171 USA töten iranischen General (Interview Alexander Graf Lambsdorff), “Umweltsau”, Leipziger Silvesternacht, Politik gegen Steuertricks, Brände in Australien, Hacker-Kongress 36C3") %>% 
  group_by(title) %>% 
  mutate(Shownotes =  paste(unlist(Shownotes), collapse=' ')) %>%  # xxx
  mutate(Shownotes =  str_replace_all(string = Shownotes, "<|>|/", " ")) %>% 
  mutate(Shownotes =  gsub(pattern = "Verabschiedung.*","",Shownotes)) %>% 
  mutate(Shownotes =  str_extract_all(Shownotes,"\\s\\s\\((.*?)\\)")) %>% # Quellen die Hinter dem Link in Klammernstehen extrahieren
  mutate(Shownotes =  str_replace_all(string = Shownotes, "c\\(\"*", "")) %>% # xxx
#  mutate(Shownotes =  paste(unlist(Shownotes), collapse=' ')) %>% 
  mutate(Shownotes =  str_replace_all(string = Shownotes, " ", "")) %>% 
  mutate(Shownotes =  str_replace_all(string = Shownotes, ";|,|https:|app.", "")) %>% 
  mutate(Shownotes =  str_replace_all(string = Shownotes, "&#8211", "")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "[\r\n]" , "")) %>% # Absätze entfernen 
  
  #Unterschiedlich benannte, aber gleiche Quellen umbenennen. 
  mutate(Shownotes =  str_replace_all(string = Shownotes, "SüddeutscheZeitung", "Süddeutsche")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "süddeutsche.de", "Süddeutsche")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bSZ\\b", "Süddeutsche")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bsz.de\\b", "Süddeutsche")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bSüddeutsche.de\\b", "Süddeutsche")) %>%
  
  mutate(Shownotes =  str_replace_all(string = Shownotes, "spiegel.de", "SpiegelOnline")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "SPON", "SpiegelOnline")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "Spon", "SpiegelOnline")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "Spiegelonline", "SpiegelOnline")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bSpiegel\\b", "SpiegelOnline")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bDerSpiegel\\b", "SpiegelOnline")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bSpiegelPlus\\b", "SpiegelOnline")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bSPIEGELPlus\\b", "SpiegelOnline")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bDERSPIEGEL\\b", "SpiegelOnline")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bSPIEGELONLINE\\b", "SpiegelOnline")) %>%
  
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bFAZ\\b", "FrankfurterAllgemeine")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bFrankfurterAllgemeine.NET\\b", "FrankfurterAllgemeine")) %>%
  
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\btwitter\\b", "Twitter")) %>%
 
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\btagesschau\\b", "Tagesschau")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bTagesschau.de\\b", "Tagesschau")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bwahl.Tagesschau\\b", "Tagesschau")) %>%
  
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\btagesSpiegelOnline\\b", "Tagesspiegel")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bDerTagesspiegel\\b", "Tagesspiegel")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bm.Tagesspiegel\\b", "Tagesspiegel")) %>%
 
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bNewYorkTimes\\b", "TheNewYorkTimes")) %>%
  
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bZeitonline\\b", "ZEITONLINE")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bzeitonline\\b", "ZEITONLINE")) %>%
  
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bhandelsblatt.com\\b", "Handelsblatt")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bhbhandelsblatt.com\\b", "Handelsblatt")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bclick.redaktion.Handelsblatt\\b", "Handelsblatt")) %>%
  
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\byoutube.com\\b", "YouTube")) %>%
  
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\btaz.de\\b", "TAZ")) %>%
  
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bZDFheute\\b", "ZDF")) %>%
  
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bzdf.de\\b", "ZDF")) %>%

  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\brki.de\\b", "RKI")) %>%

  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bheise.de\\b", "Heise")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bheiseonline\\b", "Heise")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bHeiseMagazine\\b", "Heise")) %>%
  
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bWELT\\b", "DIEWELT")) %>%
  mutate(Shownotes =  str_replace_all(string = Shownotes, "\\bwelt.de\\b", "DIEWELT")) %>%
  ungroup()


c = b %>%
  unnest_tokens(word,Shownotes,to_lower = F) 


c %>% 
  count(word, sort = T) %>% 
  view()





