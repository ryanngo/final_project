library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(jsonlite)
library(dplyr)

# Challenger Link https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv

challenger = read.csv("TFT_Challenger_MatchData.csv")


#condense data by removing rows
#Randomly sample 1000 rows since the data set is so enormous
challenger <- challenger[sample(nrow(challenger), 1000), ]

#convert columns to readable json columns
challenger$combination <- gsub("'",'"',challenger$combination)
challenger$champion <- gsub("'",'"',challenger$champion)


#unnesting combination column
clean_challenger <- challenger %>% 
  select_if(~!all(is.na(.))) %>% 
  mutate(combination = map(combination, fromJSON)) %>% 
  unnest_wider(col = combination)

#unnesting champion column
clean_challenger <- clean_challenger %>% 
  select_if(~!all(is.na(.))) %>% 
  mutate(champion = map(champion, fromJSON)) %>% 
  unnest_wider(col = champion)

# Creating a traits data set
traits_data <- clean_challenger[,c(5,7:30)]

# Creating a champion data set
champion_data <- clean_challenger[,c(5,31:ncol(clean_challenger))] 

#cleaning up the champion data
clean <- champion_data %>% 
  mutate(JarvanIV = sapply(JarvanIV, toString),
         Sona = sapply(Sona, toString),
         Rakan = sapply(Rakan, toString),
         XinZhao = sapply(XinZhao, toString),
         Neeko = sapply(Neeko, toString),
         Karma = sapply(Karma, toString),
         Soraka = sapply(Soraka, toString),
         Lulu = sapply(Lulu, toString),
         Malphite = sapply(Malphite, toString),
         Yasuo = sapply(Yasuo, toString),
         MasterYi = sapply(MasterYi, toString),
         Jinx = sapply(Jinx, toString),
         Kayle = sapply(Kayle, toString),
         MissFortune = sapply(MissFortune, toString),
         AurelionSol = sapply(AurelionSol, toString),
         KaiSa = sapply(KaiSa, toString),
         Annie = sapply(Annie, toString),
         Shaco = sapply(Shaco, toString),
         Rumble = sapply(Rumble, toString),
         Lux = sapply(Lux, toString),
         Fizz = sapply(Fizz, toString),
         Ekko = sapply(Ekko, toString),
         Gangplank = sapply(Gangplank, toString),
         Ziggs = sapply(Ziggs, toString),
         Graves = sapply(Graves, toString),
         Blitzcrank = sapply(Blitzcrank, toString),
         Lucian = sapply(Lucian, toString),
         Ezreal = sapply(Ezreal, toString),
         Vi = sapply(Vi, toString),
         ChoGath = sapply(ChoGath, toString),
         Poppy = sapply(Poppy, toString),
         Mordekaiser = sapply(Mordekaiser, toString),
         Jayce = sapply(Jayce, toString),
         Ashe = sapply(Ashe, toString),
         WuKong = sapply(WuKong, toString),
         Jhin = sapply(Jhin, toString),
         KhaZix = sapply(KhaZix, toString),
         Fiora = sapply(Fiora, toString),
         Leona = sapply(Leona, toString),
         Irelia = sapply(Irelia, toString),
         Xayah = sapply(Xayah, toString),
         Kassadin = sapply(Kassadin, toString),
         Thresh = sapply(Thresh, toString),
         Ahri = sapply(Ahri, toString),
         Syndra = sapply(Syndra, toString),
         Zoe = sapply(Zoe, toString),
         VelKoz = sapply(VelKoz, toString),
         Shen = sapply(Shen, toString),
         Darius = sapply(Darius, toString),
         Caitlyn = sapply(Caitlyn, toString),
         TwistedFate = sapply(TwistedFate, toString),
         Xerath = sapply(Xerath, toString))
glimpse(clean)

#convert empty strings as '0' and strings as '1' indicating that the champion is used in that match
clean1 <- clean
for(i in 1:nrow(clean1)){
  for(x in 2:ncol(clean1)){
  if(clean1[i,x] == ""){
    clean1[i,x] = '0'
    }else{
    clean1[i,x] = '1'
    }
  }
}
#converts columns into numeric values in order to sum champion usage
clean1[,] <- sapply(clean1[,], as.numeric)
colSums(clean1[,-1])

# Creating a new data frame to see which champion/unit most often gets x ranked (1-8, 1 being 1st to win, 8th being 1st to lose)
ranking_champion <- clean1
rank_and_champion <- ranking_champion %>%
  pivot_longer(cols = !Ranked, names_to = "Champion", values_to = "Count")

rank_and_champion_count <- rank_and_champion %>%
  count(Ranked,Champion,Count)

#Conditionally remove rows if Count == 0 in order to get the true number of champions used for each rank placement
rank_and_champion_count <- rank_and_champion_count[rank_and_champion_count$Count != 0, ]

glimpse(clean1)
glimpse(clean)

warnings()
glimpse(champion_data) 
glimpse(traits_data) 

rank_and_champion_count_copy <- rank_and_champion_count

rank_and_champion_count <- rank_and_champion_count %>%
  mutate(ranked_factor = as.factor(Ranked), n= ifelse(Ranked>4, -n,n))

rank_and_champion_count$Champion = as.factor(rank_and_champion_count$Champion)
getPalette = colorRampPalette(brewer.pal(8,"Pastel2"))
rank_and_champion_count %>%
  ggplot(aes(x = n,
             y = reorder(Champion,n),
             fill = ranked_factor)) + geom_col(data = rank_and_champion_count %>% filter(n > 0), position = position_stack(reverse = FALSE)) + geom_col(data = rank_and_champion_count %>% filter(n < 1), position = position_stack(reverse = TRUE)) + scale_fill_manual(values = brewer.pal(8,"RdBu")) + theme_linedraw() + labs(y = "Champion Names", fill = "Game Placement", title = "Which Champion had the Best Game Placement", subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")


champ_name = "Ahri"
rank_and_champion_count_copy %>%
  filter(Champion == "Ahri") %>%
  ggplot(aes(x = as.factor(Ranked), y = n, fill = as.factor(Ranked))) + geom_bar(stat="identity") + scale_fill_manual(values = brewer.pal(8,"Purples")) + labs(y = "Champion Usage", x = "Game Placement", fill = "Game Placement", title = paste("Rank Distribution for", champ_name, sep=" "), subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")


getPalette = colorRampPalette(brewer.pal(12,"Paired"))

rank_number_of_rows <- rank_and_champion_count_copy %>%
  filter(Ranked == "2") %>%
  nrow()
rank_number_of_rows

placement = "2"
rank_and_champion_count_copy %>%
  filter(Ranked == "2") %>%
  ggplot(aes(y = reorder(Champion, n), x = n, fill = reorder(Champion, n))) + geom_bar(stat="identity") + scale_fill_manual(values = getPalette(rank_number_of_rows)) + labs(x = "Champion Usage", y = "Champion Name", fill = "Champion", title = paste("Total Number of Times a Champion Received this Game Placement:", placement, sep=" "), subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")

total_champions <- length(unique(rank_and_champion_count_copy$Champion))
rank_and_champion_count_copy %>%
  ggplot(aes(y = reorder(Champion, n), x = n, fill = reorder(Champion, n))) + geom_bar(stat="identity") + scale_fill_manual(values = getPalette(total_champions)) + labs(x = "Champion Usage", y = "Champion Name", fill = "Champion", title = paste("Total Number of Times a Champion was Used", champ_name, sep=" "), subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")

glimpse(traits_data)
traits_exist <- traits_data
for(i in 1:nrow(traits_exist)){
  for(x in 2:ncol(traits_exist)){
    if(is.na(traits_exist[i,x]) || is.na(traits_exist[i,x]) == "NA"){
      traits_exist[i,x] = 0
    }else{
      traits_exist[i,x] = 1
    }
  }
}
glimpse(traits_exist) 

total_times_trait_used <- traits_exist %>%
  pivot_longer(cols = !Ranked, names_to = "Trait", values_to = "Count")
total_times_trait_used_count <- total_times_trait_used %>%
  count(Ranked,Trait, Count)
total_times_trait_used_count <- total_times_trait_used_count[total_times_trait_used_count$Count != 0, ]

glimpse(total_times_trait_used_count)

total_times_trait_used_count_copy <- total_times_trait_used_count

total_times_trait_used_count <- total_times_trait_used_count %>%
  mutate(ranked_factor = as.factor(Ranked), n= ifelse(Ranked>4, -n,n))

total_times_trait_used_count$Trait = as.factor(total_times_trait_used_count$Trait)

total_times_trait_used_count %>%
  ggplot(aes(x = n,
             y = reorder(Trait,n),
             fill = ranked_factor)) + geom_col(data = total_times_trait_used_count %>% filter(n > 0), position = position_stack(reverse = FALSE)) + geom_col(data = total_times_trait_used_count %>% filter(n < 1), position = position_stack(reverse = TRUE)) + scale_fill_manual(values = brewer.pal(8,"RdBu")) + theme_linedraw() + labs(y = "Traits", fill = "Game Placement", title = "Which Trait had the Best Game Placement", subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")


trait_name = "Chrono"
total_times_trait_used_count_copy %>%
  filter(Trait == trait_name) %>%
  ggplot(aes(x = as.factor(Ranked), y = n, fill = as.factor(Ranked))) + geom_bar(stat="identity") + scale_fill_manual(values = brewer.pal(8,"Purples")) + labs(y = "Trait Usage", x = "Game Placement", fill = "Game Placement", title = paste("Rank Distribution for", trait_name, sep=" "), subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")


getPalette = colorRampPalette(brewer.pal(12,"Paired"))

rank_number_of_rows <- total_times_trait_used_count_copy %>%
  filter(Ranked == "2") %>%
  nrow()
rank_number_of_rows

placement = "2"
total_times_trait_used_count_copy %>%
  filter(Ranked == placement) %>%
  ggplot(aes(y = reorder(Trait, n), x = n, fill = reorder(Trait, n))) + geom_bar(stat="identity") + scale_fill_manual(values = getPalette(rank_number_of_rows)) + labs(x = "Trait Usage", y = "Traits", fill = "Traits", title = paste("Total Number of Times a Trait Received this Game Placement:", placement, sep=" "), subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")

getPalette = colorRampPalette(brewer.pal(12,"Paired"))
total_traits <- length(unique(total_times_trait_used_count_copy$Trait))
total_times_trait_used_count_copy %>%
  ggplot(aes(y = reorder(Trait, n), x = n, fill = reorder(Trait, n))) + geom_bar(stat="identity") + scale_fill_manual(values = getPalette(total_traits)) + labs(x = "Trait Usage", y = "Trait", fill = "Traits", title = paste("Total Number of Times a Trait was Used", sep=" "), subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")


glimpse(challenger)
challenger_copy <- challenger
challenger_copy <- challenger_copy %>%
  mutate(ingameDuration = (ingameDuration/60),
         gameDuration = (gameDuration/60))
  
challenger_copy$Ranked = as.factor(challenger_copy$Ranked)
challenger_copy %>%
  ggplot(aes(x=gameDuration,y=ingameDuration, color=Ranked))+geom_point()

rank_options <- challenger_copy %>%
  distinct(Ranked) %>%
  arrange(Ranked)

champ_options <- rank_and_champion_count_copy %>%
  distinct(Champion) %>%
  arrange(Champion)

trait_options <- total_times_trait_used_count_copy %>%
  distinct(Trait,Ranked) %>%
  arrange(Trait,Ranked)

sequential_colors <- c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBu", "OrRd", "Oranges", "Greys", "Greens", "GnBu", "BuPu", "BuGn", "Blues")

sequential_colors_df <- data.frame(sequential_colors)
names(sequential_colors_df) <- c('Color Blind Friendly')

qual_color <- c("Set1", "Set3", "Pastel1", "Pastel2", "Accent")
qual_cb <- c("Set2", "Paired", "Dark2")

qual_colors_df <- data.frame(qual_color)
qual_cb_df <- data.frame(qual_cb)

names(qual_colors_df) <- c('General Palette')
names(qual_cb_df) <- c('Color Blind Friendly')

div_color <- c("Spectral", "RdYlGn", "RdGy")
div_cb <- c("RdYlBu", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")

div_colors_df <- data.frame(div_color)
div_cb_df <- data.frame(div_cb)

names(div_colors_df) <- c('General Palettes')
names(div_cb_df) <- c('Color Blind Friendly')

divall = c("Spectral", "RdYlGn", "RdGy", "RdYlBu", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")
pal_type = c('General Palette','General Palette','General Palette','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly')

divvie <- data.frame(divall, pal_type)
names(divvie) <- c('Color Scheme', "Type")
glimpse(divvie)
divvie$`Color Scheme` <- as.factor(as.character(divvie$`Color Scheme`))
divvie$Type <- as.factor(as.character(divvie$Type))
divvvvvvv <- split(as.list(levels(divvie$`Color Scheme`)), divvie$Type)

names(div_colors_df) <- c('General Palettes')
names(div_cb_df) <- c('Color Blind Friendly')


getPalette = colorRampPalette(brewer.pal("Paired"))
