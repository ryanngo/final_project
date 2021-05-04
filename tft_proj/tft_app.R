#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load libraries

library(shiny)
library(tidyverse)
library(shinydashboard)
library(lubridate)
library(forcats)
library(magrittr)
library(shinyjs)
library(RColorBrewer)
library(ggthemes)
library(jsonlite)

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

rank_and_champion_count_copy <- rank_and_champion_count

rank_and_champion_count <- rank_and_champion_count %>%
  mutate(ranked_factor = as.factor(Ranked), n= ifelse(Ranked>4, -n,n))

rank_and_champion_count$Champion = as.factor(rank_and_champion_count$Champion)

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

total_times_trait_used <- traits_exist %>%
  pivot_longer(cols = !Ranked, names_to = "Trait", values_to = "Count")
total_times_trait_used_count <- total_times_trait_used %>%
  count(Ranked,Trait, Count)
total_times_trait_used_count <- total_times_trait_used_count[total_times_trait_used_count$Count != 0, ]


total_times_trait_used_count_copy <- total_times_trait_used_count

total_times_trait_used_count <- total_times_trait_used_count %>%
  mutate(ranked_factor = as.factor(Ranked), n= ifelse(Ranked>4, -n,n))

total_times_trait_used_count$Trait = as.factor(total_times_trait_used_count$Trait)

challenger_copy <- challenger
challenger_copy <- challenger_copy %>%
  mutate(ingameDuration = (ingameDuration/60),
         gameDuration = (gameDuration/60))
challenger_copy$Ranked = as.factor(challenger_copy$Ranked)

rank_options <- challenger_copy %>%
  distinct(Ranked) %>%
  arrange(Ranked)

champ_options <- rank_and_champion_count_copy %>%
  distinct(Champion) %>%
  arrange(Champion)

trait_options <- total_times_trait_used_count_copy %>%
  distinct(Trait) %>%
  arrange(Trait)

sequential_colors = c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBu", "OrRd", "Oranges", "Greys", "Greens", "GnBu", "BuPu", "BuGn", "Blues")
pal_type1 = c('Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly')
seq_df <- data.frame(sequential_colors, pal_type1)
names(seq_df) <- c('Color Scheme', "Type")
seq_df$`Color Scheme` <- as.factor(as.character(seq_df$`Color Scheme`))
seq_df$Type <- as.factor(as.character(seq_df$Type))
seq_lists <- split(as.list(levels(seq_df$`Color Scheme`)), seq_df$Type)

qualitative_colors = c("Set1", "Set3", "Pastel1", "Pastel2", "Accent","Set2", "Paired", "Dark2")
pal_type2 = c('General Palette','General Palette','General Palette','General Palette','General Palette','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly')
qual_df <- data.frame(qualitative_colors, pal_type2)
names(qual_df) <- c('Color Scheme', "Type")
qual_df$`Color Scheme` <- as.factor(as.character(qual_df$`Color Scheme`))
qual_df$Type <- as.factor(as.character(qual_df$Type))
qual_lists <- split(as.list(levels(qual_df$`Color Scheme`)), qual_df$Type)



divergent_colors = c("Spectral", "RdYlGn", "RdGy", "RdYlBu", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")
pal_type3 = c('General Palette','General Palette','General Palette','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly','Color Blind Friendly')
div_df <- data.frame(divergent_colors, pal_type3)
names(div_df) <- c('Color Scheme', "Type")
div_df$`Color Scheme` <- as.factor(as.character(div_df$`Color Scheme`))
div_df$Type <- as.factor(as.character(div_df$Type))
div_lists <- split(as.list(levels(div_df$`Color Scheme`)), div_df$Type)


#selectInput("palette1",
 #           "Select a Color Palette:",
  #          choices = divvvvvvv)


ui <- dashboardPage(
  dashboardHeader(title = "Teamfight Tactics"
  ),
  dashboardSidebar(
    sidebarMenu(menuItem("Rank Distribution",
                         tabName = "ranking_tab",
                         icon = icon("pawn", lib="glyphicon")),
                menuItem("Champion Rank Distribution",
                         tabName = "champ_tab",
                         icon = icon("king", lib="glyphicon")),
                menuItem("Trait Rank Distribution",
                         tabName = "trait_tab",
                         icon = icon("queen", lib="glyphicon"))
    )),
  
  dashboardBody(
    tabItems(
    tabItem(tabName = "ranking_tab",
            fluidRow(
            box(selectInput("palette3",
                            "Select a Color Palette:",
                            choices = qual_lists),plotOutput("rank_scatter_plot",height ="500"), width="12"),
            box(selectInput("ranking_to_highlight",
            "Select Ranking to Highlight:",
            choices = rank_options),
              plotOutput("rank_line_plot",height ="500"), width="12"))),
      tabItem(tabName = "champ_tab",
                    fluidRow(
                      box(selectInput("palette1",
                                      "Select a Color Palette:",
                                      choices = qual_lists),plotOutput("champ_bar_plot",height ="500"), width="12"),
                      box(selectInput("palette6",
                                      "Select a Color Palette:",
                                      choices = div_lists), plotOutput("champ_rank_distribution",height ="500"), width = "12"),
                      box(selectInput("palette8",
                                      "Select a Color Palette:",
                                      choices = seq_lists),selectInput("champion_to_highlight",
                                      "Select Champion to Highlight:",
                                      choices = champ_options), 
                      plotOutput("highlight_champ_bar_plot",height ="500"), width="12"),
                      box(selectInput("palette4",
                                      "Select a Color Palette:",
                                      choices = qual_lists),selectInput("champion_rank_to_highlight",
                                      "Select Rank to Highlight:",
                                      choices = rank_options),
                      plotOutput("highlight_champion_rank_bar_plot",height ="500"), width="12")
                    )),
    tabItem(tabName = "trait_tab",
            fluidRow(box(selectInput("palette2",
                                     "Select a Color Palette:",
                                     choices = qual_lists), plotOutput("trait_bar_plot",height ="500"), width="12"),
              box(selectInput("palette7",
                              "Select a Color Palette:",
                              choices = div_lists), plotOutput("trait_rank_distribution",height ="500"), width = "12"),
              box(selectInput("palette9",
                              "Select a Color Palette:",
                              choices = seq_lists),selectInput("trait_to_highlight",
                              "Select Trait to Highlight:",
                              choices = trait_options),
                  plotOutput("highlight_trait_bar_plot",height ="500"), width="12"),
              box(selectInput("palette5",
                              "Select a Color Palette:",
                              choices = qual_lists),selectInput("trait_rank_to_highlight",
                              "Select Rank to Highlight:",
                              choices = rank_options),
                  plotOutput("highlight_trait_rank_bar_plot",height ="500"), width="12")    
    )))))

# Define server logic
server <- function(input, output) {
  
  output$rank_line_plot <- renderPlot({
    
    highlighted_region <- challenger_copy %>%
      filter(Ranked == input$ranking_to_highlight)
    
    challenger_copy %>%
      ggplot(aes(y = gameDuration,
                 x = ingameDuration,
                 group = Ranked)) +
      geom_point(color = "grey") +
      geom_point(data = highlighted_region,
                 color = "red") +
      geom_line(data = highlighted_region,
                color = "red") +
      theme_linedraw() +
      labs(x = "In-game Duration (in Minutes)",
           y = "Game Duration (in Minutes)",
           title = "Relationship Between In-game Duration vs Game Duration by Rank", subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")+ theme(plot.caption = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    
  })
  
  output$rank_scatter_plot <- renderPlot({
     challenger_copy %>%
      ggplot(aes(x=gameDuration,y=ingameDuration, color=Ranked))+geom_point() +
      labs(x = "In-game Duration (in Minutes)",
           y = "Game Duration (in Minutes)",
           title = "In-game Duration vs Game Duration Rank Distribution", subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")+ theme(legend.position="bottom", plot.caption = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+ scale_color_brewer(palette=input$palette3)
    
  })
  
  
  output$champ_bar_plot <- renderPlot({
    total_champions <- length(unique(rank_and_champion_count_copy$Champion))
    getPalette = colorRampPalette(brewer.pal(total_champions,input$palette1))
    rank_and_champion_count_copy %>%
      ggplot(aes(y = reorder(Champion, n), x = n, fill = reorder(Champion, n))) + geom_bar(stat="identity") + scale_fill_manual(values = getPalette(total_champions)) + labs(x = "Champion Usage", y = "Champion Name", fill = "Champion", title = paste("Total Number of Times a Champion was Used"), subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")+ theme(plot.caption = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

    
  })
  
  output$highlight_champ_bar_plot <- renderPlot({
    rank_and_champion_count_copy %>%
      filter(Champion == input$champion_to_highlight) %>%
      ggplot(aes(x = as.factor(Ranked), y = n, fill = as.factor(Ranked))) + geom_bar(stat="identity") + scale_fill_manual(values = brewer.pal(8,input$palette8)) + labs(y = "Champion Usage", x = "Game Placement", fill = "Game Placement", title = paste("Rank Distribution for", input$champion_to_highlight, sep=" "), subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")+ theme(legend.position="bottom", plot.caption = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

    
  })
  output$highlight_champion_rank_bar_plot  <- renderPlot({
    rank_number_of_rows <- rank_and_champion_count_copy %>%
      filter(Ranked == input$champion_rank_to_highlight) %>%
      nrow()
    getPalette = colorRampPalette(brewer.pal(rank_number_of_rows,input$palette4))
    rank_and_champion_count_copy %>%
      filter(Ranked == input$champion_rank_to_highlight) %>%
      ggplot(aes(y = reorder(Champion, n), x = n, fill = reorder(Champion, n))) + geom_bar(stat="identity") + scale_fill_manual(values = getPalette(rank_number_of_rows)) + labs(x = "Champion Usage", y = "Champion Name", fill = "Champion", title = paste("Total Number of Times a Champion Received this Game Placement:", input$champion_rank_to_highlight, sep=" "), subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")+ theme(plot.caption = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    
  })
  
  output$champ_rank_distribution  <- renderPlot({
    rank_and_champion_count %>%
      ggplot(aes(x = n,
                 y = reorder(Champion,n),
                 fill = ranked_factor)) + geom_col(data = rank_and_champion_count %>% filter(n > 0), position = position_stack(reverse = FALSE)) + geom_col(data = rank_and_champion_count %>% filter(n < 1), position = position_stack(reverse = TRUE)) + scale_fill_manual(values = brewer.pal(8,input$palette6)) + theme_linedraw() + labs(y = "Champion Names", fill = "Game Placement", title = "Which Champion had the Best Game Placement", subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")+ theme(plot.caption = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    
  })
  
  output$trait_bar_plot <- renderPlot({
    total_traits <- length(unique(total_times_trait_used_count_copy$Trait))
    getPalette = colorRampPalette(brewer.pal(total_traits,input$palette2))
    total_times_trait_used_count_copy %>%
      ggplot(aes(y = reorder(Trait, n), x = n, fill = reorder(Trait, n))) + geom_bar(stat="identity") + scale_fill_manual(values = getPalette(total_traits)) + labs(x = "Trait Usage", y = "Trait", fill = "Traits", title = paste("Total Number of Times a Trait was Used", sep=" "), subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")+ theme(plot.caption = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    
    
  })
  
  output$highlight_trait_bar_plot <- renderPlot({
    total_times_trait_used_count_copy %>%
      filter(Trait == input$trait_to_highlight) %>%
      ggplot(aes(x = as.factor(Ranked), y = n, fill = as.factor(Ranked))) + geom_bar(stat="identity") + scale_fill_manual(values = brewer.pal(8,input$palette9)) + labs(y = "Trait Usage", x = "Game Placement", fill = "Game Placement", title = paste("Rank Distribution for", input$trait_to_highlight, sep=" "), subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")+ theme(legend.position="bottom", plot.caption = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    
    
  })
  output$highlight_trait_rank_bar_plot  <- renderPlot({
    rank_number_of_rows <- total_times_trait_used_count_copy %>%
      filter(Ranked == input$trait_rank_to_highlight) %>%
      nrow()
    getPalette = colorRampPalette(brewer.pal(rank_number_of_rows,input$palette5))
    total_times_trait_used_count_copy %>%
      filter(Ranked == input$trait_rank_to_highlight) %>%
      ggplot(aes(y = reorder(Trait, n), x = n, fill = reorder(Trait, n))) + geom_bar(stat="identity") + scale_fill_manual(values = getPalette(rank_number_of_rows)) + labs(x = "Trait Usage", y = "Trait Name", fill = "Trait", title = paste("Total Number of Times a Trait Received this Game Placement:", input$trait_rank_to_highlight, sep=" "), subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")+ theme(plot.caption = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    
  })
  
  output$trait_rank_distribution  <- renderPlot({
    total_times_trait_used_count %>%
      ggplot(aes(x = n,
                 y = reorder(Trait,n),
                 fill = ranked_factor)) + geom_col(data = total_times_trait_used_count %>% filter(n > 0), position = position_stack(reverse = FALSE)) + geom_col(data = total_times_trait_used_count %>% filter(n < 1), position = position_stack(reverse = TRUE)) + scale_fill_manual(values = brewer.pal(8,input$palette7)) + theme_linedraw() + labs(y = "Traits", fill = "Game Placement", title = "Which Trait had the Best Game Placement", subtitle = "in Riot Games' Autochess Game, Teamfight Tactics", caption = "https://www.kaggle.com/gyejr95/tft-match-data?select=TFT_Challenger_MatchData.csv")+ theme(plot.caption = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    
    
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)
