
#import library
library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(tidyverse)
library(rpart)
library(partykit)
library(caret)
library(plotly)
library(DT)

#import data and wrangling
data <- read_csv('replay.csv')
data <- data %>%
  mutate(LeagueIndex = as.factor(LeagueIndex)) %>%
  mutate(LeagueLevel = ifelse(LeagueIndex == 1 | LeagueIndex == 2 | LeagueIndex == 3 | LeagueIndex == 4, "low", "high")) %>%
  slice(-1794) %>%
  mutate(age_group = ifelse(Age >=16 & Age <= 20, '16-20', 
                            ifelse(Age >20 & Age <=25, '21-25', 
                                   ifelse(Age > 25 & Age <= 30, '26-30', 
                                          ifelse(Age > 30 & Age <= 35, '30-35', 
                                                 ifelse(Age > 35 & Age <= 40, '36-40', '40-45'))))))

data$LeagueLevel <- factor(data$LeagueLevel, levels = c("low", "high"))
#factor levels in league index
levels(data$LeagueIndex) <- c('Bronze', 'Silver', 'Gold', 'Platinum', 'Diamond', 'Master', 'GrandMaster', 'Professional')

# Wrangle data for prediction models. 
data1 <- data %>%
  select(-GameID, -LeagueIndex, -MaxTimeStamp) %>%
  drop_na()


# Home Page ui 
ui_home <- fluidPage(
  p(h1('Learn How to Master Starcraft')), 
  fluidRow(
    column(10, 
           mainPanel(
             HTML('<center><img src="starcraft.png" width="400"></center>'))
    ),
    column(12, p(h3("Introduction")),
           p('StarCraft is a world-famous science fiction based real-time strategy game.
             Since the game was first released in 2000, it has attracted an enormous fan base and evolved into one
             of the most popular computer competition games. The story of StarCraft is set in a distant sector
             of the Milky Way galaxy, where the alien races in Terran space attempt to survive and dominate
             the others. Each player would assume roles and compete with other players real-time
             based on different story lines.'),
           p(""),
           p('In this strategy computer game, each StarCraft player plays the game using distinctive strategies,
             styles, and speed. These gaming habits are captured by a list of possible game actions statistics
             specific to StarCraft for example such as APM, action per minute, or Action Latency, the mean
             latency between two actions in milliseconds, or TotalMapExplored, etc. These game actions statistics
             are representative of a player’s efficiency and competency.'),
           p(""),
           p(h3("Analysis Goals")),
           p("We are interested in analysing how the game action statistics differ by different characteristics
             of the players, such as age and time commitment of the player. We are especially interested in how
             the game action statistics
             differ by the players’ league, which indicates how advanced the players are in the game. We are also
             interested in exploring patterns in the
             age and time commitment for players in different leagues.
             Lastly, we will provide a chance for the users to predict a player's league using their own chioces of
             model and predictor variables. The model choices include decision tree, knn, and random forest models.
             The predictor variables include general players' information (Age, Hours Per Week, etc.) and game action
             variables (Action Per Minute, Action Letency, etc.)."),
           p(""),
           p(h3("Data Description")),
           p("The dataset we use in our analysis is from a 2017 Starcraft Replay Dataset uploaded on
             Kaggle by Simon Fraser University. To navigate our website, please find a categorization and descriptions
             for variables used in our analysis below."),
           tags$ul(
             tags$li(tags$b("General Players' Information")),
             tags$ul(
               tags$li("Age: Age of each player."),
               tags$li("HoursPerWeek: Hours each player spent playing per week."),
               tags$li("TotalHours: Total hours each player spent playing"),
               tags$li("League Index: Indication of how advanced a player is. Ranks (lowest to highest):
                       Bronze, Silver, Gold, Platinum, Diamond, Master, GrandMaster, Professional leagues")
               )
             ),
           p(""),
           tags$ul(
             tags$li(tags$b("Game Action Statistics")),
             tags$ul(
               tags$li(tags$i("Action Speed Metrics")),
               tags$ul(
                 tags$li("APM: Action per minute."),
                 tags$li("ActionLatency: Mean latency from the onset of PACs (Perception Action Cycles) to their first action (milliseconds)."),
                 tags$li("ActionsInPAC: Mean number of actions within each PAC.")
               ),
               tags$li(tags$i("Hotkeys Usage Metrics")),
               tags$ul(
                 tags$li("SelectByHotkeys: Number of units selected using hotkeys per timestamp."),
                 tags$li("AssignToHotkeys: Number of units assigned to hotkeys per timestamp."),
                 tags$li("UniqueHotkeys: Number of unique hotkeys used per timestamp.")
               ),
               tags$li(tags$i("Map Usage Metrics")),
               tags$ul(
                 tags$li("MinimapAttacks: Minimum Number of attacks made in each map per timestamp."),
                 tags$li("MinimapRightClicks: Minimum Number of right-clicks made in each map per timestamp."),
                 tags$li("TotalMapExplored: Number of maps viewed by player per timestamp")
               ),
               tags$li(tags$i("Perception Action Cycle")),
               tags$ul(
                 tags$li("NumberOfPACs: Number of PACs per timestamp."),
                 tags$li("GapBetweenPACs: Mean duration between PACs (milliseconds)."),
                 tags$li("ActionLatency: Mean latency from the onset of PACs to their first action (milliseconds)."),
                 tags$li("ActionsInPAC: Mean number of actions within each PAC.")),
               tags$li(tags$i("Other Metrics")),
               tags$ul(
                 tags$li("WorkersMade: Number of SCVs, drones, probes trained per timestamp."),
                 tags$li("UniqueUnitsMade: Unique units made per timestamp."),
                 tags$li("ComplexUnitsMade: Number of ghosts, investors, and high templars trained per timestamp."),
                 tags$li("ComplexAbilityUsed: Abilities requiring specific targeting instructions used per timestamp."))
             ))
           )
           )
    )



#prediction page ui

ui_pred <- fluidPage(theme = shinytheme("sandstone"),
                     fluidRow(
                       column(12, 
                              div(style = "height:50px;", 
                                  h3("Predicting League Level")
                              ),
                              column(4,
                                     p("Want to know how well each characteristic of the player
                                       predict their league level? Here is where you can build
                                       you own prediction model. Choose a model you want to use 
                                       and several variables you think that predict a player's 
                                       league level the best. The graph on the right will show you
                                       the Accurracy, Sensitivity, Specificity, Precision of your 
                                       model, and the higher the values are, the better your model.
                                       Try to see if you can find the best model! Note that the random
                                       forest models can take a while to run."),
                                     p(""),
                                     p("Note: The prediction models predict a player's League Level based
                                       on whether it's in the low group (Bronze, Silver, Gold, Platinum) or
                                       high group (Diamond, Master, GrandMaster, Professional).
                                       The training and test data sets are split by 7:3.
                                       5-fold cross-validation is used for decision tree and
                                       knn, and 3-fold cross-validation is used for random forest to
                                       improve running time. Different tuning parameter values are used for
                                       each method.")),
                              column(3,
                                     selectInput("model", label = "Prediction Method: ",
                                                 c("Decision Tree" = "DT",
                                                   "KNN" = "knn",
                                                   "Random Forest" = "RF")),
                                     fluidRow(
                                       column(12, 
                                              checkboxGroupInput("variableInput", label = "Predictor Variables: ",
                                                                 c("Age" = "Age",
                                                                   "Total Hours" = "TotalHours",
                                                                   "Action Per Minute" = "APM",
                                                                   "Hours Per Week" = "HoursPerWeek",
                                                                   "Unit Selected By Hotkeys" = "SelectByHotkeys",
                                                                   "Unit Assinged To By Hotkeys" = "AssignToHotkeys",
                                                                   "Unique Hotkeys Used" = "UniqueHotkeys",
                                                                   "Minimum Number of Attacks" = "MinimapAttacks",
                                                                   "Minumum Number of Right Clicks" = "MinimapRightClicks",
                                                                   "Number of PAC (Perception Action Cycles)" = "NumberOfPACs",
                                                                   "Gap between PACs" = "GapBetweenPACs",
                                                                   "Latency from PAC Onset to First Action" = "ActionLatency",
                                                                   "Actions in PAC" = "ActionsInPAC",
                                                                   "Total Maps Explored" = "TotalMapExplored",
                                                                   "Number of Workers Made" = "WorkersMade",
                                                                   "Number of Units Made" = "UniqueUnitsMade",
                                                                   "Number of Complex Units Made" = "ComplexUnitsMade",
                                                                   "Number of Complex Ability Used" = "ComplexAbilityUsed"),
                                                                 selected = "Age"
                                              )
                                              
                                       ))),
                              column(5,
                                     plotOutput("stats")
                              )
                                     )))

#Datatable ui
ui_dt <- basicPage(
  h2("StarCraft Player Data"),
  p('This is the full data we used for our analysis. Each row represents the record of one player. '),
  DT::dataTableOutput("mytable")
)


#time committment variables by age ui
ui_age_time <-fluidPage(theme = shinytheme("sandstone"),
                        titlePanel(title = 'Game Action Statistics by Age'),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "age_time_variable", 
                                        label =  "Game Action Statistics",
                                        choices = c(
                                          "Total Hours" = "TotalHours",
                                          "Hours Per Week" = "HoursPerWeek"),
                                        selected = 'TotalHours'),
                            
                            #since there is no age data for professional player, 
                            #we exclude the choice of professional player for now
                            selectInput("Llevel_time", label =  "League Level  ",
                                        choices = c("Bronze" = "Bronze",
                                                    "Silver" = "Silver",
                                                    "Gold" = "Gold",
                                                    'Platinum' = 'Platinum',
                                                    "Diamond" = "Diamond",
                                                    "Master" = "Master",
                                                    "GrandMaster" = "GrandMaster"),
                                        selected = "Bronze"),
                            p('The boxplot on the right displays how each game action statistics differ by age groups, 
                              controlled by the league each player is in. We grouped the players into age brackets from 
                              16 to 45, with intervals of 5.'),
                            p(''),
                            p('Select the Game Action Statistics you are interested in and select the data by League Level. 
                              You should be able to observe the distribution of this Game Action Statistics by age.'),
                            p(''), 
                            p('As expected, since most of the Game Action Statistics are measures of players’ operation 
                              speed, the players in the older age groups in the same league tend to have lower Game Action 
                              Statistics. However, as League Level increases, the distribution of Game Action Statistics 
                              in each age group increases as well.')
                            ),
                          mainPanel(plotOutput("plot_age_time"))
                            )
                            )

#action variables by age ui
ui_age_action <-fluidPage(theme = shinytheme("sandstone"),
                          titlePanel(title = 'Game Action Statistics by Age'),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "age_action_variable", 
                                          label =  "Game Action Statistics",
                                          choices = c("Action Per Minute" = "APM",
                                                      "Latency from PAC Onset to First Action" = "ActionLatency",
                                                      "Actions in PAC" = "ActionsInPAC"),
                                          selected = 'APM'),
                              
                              #since there is no age data for professional player, 
                              #we exclude the choice of professional player for now
                              selectInput("Llevel_action", label =  "League Level  ",
                                          choices = c("Bronze" = "Bronze",
                                                      "Silver" = "Silver",
                                                      "Gold" = "Gold",
                                                      'Platinum' = 'Platinum',
                                                      "Diamond" = "Diamond",
                                                      "Master" = "Master",
                                                      "GrandMaster" = "GrandMaster"),
                                          selected = "Bronze"),
                              p('The boxplot on the right displays how each game action statistics differ by age groups, 
                                controlled by the league each player is in. We grouped the players into age brackets from 
                                16 to 45, with intervals of 5.'),
                              p(''),
                              p('Select the Game Action Statistics you are interested in and select the data by League Level. 
                                You should be able to observe the distribution of this Game Action Statistics by age.'),
                              p(''), 
                              p('As expected, since most of the Game Action Statistics are measures of players’ operation 
                                speed, the players in the older age groups in the same league tend to have lower Game Action 
                                Statistics. However, as League Level increases, the distribution of Game Action Statistics 
                                in each age group increases as well.')
                              ),
                            mainPanel(plotOutput("plot_age_action"))
                              )
                              )


#hotkeys usage variables by age ui
ui_age_hk <-fluidPage(theme = shinytheme("sandstone"),
                      titlePanel(title = 'Game Action Statistics by Age'),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "age_hk_variable", 
                                      label =  "Game Action Statistics",
                                      choices = c("Units Seleted Through Hotkeys" = "SelectByHotkeys",
                                                  "Units Assigned Through Hotkeys" = 'AssingToHotkeys',
                                                  "Unique Hotkeys Used" = "UniqueHotkeys"),
                                      selected = 'SelectByHotkeys'),
                          
                          #since there is no age data for professional player, 
                          #we exclude the choice of professional player for now
                          selectInput("Llevel_hk", label =  "League Level  ",
                                      choices = c("Bronze" = "Bronze",
                                                  "Silver" = "Silver",
                                                  "Gold" = "Gold",
                                                  'Platinum' = 'Platinum',
                                                  "Diamond" = "Diamond",
                                                  "Master" = "Master",
                                                  "GrandMaster" = "GrandMaster"),
                                      selected = "Bronze"),
                          p('The boxplot on the right displays how each game action statistics differ by age groups, 
                            controlled by the league each player is in. We grouped the players into age brackets from 
                            16 to 45, with intervals of 5.'),
                          p(''),
                          p('Select the Game Action Statistics you are interested in and select the data by League Level. 
                            You should be able to observe the distribution of this Game Action Statistics by age.'),
                          p(''), 
                          p('As expected, since most of the Game Action Statistics are measures of players’ operation 
                            speed, the players in the older age groups in the same league tend to have lower Game Action 
                            Statistics. However, as League Level increases, the distribution of Game Action Statistics 
                            in each age group increases as well.')
                          ),
                        mainPanel(plotOutput("plot_age_hk"))
                          )
                          )


#map usage variables by age ui
ui_age_map <-fluidPage(theme = shinytheme("sandstone"),
                       titlePanel(title = 'Game Action Statistics by Age'),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "age_map_variable", 
                                       label =  "Game Action Statistics",
                                       choices = c("Minimum Number of Attacks Per Map" = "MinimapAttacks",
                                                   'Number of Right Clikcs on Minimap' = 'MinimapRightClicks',
                                                   "Total Maps Explored" = "TotalMapExplored"),
                                       selected = 'MinimapAttacks'),
                           
                           #since there is no age data for professional player, 
                           #we exclude the choice of professional player for now
                           selectInput("Llevel_map", label =  "League Level  ",
                                       choices = c("Bronze" = "Bronze",
                                                   "Silver" = "Silver",
                                                   "Gold" = "Gold",
                                                   'Platinum' = 'Platinum',
                                                   "Diamond" = "Diamond",
                                                   "Master" = "Master",
                                                   "GrandMaster" = "GrandMaster"),
                                       selected = "Bronze"),
                           p('The boxplot on the right displays how each game action statistics differ by age groups, 
                             controlled by the league each player is in. We grouped the players into age brackets from 
                             16 to 45, with intervals of 5.'),
                           p(''),
                           p('Select the Game Action Statistics you are interested in and select the data by League Level. 
                             You should be able to observe the distribution of this Game Action Statistics by age.'),
                           p(''), 
                           p('As expected, since most of the Game Action Statistics are measures of players’ operation 
                             speed, the players in the older age groups in the same league tend to have lower Game Action 
                             Statistics. However, as League Level increases, the distribution of Game Action Statistics 
                             in each age group increases as well.')
                           ),
                         mainPanel(plotOutput("plot_age_map"))
                           )
                           )


#pac usage variables by age ui
ui_age_pac <-fluidPage(theme = shinytheme("sandstone"),
                       titlePanel(title = 'Game Action Statistics by Age'),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "age_pac_variable", 
                                       label =  "Game Action Statistics",
                                       choices = c("Number of PAC (Perception Action Cycles)" = "NumberOfPACs",
                                                   "Gap between PACs" = "GapBetweenPACs"),
                                       selected = 'NumberOfPACs'),
                           
                           #since there is no age data for professional player, 
                           #we exclude the choice of professional player for now
                           selectInput("Llevel_pac", label =  "League Level  ",
                                       choices = c("Bronze" = "Bronze",
                                                   "Silver" = "Silver",
                                                   "Gold" = "Gold",
                                                   'Platinum' = 'Platinum',
                                                   "Diamond" = "Diamond",
                                                   "Master" = "Master",
                                                   "GrandMaster" = "GrandMaster"),
                                       selected = "Bronze"),
                           p('The boxplot on the right displays how each game action statistics differ by age groups, 
                             controlled by the league each player is in. We grouped the players into age brackets from 
                             16 to 45, with intervals of 5.'),
                           p(''),
                           p('Select the Game Action Statistics you are interested in and select the data by League Level. 
                             You should be able to observe the distribution of this Game Action Statistics by age.'),
                           p(''), 
                           p('As expected, since most of the Game Action Statistics are measures of players’ operation 
                             speed, the players in the older age groups in the same league tend to have lower Game Action 
                             Statistics. However, as League Level increases, the distribution of Game Action Statistics 
                             in each age group increases as well.')
                           ),
                         mainPanel(plotOutput("plot_age_pac"))
                           )
                           )


#other variables by age ui
ui_age_o <-fluidPage(theme = shinytheme("sandstone"),
                     titlePanel(title = 'Game Action Statistics by Age'),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(inputId = "age_o_variable", 
                                     label =  "Game Action Statistics",
                                     choices = c('Number of Workser Made' = 'WorkersMade', 
                                                 'Number of Unique Units Made' = 'UniqueUnitsMade',
                                                 'Number of Complex Units Made' = 'ComplexUnitsMade',
                                                 'Number of Complex Ability Used' = 'ComplexAbilityUsed'),
                                     selected = 'WorkersMade'),
                         
                         #since there is no age data for professional player, 
                         #we exclude the choice of professional player for now
                         selectInput("Llevel_o", label =  "League Level  ",
                                     choices = c("Bronze" = "Bronze",
                                                 "Silver" = "Silver",
                                                 "Gold" = "Gold",
                                                 'Platinum' = 'Platinum',
                                                 "Diamond" = "Diamond",
                                                 "Master" = "Master",
                                                 "GrandMaster" = "GrandMaster"),
                                     selected = "Bronze"),
                         p('The boxplot on the right displays how each game action statistics differ by age groups, 
                           controlled by the league each player is in. We grouped the players into age brackets from 
                           16 to 45, with intervals of 5.'),
                         p(''),
                         p('Select the Game Action Statistics you are interested in and select the data by League Level. 
                           You should be able to observe the distribution of this Game Action Statistics by age.'),
                         p(''), 
                         p('As expected, since most of the Game Action Statistics are measures of players’ operation 
                           speed, the players in the older age groups in the same league tend to have lower Game Action 
                           Statistics. However, as League Level increases, the distribution of Game Action Statistics 
                           in each age group increases as well.')
                         ),
                       mainPanel(plotOutput("plot_age_o"))
                         )
                         )





#time commitment variables by league ui
ui_gas_time <- fluidPage(theme = shinytheme("sandstone"),
                         titlePanel(title = 'Game Action Statistics by Player League'),
                         sidebarLayout(
                           sidebarPanel(
                             selectInput(inputId = "gas_time_variable", 
                                         label = "Game Action Statistics",
                                         choices = c(
                                           "Total Hours" = "TotalHours",
                                           "Hours Per Week" = "HoursPerWeek"
                                         )),
                             p('The boxplot on the right displays how each Game Action Statistics is distributed 
                               among players belonging to different leagues. The leagues are arranged from lowest
                               to highest, left to right.'), 
                             p(''),
                             p('As we can see, as the more advanced a league is, the higher the distribution of each 
                               statistics will be distributed around. These Game Action Statistics in the dataset 
                               can obviously measure the skills and accomplishment of each player.')
                             ),
                           mainPanel(
                             plotOutput("plot_gas_time")
                           )
                           
                           
                           
                             ))


#action variables by league ui
ui_gas_action <- fluidPage(theme = shinytheme("sandstone"),
                           titlePanel(title = 'Game Action Statistics by Player League'),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(inputId = "gas_action_variable", 
                                           label = "Game Action Statistics",
                                           choices = c( 
                                             "Action Per Minute" = "APM",
                                             "Latency from PAC Onset to First Action" = "ActionLatency",
                                             "Actions in PAC" = "ActionsInPAC"
                                           )),
                               p('The boxplot on the right displays how each Game Action Statistics is distributed 
                                 among players belonging to different leagues. The leagues are arranged from lowest
                                 to highest, left to right.'), 
                               p(''),
                               p('As we can see, as the more advanced a league is, the higher the distribution of each 
                                 statistics will be distributed around. These Game Action Statistics in the dataset 
                                 can obviously measure the skills and accomplishment of each player.')
                               ),
                             mainPanel(
                               plotOutput("plot_gas_action")
                             )
                               ))


#hotkeys usage  variables by league ui
ui_gas_hk <- fluidPage(theme = shinytheme("sandstone"),
                       titlePanel(title = 'Game Action Statistics by Player League'),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "gas_hk_variable", 
                                       label = "Game Action Statistics",
                                       choices = c(
                                         "Units Seleted Through Hotkeys" = "SelectByHotkeys",
                                         "Units Assigned Through Hotkeys" = 'AssingToHotkeys',
                                         "Unique Hotkeys Used" = "UniqueHotkeys"
                                       )),
                           p('The boxplot on the right displays how each Game Action Statistics is distributed 
                             among players belonging to different leagues. The leagues are arranged from lowest
                             to highest, left to right.'), 
                           p(''),
                           p('As we can see, as the more advanced a league is, the higher the distribution of each 
                             statistics will be distributed around. These Game Action Statistics in the dataset 
                             can obviously measure the skills and accomplishment of each player.')
                           ),
                         mainPanel(
                           plotOutput("plot_gas_hk")
                         )
                           ))


#map usage variables by league ui
ui_gas_map <- fluidPage(theme = shinytheme("sandstone"),
                        titlePanel(title = 'Game Action Statistics by Player League'),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "gas_map_variable", 
                                        label = "Game Action Statistics",
                                        choices = c(
                                          "Minimum Number of Attacks Per Map" = "MinimapAttacks",
                                          'Number of Right Clikcs on Minimap' = 'MinimapRightClicks',
                                          "Total Maps Explored" = "TotalMapExplored"
                                        )),
                            p('The boxplot on the right displays how each Game Action Statistics is distributed 
                              among players belonging to different leagues. The leagues are arranged from lowest
                              to highest, left to right.'), 
                            p(''),
                            p('As we can see, as the more advanced a league is, the higher the distribution of each 
                              statistics will be distributed around. These Game Action Statistics in the dataset 
                              can obviously measure the skills and accomplishment of each player.')
                            ),
                          mainPanel(
                            plotOutput("plot_gas_map")
                          )
                            ))


#pac usage variables by league ui
ui_gas_pac <- fluidPage(theme = shinytheme("sandstone"),
                        titlePanel(title = 'Game Action Statistics by Player League'),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "gas_pac_variable", 
                                        label = "Game Action Statistics",
                                        choices = c(
                                          "Number of PAC (Perception Action Cycles)" = "NumberOfPACs",
                                          "Gap between PACs" = "GapBetweenPACs"
                                        )),
                            p('The boxplot on the right displays how each Game Action Statistics is distributed 
                              among players belonging to different leagues. The leagues are arranged from lowest
                              to highest, left to right.'), 
                            p(''),
                            p('As we can see, as the more advanced a league is, the higher the distribution of each 
                              statistics will be distributed around. These Game Action Statistics in the dataset 
                              can obviously measure the skills and accomplishment of each player.')
                            ),
                          mainPanel(
                            plotOutput("plot_gas_pac")
                          )
                            ))


#other variables by league ui
ui_gas_o <- fluidPage(theme = shinytheme("sandstone"),
                      titlePanel(title = 'Game Action Statistics by Player League'),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "gas_o_variable", 
                                      label = "Game Action Statistics",
                                      choices = c( 
                                        'Number of Workser Made' = 'WorkersMade', 
                                        'Number of Unique Units Made' = 'UniqueUnitsMade',
                                        'Number of Complex Units Made' = 'ComplexUnitsMade',
                                        'Number of Complex Ability Used' = 'ComplexAbilityUsed'
                                      )),
                          p('The boxplot on the right displays how each Game Action Statistics is distributed 
                            among players belonging to different leagues. The leagues are arranged from lowest
                            to highest, left to right.'), 
                          p(''),
                          p('As we can see, as the more advanced a league is, the higher the distribution of each 
                            statistics will be distributed around. These Game Action Statistics in the dataset 
                            can obviously measure the skills and accomplishment of each player.')
                          ),
                        mainPanel(
                          plotOutput("plot_gas_o")
                        )
                          ))



#scatterplot
ui_scatter <- fluidPage(theme = shinytheme("sandstone"),
                        titlePanel(title = 'Game Action Statistcs Association'),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput('x_scatter', label = 'x-axis Variable:', 
                                        choices = c("Total Hours" = "TotalHours",
                                                    "Hours Per Week" = "HoursPerWeek",
                                                    "Action Per Minute" = "APM",
                                                    "Latency from PAC Onset to First Action" = "ActionLatency",
                                                    "Actions in PAC" = "ActionsInPAC",
                                                    "Units Seleted Through Hotkeys" = "SelectByHotkeys",
                                                    "Units Assigned Through Hotkeys" = 'AssingToHotkeys',
                                                    "Unique Hotkeys Used" = "UniqueHotkeys",
                                                    "Minimum Number of Attacks Per Map" = "MinimapAttacks",
                                                    'Number of Right Clikcs on Minimap' = 'MinimapRightClicks',
                                                    "Total Maps Explored" = "TotalMapExplored",
                                                    "Number of PAC (Perception Action Cycles)" = "NumberOfPACs",
                                                    "Gap between PACs" = "GapBetweenPACs",
                                                    'Number of Workser Made' = 'WorkersMade', 
                                                    'Number of Unique Units Made' = 'UniqueUnitsMade',
                                                    'Number of Complex Units Made' = 'ComplexUnitsMade',
                                                    'Number of Complex Ability Used' = 'ComplexAbilityUsed'),
                                        selected = 'APM'),
                            selectInput('y_scatter', label = 'y-axis Variable:', 
                                        choices = c("Total Hours" = "TotalHours",
                                                    "Hours Per Week" = "HoursPerWeek",
                                                    "Action Per Minute" = "APM",
                                                    "Latency from PAC Onset to First Action" = "ActionLatency",
                                                    "Actions in PAC" = "ActionsInPAC",
                                                    "Units Seleted Through Hotkeys" = "SelectByHotkeys",
                                                    "Units Assigned Through Hotkeys" = 'AssingToHotkeys',
                                                    "Unique Hotkeys Used" = "UniqueHotkeys",
                                                    "Minimum Number of Attacks Per Map" = "MinimapAttacks",
                                                    'Number of Right Clikcs on Minimap' = 'MinimapRightClicks',
                                                    "Total Maps Explored" = "TotalMapExplored",
                                                    "Number of PAC (Perception Action Cycles)" = "NumberOfPACs",
                                                    "Gap between PACs" = "GapBetweenPACs",
                                                    'Number of Workser Made' = 'WorkersMade', 
                                                    'Number of Unique Units Made' = 'UniqueUnitsMade',
                                                    'Number of Complex Units Made' = 'ComplexUnitsMade',
                                                    'Number of Complex Ability Used' = 'ComplexAbilityUsed'), 
                                        selected = 'ActionLatency'),
                            tableOutput('hover_data_scatter'), 
                            p('Use the dropdown menus for x-axis variable and y-axis variable to create a 
                              scatterplot that investigates the relationship between the variable you have selected.'),
                            p(''), 
                            p('Red dots indicate players belonging to the lower four leagues, Bronze, Silver, Gold, and Platinum. 
                              Blue dots indicate players belonging to the top four leagues, Diamond, Master, Grand Master, and Professional'),
                            p(''), 
                            p('Hover your mouse over any dots to see the exact values it indicates.')
                            
                            ),
                          mainPanel(plotOutput('replay_scatter', hover = 'hover_coord_scatter'))
                          
                          ))



server <- function(input, output, session) {
  
  ##for font page
  
  output$myImage <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.jpg')
    
    # Generate the PNG
    png(outfile, width = 400, height = 300)
    hist(rnorm(input$obs), main = "Generated in renderImage()")
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 600,
         height = 450,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  ## for ui_pred
  output$stats <- renderPlot({
    var <- paste(input$variableInput, collapse = " + ")
    varNames <- paste("LeagueLevel ~ ", var)
    
    # Splitting train and test datasets
    set.seed(20200315)
    n <- nrow(data1)
    train_index <- sample(n, size=round(.7*n))
    starcraft_train <- data1 %>% slice(train_index)
    starcraft_test <- data1 %>% slice(-train_index) 
    
    train_control_5 <- trainControl(
      method = "cv",
      number = 5)
    
    train_control_3 <- trainControl(
      method = "cv",
      number = 3)
    
    if (input$model == "DT") {
      if (length(input$variableInput) != 0){
        
        starcraft_cv_dt <- train(
          eval(parse(text = varNames)),
          data = starcraft_train,
          method = "rpart",           
          tuneGrid = data.frame(cp = seq(0.0, .05, by = 0.0025)),   
          trControl = train_control_5)
        
        confm_test_dt <- confusionMatrix(
          data = predict(starcraft_cv_dt, newdata=starcraft_test), 
          reference = starcraft_test$LeagueLevel, 
          positive = "low")
        
        acc <- round(confm_test_dt$overall[[1]], 4)
        other <- round(confm_test_dt$byClass[c(1,2,5)], 4)
        
        df <- data.frame(names = c("Accuracy", "Sensitivity", "Specificity", "Precision"),
                         values = c(acc, other[[1]], other[[2]], other[[3]]))
        
        ggplot(data=df, aes(x=names, y=values)) +
          geom_bar(stat="identity", fill="lightpink")+
          geom_text(aes(label=values), vjust=-0.3, size=4.5)+
          labs(title = "Performance Statistics", x = "", y = "") +
          theme_light()+
          theme(plot.title = element_text(size = 16, face = 4),
                panel.grid.major = element_line(color = "white", size = 0.25), 
                panel.grid.minor = element_blank(),
                axis.text.x = element_text(size = 13, face = "bold"))
      }
    }
    
    else if (input$model == "knn") {
      if (length(input$variableInput) != 0){
        starcraft_cv_knn <- train(
          eval(parse(text = varNames)),
          data = starcraft_train, 
          method = "knn",          
          preProc = c("center", "scale"),  
          tuneGrid = data.frame(k = seq(1,21, by = 2)),            
          trControl = train_control_5   
        )  
        
        confm_test_knn <- confusionMatrix(
          data = predict(starcraft_cv_knn, newdata=starcraft_test), 
          reference = starcraft_test$LeagueLevel, 
          positive = "low")
        
        acc <- round(confm_test_knn$overall[[1]], 4)
        other <- round(confm_test_knn$byClass[c(1,2,5)], 4)
        
        df <- data.frame(names = c("Accuracy", "Sensitivity", "Specificity", "Precision"),
                         values = c(acc, other[[1]], other[[2]], other[[3]]))
        
        par(mfcol = c(2,1))
        ggplot(data=df, aes(x=names, y=values)) +
          geom_bar(stat="identity", fill="lightblue")+
          geom_text(aes(label=values), vjust=-0.3, size=4.5)+
          labs(title = "Performance Statistics", x = "", y = "") +
          theme_light()+
          theme(plot.title = element_text(size = 16, face = 4),
                panel.grid.major = element_line(color = "white", size = 0.25), 
                panel.grid.minor = element_blank(),
                axis.text.x = element_text(size = 13, face = "bold"))
      }
    }
    
    else if (input$model == "RF") {
      if (length(input$variableInput) != 0){
        
        starcraft_cv_rf <- train(
          eval(parse(text = varNames)),
          data = starcraft_train, 
          method = "rf", 
          tuneGrid = data.frame(mtry = seq(1,5, by = 1)),  
          trControl = train_control_3    
        )
        
        confm_test_rf <- confusionMatrix(
          data = predict(starcraft_cv_rf, newdata=starcraft_test), 
          reference = starcraft_test$LeagueLevel, 
          positive = "low")
        
        acc <- round(confm_test_rf$overall[[1]], 4)
        other <- round(confm_test_rf$byClass[c(1,2,5)], 4)
        
        df <- data.frame(names = c("Accuracy", "Sensitivity", "Specificity", "Precision"),
                         values = c(acc, other[[1]], other[[2]], other[[3]]))
        
        ggplot(data=df, aes(x=names, y=values)) +
          geom_bar(stat="identity", fill="lightgreen")+
          geom_text(aes(label=values), vjust=-0.3, size=4.5)+
          labs(title = "Performance Statistics", x = "", y = "") +
          theme_light()+
          theme(plot.title = element_text(size = 16, face = 4),
                panel.grid.major = element_line(color = "white", size = 0.25), 
                panel.grid.minor = element_blank(),
                axis.text.x = element_text(size = 13, face = "bold"))
      }
    }
  })
  
  ##for ui_dt
  output$mytable <- DT::renderDataTable({
    data
  })
  
  
  ## for ui_age_time
  output$plot_age_time <- renderPlot({
    ggplot(filter(data, LeagueIndex == input$Llevel_time), aes_string(x = 'age_group', y = input$age_time_variable)) + 
      geom_boxplot(aes(fill = age_group)) + 
      theme(text = element_text(size=20), legend.position = 'none') +
      labs(x = 'Age Group') + 
      scale_color_discrete(guide = FALSE)
  })
  #for ui_age_action
  output$plot_age_action <- renderPlot({
    ggplot(filter(data, LeagueIndex == input$Llevel_action), aes_string(x = 'age_group', y = input$age_action_variable)) + 
      geom_boxplot(aes(fill = age_group)) + 
      theme(text = element_text(size=20), legend.position = 'none') +
      labs(x = 'Age Group') + 
      scale_color_discrete(guide = FALSE)
  })
  #for ui_age_hk 
  output$plot_age_hk <- renderPlot({
    ggplot(filter(data, LeagueIndex == input$Llevel_hk), aes_string(x = 'age_group', y = input$age_hk_variable)) + 
      geom_boxplot(aes(fill = age_group)) + 
      theme(text = element_text(size=20), legend.position = 'none') +
      labs(x = 'Age Group') + 
      scale_color_discrete(guide = FALSE)
  })
  
  #for ui_age_map
  output$plot_age_map <- renderPlot({
    ggplot(filter(data, LeagueIndex == input$Llevel_map), aes_string(x = 'age_group', y = input$age_map_variable)) + 
      geom_boxplot(aes(fill = age_group)) + 
      theme(text = element_text(size=20), legend.position = 'none') +
      labs(x = 'Age Group') + 
      scale_color_discrete(guide = FALSE)
  })
  
  #for ui_age_pac
  output$plot_age_pac <- renderPlot({
    ggplot(filter(data, LeagueIndex == input$Llevel_pac), aes_string(x = 'age_group', y = input$age_pac_variable)) + 
      geom_boxplot(aes(fill = age_group)) + 
      theme(text = element_text(size=20), legend.position = 'none') +
      labs(x = 'Age Group') + 
      scale_color_discrete(guide = FALSE)
  })
  #for ui_age_o
  output$plot_age_o <- renderPlot({
    ggplot(filter(data, LeagueIndex == input$Llevel_o), aes_string(x = 'age_group', y = input$age_o_variable)) + 
      geom_boxplot(aes(fill = age_group)) + 
      theme(text = element_text(size=20), legend.position = 'none') +
      labs(x = 'Age Group') + 
      scale_color_discrete(guide = FALSE)
  })
  
  
  
  ## for ui_gas_time
  output$plot_gas_time <- renderPlot({
    ggplot(data, aes_string(x= 'LeagueIndex', y = input$gas_time_variable)) + 
      geom_boxplot(fill = "#69b3a2") +
      labs(x = 'League Index', y = input$gas_time_variable) +
      theme(text = element_text(size=20), legend.position="none")
  })
  
  ## for ui_gas_action
  output$plot_gas_action <- renderPlot({
    ggplot(data, aes_string(x= 'LeagueIndex', y = input$gas_action_variable)) + 
      geom_boxplot(fill = "#69b3a2") +
      labs(x = 'League Index', y = input$gas_action_variable) +
      theme(text = element_text(size=20), legend.position="none")
  })
  
  ## for ui_gas_hk
  output$plot_gas_hk <- renderPlot({
    ggplot(data, aes_string(x= 'LeagueIndex', y = input$gas_hk_variable)) + 
      geom_boxplot(fill = "#69b3a2") +
      labs(x = 'League Index', y = input$gas_hk_variable) +
      theme(text = element_text(size=20), legend.position="none")
  })
  
  ## for ui_gas_map
  output$plot_gas_map <- renderPlot({
    ggplot(data, aes_string(x= 'LeagueIndex', y = input$gas_map_variable)) + 
      geom_boxplot(fill = "#69b3a2") +
      labs(x = 'League Index', y = input$gas_map_variable) +
      theme(text = element_text(size=20), legend.position="none")
  })
  
  ## for ui_gas_pac
  output$plot_gas_pac <- renderPlot({
    ggplot(data, aes_string(x= 'LeagueIndex', y = input$gas_pac_variable)) + 
      geom_boxplot(fill = "#69b3a2") +
      labs(x = 'League Index', y = input$gas_pac_variable) +
      theme(text = element_text(size=20), legend.position="none")
  })
  
  ## for ui_other_pac
  output$plot_gas_o <- renderPlot({
    ggplot(data, aes_string(x= 'LeagueIndex', y = input$gas_o_variable)) + 
      geom_boxplot(fill = "#69b3a2") +
      labs(x = 'League Index', y = input$gas_o_variable) +
      theme(text = element_text(size=20), legend.position="none")
  })
  
  
  
  #for ui_scatter
  
  output$replay_scatter <- renderPlot({
    ggplot(data, aes_string(x = input$x_scatter, y = input$y_scatter)) + 
      geom_point(aes(color = LeagueLevel), alpha = 0.75) + 
      theme(axis.text = element_text(size = 16), 
            axis.title = element_text(size = 16))
  })
  
  output$hover_data_scatter <- renderTable({
    nearPoints(data, input$hover_coord_scatter) %>% select('GameID', input$x_scatter, input$y_scatter)
  })
  
}




#navigation bar: combine all ui
ui <- navbarPage("Starcraft Analysis", theme = shinytheme("sandstone"),
                 tabPanel('Welcome', 
                          ui_home),
                 tabPanel('Data', 
                          ui_dt),
                 navbarMenu("Game Action Statistics by League",
                            tabPanel('Time Committment', ui_gas_time), 
                            tabPanel('Action Speed Metrics', ui_gas_action), 
                            tabPanel('Hotkeys Usage Metrics', ui_gas_hk),
                            tabPanel('Map Usage Metrics', ui_gas_map), 
                            tabPanel('Perception Action Cycle', ui_gas_pac),
                            tabPanel('Other Metrics', ui_gas_o)),
                 navbarMenu("Game Action Statistics by Age",
                            tabPanel('Time Committment', ui_age_time), 
                            tabPanel('Action Speed Metrics', ui_age_action), 
                            tabPanel('Hotkeys Usage Metrics', ui_age_hk),
                            tabPanel('Map Usage Metrics', ui_age_map), 
                            tabPanel('Perception Action Cycle', ui_age_pac),
                            tabPanel('Other Metrics', ui_age_o)),
                 tabPanel('Game Action Statistics Association', 
                          ui_scatter),
                 tabPanel("League Level Prediction",
                          ui_pred)
)

server <- server

shinyApp(ui, server)




