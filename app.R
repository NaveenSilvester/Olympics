############################################################################################################
#### 									Author: Naveen Silvester										####
#### 									Date: 	 29-Sep-2016											####
#### 									Version: 01														####
#### 									Last Edited On: 29-Sep-2016										####
#### 									Contact Email: silvester.naveen@gmail.com					####
#### 																									####
#### 									Description: This app is to analyze Olympics data				####
####									Language: R (3.3.1)												####
############################################################################################################

############################################################################################################
#### The following libraries are required for the app to work											####
############################################################################################################
library(shiny)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(dplyr)
library(markdown)
library(RColorBrewer)
library(DT)
library(maps)
############################################################################################################
#### The below list of data are read from respective files and the same is used by the App				####
############################################################################################################
History <- read.table("History.txt", sep="\t", header=TRUE)
Country <- read.table("Country.txt", sep="\t", header=TRUE)
rdf <- read.table("CountriesOlympicHistory.txt", sep="\t", header=TRUE)
Bardf <- read.table("CountryGSBMedalsHistory1.txt", sep="\t", header=TRUE)
Countries2016Olympics <- read.table("Countries2016Olympics.txt", sep="\t", header=TRUE)
countrylist <- as.vector(Countries2016Olympics$Country)
# View(countrylist)
# View(Countries2016Olympics)
CodeAthletesSports <- Country[,2:3]
print ("Completed Loading")
MedalVsSportEvent <- read.csv("MedalVsSportEvent.csv", header=TRUE)
CountryAthelete2016 <- read.csv("CountryAthelete.csv", header=TRUE)
Country_Winners_List_2016 <- as.vector(unique(CountryAthelete2016[,1]))

############################################################################################################
#### This section of the code is for the Shiny User Interface											####
############################################################################################################
ui <- dashboardPage( skin= "purple",
  dashboardHeader(title = tags$a(href='https://www.olympic.org/',
						  tags$img(src='rings.png', height='48px', width='45px'),"Olympics")),
				 dashboardSidebar(     
									 sidebarMenu(
													
														# The side bar tab menu are provided in this section of the code
														menuItem("Olympics-Past2Present", tabName = "Olympics-GlobalView", icon = icon("globe")),
														menuItem("Rio-Olympics-CountryView", tabName = "Olympics-CountryView", icon = icon("tachometer")),
														menuItem("Rio-Olympics-AthleteView", tabName = "AthleteView", icon=icon("user")),
														menuItem("Dataset Description", tabName = "Guide", icon = icon("book")),
														menuItem("Contact", tabName = "Contact", icon = icon("envelope"))
														# html("Developer: Naveen Silvester @ Bangalore, India; Email: silvester.naveen@gmail.com
														# Date: 29-Sep-2016")
												)
								  ),
  dashboardBody(
				tabItems(
						  ############################################################################################################
                          # First tab Olympics-Past2Present 
						  ############################################################################################################
						  tabItem(tabName = "Olympics-GlobalView",
									fluidRow( align="center", 
											strong("To go on Animation View, Click the Play Button provided below the slider; the App will slide you through Athens to Rio Olympics."),br(),
											strong("For best experience, scroll down the page and also make it full page view by clicking the multiple horizontal bar on the header of the page."),
											sliderInput("GlobalViewSlider1", "", 1892, 2018, 1896, step=4, sep = "", animate = animationOptions(loop = FALSE, interval = 10000),width='100%')
									),
									fluidRow( align="center", 
												strong("Snapshot indicates the popularity Olympics gained through the time from the 1st Olympics in Athens to the latest Rio Olympics; the bubble chart indicates the increasing trend the participation of athletes and inclusion of new Sports events."),
												htmlOutput("GapInOlympics"),
												plotlyOutput("GlobalViewBubblePlot1", height=300)
									),
									fluidRow(
										column(4, align="center",
										br(), strong("Which are the countries who won Olympic medal for the selected year?"),
										textOutput("SelectedYear1"),
										plotlyOutput("GlobalViewMapPlot1", height=200)
										),
										column(8, align = "center",
										br(), strong("What is the proportion of Gold, Silver and Bronze Medals won by each country?"),
										textOutput("SelectedYear"),
										plotlyOutput("GlobalViewStackBarPlot1", height=200)
										)
									)												
								),
							############################################################################################################	
							# CountryView tab content
							############################################################################################################
							  tabItem(tabName = "Olympics-CountryView",
										fluidRow( 
													column(2, 
																 HTML('<p><img src="2016_Emblem.jpg", height="80px", width="200px" /></p>')
															),
													column(8, align="center",
															selectInput('CountryViewSelectedCountry', label = (strong('Select a Country to know the number of medals won by them')),
															choices = countrylist, multiple=FALSE, selected = 'UnitedStates')
														  ),
													column(2, align="right",
																htmlOutput("picture")
														)
												),
										fluidRow(	align="center",
														plotlyOutput("CountryViewBarPlot1"),
															br(), strong("Double click the bar graph to know the athletes and the sports/events in which they won the medal"), 
															br(),
															br(),
														dataTableOutput(outputId = "CountryView_selected_rows")
												)
									),
							############################################################################################################
							# AthleteView tab content
							############################################################################################################
							  tabItem(tabName = "AthleteView",
										fluidRow( align = "center",
														selectInput('AthleteViewSelectedCountry', label = (strong('Select a Country to know the number of medals won by them')),
															choices = Country_Winners_List_2016, multiple=FALSE, selected = 'UnitedStates')
															
												),
										fluidRow( align = "center",
															uiOutput("AthleteView_AthleteNames"),
															htmlOutput("Sport_picture"),
															textOutput("AthleteView_AthleteScore")
												),
												fluidRow( align = "center",
															dataTableOutput("AthleteView_AthleteGoldDetails")													
												)
									),
							############################################################################################################
							# UserGuide tab content
							############################################################################################################
							  tabItem(tabName = "Guide",
										fluidRow(
													includeMarkdown("DataDescription.md")
												)
									),
							############################################################################################################
							# Contact tab content
							############################################################################################################
							  tabItem(tabName = "Contact",
										fluidRow(
													includeMarkdown("Contact.md")
												)
									)
									
			)
	)
)
############################################################################################################
#### This section of the code contains the server functions												####
############################################################################################################
server <- function(input, output) 
{ 

			###################################################################################################################
			#### This section of the code is used to display the SportIcon based on user selection in the AthleteView tab
			###################################################################################################################

			output$Sport_picture<-renderText({
							SelectedCountry <- input$AthleteViewSelectedCountry
							NameOfAthlete <- input$AthleteViewSelectedAthelete
							
							GoldMedals <- subset(MedalVsSportEvent , NameOfGoldMedalWinner == NameOfAthlete, select = c("Sport","Event", "EventType","NameOfGoldMedalWinner"))
							NoOfGoldMedals <- nrow(GoldMedals)
							GoldTag <- replicate(NoOfGoldMedals, "Gold")
							GoldMedals$Medal <- GoldTag
							colnames(GoldMedals) <- c("Sport", "Event","Event Type", "NameOfAthlete","Medal")

							SilverMedals <- subset(MedalVsSportEvent , NameOfSilverMedalWinner == NameOfAthlete, select = c("Sport","Event", "EventType","NameOfSilverMedalWinner"))
							NoOfSilverMedals <- nrow(SilverMedals)
							SilverTag <- replicate(NoOfSilverMedals, "Silver")
							SilverMedals$Medal <- SilverTag
							colnames(SilverMedals) <- c("Sport", "Event","Event Type", "NameOfAthlete","Medal")

							BronzeMedals <- subset(MedalVsSportEvent , NameOfBronzeMedalWinner == NameOfAthlete, select = c("Sport","Event", "EventType","NameOfBronzeMedalWinner"))
							NoOfBronzeMedals <- nrow(BronzeMedals)
							BronzeTag <- replicate(NoOfBronzeMedals, "Bronze")
							BronzeMedals$Medal <- BronzeTag
							colnames(BronzeMedals) <- c("Sport", "Event","Event Type", "NameOfAthlete","Medal")

							if(nrow(GoldMedals) > 0 && nrow(SilverMedals) > 0 && nrow(BronzeMedals) > 0)
							{
								MedalTable <- rbind(GoldMedals,SilverMedals,BronzeMedals)
								MedalWonInSport <- MedalTable[1,1]
								SportImage <- paste(MedalWonInSport,".png", sep="")
								src = SportImage
								c('<img src="',src,'", height="48px", width="80px">')
							}
							else if (nrow(GoldMedals) > 0 && nrow(SilverMedals) > 0 && nrow(BronzeMedals) < 1)
							{
								MedalTable <- rbind(GoldMedals,SilverMedals)
								MedalWonInSport <- MedalTable[1,1]
								SportImage <- paste(MedalWonInSport,".png", sep="")
								src = SportImage
								c('<img src="',src,'", height="48px", width="80px">')								
							}
							else if (nrow(GoldMedals) > 0 && nrow(SilverMedals) < 1 && nrow(BronzeMedals) > 0)
							{
								MedalTable <- rbind(GoldMedals,BronzeMedals)
								MedalWonInSport <- MedalTable[1,1]
								SportImage <- paste(MedalWonInSport,".png", sep="")
								src = SportImage
								c('<img src="',src,'", height="48px", width="80px">')								
							}
							else if (nrow(GoldMedals) < 1 && nrow(SilverMedals) > 0 && nrow(BronzeMedals) > 0)
							{
								MedalTable <- rbind(SilverMedals,BronzeMedals)
								MedalWonInSport <- MedalTable[1,1]
								SportImage <- paste(MedalWonInSport,".png", sep="")
								src = SportImage
								c('<img src="',src,'", height="48px", width="80px">')								
							}
							else if (nrow(GoldMedals) > 0 && nrow(SilverMedals) < 1 && nrow(BronzeMedals) < 1)
							{
								MedalTable <- GoldMedals
								MedalWonInSport <- MedalTable[1,1]
								SportImage <- paste(MedalWonInSport,".png", sep="")
								src = SportImage
								c('<img src="',src,'", height="48px", width="80px">')								
							}
							else if (nrow(GoldMedals) < 1 && nrow(SilverMedals) > 0 && nrow(BronzeMedals) < 1)
							{
								MedalTable <- SilverMedals
								MedalWonInSport <- MedalTable[1,1]
								SportImage <- paste(MedalWonInSport,".png", sep="")
								src = SportImage
								c('<img src="',src,'", height="48px", width="80px">')								
							}
							else if (nrow(GoldMedals) < 1 && nrow(SilverMedals) < 1 && nrow(BronzeMedals) > 0)
							{
								MedalTable <- BronzeMedals
								MedalWonInSport <- MedalTable[1,1]
								SportImage <- paste(MedalWonInSport,".png", sep="")
								src = SportImage
								c('<img src="',src,'", height="48px", width="80px">')								
							}			
			
				  })
			############################################################################################################
			#### This function return the PullDown Menu
			############################################################################################################

			output$AthleteView_AthleteNames <- renderUI({
					SelectedCountry <- input$AthleteViewSelectedCountry
					NamesOfAtheleteForSelectedCountry <- as.data.frame(subset(CountryAthelete2016, Country == SelectedCountry, select = c("Athelete")))
					Athletes <- as.vector(unique(NamesOfAtheleteForSelectedCountry[,1]))
					selectInput('AthleteViewSelectedAthelete', label = (strong('Select an Athlete to know the number of medals won by them')),
					choices = Athletes, multiple=FALSE)					
			})

			############################################################################################################
			#### This section of code is a function with returns the Athelete profile as text output
			############################################################################################################
			output$AthleteView_AthleteScore <- renderText ({
							SelectedCountry <- input$AthleteViewSelectedCountry
							NameOfAthlete <- input$AthleteViewSelectedAthelete
							
							GoldMedals <- subset(MedalVsSportEvent , NameOfGoldMedalWinner == NameOfAthlete, select = c("Sport","Event", "EventType","NameOfGoldMedalWinner"))
							NoOfGoldMedals <- nrow(GoldMedals)
							GoldTag <- replicate(NoOfGoldMedals, "Gold")
							GoldMedals$Medal <- GoldTag
							colnames(GoldMedals) <- c("Sport", "Event","Event Type", "NameOfAthlete","Medal")

							SilverMedals <- subset(MedalVsSportEvent , NameOfSilverMedalWinner == NameOfAthlete, select = c("Sport","Event", "EventType","NameOfSilverMedalWinner"))
							NoOfSilverMedals <- nrow(SilverMedals)
							SilverTag <- replicate(NoOfSilverMedals, "Silver")
							SilverMedals$Medal <- SilverTag
							colnames(SilverMedals) <- c("Sport", "Event","Event Type", "NameOfAthlete","Medal")

							BronzeMedals <- subset(MedalVsSportEvent , NameOfBronzeMedalWinner == NameOfAthlete, select = c("Sport","Event", "EventType","NameOfBronzeMedalWinner"))
							NoOfBronzeMedals <- nrow(BronzeMedals)
							BronzeTag <- replicate(NoOfBronzeMedals, "Bronze")
							BronzeMedals$Medal <- BronzeTag
							colnames(BronzeMedals) <- c("Sport", "Event","Event Type", "NameOfAthlete","Medal")

							if(nrow(GoldMedals) > 0 && nrow(SilverMedals) > 0 && nrow(BronzeMedals) > 0)
							{
								MedalTable <- rbind(GoldMedals,SilverMedals,BronzeMedals)
								TotalMedals <- nrow(MedalTable)
								Profile_Message <- paste(NameOfAthlete , " from ", SelectedCountry, " has won a total of ", TotalMedals, " in 2016 Rio Olympics by participating in the following events ", TotalEvents, ".", "The medal statistics includes ", nrow(GoldMedals), "Gold ", nrow(SilverMedals), " Silver and ", nrow(BronzeMedals), " Bronze medal. The details are provided in the table below.")
							}
							else if (nrow(GoldMedals) > 0 && nrow(SilverMedals) > 0 && nrow(BronzeMedals) < 1)
							{
								MedalTable <- rbind(GoldMedals,SilverMedals)
								TotalMedals <- nrow(MedalTable)
								Profile_Message <- paste(NameOfAthlete , " from ", SelectedCountry, " has won a total of ", TotalMedals, " medals in 2016 Rio Olympics.", "The medal statistics includes ", nrow(GoldMedals), " Gold and ", nrow(SilverMedals), " Silver. The details are provided in the table below.")
							}
							else if (nrow(GoldMedals) > 0 && nrow(SilverMedals) < 1 && nrow(BronzeMedals) > 0)
							{
								MedalTable <- rbind(GoldMedals,BronzeMedals)
								TotalMedals <- nrow(MedalTable)
								Profile_Message <- paste(NameOfAthlete , " from ", SelectedCountry, " has won a total of ", TotalMedals, " medals in 2016 Rio Olympics.", "The medal statistics includes ", nrow(GoldMedals), " Gold and ", nrow(BronzeMedals), " Bronze. The details are provided in the table below.")
							}
							else if (nrow(GoldMedals) < 1 && nrow(SilverMedals) > 0 && nrow(BronzeMedals) > 0)
							{
								MedalTable <- rbind(SilverMedals,BronzeMedals)
								TotalMedals <- nrow(MedalTable)
								Profile_Message <- paste(NameOfAthlete , " from ", SelectedCountry, " has won a total of ", TotalMedals, " medals in 2016 Rio Olympics.", "The medal statistics includes ", nrow(SilverMedals), " Silver and ", nrow(BronzeMedals), " Bronze. The details are provided in the table below.")
							}
							else if (nrow(GoldMedals) > 0 && nrow(SilverMedals) < 1 && nrow(BronzeMedals) < 1)
							{
								MedalTable <- GoldMedals
								TotalMedals <- nrow(MedalTable)
								if (TotalMedals > 1)
								{
									Profile_Message <- paste(NameOfAthlete , " from ", SelectedCountry, " has won a total of ", TotalMedals, " Gold medals in 2016 Rio Olympics. The details are provided in the table below.")
								}
								else
								{
									Profile_Message <- paste(NameOfAthlete , " from ", SelectedCountry, " has won a Gold medal in 2016 Rio Olympics. The details are provided in the table below.")								
								}
							}
							else if (nrow(GoldMedals) < 1 && nrow(SilverMedals) > 0 && nrow(BronzeMedals) < 1)
							{
								MedalTable <- SilverMedals
								TotalMedals <- nrow(MedalTable)
								if (TotalMedals > 1)
								{
									Profile_Message <- paste(NameOfAthlete , " from ", SelectedCountry, " has won a total of ", TotalMedals, " Silver medals in 2016 Rio Olympics. The details are provided in the table below.")
								}
								else
								{
									Profile_Message <- paste(NameOfAthlete , " from ", SelectedCountry, " has won a Silver medal in 2016 Rio Olympics. The details are provided in the table below.")								
								}							
							}
							else if (nrow(GoldMedals) < 1 && nrow(SilverMedals) < 1 && nrow(BronzeMedals) > 0)
							{
								MedalTable <- BronzeMedals
								TotalMedals <- nrow(MedalTable)
								if (TotalMedals > 1)
								{
									Profile_Message <- paste(NameOfAthlete , " from ", SelectedCountry, " has won a total of ", TotalMedals, " Bronze medals in 2016 Rio Olympics. The details are provided in the table below.")
								}
								else
								{
									Profile_Message <- paste(NameOfAthlete , " from ", SelectedCountry, " has won a Bronze medal in 2016 Rio Olympics. The details are provided in the table below.")								
								}							
							}
			})
			
			############################################################################################################
			#### This section of the code returns a table object which is printed as a data table in the AthleteView
			############################################################################################################
			output$AthleteView_AthleteGoldDetails <- renderDataTable ({
							NameOfAthlete <- input$AthleteViewSelectedAthelete
							GoldMedals <- subset(MedalVsSportEvent , NameOfGoldMedalWinner == NameOfAthlete, select = c("Sport","Event", "EventType","NameOfGoldMedalWinner"))
							NoOfGoldMedals <- nrow(GoldMedals)
							GoldTag <- replicate(NoOfGoldMedals, "Gold")
							GoldMedals$Medal <- GoldTag
							colnames(GoldMedals) <- c("Sport", "Event","Event Type", "NameOfAthlete","Medal")

							SilverMedals <- subset(MedalVsSportEvent , NameOfSilverMedalWinner == NameOfAthlete, select = c("Sport","Event", "EventType","NameOfSilverMedalWinner"))
							NoOfSilverMedals <- nrow(SilverMedals)
							SilverTag <- replicate(NoOfSilverMedals, "Silver")
							SilverMedals$Medal <- SilverTag
							colnames(SilverMedals) <- c("Sport", "Event","Event Type", "NameOfAthlete","Medal")

							BronzeMedals <- subset(MedalVsSportEvent , NameOfBronzeMedalWinner == NameOfAthlete, select = c("Sport","Event", "EventType","NameOfBronzeMedalWinner"))
							NoOfBronzeMedals <- nrow(BronzeMedals)
							BronzeTag <- replicate(NoOfBronzeMedals, "Bronze")
							BronzeMedals$Medal <- BronzeTag
							colnames(BronzeMedals) <- c("Sport", "Event","Event Type", "NameOfAthlete","Medal")

							if(nrow(GoldMedals) > 0 && nrow(SilverMedals) > 0 && nrow(BronzeMedals) > 0)
							{
								MedalTable <- rbind(GoldMedals,SilverMedals,BronzeMedals)
								DT::datatable(MedalTable, options = list(paging = FALSE))
								DT::datatable(MedalTable, options = list(searching = FALSE))
							}
							else if (nrow(GoldMedals) > 0 && nrow(SilverMedals) > 0 && nrow(BronzeMedals) < 1)
							{
								MedalTable <- rbind(GoldMedals,SilverMedals)
								DT::datatable(MedalTable, options = list(paging = FALSE))
								DT::datatable(MedalTable, options = list(searching = FALSE))								
							}
							else if (nrow(GoldMedals) > 0 && nrow(SilverMedals) < 1 && nrow(BronzeMedals) > 0)
							{
								MedalTable <- rbind(GoldMedals,BronzeMedals)
								DT::datatable(MedalTable, options = list(paging = FALSE))
								DT::datatable(MedalTable, options = list(searching = FALSE))								
							}
							else if (nrow(GoldMedals) < 1 && nrow(SilverMedals) > 0 && nrow(BronzeMedals) > 0)
							{
								MedalTable <- rbind(SilverMedals,BronzeMedals)
								DT::datatable(MedalTable, options = list(paging = FALSE))
								DT::datatable(MedalTable, options = list(searching = FALSE))								
							}
							else if (nrow(GoldMedals) > 0 && nrow(SilverMedals) < 1 && nrow(BronzeMedals) < 1)
							{
								MedalTable <- GoldMedals
								DT::datatable(MedalTable, options = list(paging = FALSE))
								DT::datatable(MedalTable, options = list(searching = FALSE))								
							}
							else if (nrow(GoldMedals) < 1 && nrow(SilverMedals) > 0 && nrow(BronzeMedals) < 1)
							{
								MedalTable <- SilverMedals
								DT::datatable(MedalTable, options = list(paging = FALSE))
								DT::datatable(MedalTable, options = list(searching = FALSE))								
							}
							else if (nrow(GoldMedals) < 1 && nrow(SilverMedals) < 1 && nrow(BronzeMedals) > 0)
							{
								MedalTable <- BronzeMedals
								DT::datatable(MedalTable, options = list(paging = FALSE))
								DT::datatable(MedalTable, options = list(searching = FALSE))								
							}
			})

			###################################################################################################################
			#### This section of the code is used to display the Country flag based on user selection in the CountryView tab
			###################################################################################################################
			output$picture<-renderText({
											CountryFilter <-input$CountryViewSelectedCountry
											CountryCode <- as.data.frame(subset(Countries2016Olympics, Country==CountryFilter, select=c("Code")))
											FlagImage <- paste(CountryCode[1,1],"_Flag.png", sep="")
											print("The flag image")
											print(FlagImage)
											src = FlagImage
											c('<img src="',src,'", height="48px", width="80px">')
								  })
			###################################################################################################################
			#### This section of the code returns an BarPlot object which is printed as clickable bar plot in CountryViewTab 
			###################################################################################################################
			output$CountryViewBarPlot1 <- renderPlotly ({			
											CountryFilter <-input$CountryViewSelectedCountry
											CountryCode <- subset(Countries2016Olympics, Country==CountryFilter, select=c("Code"))
											FlagImage <- paste(CountryCode,"_Flag.png")
											
											tempCountryViewBarPlot1 <- as.data.frame(t(subset(Countries2016Olympics, Country==CountryFilter, select=c("Gold", "Silver", "Bronze"))))
											rownames(tempCountryViewBarPlot1) <- c()
											Medals <- c("Gold","Silver","Bronze")
											DataForMedalPlot <- cbind(Medals, tempCountryViewBarPlot1)
											colnames(DataForMedalPlot) <- c("Medal Category","No Of Medals")
											# View(DataForMedalPlot)
											MedalCategory <- DataForMedalPlot[,1]
											NoOfMedals <- DataForMedalPlot[,2]
											minimumMedals <- length(NoOfMedals[NoOfMedals > 10])
											noMedalsFlag <- sum(NoOfMedals[NoOfMedals > 0])
											myTitleCountryViewBarPlot1 <- paste("Distribution of Medals won by", CountryFilter, "in 2016 Rio Olympics") 
											if (minimumMedals > 0 )
											{
														a <- list(
														autotick = FALSE,
														ticks = "outside",
														tick0 = 0,
														dtick = 5,
														showline = TRUE,
														ticklen = 5,
														tickwidth = 2,
														showline = FALSE,
														tickcolor = toRGB("blue")
														)
													plot_ly(DataForMedalPlot, x = MedalCategory, y = NoOfMedals, type = "bar", source = "SelectedCountry", marker = list(color = c('#FFD700','#C0C0C0',' #cd7f32')), showlegend = FALSE) %>% 
															layout(font=2,title = myTitleCountryViewBarPlot1, yaxis = a)
															layout(plot_bgcolor='rgb(222, 228, 237)')
															layout(showlegend = FALSE)
															layout(paper_bgcolor='transparent')
											}
											else
											{
												
												if (noMedalsFlag < 1)
												{
													myTitleCountryViewBarPlot1 <- paste(CountryFilter , "did not win any medal in 2016 Rio Olympics") 
														a <- list(
														autotick = FALSE,
														ticks = "outside",
														tick0 = 0,
														dtick = 1,
														outlinecolor = "#ffff00",
														showline = TRUE,
														ticklen = 5,
														tickwidth = 2,
														showline = FALSE,
														hoverinfo = "x+y",
														tickcolor = toRGB("blue")
														)											
													plot_ly(DataForMedalPlot, x = MedalCategory, y = NoOfMedals, type = "bar", source = "SelectedCountry", marker = list(color = c('#FFD700','#C0C0C0',' #cd7f32')), showlegend = FALSE) %>% 
													 layout(font=2,title = myTitleCountryViewBarPlot1, yaxis = a)
													 layout(plot_bgcolor='rgb(222, 228, 237)')
													 layout(showlegend = FALSE)
													 layout(paper_bgcolor='transparent')												
												}
												else
												{
														a <- list(
														autotick = FALSE,
														ticks = "outside",
														tick0 = 0,
														dtick = 1,
														outlinecolor = "#ffff00",
														showline = TRUE,
														ticklen = 5,
														tickwidth = 2,
														showline = FALSE,
														hoverinfo = "x+y",
														tickcolor = toRGB("blue")
														)											
													plot_ly(DataForMedalPlot, x = MedalCategory, y = NoOfMedals, type = "bar", source = "SelectedCountry", marker = list(color = c('#FFD700','#C0C0C0',' #cd7f32')), showlegend = FALSE) %>% 
													 layout(font=2,title = myTitleCountryViewBarPlot1, yaxis = a)
													 layout(plot_bgcolor='rgb(222, 228, 237)')
													 layout(showlegend = FALSE)
													 layout(paper_bgcolor='transparent')
												}
														
											}
			})

			############################################################################################################			
			#### This section of the code returns a table object which is later printed as a table in CountryView Tab
			#### The table printed takes in the argument from the CountryViewBarPlot1
			############################################################################################################
			output$CountryView_selected_rows <- renderDataTable ({
											CountryFilter <-input$CountryViewSelectedCountry
											event.data <- event_data("plotly_click", source = "SelectedCountry")
											    # If NULL dont do anything
											if(is.null(event.data) == T) return(NULL)
											print("Here is the selected block of hte graph")
											print(event.data[3])
										    if(event.data[3] == "Gold")
											{
												myselectdata <- subset(MedalVsSportEvent, Gold == CountryFilter, select=c("Sport","Event","Gold","NameOfGoldMedalWinner") )
												colnames(myselectdata) <- c("Sport","Event","Country","NameOfGoldMedalWinner")
												myselectdata
											}
										    else if(event.data[3] == "Silver")
											{
												myselectdata <- subset(MedalVsSportEvent, Silver == CountryFilter, select=c("Sport","Event","Silver","NameOfSilverMedalWinner"))
												colnames(myselectdata) <- c("Sport","Event","Country","NameOfSilverMedalWinner")
												myselectdata
											}
										    else if(event.data[3] == "Bronze")
											{
												myselectdata <- subset(MedalVsSportEvent,Bronze == CountryFilter, select=c("Sport","Event","Bronze","NameOfBronzeMedalWinner"))
												colnames(myselectdata) <- c("Sport","Event","Country","NameOfBronzeMedalWinner")
												myselectdata
											}
					})


	output$GapInOlympics <- renderText({ 
								if(is.null(input$GlobalViewSlider1) == T) return(NULL)
									OlympicYear <- input$GlobalViewSlider1			
								if(OlympicYear == 1916)
								{
									GapMessage <- "World War I led to the cancellation of Olympics in the year 1916"
									paste('<font color=\"#FF0000\"><b>World War I led to the cancellation of Olympics in the year 1916</b></font>') 
								}
								else if (OlympicYear == 1940)
								{
									GapMessage <- "World War II led to the cancellation of Olympics in the year 1940 and 1944"
									paste('<font color=\"#FF0000\"><b>World War II led to the cancellation of Olympics in the year 1940 and 1944</b></font>') 
								}
								else if (OlympicYear == 1944)
								{
									GapMessage <- "World War II led to the cancellation of Olympics in the year 1940 and 1944"
									paste('<font color=\"#FF0000\"><b>World War II led to the cancellation of Olympics in the year 1940 and 1944</b></font>') 
								}
							   })
					
	############################################################################################################							
    #### This section of the code returns a plot object which is a BubblePlot, the Bubble Plot takes arguments 
	#### from the Slide view named GlobalViewSlider1
	############################################################################################################
	output$GlobalViewBubblePlot1 <- renderPlotly({

									
									if(is.null(input$GlobalViewSlider1) == T) return(NULL)
									 print(" I HAVE ENTERED ")
									OlympicYear <- input$GlobalViewSlider1										
										print (OlympicYear)
										DataBubblePlot1 <- as.data.frame(subset(History, Year <= OlympicYear))
										plot_ly (DataBubblePlot1, x = Year, y = Athletes, text = paste("No of Events: ", Events), 
										# mode = "markers", color = Events, size = (Events/100), opacity = Events, showlegend = FALSE)
										mode = "markers", color = Events, size = Events, opacity = Events, showlegend = FALSE) %>% 
									layout(plot_bgcolor='rgb(222, 228, 237)')
									layout(showlegend = FALSE)
								layout(paper_bgcolor='transparent')
		
							  })
	############################################################################################################			
	##### This section of the code uses the arguments from the GlobalViewMapPlot1 in the Past2Present tab and
	##### and then highlights the choroplet maps accordingly
	############################################################################################################			
    output$GlobalViewMapPlot1 <- renderPlotly({
												SelectedYear <- input$GlobalViewSlider1
												df <- subset(rdf, Year == SelectedYear)
												
												# light grey boundaries
												l <- list(color = toRGB("grey"), width = 0.1)

												# specify map projection/options
												g <- list(
												  showframe = FALSE,
												  showcoastlines = FALSE,
												  projection = list(type = 'Mercator')
												)
												countrytitle <- paste("Year: ", SelectedYear)
												plot_ly(df, z = Medals, text = COUNTRY, locations = CODE, type = 'choropleth',
														color = Medals, colors = 'Greens', marker = list(line = l)
														) %>%
												layout(plot_bgcolor='rgb(222, 228, 237)')
												layout(paper_bgcolor='transparent')
							  })
	
	############################################################################################################										  
	#### This section of the code prints the Year selected by the GlobalViewSlider1 in the Past2Present tab at
	#### the side just above the Choroplet map
	############################################################################################################			
	output$SelectedYear <- renderText({
										YearOnSlider <- input$GlobalViewSlider1
										paste("Year :", YearOnSlider)
									})
	############################################################################################################										  
	#### This section of the code prints the Year selected by the GlobalViewSlider1 in the Past2Present tab at
	#### the side just above the Barchart
	############################################################################################################											
	output$SelectedYear1 <- renderText({
										YearOnSlider <- input$GlobalViewSlider1
										paste("Year :", YearOnSlider)
									})	
	############################################################################################################										  
	#### This section of the code returns a StackBar Plot object in the Past2Present tab, the tab takes arguments
	#### from the GlobalViewSlider1
	############################################################################################################												
    output$GlobalViewStackBarPlot1 <- renderPlotly({
												SelectedYear <- input$GlobalViewSlider1
												StackTitle <- paste("Year: ", SelectedYear)
												abdf <- subset(Bardf, Year == SelectedYear)
												bdf <- abdf[with(abdf, order(Country)), ]
												Country <- bdf[,2] #Countries Participated
												Medals <- bdf[,4] #Gold Medals
												Silver <- bdf[,5]
												Bronze <- bdf[,6]
												
												ax <- list(
															  title = "",
															  zeroline = FALSE,
															  showline = TRUE,
															  showticklabels = TRUE,
															  showgrid = FALSE
															)
												ay <- list(			
															  title = "",
															  zeroline = FALSE,
															  showline = TRUE,
															  showticklabels = TRUE,
															  showgrid = TRUE
															)
												
									GoldLayer <- plot_ly(
															x = Country,
															y = Medals,
															name = "Gold Medals",
															type = "bar"
														) %>%
												layout(plot_bgcolor='rgb(222, 228, 237)') %>%
												layout(paper_bgcolor='transparent')

									GoldLayer
									SilverLayer <- add_trace(
													GoldLayer,
													  x = Country,
													  y = Silver,
													  name = "Silver Medals",
													  type = "bar"
													 )
													SilverLayer

									BronzeLayer <- add_trace(
													SilverLayer,
													  x = Country,
													  y = Bronze,
													  name = "Bronze Medals",
													  type = "bar"
													 )
													BronzeLayer													

										 layout(Bronze, barmode = "stack") 
										 layout(xaxis = ax, yaxis = ay)
										 												
										FinalLayer <- layout(Bronze, barmode = "stack") 
										FinalLayer
							  })	
}
shinyApp(ui, server)