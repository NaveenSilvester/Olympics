Description of the data set
===========================

The data used in this app is pertaining to Summer Olympics from the year 1896 to 2016, a total of 28 Olympics Season. The data was compiled from various sources which include:
*	www.Wikipedia.org
*	www.Olympics.org
*	www.medalspercapita.com

The data is categorized into 7 different tables (text files) and 2 R markdown files
*		History
*		Country
*		CountriesOlympicHistory
*		CountryGSBMedalsHistory1
*		Countries2016Olympics
*		MedalVsSportEvent
*		CountryAthelete
* 		DataDescription
* 		Contact

Brief overview of the goals for the visualization:
--------------------------------------------------

The visualization in this App is categorized into the following section to glean certain specific hidden/trend or information in the data
### a. Olympics-Past2Present:

The objective of this visualization page/tab of the App is to understand the changes that Olympics has seen since the first Olympics 1896 in Athen (Greece) to 2016 Rio (Brazil). This section of the App tries to summarizes different Olympic seasons begining from the 1st Olympic dated back to 1896 to the latest in Rio 2016. 
### 	1. The Plot-1 (Bubble Chart): 
This graph is used to visualize data pertaining to 3 dimensions (NoOfAtheletes, Year and Events).
The Size of the bubble indicates the number of event that were carried out in the specific year (x-axis) and the number of participating athletes (y-axis)
#### Note: You could play the entire sequence of event from Athens to Rio using the Play Button provided below the slider
#### Insights: 
The plot indicates how the number of events, atheletes participation increased as you slide down to Rio Olympics
The bubble plot also indicates a gap in the olympics 1916 (The 1916 Summer Olympics, to be held in Berlin, capital of the German Empire, were cancelled due to World War I.); (1940 to 1944: The 12-year hiatus was because of World War II)
###		2. The choropleth map plot: 
This graph is used to visualize the number of countries who won atleast one medal in the olympics for the given year.
The country map on the choropleth gets highlighted in green shade for a given year based on the number of medals that were won by that country in the given year under study.
Darker the shade, more medals the country won in that Olympics year.
#### Note: You can see the changes in the color of the map as the Slider bar progresses from Athen to Rio time frame
#### Insights: 
The plot summarizes the increased trend observerd in the number of countries participating in Olympics. The increase was from 14 countries in Athens (Greece) to 207 countries in Rio (Brazil). This section clearly indicates how Olympics got popularity since the first Olympics.
### 	3. Stacked Bar Chart: 
This graph is used to visualize the number of Medals won and the distribution of Gold, Silver and Bronze won by a country.
Each color in the stacked bar indicates the type of medal  (Gold, Silver and Bronze)
#### Insights: 
The graph indicates how the Medals won by a country changes with passage of time (Athens to Rio); This also indicates how some country which did not get any medals in the early Olympics, started to surface as the Olympics progressed. We can also see how some countries try to retain top ranks in terms of winning medals.
###	b. Rio-Olympics-CountryView: 
The objective of this visualization page/tab of the App is to know the number of medals that were won by a given country and also know who were the athletes and which Sport/Event they participated to win respective medal for their country.
### 1. The Bar Chart: 
This chart dipicts the distribution of Medals that the selected country obtained in Rio Olympics Season	
#### Note: 
The user gets to see the Flag of the nation which is selected. The bar graphs in plot are clickable
#### Insights: 
These graphs indicates the distribution of Gold, Silver and Bronze medals
### 2. The Data Table:
The table is a coupled R shiny event with the Bar graph, this table would help to know the details of the Athelete, Sport and Events they participated to win the medal.
#### Insights: 
This page provides the information which is Country speicific and the data is limited only to Rio Olympics.
### c. Olympics-AthleteView:
The objective of this visualization page/tab of the App is to know about an Athlete of interest and the medals he/she won in 2016 Rio Olympics
#### Insights:
One could see the 2016 Olympic profile of an athlete and also know the Sport and Event for which the athlete won the medal.

	This App was build by "Naveen Silvester" Email: silvester.naveen@monsanto.com