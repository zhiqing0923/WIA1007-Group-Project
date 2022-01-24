library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(shinythemes)
library(dplyr)
library(ggplot2)

mydata <- read.csv("sample data.csv", encoding = "latin1")
migrate <- read.csv("migration.csv", encoding = "latin1")
universities <- read.csv("university.csv", encoding = "latin1")

flagIcon <- makeIcon(
  iconUrl = case_when(
    mydata$Country == "China" ~ "country_flag/China.png",
    mydata$Country == "India" ~ "country_flag/India.png",
    mydata$Country == "United States" ~ "country_flag/United State.png",
    mydata$Country == "Indonesia" ~ "country_flag/Indonesia.png",
    mydata$Country == "Pakistan" ~ "country_flag/Pakistan.png",
    mydata$Country == "Brazil" ~ "country_flag/Brazil.png",
    mydata$Country == "Nigeria" ~ "country_flag/Nigeria.png",
    mydata$Country == "Bangladesh" ~ "country_flag/Bangladesh.png",
    mydata$Country == "Russia" ~ "country_flag/Russia.png",
    mydata$Country == "Mexico" ~ "country_flag/Mexico.png",
    mydata$Country == "Japan" ~ "country_flag/Japan.png",
    mydata$Country == "Ethiopia" ~ "country_flag/Ethiopia.png",
    mydata$Country == "Philippines" ~ "country_flag/Philippines.png",
    mydata$Country == "Egypt" ~ "country_flag/Egypt.png",
    mydata$Country == "Vietnam" ~ "country_flag/Vietnam.png",
    mydata$Country == "Turkey" ~ "country_flag/Turkey.png",
    mydata$Country == "Iran" ~ "country_flag/Iran.png",
    mydata$Country == "Germany" ~ "country_flag/Germany.png",
    mydata$Country == "Thailand" ~ "country_flag/Thailand.png",
    mydata$Country == "United Kingdom" ~ "country_flag/United Kingdom.png",
    mydata$Country == "France" ~ "country_flag/France.png",
    mydata$Country == "Italy" ~ "country_flag/Italy.png",
    mydata$Country == "South Africa" ~ "country_flag/South Africa.png",
    mydata$Country == "Myanmar" ~ "country_flag/Myanmar.png",
    mydata$Country == "Kenya" ~ "country_flag/Kenya.png",
    mydata$Country == "South Korea" ~ "country_flag/South Korea.png",
    mydata$Country == "Colombia" ~ "country_flag/Colombia.png",
    mydata$Country == "Spain" ~ "country_flag/Spain.png",
    mydata$Country == "Argentina" ~ "country_flag/Argentina.png",
    mydata$Country == "Sudan" ~ "country_flag/Sudan.png",
    mydata$Country == "Ukraine" ~ "country_flag/Ukraine.png",
    mydata$Country == "Iraq" ~ "country_flag/Iraq.png",
    mydata$Country == "Afghanistan" ~ "country_flag/Afghanistan.png",
    mydata$Country == "Poland" ~ "country_flag/Poland.png",
    mydata$Country == "Canada" ~ "country_flag/Canada.png",
    mydata$Country == "Saudi Arabia" ~ "country_flag/Saudi Arabia.png",
    mydata$Country == "Uzbekistan" ~ "country_flag/Uzbekistan.png",
    mydata$Country == "Peru" ~ "country_flag/Peru.png",
    mydata$Country == "Malaysia" ~ "country_flag/Malaysia.png",
    mydata$Country == "Nepal" ~ "country_flag/Nepal.png",
    mydata$Country == "Madagascar" ~ "country_flag/Madagascar.png",
    mydata$Country == "North Korea" ~ "country_flag/North Korea.png",
    mydata$Country == "Australia" ~ "country_flag/Australia.png",
    mydata$Country == "Taiwan" ~ "country_flag/Taiwan.png",
    mydata$Country == "Sri Lanka" ~ "country_flag/Sri Lanka.png",
    mydata$Country == "Romania" ~ "country_flag/Romania.png",
    mydata$Country == "Malawi" ~ "country_flag/Malawi.png",
    mydata$Country == "Kazakhstan" ~ "country_flag/Kazakhstan.png",
    mydata$Country == "Zambia" ~ "country_flag/Zambia.png",
    mydata$Country == "Syria" ~ "country_flag/Syria.png"
  ),
  iconWidth = 25, iconHeight = 25,
  shadowWidth = 10, shadowHeight = 10
)
ui <- bootstrapPage(theme = shinytheme("sandstone"),
                    navbarPage("Countries Explorer and Recommender",
                               tabPanel("User Guide",
                                        h3("Our App:"),
                                        br(),
                                        h4("Purpose of Visit"),
                                        p("This tab allows you to choose from 3 choices, whether you plan to visit this country for a holiday, education purposes or to migrate there."),
                                        p("Then , choose a country of your interest and it will display image or informations based on what you chose for the purpose of visit."),
                                        br(),
                                        h4("MAP"),
                                        p("This tab shows a world map with icons of the countries, each icon you clicked will provide you basic informations of that particular country such as population, capital city, language and currency."),
                                        p("For more information of that country, you may click the link below."),
                                        br(),
                                        p("The sources of datasets that we used in this app: "),
                                        p(tags$a(href="https://www.un.org/en/development/desa/population/migration/data/estimates2/estimates19.asp","United Nations Population Division Department of Economic and Social Affairs")),
                                        p(tags$a(href="https://www.kaggle.com/tanuprabhu/population-by-country-2020", "Population by Country")),
                                        p(tags$a(href = "https://www.kaggle.com/joeshamen/world-university-rankings-2020", "University Rankings"))
                                        
                               ),
                               tabPanel("Purpose of Visit",
                                        sidebarPanel(width = 3, selectInput("purposeOfVisit",
                                                                            label = "Purpose of visit" ,
                                                                            choices = c("Holiday", "Migrate","Study Abroad")
                                                                            
                                        ),
                                        
                                        pickerInput("selectCountries", label = "Select a Country:",
                                                    choices = list(
                                                      `Southeast Asia` = c("Indonesia", "Malaysia", "Thailand", "Philippines", "Myanmar","Vietnam","India", "Bangladesh"),
                                                      `Southern Asia` = c("Pakistan",""),
                                                      `Western Asia` = c("Iran", "Iraq", "Saudi Arabia", "Spain","Turkey"),
                                                      `Central Asia` = c("Afghanistan", "Kazakhstan", "Uzbekistan"),
                                                      `East Asia` = c("China", "Japan", "North Korea", "South Korea", "Taiwan"),
                                                      `South America` = c("Brazil", "Colombia", "Argentina", "Peru"),
                                                      `North America` = c("United States", "Mexico", "Canada"),
                                                      `Europe` = c("Turkey", "Germany", "United Kingdom","France","Italy", "Spain","Ukraine","Poland", "Romania","Kazakhstan"),
                                                      `Africa` = c("Nigeria", "Ethiopia", "Egypt", "South Africa", "Kenya","Sudan","Madagascar","Malawi","Zambia"),
                                                      `Oceania` = c("Australia",""),
                                                      `Eastern Europe & Northern Asia` = c("Russia","")),
                                                    options = list(
                                                      
                                                      `live-search` = TRUE))
                                        
                                        ),
                                        mainPanel(uiOutput(outputId = "image",height = "600px"),
                                                  plotOutput("universityAndMigrationRate",width = "650px")
                                                  
                                        )
                               ),
                               tabPanel("MAP",
                                        mainPanel(leafletOutput("map", width = "150%")),
                                        
                                        absolutePanel(top = 40, right = 10,
                                                      pickerInput("countries", label = "Select your destination:",
                                                                  choices = list( "All countries",
                                                                                  `Southeast Asia` = c("Indonesia", "Malaysia", "Thailand", "Philippines", "Myanmar","Vietnam","India", "Bangladesh"),
                                                                                  `Southern Asia` = c("Pakistan",""),
                                                                                  `Western Asia` = c("Iran", "Iraq", "Saudi Arabia", "Spain","Turkey"),
                                                                                  `Central Asia` = c("Afghanistan", "Kazakhstan", "Uzbekistan"),
                                                                                  `East Asia` = c("China", "Japan", "North Korea", "South Korea", "Taiwan"),
                                                                                  `South America` = c("Brazil", "Colombia", "Argentina", "Peru"),
                                                                                  `North America` = c("United States", "Mexico", "Canada"),
                                                                                  `Europe` = c("Turkey", "Germany", "United Kingdom","France","Italy", "Spain","Ukraine","Poland", "Romania","Kazakhstan"),
                                                                                  `Africa` = c("Nigeria", "Ethiopia", "Egypt", "South Africa", "Kenya","Sudan","Madagascar","Malawi","Zambia"),
                                                                                  `Oceania` = c("Australia",""),
                                                                                  `Eastern Europe & Northern Asia` = c("Russia","")
                                                                  ),
                                                                  options = list(
                                                                    
                                                                    `live-search` = TRUE)
                                                      )
                                        )
                               )
                               
                    )
)

server <- function(input, output, session) {
  
  output$universityAndMigrationRate<- renderPlot({
    if(input$purposeOfVisit == "Study Abroad"){
      ggplot(universities, aes(x = Country, fill = Country)) +
        geom_bar(stat = "count", color = "black", width = 1) + coord_flip()+
        geom_text(stat = "count", aes(label = ..count..), hjust = -0.1,size = 3,position = position_dodge(width = 0.5)) +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title = "Number of Top Universities by Countries",
             caption = "Source: Times Higher Education")
    }
    
    else if(input$purposeOfVisit == "Migrate"){
      ggplot(migrate %>% filter(Country == input$selectCountries),aes(x=year,y=migrants,group = 1)) + 
        geom_line(color = "#193A6F", size = 2, aes(group = 1)) + 
        geom_point(color = "#F98125",size = 5) +
        labs(title = "International migrant stock at mid-year",
             subtitle = "Data from 2000-2019",
             caption = "Source: United Nations Population Division Department of Economic and Social Affairs") +
        theme(
          plot.title = element_text(color = "#193A6F", size = 20, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
          plot.caption = element_text(face = "italic", hjust = 0),
          axis.title.x = element_text(color = "#193A6F", size = 16, face = "bold"),
          axis.title.y = element_text(color = "#193A6F", size = 16, face = "italic")
        )
    }
  }) 
  
  
  
  output$image<-renderUI({
    if(input$purposeOfVisit == "Holiday"){
      if(input$selectCountries == "China")img(src = "china_travel.png",width = '500px', h1("Great Wall of China"), h3("The Great Wall of China is a series of fortifications that were built across the historical northern borders of ancient Chinese states and Imperial China as protection against various nomadic groups from the Eurasian Steppe. Several walls were built from as early as the 7th century BC, with selective stretches later joined together by Qin Shi Huang (220-206 BC), the first emperor of China. Little of the Qin wall remains. Later on, many successive dynasties built and maintained multiple stretches of border walls. The best-known sections of the wall were built by the Ming dynasty (1368-1644)."), h5(tags$a(href="https://en.wikipedia.org/wiki/Great_Wall_of_China","Click here for more information"))) 
      else if(input$selectCountries == "India")img(src = "india_travel.png",height = '350px', h1("Taj Mahal"), h3("The Taj Mahal lit 'Crown of the Palace', is an ivory-white marble mausoleum on the right bank of the river Yamuna in the Indian city of Agra. It was commissioned in 1632 by the Mughal emperor Shah Jahan (r. 1628-1658) to house the tomb of his favourite wife, Mumtaz Mahal; it also houses the tomb of Shah Jahan himself. The tomb is the centrepiece of a 17-hectare (42-acre) complex, which includes a mosque and a guest house, and is set in formal gardens bounded on three sides by a crenellated wall."), h5(tags$a(href="https://en.wikipedia.org/wiki/Taj_Mahal","Click here for more information"))) ##taj mahal
      else if(input$selectCountries == "United States")img(src = "unitedstates_travel.png",height = '350px', h1("Statue of Liberty"), h3("The Statue of Liberty (Liberty Enlightening the World)is a colossal neoclassical sculpture on Liberty Island in New York Harbor in New York City, in the United States. The copper statue, a gift from the people of France to the people of the United States, was designed by French sculptor Frederic Auguste Bartholdi and its metal framework was built by Gustave Eiffel. The statue was dedicated on October 28, 1886."), h5(tags$a(href="https://en.wikipedia.org/wiki/Statue_of_Liberty","Click here for more information"))) ##new york
      else if(input$selectCountries == "Indonesia")img(src = "indonesia_travel.png",height = '350px', h1("Bali"), h3("Bali is a province of Indonesia and the westernmost of the Lesser Sunda Islands. East of Java and west of Lombok, the province includes the island of Bali and a few smaller neighbouring islands, notably Nusa Penida, Nusa Lembongan, and Nusa Ceningan. The provincial capital, Denpasar, is the most populous city in the Lesser Sunda Islands and the second-largest, after Makassar, in Eastern Indonesia. The upland town of Ubud in Greater Denpasar is considered Bali's cultural centre. The province is Indonesia's main tourist destination, with a significant rise in tourism since the 1980s. Tourism-related business makes up 80% of its economy. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Bali","Click here for more information")))##bali
      else if(input$selectCountries == "Pakistan")img(src = "pakistan_travel.png",height = '350px', h1("Fairy Meadows National Park"), h3("Fairy Meadows , named by German climbers and locally known as Joot, is a grassland near one of the base camp sites of the Nanga Parbat, located in Diamer District, Gilgit-Baltistan, Pakistan. At an altitude of about 3,300 metres (10,800 ft) above sea level, it serves as the launching point for trekkers summiting on the Rakhiot face of the Nanga Parbat. In 1995, the Government of Pakistan declared Fairy Meadows a National Park. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Fairy_Meadows_National_Park","Click here for more information")))##fairy meadows
      else if(input$selectCountries == "Brazil")img(src = "brazil_travel.png",height = '350px', h1("Rio de Janeiro"), h3("Rio de Janeiro , or simply Rio, is the second-most populous city in Brazil and the sixth-most populous in the Americas. Rio de Janeiro is the capital of the state of the same name, Brazil's third-most populous state, after Sao Paulo and Minas Gerais. Part of the city has been designated as a World Heritage Site, named Rio de Janeiro: Carioca Landscapes between the Mountain and the Sea, on 1 July 2012 as a Cultural Landscape. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Rio_de_Janeiro","Click here for more information")))##Rio de Janeiro
      else if(input$selectCountries == "Nigeria")img(src = "nigeria_travel.png",height = '350px', h1("Agbokim Waterfalls"), h3("Agbokim waterfalls are situated in the Etung local government area of Cross River State in south-eastern Nigeria, very close to its border with Cameroon. The waterfalls are about 15 kilometres (9.3 mi) from Ikom and 320 kilometres (200 mi) from Calabar. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Agbokim_Waterfalls","Click here for more information")))##Agbokim Falls
      else if(input$selectCountries == "Bangladesh")img(src = "bangladesh_travel.png",height = '350px', h1("Old Dhaka"), h3("Old Dhaka is a term used to refer to the historic old city of Dhaka, the capital of Bangladesh. It was founded in 1608 as Jahangirabad or Jahangirnagar, the capital of Mughal Province of Bengal and named after the Mughal emperor Jahangir. It is located on the banks of the Buriganga River. It was one of the largest and most prosperous cities of South Asia and the center of the worldwide muslin trade. The then Nawab of Bengal Murshid Quli Khan shifted the capital from Dhaka to Murshidabad in the early-18th century. With the rise of Calcutta (now Kolkata) during the British rule, Dhaka began to decline and came to be known as the City of Magnificent Ruins. The British however began to develop the modern city from the mid-19th century. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Old_Dhaka","Click here for more information")))##Old Dhaka
      else if(input$selectCountries == "Russia")img(src = "russia_travel.png",height = '350px', h1("Moscow"), h3("Moscow is the capital and largest city of Russia. The city stands on the Moskva River in Central Russia, with a population estimated at 12.4 million residents within the city limits, over 17 million residents in the urban area, and over 20 million residents in the metropolitan area. The city covers an area of 2,511 square kilometres (970 sq mi), while the urban area covers 5,891 square kilometres (2,275 sq mi), and the metropolitan area covers over 26,000 square kilometres (10,000 sq mi). Moscow is among the world's largest cities; being the most populous city entirely in Europe, the largest urban and metropolitan area in Europe, and the largest city by land area on the European continent. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Moscow","Click here for more information")))##moscow
      else if(input$selectCountries == "Mexico")img(src = "mexico_travel.png",height = '350px', h1("Tolantongo"), h3("Tolantongo is a box canyon and resort located 17 kilometres from Ixmiquilpan on Route 27 in the Mezquital Valley, State of Hidalgo in Mexico, It is about 1.5 hours northwest of Pachuca and 198 km or three-to-four hours northwest of Mexico City. The closest village to the resort is called El Cardonal and it is part of Cardonal Municipality. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Tolantongo","Click here for more information")))##Grutas Tolantongo Hot Pools
      else if(input$selectCountries == "Japan")img(src = "japan_travel.png",height = '350px', h1("Mount Fuji"), h3("Mount Fuji , located on the island of Honshu (the \"mainland\"), is the highest mountain in Japan, standing 3,776.24 m (12,389.2 ft). It is the second-highest volcano located on an island in Asia (after Mount Kerinci on the island of Sumatra), and seventh-highest peak of an island on Earth. Mount Fuji is an active stratovolcano that last erupted from 1707 to 1708. The mountain is located about 100 km (62 mi) southwest of Tokyo and is visible from there on clear days. Mount Fuji's exceptionally symmetrical cone, which is covered in snow for about five months of the year, is commonly used as a cultural icon of Japan and it is frequently depicted in art and photography, as well as visited by sightseers and climbers. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Mount_Fuji","Click here for more information")))##mount fuji
      else if(input$selectCountries == "Ethiopia")img(src = "ethiopia_travel.png",height = '350px', h1("Simien Mountains"), h3("The Simien Mountains, in northern Ethiopia, north east of Gondar in Amhara region, are part of the Ethiopian Highlands. They are a World Heritage Site and include the Simien Mountains National Park. The mountains consist of plateaus separated by valleys and rising to pinnacles. The highest Ethiopian mountain is Ras Dejen at 4,550 m with the second highest peak of Kidis Yared at 4,453 m; other notable peaks include Mount Biuat at 4,437 m."), h5(tags$a(href="https://en.wikipedia.org/wiki/Simien_Mountains","Click here for more information")))##Simien Mountains
      else if(input$selectCountries == "Philippines")img(src = "philippines_travel.png",height = '350px', h1("Coron"), h3("Coron, officially the Municipality of Coron (Tagalog: Bayan ng Coron), is a 1st class municipality in the province of Palawan, Philippines. According to the 2020 census, it has a population of 65,855 people.The main population center of the municipality is composed of Poblacion barangays 1 to 6, where the Municipal Building, the Municipal Legislative Building, and the Judicial Hall of the Municipal Circuit Trial Court are located. Its fiesta is held annually on August 28 in honor of Saint Augustine. It is the commercial capital of the Calamian Islands."), h5(tags$a(href="https://en.wikipedia.org/wiki/Coron,_Palawan","Click here for more information")))##Coron
      else if(input$selectCountries == "Egypt")img(src = "egypt_travel.png",height = '350px', h1("Pyramid"), h3("A pyramid is a structure whose outer surfaces are triangular and converge to a single step at the top, making the shape roughly a pyramid in the geometric sense. The base of a pyramid can be trilateral, quadrilateral, or of any polygon shape. As such, a pyramid has at least three outer triangular surfaces (at least four faces including the base). The square pyramid, with a square base and four triangular outer surfaces, is a common version."), h5(tags$a(href="https://en.wikipedia.org/wiki/Pyramid","Click here for more information")))##pyramids
      else if(input$selectCountries == "Vietnam")img(src = "vietnam_travel.png",height = '350px', h1("Halong Bay"), h3("Halong is a UNESCO World Heritage Site and popular travel destination in Quang Ninh Province, Vietnam. The name HaLong means \"descending dragon\". Administratively, the bay belongs to HaLong city, Cam Pha city, and is a part of Van Don District. The bay features thousands of limestone karsts and isles in various shapes and sizes. Ha Long Bay is a center of a larger zone which includes Bai Tu Long Bay to the northeast, and Cat Ba Island to the southwest. These larger zones share a similar geological, geographical, geomorphological, climate, and cultural characters."), h5(tags$a(href="https://en.wikipedia.org/wiki/H%E1%BA%A1_Long_Bay","Click here for more information")))##Halong Bay
      else if(input$selectCountries == "Turkey")img(src = "turkey_travel.png",height = '350px', h1("Istanbul"), h3("Istanbul , formerly known as Constantinople, is the largest city in Turkey and the country's economic, cultural and historic center. The city straddles the Bosporus strait, and lies in both Europe and Asia, with a population of over 15 million residents, comprising 19% of the population of Turkey. Istanbul is the most populous city in Europe, and the world's fifteenth-largest city."), h5(tags$a(href="https://en.wikipedia.org/wiki/Istanbul","Click here for more information")))##istanbul
      else if(input$selectCountries == "Iran")img(src = "iran_travel.png",height = '350px', h1("Persepolis"), h3("Persepolis was the ceremonial capital of the Achaemenid Empire (c. 550-330 BC). It is situated in the plains of Marvdasht, encircled by southern Zagros mountains of Iran. Modern day Shiraz is situated 60 kilometres (37 mi) southwest of the ruins of Persepolis. The earliest remains of Persepolis date back to 515 BC. It exemplifies the Achaemenid style of architecture. UNESCO declared the ruins of Persepolis a World Heritage Site in 1979. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Persepolis","Click here for more information")))##Persepolis, Shiraz
      else if(input$selectCountries == "Germany")img(src = "germany_travel.png",height = '350px', h1("Munich"), h3("Munich is the capital and most populous city of Bavaria. With a population of 1,558,395 inhabitants as of 31 July 2020, it is the third-largest city in Germany, after Berlin and Hamburg, and thus the largest which does not constitute its own state, as well as the 11th-largest city in the European Union. The city's metropolitan region is home to 6 million people. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Munich","Click here for more information")))##munich
      else if(input$selectCountries == "Thailand")img(src = "thailand_travel.png",height = '350px', h1("Chiang Rai"), h3("Chiang Rai is the northernmost major city in Thailand, with a population of about 200,000 people. It is located in Mueang Chiang Rai District, Chiang Rai Province. Chiang Rai was established as a capital city in the reign of King Mangrai, in 1262 CE."), h5(tags$a(href="https://en.wikipedia.org/wiki/Chiang_Rai","Click here for more information")))##chiang rai
      else if(input$selectCountries == "United Kingdom")img(src = "unitedkingdom_travel.png",height = '350px', h1("London Bridge"), h3("Several bridges named London Bridge have spanned the River Thames between the City of London and Southwark, in central London. The current crossing, which opened to traffic in 1973, is a box girder bridge built from concrete and steel. It replaced a 19th-century stone-arched bridge, which in turn superseded a 600-year-old stone-built medieval structure. This was preceded by a succession of timber bridges, the first of which was built by the Roman founders of London."), h5(tags$a(href="https://en.wikipedia.org/wiki/London_Bridge","Click here for more information")))
      else if(input$selectCountries == "France")img(src = "france_travel.png",height = '350px', h1("Paris"), h3("Paris is the capital and most populous city of France, with an estimated population of 2,175,601 residents as of 2018, in an area of more than 105 square kilometres (41 square miles). Since the 17th century, Paris has been one of Europe's major centres of finance, diplomacy, commerce, fashion, gastronomy, science, and arts."), h5(tags$a(href="https://en.wikipedia.org/wiki/Paris","Click here for more information")))##paris
      else if(input$selectCountries == "Italy")img(src = "italy_travel.png",height = '350px', h1("Santa Margherita Ligure"), h3("Santa Margherita Ligure (Ligurian: Santa Margaita) is a comune (municipality) in the Metropolitan City of Genoa in the Italian region Liguria, located about 35 kilometres (22 mi) southeast of Genoa, in the area traditionally known as Tigullio. It has a port, used for both tourism and fishing activities. Part of comune territory is included in the Regional Natural Park of Portofino. Santa Margherita Ligure borders the following municipalities: Camogli, Portofino, Rapallo."), h5(tags$a(href="https://en.wikipedia.org/wiki/Santa_Margherita_Ligure","Click here for more information")))##Santa Margherita
      else if(input$selectCountries == "South Africa")img(src = "southafrica_travel.png",height = '350px', h1("Cape Town"), h3("Cape Town is the oldest and second largest city in South Africa, after Johannesburg, and also the seat of the Parliament of South Africa."), h5(tags$a(href="https://en.wikipedia.org/wiki/Cape_Town","Click here for more information")))##CAPE TOWN
      else if(input$selectCountries == "Myanmar")img(src = "myanmar_travel.png",height = '350px', h1("Bagan"), h3("Bagan is an ancient city and a UNESCO World Heritage Site in the Mandalay Region of Myanmar. From the 9th to 13th centuries, the city was the capital of the Bagan Kingdom, the first kingdom that unified the regions that would later constitute Myanmar. During the kingdom's height between the 11th and 13th centuries, more than 10,000 Buddhist temples, pagodas and monasteries were constructed in the Bagan plains alone, of which the remains of over 2200 temples and pagodas survive."), h5(tags$a(href="https://en.wikipedia.org/wiki/Bagan","Click here for more information")))##Bagan
      else if(input$selectCountries == "Kenya")img(src = "kenya_travel.png",height = '350px', h1("Maasai Mara"), h3("Maasai Mara, also sometimes spelled Masai Mara and locally known simply as The Mara, is a large national game reserve in Narok, Kenya, contiguous with the Serengeti National Park in Tanzania. It is named in honor of the Maasai people, the ancestral inhabitants of the area, who migrated to the area from the Nile Basin. Their description of the area when looked at from afar: \"Mara\" means \"spotted\" in the local Maasai language, due to the many short bushy trees which dot the landscape."), h5(tags$a(href="https://en.wikipedia.org/wiki/Maasai_Mara","Click here for more information")))##masai mara
      else if(input$selectCountries == "South Korea")img(src = "southkorea_travel.png",height = '350px', h1("Busan"), h3("Busan , officially known as Busan Metropolitan City, is South Korea's second-most populous city after Seoul, with a population of over 3.4 million inhabitants. Formerly romanized as Pusan, it is the economic, cultural and educational center of southeastern South Korea, with its port-Korea's busiest and the sixth-busiest in the world. The surrounding \"Southeast Economic Zone\" (including Ulsan and South Gyeongsang) is South Korea's largest industrial area."), h5(tags$a(href="https://en.wikipedia.org/wiki/Busan","Click here for more information")))##busan
      else if(input$selectCountries == "Colombia")img(src = "colombia_travel.png",height = '350px', h1("Capurgana"), h3("Capurgana is a town of the municipality of Acandi on the northwestern coast of the Gulf of Uraba in the Colombian department of Choco, adjacent to the border between Colombia and Panama. Previously a base site for eco-tourists, since 2010 the town's main source of income has been migrants preparing to hike into Panama, across the Darien Gap. In 2019 it was described as a \"smugglers' town\"."), h5(tags$a(href="https://en.wikipedia.org/wiki/Capurgan%C3%A1","Click here for more information")))##Capurgana
      else if(input$selectCountries == "Spain")img(src = "spain_travel.png", h1("Sagrada Familia"), h3("The Basilica de la Sagrada Familia also known as the Sagrada Familia, is a large unfinished minor basilica in the Eixample district of Barcelona, Catalonia, Spain. Designed by the Catalan architect Antoni Gaudi (1852-1926), his work on the building is part of a UNESCO World Heritage Site. On 7 November 2010, Pope Benedict XVI consecrated the church and proclaimed it a minor basilica."), h5(tags$a(href="https://en.wikipedia.org/wiki/Sagrada_Fam%C3%ADlia","Click here for more information"))) ##Sagrada FamÃÂ­lia
      else if(input$selectCountries == "Argentina")img(src = "argentina_travel.png",height = '350px', h1("Iguazu Falls"), h3("Iguacu Falls are waterfalls of the Iguazu River on the border of the Argentine province of Misiones and the Brazilian state of Parana. Together, they make up the largest waterfall system in the world. The falls divide the river into the upper and lower Iguazu. The Iguazu River rises near the heart of the city of Curitiba. For most of its course, the river flows through Brazil. However, most of the falls are on the Argentine side. Below its confluence with the San Antonio River, the Iguazu River forms the border between Argentina and Brazil."), h5(tags$a(href="https://en.wikipedia.org/wiki/Iguazu_Falls","Click here for more information")))##iguazu falls
      else if(input$selectCountries == "Sudan")img(src = "sudan_travel.png",height = '350px', h1("Naqa"), h3("Naqa or Nagaa is a ruined ancient city of the Kushitic Kingdom of Meroe in modern-day Sudan. The ancient city lies about 170 km (110 mi) north-east of Khartoum, and about 50 km (31 mi) east of the Nile River located at approximately MGRS 36QWC290629877. Here smaller wadis meet the Wadi Awateib coming from the center of the Butana plateau region, and further north at Wad ban Naqa from where it joins the Nile. Naqa was only a camel or donkey's journey from the Nile, and could serve as a trading station on the way to the east; thus it had strategic importance."), h5(tags$a(href="https://en.wikipedia.org/wiki/Naqa","Click here for more information")))##naqa
      else if(input$selectCountries == "Ukraine")img(src = "ukraine_travel.png",height = '350px', h1("Lviv"), h3("Lviv is the largest city in western Ukraine and the seventh-largest city in the country overall, with a population of 717,510 (2021 est.) Lviv is one of the main cultural centres of Ukraine."), h5(tags$a(href="https://en.wikipedia.org/wiki/Lviv","Click here for more information")))##lviv
      else if(input$selectCountries == "Iraq")img(src = "iraq_travel.png",height = '350px', h1("Baghdad"), h3("Baghdad is the capital of Iraq and the second-largest city in the Arab world after Cairo. It is located along the Tigris near the ruins of the ancient Akkadian city of Babylon and the Sassanid Persian capital of Ctesiphon. In the eighth century, Baghdad was chosen as the capital of the Abbasid Caliphate, and became its most notable major development project. Within a short time, the city evolved into a significant cultural, commercial, and intellectual center of the Muslim world. This, in addition to housing several key academic institutions, including the House of Wisdom, as well as a multiethnic and multi-religious environment, garnered it a worldwide reputation as the \"Center of Learning\"."), h5(tags$a(href="https://en.wikipedia.org/wiki/Baghdad","Click here for more information")))##baghdad
      else if(input$selectCountries == "Afghanistan")img(src = "afghanistan_travel.png",height = '350px', h1("Hazrat Ali Mazar"), h3("The Hazrat Ali Mazar , located in Balkh, Afghanistan, is a mosque which has a tomb attributed to the fourth caliph Ali. The site is an important Shia pilgrimage area regarding, and is also honored by Sunnis, who pay tribute to Ali's shrine every year."), h5(tags$a(href="https://en.wikipedia.org/wiki/Hazrat_Ali_Mazar","Click here for more information")))##The Shrine of Hazrat Ali in Mazar-e-Sharif
      else if(input$selectCountries == "Poland")img(src = "poland_travel.png",height = '350px', h1("Krakow"), h3("Krakow , also written in English as Krakow and traditionally known as Cracow, is the second-largest and one of the oldest cities in Poland. Situated on the Vistula River in Lesser Poland Voivodeship, the city dates back to the seventh century. Krakow was the official capital of Poland until 1596 and has traditionally been one of the leading centres of Polish academic, economic, cultural and artistic life. Cited as one of Europe's most beautiful cities, its Old Town was declared the first UNESCO World Heritage Site in the world."), h5(tags$a(href="https://en.wikipedia.org/wiki/Krak%C3%B3w","Click here for more information")))##krakow
      else if(input$selectCountries == "Canada")img(src = "canada_travel.png",height = '350px', h1("Banff National Park"), h3("Banff National Park is Canada's oldest national park, established in 1885. Located in Alberta's Rocky Mountains, 110-180 kilometres (68-112 mi) west of Calgary, Banff encompasses 6,641 square kilometres (2,564 sq mi) of mountainous terrain, with many glaciers and ice fields, dense coniferous forest, and alpine landscapes. The Icefields Parkway extends from Lake Louise, connecting to Jasper National Park in the north. Provincial forests and Yoho National Park are neighbours to the west, while Kootenay National Park is located to the south and Kananaskis Country to the southeast. The main commercial centre of the park is the town of Banff, in the Bow River valley."), h5(tags$a(href="https://en.wikipedia.org/wiki/Banff_National_Park","Click here for more information")))##banff
      else if(input$selectCountries == "Saudi Arabia")img(src = "saudiarabia_travel.png",height = '350px', h1("Qal'at al-Bahrain"), h3("The Qal'at al-Bahrain , also known as the Bahrain Fort or Portuguese Fort, is an archaeological site located in Bahrain. Archaeological excavations carried out since 1954 have unearthed antiquities from an artificial mound of 12 m (39 ft) height containing seven stratified layers, created by various occupants from 2300 BC up to the 18th century, including Kassites, Greeks, Portuguese& Persians. It was once the capital of the Dilmun civilization and was inscribed as a UNESCO World Heritage Site in 2005. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Qal%27at_al-Bahrain","Click here for more information")))##bahrain fort
      else if(input$selectCountries == "Uzbekistan")img(src = "uzbekistan_travel.png",height = '350px', h1("Khiva"), h3("Khiva is a city of approximately 90,000 people in Xorazm Region, Uzbekistan. According to archaeological data, the city was established around 1500 years ago. It is the former capital of Khwarezmia, the Khanate of Khiva, and the Khorezm People's Soviet Republic. Itchan Kala in Khiva was the first site in Uzbekistan to be inscribed in the World Heritage List (1991). The astronomer, historian and polymath, Al-Biruni (973-1048 CE) was born in either Khiva or the nearby city of Kath."), h5(tags$a(href="https://en.wikipedia.org/wiki/Khiva","Click here for more information")))##Khiva
      else if(input$selectCountries == "Peru")img(src = "peru_travel.png",height = '350px', h1("Machu Picchu"), h3("Machu Picchu is a 15th-century Inca citadel located in the Eastern Cordillera of southern Peru on a 2,430-meter (7,970 ft) mountain ridge. It is located in the Machupicchu District within Urubamba Province above the Sacred Valley, which is 80 kilometers (50 mi) northwest of Cuzco. The Urubamba River flows past it, cutting through the Cordillera and creating a canyon with a tropical mountain climate. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Machu_Picchu","Click here for more information")))##Machu Picchu
      else if(input$selectCountries == "Malaysia")img(src = "malaysia_travel.png",height = '350px', h1("Kuala Lumpur"), h3("Kuala Lumpur ,officially the Federal Territory of Kuala Lumpur and colloquially referred to as KL, is a federal territory and the capital city of Malaysia. It is the largest city in Malaysia, covering an area of 243 km2 (94 sq mi) with an estimated population of 1.73 million as of 2016. Greater Kuala Lumpur, also known as the Klang Valley, is an urban agglomeration of 7.564 million people as of 2018. It is among the fastest growing metropolitan regions in Southeast Asia, both in population and economic development."), h5(tags$a(href="https://en.wikipedia.org/wiki/Kuala_Lumpur","Click here for more information")))
      else if(input$selectCountries == "Nepal")img(src = "nepal_travel.png",height = '350px', h1("Khumbu"), h3("Khumbu (also known as the Everest Region) is a region of northeastern Nepal on the Nepalese side of Mount Everest. It is part of the Solukhumbu District, which in turn is part of Province No. 1. Khumbu is one of three subregions of the main Kirat Kulung and Sherpa settlement of the Himalaya, the other two being Solu and Pharak. It includes the town of Namche Bazaar as well as the villages of Thame, Khumjung, Pangboche, Pheriche and Kunde. The famous Buddhist monastery at Tengboche is also located in the Khumbu. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Khumbu","Click here for more information")))##Khumbu Valley
      else if(input$selectCountries == "Madagascar")img(src = "madagascar_travel.png",height = '350px', h1("Avenue of the Baobabs"), h3("The Avenue of the Baobabs, or Alley of the Baobabs, is a prominent group of Grandidier's baobabs (Adansonia grandidieri) lining the dirt road between Morondava and Belon'i Tsiribihina in the Menabe region of western Madagascar. Its striking landscape draws travelers from around the world, making it one of the most visited locations in the region. It has been a center of local conservation efforts, and was granted temporary protected status in July 2007 by the Ministry of Environment, Water and Forests - a step toward making it Madagascar's first natural monument. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Avenue_of_the_Baobabs","Click here for more information")))##the avenue of baobabs
      else if(input$selectCountries == "North Korea")img(src = "northkorea_travel.png",height = '350px', h1("Juche Tower"), h3("The Juche Tower (more formally, the Tower of the Juche Idea), completed in 1982, is a monument in Pyongyang, the capital of North Korea, and is named after the ideology of Juche introduced by the country's first leader, Kim Il-sung."), h5(tags$a(href="https://en.wikipedia.org/wiki/Juche_Tower","Click here for more information")))##Tower of the Juche Idea
      else if(input$selectCountries == "Australia")img(src = "australia_travel.png",height = '350px', h1("Uluru"), h3("Uluru, also known as Ayers Rock and officially gazetted as Uluru / Ayers Rock, is a large sandstone formation in the southern part of the Northern Territory in Australia. It lies 335 km (208 mi) southwest of the nearest large town: Alice Springs."), h5(tags$a(href="https://en.wikipedia.org/wiki/Uluru","Click here for more information")))##Uluru
      else if(input$selectCountries == "Taiwan")img(src = "taiwan_travel.png",height = '350px', h1("Taipei"), h3("Taipei, officially Taipei City, is the capital and a special municipality of the Republic of China (Taiwan). Located in Northern Taiwan, Taipei City is an enclave of the municipality of New Taipei City that sits about 25 km (16 mi) southwest of the northern port city of Keelung. Most of the city rests on the Taipei Basin, an ancient lakebed. The basin is bounded by the relatively narrow valleys of the Keelung and Xindian rivers, which join to form the Tamsui River along the city's western border. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Taipei","Click here for more information")))##taipei
      else if(input$selectCountries == "Sri Lanka")img(src = "srilanka_travel.png",height = '350px', h1("Sigiriya"), h3("Sigiriya or Sinhagiri is an ancient rock fortress located in the northern Matale District near the town of Dambulla in the Central Province, Sri Lanka. It is a site of historical and archaeological significance that is dominated by a massive column of rock around 180 metres (590 ft) high. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Sigiriya","Click here for more information")))##sigiriya Rock
      else if(input$selectCountries == "Romania")img(src = "romania_travel.png",height = '350px', h1("Sinaia"), h3("Sinaia is a town and a mountain resort in Prahova County, Romania. It is situated in the historical region of Muntenia. The town was named after the Sinaia Monastery of 1695, around which it was built. The monastery in turn is named after the Biblical Mount Sinai. King Carol I of Romania built his summer home, Pele?? Castle, near the town in the late nineteenth century."), h5(tags$a(href="https://en.wikipedia.org/wiki/Sinaia","Click here for more information")))##sinaia
      else if(input$selectCountries == "Malawi")img(src = "malawi_travel.png",height = '350px', h1("Lake Malawi"), h3("Lake Malawi, also known as Lake Nyasa in Tanzania and Lago Niassa in Mozambique, is an African Great Lake and the southernmost lake in the East African Rift system, located between Malawi, Mozambique and Tanzania."), h5(tags$a(href="https://en.wikipedia.org/wiki/Lake_Malawi","Click here for more information")))##lake malawi
      else if(input$selectCountries == "Kazakhstan")img(src = "kazakhstan_travel.png",height = '350px', h1("Ile-Alatau National Park"), h3("Ile-Alatau National Park is a national park in Kazakhstan. It was created in 1996 and covers about 200,000 ha. It is situated in the mountains south of Almaty between Gorge Turgen in the east and Chemolgan River in the west. The National Park borders Almaty Nature Reserve, which is located around Pik Talgar. "), h5(tags$a(href="https://en.wikipedia.org/wiki/Ile-Alatau_National_Park","Click here for more information")))##ILE-ALATAU NATIONAL PARK
      else if(input$selectCountries == "Zambia")img(src = "zambia_travel.png",height = '350px', h1("Victoria Falls"), h3("Victoria is a waterfall on the Zambezi River in southern Africa, which provides habitat for several unique species of plants and animals. It is located on the border between Zambia and Zimbabwe and is one of the world's largest waterfalls, with a width of 1,708 m (5,604 ft)."), h5(tags$a(href="https://en.wikipedia.org/wiki/Victoria_Falls","Click here for more information")))##victoria falls
      else if(input$selectCountries == "Syria")img(src = "syria_travel.png",height = '350px', h1("Krak des Chevaliers"), h3("Krak des Chevaliers or Crac des, also called ???i???n al-Akrad and formerly Crac de l'Ospital, is a Crusader castle in Syria and one of the most important preserved medieval castles in the world. The site was first inhabited in the 11th century by Kurdish troops garrisoned there by the Mirdasids. In 1142 it was given by Raymond II, Count of Tripoli, to the order of the Knights Hospitaller. It remained in their possession until it fell in 1271."), h5(tags$a(href="https://en.wikipedia.org/wiki/Krak_des_Chevaliers","Click here for more information")))##Krak des Chevaliers
      
      
    }
  })
  
  filteredData <- reactive({
    if (input$countries == "All countries" || input$countries == "") {
      mydata
    } else {
      filter(mydata, Country == input$countries)
    }
  })
  
  filteredIcon <- reactive({
    if (input$countries == "All countries") {
      flagIcon
    } else {
      flagIcon$iconUrl <- paste0("country_flag/", input$countries, ".png")
    }
    flagIcon
  })
  
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addMarkers(~longitude, ~latitude, 
                 icon = filteredIcon(), 
                 label = ~Country, 
                 labelOptions = labelOptions(textsize = "12px"),
                 popup = ~popup_text)
  })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addMarkers(~longitude, ~latitude, 
                 icon = filteredIcon(), 
                 label = ~Country, 
                 labelOptions = labelOptions(textsize = "12px"),
                 popup = ~popup_text)
  })
}

shinyApp(ui, server)
