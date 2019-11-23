library(shiny)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)
library(lmtest)
library(wordcloud)
library(tm)
library(stringr)

ui = navbarPage("A Look Inside The Box...Office",
                #Intro Page
                tabPanel("Introduction",
                         titlePanel("Introduction"),
                         sidebarLayout(
                             sidebarPanel(
                                 tags$i("Lights, camera, action!"), 
                                 br(),("What makes a movie successful and worth watching? Directors and consumers have thought about this question for a very long time.
                                 For this project, we will look at the data webscraped from"), tags$b("https://m.the-numbers.com/movies/"), "and find out what affects movie sales.",
                                 br(),
                                 br(),tags$b("This project consists of 4 parts:"),
                                 br(),tags$i("Data Viewer:"), 
                                 br(),("The data viewer consists of different filters to browse the webscraped data."),
                                 br(),tags$i("Data Visualization:"),
                                 br(),("This page contains the different visualizations created from the data and a brief summary/explanation of our findings."),
                                 br(),tags$i("Data Analysis:"),
                                 br(),("Applied various statistical methods to a fitted linear regression model (e.g:bptest,cook's distance,etc) and summarized our results."),
                                 br(),tags$i("Recommendation:"),
                                 br(),("Users can enter a set of criterias and the page will return a list of movies that fulfill the criterias with descending order based on how much money the movie earned.")
                                 
                                 
                             ),
                             mainPanel(
                                 h1(tags$i("What factors affect movie sales?")),
                                 img(src="lights.jpg",height = 300,width = 500),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),tags$i("Taiga Hasegawa (taigah2)"), 
                                 br(),tags$i("Seung ji Nam (sjnam2)"),
                                 br(),tags$i("Zhuofeng Lei (zlei5)")
                                 
                             )
                         )
                ),
                #Data Page
                tabPanel("Data Viewer",
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput(inputId = "genre",
                                             label = h3("Genre"),
                                             choices = list("All Genres" = "All",
                                                            "Action" = "Action",
                                                            "Adventure" = "Adventure",
                                                            "Black Comedy" = "Black Comedy",
                                                            "Comedy" = "Comedy",
                                                            "Concert/Performance" = "Concert/Performance",
                                                            "Documentary" = "Documentary",
                                                            "Drama" = "Drama",
                                                            "Horror" = "Horror",
                                                            "Multiple Genres" = "Multiple Genres",
                                                            "Musical" = "Musical",
                                                            "Reality" = "Reality",
                                                            "Romantic Comedy" = "Romantic Comedy",
                                                            "Thriller/Suspense" = "Thriller/Suspense",
                                                            "Western" = "Western"),
                                             selected = "All"),
                                 selectInput(inputId = "source",
                                             label = h3("Source"),
                                             choices = list("All",
                                                            "Based on Ballet",
                                                            "Based on Comic/Graphic Novel",
                                                            "Based on Factual Book/Article",
                                                            "Based on Fiction Book/Short Story",
                                                            "Based on Folk Tale/Legend/Fairytale",
                                                            "Based on Game",
                                                            "Based on Movie",
                                                            "Based on Musical Group",
                                                            "Based on Musical or Opera",
                                                            "Based on Play",
                                                            "Based on Real Life Events",
                                                            "Based on Religious Text",
                                                            "Based on Short Film",
                                                            "Based on Song",
                                                            "Based on Theme Park Ride",
                                                            "Based on Toy",
                                                            "Based on TV",
                                                            "Based on Web Series",
                                                            "Compilation",
                                                            "Original Screenplay",
                                                            "Remake",
                                                            "Spin-Off"
                                             ),
                                             selected = "All"),
                                 selectInput(inputId = "franchise",
                                             label = h3("Is Franchise?"),
                                             choices = list("True" = "1",
                                                            "False" = "0",
                                                            "Unselected" = "Unselected"),
                                             selected = "Unselected"
                                 ),
                                 selectInput(inputId = "creative",
                                             label = h3("Creative Type:"),
                                             choices = list("All"="All",
                                                            "Super Hero"="Super Hero",
                                                            "Science Fiction"="Science Fiction",
                                                            "Kids Fiction"="Kids Fiction",
                                                            "Fantasy"="Fantasy",
                                                            "Dramatization"="Dramatization ",
                                                            "Contemporary Fiction"="Contemporary Fiction",
                                                            "Historical Fiction"="Historical Fiction",
                                                            "Factual"="Factual",
                                                            "Multiple Creative Types"="Multiple Creative Types"
                                             ),selected = "Unselected"),
                                 selectInput(inputId = "prod_method",
                                             label = h3("Production Method:"),
                                             choices = list("All"="All",
                                                            "Live Action"="Live Action",
                                                            "Digital Animation"="Digital Animation",
                                                            "Animation/Live Action"="Animation/Live Action",
                                                            "Hand Animation"="Hand Animation",
                                                            "Stop-Motion Animation"="Stop-Motion Animation",
                                                            "Multiple Production Methods"="Multiple Production Methods"
                                             ),selected = "Unselected"),
                                 sliderInput(inputId = "run_time",
                                             label = "Minimum Running Time (Minutes)",
                                             min = 0,max = 300,value = 0),
                                 sliderInput(inputId = "budget",
                                             label = h3("Minimum Production Budget ($)"),
                                             min = 0,max = 500000000,value = 0),
                                 sliderInput(inputId = "open",
                                             label = h3("Opening Weekend ($)"),
                                             min = 0,max = 100000000,value = 0),
                                 sliderInput(inputId = "doffice",
                                             label = h3("Minimum Domestic Office ($)"),
                                             min = 0,max = 100000000,value = 0),
                                 sliderInput(inputId = "wwoffice",
                                             label = h3("Minimum Worldwide Box Office ($)"),
                                             min = 0,max = 300000000, value = 0),
                                 checkboxGroupInput(inputId = "table_col",
                                                    label = h3("Show following columns"),
                                                    choices = list(
                                                        "Release Date" = 2,
                                                        "Title" = 3,
                                                        "Production Budget" = 4,
                                                        "Opening Weekend" = 5,
                                                        "Domestic Box Office" = 6,
                                                        "World Wide Box Office" = 7,
                                                        "Trailer" = 8,
                                                        "MPAA Rating" = 9,
                                                        "Production Method" = 10,
                                                        "Genre" = 11,
                                                        "Running Time" = 12,
                                                        "Keywords" = 13,
                                                        "Production Company" = 15,
                                                        "Production Country" = 16,
                                                        "Source" = 17,
                                                        "Creative Type" = 18),
                                                    selected = c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18))
                                 
                             ),
                             mainPanel(
                                 dataTableOutput("movie_data")
                             )
                         )
                ),
                #Data Visualization Page
                tabPanel("Data Visualization",
                         navbarPage("Visual",
                                    tabPanel("Movie Counts",
                                             sidebarLayout(
                                                 sidebarPanel(
                                                     selectInput(inputId = "bar",
                                                                 label = h3("Variable"),
                                                                 choices = list("MPAA_Rating" = "MPAA_Rating",
                                                                                "Production Method" = "Production_Method",
                                                                                "Genre" = "Genre",
                                                                                "Source" = "Source",
                                                                                "Creative Type" = "Creative_Type"
                                                                 ),
                                                                 selected = "MPAA_Rating"
                                                     )
                                                 ),
                                                 mainPanel(
                                                     plotOutput("bar_graph")
                                                 )
                                             )),
                                    tabPanel("WorldwideBox.Office",
                                        
                                                 mainPanel(
                                                     h3("DomesticBox.Office vs WorldwideBox.office"),
                                                     plotOutput("DomesticBox"),
                                                     tags$i("DomesticBox.Office` is the overall profit in US and `WorldwideBox.Office is the overall profit in the world. Plot between `WorldwideBox.Office` and `DomesticwideBox.Office` are highly correlated. If a movie earns a lot of profit in the US, then it should earn a lot of profit in other parts of the world."),
                                                     h3("Opening Weekend vs WorldwideBox.office"),
                                                     plotOutput("OpeningWeekend"),
                                                     tags$i("Opening Weekend` is the profit during opening weekend and this variable also showed higly corrleated with `WorldwideBox.office. If a recently released movie does well, it should do well worldwide."),
                                                     h3("Month vs WorldwideBox.office"),
                                                     plotOutput("Month"),
                                                     tags$i("This is a boxplot of `WorldwideBox.Office` by `month. Movies released on May, Jun, Jul had higher `WorldwideBox.Office` comparing to the movies released in other months. This is probably due to an increase in consumers as students go on break."),
                                                     h3("Year vs WorldwideBox.office"),
                                                     plotOutput("Year"),
                                                     tags$i("Recently released movies showed higher `WorldwideBox.Office`."),
                                                     h3("Genre v.s. WorldwideBox.office"),
                                                     plotOutput("Genre"),
                                                     tags$i("Among `Genre`, 'Action' and 'Adventure' have higher profits than other genres."),
                                                     h3("Creative Type vs WorldwideBox.office"),
                                                     plotOutput("Creative_Type"),
                                                     tags$i("Among `Creative Types`, 'Super Hero' showed the highest `WorldwideBox.Office`.'Kids Fiction' and 'Science Fiction' also had high `WorldwideBox.Office. "),
                                                     h3("Source vs WorldwideBox.office"),
                                                     plotOutput("Source"),
                                                     tags$i("Among `Source` based on, movies 'Based on Fiction Book/Short Story' have several high `WorldwideBox.Office`, but most of `Source`s are showing similar `WorldwideBox.Office`."),
                                                     h3("Production Method vs WorldwideBox.office"),
                                                     plotOutput("Production_Method"),
                                                     tags$i("Among `Production Method`, 'Animation/Live Action' and 'Digital Animation' showed high `WorldwideBox.Office`."),
                                                     h3("See the trend of Genre"),
                                                     plotOutput("tGenre"),
                                                     tags$i("From 1930 to 2000, `WorldwideBox.Office` among different `Genre`s were similar, but from 2001 to 2010 and from 2011 to 2020, 'Action and 'Adventure' Genres showed significantly higher WorldwideBox.Office."),
                                                     h3("See the trend of Creative Type"),
                                                     plotOutput("tCreative_type"),
                                                     tags$i("From 1930 to 2000, 'Contemporay Fiction' showed high `WorldwideBox.Office`. From 2001 to 2010 and from 2011 to 2020, 'Contemporary Fiction' still showed high 'WorldwideBox.Office'. Other `Creative Type` also showed high `WorldwideBox.Office`.")
                                                 )
                                             ),
                                    tabPanel("",
                                             sidebarLayout(
                                                 sidebarPanel(
                                                     sliderInput(inputId = "nbin",
                                                                 label = h3("Number of Bins"),
                                                                 min = 10,max = 50,value = 30)
                                                 ),
                                                 mainPanel()
                                             )
                                    ),
                                    tabPanel("Word Cloud",
                                             sidebarLayout(
                                                 sidebarPanel(
                                                     selectInput(inputId = "word",
                                                                 label = h3("Variable"),
                                                                 choices = list("Title" = "Title",
                                                                                "Keywords" = "Keywords"),
                                                                 selected = "Title"),
                                                     sliderInput(inputId = "minfreq",
                                                                 label = h3("Minimum Frequency:"),
                                                                 min = 1,  max = 50, value = 10),
                                                     sliderInput(inputId = "max",
                                                                 label = h3("Maximum Number of Words:"),
                                                                 min = 1,  max = 300,  value = 100)
                                                 ),
                                                 mainPanel(
                                                     plotOutput("wordcloud")
                                                 )
                                             )
                                    )
                         )
                ),
                #Data Analysis Page
                tabPanel("Data Analysis",
                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput(inputId = "Year", label = h3("Year"),
                                             min = 1916, max = 2021, value = c(1916, 2021))
                             ),
                             mainPanel(
                                 #Please refer to "Data Analysis.Rmd" for detail
                                 h3("Fit Statistics"),
                                 verbatimTextOutput("FitStatistics"),
                                 tags$i("The significant variables change over time. The variables that have stars are significat variables because of small p-value. R square is the proportion of the variable in the dependent variable. "),
                                 h3("QQplot"),
                                 plotOutput("qqplot"),
                                 tags$i("QQplot checks the normality of the residuals. The closer to the line, the more residuals follow the normal distribution. "),
                                 h3("bptest"),
                                 verbatimTextOutput("bptest"),
                                 tags$i("BP test checks the constant variation. If the p-value is small, we reject the null hypothesis that there is the constant variance of the residuals."),
                                 h3("Cook's Distance"),
                                 plotOutput("cooks_distance"),
                                 tags$i("Cooks distance shows the outlier. If the point is over 1, it can be considered as a outlier.")
                             ))
                ),
                tabPanel("Recommendation",
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput(inputId = "Genre",
                                             label = h3("Genre:"),
                                             choices = list("Unselected"="Unselected",
                                                            "Action" = "Action",
                                                            "Adventure" = "Adventure",
                                                            "Thriller/Suspense" = "Thriller/Suspense",
                                                            "Horror"="Horror",
                                                            "Drama"="Drama",
                                                            "Romantic Comedy"="Romantic Comedy",
                                                            "Black Comedy"="Black Comedy",
                                                            "Comedy"="Comedy",
                                                            "Western"="Western",
                                                            "Concert/Performance"="Concert/Performance",
                                                            "Romantic Comedy"="Romantic Comedy",
                                                            "Documentary"="Documentary",
                                                            "Musical"="Musical",
                                                            "Multiple Genres"="Multiple Genres"
                                             ),selected = "Unselected"),
                                 selectInput(inputId = "Creative_Type",
                                             label = h3("Creative Type:"),
                                             choices = list("Unselected"="Unselected",
                                                            "Super Hero"="Super Hero",
                                                            "Science Fiction"="Science Fiction",
                                                            "Kids Fiction"="Kids Fiction",
                                                            "Fantasy"="Fantasy",
                                                            "Dramatization"="Dramatization ",
                                                            "Contemporary Fiction"="Contemporary Fiction",
                                                            "Historical Fiction"="Historical Fiction",
                                                            "Factual"="Factual",
                                                            "Multiple Creative Types"="Multiple Creative Types"
                                             ),selected = "Unselected"),
                                 selectInput(inputId = "Production_Method",
                                             label = h3("Production Method:"),
                                             choices = list("Unselected"="Unselected",
                                                            "Live Action"="Live Action",
                                                            "Digital Animation"="Digital Animation",
                                                            "Animation/Live Action"="Animation/Live Action",
                                                            "Hand Animation"="Hand Animation",
                                                            "Stop-Motion Animation"="Stop-Motion Animation",
                                                            "Multiple Production Methods"="Multiple Production Methods"
                                             ),selected = "Unselected"),
                                 textInput(inputId = "text",
                                           label = h3("Keyword input: (One Word)"))
                             ),
                             mainPanel(
                                 tableOutput("recommend")
                             )
                         ))
)

server = function(input,output){
    data = read.csv("movie.csv")
    dataset = reactive({
        temp = read.csv("movie.csv")
        if(input$genre != "All"){
            temp = filter(temp,Genre == input$genre)
        }
        if(input$source!="All"){
            temp = filter(temp,Source==input$source)
        }
        if(input$creative!="All"){
            temp = filter(temp,Creative_Type==input$creative)
        }
        if(input$prod_method!="All"){
            temp = filter(temp,Production_Method==input$prod_method)
        }
        if(input$run_time!=0){
            temp  = filter(temp,Running_Time>=input$run_time)
        }
        if(input$franchise!="Unselected"){
            temp = filter(temp,Franchise==as.numeric(input$franchise))
        }
        if(input$budget!=0){
            temp = filter(temp,ProductionBudget>=input$budget)
        }
        if(input$open!=0){
            temp = filter(temp,OpeningWeekend>=input$open)
        }
        if(input$doffice!=0){
            temp = filter(temp,DomesticBox.Office>=input$doffice)
        }
        if(input$wwoffice!=0){
            temp = filter(temp,WorldwideBox.Office>=input$wwoffice)
        }
        
        temp = temp[,c(1:15) %in% input$table_col]
        temp
    })
    #Data Visualization Stuff
    bar_output = reactive({
        switch(input$bar,
               "MPAA_Rating" = ggplot(data,aes(x= MPAA_Rating))+geom_bar(),
               "Production_Method" = ggplot(data,aes(x= Production_Method))+geom_bar()+theme(axis.text.x=element_text(angle=90,margin = margin(1, unit = "cm"),vjust =1)),
               "Genre" = ggplot(data,aes(x= Genre))+geom_bar()+theme(axis.text.x=element_text(angle=90,margin = margin(1, unit = "cm"),vjust =1)),
               "Source" = ggplot(data,aes(x= Source))+geom_bar()+theme(axis.text.x=element_text(angle=90,margin = margin(1, unit = "cm"),vjust =1)),
               "Creative_Type" = ggplot(data,aes(x= Creative_Type))+geom_bar()+theme(axis.text.x=element_text(angle=90,margin = margin(1, unit = "cm"),vjust =1))
        )
    })
    
    word_output = reactive({
        switch(input$word,
               "Title" = wordcloud(data$Title,min.freq = input$minfreq,max.words = input$max,ordered.colors = TRUE,scale = c(8,.3)),
               "Keywords" = wordcloud(data$Keywords,min.freq = input$minfreq,max.words = input$max,ordered.colors = TRUE,scale = c(8,.3)))
    })
    output$movie_data = renderDataTable({
        dataset()
    })
    output$bar_graph = renderPlot({
        bar_output()
    })
    
    output$wordcloud = renderPlot({
        word_output()    
    })
    
    movie_dummy <- read.csv("movie_dummy.csv")
    movie = read.csv("movie.csv")
    output$DomesticBox=renderPlot({
        ggplot(data=movie_dummy,aes(x=DomesticBox.Office, y=WorldwideBox.Office)) +
            geom_point(size=0.2)  
    })
    output$OpeningWeekend=renderPlot({
        ggplot(data=movie_dummy, aes(x=OpeningWeekend, y=WorldwideBox.Office)) +
            geom_point(size=0.3,na.rm=T) 
    })
    output$Month=renderPlot({
        ggplot(data=movie, aes(x=month, y=WorldwideBox.Office)) +
            geom_boxplot()
    })
    output$Year=renderPlot({
        ggplot(data=movie_dummy, aes(x=Year, y=WorldwideBox.Office)) +
            geom_jitter(size=0.3,na.rm=T) +
            xlim(1990,2021)
    })
    genre=reactive({
        movie$Genre=ifelse(movie$Genre%in%c('Black Comedy','Concert/Performance','Documentary','Musical','Western'),"etc",as.character(movie$Genre))
        movie
    })
    output$Genre=renderPlot({
        ggplot(data=genre(), aes(x=Genre, y=WorldwideBox.Office)) +
            geom_boxplot()
    })
    output$Creative_Type=renderPlot({
        ggplot(data=movie,aes(x=Creative_Type, y=`WorldwideBox.Office`)) +
            geom_boxplot()
    })
    movie_source=reactive({
        movie=movie[movie$Source==c("Based on Comic/Graphic Novel","Based on Factual Book/Article","Based on Fiction Book/Short Story","Based on Folk Tale/Legend/Fairytale","Based on Real Life Events","Based on TV","Original Screenplay","Remake"),]
        movie
    })
    output$Source=renderPlot({
        ggplot(data=movie_source(), aes(x=Source, y=WorldwideBox.Office)) +
            geom_boxplot() +
            theme(axis.text.x = element_text(angle=60,hjust=1))
    })
    output$Production_Method=renderPlot({
        ggplot(data=movie, aes(x=Production_Method,y=WorldwideBox.Office)) +
            geom_boxplot()
    })
    tgenre=reactive({
        movie[movie$Year%in% c(1930:2000),]$Year="1930~2000"
        movie[movie$Year%in% c(2001:2010),]$Year="2001~2010"
        movie[movie$Year%in% c(2011:2020),]$Year="2011~2020"
        movie
    })
    output$tGenre=renderPlot({
        ggplot(data=tgenre(), aes(x=Genre,y=WorldwideBox.Office)) +
            geom_bar(stat="identity") +
            facet_wrap(~Year) +
            theme(axis.text.x=element_text(angle=60,hjust=1))
    })
    output$tCreative_type=renderPlot({
        ggplot(data=tgenre(), aes(x=Creative_Type,y=WorldwideBox.Office)) +
            geom_bar(stat="identity") +
            facet_wrap(~Year) +
            theme(axis.text.x=element_text(angle=60,hjust=1))
    })
    
    
    #Data Analysis Stuff
    movie = read.csv("movie.csv")
    #change the row name
    row.names(movie)=movie$X
    #delete the first column
    movie=movie[,-1]
    #delete the unuseful and unappropriate variables
    movie=movie[,c(-1,-2,-4,-5,-12,-14,-15)]
    #delete the observations that has NA values
    movie=na.omit(movie)
    #transformation of the data
    minmax=function(x,shft=0.5){(x-min(x))/(max(x)-min(x))+shft}
    fit=reactive({
        min=input$Year[1]
        max=input$Year[2]
        movie=movie[min<=movie$Year&movie$Year<=max,]
        movie$Year=minmax(movie$Year)^11
        fit=lm(`WorldwideBox.Office`~.,data=movie)
    })
    #First transform the variables like the way I did in the Data Analysis.Rmd and then fit the data 
    output$FitStatistics=renderPrint({
        summary(fit())
    })
    #Shows the qqplot. Please refer to the "Data Analysis.Rmd"
    output$qqplot=renderPlot({
        
        ggplot(data.frame(res = fit()$residuals), aes(sample = res)) +
            stat_qq() + stat_qq_line()
    })
    #Shows the bptest. Please refer to the "Data Analysis.Rmd"
    output$bptest=renderPrint({
        bptest(fit())
    })
    #Shows the cooks distance. Please refer to the "Data Analysis.Rmd"
    output$cooks_distance=renderPlot({
        plot(1:length(fit()$residuals), cooks.distance(fit()))
    })
   
    #Recommendation Tab Output
    #Shows the movie data in descending order based on the value user choose 
    output$recommend = renderTable({
        x = data
        if(input$Genre!="Unselected"){
            x=data[data$Genre==input$Genre,] 
        }
        if(input$Creative_Type!="Unselected"){
            x=x[x$Creative_Type==input$Creative_Type,]
        }
        if(input$Production_Method!="Unselected"){
            x=x[x$Production_Method==input$Production_Method,]
        }
        if(!is.na(input$text)){
            s=paste0(toupper(substr(input$text,1,1)),substr(input$text,2,100))
            x=x[str_detect(x$Keywords,s),]
        }
        z=x[!is.na(x$Title),]
        z=z[,c(3,7)]
        z[order(z$`WorldwideBox.Office`,decreasing = TRUE),]
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)

