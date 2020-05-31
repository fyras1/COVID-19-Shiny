
library(leaflet)
library(shiny)
library(plotly)
require(openxlsx)
library(dplyr)
library(lubridate)
library(data.table)





date<-Sys.Date()-1
URL<-paste0("http://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",date,".xlsx")
filename<-paste0("covid_",date,".xlsx")
try(download.file(URL,
                  destfile=filename,
                  mode="wb"))


if(file.exists(filename))
{covid<- read.xlsx(filename,sheet=1) 
}   else {
  ## KI BESH T3AWED TDEPLOYI L APP BADEL L FILE HEDHA KAHAW
 covid<-read.xlsx("last_valid_file_29-05-2020.xlsx",sheet=1)
 date<-as.Date("2020-05-29")
 }

covid<-covid[complete.cases(covid),]
covid$date<-as.Date(paste0(covid$year,"-",covid$month,"-",covid$day))


countries<-read.csv("countries.csv")
countries$country<-as.character(countries$country)
dfx<-merge(covid,countries,by.x="geoId",by.y="country")



df2<-countries[,c(1,4)]
df2<-df2[ order(df2[,2]), ]
a<-as.list(as.character(df2[,1]))
names(a)<-df2[,2]

a<-append(list("United States"="US"),a)
a<-append(list("Tunisia"="TN"),a)
a<-append(list("--Select All--"="ALL"),a)



##################################################################
########################## User Inerface #########################


ui<-fluidPage(
    
    titlePanel("COVID-19 Data"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(c("dat"),
                        "Date interval:",
                        min = as.Date("2020-01-01","%Y-%m-%d"),
                        max = as.Date(date),
                        value=c(as.Date("2020-01-01"),as.Date(date)),
                        timeFormat="%Y-%m-%d"),
            selectInput("cnt","Country", a),
            checkboxInput("chk1","Display Cases",value=TRUE),
            checkboxInput("chk2","Display Deaths",value=TRUE)
            
        ),
        mainPanel(
            tabsetPanel(type="tabs",
                       tabPanel("Map",h4(textOutput("dat1")),leafletOutput("map1")),
                        
                       tabPanel("Total Cases Graphs",h3(textOutput("graph1")),plotlyOutput("graph"),br(),h3("Corresponding data table:"),br(), DT::dataTableOutput("tbl1")),
                       tabPanel("Daily cases Graphs",h3(textOutput("graph2")),plotlyOutput("dgraph"),br(),h3("Corresponding data table:"),br(),DT::dataTableOutput("tbl2")),
                       tabPanel("Documentation",
                                h4("Date slider"),
                                h6("Use the Date Slider to pick the date interval of the COVID data you're interested in."),
                                h4("Country selector"),
                                h6("Pick a country using the country selector, the map and the graphs will display data about that country only. You can select global data using --SELECT ALL-- from the menu."),
                                h4("Cases/deaths display checkboxes"),
                                h6("You can choose to display or hide the cases and/or deaths data from the map/graphs."),
                                h4("Map tab"),
                                h6("You can hover over the map to see total Cases and Deaths per country."),
                                h4("Total Cases Graphs tab"),
                                h6("Will display the cumulative sums of the COVID-19 cases and/or deaths in the selected date interval and/or country."),
                                h4("Daily Cases Graphs tab"),
                                h6("Will display a boxplot of the day-to-day data of the COVID-19 cases and/or deaths in the selected date interval and/or country."),br(),br(),br(),
                                h5("___________________________________________________"),
                                h5("created on 2020-05-11."),
                                h5("email: firas.ismail69@gmail.com"),
                                a("GitHub repository", href="https://github.com/fyras1/COVID-19-Shiny"),
                                h5("___________________________________________________"),
                                a("don't click :c", href="https://www.youtube.com/watch?v=dQw4w9WgXcQ")
                       )
                       
                              
           )
        )
    )
)



##################################################################
############################# SERVER #############################


server<- function(input, output) {
    
        
        
        dat<-reactive({
            x<-input$dat
            
            list("low"=x[1],"hi"=x[2])
        })
        
       
       
        output$dat1<-  renderText({ if(input$cnt=="ALL") 
                                        paste0("Distribution map of the total Cases/Deaths between ",as.character(dat()$low)," and ",as.character(dat()$hi)  )
                                    else
                                        paste0("Distribution map of the total Cases/Deaths between ",as.character(dat()$low)," and ",as.character(dat()$hi), " for ", names(a)[a==input$cnt][1])
                                            })
        output$graph1<-  renderText({ if(input$cnt=="ALL") 
                                        paste0("Graph of the cumulative Cases/Deaths between ",as.character(dat()$low)," and ",as.character(dat()$hi)  )
                                    else
                                        paste0("Graph of the cumulative Cases/Deaths between ",as.character(dat()$low)," and ",as.character(dat()$hi), " for ", names(a)[a==input$cnt][1])
                                            })        

        output$graph2<-  renderText({ if(input$cnt=="ALL") 
                                        paste0("Graph of the daily new Cases/Deaths between ",as.character(dat()$low)," and ",as.character(dat()$hi)  )
                                    else
                                        paste0("Graph of the daily new Cases/Deaths between ",as.character(dat()$low)," and ",as.character(dat()$hi), " for ", names(a)[a==input$cnt][1])
                                             })

        
        covid_up<-reactive({
            x<-dat()
            x1<-x$low
            x2<-x$hi

            
            covid_up<-dfx
            if(input$cnt!="ALL")
            {
                covid_up<-dfx[dfx$geoId==input$cnt,]  
            }
            
            
            covid_up<-covid_up[covid_up$date>=x1 & covid_up$date<=x2,]

            covid_up<-covid_up[order(covid_up$date),]
            covid_up
            
        })
        
        
        
        df<-reactive({
           
             df<-group_by(covid_up(),countryCode=countryterritoryCode) %>% summarise(cases=sum(cases),deaths=sum(deaths),population=mean(popData2018),lng=longitude[1], lat=latitude[1],name=name[1])

           df
            })
        
        
        output$map1<-renderLeaflet({
            
        
        map<-df()%>% 
            leaflet(options = leafletOptions(zoomControl = TRUE,minZoom = 1, maxZoom = 7)) %>% 
            addTiles() %>%
             
             addCircles(lng=df()$lng,lat=df()$lat, weight=1,radius=sqrt(df()$cases)*ifelse(input$chk1,2000,-1),label = paste0("Cases in ",df()$name," : ",format(df()$cases, big.mark=",")),color = "blue",labelOptions = labelOptions(interactive=TRUE,textsize="17px")) %>% 
             addCircles(lng=df()$lng,lat=df()$lat, weight=1,radius=sqrt(df()$deaths)*ifelse(input$chk2,2000,-1),label = paste0("Deaths in ",df()$name," : ",format(df()$deaths, big.mark=",")),color = "red",labelOptions = labelOptions(interactive=TRUE,textsize="17px")) 
        
        map
        
    })
        ## TOTAL (CUMULATIVE GRAPH)
        output$graph<-renderPlotly({
            
            dff<-covid_up()[,c("date","cases","deaths")]
            #dff<-dff[dff$cases>0,]
             
             if(input$cnt=="ALL")
                 dff<- dff %>% group_by(date) %>% summarise(cases=sum(cases),deaths=sum(deaths))
             
             
             
             
             dff$cases<-cumsum(dff$cases)
             dff$deaths<-cumsum(dff$deaths)
             
             dffr<-dff
             dffr<- dffr[seq(dim(dffr)[1],1),]
             dffr$date<-as.character(dffr$date)
             output$tbl1<-DT::renderDataTable(data.table(dffr))

             if(input$chk1 && input$chk2)
             g<- dff %>% tidyr::gather(variable,value,-date) %>%
                 transform(id=as.integer(factor(variable))) %>%
                 plot_ly(x=~date,y=~value,color=~variable,colors=c("dodgerblue3","firebrick3")) %>%
                 add_lines()
             
             else if(!input$chk1 &&  input$chk2)
                 g<-dff %>% plot_ly(x=~date,y=~deaths,type="scatter",mode="lines",color = I('firebrick3'))
             else 
                 g<-dff %>% plot_ly(x=~date,y=~cases, type="scatter",mode="lines",color = I('dodgerblue3')) 
             g
          
        })
        
        ## DAILY GRAPH
        output$dgraph<-renderPlotly({
            
            dff<-covid_up()[,c("date","cases","deaths")]

            if(input$cnt=="ALL")
                dff<- dff %>% group_by(date) %>% summarise(cases=sum(cases),deaths=sum(deaths))
            
            dffr<-dff
            dffr<- dffr[seq(dim(dffr)[1],1),]
            dffr$date<-as.character(dffr$date)
            output$tbl2<-DT::renderDataTable(data.table(dffr))
            

            if(input$chk1 && input$chk2)
                g<- dff %>% tidyr::gather(variable,value,-date) %>%
                transform(id=as.integer(factor(variable))) %>%
                plot_ly(x=~date,y=~value,color=~variable,colors=c("dodgerblue3","firebrick3"), type="bar") 
                
            
            else if(!input$chk1 &&  input$chk2)
                g<-dff %>% plot_ly(x=~date,y=~deaths,color = I('firebrick3'),type="bar") 
            else 
                g<-dff %>% plot_ly(x=~date,y=~cases,color = I('dodgerblue3'),type="bar")
            g
        
        })
}

shinyApp(ui,server)
