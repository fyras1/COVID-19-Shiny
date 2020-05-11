
library(leaflet)
library(shiny)
library(plotly)
require(openxlsx)
library(dplyr)
library(lubridate)


date<-Sys.Date()-1
URL<-paste0("http://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",date,".xlsx")
filename<-paste0("covid_",date,".xlsx")
if (!file.exists(filename))
    download.file(URL,
                  destfile=filename,
                  mode="wb")


covid<- read.xlsx(filename,sheet=1)
covid<-covid[complete.cases(covid),]
covid$date<-as.Date(paste0(covid$year,"-",covid$month,"-",covid$day))


countries<-read.csv("countries.csv")
countries$country<-as.character(countries$country)
#codes<-read.csv("data\\codes.csv" )
dfx<-merge(covid,countries,by.x="geoId",by.y="country")
#codes<-codes[,c(2,3)]

#combined<-merge(codes,countries,by.x="Alpha.2.code",by.y="country")

#dfx<-group_by(covid,countryCode=countryterritoryCode)

#dfx<-merge(df,combined[,c(2:5)],by.x="countryCode",by.y="Alpha.3.code")




df2<-countries[,c(1,4)]
df2<-df2[ order(df2[,2]), ]
a<-as.list(as.character(df2[,1]))
names(a)<-df2[,2]

a<-append(list("--Select All--"="ALL"),a)





ui<-fluidPage(
    
    titlePanel("COVID-19"),
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
                       tabPanel("Map",leafletOutput("map1")),
                        
                       tabPanel("Total Cases Graphs",plotlyOutput("graph")),
                       tabPanel("Daily cases Graphs",plotlyOutput("dgraph")),
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
                                h6("Will display a boxplot of the day-to-day data of the COVID-19 cases and/or deaths in the selected date interval and/or country.")
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
            
             dff<-covid_up()[,c("cases","deaths","date")]
             #dff<-dff[dff$cases>0,]
             
             if(input$cnt=="ALL")
                 dff<- dff %>% group_by(date) %>% summarise(cases=sum(cases),deaths=sum(deaths))
             
             dff$cases<-cumsum(dff$cases)
             dff$deaths<-cumsum(dff$deaths)
             
             if(input$chk1 && input$chk2)
             g<- dff %>% tidyr::gather(variable,value,-date) %>%
                 transform(id=as.integer(factor(variable))) %>%
                 plot_ly(x=~date,y=~value,color=~variable,colors=c("dodgerblue3","firebrick3")) %>%
                 add_lines()
             
             else if(!input$chk1 &&  input$chk2)
                 g<-dff %>% plot_ly(x=~date,y=~deaths,type="scatter",mode="lines",color = I('firebrick3'))
             else if(input$chk1 && !input$chk2)
                 g<-dff %>% plot_ly(x=~date,y=~cases, type="scatter",mode="lines",color = I('dodgerblue3')) 
             g
            #g<-plot_ly(covid_up()[covid_up()$cases>0,],x=~date,y=~cases,mode="lines")
            #g <- g %>% add_lines(y=~covid_up()[covid_up()$cases>0,"deaths"])
                                     
            #g
        })
        
        ## DAILY GRAPH
        output$dgraph<-renderPlotly({
            
            dff<-covid_up()[,c("cases","deaths","date")]
            dff<-dff[dff$cases>0,]
            
            if(input$cnt=="ALL")
                dff<- dff %>% group_by(date) %>% summarise(cases=sum(cases),deaths=sum(deaths))
          
            
            if(input$chk1 && input$chk2)
                g<- dff %>% tidyr::gather(variable,value,-date) %>%
                transform(id=as.integer(factor(variable))) %>%
                plot_ly(x=~date,y=~value,color=~variable,colors=c("dodgerblue3","firebrick3"), type="bar") 
                
            
            else if(!input$chk1 &&  input$chk2)
                g<-dff %>% plot_ly(x=~date,y=~deaths,color = I('firebrick3'),type="bar") 
            else 
                g<-dff %>% plot_ly(x=~date,y=~cases,color = I('dodgerblue3'),type="bar")
            g
            #g<-plot_ly(covid_up()[covid_up()$cases>0,],x=~date,y=~cases,mode="lines")
            #g <- g %>% add_lines(y=~covid_up()[covid_up()$cases>0,"deaths"])
            
            #g
        })
}

shinyApp(ui,server)