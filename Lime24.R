library(shiny)
library(leaflet)
library(RColorBrewer)
library(geosphere)
setwd("/Users/Kiana/Desktop/SAMPLE/FridayFeb16th")
#datatotal<-read.csv("dockless_jump.csv", head = TRUE, sep = ",")
#limesub<-subset(datatotal, Operator == "LimeBike")
# for(i in 1:dim(limesub)[1]) {
#   limesub$hourinterval[i]<- as.numeric(substr(limesub$StartDate[i],12,13))
# }
#limedata<-data.frame(limesub$TripID,limesub$StartWard,limesub$EndWard,limesub$hourinterval)
#write.csv(limedata,'limedata.csv')
# limedata<-read.csv("limedata.csv",head = TRUE, sep = ",")
# subdata<-subset(limedata,limesub.hourinterval==1)
# tablesubdata<-with(subdata,table(subdata$limesub.StartWard,subdata$limesub.EndWard,useNA = "ifany"))
# dataframetable<-as.data.frame(tablesubdata)
# names(dataframetable)<-c("origin","destination","count")
# lonlat<-read.csv('long.csv',header=TRUE,sep=",")
# final<-merge(dataframetable,lonlat,by.x = c("origin"),by.y=c("ward.number"))
# final<-merge(final,lonlat,by.x= c("destination"),by.y=c("ward.number"))
# final<-subset(final,select = -c(WARD.x,WARD.y))
limedata<-read.csv("limedata.csv",head = TRUE, sep = ",")
ui <- fluidPage(
  #*Input() functions,
  leafletOutput("mymap"),
  p(),
  selectInput("Select", label = h3("Select time of day"), choices = list(
    "12AM-1AM" = 0, "1AM-2AM" = 1, "2AM-3AM" = 2, "3AM-4AM" = 3,
    "4AM-5AM" = 4, "5AM-6AM" = 5, "6AM-7AM" = 6, "7AM-8AM" = 7,
    "8AM-9AM" = 8, "9AM-10AM" = 9, "10AM-11AM" = 10, "11AM-12PM" = 11,
    "12PM-1APM" = 12, "1PM-2PM" = 13, "2PM-3PM" = 14, "3PM-4PM" = 15,
    "4PM-5PM" = 16, "5PM-6PM" = 17, "6PM-7PM" = 18, "7PM-8PM" = 19,
    "8PM-9PM" = 20, "9PM-10PM" = 21, "10PM-11PM" = 22, "11PM-12AM" = 23
    ), selected = NULL)
  
)
server <- function(input, output,session){
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      fitBounds(-77.106877, 38.864058, -76.961994, 38.973042)
  })
  
  filteredData <- reactive({if(is.null(input$Select) == FALSE  ) {
    subdata<-subset(limedata,limesub.hourinterval== as.character(input$Select))
    tablesubdata<-with(subdata,table(subdata$limesub.StartWard,subdata$limesub.EndWard,useNA = "ifany"))
    dataframetable<-as.data.frame(tablesubdata)
    names(dataframetable)<-c("origin","destination","count")
    lonlat<-read.csv('long.csv',header=TRUE,sep=",")
    final<-merge(dataframetable,lonlat,by.x = c("origin"),by.y=c("ward.number"))
    final<-merge(final,lonlat,by.x= c("destination"),by.y=c("ward.number"))
    final<-subset(final,select = -c(WARD.x,WARD.y))
    return(final)

    
  } else {
    final<- NULL
    return(final)
  }
  })
  
  observe({
    data = filteredData()
    if(is.null(data)) {
      leafletProxy("mymap") %>%
        clearShapes()
      
    } else {
    
    flows<-gcIntermediate(data[,4:5],data[,6:7],sp=TRUE,addStartEnd=TRUE)
    flows$counts<-data$count
    flows$origins<-data$origin
    flows$destinations<-data$destination
    hover <- paste0(flows$origins, " to ",
                    flows$destinations, ': ',
                    as.character(flows$counts))
    pal <- colorFactor(brewer.pal(9, 'Set1'), flows$origins)
    leafletProxy("mymap") %>%
      clearShapes()%>%
      addPolylines(data = flows, weight = ~counts/50,label = hover,
                   group = ~origins, color = ~pal(origins)) %>%
      addLayersControl(overlayGroups = unique(flows$origins),
                       options = layersControlOptions(collapsed = FALSE))
        }})}
     
   

shinyApp(ui = ui, server = server)