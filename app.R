library(rvest)
library(dplyr)
library(reshape2)
library(googleVis)
#if (!require("DT")) devtools::install_github("rstudio/DT")
library(DT)
library(shiny)
library(stringr)

# Ipeds donation analysis -------------------------------------------------

"%ni%" <- Negate("%in%") 

d11 <- read.csv('2011.csv')
d12 <- read.csv('2012.csv')
d13 <- read.csv('2013.csv')

d11A <- read.csv('2011A.csv')
d11A$year <- 2011

d12A <- read.csv('2012A.csv')
d12A$year <- 2012

d13A <- read.csv('2013A.csv')
d13A$year <- 2013

# preserve lat and lon 
lat_lon <- c('unitid', 'institution.name', 'HD2013.Latitude.location.of.institution', 'HD2013.Longitude.location.of.institution')
lat_lon <- d13[lat_lon]
d13$HD2013.Latitude.location.of.institution <- NULL
d13$HD2013.Longitude.location.of.institution <- NULL

colnames(d11) <- c('id', 'inst', 'year', 'total_private_gifts', 'pct_revenue_from_tuition', 'endowment_per_fte')
colnames(d12) <- c('id', 'inst', 'year', 'total_private_gifts', 'pct_revenue_from_tuition', 'endowment_per_fte')
colnames(d13) <- c('id', 'inst', 'year', 'total_private_gifts', 'pct_revenue_from_tuition', 'endowment_per_fte')

data_ipeds <- rbind(d11, d12, d13)

colnames(d11A) <- c("id",                                                                  
"inst",                                                                       
"long_term_investments",                                                    
"property_plant_and_equipment",          
"intangible_assets",                       
"total_assets",                                                             
"total_liabilities",                                                        
"debt_related_to_property",                           
"total_unrestricted_net_assets",                                            
"total_restricted_net_assets",                                              
"permanently_restricted_net_assets",
"temporarily_restricted_net_assets",                                        
"total_net_assets",                                                         
"total_revenues_and_investment_return",                                     
"total_expenses",                                                           
"other_changes_in_net_assets",                                     
"total_change_in_net_assets",                                               
"net_assets__beginning_of_the_year",                                        
"adjustments_to_beginning_of_year_net_assets",                              
"net_assets__end_of_the_year",                                              
"investment_return_total",                                                
"year")    

colnames(d12A) <- c("id",                                                                  
"inst",                                                                       
"long_term_investments",                                                    
"property_plant_and_equipment",          
"intangible_assets",                       
"total_assets",                                                             
"total_liabilities",                                                        
"debt_related_to_property",                           
"total_unrestricted_net_assets",                                            
"total_restricted_net_assets",                                              
"permanently_restricted_net_assets",
"temporarily_restricted_net_assets",                                        
"total_net_assets",                                                         
"total_revenues_and_investment_return",                                     
"total_expenses",                                                           
"other_changes_in_net_assets",                                     
"total_change_in_net_assets",                                               
"net_assets__beginning_of_the_year",                                        
"adjustments_to_beginning_of_year_net_assets",                              
"net_assets__end_of_the_year",                                              
"investment_return_total",                                                
"year") 

colnames(d13A) <- c("id",                                                                  
"inst",                                                                       
"long_term_investments",                                                    
"property_plant_and_equipment",          
"intangible_assets",                       
"total_assets",                                                             
"total_liabilities",                                                        
"debt_related_to_property",                           
"total_unrestricted_net_assets",                                            
"total_restricted_net_assets",                                              
"permanently_restricted_net_assets",
"temporarily_restricted_net_assets",                                        
"total_net_assets",                                                         
"total_revenues_and_investment_return",                                     
"total_expenses",                                                           
"other_changes_in_net_assets",                                     
"total_change_in_net_assets",                                               
"net_assets__beginning_of_the_year",                                        
"adjustments_to_beginning_of_year_net_assets",                              
"net_assets__end_of_the_year",                                              
"investment_return_total",                                                
"year") 

data_ipeds_A <- rbind(d11A, d12A, d13A)
data_ipeds_A$inst <- NULL

data_ipeds <- merge(data_ipeds, data_ipeds_A, by=c('id', 'year'))

data_ipeds$intangible_assets <- NULL
data_ipeds$other_changes_in_net_assets <- NULL
data_ipeds$adjustments_to_beginning_of_year_net_assets <- NULL

nescac <- c(
'Amherst College',
'Bates College',
'Bowdoin College',
'Colby College',
'Connecticut College',
'Hamilton College',
'Middlebury College',
'Trinity College',
'Tufts University',
'Wesleyan University',
'Williams College')

nescac_data <- subset(data_ipeds, data_ipeds$inst %in% nescac)

var.defs <- read.csv('var_defs.csv')
var.defs$name <- as.character(var.defs$name)
var.defs$name <- str_trim(var.defs$name, side = "both")
var.defs$def <- as.character(var.defs$def)



# Shiny Application -------------------------------------------------------

server <- function(input, output) {
  
  
# Colby donation analysis -------------------------------------------------

webpage <- html("http://www.colby.edu/alumni_parents_cs/customcf/imod_participation.cfm")

# read in data from colby website 
data <-
do.call("rbind", sapply(1:9, FUN = function(x) 
                                    webpage %>%
                                    html_nodes("table") %>%
                                    .[[x]] %>%
                                    html_table(fill = TRUE),  
                             simplify = FALSE))

# clean data 
c_names <- c('Class', 'Alumni', 'Goal_Participation', 
                    'Current_Participation', 'Goal_Donors', 'Current_Donors', 
                    'Remaining_Donors', 'Goal_Dollars', 'Gifts_Current',
                    'Pledges', 'Total_Commitments', 'Dollars_to_Goal')

#clean up colnames 
colnames(data) <- c_names
data <- data[c_names]
data <- subset(data, data$Class!='Class')

# convert all variables to factors from characters 
data <- as.data.frame(lapply(data, as.factor))

# preserve formatted data for table
table <- data

colnames(table) <- c('Class', 'Almuni', 'Participation Goal', 
                    'Current Participation', 'Goal Donors', 'Current Donors', 
                    'Remaining Donors', 'Goal Dollars', 'Gifts Current',
                    'Pledges', 'Total Commitments', 'Dollars to Goal')

# convert currency variables to numeric 
currency_to_numeric <- function(x){
  x <- as.character(x)
  x <- gsub('\\$','',x)
  x <- gsub('\\,','',x)
  x <- as.numeric(x)
  return(x)
}

data$Goal_Dollars <- currency_to_numeric(data$Goal_Dollars)
data$Gifts_Current <- currency_to_numeric(data$Gifts_Current)
data$Pledges <- currency_to_numeric(data$Pledges)
data$Total_Commitments <- currency_to_numeric(data$Total_Commitments)
data$Dollars_to_Goal <- currency_to_numeric(data$Dollars_to_Goal)


# convert percent variables to numeric 
percent_to_numeric <- function(x){
  x <- as.character(x)
  x <- gsub('\\%','',x)
  x <- as.numeric(x)
  return(x)
}

data$Goal_Participation <- percent_to_numeric(data$Goal_Participation)
data$Current_Participation <- percent_to_numeric(data$Current_Participation)

# convert remaining factors to numeric 
data$Class <- as.numeric(as.character(data$Class))
data$Alumni <- as.numeric(as.character(data$Alumni))
data$Current_Donors <- as.numeric(as.character(data$Current_Donors))
data$Remaining_Donors <- as.numeric(as.character(data$Remaining_Donors))
data$Goal_Donors <- as.numeric(as.character(data$Goal_Donors))

# create properly ordered class year factor 
data <- data[order(data$Class),]
data$Class <- as.factor(data$Class)

table_names <- names(table)
table_names <- table_names[table_names != "Class"]

# names crosswalk 
c_walk <- as.data.frame(cbind(data = colnames(data), table = colnames(table)))
  
  
output$var = renderUI({
      selectInput('var', strong('Select Variable'), table_names)
    })

output$vardef = renderUI({
      selectInput('var.def', strong('Variable Definition List'), var.defs$name, width='400px')
    })  
  
  
def_var <- reactive({
          as.character(var.defs$def[var.defs$name==as.character(input$var.def)])
   
  })
  
output$definition <- renderText({
    def_var()
  })
  
graph_var <- reactive({
            as.character(c_walk$data[c_walk$table==input$var])  
  })

 output$summary <- renderPrint({
    graph_var()
  })
  
# data table
output$tbl = DT::renderDataTable({
      DT::datatable(table)
    })
  

# graph
  output$line <- renderGvis({
                         gvisLineChart(data, xvar = "Class", yvar = graph_var(),
                                 options = list(hAxis="{title: 'Class Year'}",
                                                #vAxis="{title: 'input$var'}",
                                                #series="[{color: '#8F8461'}, {color: '#E44A28'}]",
                                                #title='Colby College Alumni Donation Data',
                                                chartid="ZoomZoom",
                                                crosshair="{trigger:'both'}",
                                                width = 1500,
                                                height = 600,
                                                legend="none",
                                      focusTarget = 'category')
    )
})
    
output$motion <- renderGvis({
    
    gvisMotionChart(nescac_data, 
                       idvar="inst", 
                       timevar="year",
                       options=list(width = 1500, height = 800,
                         state='{"dimensions":{"iconDimensions":["dim0"]},"playDuration":15000,"orderedByX":false,"xZoomedIn":false,"nonSelectedAlpha":0.4,"orderedByY":false,"yZoomedDataMax":64659575,"xAxisOption":"_TIME","colorOption":"_UNIQUE_COLOR","xLambda":1,"yZoomedDataMin":10784497,"xZoomedDataMax":1356998400000,"yAxisOption":"3","yZoomedIn":false,"iconKeySettings":[{"key":{"dim0":"Colby College"},"LabelY":-113,"trailStart":"2011","LabelX":-43}],"uniColorForNonSelected":false,"iconType":"BUBBLE","showTrails":true,"yLambda":1,"xZoomedDataMin":1293840000000,"time":"2011","sizeOption":"_UNISIZE","duration":{"multiplier":1,"timeUnit":"Y"}};')
)
  
  
  
  })
}



ui <- shinyUI(fluidPage(
  
  titlePanel("Colby College and NESCAC Fundraising Analysis"),
  
tabsetPanel(type = "tabs",
tabPanel("Colby",  
  
   fluidRow(
   column(2, includeHTML("colby.html"), br(), uiOutput('var')),
   column(10, htmlOutput("line"))),

  fluidRow(
  column(12, DT::dataTableOutput('tbl'))),
  
  fluidRow(
  column(12, includeHTML("rich.html")))
), 
  
  
tabPanel("NESCAC",
   fluidRow(
   column(8, htmlOutput("motion")),
   column(3, offset = 1, includeHTML("help.html"), 
                         uiOutput('vardef'),
                         textOutput('definition'), 
                         br(), 
                         includeHTML("rich.html"))))
    
  )))
  

shinyApp(ui = ui, server = server)

