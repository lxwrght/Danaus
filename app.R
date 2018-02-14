## Author: A.D. Wright
## Project: Monarch example - Girls Math and Science Day 2018

#########
## Part - Working directory, data, packages
#########

## Set your working directory

## Load Monarch data
monarchs <- read.csv('monarchs_shiny.csv')

## Packages
  #Shiny
if(!require(shiny)) {install.packages("shiny");require(shiny)}
  #ggplot2
if(!require(ggplot2)) {install.packages("ggplot2");require(ggplot2)}
  #ggfortify
if(!require(ggfortify)) {install.packages("ggfortify");require(ggfortify)}
  #ggthemes
if(!require(ggthemes)) {install.packages("ggthemes");require(ggthemes)}
  #shinythemes
if(!require(shinythemes)) {install.packages("shinythemes");require(shinythemes)}


#########
## Part - Data Manipulation & Modeling
#########

#Growth Rate
#Calculated via monarch_jags.R   N[y] ~ dpois(r*N[y-1])
growthRate <- 0.963 

# Estimated slope values for: growthRate <- int + alpha*milk + beta*nec + gamma*log
int <- growthRate
alpha <- 0.02713
beta <-  0.01023  
gamma <- -0.01456
deltaU <- 0.02   
deltaD <- -0.0175   

## Monarch data
monData <- data.frame(N =monarchs$Numbers, Y=monarchs$X)
monData$P <- as.factor(rep(1,23))

#########
## Part - SHINY APP
#########

##
#### CSS Edits (should create a seperate file in /www for 'styles.css' instead of putting it here)
##

sliderCSS <- "
.irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
background: DarkSlateGrey;
border-color: DarkSlateGrey;
}
"

plotCSS <- "
.shiny-plot-output{
box-shadow: 1px 1px 10px grey;
}
"

##
#### UI
##


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"), #united , cerulean

##CSS Arguments
 tags$style(sliderCSS),
#Panels
  #Title
    # Application title
    #titlePanel(title="Monarchs & Math"),
    titlePanel(title=div("'Monarchs & Math' Module",img(src="ZQEL-butterfly-mich.png",height=50))), 
  #Slide bars
     # Sidebar with a slider input for number of bins 
     sidebarLayout(
        sidebarPanel(
          
        h3('See the Future!'),  
        helpText("Hit the submit button to predict future Monarch abundances and estimate the predicted popualtion growth rate."),
        #Submit Button
        actionButton("go","Submit"),
        br(),   
        br(),           
        textOutput('growth_rate'),  
        br(),
        textOutput('pop_size'),  
        h3('Manage!'),
        helpText("Try altering covariate values to 'manage' monarch habitat across its' range. Re-hit the Submit Button. "),
        #Milkweed 
        sliderInput("milk",
                       "Relative Amount of Summer Milkweed Habitat",
                       min = 1,
                       max = 4,
                       value = 2),
        #Nectar
        sliderInput("nectar",
                       "Relative Amount of Migratory Nectar Habitat",
                       min = 1,
                       max = 4,
                       value = 2),
        #Logging
        sliderInput("log",
                    "% of Wintering Grounds Available for Logging",
                    min = 1,
                    max = 10,
                    value = 5),
        #Temp   
        h3('Climate Scenarios:'),
        sliderInput("temp",                          
                    "Temperature Anomaly (Celsius)", 
                    min = -2.0,                        
                    max = 2.0,                         
                    value = 0)                       
        # #Submit button
        # submitButton("Submit"),
           
        ), #End sidebarPanel

  #Plot      
        # Show a plot of the generated distribution
        mainPanel(
          #img(src="ZQEL-Horiz-mich.png",height=50,align='right'),
          h4("How Does the Future Look?"),
          #helpText("This figure shows the estimated annual abundance of monarchs at their wintering grounds over the past 23 years, and shows predictions over the next 20 years. Background colors correspond to: Green - Least Concern, Light Red - At Risk, Red - Extinct."),
          tags$style(plotCSS),
          plotOutput("abundancePlot",width='100%'),
          h4("How Uncertain Are We?"),
          #helpText('The dotted blue line represents the estimated Monarch abundance in 2043, and the blue curve represents what we should expect to see and its corresponding uncertainty.'),
          plotOutput("probabilityPlot",width='100%'),
          br(),
          helpText("The top figure,'How Does the Future Look?', shows the previously estimated (1994-2016) Monarch abundances in their wintering grounds and our predicted abundances in the future (2017-2036) given the covaraite values set in the sliders bars in the left panel. Background colors correspond to the status of the population (Dark Red = Extinct, Light Red = At risk, Green = Viable)."),
          helpText("The lower figure, 'How Uncertain Are We?', illustrates the uncertainty in our ability to predict the future. The dotted blue line represents the estimated Monarch abundance in 2043 in that single iteration, and the blue curve represents all possible values (x-axis) and their probability of occuring (y-axis). The dotted gray line indicates our target population size.")
          ) #End Main Panel
     ) #End sidebar layout
  ) #End UI script



##
#### SERVER
##


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  ##
  #### Predict data using inputs from ui.R
  ##    
  
  # N <- c(monarchs$N, rep(NA,20))
  # years <- 1:length(N)  
  # predictions <- data.frame(N, years)
  
  v <- reactiveValues(pred=NULL,r=NULL,static=NULL)
  
  observeEvent(input$go,{
    
    #Eqs
    v$r <- int + alpha*input$milk + beta*input$nectar + gamma*input$log + deltaU*input$temp + deltaD*(input$temp^2)
    v$r <- round(v$r, 2)
    #Empty data frames for loops below
    n <- c(monData$N[23], rep(NA,20))
    nStatic <- c(monData$N[23], rep(NA,20))
    
    #with stochasticity
    for(i in 2:length(n)){
      if(n[i-1] <= 0) {
        n[i] = 0
      } else{
        n[i] <- rpois(1,n[i-1]*v$r) + rnorm(1,0,20000000)
      }
    }
    

    
    for(i in 1:length(n)){
      if(n[i] < 0) {
        n[i] = 0
      }
    }
    
    #Buidling data ouput
    years <- 21 + 1:length(n)
    N <- n[2:21]
    Y <- 2017:2036
    P <- as.factor(rep(2,20))
    #Final data ouput
    v$pred <- data.frame(N=N,Y=Y,P=P) 
    
    
    #withOUT stochasticity
    for(i in 2:length(nStatic)){
      if(nStatic[i-1] <= 0) {
        nStatic[i] = 0
      } else{
        nStatic[i] <- rpois(1,nStatic[i-1]*v$r) 
      }
    }
    
    #Buidling data ouput
    Nstatic <- nStatic[2:21]
    #Final data ouput
    v$static <- data.frame(N=Nstatic,Y=Y,P=P) 
    
 }) # eventReactive()
  
  #PLOT 1
  output$abundancePlot <- renderPlot({
    
    allData <- rbind(monData, v$pred)
    allData$N <- allData$N/1000000
    cols <- c("LINE1"="#f04546","LINE2"="#3591d1")
    
    ##PLOT
      ggplot(data=allData, aes(x=Y, y=N)) + #, color=P
        geom_rect(aes(ymin=-Inf,ymax=1, xmin=-Inf,xmax=Inf), fill='red', alpha=0.01) +
        geom_rect(aes(ymin=1,ymax=150, xmin=-Inf,xmax=Inf), fill='red', alpha=0.005) +
        geom_rect(aes(ymin=150,ymax=Inf, xmin=-Inf,xmax=Inf), fill='green3', alpha=0.005) +
        geom_point(size=3, color='blue',shape=1) +
        geom_line(size=0.5,linetype="dashed", color='blue') +
        geom_vline(xintercept=2016.5, linetype="dotted", size=1) + 
        ylab('Abundance (in Millions)') + xlab('Year') + xlim(1994,2036) + ylim(0,500) +
        theme(axis.text.y=element_text(color='black', size=14), 
               axis.text.x=element_text(color='black', size=14), 
               axis.title.y=element_text(size=18),
               axis.title.x=element_text(size=18),
               legend.position="top") +

        scale_linetype_manual(values = c("solid","dashed"),name="Equations", 
                              labels = c("Equation 1","Equation 2"),guide="legend")
        
   }) #, height=400, width=600) #end renderPlot()
  
  #PLOT 2
  output$probabilityPlot <- renderPlot({
    
    allDataS <- rbind(monData, v$static)
    allDataS$N <- allDataS$N/1000000
    
    allData <- rbind(monData, v$pred)
    allData$N <- allData$N/1000000
    
    ##PLOT
    ggdistribution(dnorm, seq(0:500), mean=allDataS$N[43], sd=60, colour='blue', fill='blue',p=NULL) +
        geom_vline(xintercept=allData$N[43], linetype="dashed", size=0.75, color='blue4') + 
        geom_vline(xintercept=150, linetype="dashed", size=0.75, color='grey75') +
        labs(x='Abundance (in Millions)',y='Year') + 
        #theme_tufte(ticks=FALSE, base_size = 14, base_family = 'calibri') +  
        theme(axis.text.y=element_text(color='black', size=14), 
             axis.text.x=element_text(color='black', size=14), 
             axis.title.y=element_text(size=18),
             axis.title.x=element_text(size=18)
       ) 

  }) #, height=400, width=600) #end renderPlot()

  output$growth_rate <- renderText({
    paste("Population Growth Rate = ",v$r)
  }) #end renderText
  
  output$pop_size <- renderText({
    paste("Population Size (2036) = ",round(v$pred[20,1]/1000000))
  }) #end renderText
  

  
} #end server()

# Run the application 
shinyApp(ui = ui, server = server)


