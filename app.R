## Author: A.D. Wright
## Project: Monarch example - Girls Math and Science Day 2018

#########
## Part - Working directory, data, packages
#########

## Set your working directory
  #Currently set via Danaus.Rproj
#test test

## Load Monarch data
  ## This data is publicy available via the World Wildlife Fund, we used a correction factor to translate hectares to abundance [citation?]
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

# PopulationGrowth Rate
  #Calculated via monarch_jags.R   N[y] ~ dpois(r*N[y-1])
growthRate <- 0.963 

# Parameter values for growthrate linear model: r <- int + alpha*milk + beta*nec + gamma*log + deltaU*temp + deltaU*(temp^2)
int <- growthRate
alpha <- 0.02713
beta <-  0.01023  
gamma <- -0.01456
deltaU <- 0.02   
deltaD <- -0.0175   

## Monarch dataframe necessary for shiny app
monData <- data.frame(N=monarchs$Numbers, Y=monarchs$X)
monData$P <- as.factor(rep(1,23))

#########
## Part - SHINY APP
#########

##
#### CSS Edits (should create a seperate file in /www for 'styles.css' instead of putting it here - eventually...)
##

## Edits the color of the sliderBar
sliderCSS <- "
.irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
background: DarkSlateGrey;
border-color: DarkSlateGrey;
}
"

## Adds a shadow around the plots
plotCSS <- "
.shiny-plot-output{
box-shadow: 1px 1px 10px grey;
}
"

##
#### UI file
##


# Define UI for the app
ui <- fluidPage(theme = shinytheme("cerulean"), #united , cerulean

##CSS Arguments
 tags$style(sliderCSS),
 tags$style(plotCSS),
#Panels
  #Title
    titlePanel(title=div("'Monarchs & Math' Module",img(src="ZQEL-butterfly-mich.png",height=50))), 
  #Slider bars
     # Sidebar with a slider inputs & submit button
     sidebarLayout(
        sidebarPanel(
          
        h3('See the Future!'),  
        helpText("Hit the submit button to predict future Monarch abundances and estimate the predicted popualtion growth rate."),
        #Submit Button
        actionButton("go","Submit"),
        br(),   
        br(),   
        #Reported values for growth rate and final population size (see server())
        textOutput('growth_rate'),  
        br(),
        textOutput('pop_size'),  
        h3('Manage!'),
        helpText("Try altering covariate values to 'manage' monarch habitat across its range. Re-hit the Submit Button. "),
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
           
        ), #End sidebarPanel

  #Plot      
        # Show plots in main panel
        mainPanel(
          #img(src="ZQEL-Horiz-mich.png",height=50,align='right'),
          h4("How Does the Future Look?"),
          #helpText("This figure shows the estimated annual abundance of monarchs at their wintering grounds over the past 23 years, and shows predictions over the next 20 years. Background colors correspond to: Green - Least Concern, Light Red - At Risk, Red - Extinct."),
          plotOutput("abundancePlot",width='100%'),
          h4("How Uncertain Are We?"),
          #helpText('The dotted blue line represents the estimated Monarch abundance in 2043, and the blue curve represents what we should expect to see and its corresponding uncertainty.'),
          plotOutput("probabilityPlot",width='100%'),
          br(),
          helpText("The Top Figure, 'How Does the Future Look?', shows the estimated (1994-2016) Monarch abundances in their wintering grounds (left of dashed line), and our predicted abundances (right of dashed line) in the future (2017-2036) given the covariate values set in the slider bars in the left panel. Background colors correspond to the status of the population (Dark Red = Extinct, Light Red = At Risk, Green = Viable) and the abundance values are indidcated by blue circles."),
          helpText("The Lower Figure, 'How Uncertain Are We?', illustrates the uncertainty in our ability to predict the future. The dotted blue line represents the estimated Monarch abundance in 2036 in that single iteration, and the blue curve represents all possible values (x-axis) and their probability of occurring (y-axis). The dotted gray line indicates our target population size (Viable).")
          
          ) #End Main Panel
     ) #End sidebar layout
  ) #End UI script


##
#### SERVER
##


# Define server logic for ouputs
server <- function(input, output) {
   
  ##
  #### Predict data using inputs from ui.R
  ##    
  
  
  #Define and calculate the reactive values for each iteration
  v <- reactiveValues(pred=NULL,r=NULL,static=NULL)
  
  #Submit button
  observeEvent(input$go,{
    
    #Eqs
    v$r <- int + alpha*input$milk + beta*input$nectar + gamma*input$log + deltaU*input$temp + deltaD*(input$temp^2)
    #Round for simpler output
    v$r <- round(v$r, 2)
    #Create empty data frames for loops below
      #For figure 1
    n <- c(monData$N[23], rep(NA,20))
      #For figure 2
    nStatic <- c(monData$N[23], rep(NA,20))
    
    #with stochasticity (Figure 1)
    for(i in 2:length(n)){
      if(n[i-1] <= 0) {
        n[i] = 0
      } else{
        n[i] <- rpois(1,n[i-1]*v$r) + rnorm(1,0,20000000)
      }
    }
    
    #The first negative value doesn't get set to 0 in above loop, this corrects that
    for(i in 1:length(n)){
      if(n[i] < 0) {
        n[i] = 0
      }
    }
    
    #Building data ouput
    years <- 21 + 1:length(n)
    N <- n[2:21]
    Y <- 2017:2036
    P <- as.factor(rep(2,20))
    #Final data ouput
    v$pred <- data.frame(N=N,Y=Y,P=P) 
    
    
    #withOUT stochasticity (Figure 2)
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
 
  
  ##
  #### PLOT OUTPUTS
  ##    
  
  #PLOT 1
  output$abundancePlot <- renderPlot({
    
    #Bind original data and predicted data
    allData <- rbind(monData, v$pred)
    #Put it on a (in millions) scale
    allData$N <- allData$N/1000000
      #Whis it this line in here? For Legend, but legend issn't working
        #cols <- c("LINE1"="#f04546","LINE2"="#3591d1")
    
    ##PLOT
      ggplot(data=allData, aes(x=Y, y=N)) + #, color=P
        geom_rect(aes(ymin=-Inf,ymax=1, xmin=-Inf,xmax=Inf), fill='red', alpha=0.01) +
        geom_rect(aes(ymin=1,ymax=150, xmin=-Inf,xmax=Inf), fill='red', alpha=0.005) +
        geom_rect(aes(ymin=150,ymax=Inf, xmin=-Inf,xmax=Inf), fill='green', alpha=0.005) +
        geom_point(size=3, color='blue',shape=1) +
        geom_line(size=0.5,linetype="dashed", color='blue') +
        geom_vline(xintercept=2016.5, linetype="dotted", size=1) + 
        ylab('Abundance (in Millions)') + xlab('Year') + xlim(1994,2036) + ylim(0,500) +
        theme(axis.text.y=element_text(color='black', size=14), 
               axis.text.x=element_text(color='black', size=14), 
               axis.title.y=element_text(size=18),
               axis.title.x=element_text(size=18),
               legend.position="top") #+

        # scale_linetype_manual(values = c("solid","dashed"),name="Equations", 
        #                       labels = c("Equation 1","Equation 2"),guide="legend")
        
   }) #, height=400, width=600) #end renderPlot()  #Can set height and width  here if you want plot size to be fixed, regardless of window size, however, doesn't work with box-shadow code as written
  
  #PLOT 2
  output$probabilityPlot <- renderPlot({
    
    #Bind original data and predicted data
    allDataS <- rbind(monData, v$static)
    allData <- rbind(monData, v$pred)
    #Put it on a (in millions) scale
    allDataS$N <- allDataS$N/1000000
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

  ##
  #### TEXT OUTPUTS
  ##
  
  output$growth_rate <- renderText({
    paste("Population Growth Rate = ",v$r)
  }) #end renderText
  
  output$pop_size <- renderText({
    paste("Population Size (2036) = ",round(v$pred[20,1]/1000000))
  }) #end renderText
  
  
} #end server()

##
#### Final fun() to run entire script
##

# Run the application 
shinyApp(ui = ui, server = server)


