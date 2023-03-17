#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(parsnip)
library(tidymodels)
library(ggthemes)

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme('cyborg'),
    #setBackgroundImage(src = 'R.jpeg'),

    # Application title
    title = "Titanic Survival", 
    #titlePanel("Old Faithful Geyser Data"),
    
   
               tabPanel("Would I survive?",
                        column(width = 4, #style = "background-color:#bcbcbc;",
                               sliderInput("age",
                                           h3("Age:"),
                                           min = 1,
                                           max = 100,
                                           value = 30),
                               selectInput("pclass",
                                           "Class of Travel", choices=c(1,2,3)),
                               selectInput("emb",
                                           "Embarkation Point:",
                                           choices = c("Southampton","Queenstown","Cork")),
                               selectInput("sex",
                                           "Sex:",
                                           choices= c("Male","Female")),
                               actionBttn("btn","Go"),
                               br()
                               ),
                        column(width = 8,
                               h2("Your Odds of Survival are:"),
                               h3(textOutput("op"))
                               
                               )
                        
                        
                        ),
               tabPanel("Statistics 1",
                        "From looking at the sample data available you can clearly see the significant disadvantage male passengers had on the Titanic",
                        plotOutput("p1", width = '50%')
                        ),
               tabPanel("Statistics 2")
    
    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #         
    #         sliderInput("age",
    #                     "Age:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30),
    #         selectInput("pclass",
    #                     "Class of Travel", choices=c(1,2,3)),
    #         selectInput("emb",
    #                     "Embarkation Point:",
    #                     choices = c("Southampton","Queenstown","Cork")),
    #         selectInput("sex",
    #                     "Sex:",
    #                     choices= c("Male","Female"))
    #         
    #     )
        


        # Show a plot of the generated distribution
        # mainPanel(
        #    plotOutput("distPlot")
        #)
    #)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    mod <- readRDS('./mod.RDS')
    
    rec_obj <- read_rds('./rec.RDS')
    
    #test <- read_csv('./data/test.csv')
    
    tt <- read_csv('./data/test.csv') %>% filter(row_number()==0)
    trn <- read_csv('./data/train.csv') 
    

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
    
    output$op <- renderText({
        
        if (input$btn == 0)
            return()
        
        #input$btn
        
        #isolate({
            t=c(input$age,input$pclass, str_to_lower(input$sex),str_to_upper(str_sub(input$emb, start=1, end = 1)))
            #})
        print(t)
        t <- as_tibble(t(t))
        colnames(t) <- c("Age","Pclass","Sex","Embarked")
        t <- t %>% 
            mutate(Age = as.numeric(Age),
                   Pclass = as.numeric(Pclass))
        
        tt2 <- tt %>% bind_rows(t)   
        
        t_out <- tt2 %>% bind_cols(predict(mod, new_data=bake(rec_obj, new_data=tt2), type= 'prob')) %>% 
            select(.pred_1) %>% 
            pull()
        print(t_out)
        
        unlist(scales::label_percent()(t_out))
        
        
    })
    
    output$p1 <- renderPlot({
        p1 <- trn %>% 
            group_by(Sex) %>% 
            tally() %>% 
            as_tibble() %>% 
            left_join (trn %>% 
                           group_by(Sex) %>% summarise(survived=sum(Survived)) %>%  as_tibble()) %>% 
            pivot_longer(cols=c("n","survived")) %>% 
            
            ggplot(aes(x=Sex, y=value)) +
            geom_col(aes(group = name, fill = name), position = 'dodge')#+
            #geom_col(aes(y=survived), fill = 'blue') +
            #theme_hc(style = "darkunica")  + scale_fill_hc(palette="darkunica")
        
        p1 + theme(legend.position = 'none', axis.title.y = element_blank(),
                panel.background = element_rect(fill='transparent'), #transparent panel bg
                plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
                panel.grid.major = element_blank(), #remove major gridlines
                panel.grid.minor = element_blank(), #remove minor gridlines
                legend.background = element_rect(fill='transparent'), #transparent legend bg
                legend.box.background = element_rect(fill='transparent') #transparent legend panel
            ) +
            ggtitle("Survival Rates of Titanic Passengers by Sex") + scale_fill_hc(palette="darkunica")
    })
    
    session$onSessionEnded(function() {
        stopApp()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
