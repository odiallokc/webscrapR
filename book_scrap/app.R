#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(tidyverse, rvest, glue)

# Define UI for application that draws a histogram
ui <- fluidPage(

    column(12, tags$h2('Real-time web scraper with R')),
    
    # Application title
    titlePanel("Book List Scrapping App"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            width = 3,
            
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            
            sliderInput("nrow",
                        "Number of rows:",
                        min = 1,
                        max = 50,
                        value = 30),
            
            selectInput(
                inputId = 'genreSelect',
                label = 'Genre',
                choices = c('Business', 'Classics', 'Fiction', 'Horror', 'Music'),
                selected = 'Business',
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            width = 9,
            tableOutput('table1'),
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    
    output$table1 <- renderTable({
        mappings <- c('Business' = 'business_35', 'Classics' = 'classics_6', 'Fiction' = 'fiction_10',
                      'Horror' = 'horror_31', 'Music' = 'music_14') 
        
        url <- glue('http://books.toscrape.com/catalogue/category/books/', mappings[input$genreSelect], '/index.html')
        
        titles <- read_html(url) %>% 
            html_nodes('h3') %>%
            html_nodes('a') %>% 
            html_text()
        
        urls <- read_html(url) %>%
            html_nodes('.image_container') %>% 
            html_nodes('a') %>% 
            html_attr('href') %>% 
            str_replace_all('../../../', '/')
        
        imgs <- read_html(url) %>% 
            html_nodes('.image_container') %>%
            html_nodes('img') %>%
            html_attr('src') %>%
            str_replace_all('../../../../', '/')
        
        ratings <- read_html(url) %>% 
            html_nodes('p.star-rating') %>% 
            html_attr('class') %>% 
            str_replace_all('star-rating ', '')
        
        prices <- read_html(url) %>% 
            html_nodes('.product_price') %>% 
            html_nodes('.price_color') %>% 
            html_text()
        
        availability <- read_html(url) %>% 
            html_nodes('.product_price') %>% 
            html_nodes('.instock') %>% 
            html_text() %>% 
            str_trim()
        
        df <- data.frame(
            Title = titles, 
            URL = urls, 
            SourceImage = imgs, 
            Rating = ratings, 
            Price = prices, 
            Availability = availability
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
