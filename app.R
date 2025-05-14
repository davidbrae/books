library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(plotly)
library(stringr)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "My Reading Journey"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Reading Patterns", tabName = "patterns", icon = icon("chart-line")),
      menuItem("Book Details", tabName = "details", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_books", width = 3),
          valueBoxOutput("total_pages", width = 3),
          valueBoxOutput("avg_pages", width = 3),
          valueBoxOutput("current_year_books", width = 3)
        ),
        fluidRow(
          box(
            title = "Books Read by Year",
            plotlyOutput("books_by_year_plot"),
            width = 6
          ),
          box(
            title = "Pages Read by Year",
            plotlyOutput("pages_by_year_plot"),
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Top Authors",
            plotlyOutput("top_authors_plot"),
            width = 6
          ),
          box(
            title = "Books by Author Gender",
            plotlyOutput("gender_plot"),
            width = 6
          )
        )
      ),
      
      # Reading Patterns tab
      tabItem(
        tabName = "patterns",
        fluidRow(
          box(
            title = "Books by Nationality",
            plotlyOutput("nationality_plot"),
            width = 6
          ),
          box(
            title = "Books by Language",
            plotlyOutput("language_plot"),
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Reading by Month",
            plotlyOutput("month_plot"),
            width = 6
          ),
          box(
            title = "Average Pages by Year",
            plotlyOutput("avg_pages_year_plot"),
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Reading Timeline",
            plotlyOutput("timeline_plot"),
            width = 12
          )
        )
      ),
      
      # Book Details tab
      tabItem(
        tabName = "details",
        fluidRow(
          box(
            title = "Book Filter",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(4, selectInput("author_filter", "Author:", choices = NULL, multiple = TRUE)),
              column(4, selectInput("nationality_filter", "Nationality:", choices = NULL, multiple = TRUE)),
              column(4, selectInput("language_filter", "Language:", choices = NULL, multiple = TRUE))
            ),
            fluidRow(
              column(4, selectInput("year_filter", "Year:", choices = NULL, multiple = TRUE)),
              column(4, sliderInput("pages_filter", "Pages:", min = 0, max = 1100, value = c(0, 1100))),
              column(4, selectInput("gender_filter", "Gender:", choices = NULL, multiple = TRUE))
            )
          )
        ),
        fluidRow(
          box(
            title = "Book List",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("book_table")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Read and clean data
  books_data <- reactive({
    # Read data from the CSV file
    books <- read.csv("Books.csv", stringsAsFactors = FALSE)
    
    # Clean data
    books$Year <- as.numeric(as.character(books$Year))
    books$Pages <- as.numeric(as.character(books$Pages))
    
    # Create date column if Month and Day are available
    books$Date <- NA
    for (i in 1:nrow(books)) {
      if (!is.na(books$Month[i]) && !is.na(books$Day[i]) && !is.na(books$Year[i])) {
        # Convert month name to number if it's a text month
        if (is.character(books$Month[i])) {
          month_num <- match(books$Month[i], month.abb)
          if (is.na(month_num)) {
            month_num <- match(tolower(books$Month[i]), tolower(month.name))
          }
          
          if (!is.na(month_num)) {
            books$Date[i] <- as.Date(paste(books$Year[i], month_num, books$Day[i], sep = "-"))
          }
        } else {
          # If Month is already numeric
          books$Date[i] <- as.Date(paste(books$Year[i], books$Month[i], books$Day[i], sep = "-"))
        }
      }
    }
    
    # Fill in missing values
    books$Pages[is.na(books$Pages)] <- mean(books$Pages, na.rm = TRUE)
    
    return(books)
  })
  
  # Update filters when data is loaded
  observe({
    books <- books_data()
    
    updateSelectInput(session, "author_filter", choices = c("All", sort(unique(books$Author))))
    updateSelectInput(session, "nationality_filter", choices = c("All", sort(unique(books$Nationality))))
    updateSelectInput(session, "language_filter", choices = c("All", sort(unique(books$Language))))
    updateSelectInput(session, "year_filter", choices = c("All", sort(unique(books$Year))))
    updateSelectInput(session, "gender_filter", choices = c("All", sort(unique(books$Gender))))
  })
  
  # Filter data based on user selections
  filtered_data <- reactive({
    books <- books_data()
    
    if (!is.null(input$author_filter) && !("All" %in% input$author_filter)) {
      books <- books[books$Author %in% input$author_filter, ]
    }
    
    if (!is.null(input$nationality_filter) && !("All" %in% input$nationality_filter)) {
      books <- books[books$Nationality %in% input$nationality_filter, ]
    }
    
    if (!is.null(input$language_filter) && !("All" %in% input$language_filter)) {
      books <- books[books$Language %in% input$language_filter, ]
    }
    
    if (!is.null(input$year_filter) && !("All" %in% input$year_filter)) {
      books <- books[books$Year %in% input$year_filter, ]
    }
    
    if (!is.null(input$gender_filter) && !("All" %in% input$gender_filter)) {
      books <- books[books$Gender %in% input$gender_filter, ]
    }
    
    books <- books[books$Pages >= input$pages_filter[1] & books$Pages <= input$pages_filter[2], ]
    
    return(books)
  })
  
  # Value boxes
  output$total_books <- renderValueBox({
    books <- books_data()
    valueBox(
      nrow(books),
      "Total Books Read",
      icon = icon("book"),
      color = "purple"
    )
  })
  
  output$total_pages <- renderValueBox({
    books <- books_data()
    valueBox(
      format(sum(books$Pages, na.rm = TRUE), big.mark = ","),
      "Total Pages Read",
      icon = icon("file-alt"),
      color = "blue"
    )
  })
  
  output$avg_pages <- renderValueBox({
    books <- books_data()
    valueBox(
      round(mean(books$Pages, na.rm = TRUE)),
      "Average Pages per Book",
      icon = icon("chart-bar"),
      color = "green"
    )
  })
  
  output$current_year_books <- renderValueBox({
    books <- books_data()
    current_year <- max(books$Year, na.rm = TRUE)
    current_year_books <- books[books$Year == current_year, ]
    valueBox(
      nrow(current_year_books),
      paste("Books Read in", current_year),
      icon = icon("calendar"),
      color = "orange"
    )
  })
  
  # Books by year plot
  output$books_by_year_plot <- renderPlotly({
    books <- books_data()
    books_by_year <- books %>%
      group_by(Year) %>%
      summarise(Count = n())
    
    p <- ggplot(books_by_year, aes(x = Year, y = Count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_line(color = "darkred", size = 1, group = 1) +
      geom_point(color = "darkred", size = 3) +
      theme_minimal() +
      labs(x = "Year", y = "Number of Books")
    
    ggplotly(p)
  })
  
  # Pages by year plot
  output$pages_by_year_plot <- renderPlotly({
    books <- books_data()
    pages_by_year <- books %>%
      group_by(Year) %>%
      summarise(Total_Pages = sum(Pages, na.rm = TRUE))
    
    p <- ggplot(pages_by_year, aes(x = Year, y = Total_Pages)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      geom_line(color = "darkred", size = 1, group = 1) +
      geom_point(color = "darkred", size = 3) +
      theme_minimal() +
      labs(x = "Year", y = "Total Pages")
    
    ggplotly(p)
  })
  
  # Top authors plot
  output$top_authors_plot <- renderPlotly({
    books <- books_data()
    top_authors <- books %>%
      group_by(Author) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      head(10)
    
    p <- ggplot(top_authors, aes(x = reorder(Author, Count), y = Count)) +
      geom_bar(stat = "identity", fill = "coral") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Author", y = "Number of Books")
    
    ggplotly(p)
  })
  
  # Gender plot
  output$gender_plot <- renderPlotly({
    books <- books_data()
    gender_counts <- books %>%
      group_by(Gender) %>%
      summarise(Count = n())
    
    p <- ggplot(gender_counts, aes(x = "", y = Count, fill = Gender)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(fill = "Gender")
    
    ggplotly(p)
  })
  
  # Nationality plot
  output$nationality_plot <- renderPlotly({
    books <- books_data()
    nationality_counts <- books %>%
      group_by(Nationality) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      head(10)
    
    p <- ggplot(nationality_counts, aes(x = reorder(Nationality, Count), y = Count)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Nationality", y = "Number of Books")
    
    ggplotly(p)
  })
  
  # Language plot
  output$language_plot <- renderPlotly({
    books <- books_data()
    language_counts <- books %>%
      group_by(Language) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      head(10)
    
    p <- ggplot(language_counts, aes(x = reorder(Language, Count), y = Count)) +
      geom_bar(stat = "identity", fill = "mediumorchid") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Language", y = "Number of Books")
    
    ggplotly(p)
  })
  
  # Month plot
  output$month_plot <- renderPlotly({
    books <- books_data()
    books$Month_num <- NA
    
    for (i in 1:nrow(books)) {
      if (!is.na(books$Month[i])) {
        if (is.character(books$Month[i])) {
          month_num <- match(books$Month[i], month.abb)
          if (is.na(month_num)) {
            month_num <- match(tolower(books$Month[i]), tolower(month.name))
          }
          
          if (!is.na(month_num)) {
            books$Month_num[i] <- month_num
          }
        } else {
          books$Month_num[i] <- books$Month[i]
        }
      }
    }
    
    books_with_month <- books[!is.na(books$Month_num), ]
    
    month_counts <- books_with_month %>%
      group_by(Month_num) %>%
      summarise(Count = n())
    
    month_counts$Month_name <- month.name[month_counts$Month_num]
    month_counts <- month_counts[order(month_counts$Month_num), ]
    
    p <- ggplot(month_counts, aes(x = factor(Month_name, levels = month.name), y = Count)) +
      geom_bar(stat = "identity", fill = "salmon") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Month", y = "Number of Books")
    
    ggplotly(p)
  })
  
  # Average pages by year plot
  output$avg_pages_year_plot <- renderPlotly({
    books <- books_data()
    avg_pages_by_year <- books %>%
      group_by(Year) %>%
      summarise(Avg_Pages = mean(Pages, na.rm = TRUE))
    
    p <- ggplot(avg_pages_by_year, aes(x = Year, y = Avg_Pages)) +
      geom_line(color = "purple", size = 1) +
      geom_point(color = "purple", size = 3) +
      theme_minimal() +
      labs(x = "Year", y = "Average Pages per Book")
    
    ggplotly(p)
  })
  
  # Timeline plot
  output$timeline_plot <- renderPlotly({
    books <- books_data()
    books_with_date <- books[!is.na(books$Date), ]
    
    if (nrow(books_with_date) > 0) {
      books_with_date <- books_with_date[order(books_with_date$Date), ]
      books_with_date$Index <- 1:nrow(books_with_date)
      
      p <- ggplot(books_with_date, aes(x = Date, y = Index, text = paste("Title:", Book, "<br>Author:", Author))) +
        geom_point(aes(color = Gender), size = 3) +
        theme_minimal() +
        labs(x = "Date", y = "Book Index")
      
      ggplotly(p, tooltip = "text")
    } else {
      p <- ggplot() +
        annotate("text", x = 0, y = 0, label = "No dates available") +
        theme_void()
      
      ggplotly(p)
    }
  })
  
  # Book table
  output$book_table <- renderDT({
    books <- filtered_data()
    
    datatable(
      books[, c("Book", "Author", "Year", "Language", "Nationality", "Pages", "Gender")],
      options = list(
        pageLength = 10,
        searching = TRUE,
        ordering = TRUE
      )
    )
  })
}

# Run the app
shinyApp(ui, server)