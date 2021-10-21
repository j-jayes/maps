# Shiny app for exploring the Ames Iowa housing dataset.

library(shiny)
library(tidyverse)
library(AmesHousing)
library(leaflet)
library(here)
library(scales)
library(glue)

df <- read_rds(here("ames-map", "data", "ames-data.rds"))

cts_choices <- c(
  "Lot Frontage" = "lot_frontage",
  "Lot Area" = "lot_area",
  "Year Built" = "year_built",
  "Year Remod Add" = "year_remod_add",
  "Gr Liv Area" = "gr_liv_area"
)



# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput("cts_var", "Factor Variable",
        choices = cts_choices
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map"),
      plotOutput("plot_cts")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderLeaflet({

        df %>%
            mutate(
                transformed_price = log2(sale_price),
                price_color = colorNumeric(c("blue", "red"), sale_price)(sale_price)
            ) %>%
            mutate(across(c(lot_area, gr_liv_area), ~ number(.x, big.mark = " ", accuracy = 1, suffix = " sq ft")),
                   across(sale_price, ~ dollar(.x, big.mark = " ", accuracy = 1)),
                   mo_sold = lubridate::month(mo_sold, label = T, abbr = F),
                   date_sold = glue({"{mo_sold} {yr_sold}"})) %>%
            gather(
                key, value,
                pid,
                sale_price,
                date_sold,
                year_built,
                neighborhood,
                lot_area,
                gr_liv_area,
                overall_cond,
                overall_qual
            ) %>%
            mutate(
                key = str_to_title(str_replace_all(key, "_", " ")),
                key = paste0("<b>", key, "</b>")
            ) %>%
            replace_na(list(value = "Unknown")) %>%
            nest(data = c(key, value)) %>%
            mutate(html = map(data,
                              knitr::kable,
                              format = "html",
                              escape = FALSE,
                              col.names = c("", "")
            )) %>%
            leaflet() %>%
            addTiles() %>%
            addCircleMarkers(
                lat = ~latitude,
                lng = ~longitude,
                color = ~price_color,
                popup = ~html,
                radius = 2
            ) %>%
            addMeasure()
    })

    output$plot_cts <- renderPlot({

        plot_cts <- function(var) {

            # this creates a label for the y-axis
            var_str <- var %>%
                str_replace_all(., "_", " ") %>%
                str_to_title()
            # here we make the plot
            df %>%
                ggplot(aes(sale_price, y = .data[[var]])) +
                geom_point(colour = "midnightblue", alpha = .5) +
                geom_smooth() +
                scale_x_log10(labels = dollar_format()) +
                theme(legend.position = "none") +
                labs(
                    x = "Sale price (log scale)",
                    y = glue("{var_str}"),
                    title = glue("Scatter plot and smoothed line of sale price by {var_str}")
                )
        }

        plot_cts(input$cts_var)

    })

    output$plot_fct <- renderPlot({

        plot_cts <- function(var) {

            # this creates a label for the y-axis
            var_str <- var %>%
                str_replace_all(., "_", " ") %>%
                str_to_title()
            # here we make the plot
            df %>%
                ggplot(aes(sale_price, y = .data[[var]])) +
                geom_point(colour = "midnightblue", alpha = .5) +
                geom_smooth() +
                scale_x_log10(labels = dollar_format()) +
                theme(legend.position = "none") +
                labs(
                    x = "Sale price (log scale)",
                    y = glue("{var_str}"),
                    title = glue("Scatter plot and smoothed line of sale price by {var_str}")
                )
        }

        plot_cts(input$cts_var)

    })
}

# Run the application
shinyApp(ui = ui, server = server)
