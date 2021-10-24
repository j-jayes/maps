# Shiny app for exploring the Ames Iowa housing dataset.

library(shiny)
library(tidyverse)
library(leaflet)
library(here)
library(scales)
library(glue)
library(bslib)
library(thematic)
library(ggridges)

thematic_shiny()
theme_set(theme_light())
theme_update(text = element_text(size = 17))

df <- read_rds("CopyOfames-data.rds")

cts_choices <- c(
    "Lot Frontage" = "lot_frontage",
    "Lot Area" = "lot_area",
    "Year Built" = "year_built",
    "Year Remod Add" = "year_remod_add",
    "Gr Liv Area" = "gr_liv_area"
)

fct_choices <- c(
    "Municipal zoning" = "ms_zoning",
    "Lot shape" = "lot_shape",
    "Building type" = "bldg_type",
    "House style" = "house_style",
    "Roof style" = "roof_style",
    "1st Exterior material" = "exterior_1st",
    "Foundation material" = "foundation",
    "Exterior quality" = "exter_qual",
    "Exterior condition" = "exter_cond",
    "Overall quality" = "overall_qual",
    "Overall condition" = "overall_cond",
    "Basement full bathroom" = "bsmt_full_bath",
    "Full bathroom" = "full_bath",
    "Half bathroom" = "half_bath",
    "Bedrooms above ground" = "bedroom_abv_gr",
    "Kitchen" = "kitchen_abv_gr",
    "Total # rooms above ground" = "tot_rms_abv_grd",
    "Fireplaces" = "fireplaces",
    "# Cars garage" = "garage_cars"
)

ui <- fluidPage(
    theme = bs_theme(bootswatch = "pulse", font_scale = 1.5),
    # Application title
    titlePanel("Ames housing dataset explorer"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            dateRangeInput("date_sold", "Select date sold range",
                           min = "2006-01-01",
                           max = "2010-12-01",
                           start = "2006-01-01",
                           end = "2010-12-01",
                           startview = "year",
                           format = "yyyy-mm"
            ),
            selectizeInput("cts_var", "Select scatterplot x-variable",
                           choices = cts_choices,
                           selected = "year_built"
            ),
            selectizeInput("fct_var", "Select ridgeplot y-variable",
                           choices = fct_choices,
                           selected = "bldg_type"
            )
        ),
        mainPanel(
            fluidRow(column(10, leafletOutput("map"))),
            fluidRow(
                column(5, "", plotOutput("plot_cts")),
                column(5, "", plotOutput("plot_fct"))
            )
        )
    )
)

# Define server logic
server <- function(input, output) {

    df_filtered <- reactive({
        df %>%
            filter(between(date_sold, input$date_sold[1], input$date_sold[2]))
    })

    output$map <- renderLeaflet({
        df_filtered() %>%
            mutate(
                transformed_price = log2(sale_price),
                price_color = colorNumeric(c("blue", "red"), sale_price)(sale_price)
            ) %>%
            mutate(across(c(lot_area, gr_liv_area), ~ number(.x, big.mark = " ", accuracy = 1, suffix = " sq ft")),
                   across(sale_price, ~ dollar(.x, big.mark = " ", accuracy = 1)),
                   mo_sold = lubridate::month(mo_sold, label = T, abbr = F),
                   date_sold = glue({
                       "{mo_sold} {yr_sold}"
                   })
            ) %>%
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
        plot_cts(df_filtered(), input$cts_var)
    })

    output$plot_fct <- renderPlot({
        plot_fct(df_filtered(), input$fct_var)
    })

    ### Functions

    plot_fct <- function(df, var) {

        # this creates a label for the y-axis
        var_str <- var %>%
            str_replace_all(., "_", " ") %>%
            str_to_title()
        # here we make the plot
        df %>%
            mutate(
                !!sym(var) := factor(!!sym(var)),
                !!sym(var) := fct_lump(!!sym(var), 9),
                # !!sym(var) := fct_reorder(!!sym(var), sale_price)
            ) %>%
            ggplot(aes(sale_price, y = .data[[var]], fill = .data[[var]])) +
            geom_density_ridges() +
            scale_x_log10(labels = dollar_format()) +
            theme(legend.position = "none") +
            labs(
                x = "Sale price (log scale)",
                y = glue("{var_str}"),
                title = glue("Distribution of sale price by {var_str}")
            )
    }

    plot_cts <- function(df, var) {

        # this creates a label for the y-axis
        var_str <- var %>%
            str_replace_all(., "_", " ") %>%
            str_to_title()
        # here we make the plot
        df %>%
            ggplot(aes(y = sale_price, x = .data[[var]])) +
            geom_point(alpha = .5, colour = "midnightblue") +
            geom_smooth() +
            scale_y_log10(labels = dollar_format()) +
            theme(legend.position = "none") +
            labs(
                y = "Sale price (log scale)",
                x = glue("{var_str}"),
                title = glue("Scatter plot of sale price by {var_str}")
            )
    }
}

# Run the application
shinyApp(ui = ui, server = server)
