# load clean data
data <- rio::import("data/cleaned_data.csv")

data_leaflet <- rio::import("data/world_country_and_usa_states_latitude_and_longitude_values.csv")

data_leaflet2 <- data_leaflet |>
    dplyr::select(country,latitude,longitude)

data_map <- data |>
    group_by(Country) |>
    summarize(total_revenue = sum(Revenue)) |>
    arrange(Country) |>
    left_join(data_leaflet2, by = join_by(Country == country))



# Define UI --------------------Country# Define UI ---------------------------------------------------------------
ui <- 
    page_fluid(
        useWaiter(),
        waiterPreloader(),
        # app title ----
        titlePanel("Business-Oriented Dashboard"),
        # theme
        theme = bslib::bs_theme(
            bootswatch = "flatly",
            version = "5"),
        # sidebar layout with input and output definitions ----
        navset_card_pill(
            sidebar = sidebar(
                selectInput(
                    inputId = "country",
                    label = "Choose country",
                    choices = sort(unique(data$Country)),
                    selected = "Belgium",
                    multiple = TRUE
                ),
                sliderInput(
                    inputId = "period",
                    label = "Choose period",
                    min = min(data$InvoiceDate),
                    max = max(data$InvoiceDate),
                    value = c(max(data$InvoiceDate) %m-% months(6),
                              max(data$InvoiceDate)),
                    step = 30
                ),
                selectInput(
                    inputId = "format",
                    label = "Select File Format",
                    choices = sort(c("CSV"="csv","Excel"="xlsx"))
                ),
                downloadButton("downloadData", label = "Download", class = NULL)
            ),
            nav_panel(
                title = "Visualizations",
                layout_column_wrap(
                    width = "400px",
                    card(card_header("Plotly - Sales Trends"),
                         full_screen = T,
                         card_body(shinycssloaders::withSpinner(plotlyOutput("plotly"),
                                                                type = 7))
                    ),
                    card(card_header("Highcharter - Top Products"),
                         full_screen = T,
                         card_body(shinycssloaders::withSpinner(highchartOutput("highchart"),
                                                                type = 7))
                         ),
                )
            ),
            nav_panel(
                title = "Leaflet",
                card(card_header("Leaflet - Sales by Country"),
                     full_screen = T,
                     card_body(shinycssloaders::withSpinner(leafletOutput("leaflet"),
                                                            type = 7))
                ),
            ),
            nav_panel(
                title = "DT",
                card(
                    card_header("DT - Interactive Transaction Table"),
                    full_screen = T,
                    card_body(shinycssloaders::withSpinner(dataTableOutput("dt"),
                                                           type = 7))
                )
            ),
            nav_panel(
                title = "Reactable",
                card(card_header("Reactable - Customer Insights"),
                     full_screen = T,
                     card_body(shinycssloaders::withSpinner(reactableOutput("react"),
                                                            type = 7))
                )
            )
        )
    )

# Define server logic -----------------------------------------------------
server <- function(input, output) {
    shinyalert("Welcome!", "Please explore the dashboard")
    data_filtered <- reactive({
        data |>
            dplyr::filter(InvoiceDate >= input$period[1] & InvoiceDate <= input$period[2]) |>
            dplyr::filter(Country == input$country)
            })
    data_map_filtered <- reactive({
        data_map |>
            dplyr::filter(Country == input$country)
    })
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data","_", Sys.Date(),".", input$format, sep='')
        },
        content = function(con) {
            rio::export(data_filtered(), con)
        }
    )
    
    output$plotly <- renderPlotly({
        data_filtered() |>
            mutate(month = lubridate::month(InvoiceDate)) |>
            group_by(month) |>
            summarize(total_revenue = sum(Revenue)) |>
            plot_ly(x = ~month,
                    y = ~total_revenue,
                    type="scatter",
                    mode="lines") |>
            layout(title = "Revenue by InvoiceDate",
                   xaxis = list(title = "Months"),
                   yaxis = list (title = "Revenues"))
    })
    
    #to check error when the filter returns no results
    output$highchart <- renderHighchart({
        data_filtered() |>
            group_by(StockCode) |>
            summarize(total_quantity = sum(Quantity)) |>
            arrange(desc(total_quantity)) |>
            slice_head(n=10) |>
            hchart("column", hcaes(x = StockCode, y = total_quantity),
                   color = "#0198f9", name = "Quantity sold per product") |>
            hc_title(text = "Top 10  products") |>
            hc_xAxis(title = list(text = "Product")) |>
            hc_yAxis(title = list(text = "Quantity"))
    })

    "Total sales per country"
    output$leaflet <- renderLeaflet({
        leaflet() |>
            addTiles() |> 
            setView(49.48,6.07, zoom = 2) |>
            addMarkers(data=data_map_filtered(), popup=paste(data_map_filtered()$Country,"(revenues):",data_map_filtered()$total_revenue))
        })
    
    # "sales transactions"
        output$dt <- renderDataTable({
        data_filtered() |>
            DT::datatable(class = 'cell-border stripe',
                          rownames = FALSE,
                          filter = 'top',
                          options = list(pageLength = 5))
    })

    # "top customers"    
    output$react <- renderReactable({
        data_filtered() |>
            reactable::reactable()
    })
}
# Run the application -----------------------------------------------------
#shinyApp(ui, server, options = list(display.mode = "showcase"))
shinyApp(ui, server, options = list())