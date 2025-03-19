# load clean data
data <- rio::import("data/cleaned_data.csv")

# Define UI ---------------------------------------------------------------
ui <- 
    page_fluid(
        # app title ----
        titlePanel("Business-Oriented Dashboard"),
        # theme
        theme = bslib::bs_theme(
            bootswatch = "flatly",
            version = "5"),
        # sidebar layout with input and output definitions ----
        navset_card_pill(
            sidebar = sidebar(),
            nav_panel(
                title = "Visualizations",
                layout_columns(
                    col_widths = c(6,6),
                    card(card_header("Plotly - Sales Trends"),
                         full_screen = T,
                         card_body(plotlyOutput("plotly"))
                    ),
                    card(card_header("Highcharter - Top Products"),
                         full_screen = T,
                         card_body(highchartOutput("highchart"))
                         ),
                    # card(card_header("Leaflet - Sales by Country"),
                    #      full_screen = T,
                    #      card_body(leafletOutput("leaflet"))
                    #      ),
                )
            ),
            nav_panel(
                title = "DT",
                layout_columns(
                    col_widths = c(12),
                    card(
                        card_header("DT - Interactive Transaction Table"),
                        full_screen = T,
                        card_body(dataTableOutput("dt"))
                    )
                )
            ),
            nav_panel(
                title = "Reactable",
                layout_columns(
                    col_widths = c(12),
                    card(card_header("Reactable - Customer Insights"),
                         full_screen = T,
                         card_body(reactableOutput("react"))
                    )
                )
            )
        )
    )

# Define server logic -----------------------------------------------------
server <- function(input, output) {
    # data_filtered <- reactive({
    #     data |>
    #         dplyr::filter(Year == input$year) |>
    #         dplyr::filter(Symbol == input$ticker)
    # })
    
    output$plotly <- renderPlotly({
        data |>
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
    
    output$highchart <- renderHighchart({
        data |>
            group_by(StockCode) |>
            summarize(total_revenue = sum(Revenue)) |>
            arrange(desc(total_revenue)) |>
            slice_head(n=10) |>
            hchart("column", hcaes(x = StockCode, y = total_revenue),
                   color = "#0198f9", name = "Top 10 products") |>
            hc_title(text = "Top 10  products") |>
            hc_xAxis(title = list(text = "Products")) |>
            hc_yAxis(title = list(text = "Revenues"))
    })
    
    # output$leaflet <- renderLeaflet({
    #     # browser()
    #     data2 |>
    #         select(Country,Revenue)
    #         # summarize()
    # })
    
    output$dt <- renderDataTable({
        data |>
            DT::datatable(class = 'cell-border stripe',
                          rownames = FALSE,
                          filter = 'top',
                          options = list(pageLength = 5))
    })
    
    output$react <- renderReactable({
        data |>
            reactable::reactable()
    })
}
# Run the application -----------------------------------------------------
#shinyApp(ui, server, options = list(display.mode = "showcase"))
shinyApp(ui, server, options = list())