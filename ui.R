ui <- dashboardPage(
  options = list(sidebarExpandOnHover = TRUE),
  header = dashboardHeader(title = "Fake School Data", fixed = TRUE,
                           tags$head(tags$style(HTML(".brand-link {text-align:center !important;}")))
                           # rightUi = (tags$li(a(onclick = "onclick =window.open('https://github.com')",
                           #           href = NULL,
                           #           icon("github"),
                           #           title = "GitHub",
                           #           style = "cursor: pointer;"),
                           #         class = "dropdown")) 
                           # Leave out for now. Odd circle follows the icon regardless of where you place it.
                           ),
  sidebar = dashboardSidebar(minified = FALSE, collapsed = FALSE,
                             sidebarMenu( id = "tabs",
                               menuItem("Regression", tabName = "regression", icon = icon("chart-line"),
                                        menuSubItem("Simple Regression", tabName = "simple"),
                                        menuSubItem("Multiple Regression", tabName = "multi")
                               ),
                               menuItem("Comparisons", tabName = "comparisons", icon = icon("balance-scale"),
                                        menuSubItem("Demographics", tabName = "demographics")
                               ),
                               menuItem("Heatmaps", tabName = "heatmaps", icon = icon("map")
                               ))
  ),
  body = dashboardBody(
    # tags$style(type="text/css",
    #            ".shiny-output-error { visibility: hidden; }",
    #            ".shiny-output-error:before { visibility: hidden; }"
    # ),
    # waiter::autoWaiter(c("myMap", "kw_plot"), color = transparent(.3), html = spin_throbber()),
    useWaiter(),
    tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);"),
    # Add the code below if you want to view the dashboard in browser.
      # tags$style("body {
      # -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
      # zoom: 0.8; /* Other non-webkit browsers */
      # zoom: 80%; /* Webkit browsers */
      #       }"),
    tags$head(tags$style(HTML(
      "#.+ div>.waiter-overlay-content{
      position: absolute;
      top: 0px;
      left: 0px;
    }"
    ))),
    tags$head(tags$style(HTML(
      ".waiter-overlay{
      position: static !important;
      top:0;
      bottom:0;
      left:0;
      right:0;
    }"
    ))),
    tags$script('
      // Bind function to the toggle sidebar button
      $(".sidebar-toggle").on("click",function(){
        $(window).trigger("resize"); // Trigger resize event
      })'
    ),
    tags$script(HTML("$('body').addClass('fixed');")),
    uiOutput('datatable_darkcontrol'),
    tabItems(
      tabItem(
        tabName = "regression",
        titlePanel = "something"
      ),
      tabItem(
        tabName = "simple",
        titlePanel = "Drivers of Engagement and/or Leave Intentions",
        fluidRow(
          column(width = 6,
            box(title = HTML("Model Summaries"), width = 12,
                fluidRow(column(width = 4,
                                uiOutput("simple_outcome"),
                                tags$head(tags$style(HTML("#simple_outcome + div > .selectize-input {height: 100%; width: 100%; font-size: 100%;}")))
                                # NOTE: This is no longer needed after switching to bs4Dash; however, keeping syntax incase I swap back to shinydashboardplus 
                                )),
                fluidRow(column(width = 12,
                  DT::dataTableOutput("origTable")))
                ),
            box(title = HTML("Coefficients"), dataTableOutput("coefs", width = "100%"), width = 12)),
          column(width = 6,
            box(plotOutput("scatterplot", width = "100%", height = "50em"), width = 12, height = "50em"),
                 )),
        fluidRow(box(title = HTML("Assumption Checks<br>"), width = 12,
          fluidRow(column(width = 6,
                     tabBox(
                       tabPanel("Normality (QQ)",
                                box(plotOutput("Norm1", width = "100%"), width = NULL, collapsible = FALSE, headerBorder = FALSE)),
                       tabPanel("Normality (Distribution)",
                                box(plotOutput("Norm2", width = "100%"), width = NULL, collapsible = FALSE, headerBorder = FALSE)),
                       tabPanel("Homogeneity",
                                box(plotOutput("homogeneity", width = "100%"), width = NULL, collapsible = FALSE, headerBorder = FALSE)), width = NULL)),
                  column(width = 6,
                    tabBox(
                       tabPanel("Posterior Predictive Check",
                                box(plotOutput("pp_check", width = "100%"), width = NULL, collapsible = FALSE, headerBorder = FALSE)),
                       tabPanel("Influential Observations",
                                box(plotOutput("outliers", width = "100%"), width = NULL, collapsible = FALSE, headerBorder = FALSE)), width = NULL)))))
        
      ),
      tabItem(
        tabName = "multi",
        titlePanel = "something",
        fluidRow(
          box(title = HTML("Model Summary"), width = 12, 
              fluidRow(column(width = 4,
                              uiOutput("multi_outcome"),
                              tags$head(tags$style(HTML("#multi_outcome + div > .selectize-input {height: 100%; width: 25%; font-size: 100%;}"))),
              )
              ),
              fluidRow(
                DT::dataTableOutput("multisumm", width = "100%")))
              ),
        fluidRow(
          column(width = 7, 
                 box(title = HTML("Coefficients"), DT::dataTableOutput("multicoefs", width = "100%"), width = NULL)),
          column(width = 5,
                 box(title = HTML("Relative Importance"), billboarderOutput("donut"), width = NULL),
                 ))
      ),
      tabItem(
        tabName = "comparisons",
        titlePanel = "something"
      ),
      tabItem(
        tabName = "demographics",
        titlePanel = "Comparisons between levels of each Demographic",
        fluidRow(
          box(title = HTML("Kruskal-Wallis Tests"), width = 6, 
              fluidRow(column(width = 5,
                              uiOutput("demo"),
                                tags$head(tags$style(HTML("#demo + div > .selectize-input {height: 100%; width: 25%; font-size: 100%;}")))
              )
              ),
              fluidRow(
                DT::dataTableOutput("KWTable", width = "100%"))),
          tabBox(
                 tabPanel("Dunn Test",
                          dataTableOutput("dunn", width = "100%")),
                 tabPanel("Descriptives",
                          dataTableOutput("demo_summ", width = "100%"))
              )
        ),
        fluidRow(
          box(title = HTML("Kruskal-Wallis Violin Plots"), width = 12,
              plotOutput("kw_plot", width = "100%", height = "60vh")
          ))
      ),
      tabItem(
        tabName = "heatmaps",
        titlePanel = "Heat Maps",
        fluidRow(column(width = 8,
          box(fluidRow(align = "left",
                     column(width = 4,
                            uiOutput("map_variable"),
                            tags$head(tags$style(HTML("#map_variable + div > .selectize-input {height: 100%; width: 25%; font-size: 100%;}")))),
                     column(width = 8,
                            selectizeInput(inputId = "selected_locations",
                                           label = "Districts selected: (Click on the box or map to select districts)",
                                           choices = NULL,
                                           selected = NULL,
                                           multiple = TRUE),
                            tags$head(tags$style(HTML("#selected_locations + div > .selectize-input {height: 100%; width: 75%; font-size: 100%;}"))))),
            leafletOutput("myMap", width = "100%", height = "40em"), width = 12)),
        column(width = 4, tabBox(tabPanel("Top Schools",
                                valueBoxOutput("highest_one", width = 12),
                                valueBoxOutput("highest_two", width = 12),
                                valueBoxOutput("highest_three", width = 12)),
              tabPanel("Bottom Schools",
                       valueBoxOutput("lowest_one", width = 12),
                       valueBoxOutput("lowest_two", width = 12),
                       valueBoxOutput("lowest_three", width = 12)),  width = 12
        ))),
        fluidRow(      
          box(DT::dataTableOutput("maptable", width = "100%"), width = 6),
          box(highchartOutput("mapbar"), width = 6))
      )
    )
  ),
  controlbar = dashboardControlbar(
    collapsed = TRUE,
    div(class = "p-3", skinSelector()),
    pinned = FALSE
  ),
  title = "DashboardPage"
  )