server <- function(input, output, session) {

  
  output$'datatable_darkcontrol' = renderUI({
    if(input$'dark_mode'){
      return(
        tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, 
                         .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, 
                         .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {color: #D8D5E1 !important;}
                         .bb-tooltip-container{color: #000000}
                         .bb-legend text{fill: #fff}"))
      )
    }else{
      return(
        tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, 
                         .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, 
                         .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {color: #000000 !important;}"))
      ) 
    }
  })   
  

  
  
  
  
  ########## Regression Tab ################
  # Consider adding less technical visuals. More basic information
  # Possibly put Correlation matrix here or in next section.
  
  ########## Linear Regression Tab ################      
  
  output$simple_outcome <- renderUI({selectInput("outcome", "Outcome Variable:",
                                                 c(unique(All_Summary$Outcome)), 
                                                 selected = "Engagement",
                                                 multiple = FALSE
  )
  }) 
  
  
  df <- reactive({
    
    req(input$outcome)
    
    if(input$outcome == "Engagement"){
      E_Summary
    } else {
      L_Summary
    }
    
  })
  
  RowClicked <- reactiveValues(
    x = 1
    )
  
  output$origTable <- 
    DT::renderDataTable(
    if(input$'dark_mode'){
        datatable(df(), selection = list(mode = 'single', selected = RowClicked$x), options=list(
          columnDefs = list(list(visible=FALSE, targets=c(4, 7:15))),
          pageLength = 5,
          lengthMenu = c(5, 10, 15, 20),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"))) %>% 
          formatStyle(columns = colnames(df()),backgroundColor = "#282828",color = "#fff",target = 'row')
    }else{
      datatable(df(), selection = list(mode = 'single', selected = RowClicked$x), options=list(
        columnDefs = list(list(visible=FALSE, targets=c(4, 7:15))),
        pageLength = 5,
        lengthMenu = c(5, 10, 15, 20))
      )
      }
        )
                                                
  
  #Prevent "night/day" toggle from resetting data. Note: Not a perfect fix. For optimization, should look into proxy and replace w/ DT package.
  observeEvent(input$'dark_mode',{
    RowClicked$x = input$origTable_rows_selected
  }
               )
  

  sel <- reactive({
    req(input$origTable_rows_selected)
    
    row_count <- input$origTable_rows_selected
    datum <- df()[row_count,]
    P_Selected <- datum[,15]
  })
  
 

  pred <- reactive({
    req(sel())
    req(input$outcome)
    
    if(input$outcome == "Engagement"){
      pred <- E_Coefs %>%
        dplyr::filter(E_Coefs$Predictor == sel())
    } else {
      pred <- L_Coefs %>%
        dplyr::filter(L_Coefs$Predictor == sel())
    }
  })
  
  

  
  
    output$coefs <- DT::renderDataTable(
        
        if(input$'dark_mode'){
          datatable(pred(), options = list(
            pageLength = 5,
            lengthMenu = c(5, 10, 15, 20),
            columnDefs = list(list(visible=FALSE, targets=c(1,7,8))),
            dom = 'tp',
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"))) %>% formatStyle(columns = colnames(pred()),backgroundColor = "#282828",color = "#fff",target = 'row')
            }
        else{
          datatable(pred(), options = list(
            pageLength = 5,
            lengthMenu = c(5, 10, 15, 20),
            columnDefs = list(list(visible=FALSE, targets=c(1,7,8))),
            dom = 'tp'))
            })

      output$scatterplot <- renderPlot({
        req(input$origTable_rows_selected)
        
        reg_data <- Fake_Reg_Data
        Continuous <- colnames(reg_data[,c(1:10)])
        Categorical <- colnames(reg_data[,c(11:19)])
        
        row_count <- input$origTable_rows_selected
        data <- df()[row_count, ]
        
        
        O_Selected <- data[,14]
        P_Selected <- data[,15]
        O_V_P_Selected <- data[,1]

        if(P_Selected %in% Continuous){
          ggplot(reg_data,aes_string(O_Selected, P_Selected)) +
            geom_point() +
            geom_smooth(method='lm', se=TRUE, color='turquoise4') +
            theme_minimal() +
            labs(x = O_Selected, y = P_Selected, title = O_V_P_Selected) +
            theme(plot.title = element_text(hjust=0.5, size=24, face='bold'),
                  axis.text = element_text(size = 14),
                  axis.text.x = element_text(size = 16),
                  axis.text.y = element_text(size = 16))
        }else {
          ggplot2::ggplot(reg_data, aes(x=!!sym(P_Selected), y=!!sym(O_Selected), color = !!sym(P_Selected))) +
            geom_boxplot(size=1,
                         outlier.shape = 1,
                         outlier.color = "black",
                         outlier.size  = 3) +
            geom_jitter(alpha = 0.5, 
                        width = .2) +
            labs(x = "", y = "", title = O_V_P_Selected) +
            theme_minimal() +
            theme(legend.position = "none") + 
            theme(plot.title = element_text(hjust=0.5, size=24, face='bold'),
                  axis.text = element_text(size = 14),
                  axis.text.x = element_text(size = 16))
          
          
          
        } 
      }) %>%
        bindCache(input$origTable_rows_selected, input$outcome)
  
  Mod <- reactive({
    
    req(input$origTable_rows_selected)
    req(input$outcome)
    
    if(input$outcome == "Engagement"){
      E_Models[[input$origTable_rows_selected]]
    } else {
      L_Models[[input$origTable_rows_selected]]
    }
  })
  
  output$Norm1 <- renderPlot({
    plot(check_normality(Mod()), type = "qq", theme = "ggplot2::theme_minimal")
  })%>%
    bindCache(Mod())
  
  output$Norm2 <- renderPlot({
    plot(check_normality(Mod(), theme = "ggplot2::theme_minimal"))
  })%>%
    bindCache(Mod())
  
  
  output$homogeneity <- renderPlot({
    
    reg_data <- Fake_Reg_Data
    
    
    Continuous <- colnames(reg_data[,c(1:10)])
    Categorical <- colnames(reg_data[,c(11:19)])
    
    row_count <- input$origTable_rows_selected
    data <- df()[row_count, ]
    
    
    O_Selected <- data[,14]
    P_Selected <- data[,15]
    O_V_P_Selected <- data[,1]
    
    
    
    if(P_Selected %in% Continuous){
      check_model(Mod(), check = "homogeneity", panel = FALSE, theme = "ggplot2::theme_minimal")
    }else {
      
      y_resid <- rstandard(Mod())
      
      ggplot2::ggplot(reg_data, aes(x=!!sym(P_Selected), y=y_resid)) +
        geom_boxplot(size=1,
                     outlier.shape = 1,
                     outlier.color = "black",
                     outlier.size  = 3) +
        geom_abline(intercept = 0, slope = 0, size = 2, linetype = 2, color = "gray") +
        labs(x = "", y = "Residuals") +
        ggtitle("Homogeneity of Variance", subtitle = "Every boxplot should intersect with the reference line.") +
        theme_minimal() +
        theme(legend.position = "none") + 
        theme(plot.subtitle = element_text(size = 12))
      
    }
    
    
  })%>%
    bindCache(input$origTable_rows_selected,Mod())
  
  output$pp_check <- renderPlot({
    check_posterior_predictions(Mod(), check_range = TRUE)
  })%>%
    bindCache(Mod())
  
  output$outliers <- renderPlot({
    check_model(Mod(), check = "outliers", panel = FALSE, theme = "ggplot2::theme_minimal")
  })%>%
    bindCache(Mod())
  
  
  
  
  ########## Multiple Regression Tab ################       
  
  output$multi_outcome <- renderUI({selectInput("outcome2", "Outcome Variable:",
                                                c(unique(All_Summary$Outcome)), 
                                                selected = "Engagement",
                                                multiple = FALSE
  )
  })
  
  Multi_Summ <- reactive({
    
    req(input$outcome2)
    
    if(input$outcome2 == "Engagement"){
      ME_Summary
    } else {
      ML_Summary
    }
    
  })
  
  Multi_Coef <- reactive({
    
    req(input$outcome2)
    
    if(input$outcome2 == "Engagement"){
      ME_Coefs
    } else {
      ML_Coefs
    }
    
  })
  
  Multi_Donut <- reactive({
    
    req(input$outcome2)
    
    if(input$outcome2 == "Engagement"){
      ME_Donut
    } else {
      ML_Donut
    }
    
  })
  
  output$multisumm <- DT::renderDT({
    
    if(input$'dark_mode'){
      datatable(Multi_Summ(), rownames = FALSE, options = list(
        dom = "t",
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"))) %>% formatStyle(columns = colnames(Multi_Summ()),backgroundColor = "#282828",color = "#fff",target = 'row')
      }
    else{
      datatable(Multi_Summ(), rownames = FALSE, options = list(
        dom = "t"
      ))
      }
    })
  
  
  
  output$multicoefs <- DT::renderDT({
    
    if(input$'dark_mode'){
      datatable(Multi_Coef(), rownames = FALSE, options = list(
        dom = "t",
        pageLength = 10,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"))) %>% 
        formatStyle(columns = colnames(Multi_Coef()),backgroundColor = "#282828",color = "#fff",target = 'row') %>%
          DT::formatStyle('Importance',
                        background = DT::styleColorBar(range(0:1), 'lightblue'),
                        backgroundSize = '98% 88%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center')
      }
    else{
      datatable(Multi_Coef(), rownames = FALSE, options = list(
        dom = "t",
        pageLength = 10)) %>%
            DT::formatStyle('Importance',
                        background = DT::styleColorBar(range(0:1), 'lightblue'),
                        backgroundSize = '98% 88%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center')
    }
  })

  
  output$donut <- renderBillboarder({
    billboarder() %>% bb_piechart(Multi_Donut()) %>% bb_legend(position = 'right') %>%
      bb_color(palette = c('#e6194B','#f58231','#ffe119','#bfef45','#3cb44b', '#42d4f4', '#4363d8', '#911eb4'))
  })
  
  
  
  
  ########## Demographic Comparisons Tab ################  
  
  output$demo <- renderUI({selectInput("Demographic", "Demographic of Interest:",
                                       c(unique(All_KW$Demographic)), 
                                       selected = "Union",
                                       multiple = FALSE)
  })
  
  
RowClicked2 <- reactiveValues(
     x = 1
  )

  
  
  
  KW_Data <- reactive({
    req(input$Demographic)
    
    All_KW %>%
      dplyr::filter(str_detect(Demographic, input$Demographic))
  })
  

  
  output$KWTable <- 
    DT::renderDataTable({
      
      if(input$'dark_mode'){
        datatable(KW_Data(), rownames = FALSE, selection = list(mode = 'single', selected = RowClicked2$x), 
            options=list(
                columnDefs = list(list(visible=FALSE, targets=c(0))),
                pageLength = 5,
                lengthMenu = c(5, 10, 15, 20),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                  "}"))) %>% formatStyle(columns = colnames(KW_Data()),backgroundColor = "#282828",color = "#fff",target = 'row')
      }else{
        datatable(KW_Data(), rownames = FALSE, selection = list(mode = 'single', selected = RowClicked2$x), 
                  options=list(
                    columnDefs = list(list(visible=FALSE, targets=c(0))),
                    pageLength = 5,
                    lengthMenu = c(5, 10, 15, 20)))
      }
    })
  
  observeEvent(input$'dark_mode',{
    RowClicked2$x = input$KWTable_rows_selected
    }
  )
  
   
  
  Dunn_Data <- reactive({
    
    req(input$KWTable_rows_selected)
    
    row_count <- input$KWTable_rows_selected
    item <- KW_Data()[row_count, 2]
    
    All_Dunn %>%
      dplyr::filter(str_detect(Demographic, input$Demographic)) %>%
      dplyr::filter(Item %in% item)
  })
  
  
  
  output$dunn <- DT::renderDataTable({
    if(input$'dark_mode'){
      datatable(Dunn_Data(), rownames = FALSE, options = list(
                  pageLength = 5,
                  lengthMenu = c(5, 10, 15, 20),
                  columnDefs = list(list(visible=FALSE, targets=c(0:1, 10, 12))),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}"))) %>% formatStyle(columns = colnames(Dunn_Data()),backgroundColor = "#282828",color = "#fff",target = 'row')
    }else{
      datatable(Dunn_Data(), rownames = FALSE, options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 15, 20),
        columnDefs = list(list(visible=FALSE, targets=c(0:1, 10, 12))))
        )
     }
  })
  
  
  Demo_Data <- reactive({
    
    req(input$KWTable_rows_selected)
    
    row_count <- input$KWTable_rows_selected
    item <- KW_Data()[row_count, 2]
    
    Demo_Summs %>%
      dplyr::filter(str_detect(Demographic, input$Demographic)) %>%
      dplyr::filter(Item %in% item)
  })
  
  output$demo_summ <- DT::renderDataTable({ 
    
    if(input$'dark_mode'){ 
      datatable(Demo_Data(), rownames = FALSE,  
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15, 20),
          columnDefs = list(list(visible=FALSE, targets=c(6:7))),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"))) %>% formatStyle(columns = colnames(Demo_Data()),backgroundColor = "#282828",color = "#fff",target = 'row')
      }
    else{ 
      datatable(Demo_Data(), rownames = FALSE, 
          options = list(
            pageLength = 5,
            lengthMenu = c(5, 10, 15, 20),
            columnDefs = list(list(visible=FALSE, targets=c(6:7))))
            )
      }
  })
  
  
  output$kw_plot <- renderPlot({
    
    req(input$KWTable_rows_selected)
    
    Plot_Data <- Fake_Data
    Demographic <- input$Demographic
    
    row_count <- input$KWTable_rows_selected
    Item <- KW_Data()[row_count, 2]
    
    ggbetweenstats(
      data = Plot_Data,
      x = !!Demographic,
      y = !!Item, 
      type = "nonparametric",
      plot.type = "boxviolin",
      pairwise.comparisons = TRUE,
      pairwise.display = "significant",
      p.adjust.method = "bonferroni",
      caption = NULL,
      messages = FALSE,
      pairwise.annotation = "p.value",
      point.args = list(alpha = 0),
      centrality.type = "nonparametric",
      # title = paste0("Comparison of ", colnames(Data)[1], " by ", colnames(Data)[60]),
      ggplot.component = list(theme(plot.subtitle = element_text(size = 15, hjust=0.5),
                                    axis.text = element_text(size=10),
                                    axis.title = element_text(size=15)
      )),
      ggsignif.args = list(textsize = 5, tip_length = 0.01),
      centrality.label.args = list(size  = 5, nudge_y = -0.25)
    )
    
    
  })%>%
    bindCache(input$KWTable_rows_selected, input$Demographic) 
  
  
  
  
  
  
  ########## District Comparison Tab ################
  
  
  ########## Heatmaps ################  
  
  
  
  updateSelectizeInput(session, "selected_locations", choices = Testing_District$NAME, server = TRUE)
  
  output$map_variable <- renderUI({
    selectInput("variable", "Variable of Interest:",
                c(colnames(Fake_District_Data)[51:59]),
                selected = "Engagement",
                multiple = FALSE)
  })
  
  w <- Waiter$new(id = "myMap", html = spin_throbber())
  
  observeEvent(input$variable, {
    w$show()
  })
  
  
  output$myMap <- renderLeaflet({
    
    req(input$variable)

    paletteNum <- colorNumeric('RdBu', domain = Testing_District[[input$variable]],  na.color = "#9f9f9f")
    paletteNum_noNA <- colorNumeric('RdBu', domain = Testing_District[[input$variable]],  na.color = NA, reverse = TRUE)
    popup_duh <- paste0("District: ", Testing_District$NAME,"<br>",
                        "n: ", Testing_District$n, "<br>",
                        input$variable, ": ", Testing_District[[input$variable]]) %>%
      lapply(htmltools::HTML)   
    

    
    #initial map output
  
          # minZoom = 7, maxZoom = 7 # Lock zoom on map.
          # dragging = FALSE # Prevent dragging
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% #removes the +/- button on the map.
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = Testing_District,
                  color = "white",
                  weight = 0.5,
                  smoothFactor = .3,
                  fillOpacity = .75,
                  fillColor = ~paletteNum(Testing_District[[input$variable]]),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = .7,
                    bringToFront = TRUE),
                  group = "regions",
                  layerId = ~NAME,
                  label = ~ popup_duh,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto",
                                              opacity = .9)) %>%
      addPolygons(data = Testing_District,
                  fillColor = "#01ff70",
                  fillOpacity = 1,
                  weight = 1,
                  color = "white",
                  stroke = TRUE,
                  layerId = ~Dist_Code,
                  group = ~NAME) %>%
      hideGroup(group = Testing_District$NAME) %>%
      addLegend(pal = paletteNum_noNA, 
                values = Testing_District[[input$variable]], 
                title = input$variable, 
                position = 'topright',
                opacity = 0.7,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) 
   
  }) 

  
  proxy <- leafletProxy("myMap")
  

  
  #create empty vector to hold all click ids
  selected_dist <- reactiveValues(groups = vector())
  
  
  
  observeEvent(input$myMap_shape_click,{
    if(input$myMap_shape_click$group == "regions"){
      selected_dist$groups <- c(selected_dist$groups, input$myMap_shape_click$id)
      proxy %>% showGroup(group = input$myMap_shape_click$id)
    } else {
      selected_dist$groups <- setdiff(selected_dist$groups, input$myMap_shape_click$group)
      proxy %>% hideGroup(group = input$myMap_shape_click$group)
    }
    updateSelectizeInput(session,
                         inputId = "selected_locations",
                         label = "Districts selected: (Click on the box or map to select districts)",
                         choices = Testing_District$NAME,
                         selected = selected_dist$groups)
  })
  
  observeEvent(input$variable,{ 
    selected_dist$groups <- vector()
    updateSelectizeInput(session,
                         inputId = "selected_locations",
                         label = "Districts selected: (Click on the box or map to select districts)",
                         choices = Testing_District$NAME,
                         selected = selected_dist$groups)
    
  })
  
 highest <- reactive({
   req(input$variable)
          Testing_District_Data %>% select(., c("NAME", input$variable)) %>% top_n(., 3) %>% arrange(desc(.[,2]))
  })
 lowest <- reactive({
   req(input$variable)
   Testing_District_Data %>% select(., c("NAME", input$variable)) %>% top_n(., -3) %>% arrange(.[,2])
 })
 
  
 output$highest_one <- renderValueBox({
   valueBox(
     value = tags$p(highest()[1,1], style = "font-size: 200%; margin-top: 1em;"),
     subtitle = NULL,
     color = "success",
     icon = icon("1", verify_fa = FALSE),
     elevation = 5
   )
 })
 
 # to pull their avg. score = highest()[1,2], preferred not having it in subtitle
  
 output$highest_two <-  renderValueBox({
   valueBox(
     value = tags$p(highest()[2,1], style = "font-size: 200%; margin-top: 1em;"),
     subtitle = NULL,
     color = "success",
     icon = icon("2", verify_fa = FALSE),
     elevation = 3
   )
 })
  
 output$highest_three <- renderValueBox({
   valueBox(
     value = tags$p(highest()[3,1], style = "font-size: 200%; margin-top: 1em;"),
     subtitle = NULL,
     color = "success",
     icon = icon("3", verify_fa = FALSE),
     elevation = 0
   )
 })
  
 
 
 output$lowest_one <- renderValueBox({
   valueBox(
     value = tags$p(lowest()[1,1], style = "font-size: 200%; margin-top: 1em;"),
     subtitle = NULL,
     color = "danger",
     icon = icon("1", verify_fa = FALSE),
     elevation = 5
   )
 })
 
 output$lowest_two <-  renderValueBox({
   valueBox(
     value = tags$p(lowest()[2,1], style = "font-size: 200%; margin-top: 1em;"),
     subtitle = NULL,
     color = "danger",
     icon = icon("2", verify_fa = FALSE),
     elevation = 3
   )
 })
 
 output$lowest_three <- renderValueBox({
   valueBox(
     value = tags$p(lowest()[3,1], style = "font-size: 200%; margin-top: 1em;"),
     subtitle = NULL,
     color = "danger",
     icon = icon("3", verify_fa = FALSE),
     elevation = 0
   )
 })
 
  
  
  observeEvent(input$selected_locations, {
    removed_via_selectInput <- setdiff(selected_dist$groups, input$selected_locations)
    added_via_selectInput <- setdiff(input$selected_locations, selected_dist$groups)
    
    if(length(removed_via_selectInput) > 0){
      selected_dist$groups <- input$selected_locations
      proxy %>% hideGroup(group = removed_via_selectInput)
    }
    
    if(length(added_via_selectInput) > 0){
      selected_dist$groups <- input$selected_locations
      proxy %>% showGroup(group = added_via_selectInput)
    }
  }, ignoreNULL = FALSE)
  
  selected_data <- reactive({
    
    req(selected_dist$groups)
    
    selected_data <- Testing_District_Data[,c(5, 1, 16:73)] %>%
      filter(.$NAME %in% selected_dist$groups)
    
    if(input$variable == "Engagement"){
      selected_data <- selected_data[,c(1, 3:13)]
    }
    else if(input$variable == "Environment_Factors"){
      selected_data <- selected_data[,c(1, 14:18)]
    }
    else if(input$variable == "Resource_Factors"){
      selected_data <- selected_data[,c(1, 19:21)]
    }
    else if(input$variable == "Interpersonal_Factors"){
      selected_data <- selected_data[,c(1, 22:27)]
    }
    else if(input$variable == "Perceived_Organizational_Support"){
      selected_data <- selected_data[,c(1, 28:31)]
    }
    else if(input$variable == "Leadership"){
      selected_data <- selected_data[,c(1, 32:36)]
    }
    else if(input$variable == "Justice"){
      selected_data <- selected_data[,c(1, 37:41)]
    }
    else if(input$variable == "Voice"){
      selected_data <- selected_data[,c(1, 42:47)]
    }
    else if(input$variable == "Inentions_to_Leave"){
      selected_data <- selected_data[,c(1, 48:51)]
    }
    else{
      selected_data <- selected_data[,c(2,1)]
    }
  })
  
  
  output$maptable = DT::renderDataTable({
    
    if(input$'dark_mode'){
      datatable(selected_data(), options=list(
        pageLength = 3,
        lengthMenu = c(5, 10, 15, 20),
        dom = 'tp',
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"))) %>% formatStyle(columns = colnames(selected_data()),backgroundColor = "#282828",color = "#fff",target = 'row')
    }else{
      datatable(selected_data(), options=list(
        pageLength = 3,
        lengthMenu = c(5, 10, 15, 20),
        dom = 'tp'))
      }
    })
  
  
  
  output$mapbar = renderHighchart({
    selected_data <- selected_data() %>%
      pivot_longer(!NAME, names_to = "Item", values_to = "Rating")
    
    set.seed(43)
    colz <- rainbow(12) %>% substr(., 0, 7) %>% sample(., replace = FALSE)
    
    Yus <- hchart(selected_data, "column", hcaes(Item, Rating, group = NAME)) %>% 
            hc_xAxis(title = list(text = "")) %>% 
            hc_yAxis(title = list(text = "")) %>%
            hc_colors(colz)
    #Note: Had to add here instead of w/ other css if changes. For whatever reason, didn't like: 
                          # ".highcharts-axis-labels.highcharts.xaxis-labels{fill: #fff} or any variation. Could be mistake on my end.
    if(input$'dark_mode'){
    Yus <- Yus %>%
        hc_yAxis(labels = list(style = list(color = "white"))) %>%
        hc_xAxis(labels = list(style = list(color = "white"))) %>%
        hc_legend(itemStyle = list(color = "white"))
    Yus
    }
    else{
      Yus
    }
  })
  

  
  
  ########## Pivot Table Tab ################    
  
  #     output$pivot <- renderRpivotTable({ rpivotTable(U_Data, sorters = "
  # function(attr) {
  # var sortAs = $.pivotUtilities.sortAs;
  # if (attr == \"Q1\", \"Q2\") { return sortAs([\"Very Dissatisfied\", \"Dissatisfied\", \"Unsure\", \"Satisfied\", \"Very Satisfied\"]); }
  # }")
  #     })
  
  # Have to go through and add each item and/or demographic to the order I would want them to appear.
  # Above example just has the first 2 items so far.
  # May be possible to define factor level first in U_Data. Probably easier. Will fix when I have time.
  
  
}




# Additional considerations
#   - Swap out DT for Reactable. Comments have noted it may work better with toggling of light/dark mode. However, no in-built "on click" event (i.e., learn java)
#   - Alternatively, and just for learning in general, look into proxy tabling. Current solution is annoying as it re-renders the table.
#   - Waiter is ... messed up. Can either be relative and work on most elements, or fixed to work on map. Possible to solve with an If statement; however, currently unsure if loading will even be an issue.
#   - Caching! I'm not positive that the variables I tell it to cache are all necessary. Potentially cause issues.
#       - One potential, albeit hacky workaround, save all charts/graphs (regression/comparison tabs) to lists, and just call them rather than plotting the graph in-real time.
#   - Order in highcharter plots. Unfortunately, if anything like my experience with InputPicker, I will need to isolate the "map-click" event w/ js and store to a seperate list. 
#       - Long-term consideration. Slightly more complicated than previous solution as current map allows for on-click or selectize.

