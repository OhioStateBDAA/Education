function(input, output, session){
  width <- runjs("Shiny.onInputChange('width', screen.width);")
  height <- runjs("Shiny.onInputChange('height', screen.height);")
  # SCREEN_WIDTH_RATIO <- input$width/1920
  # observe({
  #   print(input$width)
  #   print(input$height)
  # })
  
  # reactive value containing user's authentication status
  user_input <- reactiveValues(authenticated = TRUE, valid_credentials = FALSE, 
                               user_locked_out = FALSE, status = "")
  
  #group <- reactiveVal(NULL)
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        fluidRow(
          column(width = 3, offset = 4,
                 br(), br(), br(), br(),
                 uiOutput("uiLogin"),
                 uiOutput("pass")
          )
        )
      )
    } else {
      tabItems(
        tabItem("map",
                div(class="outer",
                    tags$head(
                      # Include our custom CSS
                      includeCSS("styles.css")
                    ),
                    leafletOutput("columbus_map", width="100%", height="100%")
                ),
                #valueBoxOutput("zest", width = 3),
                valueBoxOutput("sqft", width = 3),
                valueBoxOutput("rooms", width = 3),
                box(solidHeader = T, status="danger",
                    htmlOutput("houseInfo"), width = 3)
        ),
        tabItem("database",
                div(style="overflow-y:scroll",
                    div(style="display:inline-block", actionButton("all", "Show all columns")),
                    div(style="display:inline-block", actionButton("primary", "Show primary columns")),
                    DT::dataTableOutput("db")
                )
        ))
    }
  })
  
  output$sidebar_ui <- renderUI({
    if(user_input$authenticated == TRUE){
      dashboardSidebar(
        sidebarMenu(
          menuItem("Mapping", tabName="map", icon=icon("map")),
          menuItem("Databases", icon=icon("database"),
                   menuSubItem("Original", tabName = "database", icon=icon("check-square-o")))
        ),
        if(is.null(input$source)){
          radioButtons("source", label = "Choose a source:", choices = c("Sheriff"), inline = T, selected= "Sheriff")
        } else {
          radioButtons("source", label = "Choose a source:", choices = c("Sheriff"), inline = T, selected= input$source)
        }
        ,
        sidebarMenu(
          menuItem("Zip Menu", icon=icon("map-pin"),
                   #checkboxInput('select_all', 'Select all:', value=T),
                   actionLink("selectall","Select All"),
                   checkboxGroupInput("zipChoices", label = "Choose zipcodes:", choices = sort(unique(sheriff$Zip)), selected = unique(sheriff$Zip)))
        ),
        sliderInput("bedrooms", label = "Bedrooms:", min=min(sheriff$Bedrooms,na.rm=T), max=max(sheriff$Bedrooms,na.rm=T),
                    value=c(min(sheriff$Bedrooms,na.rm=T),max(sheriff$Bedrooms,na.rm=T)),step=1),
        sliderInput("finishedsqft", label = "Finished sqft:", min=min(sheriff$FnAreaAbvGrd,na.rm=T), max=max(sheriff$FnAreaAbvGrd,na.rm=T),
                    value=c(min(sheriff$FnAreaAbvGrd,na.rm=T),max(sheriff$FnAreaAbvGrd,na.rm=T))),
        uiOutput("conditional_filters_status"),
        uiOutput("conditional_filters_saledate"),
        uiOutput("conditional_filters_minbid"),
        div(style="display:inline-block", actionButton("clear_filter", "Clear Filter")),
        div(style="display:inline-block", actionButton("filter", "Filter"))
      )
    }
  })
  
  observeEvent(input$selectall, {
    zips <- getDataset(input$source)
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"zipChoices","Choose zipcode(s):",choices=sort(unique(zips$Zip)))
    }
    else
    {
      updateCheckboxGroupInput(session,"zipChoices","Choose zipcode(s):",choices=sort(unique(zips$Zip)),selected=sort(unique(zips$Zip)))
    }
  })
  
  # authenticate user by:
  #   1. checking whether their user name and password are in the credentials 
  #       data frame and on the same row (credentials are valid)
  #   2. if credentials are valid, retrieve their lockout status from the data frame
  #   3. if user has failed login too many times and is not currently locked out, 
  #       change locked out status to TRUE in credentials DF and save DF to file
  #   4. if user is not authenticated, determine whether the user name or the password 
  #       is bad (username precedent over pw) or he is locked out. set status value for
  #       error message code below
  observeEvent(input$login_button, {
    showNotification("Logging in..", type = "warning")
    credentials <- s3readRDS("credentials/credentials.rds", "restore614")
    
    row_username <- which(credentials$user == input$user_name)
    row_password <- which(credentials$pw == digest(input$password)) # digest() makes md5 hash of password
    
    # if user name row and password name row are same, credentials are valid
    #   and retrieve locked out status
    if (length(row_username) == 1 && 
        length(row_password) >= 1 &&  # more than one user may have same pw
        (row_username %in% row_password)) {
      user_input$valid_credentials <- TRUE
      user_input$user_locked_out <- credentials$locked_out[row_username]
    }
    
    # if user is not currently locked out but has now failed login too many times:
    #   1. set current lockout status to TRUE
    #   2. if username is present in credentials DF, set locked out status in 
    #     credentials DF to TRUE and save DF
    if (input$login_button == num_fails_to_lockout & 
        user_input$user_locked_out == FALSE) {
      
      user_input$user_locked_out <- TRUE
      
      if (length(row_username) == 1) {
        credentials$locked_out[row_username] <- TRUE
        
        s3saveRDS(credentials, "credentials/credentials.rds", "restore614")
      }
    }
    
    # if a user has valid credentials and is not locked out, he is authenticated      
    if (user_input$valid_credentials == TRUE & user_input$user_locked_out == FALSE) {
      user_input$authenticated <- TRUE
    } else {
      user_input$authenticated <- FALSE
    }
    
    # if user is not authenticated, set login status variable for error messages below
    if (user_input$authenticated == FALSE) {
      if (user_input$user_locked_out == TRUE) {
        user_input$status <- "locked_out"  
      } else if (length(row_username) > 1) {
        user_input$status <- "credentials_data_error"  
      } else if (input$user_name == "" || length(row_username) == 0) {
        user_input$status <- "bad_user"
      } else if (input$password == "" || length(row_password) == 0) {
        user_input$status <- "bad_password"
      }
    }
    
    if(user_input$authenticated == TRUE){
      # bucket <- get_bucket('restore614')
      # group_name <- credentials[credentials$user == input$user_name, "group"]
      # group(group_name)
      # folder_exists <- F
      # taxlien_exists <- F
      # 
      # for(i in 1:length(bucket)){
      #   path <- bucket[i]$Contents$Key
      #   if(grepl(paste0("group_files/",group_name,"/"), path)){
      #     folder_exists <- T
      #   }
      # }
      # if(!folder_exists){
      #   put_folder(paste0("group_files/", group_name), "restore614")
      # }
      # 
      # output$login_info <- renderMenu({
      #   dropdownMenu(
      #     notificationItem(paste0("Logged in as: ", input$user_name)))
      # })
      # 
      # for(i in 1:length(bucket)){
      #   path <- bucket[i]$Contents$Key
      #   if(grepl(paste0("group_files/", group_name, "/personal.rds"), path)){
      #     taxlien_exists <- T
      #   }
      # }
      # if(taxlien_exists) {
      #   personal$data <- s3readRDS(paste0("group_files/",group_name,"/personal.rds"),bucket="restore614")
      # }
      showNotification("Login success!", type = "message")
    }
  })
  
  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({
    wellPanel(
      textInput("user_name", "User Name:"),
      
      passwordInput("password", "Password:"),
      
      actionButton("login_button", "Log in", width="100%")
    )
  })
  
  # red error message if bad credentials
  output$pass <- renderUI({
    if (user_input$status == "locked_out") {
      h5(strong(paste0("Your account is locked because of too many\n",
                       "failed login attempts. Contact administrator."), style = "color:red"), align = "center")
    } else if (user_input$status == "credentials_data_error") {    
      h5(strong("Credentials data error - contact administrator!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_user") {
      h5(strong("User name not found!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_password") {
      h5(strong("Incorrect password!", style = "color:red"), align = "center")
    } else {
      ""
    }
  })  
  
  
  getDataset <- function(choice){
    switch(choice,
           "Sheriff" = sheriff,
           "TaxLien" = taxlien)
  }
  
  columbus_map <- function(df){
    icons <- awesomeIcons(
      icon = 'ios-home',
      iconColor = 'black',
      library = 'ion',
      markerColor = "red"
    )
    leafletProxy("columbus_map", session=session, data=df) %>%
      removeWebGLHeatmap(layerId = "heat") %>%
      clearGroup("markers") %>%
      addAwesomeMarkers(lng=~Longitude, lat = ~Latitude, group = "markers", icon=icons, layerId=df$Address, label = ~Address)
  }
  
  original_dt <- function(df){
    DT::datatable(df,
                  class   = 'cell-border stripe compact hover',
                  escape  = F, selection = 'single',
                  rownames = FALSE,
                  options = list(
                    scrollX = T,
                    #autoWidth  = T,
                    #targets = 5,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 100 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
                      "}"),
                    rowCallback = JS("function(r,d) {$(r).attr('height', '50px')}"),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().body()).css({'font-size': '80%'});",
                      "}"),
                    LengthMenu = c(5, 10, 15),
                    #columnDefs = list(list(
                    #className = 'dt-body-left',
                    #targets = 5)),
                    pageLength = 5, server = T))
  }
  
  #data <- reactiveVal(taxlien)
  
  observeEvent(input$source, {
    newdata <- switch(input$source,
                      "Sheriff" = sheriff,
                      "TaxLien" = taxlien)
    
    updateCheckboxGroupInput(session, "zipChoices", label = "Choose zipcode(s):", 
                             choices = sort(unique(newdata$ZipCode)), selected = unique(newdata$ZipCode))
    updateCheckboxGroupInput(session, "cols", label = "Columns to show:",
                             choices = colnames(newdata), selected = colnames(newdata))
    columbus_map(newdata)
    
    if(input$source == "Sheriff"){
      
      min_bid <- as.numeric(gsub("\\$|,", "", sheriff$OpeningBid))
      output$conditional_filters_status <- renderUI({
        selectInput("sheriff_status", "Sheriff Status:",
                    choices=unique(sheriff$Status), selected="ACTIVE")
      })
      output$conditional_filters_saledate <- renderUI({
        dateRangeInput("sheriff_sale_date", "Sheriff Sale Date", min=min(sheriff$SellDate, na.rm = T), max=max(sheriff$SellDate, na.rm=T),
                       start = min(sheriff$SellDate, na.rm = T), end = max(sheriff$SellDate, na.rm=T))
      })
      output$conditional_filters_minbid <- renderUI({
        sliderInput("sheriff_min_bid", "Sheriff Min Bid:",
                    min=min(min_bid, na.rm = T),max=max(summary(sheriff$OpeningBid)[5], na.rm=T),
                    value=c(min(min_bid, na.rm = T),max(summary(sheriff$OpeningBid)[5], na.rm=T)))
      })
    } else {
      output$conditional_filters_status <- renderUI({})
      output$conditional_filters_saledate <- renderUI({})
      output$conditional_filters_minbid <- renderUI({})
    }
    
  })
  
  
  observeEvent(input$zipChoices, {
    zip_data <- subset(sheriff, Zip %in% input$zipChoices)
    
    updateSliderInput(session, "bedrooms", label = "Bedrooms:", min=min(zip_data$Bedrooms,na.rm=T), max=max(zip_data$Bedrooms,na.rm=T),
                      value=c(min(zip_data$Bedrooms,na.rm=T),max(zip_data$Bedrooms,na.rm=T)),step=1)
    updateSliderInput(session, "finishedsqft", label = "Finished sqft:", min=min(zip_data$FnAreaAbvGrd,na.rm=T), max=max(zip_data$FnAreaAbvGrd,na.rm=T),
                      value=c(min(zip_data$FnAreaAbvGrd,na.rm=T),max(zip_data$FnAreaAbvGrd,na.rm=T)))
  })
  
  output$columbus_map <- renderLeaflet({
    icons <- awesomeIcons(
      icon = 'ios-home',
      iconColor = 'black',
      library = 'ion',
      markerColor = 'red'
    )
    leaflet(sheriff) %>%
      setMaxBounds(lat1 = 39.45, lat2 = 41.39, lng1 = -83.66, lng2 = -81.45) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lng = -83.029, lat = 39.981, zoom = 11) %>%
      addAwesomeMarkers(lng=~Longitude, lat = ~Latitude, group = "markers", icon=icons, layerId=sheriff$Address, label = ~Address)
    
    # leaflet(taxlien, options = providerTileOptions(minZoom=11, maxZoom=15)) %>%
    #   setMaxBounds(lat1 = 39.45, lat2 = 41.39, lng1 = -83.66, lng2 = -81.45) %>%
    #   addProviderTiles("OpenStreetMap.Mapnik") %>%
    #   setView(lng = -83.029, lat = 39.981, zoom = 11) %>%
    #   addWebGLHeatmap(lng = ~Longitude, lat= ~Latitude, size = 3000, layerId = "heat")
  })
  
  # observe({
  #   updateCheckboxGroupInput(
  #     session, 'zips', choices = unique(data()$Zip),
  #     if(!is.null(input$select_all)){
  #       selected = if (input$select_all) unique(data()$Zip)
  #     }
  #     
  #   )
  # })
  
  observeEvent(input$filter, {
    print(input$zipChoices)
    df <- getDataset(input$source)
    z <- input$zipChoices
    # bed <- if(input$bedrooms == "") data()$bedrooms else input$bedrooms
    # bath <- if(input$bathrooms == "") data()$bathrooms else input$bathrooms
    
    filtered_data <- subset(df, df$Zip %in% z & 
                              df$Bedrooms >= input$bedrooms[1] & df$Bedrooms <= input$bedrooms[2] &
                              df$FnAreaAbvGrd >= input$finishedsqft[1] & df$FnAreaAbvGrd <= input$finishedsqft[2] &
                              if(input$source == "Sheriff"){df$Status == input$sheriff_status & 
                                  df$SellDate >= as.Date(input$sheriff_sale_date[1], origin = "1970-01-01", format = "%m/%d/%Y") & df$SellDate <= as.Date(input$sheriff_sale_date[2], origin = "1970-01-01", format = "%m/%d/%Y") & 
                                  df$OpeningBid >= input$sheriff_min_bid[1] & df$OpeningBid <= input$sheriff_min_bid[2]}else{T})
    
    
    #if(input$source == "Sheriff"){
    #  filtered_data <- subset(filtered_data, df$Status == input$sheriff_status & 
    #                            df$SellDate >= as.Date(input$sheriff_sale_date[1], origin = "1970-01-01", format = "%m/%d/%Y") & df$SellDate <= as.Date(input$sheriff_sale_date[2], origin = "1970-01-01", format = "%m/%d/%Y") & 
    #                            df$OpeningBid >= input$sheriff_min_bid[1] & df$OpeningBid <= input$sheriff_min_bid[2])
    #}
    if(nrow(filtered_data) == 0){
      showNotification("Your filters returned no data. Please try different filter settings.",
                       type="warning")
    } else {
      
      columbus_map(filtered_data)
     # data(filtered_data)
    }
  })
  
  observeEvent(input$clear_filter, {
    newdata <- switch(input$source,
                      "Sheriff" = sheriff,
                      "TaxLien" = taxlien)
    #data(newdata)
    
    updateCheckboxGroupInput(session, "zipChoices", label = "Choose zipcode(s):", 
                             choices = sort(unique(newdata$Zip)), selected = unique(newdata$Zip))
    updateSliderInput(session, "bedrooms", label = "Bedrooms:", min=min(newdata$Bedrooms,na.rm=T), max=max(newdata$Bedrooms,na.rm=T),
                      value=c(min(newdata$Bedrooms,na.rm=T),max(newdata$Bedrooms,na.rm=T)),step=1)
    updateSliderInput(session, "finishedsqft", label = "Finished sqft:", min=min(newdata$FnAreaAbvGrd,na.rm=T), max=max(newdata$FnAreaAbvGrd,na.rm=T),
                      value=c(min(newdata$FnAreaAbvGrd,na.rm=T),max(newdata$FnAreaAbvGrd,na.rm=T)))
    if(input$source == "Sheriff"){
      updateSelectInput(session, "sheriff_status", "Sheriff Status:",choices=unique(sheriff$Status), selected="ACTIVE")
      updateDateRangeInput(session, "sheriff_sale_date", "Sheriff Sale Date:", min=min(sheriff$SellDate, na.rm = T), max=max(sheriff$SellDate, na.rm=T),
                           start = min(sheriff$SellDate, na.rm = T), end = max(sheriff$SellDate, na.rm=T))
      updateSliderInput(session, "sheriff_min_bid", "Sheriff Min Bid:",
                        min=min(newdata$OpeningBid, na.rm = T),max=max(summary(sheriff$OpeningBid)[5], na.rm=T),
                        value=c(min(newdata$OpeningBid, na.rm = T),max(summary(sheriff$OpeningBid)[5], na.rm=T)))
    }
    columbus_map(newdata)
  })
  
  # observeEvent(input$bedrooms, {
  #   output$text <- renderPrint({
  #     input$bedrooms
  #   })
  # })
  
  # output$zest <- renderValueBox({
  #   valueBox(paste("$",round(median(sheriff$zestimate.amount,na.rm = T),0)),
  #            "Average Zestimate", icon=icon("dollar"),
  #            color="red")
  # })
  
  output$sqft <- renderValueBox({
    valueBox(round(median(sheriff$FnAreaAbvGrd, na.rm = T),0),
             "Average Square Footage", icon=icon("square"),
             color="red")
  })
  
  output$rooms <- renderValueBox({
    valueBox(round(mean(sheriff$Bedrooms,na.rm = T),0),
             "Average # Bedrooms", icon=icon("hotel"),
             color="red")
  })
  
  # output$house <- renderBox({
  #   infoBox("House data")
  # })
  
  observeEvent(input$columbus_map_marker_click, {
    click <- input$columbus_map_marker_click
    df <- sheriff[sheriff$Address == click$id,]
    output$houseInfo <- renderPrint({
      HTML("<h5><center>Information for: </center></h3>", "<center>", str_to_title(df[, "Address"]), "</center>", 
           "<div style=text-align:center;>", paste0("<img src = ", paste0("clean/", as.character(df[,"LotNo1"]), "_clean.png"),
                                                    ">"), "</div>","<hr/>",
           "<strong> Year Built: </strong>", df[, "YearBuilt"], "<br/>",
           "<strong> # Bedrooms: </strong>", df[, "Bedrooms"], "<br/>",
           "<strong> Finished Sq Footage: </strong>", df[, "FnAreaAbvGrd"], "<br/>",
           "<strong> Sheriff Min Bid: </strong>", dollar(df[, "OpeningBid"]), "<br/>",
           "<strong> Appraised: </strong>", as.character(df[, "Appraised"]), "<br/>",
           "<strong> Sheriff Sale Date: </strong>", as.character(df[, "SellDate"])
           #"<br/>",
           #"<strong> Zestimate: </strong>", paste0("$",df[, "zestimate.amount"]), "<br/>",
           #"<center>", paste0("<a href=\"", df[, "links.homedetails"],"\" target=\"_blank\">Home details</a>"),"</center>"
      )
    })
  })
  
  output$db <- DT::renderDataTable({
    original_dt(sheriff)
  })
  
  observeEvent(input$all, {
    output$db <- DT::renderDataTable({
      original_dt(sheriff)
    })
  })
  
  # observeEvent(input$primary, {
  #   df <- switch(input$source,
  #                "TaxLien" = sheriff[,c("Address", "Zip", "bathrooms", "bedrooms", "zestimate.amount")],
  #                "Sheriff" = sheriff[,c("Address", "Zip", "Type", "Status", "SellDate", "Appraised",
  #                                      "OpeningBid","Deposit", "Defendant", "bathrooms", "bedrooms", "zestimate.amount")]
  #   )
  #   output$db <- DT::renderDataTable({
  #     original_dt(df)
  #   })
  # })
  # 
  # initial_form_financials <- data.frame("$" = rep(0,10), "% of ARV" = rep(0,10), stringsAsFactors = F)
  # rownames(initial_form_financials) <- c("ARV", "Purchase Price", "Buy-Hold Cost", "Sell Costs", "TRC",
  #                                        "Total Costs", "Estimated Profit", "Project Weeks", "Profit/Project Month",
  #                                        "TRC/Square Ft")
  # 
  # financials <- reactiveVal(initial_form_financials)
  # 
  # 
  # observeEvent(input$form_financials,{
  #   initial_form_financials <- data.frame("$" = rep(0,10), "% of ARV" = rep(0,10), stringsAsFactors = F)
  #   rownames(initial_form_financials) <- c("ARV", "Purchase Price", "Buy-Hold Cost", "Sell Costs", "TRC",
  #                                          "Total Costs", "Estimated Profit", "Project Weeks", "Profit/Project Month",
  #                                          "TRC/Square Ft")
  #   if(is.null(input$form_financials)){}
  #   else if(!identical(initial_form_financials,input$form_financials)){
  #     # hot.to.df function will convert your updated table into the dataframe
  #     form <- as.data.frame(hot_to_r(input$form_financials))
  #     
  #     form[6,1] <- sum(form[2:5,1])
  #     form[7,1] <- form[1,1]-form[6,1]
  #     # Add some test cases
  #     if(form[1,1] != 0){
  #       form[,2] <- round(form[,1]/form[1,1]*100,0)
  #     }
  #     form[8,2] <- ""
  #     if(form[8,1] != 0){
  #       form[9,1] <- form[7,1]/(form[8,1]/4)
  #     }
  #     if(!is.null(input$db_rows_selected)){
  #       form[10,1] <- form[5,1]/sheriff[input$db_rows_selected,"finishedSqFt"]
  #     }
  #     
  #     colnames(form) <- c("$", "% of ARV")
  #     financials(form)
  #   }
  # })
  # 
  # 
  # 
  # observeEvent(input$db_rows_selected, {
  #   
  #   if(!identical(character(0),input$db_rows_selected)){
  #     if(is.null(personal$data)| !(sheriff[input$db_rows_selected, "Address"] %in% personal$data$Address)){
  #       empty_form_financials <- data.frame("$" = rep(0,10), "% of ARV" = rep(0,10), stringsAsFactors = F)
  #       rownames(empty_form_financials) <- c("ARV", "Purchase Price", "Buy-Hold Cost", "Sell Costs", "TRC",
  #                                            "Total Costs", "Estimated Profit", "Project Weeks", "Profit/Project Month",
  #                                            "TRC/Square Ft")
  #       financials(empty_form_financials)
  #       # Initiate your table
  #       #initial_form_financials <- reactive({empty_form_financials})
  #       
  #       #calculated_form_financials <- calc_form(empty_form_financials)
  #       
  #       output$background_info <- renderUI({
  #         box(solidHeader=T, width = 12,
  #             column(6,
  #                    textInput("background_source", label = "Source:", value=input$source)),
  #             column(6,
  #                    selectInput("background_status", label = "Status:", choices=c("New", "Active", "Hot", "No", "Refer", "Negotiating", "Purchased"), selected="New", width="100%")),
  #             dateInput("updated", label = "Updated:", width = "50%"))
  #       })
  #       
  #       output$form_description <- renderRHandsontable({
  #         empty_form_description <- data.frame("Comment" = rep("",6), stringsAsFactors = F)
  #         rownames(empty_form_description) <- c("Neighborhood", "Lot", "Parking", "Sourcing Risk", "Selling Risk",
  #                                               "Repair Risk")
  #         rhandsontable(empty_form_description, colHeaders = F, rowHeaderWidth = 100*SCREEN_WIDTH_RATIO, width = 600*SCREEN_WIDTH_RATIO) %>%
  #           hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
  #           hot_cols(colWidths = 295*SCREEN_WIDTH_RATIO) %>%
  #           hot_rows(rowHeights = 25)
  #       })
  #       
  #       output$form_financials <- renderRHandsontable({
  #         rhandsontable(financials(), rowHeaderWidth = 130*SCREEN_WIDTH_RATIO, width = 600*SCREEN_WIDTH_RATIO) %>%
  #           hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
  #           hot_cols(colWidths = 150*SCREEN_WIDTH_RATIO) %>%
  #           hot_rows(rowHeights = 25) %>%
  #           hot_col(2, readOnly = T)
  #       })
  #       
  #       output$form_comments <- renderUI({
  #         textAreaInput("comments", label = "Comments")
  #       })
  #     } else {
  #       print("Here")
  #       personal_data <- personal$data[personal$data$Address == sheriff[input$db_rows_selected, "Address"],]
  #       form_financials <- data.frame(t(personal_data[1,c("ARV", "Purchase Price", "Buy-Hold Cost", "Sell Costs", "TRC",
  #                                                         "Total Costs", "Estimated Profit", "Project Weeks", "Profit/Project Month",
  #                                                         "TRC/Square Ft")]), stringsAsFactors = F)
  #       form_financials <- cbind.data.frame(form_financials, data.frame("% of ARV" = rep(0,10)), stringsAsFactors = F)
  #       form_financials[,1] <- as.numeric(form_financials[,1])
  #       rownames(form_financials) <- c("ARV", "Purchase Price", "Buy-Hold Cost", "Sell Costs", "TRC",
  #                                      "Total Costs", "Estimated Profit", "Project Weeks", "Profit/Project Month",
  #                                      "TRC/Square Ft")
  #       
  #       financials(form_financials)
  #       # Initiate your table
  #       #initial_form_financials <- reactive({form_financials})
  #       
  #       output$form_financials <- renderRHandsontable({
  #         rhandsontable(financials(), rowHeaderWidth = 130*SCREEN_WIDTH_RATIO, width = 600*SCREEN_WIDTH_RATIO) %>%
  #           hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
  #           hot_cols(colWidths = 150*SCREEN_WIDTH_RATIO) %>%
  #           hot_rows(rowHeights = 25) %>%
  #           hot_col(2, readOnly = T)
  #       })
  #       
  #       #calculated_form_financials <- calc_form(initial_form_financials)
  #       
  #       output$background_info <- renderUI({
  #         box(solidHeader=T, width = 12,
  #             column(6,
  #                    textInput("background_source", label = "Source:", value=personal_data$Source)),
  #             column(6,
  #                    selectInput("background_status", label = "Status:", choices=c("New", "Active", "Hot", "No", "Refer", "Negotiating", "Purchased"), selected=personal_data$Status)),
  #             dateInput("updated", label = "Updated:", width = "50%", value=as.Date(as.numeric(personal_data$Updated), origin="1970-01-01")))
  #       })
  #       
  #       output$form_description <- renderRHandsontable({
  #         form_description <- data.frame(t(personal_data[1,c("Neighborhood", "Lot", "Parking", "Sourcing Risk", "Selling Risk",
  #                                                            "Repair Risk")]))
  #         rownames(form_description) <- c("Neighborhood", "Lot", "Parking", "Sourcing Risk", "Selling Risk",
  #                                         "Repair Risk")
  #         form_description[,1] <- as.character(form_description[,1])
  #         
  #         rhandsontable(form_description, colHeaders = F, rowHeaderWidth = 100*SCREEN_WIDTH_RATIO, width = 500*SCREEN_WIDTH_RATIO) %>%
  #           hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
  #           hot_cols(colWidths = 295*SCREEN_WIDTH_RATIO) %>%
  #           hot_rows(rowHeights = 25)
  #       })
  #       
  #       
  #       output$form_comments <- renderUI({
  #         textAreaInput("comments", label = "Comments", value = personal_data$Comments)
  #       })
  #       
  #       
  #     }
  #   } else {
  #     output$background_info <- renderUI({})
  #     output$form_description <- renderRHandsontable({})
  #     output$form_financials <- renderRHandsontable({})
  #     output$form_comments <- renderUI({})
  #   }
  #   
  #   lon = as.numeric(sheriff[input$db_rows_selected, "Longitude"])
  #   lat = as.numeric(sheriff[input$db_rows_selected, "Latitude"])
  #   print(lon)
  #   print(lat)
  #   
  #   output$location <- renderLeaflet({
  #     leaflet(options = providerTileOptions(minZoom=12, maxZoom=16)) %>%
  #       setView(lat = lat, lng = lon, zoom=12) %>%
  #       addProviderTiles("OpenStreetMap.Mapnik") %>%
  #       clearGroup("location_marker") %>%
  #       addMarkers(lat = lat, lng = lon, "location_marker")
  #   })
  #   
  #   output$help_submit <- renderUI({
  #     fluidRow(
  #       h4("Personal Data for:"),
  #       HTML("<center>",sheriff[input$db_rows_selected,"Address"],"</center>"),
  #       leafletOutput("location", height = 150),
  #       hr(),
  #       h4("Help"),
  #       helpText("Please enter personal data based on your own findings into this form. When finished,
  #                press 'submit'. Your data will be kept in the 'personal' database tab."),
  #       hr(),
  #       actionButton("submit_form", label="Submit", icon=icon("check"), width="100%")
  #       )
  #     
  #   })
  #   
  #   
  # })
  
  # observeEvent(input$submit_form, {
  #   address <- sheriff[input$db_rows_selected,"Address"]
  #   source <- input$background_source
  #   status <- input$background_status
  #   updated <- Sys.Date()
  #   description <- as.data.frame(hot_to_r(input$form_description))
  #   financials <- as.data.frame(hot_to_r(input$form_financials))
  #   comments <- input$comments
  #   
  #   cols <- c("Address", "Source", "Status", "Updated", "Neighborhood", "Lot", "Parking", "Sourcing Risk", "Selling Risk",
  #             "Repair Risk", "ARV", "Purchase Price", "Buy-Hold Cost", "Sell Costs", "TRC",
  #             "Total Costs", "Estimated Profit", "Project Weeks", "Profit/Project Month",
  #             "TRC/Square Ft", "Comments")
  #   personal_data <- c(address, source, status, updated, description[,1], financials[,1], comments)
  #   names(personal_data) <- cols
  #   if(!is.null(personal$data) & !(address %in% personal$data$Address)){
  #     newdata <- rbind.data.frame(personal$data, personal_data, stringsAsFactors = F)
  #   } else if(address %in% personal$data$Address){
  #     newdata <- personal$data
  #     newdata[newdata$Address == address,] <- personal_data
  #   } else {
  #     newdata <- data.frame(as.list(personal_data), stringsAsFactors = F)
  #   }
  #   colnames(newdata) <- cols
  #   newdata[,"Updated"] <- as.Date(as.numeric(newdata[,"Updated"]), origin="1970-01-01")
  #   s3saveRDS(newdata, paste0("group_files/", group(), "/personal.rds"), "restore614")
  #   personal$data <- newdata
  #   
  #   showNotification("Submission successful", type="default", session=session)
  # })
  # 
  # 
  # output$personal_db <- renderRHandsontable({
  #   db <- personal$data
  #   db[,"Updated"] <- as.Date(as.numeric(db[,"Updated"]), origin = "1970-01-01")
  #   rhandsontable(db, rowHeaders = F, selectCallback = T) %>%
  #     hot_col("Total Costs", readOnly = T) %>%
  #     hot_col("Estimated Profit", readOnly = T) %>%
  #     hot_col("Profit/Project Month", readOnly = T) %>%
  #     hot_col("TRC/Square Ft", readOnly = T)
  # })
  # 
  # 
  # observeEvent(input$personal_db, {
  #   df <- getDataset(input$source)
  #   sqft <- df[df$Address == personal$data$Address, "finishedSqFt"]
  #   form <- as.data.frame(hot_to_r(input$personal_db))
  #   form[,"Total Costs"] <- as.numeric(form[,"Purchase Price"]) + as.numeric(form[,"Buy-Hold Cost"]) + as.numeric(form[,"Sell Costs"]) + as.numeric(form[,"TRC"])
  #   form[,"Estimated Profit"] <- as.numeric(form[,"ARV"]) - form[,"Total Costs"]
  #   form[,"Profit/Project Month"] <- form[,"Estimated Profit"]/(as.numeric(form[,"Project Weeks"])/4)
  #   form[,"TRC/Square Ft"] <- as.numeric(form[,"TRC"])/sqft
  #   
  #   personal$data <- form
  # })
  # 
  # observeEvent(input$personal_db_select$select$r, {
  #   row <- input$personal_db_select$select$r
  #   db <- as.data.frame(hot_to_r(input$personal_db))
  #   address <- db[row,"Address"]
  #   source <- db[row, "Source"]
  #   source_db <- getDataset(source)
  #   lon <- source_db[source_db$Address == address, "Longitude"]
  #   lat <- source_db[source_db$Address == address, "Latitude"]
  #   output$personal_br <- renderValueBox({valueBox(source_db[source_db$Address == address, "bedrooms"], subtitle = "Bedrooms", color = "red")})
  #   output$personal_sqft <- renderValueBox({valueBox(source_db[source_db$Address == address, "finishedSqFt"], subtitle = "Sq Footage", color = "red")})
  #   output$personal_zest <- renderValueBox({valueBox(paste0("$", formatC(as.numeric(source_db[source_db$Address == address, "zestimate.amount"]), format="f", digits=2, big.mark=",")), subtitle = "Zestimate", color = "red")})
  #   if(source == "Sheriff"){
  #     output$sheriff_personal <- renderUI({
  #       fluidRow(
  #         valueBoxOutput("personal_status"),
  #         valueBoxOutput("personal_selldate"),
  #         valueBoxOutput("personal_openingbid")
  #       )
  #     })
  #     output$personal_status <- renderValueBox({valueBox(source_db[source_db$Address == address, "SellDate"], subtitle = "Sale Date", color = "red")})
  #     output$personal_selldate <- renderValueBox({valueBox(source_db[source_db$Address == address, "Status"], subtitle = "Status", color = "red")})
  #     output$personal_openingbid <- renderValueBox({valueBox(paste0("$", formatC(as.numeric(source_db[source_db$Address == address, "OpeningBid"]), format="f", digits=2, big.mark=",")), subtitle = "Opening Bid", color = "red")})
  #   } else {
  #     output$sheriff_personal <- renderUI({
  #       fluidRow()
  #     })
  #   }
  #   
  #   output$location_personal <- renderLeaflet({
  #     leaflet(options = providerTileOptions(minZoom=12, maxZoom=16)) %>%
  #       setView(lat = lat, lng = lon, zoom=12) %>%
  #       addProviderTiles("OpenStreetMap.Mapnik") %>%
  #       clearGroup("location_marker") %>%
  #       addMarkers(lat = lat, lng = lon, "location_marker")
  #   })
  # })
  # 
  # 
  # 
  # observeEvent(input$submit_pdb, {
  #   db <- as.data.frame(hot_to_r(input$personal_db))
  #   personal$data <- db
  #   s3saveRDS(db, paste0("group_files/", group(), "/personal.rds"), "restore614")
  # })
  # 
  # output$download_pdb <- downloadHandler(
  #   filename = function() {
  #     paste("housing_personal_", Sys.Date(), ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(personal$data, file, row.names = FALSE)
  #   }
  # )
}