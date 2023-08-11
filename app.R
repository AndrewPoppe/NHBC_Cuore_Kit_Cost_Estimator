#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(plotly)
library(ggplot2)
library(tidyverse)
library(glue)
library(matlab)
library( RColorBrewer )

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("NHBC Cuore Kit Cost Estimator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "product",label = "Cuore product", choices = list(
        `Jerseys` = c("Short Sleeve Jersey" = "jersey_short",
                      "Long Sleeve Jersey" = "jersey_long"),
        `Shorts and Bibs` = c(
          "Bib Shorts" = "bib_short",
          "Bib Tights" = "bib_long"),
        `Cold Weather` = c(
          "Vest"  = "vest",
          "Jacket" = "jacket")
      )),
      selectInput(inputId = "lockedIn", label = "Locked in Price Tier", choices = c(
        '1+'  = '1',
        '5+'  = '5',
        '10+' = '10',
        '20+' = '20',
        '35+' = '35',
        '50+' = '50'
      )),
      bsTooltip("lockedIn", "This is the tier that we have previously locked in, which represents the current maximum price tier per item.", "top"),
      sliderInput("subsidy",
                  "Amount to subsidize each item by:",
                  min = 0,
                  max = 200,
                  value = 0
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        plotlyOutput("kitPlot", width = "95%", height = 700)
      ),
      fluidRow(width=12,align="center",
               plotOutput("colorBar", width = "66%")
      )
    ),
  ),
  
)

getPriceOwed <- function(pricesPerItem, requestedPriceTier, numItems, proRate, lockedIn) {
  
  if (numItems >= requestedPriceTier) {
    actualPriceTier <- requestedPriceTier
    numItemsDifference <- 0
  } else if (numItems >= 35) {
    actualPriceTier <- 35
  } else if (numItems >= 20) {
    actualPriceTier <- 20
  } else if (numItems >= 10) {
    actualPriceTier <- 10
  } else if (numItems >= 5) {
    actualPriceTier <- 5
  } else {
    actualPriceTier <- 1
  }
  
  actualPriceTier <- as.character(max(actualPriceTier, lockedIn))
  
  actualPricePerItemRaw <- pricesPerItem[actualPriceTier]
  lockedInPrice <- pricesPerItem[as.character(lockedIn)]
  
  requestedPrice <- pricesPerItem[as.character(requestedPriceTier)]
  priceDifference <-  max(0, actualPricePerItemRaw - requestedPrice)
  
  actualPricePerItemMember <- max(0, min(requestedPrice,lockedInPrice) - proRate)
  
  proRateTotal <- numItems * proRate
  
  # If we just want to pay the difference per item
  owedToCuore <- numItems * priceDifference
  ourCostTotalOption1 <- proRateTotal + owedToCuore
  
  # If we want to buy the remaining # of items to meet minimum
  numItemsDifference <- max(0, as.integer(requestedPriceTier) - numItems)
  costToBuyIntoPriceTier <- numItemsDifference * actualPricePerItemRaw
  ourCostTotalOption2 <- proRateTotal + costToBuyIntoPriceTier
  
  if (numItemsDifference == 0) {
    strategy <- "We met the minimum"
    totalCost <- proRateTotal
  } else if (ourCostTotalOption1 >= ourCostTotalOption2) {
    strategy <- "Club buys additional items to meet price tier minimum"
    totalCost <- ourCostTotalOption2
  } else {
    strategy <- "Club pays Cuore to make up the difference"
    totalCost <- ourCostTotalOption1
  }
  
  return(c(
    "requestedPriceTier" = paste0('Selected: ',requestedPriceTier,'+'),
    "itemsSold" = numItems,
    "priceTierMet" = paste0(actualPriceTier,'+'),
    "proRatePerItem" = proRate,
    "totalPricePerItem" = actualPricePerItemRaw,
    "memberPricePerItem" = actualPricePerItemMember,
    "itemsUntilMinimum" = numItemsDifference,
    "strategy" = strategy,
    "totalCost" = totalCost
  ))
  
}

createText <- function(priceTierMet, totalPricePerItem, memberPricePerItem, itemsUntilMinimum, strategy) {
  glue('Actual Tier Met: {priceTierMet}\nItems Needed for Target Tier: {itemsUntilMinimum}\nStrategy: {strategy}\nTotal Price per Item: {totalPricePerItem}\nMember Price per Item: {memberPricePerItem}')
}

productChoices = list(
  "Short Sleeve Jersey" = "jersey_short",
  "Long Sleeve Jersey" = "jersey_long",
  "Bib Shorts" = "bib_short",
  "Bib Tights" = "bib_long",
  "Vest"  = "vest",
  "Jacket" = "jacket"
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$colorBar <- renderPlot({
    image(seq(0,250,len = 100),1,matrix(1:100,ncol=1),col=jet.colors(100),axes=F,xlab = 'Cost to Member per Item ($US)',ylab='', useRaster = T)
    axis(1)
    box(lwd=2)
  }, height = 150)
  
  
  values <- reactiveValues()
  
  
  
  observe({
    if (input$product == "jersey_short") {
      values$prices = c('1' = 158.50, '5' = 104.50, '10' =  85.50, '20' =  78.00, '35' =  73.00, '50' =  69.50)
      values$lockedIn = '20'
    } else if (input$product == "jersey_long") {
      values$prices = c('1' = 224.50, '5' = 148.00, '10' = 121.00, '20' = 110.00, '35' = 103.50, '50' =  98.50)
      values$lockedIn = '5'
    } else if (input$product == "bib_short") {
      values$prices = c('1' = 212.50, '5' = 143.50, '10' = 119.50, '20' = 110.00, '35' = 104.00, '50' =  99.00)
      values$lockedIn = '20'
    } else if (input$product == "bib_long") {
      values$prices = c('1' = 243.00, '5' = 169.00, '10' = 143.50, '20' = 133.00, '35' = 126.50, '50' = 121.50)
      values$lockedIn = '5'
    } else if (input$product == "vest") {
      values$prices = c('1' = 151.50, '5' = 100.00, '10' =  82.00, '20' =  74.50, '35' =  70.00, '50' =  66.50)
      values$lockedIn = '5'
    } else if (input$product == "jacket") {
      values$prices = c('1' = 234.50, '5' = 177.50, '10' = 152.00, '20' = 139.50, '35' = 131.00, '50' = 124.50)
      values$lockedIn = '10'
    }
  })
  
  observeEvent(input$product, {
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      inputId = 'lockedIn',
      label = NULL,
      choices = NULL,
      selected = values$lockedIn
    )
  })
  
  output$kitPlot <- renderPlotly({
    
    itemsSoldArray = 1:50
    tierArray = c('5', '10', '20', '35', '50')
    bounds = expand.grid(items.sold = itemsSoldArray, requestedTier = tierArray)
    priceInfoArray = t(apply(bounds, 1, function(params) {
      numItems <- as.integer(params[1])
      requestedTier <- as.integer(params[2])
      getPriceOwed(pricesPerItem = values$prices, requestedPriceTier = requestedTier, numItems = numItems, proRate = input$subsidy, lockedIn = as.integer(input$lockedIn))
    }))
    
    priceInfo <- as_tibble(priceInfoArray) %>%
      mutate(across(c(2,4,5,6,7,9), as.numeric)) %>%
      mutate(across(1, ~factor(.x, levels=c('Selected: 1+','Selected: 5+','Selected: 10+','Selected: 20+','Selected: 35+','Selected: 50+')))) %>%
      mutate(across(3, ~factor(.x, levels=c('1+','5+','10+','20+','35+','50+'))))
    colnames(priceInfo) = c('Requested Tier', '# Items Sold', 'priceTierMet', 'proRatePerItem', 'totalPricePerItem','Member Price per Item', 'itemsUntilMinimum', 'strategy', 'Total Club Cost')
    
    memberPriceBreaks = seq(0,250,10)
    
    p <- ggplot(priceInfo, aes(x=`# Items Sold`, y=`Total Club Cost`, fill=`Member Price per Item`), tooltip = "text") +
      geom_text(aes(x=0, y = max(`Total Club Cost`), label=glue("Member cost: ${formatC(as.numeric(`Member Price per Item`), format = 'f', flag='0', digits = 2)}"))) +
      geom_col(aes(text=createText(priceTierMet, totalPricePerItem, `Member Price per Item`, itemsUntilMinimum, strategy))) +
      facet_grid(rows=vars(`Requested Tier`)) +
      scale_fill_gradientn(colors=jet.colors(250), limits = c(0,250)) +
      scale_color_gradientn(colors=jet.colors(250), limits = c(0,250)) +
      guides(fill = F) +
      ggtitle(glue('Club Costs in $US (total) - {names(productChoices)[productChoices == input$product]} with ${input$subsidy} Subsidy')) + 
      theme_bw() + 
      theme(plot.margin = margin(10,10,10,10, "points"))
    ggplotly(p) %>% style(textposition = "bottomright", traces = 1:5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
