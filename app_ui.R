# Assignment 4
# app's ui components
page_one <- tabPanel(
  "Introduction and Summary tab",
  titlePanel("Exploring CO2 Emissions over time for Different Eountries"),
  
  p("The topic of climate change becomes more relevant everyday as daily human 
  operations contine to cause irreperable harm on our planet. By analyzing the 
  global emission of greenhouse gases and defining the most contributive 
  countries, nations can begin to understand their own impact on climate change 
  and work towards creating policies that to mitigate such damages."), 
  
  p("This can be seen from my first variable, which organized data from The Global
  Carbon Project and displays historically, which countries have produced the 
  most greenhouse gases. The top three countries were the United States, China,
  and Russion, which have been recorded to produce 416723.088, 235564.016, and
  115344.148 million tonnes of green house gases respectively."), 
  
  p("After analyzing the historic role countries have contributed to climate
  change, I chose to analyze which countries produce the most emissions per 
  capita in the most recent year. The result shows the countries that have been
  disproportionately emitted greenhouse gases relative to their population. 
  This is important because recent technologies have been developed to allow
  humans to create and harness energy in a more sustainable manner, technology 
  that should be implemented most urgently in Qatar, New Caledonia, and Mongolia.
  Qatar produced around 37 tonnes per person in 2020 compared to the United 
  States which produced around 14 tonnes per person."),
  
  p("My third variable focuses on how the amount of greenhouse gases emitted by the 
  historically top three contributors have changed. This variable pulls the 
  yearly change in emissions as well as yearly changes in emissions per capita
  from the United States, China, and Russia. Although from 2015-2020, the United
  States has decreased their emissions the most (-543.045 million tonnes from 
  2019-2020), this nations emissions per capita still remains the largest 
  (14.238 tonnes per person in 2020) out of the 3 countries. This means although 
  there has been progress, the United States needs to implement more changes to
  continue decreasing its emission of greenhouse gases."),
 
  p("Next, I decided to analyze each countries average greenhouse gases emission in
  the past 10 years (from 2010-2020). From this analysis, I found that in the 
  past 10 year, China has contributed the most average emissions of 10018 
  million tonnes, followed by the United States with 5305 million tonnes, and 
  India with 2272 million tonnes. This variable shows a more recent depiction of
  various countries' roles in contributing to climate change."), 
 
  p("Finally, as a citizen of the United States, I wanted to see how each producer 
  of greenhouse gases contributed to the overall emission from the United States.
  I did this by calculating the percentage of emissions from different producers
  in the most recent year. I found that oil contributes the most to the United
  State's emissions as it makes up 42.9%. This means that as a nation, we need 
  to focus on finding more sustainable energy sources that output less 
  greenhouse gases.")
)

page_two <- tabPanel(
  "CO2 Emissions per Country",
  sidebarLayout( sidebarPanel(
    sliderInput(
      inputId = "year",
      label = "Year Selection",
      min = 1900,
      max = 2020,
      value = 2020,   # starting value
      sep = ""        # no commas in year
    ),
    
    radioButtons(inputId = "change", label = "Abs or Change",
                 choices = list("Absolute Value" = 1, "Change in Value" = 2)),
    
    radioButtons(inputId = "var", label = "Variable Selection",
                 choices = list("Emissions" = 1, "Emissions Per Capita" = 2))
  ),
  
  mainPanel(
    textOutput(outputId = "message"), 
    br(), br(),
    plotlyOutput(outputId = "chart"),
    
    p("The interactive section of this project allows users to explore how the 
      yearly emissions and emissions per capita of the United States, China, 
      and Russia have shifted. Usersncan view the absolute value of these 
      changes or the actual value to analyze if the emissions decreased. An
      interesting year users can view are 2016, when all three countries
      decreased their output. I chose these three countrie sas they have 
      historically contributed the most greenhouse gases into the atmosphere.
      Additionally, these countries have influence in setting international
      policies with consistent roles in the UN.")
  ) )
)

ui <- navbarPage(
  "Assignment 4",
  page_one, 
  page_two
)