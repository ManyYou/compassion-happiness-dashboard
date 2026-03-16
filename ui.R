#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# ui.R

library(shiny)
library(leaflet)
library(plotly)

fluidPage(
  tags$head(
    tags$style(HTML("
      /* ─────────────────────────────────────────────────────────────────────
         Global padding/margins adjustments so content sits close to edges 
         with a tiny buffer
      ───────────────────────────────────────────────────────────────────── */
      html, body {
        margin: 0;
        padding: 0;
      }
      .container-fluid {
        padding-left: 5px !important;
        padding-right: 5px !important;
      }
      .row {
        margin-left: 0 !important;
        margin-right: 0 !important;
      }
      [class*='col-'] {
        padding-left: 0 !important;
        padding-right: 0 !important;
      }

      /* ─────────────────────────────────────────────────────────────────────
         SLIDESHOW (Slide panels): thin border + small padding
      ───────────────────────────────────────────────────────────────────── */
      .slideshow-box {
        border: 2px solid #cccccc;
        border-radius: 6px;
        padding: 12px;
        margin: 6px 0;  /* 6px top/bottom, 0px left/right */
        background-color: #ffffff;
      }

      /* ─────────────────────────────────────────────────────────────────────
         Title images: maximum height, centered
      ───────────────────────────────────────────────────────────────────── */
      .title-img {
        max-height: 200px;
        width: auto;
        display: block;
        margin: auto;
      }

      /* ─────────────────────────────────────────────────────────────────────
         Introduction box: boxed, slight top margin
      ───────────────────────────────────────────────────────────────────── */
      .intro-box {
        border: 1px solid #999999;
        border-radius: 4px;
        padding: 12px;
        margin-top: 6px;
        background-color: #fafafa;
      }

      /* ─────────────────────────────────────────────────────────────────────
         Section labels (II. Overview, III. Relationship…)
      ───────────────────────────────────────────────────────────────────── */
      .section-label {
        font-size: 18px;
        font-weight: bold;
        margin: 10px 0 6px 0;
      }

      /* ─────────────────────────────────────────────────────────────────────
         Plotly container margins (so axes/labels never get clipped)
      ───────────────────────────────────────────────────────────────────── */
      .plotly-container {
        margin-left: 6px;
        margin-right: 6px;
        margin-top: 6px;
        margin-bottom: 6px;
      }

      /* ─────────────────────────────────────────────────────────────────────
         Partitioned‐poster bubble panel: fixed height + hide overflow
      ───────────────────────────────────────────────────────────────────── */
      .bubble-panel {
        border: 1px solid #cccccc;
        border-radius: 6px;
        padding: 8px;
        background-color: #ffffff;
        height: 480px;
        overflow: hidden;
        margin: 6px;
      }

      /* ─────────────────────────────────────────────────────────────────────
         Bubble narrative: same height, scroll if needed
      ───────────────────────────────────────────────────────────────────── */
      .bubble-narrative {
        border: 1px solid #cccccc;
        border-radius: 6px;
        padding: 8px;
        background-color: #ffffff;
        height: 480px;
        overflow-y: auto;
        margin: 6px;
      }
    "))
  ),
  
  # ====================================================================
  # HEADER ROW: Left Image | Center Title+Intro | Right Image
  # ====================================================================
  fluidRow(
    column(
      width = 3,
      tags$img(
        src    = "https://socialecology.uci.edu/sites/default/files/news_images/istock-496064258.jpg",
        class  = "title-img",
        height = "200px",
        alt    = "Left Project Image"
      )
    ),
    
    column(
      width = 6,
      align = "center",
      h2(
        "Compassion and Happiness",
        style = "margin: 16px 0; font-weight: bold;"
      ),
      div(
        class = "intro-box",
        align = "left", 
        tags$h4("I. Introduction", style = "font-weight: bold;"),
        tags$p(
          "Welcome to the “Compassion and Happiness” data visualization project. In this project, we shall explore the relationship between 
           compassion and happiness at country level to understand whether countries with higher compassion also tend to report higher 
           happiness.",
          tags$br(), tags$br(),
          "Both the compassion and happiness scores range incrementally from 0 to 10, with 10 indicating the population's highly favorable sentiment regarding 
          compassion or happiness. The happiness score is sourced from the World Happiness Report, whereas the compassion score is calculated based on the survey 
          answers towards questions related to their compassion values such as their sense of collectivism, sourced from the World Value Survey. Due to the limited number of countries with respondents, we only have about 80 country data in our analysis.",
          tags$br(), tags$br(),
          "To enrich our understanding of the relationship between these two variables, we shall also analyze their interactions across other socio economic related features
          as well such as: GDP per capita, Gini index (level of inequality), Net Social Spending as % of GDP (only OECD data available) that are sourced from authoritative sources such as the World Bank."
        )
      )
    ),
    
    column(
      width = 3,
      tags$img(
        src    = "https://api.brusselstimes.com/wp-content/uploads/2022/10/6bce0928-onimnartofimis47.jpg",
        class  = "title-img",
        height = "200px",
        alt    = "Right Project Image"
      )
    )
  ),
  
  
  # ====================================================================
  # II. Overview Label
  # ====================================================================
  fluidRow(
    column(
      width = 12,
      div(class = "section-label", "II. Overview")
    )
  ),
  
  
  # ====================================================================
  # SLIDESHOW BOX: Slide 1 & Slide 2
  # ====================================================================
  fluidRow(
    column(
      width = 12,
      div(
        class = "slideshow-box",
        tabsetPanel(
          id   = "slides",
          type = "hidden",
          
          # ----------------------------------------------------------------
          # SLIDE 1: World‐Map Choropleths
          # ----------------------------------------------------------------
          tabPanel(
            title = "slide1",
            value = "slide1",
            
            fluidRow(
              column(
                width = 4,
                span(
                  actionButton(
                    "prev1", "Back",
                    class = "btn btn-primary",
                    style = "margin-right:4px;"
                  ),
                  actionButton(
                    "next1", "Next",
                    class = "btn btn-primary"
                  )
                )
              ),
              column(
                width = 4, align = "center",
                tags$div(
                  style = "font-size:18px; font-weight:bold;",
                  "Slide 1: Compassion and Happiness Scores across the World"
                )
              ),
              column(width = 4)
            ),
            
            fluidRow(
              column(
                width = 6,
                tags$div(
                  style = "margin:6px 0 6px; font-size:16px; font-weight:bold;",
                  "Compassion"
                ),
                div(class = "plotly-container",
                    leafletOutput("mapComp", height = "440px")
                )
              ),
              column(
                width = 6,
                tags$div(
                  style = "margin:6px 0 6px; font-size:16px; font-weight:bold;",
                  "Happiness"
                ),
                div(class = "plotly-container",
                    leafletOutput("mapHapp", height = "440px")
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                tags$div(
                  style = "margin-top:10px; font-size:15px; line-height:1.4; color:#333;",
                  strong("Description:"), HTML("&nbsp;"),
                  tags$br(), tags$br(),
                  "The choropleth maps above depict the distribution of compassion and happiness scores across the world.",
                  tags$br(), tags$br(),
                  "Most of the countries have a compassion score of at least 4, but there are some countries with score below this mostly from South America. The countries with very high compassion score 
                  tend to come from wealthy developed countries in North America, Europe, and Oceania. Besides these countries, the rest exhibit moderate (around 4-6) compassion score.",
                  tags$br(), tags$br(),
                  "On the Happiness Score side, most countries have moderate happiness (4 to 6), whereas similarly as compassion, the happiest countries tend to be wealthy countries located in the above mentioned continents.
                  One thing to note is that the countries with low compassion score in South America also exhibit moderate happiness level despite the low compassion score.",
                  tags$br(), tags$br(),
                  "This result suggests that there appear to be some connection between compassion and happiness depending on the region as well as the wealth of that region."
                )
              )
            )
          ),
          
          
          # ----------------------------------------------------------------
          # SLIDE 2: Top/Bottom 10 Ordered Bar Charts
          # ----------------------------------------------------------------
          tabPanel(
            title = "slide2",
            value = "slide2",
            
            fluidRow(
              column(
                width = 4,
                span(
                  actionButton(
                    "prev2", "Back",
                    class = "btn btn-primary",
                    style = "margin-right:4px;"
                  ),
                  actionButton(
                    "next2", "Next",
                    class = "btn btn-primary"
                  )
                )
              ),
              column(
                width = 4, align = "center",
                tags$div(
                  style = "font-size:18px; font-weight:bold;",
                  "Slide 2: Top/Bottom 10 Compassion and Happiness Score"
                )
              ),
              column(width = 4)
            ),
            
            fluidRow(
              column(
                width = 6,
                tags$div(
                  style = "margin:6px 0 6px; font-size:16px; font-weight:bold;",
                  "Compassion"
                ),
                div(class = "plotly-container",
                    plotlyOutput("barComp", height = "440px", width = "100%")
                )
              ),
              column(
                width = 6,
                tags$div(
                  style = "margin:6px 0 6px; font-size:16px; font-weight:bold;",
                  "Happiness"
                ),
                div(class = "plotly-container",
                    plotlyOutput("barHapp", height = "440px", width = "100%")
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                tags$div(
                  style = "margin-top:10px; font-size:15px; line-height:1.4; color:#333;",
                  strong("Description:"), HTML("&nbsp;"),
                  tags$br(), tags$br(),
                  "The above charts depict the top 10 and bottom 10 countries for either compassion score (left) or happiness score (right). ",
                  tags$br(), tags$br(),
                  "For the top 10 countries, we see similar developed and wealthy countries for both graphs such as Netherland, Germany, and United Kingdom. On the other hand, the same cannot be said for the bottom 10 since none of the bottom 10 countries for compassion score appear in the bottom 10 for happiness. This insight further reinforces our previous observation with the choropleth map that the relationship between compassion and happiness appears strong for wealthy and developed countries who have both high compassion and happiness scores. "
                )
              )
            )
          )
        )
      )
    )
  ),
  
  
  # ====================================================================
  # III. Relationship between Compassion and Happiness
  # ====================================================================
  fluidRow(
    column(
      width = 12,
      div(class = "section-label", "III. Relationship between Compassion and Happiness")
    )
  ),
  
  
  # ====================================================================
  # PARTITIONED POSTER
  # Top row: Bubble scatter + Narrative
  # ====================================================================
  fluidRow(
    column(
      width = 6,
      div(class = "bubble-panel",
          fluidRow(
            column(
              width = 6,
              selectizeInput(
                inputId  = "continent_filter",
                label    = "Select Continents:",
                choices  = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
                selected = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
                multiple = TRUE
              )
            ),
            column(
              width = 6,
              selectInput(
                inputId  = "bubble_feature",
                label    = "Bubble Size Feature:",
                choices  = c(
                  "None"                      = "none",
                  "Gini"                      = "gini",
                  "GDP per Capita"            = "gdp_per_capita",
                  "Social Spending (% GDP)"   = "percent_gdp_social_spending"
                ),
                selected = "none"
              )
            )
          ),
          div(class = "plotly-container",
              plotlyOutput("bubblePlot", height = "380px", width = "100%")
          )
      )
    ),
    
    column(
      width = 6,
      div(class = "bubble-narrative",
          h4("Bubble Plot Analysis:",
             style = "font-weight: bold;"),
          uiOutput("bubbleNarrative")
      )
    )
  ),
  
  
  # ====================================================================
  # PARTITIONED POSTER
  # Second row: Parallel coords (left) + Correlation Matrix (right)
  # ====================================================================
  fluidRow(
    column(
      width = 6,
      div(style = "margin: 6px;",
          h4("Features Parallel Correlation", style = "text-align:center;"),
          plotlyOutput("parcoordsPlot", height = "400px", width = "100%")
      )
    ),
    column(
      width = 6,
      div(style = "margin: 6px;",
          h4("Features Correlation Matrix", style = "text-align:center;"),
          plotlyOutput("corrMatrixPlot", height = "400px", width = "100%")
      )
    )
  ),
  
  
  # ====================================================================
  # PARTITIONED POSTER
  # Third row: Narrative under parallel coordinates
  # ====================================================================
  fluidRow(
    column(
      width = 12,
      div(
        style = "
          margin-top: 10px;
          padding: 12px;
          border: 1px solid #cccccc;
          border-radius: 4px;
          background-color: #fafafa;
        ",
        h4("Compassion and Happiness Analysis",
           style = "font-weight: bold;"),
        p(
          "When viewed together, the parallel‐coordinates and correlation‐matrix plots reveal a clear multivariate pattern: Compassion only translates into higher Happiness in the context of wealth and low inequality. The heatmap shows that Compassion and Happiness correlate only weakly on their own (r = 0.35), whereas GDP per capita drives Happiness much more strongly (r = 0.67) and is itself tightly linked to Compassion (r = 0.65) and inversely to Gini (r = –0.44). In the parallel‐coords, Europe, North America, and Oceania (green, blue, and pink lines) all bundle together at high GDP (20–80 k) and low Gini (0.25–0.40), with Compassion of 6–10 and Happiness of 6–8. By contrast, Asia and Africa (yellow and orange) fan out across Gini and GDP—many low‑income, high‑inequality countries report similar Compassion scores (3–5) but wildly different Happiness (3–7). In other words, high empathy only delivers high wellbeing when underpinned by economic resources and more equitable income distribution; without those, a compassionate populace may still struggle to achieve greater happiness."
        )
      )
    )
  ),
  
  # ====================================================================
  # REFERENCES
  # ====================================================================
  fluidRow(
    column(
      width = 12,
      tags$hr(),
      h4("References"),
      tags$ul(
        tags$li("World Value Survey (2018–2023). Values, social trust, and norms. Retrieved from https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp"),
        tags$li("OECD Social Expenditure Database (SOCX). Social expenditure (% of GDP). Retrieved from https://www.oecd.org/en/data/datasets/social-expenditure-database-socx.html"),
        tags$li("World Happiness Report 2022. Wellbeing Research Centre, University of Oxford. Retrieved from https://worldhappiness.report/ed/2022/"),
        tags$li("World Bank (2023). Gini coefficient. Retrieved from https://data.worldbank.org/indicator/SI.POV.GINI"),
        tags$li("World Bank (2023). GDP per capita. Retrieved from https://data.worldbank.org/indicator/NY.GDP.PCAP.CD")
      )
    )
  )
)



