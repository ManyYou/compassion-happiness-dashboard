#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# server.R

library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(forcats)
library(plotly)

function(input, output, session) {
  
  # ────────────────────────────────────────────────────────────────────────────
  # 1) LOAD & PREPARE DATA
  # ────────────────────────────────────────────────────────────────────────────
  
  # 1a) Read CSV (must be in same folder)
  raw_data <- read.csv("wrangled_data_source.csv", stringsAsFactors = FALSE)
  
  # 1b) Get world polygons (sf) from rnaturalearth
  world_sf <- ne_countries(type = "countries", returnclass = "sf")
  
  # 1c) Join CSV onto polygons by ISO3, then clamp original compassion_score to [–1, +1]
  #      and rescale to [0,10].  Happiness is already [0,10].
  world_data <- world_sf %>%
    left_join(raw_data, by = c("iso_a3" = "country_code")) %>%
    mutate(
      # STEP 1: clamp raw compassion_score into [–1, +1]
      compassion_clamped = ifelse(
        is.na(compassion_score),
        NA_real_,
        pmin(1, pmax(-1, compassion_score))
      ),
      # STEP 2: map that clamped value to [0,10]
      compassion_0_10 = ifelse(
        is.na(compassion_clamped),
        NA_real_,
        (compassion_clamped + 1) * 5
      ),
      # We assume raw happiness_score is already 0–10
      happiness_0_10 = ifelse(
        is.na(happiness_score),
        NA_real_,
        happiness_score
      )
    )
  
  # 1d) Six‐step sequential "green" palette for choropleth (0–10)
  green_seq <- c("#edf8e9","#c7e9c0","#a1d99b","#74c476","#31a354","#006d2c")
  
  palComp <- colorNumeric(
    palette  = green_seq,
    domain   = c(0, 10),
    na.color = "#666666"
  )
  palHapp <- colorNumeric(
    palette  = green_seq,
    domain   = c(0, 10),
    na.color = "#666666"
  )
  
  
  # ────────────────────────────────────────────────────────────────────────────
  # 2) SLIDE 1: LEAFLET CHOROPLETH MAPS (0–10 scale, sequential green)
  # ────────────────────────────────────────────────────────────────────────────
  
  output$mapComp <- renderLeaflet({
    leaflet(world_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addPolygons(
        fillColor   = ~palComp(compassion_0_10),
        color       = "#999999",
        weight      = 0.4,
        fillOpacity = 0.85,
        label       = ~paste0(
          name, " — ",
          ifelse(
            is.na(compassion_0_10),
            "No Data",
            sprintf("%.2f", compassion_0_10)
          )
        ),
        highlight   = highlightOptions(
          weight       = 2,
          color        = "red",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position  = "bottomleft",
        pal       = palComp,
        values    = c(0, 10),
        title     = "Compassion (0 → 10)",
        labFormat = labelFormat(digits = 1),
        opacity   = 1
      )
  })
  
  
  output$mapHapp <- renderLeaflet({
    leaflet(world_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addPolygons(
        fillColor   = ~palHapp(happiness_0_10),
        color       = "#999999",
        weight      = 0.4,
        fillOpacity = 0.85,
        label       = ~paste0(
          name, " — ",
          ifelse(
            is.na(happiness_0_10),
            "No Data",
            sprintf("%.2f", happiness_0_10)
          )
        ),
        highlight   = highlightOptions(
          weight       = 2,
          color        = "red",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position  = "bottomleft",
        pal       = palHapp,
        values    = c(0, 10),
        title     = "Happiness (0 → 10)",
        labFormat = labelFormat(digits = 1),
        opacity   = 1
      )
  })
  
  
  # ────────────────────────────────────────────────────────────────────────────
  # 3) PREPARE DATA FOR ORDERED BAR CHARTS (SLIDE 2)
  # ────────────────────────────────────────────────────────────────────────────
  dv_data <- reactive({
    df <- raw_data %>%
      filter(!is.na(compassion_score), !is.na(happiness_score)) %>%
      # clamp + rescale compassion once more (to match exactly what map did)
      mutate(
        compassion_clamped = pmin(1, pmax(-1, compassion_score)),
        compassion_0_10     = (compassion_clamped + 1) * 5
      )
    
    # Compassion bottom/top 10 (based on compassion_0_10)
    comp_sorted   <- df %>% arrange(compassion_0_10)
    comp_bottom10 <- head(comp_sorted, 10)
    comp_top10    <- tail(comp_sorted, 10)
    
    # Happiness bottom/top 10 (happiness_score is already 0–10)
    happ_sorted   <- df %>% arrange(happiness_score)
    happ_bottom10 <- head(happ_sorted, 10)
    happ_top10    <- tail(happ_sorted, 10)
    
    list(
      comp_bottom = comp_bottom10,
      comp_top    = comp_top10,
      happ_bottom = happ_bottom10,
      happ_top    = happ_top10
    )
  })
  
  
  # ────────────────────────────────────────────────────────────────────────────
  # 4) SLIDE 2: ORDERED HORIZONTAL BAR CHARTS (ascending)
  # ────────────────────────────────────────────────────────────────────────────
  
  output$barComp <- renderPlotly({
    d <- dv_data()
    
    df_comb <- bind_rows(
      d$comp_bottom %>% mutate(group = "Bottom 10"),
      d$comp_top    %>% mutate(group = "Top 10")
    ) %>%
      arrange(compassion_0_10) %>%
      mutate(
        group   = factor(group, levels = c("Bottom 10", "Top 10")),
        country = factor(country, levels = country)  # preserve ascending order
      )
    
    p <- ggplot(
      df_comb,
      aes(
        x    = compassion_0_10,
        y    = country,
        text = paste0(country, ": ", sprintf("%.2f", compassion_0_10)),
        fill = group
      )
    ) +
      geom_col(width = 0.6, show.legend = TRUE) +
      scale_fill_manual(
        values = c("Bottom 10" = "#a1d99b", "Top 10" = "#31a354"),
        labels = c("Bottom 10", "Top 10"),
        name   = ""
      ) +
      scale_x_continuous(
        limits = c(0, 10),
        breaks = seq(0, 10, 2)
      ) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x        = element_text(size = 12),
        axis.text.y        = element_text(size = 10),
        legend.position    = "top"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 100, r = 20, t = 40, b = 60)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  output$barHapp <- renderPlotly({
    d <- dv_data()
    
    df_comb <- bind_rows(
      d$happ_bottom %>% mutate(group = "Bottom 10"),
      d$happ_top    %>% mutate(group = "Top 10")
    ) %>%
      arrange(happiness_score) %>%
      mutate(
        group   = factor(group, levels = c("Bottom 10", "Top 10")),
        country = factor(country, levels = country)
      )
    
    p <- ggplot(
      df_comb,
      aes(
        x    = happiness_score,
        y    = country,
        text = paste0(country, ": ", sprintf("%.2f", happiness_score)),
        fill = group
      )
    ) +
      geom_col(width = 0.6, show.legend = TRUE) +
      scale_fill_manual(
        values = c("Bottom 10" = "#a1d99b", "Top 10" = "#31a354"),
        labels = c("Bottom 10", "Top 10"),
        name   = ""
      ) +
      scale_x_continuous(
        limits = c(0, 10),
        breaks = seq(0, 10, 2)
      ) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x        = element_text(size = 12),
        axis.text.y        = element_text(size = 10),
        legend.position    = "top"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 100, r = 20, t = 40, b = 60)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # ────────────────────────────────────────────────────────────────────────────
  # 5) SLIDE NAVIGATION LOGIC
  # ────────────────────────────────────────────────────────────────────────────
  observeEvent(input$next1, {
    updateTabsetPanel(session, "slides", selected = "slide2")
  })
  observeEvent(input$prev1, {
    updateTabsetPanel(session, "slides", selected = "slide2")
  })
  observeEvent(input$next2, {
    updateTabsetPanel(session, "slides", selected = "slide1")
  })
  observeEvent(input$prev2, {
    updateTabsetPanel(session, "slides", selected = "slide1")
  })
  
  
  # ────────────────────────────────────────────────────────────────────────────
  # 6) PARTITIONED POSTER: BUBBLE SCATTER, PARALLEL COORDS, & SMALL SCATTERS
  # ────────────────────────────────────────────────────────────────────────────
  
  # 6a) Define a fixed color mapping for continents:
  continent_colors <- c(
    "Africa"        = "#FC8D62",
    "Asia"          = "#FFD92F",
    "Europe"        = "#66C2A5",
    "North America" = "#8DA0CB",
    "Oceania"       = "#E78AC3",
    "South America" = "#A6D854"
  )
  
  output$bubblePlot <- renderPlotly({
    req(input$continent_filter, input$bubble_feature)
    
    df <- world_data %>%
      filter(
        continent %in% input$continent_filter,
        !is.na(compassion_0_10),
        !is.na(happiness_0_10)
      )
    
    if (input$bubble_feature == "none") {
      p <- ggplot(
        df,
        aes(
          x     = compassion_0_10,
          y     = happiness_0_10,
          color = continent,
          text  = paste0(
            name, "<br>",
            "Compassion: ", sprintf("%.2f", compassion_0_10), "<br>",
            "Happiness: ", sprintf("%.2f", happiness_0_10)
          )
        )
      ) +
        geom_point(alpha = 0.7, size = 6) +
        scale_color_manual(values = continent_colors) +
        scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1), name = "Compassion Score (0–10)") +
        scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1), name = "Happiness Score (0–10)") +
        theme_minimal(base_size = 13) +
        theme(
          axis.text    = element_text(size = 11),
          axis.title   = element_text(size = 13, face = "bold"),
          legend.title = element_text(size = 12),
          legend.text  = element_text(size = 10)
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          xaxis = list(range = c(0, 10)),
          yaxis = list(range = c(0, 10)),
          margin = list(l = 60, r = 20, t = 20, b = 80)
        ) %>%
        config(displayModeBar = FALSE)
      
    } else {
      df2 <- df %>%
        filter(!is.na(.data[[input$bubble_feature]]))
      
      p <- ggplot(
        df2,
        aes(
          x     = compassion_0_10,
          y     = happiness_0_10,
          size  = .data[[input$bubble_feature]],
          color = continent,
          text  = paste0(
            name, "<br>",
            "Compassion: ", sprintf("%.2f", compassion_0_10), "<br>",
            "Happiness: ", sprintf("%.2f", happiness_0_10), "<br>",
            input$bubble_feature, ": ", sprintf("%.2f", .data[[input$bubble_feature]])
          )
        )
      ) +
        geom_point(alpha = 0.7) +
        scale_size_continuous(range = c(3, 15), name = NULL) +
        scale_color_manual(values = continent_colors) +
        scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1), name = "Compassion Score (0–10)") +
        scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1), name = "Happiness Score (0–10)") +
        theme_minimal(base_size = 13) +
        theme(
          axis.text    = element_text(size = 11),
          axis.title   = element_text(size = 13, face = "bold"),
          legend.title = element_text(size = 12),
          legend.text  = element_text(size = 10)
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          xaxis = list(range = c(0, 10)),
          yaxis = list(range = c(0, 10)),
          margin = list(l = 60, r = 20, t = 20, b = 80)
        ) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  
  # 6b) Dynamic narrative for bubble plot
  output$bubbleNarrative <- renderUI({
    feature <- input$bubble_feature
    narr <- switch(
      feature,
      "none"                        = paste(
        "For Europe (green), North America (blue), Oceania (pink), and South America (light‑green), you can clearly see a gentle upward trend: as average Compassion rises from about 3–4 up toward 8–10, their Happiness scores also tick up, but only modestly. In other words, those regions show a weak positive correlation with a small slope. By contrast, the Asian (yellow) and African (orange) points are all over the place: at a Compassion score of 4–5, some Asian countries report Happiness near 3 while others report values above 6, and African countries show similar spread. That high vertical variance for the same Compassion level indicates that, for Asia and Africa, empathy alone is not a strong predictor of national well‑being as other socioeconomic or cultural factors might be at play."
      ),
      "gini"                        = paste(
        "With bubble size now encoding each country’s Gini coefficient, we can see that nations with lower income inequality (smaller bubbles) generally cluster in the upper‑right quadrant of the plot—particularly in Europe and North America—combining high Compassion (6–10) with high Happiness (6–8). In contrast, larger bubbles—indicating higher inequality—are found toward the center‑left region, where many African, Asian, and South American countries sit at moderate Compassion scores (3–5) but lower average Happiness (3–5). This inverse gradient suggests that, even when compassion levels are comparable, greater inequality is associated with reduced well‑being, highlighting how income distribution may temper the positive effects of social empathy on national happiness."
      ),
      "gdp_per_capita"              = paste(
        "When GDP per capita drives bubble size, the largest circles—typically Europe (green), North America (blue), and Oceania (pink)—all sit in the upper‑right of the plot. These high‑income countries combine strong Compassion (7–10) and high Happiness (6–8), reinforcing the idea that wealthier economies tend to report both greater social empathy and well‑being. Medium‑sized bubbles (mid‑range GDP) appear mostly in Asia (yellow) and South America (light‑green) around Compassion 4–6 and Happiness 5–6, showing more moderate outcomes. The smallest bubbles, representing lower‑income nations—largely across Africa (orange) and parts of Asia—cluster around lower Compassion (3–5) and Happiness (3–5). Overall, there’s a clear positive gradient: higher GDP per capita is generally associated with higher Compassion and higher Happiness, though regional variation still produces some outliers."
      ),
      "percent_gdp_social_spending" = paste(
        "When bubble size reflects social‐welfare spending, you can see that the largest circles—mostly European (green), North American (blue), and Oceanian (pink) countries—cluster in the upper‐right quadrant, combining high Compassion (around 7–10) and high Happiness (around 6–8). These regions invest heavily in social programs and also report strong wellbeing. Mid‑sized bubbles (moderate spending) appear in South America (light green) and parts of Asia (yellow) around Compassion 4–6 and Happiness 5–6, suggesting that even modest social spending supports middling happiness when empathy is similar. The smallest bubbles—lower spending countries in Africa (orange) and some Asian nations—tend to fall toward the middle left, where Compassion scores are around 3–5 and Happiness hovers closer to 4–5. Overall, there’s a gentle upward trend: at comparable compassion levels, higher social spending corresponds to slightly higher happiness, highlighting the role of welfare investment in amplifying the positive effects of social empathy."
      ),
      "Please select a feature above."
    )
    tagList(p(narr))
  })
  
  
  # 6c) Parallel Coordinates plot
  output$parcoordsPlot <- renderPlotly({
    req(input$continent_filter)
    
    df <- world_data %>%
      filter(
        continent %in% input$continent_filter,
        !is.na(compassion_0_10),
        !is.na(gini),
        !is.na(gdp_per_capita),
        !is.na(happiness_0_10)
      )
    
    # reuse your continent_colors mapping
    cont_lvls  <- names(continent_colors)
    # map each row’s continent to an index 1…N
    cont_idx   <- as.integer(factor(df$continent, levels = cont_lvls))
    # normalize into [0,1] so it matches our colorscale stops
    cont_norm  <- (cont_idx - 1) / (length(cont_lvls) - 1)
    
    # build a matching colorscale
    colorscale <- lapply(seq_along(cont_lvls), function(i) {
      list((i - 1)/(length(cont_lvls)-1), continent_colors[[ cont_lvls[i] ]])
    })
    
    plot_ly(
      type = 'parcoords',
      line = list(
        color      = cont_norm,
        colorscale = colorscale,
        cmin       = 0,
        cmax       = 1,
        showscale  = FALSE
      ),
      dimensions = list(
        list(label = "Compassion (0–10)", values = df$compassion_0_10, range = c(0, 10)),
        list(label = "Gini",              values = df$gini),
        list(label = "GDP per Capita",    values = df$gdp_per_capita),
        list(label = "Happiness (0–10)",  values = df$happiness_0_10, range = c(0, 10))
      )
    ) %>%
      layout(
        title  = "Line Colors Represent Continent",
        margin = list(l = 50, r = 150, t = 50, b = 50)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  
  # 6d) Correlation matrix (no Social Spending)
  output$corrMatrixPlot <- renderPlotly({
    # 1) Prepare numeric data
    df_num <- world_data %>%
      sf::st_drop_geometry() %>%
      select(compassion_0_10, gini, gdp_per_capita, happiness_0_10) %>%
      rename(
        Compassion = compassion_0_10,
        Gini       = gini,
        GDP        = gdp_per_capita,
        Happiness  = happiness_0_10
      ) %>%
      na.omit() %>%
      mutate(across(everything(), as.numeric))
    
    # 2) Compute correlation matrix
    corr_mat <- round(cor(df_num, use = "complete.obs"), 2)
    
    # 3) Melt to long form
    library(reshape2)
    m <- melt(corr_mat, varnames = c("Feature_Y", "Feature_X"), value.name = "r")
    
    # 4) Compute text color per cell (white on |r| > 0.5)
    m$text_color <- ifelse(abs(m$r) > 0.5, "white", "black")
    
    # 5) Plot with ggplot2 (no TRUE/FALSE legend)
    p <- ggplot(m, aes(x = Feature_X, y = Feature_Y, fill = r)) +
      geom_tile() +
      geom_text(aes(label = r), color = m$text_color, size = 5) +
      scale_fill_gradient2(
        low      = "blue",
        mid      = "white",
        high     = "red",
        midpoint = 0,
        limits   = c(-1, 1),
        name     = "r"
      ) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid  = element_blank()
      )
    
    # 6) Convert to plotly
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
}

