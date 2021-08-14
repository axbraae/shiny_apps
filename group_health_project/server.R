#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

server <- function(input, output) {

    # Scotland Survey Visualisation
    output$trendPlot <- renderPlot({
        scottish_survey %>%
            filter(sex == input$gender_input,
                   scottish_health_survey_indicator == input$indicator_input) %>%
            ggplot() +
            aes(x = year, y = percentage, colour = sex) +
            geom_line() +
            geom_point() +
            scale_x_continuous(breaks = 2008:2019) +
            expand_limits(y = c(1, 100)) +
        theme_economist() +
        theme(axis.text.x = element_text(face = "bold", size = 12),
              axis.text.y = element_text(face = "bold", size = 12),
              title =element_text(size=14, face='bold'),
              axis.title=element_text(size=16),
              legend.text = element_text(face = "bold")) +
        labs(x = "\nYear",
             y = "Percent\n",
             colour = "",
             title = "Scottish Health Survey - Scotland level data\n",
             subtitle = "Trends from 2008 to 2019 for the key Health Indicators") +
        scale_colour_manual(values = c("All" = "black", "Male" = "#2E45B8", "Female" = "#C91D42"))

    },
    height = 400, width = 1300)
    
    # Area Level Survey Visualisation
    output$localPlot <- renderPlot({
        scottish_survey_local %>%
            filter(sex == input$sex_input,
                   scottish_health_survey_indicator == input$indic_input)%>%
            group_by(scottish_health_survey_indicator, sex) %>%
            mutate(scotland_percent = mean(percentage)) %>%
            ggplot() +
            aes(x= ca_name, y = percentage, fill = case_when(
                ca_name == "Scotland" ~ "Scotland",
                percentage < scotland_percent ~ "Below Scotland",
                percentage > scotland_percent ~ "Above Scotland"
                )) +
            geom_col() +
            theme_economist() +
            theme(axis.text.x = element_text(face = "bold", size = 12, angle = 45, hjust = 1, vjust = 1),
                  axis.text.y = element_text(face = "bold", size = 12),
                  title = element_text(size=14, face='bold'),
                  axis.title=element_text(size=16),
                  legend.text = element_text(face = "bold"),
                  ) +
            labs(x = "",
                 y = "Percent\n",
                 fill = "",
                 title = "Scottish Health Survey - Local area level data\n",
                 subtitle = "Local authority comparison against National average for the key Health Indicators for 2016 - 2019") +
        scale_fill_manual(values = c("Above Scotland" = "#F6423C", "Below Scotland" = "#1DC9A4", "Scotland" = "#141F52"))
                


    },
    height = 400, width = 1300)
  
      
      #Green space Geospatial Graph
      output$greenspacemap <- renderPlot({
        greenspace_la_geo %>%
          ggplot() +
          geom_sf(aes(fill = mean_percent), colour = "black") +
          theme_economist() +
          theme(axis.text.x = element_text(face = "bold", size = 10),
                axis.text.y = element_text(face = "bold", size = 10),
                title =element_text(size=12, face='bold'),
                axis.title=element_text(size=12),
                legend.position = "bottom",
                legend.text = element_text(size = "10")) +
          labs(title = "Less than 5 min",
               subtitle = "\nDistance to Greenspace 2016-2019",
               fill = "Mean percent")
      },
      width = 650, height = 650)
      
      #Scottish survey local Geospatial Graph
      output$indicatormap <- renderPlot({
        scottish_survey_la_geo %>%   
          filter(scottish_health_survey_indicator == input$map_indic_input | 
                                              is.na(scottish_health_survey_indicator),
                                            sex == "All" | is.na(sex)) %>% 
          ggplot() +
          geom_sf(aes(fill = percentage), colour = "black") +
          theme_economist() +
          theme(axis.text.x = element_text(face = "bold", size = 10),
                axis.text.y = element_text(face = "bold", size = 10),
                title =element_text(size=12, face='bold'),
                axis.title=element_text(size=12),
                legend.position = "bottom",
                legend.text = element_text(size = "8")) +
          labs(title = "Health Indicators",
               subtitle = "\n2016-2019",
               fill = "Mean percent")
      },
      width = 650, height = 650)
      
      #Summary table for Health Indicator and Greenspace tab
      
      output$greenspace_indicator_table <- DT::renderDataTable({
        local_greenspace %>% 
          filter(
            scottish_health_survey_indicator == input$map_indic_input
          ) %>% 
          rename("Local Authority" = ca_name,
                 "Mean Percentage with Indicator" = indicator_percentage,
                 "Mean Percentage Living 5 min from Greenspace" = mean_percent) %>%
          select(-scottish_health_survey_indicator) %>% 
          DT::datatable(
            caption = htmltools::tags$caption(paste0("Health Survey Indicators by Local Authority 2016-2019 for ",
                                   input$map_indic_input), style="color:black"),
            options = list(dom = "t"),
            rownames = FALSE,
            style = "bootstrap") 
      })
      
      #Summary Stats tables
      output$greenspace_stats_table <- DT::renderDataTable({
        greenspace %>% 
          group_by(distance_to_nearest_green_or_blue_space) %>% 
          filter(
            date_code >= 2016,
            str_detect(area_code, "^S92"),
            age == input$age_table_input, 
            gender == "All",
            urban_rural_classification == "All",
            simd_quintiles == "All",
            type_of_tenure == "All",
            household_type == "All",
            ethnicity == "All") %>% 
          summarise("Median percent" = median(value_percent),
                    "Mean percent" = mean(value_percent),
                    "Std deviation" = round(sd(value_percent), digits = 2)) %>%
          mutate(distance_to_nearest_green_or_blue_space =
                   str_replace(distance_to_nearest_green_or_blue_space,
                               "Within a", "A")) %>% 
          arrange(distance_to_nearest_green_or_blue_space) %>% 
          rename("Distance to Green or Blue space" = 
                   distance_to_nearest_green_or_blue_space) %>%
          select("Distance to Green or Blue space", "Median percent", 
                 "Mean percent", "Std deviation") %>% 
          DT::datatable(
            caption = htmltools::tags$caption(paste0(
              "Summary of Distance to Green or Blue Space for Scotland 2016-2019 for ",
              input$age_table_input), style="color:black"),
            options = list(dom = "t"),
            rownames = FALSE,
            style = "bootstrap") 
      })
      
      output$indicator_stats_table <- DT::renderDataTable({
        scottish_survey_local_stats %>% 
          filter(scottish_health_survey_indicator == input$indic_table_input,
            sex == "All") %>%
          select("Percent", "95% Lower Confidence Limit", "95% Upper Confidence Limit") %>% 
          rename("Mean Percent" = "Percent") %>% 
          DT::datatable(
            caption = htmltools::tags$caption(paste0(
              "Health Survey Indicators for Scotland 2016-2019: ", 
              input$indic_table_input), style="color:black"),
            options = list(dom = "t"),
            rownames = FALSE,
            style = "bootstrap")           
      })
      
      

      # life expectancy at zero years
      output$birth_plot <- renderPlot({
        
        life %>% 
          filter(age == "0 years",
                 date_code == "2016-2018",
                 simd_quintiles == input$simd_quintiles,
                 urban_rural_classification == "All",
                 str_detect(area_code, "^S92")) %>% 
          ggplot() +
          aes(x = sex, y = years_to_live, fill = sex) +
          geom_col() +
          scale_y_continuous() +
          labs(
            x = "\nSex",
            y = "Life expectancy (years)\n",
            title = "Life expectancy in Scotland at birth in 2016-2018\n",
            subtitle = "Data from the Scottish Government\n") +
          theme_economist() +
          theme(axis.title = element_text(size = 16, face = "bold"),
                axis.text.y = element_text(size = 10, face = "bold"),
                axis.text.x = element_text(size = 10, face = "bold"),
                title = element_text(size = 14, face = "bold"),
                legend.position = "none") +
          expand_limits(y = c(1,100)) +
          geom_text(aes(label = years_to_live), vjust = -0.5) +
          scale_fill_manual(values = c("Male" = "#2E45B8", "Female" = "#C91D42"))
      },
      height = 400, width = 1300)
      
      # bar graph of male life expectancy, with age on the x axis, years to live on y axis
      output$gender_plot <- renderPlot({
        life %>% 
          filter(sex != "All",
                 date_code == "2016-2018",
                 simd_quintiles == input$simd_quintiles,
                 urban_rural_classification == "All",
                 str_detect(area_code, "^S92")) %>% 
          ggplot() +
          aes(x = reorder(age_new, desc(years_to_live)), y = years_to_live, fill = sex) +
          geom_col(position = "dodge", alpha = 1) +
          labs(
            x = "\nAge (years)",
            y = "Life expectancy (years)\n",
            title = "Life expectancy in Scotland in 2016-2018\n",
            subtitle = "Data from the Scottish Government\n",
            fill = "") +
          theme_economist() +
          theme(axis.text = element_text(angle = 90),
                axis.title = element_text(size = 16, face = "bold"),
                axis.text.y = element_text(size = 10, face = "bold"),
                axis.text.x = element_text(size = 10, face = "bold"),
                title = element_text(size = 14, face = "bold"),
                legend.text = element_text(face = "bold")) +
          expand_limits(y = c(1,100)) +
          scale_fill_manual(values = c("Male" = "#2E45B8", "Female" = "#C91D42"))
        
      },
      height = 400, width = 1300)
      
}

