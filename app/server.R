# Define server logic 
server <- function(input, output, session) {
  
  # define two central reactive data frames for Mann-Whitney and Wilcoxon distributions
  df = reactiveValues(mw = NULL,
                      w = NULL)
  # define two central reactive values to capture the lower and upper critical values for MW
  cv = reactiveValues(mw_lower = NULL,
                      mw_upper = NULL,
                      w_lower = NULL,
                      w_upper = NULL,)
  
  observeEvent(input$mw_m | input$mw_n,
               {
                 m = input$mw_m
                 n = input$mw_n
                 
                 # create rank sums for all combinations of both size m and n
                 df_mw = data.frame(ranksum = colSums(combn(x = m + n, m = m)))
                 
                 # calculate the cumulative probabilities
                 df_mw = df_mw %>% 
                   arrange(ranksum) %>% 
                   mutate(cdf = rank(x = ranksum, ties.method = "max") / n()) 
                 
                 df$mw = df_mw
               }
  )
  
  output$mw_probability = renderUI({
    
    df_mw = df$mw
    
    if (is.na(input$mw_statistic)) {""}
    else {
      if (input$mw_tails == -1) {
        # identify the dots that are less than or equal to the test statistic, Wm
        numerator = df_mw %>% 
          filter(ranksum <= input$mw_statistic) %>% 
          nrow()
        symbol = "≤"
      }
      if (input$mw_tails == 1) {
        # identify the dots that are greater than or equal to the test statistic, Wm
        numerator = df_mw %>% 
          filter(ranksum >= input$mw_statistic) %>% 
          nrow()
        symbol = "≥"
      }
      if (input$mw_tails == 2) {
        # determine whether test statistic is below or above 'centre' of distribution
        if (input$mw_statistic <= 0.5 * input$mw_m * (input$mw_m + input$mw_n + 1)) {
          numerator = df_mw %>% 
            filter(ranksum <= input$mw_statistic) %>% 
            nrow()
          symbol = "≤"
        } else {
          numerator = df_mw %>% 
            filter(ranksum >= input$mw_statistic) %>% 
            nrow()
          symbol = "≥"
        }
      }
      
      if (input$mw_pv == TRUE) {
        if(input$mw_tails == 2) {
          pv1 = "p-\\mathrm{value} = 2 × "
          pv2 = "2 × "
          pv3 = 2
        } else {
          pv1 = "p-\\mathrm{value} = "
          pv2 = ""
          pv3 = 1}
      } else {
        pv1 = ""
        pv2 = ""
        pv3 = 1
      }
      
      # total number of combinations
      denominator = df_mw %>% nrow()
      
      withMathJax(
        helpText(
          paste0("$$",
                 pv1,
                 "P(W_m ", symbol, " ",
                 as.numeric(input$mw_statistic),
                 ") = ",
                 pv2,
                 "\\frac{",
                 numerator,
                 "}{",
                 denominator,
                 "} = ",
                 round(numerator / denominator,4) * pv3,
                 "$$"
          )
        )
      )
    }
  })
  
  output$mw_critical_value = renderText({
    m = input$mw_m
    n = input$mw_n
    df_mw = df$mw
    
    # identify the critical value for the stated level of significance and tail
    if (input$mw_significance > 0 & input$mw_tails == -1) {
      cv$mw_lower = df_mw %>%
        filter(cdf > input$mw_significance) %>%
        arrange(ranksum) %>% 
        slice(1) %>%
        pull(ranksum) - 1
      
      #  check if critical value is below minimum allowed
      if (cv$mw_lower >= min(0.5 * m * (m + 1), 0.5 * n * (n+1))) {
        paste0(as.numeric(input$mw_significance) * 100,
               "% Critical value for lower tail = ",
               cv$mw_lower)
      } else {"No critical value exists for lower tail."}
      
    }
    else if (input$mw_significance > 0 & input$mw_tails == 1) {
      # logic is cv$mw_upper = m*(m+n+1) - cv$mw_lower to get symmetry about 'centre'
      cv$mw_upper = m*(m + n + 1) - (df_mw %>%
                                       filter(cdf > input$mw_significance) %>%
                                       arrange(ranksum) %>% 
                                       slice(1) %>%
                                       pull(ranksum) - 1)
      
      #  check if critical value is above maximum allowed
      if (cv$mw_upper <= max(0.5*(m+n)*(m+n+1) - 0.5*n*(n+1), 0.5*(m+n)*(m+n+1) - 0.5*m*(m+1))) {
        paste0(as.numeric(input$mw_significance) * 100,
               "% Critical value for upper tail = ",
               cv$mw_upper)
      } else {"No critical value exists for upper tail."}
    }
    
    else if (input$mw_significance > 0 & input$mw_tails == 2) {
      cv$mw_lower = df_mw %>%
        filter(cdf > as.numeric(input$mw_significance) / 2) %>%
        arrange(ranksum) %>% 
        slice(1) %>%
        pull(ranksum) - 1
      
      cv$mw_upper = m*(m+n+1) - cv$mw_lower
      
      #  check if critical value(s) are below/above allowed extremes
      if (cv$mw_lower >= min(0.5 * m * (m + 1), 0.5 * n * (n+1))) {
        paste0(as.numeric(input$mw_significance) * 100 / 2,
               "% Critical values for tails = ",
               cv$mw_lower,
               " and ",
               cv$mw_upper)
      } else {"No critical values exist for either tail."}
    }
    
  })
  
  output$mw_plot = renderPlot({
    
    df_mw = df$mw
    cv_mw_lower = as.numeric(cv$mw_lower)
    cv_mw_upper = as.numeric(cv$mw_upper)
    # m = as.numeric(input$mw_m)
    # n = as.numeric(input$mw_n)
    
    # highlight the appropriate dots for critical region
    if (input$mw_significance > 0 & input$mw_tails == -1) {
      df_mw = df_mw %>% 
        mutate(critical_region = ranksum <= cv_mw_lower)
    } else if (input$mw_significance > 0 & input$mw_tails == 1) {
      df_mw = df_mw %>% 
        mutate(critical_region = ranksum >= cv_mw_upper)
    } else if (input$mw_significance > 0 & input$mw_tails == 2) {
      df_mw = df_mw %>% 
        mutate(critical_region = (ranksum <= cv_mw_lower) | (ranksum >= cv_mw_upper))
    } else {
      df_mw = df_mw  %>% 
        mutate(critical_region = FALSE)
    }
    
    # highlight the appropriate dots for p-value
    if (is.na(input$mw_statistic)) {
      df_mw = df_mw %>% 
        mutate(highlight = TRUE)
    } else {
      if (input$mw_tails == -1) {
        df_mw = df_mw %>% 
          mutate(highlight = ranksum <= input$mw_statistic)
      } else if (input$mw_tails == 1) {
        df_mw = df_mw %>% 
          mutate(highlight = ranksum >= input$mw_statistic)
      } else if (input$mw_tails == 2) {
        if (input$mw_statistic <= 0.5 * input$mw_m * (input$mw_m + input$mw_n + 1)) {
          df_mw = df_mw %>% 
            mutate(highlight = ranksum <= input$mw_statistic)
        } else  {
          df_mw = df_mw %>% 
            mutate(highlight = ranksum >= input$mw_statistic)
        }
      }
    }
    
    # this next line ensures that TRUE always gets the alpha of 1, even it it's the only factor level
    df_mw$highlight = factor(df_mw$highlight, levels = c(FALSE, TRUE))
    
    ggplot(data = df_mw,
           mapping = aes(x = ranksum, fill = critical_region, alpha = highlight
           )) +
      geom_dotplot(binwidth = 1,
                   dotsize = 0.5,
                   stackratio = 1.1,
                   show.legend = FALSE) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.length = unit(-0.8, "cm"),
            axis.text.x = element_text(size = 14, vjust = 2),
            axis.title.x = element_text(size = 16),
            panel.background = element_blank()) +
      labs(x = "Rank Sum Values of Wm", y = NULL) +
      scale_x_continuous(breaks = seq(0,200,2)) +
      scale_fill_manual(values = c("blue", "red")) +
      scale_alpha_manual(values = c("FALSE" = 0.5, "TRUE" = 1))
    
  }
  , height = 600
  )
  
  observeEvent(input$w_n,
               {
                 n = input$w_n
                 # create ranks sums for Wilcoxon test
                 df_w = data.frame(
                   ranksum = unlist(
                     sapply(
                       c(0:n), 
                       function(i) colSums(combn(n, i))
                     )
                   )
                 )
                 
                 # calculate the cumulative probabilities
                 df_w = df_w %>% 
                   arrange(ranksum) %>% 
                   mutate(cdf = rank(x = ranksum, ties.method = "max") / n()) 
                 
                 df$w = df_w
               }
  )
  
  output$w_probability = renderUI({
    
    df_w = df$w
    
    if (is.na(input$w_statistic)) {""}
    else {
      if (input$w_tails == -1) {
        # identify the dots that are less than or equal to the test statistic, W
        numerator = df_w %>% 
          filter(ranksum <= input$w_statistic) %>% 
          nrow()
        symbol = "≤"
      }
      if (input$w_tails == 1) {
        # identify the dots that are greater than or equal to the test statistic, W
        numerator = df_w %>% 
          filter(ranksum >= input$w_statistic) %>% 
          nrow()
        symbol = "≥"
      }
      if (input$w_tails == 2) {
        # determine whether test statistic is below or above 'centre' of distribution
        if (input$w_statistic <= 0.25 * input$w_n * (input$w_n + 1)) {
          numerator = df_w %>% 
            filter(ranksum <= input$w_statistic) %>% 
            nrow()
          symbol = "≤"
        } else {
          numerator = df_w %>% 
            filter(ranksum >= input$w_statistic) %>% 
            nrow()
          symbol = "≥"
        }
      }
      
      if (input$w_pv == TRUE) {
        if(input$w_tails == 2) {
          pv1 = "p-\\mathrm{value} = 2 × "
          pv2 = "2 × "
          pv3 = 2
        } else {
          pv1 = "p-\\mathrm{value} = "
          pv2 = ""
          pv3 = 1}
      } else {
        pv1 = ""
        pv2 = ""
        pv3 = 1
      }
      
      # total number of combinations
      denominator = df_w %>% nrow()
      
      withMathJax(
        helpText(
          paste0("$$",
                 pv1,
                 "P(W ", symbol, " ",
                 as.numeric(input$w_statistic),
                 ") = ",
                 pv2,
                 "\\frac{",
                 numerator,
                 "}{",
                 denominator,
                 "} = ",
                 round(numerator / denominator,4) * pv3,
                 "$$"
          )
        )
      )
    }
  })
  
  output$w_critical_value = renderText({
    n = input$w_n
    df_w = df$w
    
    # identify the critical value for the stated level of significance and tail
    if (input$w_significance > 0 & input$w_tails == -1) {
      cv$w_lower = df_w %>%
        filter(cdf > input$w_significance) %>%
        arrange(ranksum) %>% 
        slice(1) %>%
        pull(ranksum) - 1
      
      #  check if critical value is below minimum allowed
      if (cv$w_lower >= 0) {
        paste0(as.numeric(input$w_significance) * 100,
               "% Critical value for lower tail = ",
               cv$w_lower)
      } else {"No critical value exists for lower tail."}
      
    }
    else if (input$w_significance > 0 & input$w_tails == 1) {
      cv$w_upper = df_w %>%
        filter(cdf > 1 - as.numeric(input$w_significance)) %>%
        arrange(ranksum) %>% 
        slice(1) %>%
        pull(ranksum) + 1
      
      #  check if critical value is above maximum allowed
      if (cv$w_upper <= 0.5 * n * (n + 1)) {
        paste0(as.numeric(input$w_significance) * 100,
               "% Critical value for upper tail = ",
               cv$w_upper)
      } else {"No critical value exists for upper tail."}
    }
    else if (input$w_significance > 0 & input$w_tails == 2) {
      cv$w_lower = df_w %>%
        filter(cdf > as.numeric(input$w_significance) / 2) %>%
        arrange(ranksum) %>% 
        slice(1) %>%
        pull(ranksum) - 1
      
      cv$w_upper = 0.5 * n * (n + 1) - cv$w_lower
      
      #  check if critical value(s) are below/above allowed extremes
      if (cv$w_lower >= 0) {
        paste0(as.numeric(input$w_significance) * 100 / 2,
               "% Critical values for tails = ",
               cv$w_lower,
               " and ",
               cv$w_upper)
      } else {"No critical values exist for either tail."}
      
    }
    
  })
  
  output$w_plot = renderPlot({
    
    df_w = df$w
    cv_w_lower = as.numeric(cv$w_lower)
    cv_w_upper = as.numeric(cv$w_upper)
    
    # highlight the appropriate dots for critical region
    if (input$w_significance > 0 & input$w_tails == -1) {
      df_w = df_w %>% 
        mutate(critical_region = ranksum <= cv_w_lower)
    } else if (input$w_significance > 0 & input$w_tails == 1) {
      df_w = df_w %>% 
        mutate(critical_region = ranksum >= cv_w_upper)
    } else if (input$w_significance > 0 & input$w_tails == 2) {
      df_w = df_w %>% 
        mutate(critical_region = (ranksum <= cv_w_lower) | (ranksum >= cv_w_upper))
    } else {
      df_w = df_w  %>% 
        mutate(critical_region = FALSE)
    }
    
    # highlight the appropriate dots for p-value
    if (is.na(input$w_statistic)) {
      df_w = df_w %>% 
        mutate(highlight = TRUE)
    } else {
      if (input$w_tails == -1) {
        df_w = df_w %>% 
          mutate(highlight = ranksum <= input$w_statistic)
      } else if (input$w_tails == 1) {
        df_w = df_w %>% 
          mutate(highlight = ranksum >= input$w_statistic)
      } else if (input$w_tails == 2) {
        if (input$w_statistic <= 0.25 * input$w_n * (input$w_n + 1)) {
          df_w = df_w %>% 
            mutate(highlight = ranksum <= input$w_statistic)
        } else  {
          df_w = df_w %>% 
            mutate(highlight = ranksum >= input$w_statistic)
        }
      }
    }
    
    # this next line ensures that TRUE always gets the alpha of 1, even it it's the only factor level
    df_w$highlight = factor(df_w$highlight, levels = c(FALSE, TRUE))
    
    ggplot(data = df_w,
           mapping = aes(x = ranksum, fill = critical_region, alpha = highlight
           )) +
      geom_dotplot(binwidth = 1,
                   dotsize = 0.5,
                   stackratio = 1.1,
                   show.legend = FALSE) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.length = unit(-0.8, "cm"),
            axis.text.x = element_text(size = 14, vjust = 2),
            axis.title.x = element_text(size = 16),
            panel.background = element_blank()) +
      labs(x = "Signed Rank Sum Values of W", y = NULL) +
      scale_x_continuous(breaks = seq(0,200,2)) +
      scale_fill_manual(values = c("blue", "red")) +
      scale_alpha_manual(values = c("FALSE" = 0.5, "TRUE" = 1))
    
  }
  , height = 600,
  )
}