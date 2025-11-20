#* Analysis
  #++ Figs 1 and 2
    #--- Normality Tests
      #_Run function on each tibble
        norm_1A <- shapiro(BW)
        norm_1B <- shapiro(Fig_1B)
        norm_1C <- shapiro(Fig_1C)
        norm_1D <- shapiro(Fig_1D)
        norm_2A <- shapiro(Fig_2A)
        norm_2B <- shapiro(Fig_2B)
        norm_2C <- shapiro(Fig_2C)
        norm_2D <- shapiro(Fig_2D)
        norm_2E <- shapiro(Fig_2E)
    #--- Transform datasets as appropriate
      #_Run on appropriate tibbles based on normality tests
        Fig_1B_log <- log_trans(Fig_1B)
        Fig_1D_log <- log_trans(Fig_1D)
        Fig_2B_log <- log_trans(Fig_2B)
        Fig_2C_log <- log_trans(Fig_2C)
        Fig_2D_log <- log_trans(Fig_2D)
    #--- Run 2-way ANOVA using aov where compatible + posthoc
      #_Define function to run ANOVAs using aov and posthoc using Tukey HSD

      #_Run on the compatible datasets
        results_1B <- run_aov(Fig_1B_log, 'FE', 'Fig_1B_log')
        results_1D <- run_aov(Fig_1D_log, 'FM', 'Fig_1D_log')
        results_2C <- run_aov(Fig_2C_log, 'FPG3', 'Fig_2C_log')
        results_2D <- run_aov(Fig_2D_log, 'HOMA2_IR', 'Fig_2D_log')
      #_Combine results in a tibble
        fig_1B_1D_2C_2D_two_way <- bind_rows(results_1B, results_1D, results_2C, results_2D)
    #--- Run ANOVA using ezANOVA for 2B since it is 3-way RM
      #_Long pivot log transformed tibble
        Fig_2B_log <- pivot_longer(Fig_2B_log, 
                                    cols = c(G0, G15), 
                                    names_to = "Time", 
                                    values_to = "Glucose")
      #_Run ezANOVA
        fig_2b_rm_ANOVA <- ezANOVA(data = Fig_2B_log,
                                dv = .(Glucose), # Dependent variable
                                wid = .(ID), # Subject identifier
                                within = .(Time), # Within-subjects factor
                                between = .(Sex, Diet), # Between-subjects factors
                                detailed = TRUE, # Get a detailed output
                                type = 3) # Type of sums of squares to use
        fig_2B_RM_three_way_RM <- as_tibble(fig_2b_rm_ANOVA$ANOVA) # Convert to tibble
    #--- Run ART ANOVAs on non-normal & non-lognormals (1C and 2E)
      #_Prepare 1C and 2E for ART
        Fig_1C_art_ready <- Fig_1C %>%
          mutate(Sex = factor(Sex),
                Diet = factor(Diet))
        Fig_2E_art_ready <- Fig_2E %>%
          mutate(Sex = factor(Sex),
                Diet = factor(Diet))
      #_Perform ART ANOVA on 2C
        ART_Fig_1C <- art(LM ~ Sex * Diet, data = Fig_1C_art_ready)
        fig_1C_two_way_ART <- anova(ART_Fig_1C)
        # No sex:diet interaction so no post-hoc test
      #_Perform ART ANOVA on 2E
        ART_Fig_2E <- art(ins45 ~ Sex * Diet, data = Fig_2E_art_ready)
        fig_2E_two_way_ART <- anova(ART_Fig_2E)
        # Sex:diet interaction so will do post-hoc
      #_Perform Tukey HSD on 2E
        fig_2E_posthoc_ART <- art.con(ART_Fig_2E, "Sex:Diet", adjust="tukey") %>% 
        summary() %>%  # add significance stars to the output
        mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                            cutpoints = c(0, .001, .01, .05, .10, 1),
                            symbols = c("***", "**", "*", ".", " ")))
    #--- Run ART ANOVAs on non-normal & non-lognormal RMs (1A and 2A)
      #_Prepare 1A and 2A for ART (convert to factors)
        Fig_1A_art_ready_i <- Fig_1A %>%
          mutate(Sex = factor(Sex),
                Diet = factor(Diet),
                ID = factor(ID))
        Fig_2A_art_ready_i <- Fig_2A %>%
          mutate(Sex = factor(Sex),
                Diet = factor(Diet),
                ID = factor(ID))
      #_Prepare 1A and 2A for ART (pivot long)
        Fig_1A_art_ready <- Fig_1A_art_ready_i %>%
          pivot_longer(cols = BW0:BW8,
                      names_to = "Time",
                      names_prefix = "BW",
                      values_to = "Weight",
                      values_drop_na = TRUE) %>%
          mutate(Time = as.factor(Time))
        Fig_2A_art_ready <- Fig_2A_art_ready_i %>%
          pivot_longer(cols = G0:G45,
                      names_to = "Time",
                      names_prefix = "G",
                      values_to = "Glucose",
                      values_drop_na = TRUE) %>%
          mutate(Time = as.factor(Time))
      #_Perform ART ANOVA on 1A
        Art_Fig_1A <- art(Weight ~ Sex * Diet * Time + (1|ID), data = Fig_1A_art_ready)
        fig_1A_three_way_RM_ART <- anova(Art_Fig_1A)
      #_Perform ART ANOVA on 2A
        Art_Fig_2A <- art(Glucose ~ Sex * Diet * Time + (1|ID), data = Fig_2A_art_ready)
        fig_2A_three_way_RM_ART <- anova(Art_Fig_2A)
  #++ Fig 3ABC
    #--- Normality Tests
      #_Define column names to run on
        glucose_columns <- c("Gonadal", "RP", "SQ", "Brown", "EDL", "Gastroc", "Soleus", "Heart")
      #_Run on Fig_3ABC, combine into tibble
        norm_3ABC <- lapply(glucose_columns, function(column_name) {
          glucose_values <- Fig_3ABC[[column_name]]
          test_results <- perform_shapiro_lognormal_tests(glucose_values)
          return(test_results)
        })
        norm_3ABC_tb <- tibble(
          Glucose_Column = glucose_columns,
          Shapiro_p_value = sapply(norm_3ABC, function(x) x$Shapiro_p_value),
          Normality = sapply(norm_3ABC, function(x) x$Normality),
          Shapiro_log_p_value = sapply(norm_3ABC, function(x) x$Shapiro_log_p_value),
          Lognormality = sapply(norm_3ABC, function(x) x$Lognormality)
        )
    #--- Transform 3ABC columns appropriately
      #_All 3ABC were lognormal so transforming all
        Fig_3ABC_log <- Fig_3ABC %>%
          mutate(across(.cols = 4:11, .fns = ~log(.))) %>%
          select(-1) # Remove subject ID
    #--- Run Two Way ANOVA on each column
      #_Define dependent variables and make a list
        dependent_vars_3ABC <- names(Fig_3ABC_log)[-(1:2)]
      #_Perform two-way ANOVA for each dependent variable and store summaries
          anova_summaries_3ABC <- map(dependent_vars_3ABC, ~ {
            formula <- as.formula(paste(.x, "~ Sex * Diet + Sex:Diet"))
            aov_out <- aov(formula, data = Fig_3ABC_log)
            tidy(aov_out)
          })
      #_Extract p-values
        p_values_3ABC <- map2(dependent_vars_3ABC, anova_summaries_3ABC, ~ {
          data.frame(
            Variable = .x,
            Sex_p_value = .y %>% filter(term == "Sex") %>% pull(p.value),
            Diet_p_value = .y %>% filter(term == "Diet") %>% pull(p.value),
            Sex_X_Diet_p_value = .y %>% filter(term == "Sex:Diet") %>% pull(p.value)
          )
        }) %>% bind_rows()
      #_Store results into tibble
        fig_3ABC_two_way <- as_tibble(p_values_3ABC)
  #++ Fig 3D
    #--- Normality Test
      #_First, need to remove FHF12 (has 1 value and need 2 values for RM)
        Fig_3D <- Fig_3D %>%
          filter(ID != "FHF12")
      #_Create log transformed 3D version
      Fig_3D_log <- Fig_3D %>%
          mutate(across(.cols = 5, .fns = ~log(.)))
      #_Perform normality and lognormality tests manually
        norm_3D <- shapiro.test(Fig_3D[[5]])
        lognorm_3D <- shapiro.test(Fig_3D_log[[5]])
    #--- Run ANOVA using ezANOVA for 3D since it is 3-way RM
      #_Was lognormal so prepare lognormal tibble
        Fig_3D_log <- Fig_3D_log %>%
            mutate(Sex = as.factor(Sex),
                  Diet = as.factor(Diet),
                  Treatment = as.factor(Treatment),
                  ID = as.factor(ID))
      #_Run using ezANOVA since it is compatible
        fig_3D_rm_anova <- ezANOVA(data = Fig_3D_log,
                                dv = .(Uptake), # Dependent variable
                                wid = .(ID), # Subject identifier
                                within = .(Treatment), # Within-subjects factor
                                between = .(Sex, Diet), # Between-subjects factors
                                detailed = TRUE, # Get a detailed output
                                type = 3) # Type of sums of squares to use
        fig_3D_RM_three_way_RM  <- as_tibble(fig_3D_rm_anova$ANOVA)