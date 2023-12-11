
graft_plot <- function(model, graft = "quad"){
  
  name <- if (graft == "quad") {
    "Extensor"
  } else if (graft == "hs") {
    "Flexor"
  } else {
    "name"
  }
  
  knot_pos <- NULL # set to 0 for logic to work later
  
  moderator <- as.character(model$call$mods[2]) # get the moderator variable
  data <- model$data # get the data
  
  last_timepoint <- data %>% filter(!is.na(vi)) %>% summarise(round(max(timepoint_mean),0)) %>% as.numeric
  last_timepoint_1 <- data %>% filter(!is.na(vi), graft_group2 != "other") %>% summarise(round(max(timepoint_mean),0)) %>% as.numeric
  last_timepoint_2 <- data %>% filter(!is.na(vi), graft_group2 == "other") %>% summarise(round(max(timepoint_mean),0)) %>% as.numeric
  
  if (str_detect(moderator, "rcs")) { # if a rcs is used, extract the information
    spline <- as.list(model$formula.mods[[2]][[3]][[3]]) 
    knot_pos <- unlist(spline[sapply(spline, is.numeric)]) # this gets the vector of knot positions or no. of knots
    
  }
  
  if (str_detect(moderator, "\\*")) {
  
  # depending on the type od moderator (i.e. log, linear, rcs...) calculate the predicted curve points
  mod_type <- if (str_detect(moderator, "log")) {
    
    #log
    line1 <- data.frame(predict(model, 
                                newmods = cbind(rep(0, 100), log(seq(1,100, length = 100)), rep(0, 100)), 
                                transf = exp)) %>% mutate(x = row_number())
    
    line2 <- data.frame(predict(model, 
                                newmods = cbind(rep(1, 100), log(seq(1,100, length = 100)), rep(1, 100)*log(seq(1,100, length = 100))), 
                                transf = exp)) %>% mutate(x = row_number())
    
    
  }  else if (length(knot_pos) == 3) {
    
    # 3 knot rcs
    line1 <- data.frame(predict(model, newmods = 
                                  matrix(c(rep(0, 100), 
                                           rcspline.eval(seq(1, 100, length = 100), knot_pos, inclx = TRUE), 
                                           rcspline.eval(seq(1, 100, length = 100), knot_pos, inclx = TRUE)*0),
                                         nrow = 100, 
                                         ncol = 5), transf = exp)) %>% mutate(x = row_number())
    line2 <- data.frame(predict(model, newmods = 
                                  matrix(c(rep(1, 100), 
                                           rcspline.eval(seq(1, 100, length = 100), knot_pos, inclx = TRUE), 
                                           rcspline.eval(seq(1, 100, length = 100), knot_pos, inclx = TRUE)*1),
                                         nrow = 100, 
                                         ncol = 5), transf = exp)) %>% mutate(x = row_number())
    
    
  } else if (length(knot_pos) == 4) {
    
    # 4 knot rcs
    line1 <- data.frame(predict(model, newmods = 
                                  matrix(c(rep(0, 100), 
                                           rcspline.eval(seq(1, 100, length = 100), knot_pos, inclx = TRUE), 
                                           rcspline.eval(seq(1, 100, length = 100), knot_pos, inclx = TRUE)*0),
                                         nrow = 100, 
                                         ncol = 7), transf = exp)) %>% mutate(x = row_number())
    line2 <- data.frame(predict(model, newmods = 
                                  matrix(c(rep(1, 100), 
                                           rcspline.eval(seq(1, 100, length = 100), knot_pos, inclx = TRUE), 
                                           rcspline.eval(seq(1, 100, length = 100), knot_pos, inclx = TRUE)*1),
                                         nrow = 100, 
                                         ncol = 7), transf = exp)) %>% mutate(x = row_number())
    
  }
  } else {
    
    
    # depending on the type od moderator (i.e. log, linear, rcs...) calculate the predicted curve points
    mod_type <- if (str_detect(moderator, "log")) {
      
      #log
      line1 <- data.frame(predict(model, 
                                  newmods = cbind(rep(0, 100), log(seq(1,100, length = 100))), 
                                  transf = exp)) %>% mutate(x = row_number())
      
      line2 <- data.frame(predict(model, 
                                  newmods = cbind(rep(1, 100), log(seq(1,100, length = 100))), 
                                  transf = exp)) %>% mutate(x = row_number())
      
      
    }  else if (length(knot_pos) == 3) {
      
      # 3 knot rcs
      line1 <- data.frame(predict(model, newmods = 
                                    matrix(c(rep(0, 100), 
                                             rcspline.eval(seq(1, 100, length = 100), knot_pos, inclx = TRUE)),
                                           nrow = 100, 
                                           ncol = 3), transf = exp)) %>% mutate(x = row_number())
      line2 <- data.frame(predict(model, newmods = 
                                    matrix(c(rep(1, 100), 
                                             rcspline.eval(seq(1, 100, length = 100), knot_pos, inclx = TRUE)),
                                           nrow = 100, 
                                           ncol = 3), transf = exp)) %>% mutate(x = row_number())
      
      
    } else if (length(knot_pos) == 4) {
      
      # 4 knot rcs
      line1 <- data.frame(predict(model, newmods = 
                                    matrix(c(rep(0, 100), 
                                             rcspline.eval(seq(1, 100, length = 100), knot_pos, inclx = TRUE)),
                                           nrow = 100, 
                                           ncol = 4), transf = exp)) %>% mutate(x = row_number())
      line2 <- data.frame(predict(model, newmods = 
                                    matrix(c(rep(1, 100), 
                                             rcspline.eval(seq(1, 100, length = 100), knot_pos, inclx = TRUE)),
                                           nrow = 100, 
                                           ncol = 4), transf = exp)) %>% mutate(x = row_number())
    }
  }
  
  # calculate ci points
  ci1 <-  pointsfunction(line1)
  ci2 <-  pointsfunction(line2)
  
  ci1 <- ci1 %>% filter(x < last_timepoint_1 + 6)
  ci2 <- ci2 %>% filter(x < last_timepoint_1 + 6)
  line1 <- line1 %>% filter(x < last_timepoint_1 + 6)
  line2 <- line2 %>% filter(x < last_timepoint_2 + 6)
  
  plot <- data %>%
    ggplot(aes(x = timepoint_mean, y = exp(yi), group = interaction(cohort, graft_group2), colour = graft_group2)) +
    geom_point(aes(size = acl_n), alpha = 0.1) + 
    geom_line(alpha = 0.1) + 
    scale_y_continuous(breaks = c(0.6, 0.8, 1.0, 1.2), labels = c("-40%", "-20%", "0%", "+20%")) +
    scale_colour_manual(values = c("red", "blue"), labels = c(name, "Other"), name = "Graft") +
    #scale_y_continuous(labels = scales::percent, limits = c(-0.75, 0.25)) +
    #scale_x_continuous(trans = 'log10', limits = c(3,150)) +
    coord_cartesian(xlim = c(0,100), ylim = c(0.5, 1.2)) +
    scale_size(range = c(0, 10)) +
    labs(x = "Time since surgery (Months)", y = "Percentage Deficit", colour = "Measure", size = "Participants (n)") +
    #geom_smooth(aes(x = timepoint_mean, y = yi), method = "lm", colour = "green", inherit.aes = FALSE) +
    #geom_abline(intercept = -0.2198, slope = 0.0014, colour = "red") +
    geom_line(data = line1, aes(x = x, y = pred), colour = "red", linewidth = 1.3, inherit.aes = FALSE) +
    geom_line(data = line2, aes(x = x, y = pred), colour = "blue", linewidth = 1.3, inherit.aes = FALSE) +
    geom_polygon(data = ci1, aes(x = x, y = ci.ub), fill = "red",  alpha = 0.1, inherit.aes = FALSE) +
    geom_polygon(data = ci2, aes(x = x, y = ci.ub), fill = "blue",  alpha = 0.1, inherit.aes = FALSE) +
    theme_mgpub()
  
  return(plot)
  
}

