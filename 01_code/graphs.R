#---------------------------------#
# G1: Horizontal Distribution    #
#---------------------------------#
g1_horizontal <- function(xvar) {
  
  lp <- lp_all %>% drop_na(LP_mf)
  for (i in unique(lp$countrycode)) {
    
    print(i)
    
    cnt <- lp_all %>% filter(countrycode==i)
    reg <- reg_all %>% filter(region==cnt$region)
    inc <- inc_all %>% filter(incomelevel==cnt$incomelevel)  
    
    colours <- c('#00429d','#1d6ca5','#3a94ac','#fe9125','#fccb53','#fbff7c')
    # Make the text white if the box is blue
    text <- ifelse(cnt[[xvar]]<40, "white", "black")
  
    adjust <- ifelse(abs(reg[[xvar]]-inc[[xvar]])<6, 0.0015,
                     0)
    ggplot() +
      geom_point(data = lp_all,
                 aes(x = get(xvar), y = 1.025, fill = get(xvar)),
                 color = "white",
                 alpha = 0.07, size = 7, shape = 22, stroke = 1) +
      geom_point(data = lp_all %>% filter(countrycode == i),
                 aes(x = get(xvar), y = 1.025, fill = get(xvar)),
                 color = "white",
                 alpha = 1, size = 8, shape = 22, stroke = 2) +
      geom_point(data = lp_all, ## graph a "color" one so the color scheme follows
                 aes(x = get(xvar), y = 1.025, color = get(xvar)),
                 alpha = 0, size = 8, shape = 22, stroke = 2) +
      geom_point(data = reg_all %>% filter(region == cnt$region),
                 aes(x = get(xvar), y = 1.025, color = get(xvar)),
                 alpha = 1, size = 8, shape = 124, stroke = 2) +
      geom_point(data = inc_all %>% filter(incomelevel == cnt$incomelevel),
                 aes(x = get(xvar), y = 1.025, color = get(xvar)),
                 alpha = 1, size = 8, shape = 124, stroke = 2) +
      ylim(1, 1.03) +
      xlim(0, 100) +
      labs(x = "", y = "") +
      theme_minimal() +
      scale_fill_gradientn(colors = colours) +
      scale_color_gradientn(colors = colours) +
      theme(axis.line.y = element_blank(),
            axis.text.y = element_blank(),  # Remove y-axis labels
            axis.ticks.y = element_blank(),  # Remove y-axis ticks
            axis.text.x = element_text(margin = margin(t = -100)),  # Reduce spacing
            panel.grid = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title.align = 0,
            legend.title = element_blank(),
            legend.position = "none") +
      geom_text(data = lp_all %>% filter(countrycode == i),
                aes(label = i, y = 1.033, x = get(xvar)), color = "black", size = 4) +
      geom_text(data = lp_all %>% filter(countrycode == i),
                aes(label = round(get(xvar),digits=0), y = 1.025, x = get(xvar)), color = text, size = 4) +
      geom_text(data= reg_all %>% filter(region == cnt$region),
                aes(label = region, y = 1.02, x = get(xvar)), color = "grey60", size = 3) +
      
      geom_text(data = inc_all %>% filter(incomelevel == cnt$incomelevel),
                aes(label = incomelevel, y = 1.02+adjust, x = get(xvar)), color = "grey60", size = 3)
    
    #Save graph
    ggsave(filename = file.path(GraphFolder, paste("01_horizontal.png", sep = '')),width = 9,height = 1.3, bg="white")
  }
}

#---------------------------------#
# T1: Nice Table                  #
#---------------------------------#
t1 <- function() {
  cnt <- lp_all %>% filter(countrycode==i)
  cnt_hci <- hci_all %>% filter(countrycode==i)
  
  #cnt <- format(cnt, digits = 2)
  
  df <- data.frame(Indicator=c("Learning Poverty", "Learning Deprivation", "Schooling Deprivation", "Human Capital Index", "Learning Adjusted Years of Schooling"),
                   Boys=c(round(cnt$LP_m,digits=0), round(cnt$BMP_m,digits=1), round(cnt$OOS_m,digits=1), round(cnt_hci$HCI_m,digits=2), round(cnt_hci$LAYS_m,digits=1)),
                   Girls=c(round(cnt$LP_f,digits=0), round(cnt$BMP_f,digits=1), round(cnt$OOS_f,digits=1), round(cnt_hci$HCI_f,digits=2), round(cnt_hci$LAYS_f,digits=1)),
                   All=c(round(cnt$LP_mf,digits=0), round(cnt$BMP_mf,digits=1), round(cnt$OOS_mf,digits=1), round(cnt_hci$HCI_mf,digits=2), round(cnt_hci$LAYS_mf,digits=1)))
  
  
  rownames(df) <- df[,1]
  df <- df %>% select(-Indicator)
  
  # Make the table string, so that the "NA" can be formatted differnetly
  df <- df %>% mutate(across(everything(), as.character))  
  df <- df %>% replace_na(list(Boys = "—", Girls = "—", All = "—"))
  #df <- df %>% mutate_all(~if_else(. is.na(), "—", .))
  
  table <- kable(df) %>% kable_styling(latex_options = "striped", font_size=8,
                                       bootstrap_options = c("condensed")) 
  
  return(table)
}

#--------------------#
# G2: LP by Sex      #
#--------------------#

g2 <- function() {
  
  lp <- lp_all %>% drop_na(LP_mf)
  
  for (i in unique(lp$countrycode)) {
    print(i)
    colours <- c('#1d6ca5', '#47849d', '#709c7b', '#90b433', '#c3c535', '#e8d856', '#f0f472')
    
    # Diagonal fill
    BR <- data.frame(x=c(0, 100, 100), y=c(0, 0, 100))
    
    # region legend
    #pop_legend <- readPNG(paste0(TemplateFolder,"/Rmd_template_images/population_legend.png"))
    
    
    ggplot() +
      geom_polygon(data=BR, aes(x, y), fill="grey", alpha=0.1) +
      xlim(0, 100) +
      ylim(0, 100) +
      geom_point(data = lp_all,
                 aes(x = LP_f, y = LP_m, fill = region, color = region, size = population_2019_all),
                 alpha = 0.2, shape = 21) +
      geom_point(data = lp_all %>% filter(countrycode == i),
                 aes(x = LP_f, y = LP_m, fill = region, size = population_2019_all),
                 alpha = 1, shape = 21, color = "grey40") +
      geom_text(data  = lp_all %>% filter(countrycode == i), aes(x=LP_f, y=LP_m+3, label = countrycode),size=10) +
      geom_point(aes(x=90, y=11), size=24, shape=1, color="black") +
      geom_point(aes(x=90, y=9.2), size=10, shape=1, color="black") +
      geom_point(aes(x=90, y=8.2), size=3, shape=1, color="black") +
      scale_size(range = c(.1, 24), name = "Population",
                 breaks=c(500000,3000000,25000000, 120000000),
                 labels = scales::label_number_si(accuracy = 1, suffix = ""))+
      scale_fill_manual(values = colours) +
      scale_color_manual(values = colours) +
      labs(y = "Learning Poverty, Boys", x = "Learning Poverty, Girls") +
      theme_minimal() +
      coord_cartesian(clip="off") +
      theme(
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="bottom",
        legend.justification="right",
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        axis.text= element_text(size=30),
        axis.title = element_text(size=30),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 2, unit = "pt")
      ) +
      
      annotate("text", x = 15, y = 100, label = "Boys Disadvantaged", color="grey40",size=10)+
      annotate("text", x = 83, y = 4, label = "Girls Disadvantaged",color="grey40", size=10)+
      annotate("text", x = 90, y = 16.1, label = "Population (M)", color="grey20",size=6)+
      annotate("text", x = 87, y = 14.2, label = "120 -----", color="grey20",size=4)+
      annotate("text",x=87,y= 10.7,label="23 ------",color="grey20",size=4) +
      annotate("text",x=87,y= 8.5,label="1 ------",color="grey20",size=4) +
      guides(size = FALSE,
             fill = guide_legend(override.aes = list(size=8, color=NA),
                                 label.position = "top",
                                 nrow=1,
                                 
                                 
             ))
    
    ggsave(filename = file.path(GraphFolder, paste("fig2_", i, ".svg", sep = '')),width = 14,height = 14, bg="white")
  }
}

#--------------------------------------------#
# G3: Years of Assessment Participation      #
#--------------------------------------------#

g3 <- function() {
  adjustment = 3
  colors <- c("ILSA"="#3BA19B", "NLSA"="#FE9125","Other"="#FCCB53")
  bubblesize <- 12
  
  # Put a line break in sacmeq
  ass_all <- ass_all %>% mutate(glad1=ifelse(glad1=="SACMEQ", "SAC\nMEQ", glad1),
                                glad2=ifelse(glad1=="SACMEQ", "SAC\nMEQ", glad2))
  
  for (i in unique(lp$countrycode)) {
    
    
    print(i)
    cnt <- ass_all %>% filter(countrycode==i)
    cnt[cnt == ""] <- NA
    
    
    if (all(is.na(cnt$glad2))) {
      # First case where there is only 1 test across all years
      ggplot() +
        geom_blank(aes(color = type1), data = ass_all) +
        geom_point(data=cnt, aes(x=year,y=1), color="grey", size=bubblesize) +
        geom_point(data=cnt %>% filter(!is.na(glad1)), aes(x=year,y=1, color=type1),size=bubblesize) +
        theme_minimal() +
        coord_cartesian(ylim=c(0.5,1.5), clip="off") +
        labs(x="", y="") +
        theme(
          panel.grid = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="top",
          legend.text=element_text(size=20/adjustment),
          axis.text= element_text(size=35/adjustment),
          axis.text.y = element_blank(),
          axis.title = element_text(size=35/adjustment),
          legend.title = element_blank(),
          legend.justification="right"
        ) +
        scale_color_manual(values=colors, drop=FALSE
        ) +
        scale_x_continuous(breaks=c(2011,2016,2021)) +
        guides(color = guide_legend(override.aes = list(size=5),
                                    label.position = "top",
        )) +
        geom_text(data= cnt %>% filter(type1 != "NLSA"), # cases with ILSAs/AMPLs
                  aes(label = glad1, y = 1, x = year), color = "white", size = 6.5/adjustment) +
        geom_text(data= cnt %>% filter(type1 == "NLSA"),
                  aes(label = "NLSA", y = 1, x = year), color = "white", size = 6.5/adjustment) + # cases with NLAs
        geom_text(data= cnt %>% filter(gpf1 == 1 & type1=="ILSA"),
                  aes(label = "*", y = 1.2, x = year+0.3), color = "#3BA19B", size = 7.5/adjustment)+
        geom_text(data= cnt %>% filter(gpf1 == 1 & type1=="NLSA"),
                  aes(label = "*", y = 1.2, x = year+0.3), color = "#FE9125", size = 7.5/adjustment)+
        geom_text(data= cnt %>% filter(gpf1 == 1 & type1=="Other"),
                  aes(label = "*", y = 1.2, x = year+0.3), color = "#FCCB53", size = 7.5/adjustment)   #+
      #        annotate("text", x=2013.7, y=2,label ="* = Aligned with Global Proficiency Framework (GPF)")
      
      
      
    } else {
      # Second case where there is 2 tests in the same year (usually when countries have their own NLA + an international assessment year)
      ggplot() +
        geom_blank(aes(color = type1), data = ass_all) +
        geom_point(data=cnt, aes(x=year,y=1), color="grey", size=bubblesize) +
        geom_point(data=cnt %>% filter(!is.na(glad1)), aes(x=year,y=1, color=type1),size=bubblesize) +
        geom_point(data=cnt %>% filter(!is.na(glad2)), aes(x=year,y=1.6, color=type2),size=bubblesize) +
        theme_minimal() +
        coord_cartesian(ylim=c(0.5,1.9), clip="off") +
        labs(x="", y="") +
        theme(panel.grid = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position="top",
              axis.text= element_text(size=35/adjustment),
              axis.text.y = element_blank(),
              axis.title = element_text(size=35/adjustment),
              legend.title = element_blank(),
              legend.text=element_text(size=20/adjustment),
              legend.justification="right") +
        scale_color_manual(values=colors) +
        scale_x_continuous(breaks=c(2011,2016,2021)) +
        guides(color = guide_legend(override.aes = list(size=5), label.position = "top")) +
        geom_text(data= cnt %>% filter(type1 != "NLSA"), # cases with ILSAs/AMPLs
                  aes(label = glad1, y = 1, x = year), color = "white", size = 6.5/adjustment) +
        geom_text(data= cnt %>% filter(type2 != "NLSA"),
                  aes(label = glad2, y = 1.6, x = year), color = "white", size = 6.5/adjustment)+
        geom_text(data= cnt %>% filter(type1 == "NLSA"),
                  aes(label = "NLA", y = 1, x = year), color = "white", size = 6.5/adjustment) + # cases with NLAs
        geom_text(data= cnt %>% filter(type2 == "NLSA"),
                  aes(label = "NLA", y = 1.6, x = year), color = "white", size = 6.5/adjustment) +
        geom_text(data= cnt %>% filter(gpf1 == 1 & type1=="ILSA"),
                  aes(label = "*", y = 1.2, x = year+0.3), color = "#3BA19B", size = 7.5/adjustment) +
        geom_text(data= cnt %>% filter(gpf1 == 1 & type1=="NLSA"),
                  aes(label = "*", y = 1.2, x = year+0.3), color = "#FE9125", size = 7.5/adjustment) +
        geom_text(data= cnt %>% filter(gpf1 == 1 & type1=="Other"),
                  aes(label = "*", y = 1.2, x = year+0.3), color = "#FCCB53", size = 7.5/adjustment) +
        geom_text(data= cnt %>% filter(gpf2 == 1 & type1=="ILSA"),
                  aes(label = "*", y = 1.8, x = year+0.3), color = "#3BA19B", size = 7.5/adjustment) +
        geom_text(data= cnt %>% filter(gpf2 == 1 & type1=="NLSA"),
                  aes(label = "*", y = 1.8, x = year+0.3), color = "#FE9125", size = 7.5/adjustment) +
        geom_text(data= cnt %>% filter(gpf2 == 1 & type1=="Other"),
                  aes(label = "*", y = 1.8, x = year+0.3), color = "#FCCB53", size = 7.5/adjustment) #+
      #annotate("text", x=2013.5, y=2.4,label ="* = Aligned with Global Proficiency Framework (GPF)")
    }
    
    # Save
    ggsave(filename = file.path(GraphFolder, paste("fig3_", i, ".svg", sep = '')),width = 9,height = 2.5, bg="white")
    
  }
}

#-----------------------------------------#
# Graph 5: Filled Rectangle 
#-----------------------------------------#

# Dummy data for now 
students <- data.frame(
  countrycode = c("ALB", "ALB"),
  m_bully = c(0.5, 0.7),
  m_belong = c(0.8,0.1),
  gender = c("Girl", "Boy")
)

g4 <- function(xvar, color1, color2, lab) {
  
  cnt <- students %>% filter(countrycode==i)
  
  rect_df <- data.frame(
    xmin = c(0),
    xmax = c(1),
    ymin = c(0),
    ymax = c(1) #,
  )
  
  scale_factors <- cnt[[xvar]]
  
  labels <- lab
  
  
  # Define a function to scale the rectangle heights based on the scaling factor
  scale_rect_height <- function(height, factor) {
    box_height <- max(height)
    scaled_height <- factor * box_height
    return(scaled_height)
  }
  
  cnt <- cnt %>% select(c("countrycode", xvar, "gender"))
  
  students_wide <- cnt %>%
    pivot_wider(names_from = gender, values_from = xvar)
  
  # Use the function to scale the heights of the rectangles
  #### NOTE : :: : when getting the real data, make sure the girl boy order isn't messed up
  rect_df$girl <- mapply(scale_rect_height, rect_df$ymax - rect_df$ymin, scale_factors[1]) + rect_df$ymin
  rect_df$boy <- mapply(scale_rect_height, rect_df$ymax - rect_df$ymin, scale_factors[2]) + rect_df$ymin
  
  # Create a dataframe of the new scaled rectangles
  # GIRL
  scaled_rect_df <- data.frame(
    xmin = rect_df$xmin,
    xmax = rect_df$girl,
    ymin = rect_df$ymin,
    ymax = rect_df$girl,
    rect_color = color1 # Rename color column to avoid conflict
  )
  scaled_rect_df <- cbind(scaled_rect_df,scale_factors[1])
  scaled_rect_df <- cbind(scaled_rect_df, labels)
  
  # BOY
  scaled_rect_df_b <- data.frame(
    xmin = rect_df$xmin,
    xmax = rect_df$boy,
    ymin = rect_df$ymin,
    ymax = rect_df$boy,
    rect_color = color2 # Rename color column to avoid conflict
  )
  
  scaled_rect_df_b <- cbind(scaled_rect_df_b,scale_factors[2])
  scaled_rect_df_b <- cbind(scaled_rect_df_b, labels)
  
  # adjust fonsizes 
  if (students_wide$Girl <= 0.2) {
    girlsize <- 3
  } else {
    girlsize <- 4
    
  }
  
  if (students_wide$Boy <= 0.2) {
    boysize <- 3
  } else {
    boysize <- 4
  }
  
  if (abs(students_wide$Boy-students_wide$Girl)>0.5) {
    pos <- 0.15
  } else {
    pos <- ifelse(students_wide$Boy > students_wide$Girl, students_wide$Boy-0.1, students_wide$Girl-0.1)
  }
  
  if (students_wide$Girl < students_wide$Boy) {
    # Create the plot
    ggplot(rect_df) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                color = color1, fill="white", linewidth=1) +
      geom_rect(data = scaled_rect_df_b, aes(xmin = xmin, xmax = sqrt(xmax), ymin = ymin, ymax = sqrt(ymax)), fill = color1) +
      geom_rect(data = scaled_rect_df, aes(xmin = xmin+0.01, xmax = sqrt(xmax), ymin = ymin+0.01, ymax = sqrt(ymax)), fill = color2) +
      coord_equal() +
      theme(legend.position = "none") +
      theme_void() +
      geom_text(data = scaled_rect_df, aes(x = ((xmin + xmax) / 2), y = sqrt(ymax)+0.05, label = paste0(scale_factors[1] * 100,"%")),
                color = color2, size = girlsize) +
      geom_text(data = scaled_rect_df_b, aes(x = sqrt(xmax)+0.05, y = (ymin+ymax)/2, label = paste0(scale_factors[2] * 100,"%")),
                color = color1, size = boysize,angle = 270) +
      geom_text(aes(x = xmin+0.02, y = ymin+0.15, label=lab), color="white", size=3, hjust = 0)
    
    
  } else { 
    
    ggplot(rect_df) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                color = color1, fill="white", linewidth=1) +
      geom_rect(data = scaled_rect_df, aes(xmin = xmin+0.005, xmax = sqrt(xmax), ymin = ymin+0.005, ymax = sqrt(ymax)), fill = color2) +
      geom_rect(data = scaled_rect_df_b, aes(xmin = xmin, xmax = sqrt(xmax), ymin = ymin, ymax = sqrt(ymax)), fill = color1) +
      coord_equal() +
      theme(legend.position = "none") +
      theme_void() +
      geom_text(data = scaled_rect_df, aes(x = ((xmin + xmax) / 2), y = sqrt(ymax)+0.05, label = paste0(scale_factors[1] * 100,"%")),
                color = color2, size = girlsize) +
      geom_text(data = scaled_rect_df_b, aes(x = sqrt(xmax)+0.05, y = (ymin+ymax)/2, label = paste0(scale_factors[2] * 100,"%")),
                color = color1, size = boysize,angle = 270) +
      geom_text(aes(x = xmin+0.02, y = ymin+pos, label=lab), color="white", size=3, hjust = 0)
  }
  
  ggsave(filename = file.path(GraphFolder, paste("fig4_", xvar, "_", i, ".svg", sep = '')),width = 2,height = 2, bg="white")
  
  # make a dummy blank
  ggplot(rect_df) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              color = "white", fill="white", linewidth=1)+ theme_void()
  
  ggsave(filename = file.path(GraphFolder, paste("fig4.svg", sep = '')),width = 2,height = 2, bg="white")
  
  
}

# Set 1 (Assets): #00429d, #80a0ce
g4("m_belong", "#00429d", "#80a0ce", "Once A\n Month")

# Set 2 (Safety): #618fbf, #b0c7df
g4("m_bully", "#618fbf", "#b0c7df", "Feel like\nthey belong")

# Set 3 (Bullying): #85b7ce, #c2dbe7
g4("m_bully", "#85b7ce", "#c2dbe7", "Belongs")

# Set 4 (Belonging): #b1dfdb, #d8efed
g4("m_bully", "#b1dfdb", "#d8efed", "Belongs")

# Set 5 (Social Mobility): #baba78, #dcdcbb
g4("m_bully", "#baba78", "#dcdcbb", "Belongs")



#-----------------------------------------#
# Graph 6: Range Plot
#-----------------------------------------#

g6 <- function(xvar) {
  # https://stackoverflow.com/questions/66879914/how-to-make-range-plot-in-r
  
  equity <- equity %>% mutate(subgroup=ifelse(subgroup=="Rural", "Non-Urban", subgroup),
                              col=ifelse(label==1, 250, 600))
  
  col1 <- "#b1dfdb"
  col2 <- "#EDED9F"
  
  #equity_wide <-  equity %>% pivot_wider(
  #  id_cols = c("countrycode", "countryname","type"),
  #  names_from = "label",
  #  values_from = c("subgroup", "m_ldmath", "m_ldread", "m_ldscience", "col"),
  #  names_sep = "_"
  #)
  
  equity %>% 
    ggplot() +
    ggforce::geom_link2(aes(x=type, y=get(xvar), color = col), size=1.5,  lineend = "round") + 
    scale_color_gradient(low = col1, high = col2) +
    geom_point(data=equity %>% filter(label==1), aes(x=type, y=m_ldmath),color=col1,size=4) +
    geom_point(data=equity %>% filter(label==2), aes(x=type, y=m_ldmath),color=col2,size=4) +
    theme_minimal() + 
    coord_flip(clip="off") +
    theme(
      axis.line.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size=12),
      axis.ticks.x = element_blank(),  # Reduce spacing
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      legend.title = element_blank()
    ) + 
    labs(x="", y="") + 
    geom_text_repel(data=equity, aes(x=type, y = get(xvar), label = subgroup), vjust=-1, segment.color = 'transparent', color="grey50") + 
    geom_text(data=equity %>% filter(label==1), aes(x=type, y = get(xvar), label = get(xvar)), hjust=1.3, color=col1) +
    geom_text(data=equity %>% filter(label==2), aes(x=type, y = get(xvar), label = get(xvar)), hjust=-0.5, color="grey50")
  
  ggsave(filename = file.path(GraphFolder, paste("fig3_", xvar, "_", i, ".svg", sep = '')),width = 9,height = 4, bg="white")
  
  
}

#-----------------------------------------#
# Graph 7: Proficiency Levels Over Time
#-----------------------------------------#

g7 <- function(xvar) {
  
  # Keep countries that have the latest data
  mpl18 <- mpl %>% filter(year == yr)
  mpl18 <- unique(mpl18$countrycode)
  
  mpl <- mpl[mpl$countrycode %in% mpl18, ]
  
  # Order the proficiency levels
  mpl <- mpl %>% mutate(order=ifelse(level=="Basic",2, ifelse(level=="Below Minimum",3,
                                                              ifelse(level=="High", 1, level))),
                        a=ifelse(year==2018,1,0.5))
  
  mpl <- mpl %>% mutate(level=fct_reorder(level, as.integer(order)))
  
  
  ## add filter to only loop countries included in 2022 PISA
  ## add filter to calculate regional aggregate for the given country, per year
  for (i in unique(mpl$countrycode)) {
    
    print(i)
    
    
    cnt <- mpl %>% filter(countrycode==i)
    
    
    colours <- c( '#00429d', '#3e67ae', '#618fbf') #c( '#00429d', '#3e67ae', '#618fbf', '#85b7ce', '#b1dfdb', '#ffffe0')
    
    
    ggplot(cnt, aes(x = as.factor(year), fill = level, y = get(xvar) * 100, alpha = a)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = colours) +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),  # Reduce spacing
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()
      ) + 
      scale_alpha_identity()+
      geom_text(aes(label = get(xvar) * 100), vjust=1.5, position="stack",color = "white") + 
      guides(fill = guide_legend(reverse = TRUE))
    
    
    ggsave(filename = file.path(GraphFolder, paste("fig2_", xvar, "_", i, ".svg", sep = '')),width = 4,height = 4, bg="white")
    
    
    
    
  }
}

# ------------------------------------------------#
# Graph 8: Scatter Distribution over time         #
# ------------------------------------------------#
bmp_panel <- function(i) {
  ld <- read.csv(paste0(folder,"/073_outputs/PIRLS_Panel.csv"))
  ld[c('group', 'indicator')] <- str_split_fixed(ld$subgroup, '=', 2)
  ld <- ld %>% filter(group=="all")
  
  # create a sequence by group
  ld <- ld %>% group_by(code, group) %>% 
    mutate(n = row_number(),
           year = as.integer(year)) %>% 
    arrange(year)
  
  
  ld <- ld %>% mutate(g=ifelse(subgroup=="all", "All",
                               ifelse(subgroup=="male=0", "Girls",
                                      ifelse(subgroup=="male=1", "Boys",
                                             subgroup))),
                      bmp=m_bmp)
  
  
  cnt <- ld %>% filter(code==i & year == 2021)
  
  ## Create a median by group
  int <- ld %>% filter(year == 2021)
  int <- int %>% filter(countrycode!="TWN")
  # By region
  median <- int %>%
    group_by(reg) %>%
    summarise(p50_r = median(m_bmp))
  
  median <- median %>% filter(reg==cnt$reg)
  
  # Global
  median_g <- int %>%
    group_by(subgroup) %>%
    summarise(p50_g = median(m_bmp))
  
  median$p50_r <- round(median$p50_r*100,digits=1)
  
  median_g$p50_g <- round(median_g$p50_g*100, digits=1)
  
  # Position the median so it doesn't overlap 
  median$pos <- ifelse(median$p50_r-median_g$p50_g<(-1),median$p50_r-1.6, 
                       ifelse(median$p50_r-median_g$p50_g<1, median$p50_r+1.6, median$p50_r))
  
  median_g$pos <- ifelse(median$p50_r-median_g$p50_g<(1),median_g$p50_g-1, 
                         ifelse(median$p50_r-median_g$p50_g<(-1), median_g$p50_g+1, median_g$p50_g))  
  
  if ("BRA" %in% cnt$code | "ZAF" %in% cnt$code) {
    
    ggplot() +
      geom_point(data = ld %>% filter(group == "all"),
                 aes(x=year,y=bmp*100,color=bmp),alpha=0.8, size=4) +
      labs(x = "", y = "") + theme_minimal() +
      scale_x_continuous(limits = c(2001, 2021), 
                         expand = c(0.05, 0.05), breaks = seq(2001, 2021, by = 5)) + # modified expand and breaks
      ylim(0,100) +
      scale_color_gradientn(colors = c('#e6ffdd','#b7e1dc' , '#b9b5e4', '#c181ec', '#c735f5')) +
      theme(axis.line.y = element_blank(), 
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size=9, color="black"),
            axis.text.x = element_text(size=9, color="black"),
            panel.grid = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title.align = 0,
            legend.title=element_blank(),
            plot.margin = unit(c(0, 1, 0, 1), "cm"),
            legend.position = "none",
      ) +
      geom_text(data=ld %>% filter(code==i & year == 2021),
                aes(label =i, x=year, y = (bmp*100)+5), color="black", size=3) +
      geom_hline(yintercept = median_g$p50_g, linetype = "dashed", color = "#b6b6b6",size=0.2)+
      geom_point(data = ld %>% filter(code==i),
                 aes(x=year,y=bmp*100),fill="transparent",color = "black",shape=21, size=6.5) +
      geom_text(data=ld %>% filter(code==i),
                aes(label = round(bmp*100,digits=1), x=year, y = (bmp*100)), color="black", size=3,na.rm = TRUE) +
      annotation_custom(
        grob = textGrob(label = paste("Global Median:", median_g$p50_g), hjust = -0.05, gp = gpar(cex = 0.5, col="grey50")),
        ymin = median_g$pos,  # Vertical position of the textGrob
        ymax = median_g$pos,
        xmin = 1995,                 # Note: The grobs are positioned outside the plot area
        xmax = 1995
      ) +
      coord_cartesian(clip="off")
  } else {
    
    ggplot() +
      geom_point(data = ld %>% filter(group == "all"),
                 aes(x=year,y=bmp*100,color=bmp),alpha=0.8, size=4) +
      labs(x = "", y = "") + theme_minimal() +
      scale_x_continuous(limits = c(2001, 2021), 
                         expand = c(0.05, 0.05), breaks = seq(2001, 2021, by = 5)) + # modified expand and breaks
      ylim(0,100) +
      scale_color_gradientn(colors = c('#e6ffdd','#b7e1dc' , '#b9b5e4', '#c181ec', '#c735f5')) +
      theme(axis.line.y = element_blank(), 
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size=9, color="black"),
            axis.text.x = element_text(size=9, color="black"),
            panel.grid = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title.align = 0,
            legend.title=element_blank(),
            plot.margin = unit(c(0, 1, 0, 1), "cm"),
            legend.position = "none",
      ) +
      geom_text(data=ld %>% filter(code==i & year == 2021),
                aes(label =i, x=year, y = (bmp*100)+5), color="black", size=3) +
      geom_hline(yintercept = median$p50_r, linetype = "dashed", color = "#b6b6b6",size=0.2)+
      geom_hline(yintercept = median_g$p50_g, linetype = "dashed", color = "#b6b6b6",size=0.2)+
      geom_point(data = ld %>% filter(code==i),
                 aes(x=year,y=bmp*100),fill="transparent",color = "black",shape=21, size=6.5) +
      geom_text(data=ld %>% filter(code==i),
                aes(label = round(bmp*100,digits=1), x=year, y = (bmp*100)), color="black", size=3,na.rm = TRUE) +
      annotation_custom(
        grob = textGrob(label = paste("Global Median:", median_g$p50_g), hjust = -0.05, gp = gpar(cex = 0.5, col="grey50")),
        ymin = median_g$pos,  # Vertical position of the textGrob
        ymax = median_g$pos,
        xmin = 1995,                 # Note: The grobs are positioned outside the plot area
        xmax = 1995
      ) +
      annotation_custom(
        grob = textGrob(label = paste(cnt$reg, "Median:", median$p50_r), hjust = -0.05, gp = gpar(cex = 0.5, col="grey50")),
        ymin = median$pos,  # Vertical position of the textGrob
        ymax = median$pos,
        xmin = 1995,                 # Note: The grobs are positioned outside the plot area
        xmax = 1995
      ) +
      coord_cartesian(clip="off")
  }
  
  
  
  
  ggsave(paste0(GraphFolder, "/g1",i, ".svg"), width=3.7, height=3.7, bg="white")
  
  
  
}


# ---------------------------------#
# Graph 9: Make efficient filters for loops         #
# ---------------------------------#
bmp_subgroup <- function(i) {
  
  
  ld <- read.csv(paste0(folder,"/073_outputs/PIRLS_Panel.csv"))
  

  ld[c('group', 'indicator')] <- str_split_fixed(ld$subgroup, '=', 2)
  ld <- ld %>% filter(year == 2021 | year == 2016)
  ld <- ld %>% filter(subgroup != "all" & subgroup!= "has_qescs=0" &  
                        subgroup!= "has_qescs=1" & group != "ece") 
  colnames(ld)
  # create a sequence by group
  ld <- ld %>% mutate(g=ifelse(subgroup=="all", "All",
                               ifelse(subgroup=="male=0", "Female",
                                      ifelse(subgroup=="male=1", "Male",
                                             ifelse(subgroup=="urban=0", "Rural",
                                                    ifelse(subgroup=="urban=1", "Urban",
                                                           indicator))))),
                      bmp=m_bmp,
                      label = ifelse(group=="male", "Sex",
                                     ifelse(group=="urban", "Location",
                                            "ESCS")))
  ld <- ld %>% mutate(g = ifelse(g=="1", "Poorest: Q1", 
                                 ifelse(g=="2", "Q2",
                                        ifelse(g=="3", "Q3",
                                               ifelse(g=="4", "Q4",
                                                      ifelse(g=="5", "Richest: Q5",
                                                             g))))))
  
  # Save an empty graph in case quintile data doesn't exist
  ggplot() + theme_void()
  
  ggsave(paste0(GraphFolder, "/g2",i, "_qescs.svg"), width=4, height=6, bg="white")
  
  
  ld <- ld %>% filter(code==i)
  colors <- c('#bca1e7', '#b7c7e1', '#b9e8db')
  
  ld$m_bmp <- round(ld$m_bmp*100,digits=1)
  max=max(ld$m_bmp) 
  
  k = 1
  for (j in unique(ld$group)) {
    
    
    df_year <- ld
    #  j <- "qescs"
    df_year <- df_year[df_year$group==j,]
    
    
    ## Save a longer image if 2016 comparison is available
    if (2016 %in% df_year$year) {
      h=6
      
    } else {
      h=4
    }
    color <- colors[k]
    
    ### Now Save Each Subgroup
    ggplot(data = df_year %>% filter(code==i)) +
      # First bar chart
      geom_bar(aes(x = reorder(g, m_bmp), y = m_bmp), fill = color, position="dodge", stat = "identity",width=0.9) +
      geom_text(aes(x = g, y = (m_bmp)+2, label = sprintf("%.1f",m_bmp)),color="black",size=5) +
      coord_flip(clip="off") +
      theme_minimal() +
      ylim(0,(max)+10) +
      labs(x="", y="") +
      facet_wrap(.~ year, ncol = 1) +
      scale_fill_manual(values=c("#f57360")) +
      theme(axis.line.y = element_blank(), 
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 8, hjust = -2),
            axis.text.y = element_text(size = 10),
            panel.grid = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title.align = 0,
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.position = "none",
            strip.text = element_text(size = 8))
    
    
    ggsave(paste0(GraphFolder, "/g2",i, "_", j, ".svg"), width=4, height=h, bg="white")
    
    
    print(paste0(i," ", j))
    
    # Move to next color in the palette list
    k=k+1
    
    
  }
}

# ------------------------------------------#
# Graph 10: Distribution by Group           #
# ------------------------------------------#

reading1 <- function(i) {
  
  
  tryCatch({
    
    text2 <- read.csv(file.path(folder,"073_outputs/all_results.csv"), header = T)
    ## Subset 
    reading <- text2 %>% select(c(countrycode, countryname, teacherreadingtime, reg))
    
    reading2 <- reading %>% filter(countrycode==i)
    # mean by region
    mean_by_region <- reading %>%
      group_by(reg) %>%
      summarize(mean_region = mean( teacherreadingtime, na.rm = TRUE))
    
    mean_by_region = mean_by_region %>% filter(reg==reading2$reg) 
    
    # global mean
    mean_all <- reading %>%
      summarize(mean_all = mean(teacherreadingtime, na.rm = TRUE))
    
    # Bind results
    reading$mean_g <- mean_all$mean_all
    reading$mean_r <- mean_by_region$mean_region
    
    reading$teacherreadingtime <- round(reading$teacherreadingtime, digits=1)
    
    mean_by_region$pos <- ifelse(mean_by_region$mean_region>mean_all$mean_all,mean_by_region$mean_region+15,mean_by_region$mean_region-15)
    
    ## GRAPH Reading Time
    ggplot(reading %>% filter(!is.na(teacherreadingtime)), aes(x = reorder(countryname, -teacherreadingtime), y = teacherreadingtime, fill=reg)) +
      geom_bar(stat = "identity", alpha = 0.3) +
      geom_bar(data = reading %>% filter(countrycode == i), stat = "identity", alpha = 1, color="grey50") +
      scale_fill_manual(values = c('#c735f5', '#be94e9', '#b6d3df', '#f9ddbe', '#f1bb9f', '#ff87a2')) + 
      geom_hline(yintercept = mean_all$mean_all, color = "grey", linetype = "dashed", size = 0.8) +
      geom_hline(yintercept = mean_by_region$mean_region, color = "grey", linetype = "dashed", size = 0.8) +
      geom_text(data = reading %>% filter(countrycode == i), aes(label = teacherreadingtime), vjust = -0.2, color = "black", size = 3) +
      labs(y = "", x = "Minutes of Instructional Reading Time per Week") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = unit(c(0.4, 1.5, 0.4, 1), "cm"),
            axis.text.x=element_text(size=10, angle=90,hjust = 1, vjust=0.5, margin = margin(t = -10)),
            axis.title.x = element_text(size=10),
            legend.text =element_text(size=5),
            axis.text.y=element_text(size=10),
            legend.position="bottom",
            legend.title = element_blank()) +
      annotation_custom(
        grob = textGrob(label = paste("Global Median:", as.integer(reading$mean_g)), hjust = -0.05, gp = gpar(cex = 0.5, col="grey50")),
        ymin = mean_all$mean_all+15,  # Vertical position of the textGrob
        ymax = mean_all$mean_all+15,
        xmin = 45,                 # Note: The grobs are positioned outside the plot area
        xmax = 45
      ) +
      scale_x_discrete(expand = c(0, 0.5))+
      coord_cartesian(clip="off") +
      annotation_custom(
        grob = textGrob(label = paste(reading2$reg, "Median:", as.integer(mean_by_region$mean_region)), hjust = -0.05, gp = gpar(cex = 0.5, col="grey50")),
        ymin = mean_by_region$pos,  # Vertical position of the textGrob
        ymax = mean_by_region$pos,
        xmin = 45,                 # Note: The grobs are positioned outside the plot area
        xmax = 45
      )
    
    
    ggsave(paste0(GraphFolder, "/g7a",i, ".svg"), width=13, height=6, bg="white")
  }, error = function(err) {
    
    nodata_plot()
    
    ggsave(paste0(GraphFolder, "/g7a",i, ".svg"), width=4, height=4, bg="white")
    
    
  })
  
}

