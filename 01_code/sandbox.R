# WIP # SANDBOX
text2 <- read.csv(file.path(folder,"071_rawdata/all_results.csv"), header = T)
library(haven)

glad <- read_dta(file.path(folder,"071_rawdata/WLD_2021_PIRLS_v01_M_wrk_A_GLAD_ALL.dta"))

test <- glad %>% filter(countrycode == "IRN")


library(tidyverse)
library(hrbrthemes)
library(tm)
#library(proustr) remove stop words 

# Load dataset from github

# library
library(VennDiagram)

colnames(glad)
test <- test %>% select(c(countrycode, school_weight, cov_c_mitigation_a, cov_c_mitigation_b, cov_c_mitigation_c, cov_c_mitigation_d,
                          cov_c_mitigation_e, cov_c_mitigation_f))

test <- test %>%
  rowwise() %>%
  mutate(
    # ALL COMBINATIONS OF A
    cov_c_a = as.numeric(cov_c_mitigation_a == 1 & cov_c_mitigation_b == 0 & cov_c_mitigation_c == 0 & cov_c_mitigation_d == 0 & cov_c_mitigation_e == 0 & cov_c_mitigation_f == 0),
    cov_c_ab = as.numeric(cov_c_mitigation_a == 1 & cov_c_mitigation_b == 1 & cov_c_mitigation_c == 0 & cov_c_mitigation_d == 0 & cov_c_mitigation_e == 0 & cov_c_mitigation_f == 0),
    cov_c_abc = as.numeric(cov_c_mitigation_a == 1 & cov_c_mitigation_b == 1 & cov_c_mitigation_c == 1 & cov_c_mitigation_d == 0 & cov_c_mitigation_e == 0 & cov_c_mitigation_f == 0),
    cov_c_abcd = as.numeric(cov_c_mitigation_a == 1 & cov_c_mitigation_b == 1 & cov_c_mitigation_c == 1 & cov_c_mitigation_d == 1 & cov_c_mitigation_e == 0 & cov_c_mitigation_f == 0),
    cov_c_abcde = as.numeric(cov_c_mitigation_a == 1 & cov_c_mitigation_b == 1 & cov_c_mitigation_c == 1 & cov_c_mitigation_d == 1 & cov_c_mitigation_e == 1 & cov_c_mitigation_f == 0),
    cov_c_abcdef = as.numeric(cov_c_mitigation_a == 1 & cov_c_mitigation_b == 1 & cov_c_mitigation_c == 1 & cov_c_mitigation_d == 1 & cov_c_mitigation_e == 1 & cov_c_mitigation_f == 1),
    # ALL COMBINATIONS OF B
    cov_c_b = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 1 & cov_c_mitigation_c == 0 & cov_c_mitigation_d == 0 & cov_c_mitigation_e == 0 & cov_c_mitigation_f == 0),
    cov_c_bc = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 1 & cov_c_mitigation_c == 1 & cov_c_mitigation_d == 0 & cov_c_mitigation_e == 0 & cov_c_mitigation_f == 0),
    cov_c_bcd = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 1 & cov_c_mitigation_c == 1 & cov_c_mitigation_d == 1 & cov_c_mitigation_e == 0 & cov_c_mitigation_f == 0),
    cov_c_bcde = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 1 & cov_c_mitigation_c == 1 & cov_c_mitigation_d == 1 & cov_c_mitigation_e == 1 & cov_c_mitigation_f == 0),
    cov_c_bcdf = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 1),
    
    # ALL COMBINATIONS OF C
    cov_c_c = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 0 & cov_c_mitigation_c == 1 & cov_c_mitigation_d == 0 & cov_c_mitigation_e == 0 & cov_c_mitigation_f == 0),
    cov_c_cd = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 0 & cov_c_mitigation_c == 1 & cov_c_mitigation_d == 1 & cov_c_mitigation_e == 0 & cov_c_mitigation_f == 0),
    cov_c_cde = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 0 & cov_c_mitigation_c == 1 & cov_c_mitigation_d == 1 & cov_c_mitigation_e == 1 & cov_c_mitigation_f == 0),
    cov_c_cdef = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 0 & cov_c_mitigation_c == 1 & cov_c_mitigation_d == 1 & cov_c_mitigation_e == 1 & cov_c_mitigation_f == 1),
    
    # ALL COMBINATIONS OF D
    cov_c_d = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 0 & cov_c_mitigation_c == 0 & cov_c_mitigation_d == 1 & cov_c_mitigation_e == 0 & cov_c_mitigation_f == 0),
    cov_c_de = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 0 & cov_c_mitigation_c == 0 & cov_c_mitigation_d == 1 & cov_c_mitigation_e == 1 & cov_c_mitigation_f == 0),
    cov_c_def = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 0 & cov_c_mitigation_c == 0 & cov_c_mitigation_d == 1 & cov_c_mitigation_e == 1 & cov_c_mitigation_f == 1),
    
    # ALL COMBINATIONS OF E
    cov_c_e = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 0 & cov_c_mitigation_c == 0 & cov_c_mitigation_d == 0 & cov_c_mitigation_e == 1 & cov_c_mitigation_f == 0),
    cov_c_ef = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 0 & cov_c_mitigation_c == 0 & cov_c_mitigation_d == 0 & cov_c_mitigation_e == 1 & cov_c_mitigation_f == 1),
    
    # F
    cov_c_f = as.numeric(cov_c_mitigation_a == 0 & cov_c_mitigation_b == 0 & cov_c_mitigation_c == 0 & cov_c_mitigation_d == 0 & cov_c_mitigation_e ==0 &  cov_c_mitigation_f == 1)
    
  )



weighted_mean <- test %>%
  group_by(countrycode) %>%
  summarize(across(starts_with("cov_c_"), ~ weighted.mean(., w = school_weight, na.rm = TRUE), .names = "{.col}"))

weighted_mean <- weighted_mean %>%
  mutate(across(starts_with("cov_c"), ~ . * 100))

colnames(weighted_mean)
ggplot() + 
  geom_point(data = weighted_mean, aes(x = 1, y = 1, size = cov_c_mitigation_a), position = position_nudge(x = 0.02, y = 0.02)) +
  geom_point(data = weighted_mean, aes(x = 1, y = 1, size = cov_c_mitigation_b), position = position_nudge(x = -0.02, y = -0.02)) +
  geom_point(data = weighted_mean, aes(x = 1, y = 1, size = cov_c_mitigation_c), position = position_nudge(x = -0.01, y = 0.01)) +
  ylim(1, 1.3) + xlim(1, 1.3)

#test <- test %>%
#  pivot_longer(cols = starts_with("cov_c_"),
#               names_to = "cov_category",
#               values_to = "cov_value")

test <- test %>% filter(is.na(cov_value))
#Make the plot
venn.diagram(
  x = list(
    test %>% filter(cov_category=="cov_c_mitigation_a") %>% select(cov_value) %>% unlist() , 
    test %>% filter(cov_category=="cov_c_mitigation_b") %>% select(cov_value) %>% unlist() , 
    test %>% filter(cov_category=="cov_c_mitigation_c") %>% select(cov_value) %>% unlist()
  ),
  category.names = c("Printed Learning" , "Internet" , "Teacher"),
  filename = 'C:/Users/wb576985/GitHub/GLAD-Analytics/07_pirls_2021/07_2pager/073_interm_output/raw_output_2pagers/venn.png',
  output = TRUE ,
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  lwd = 1,
  col=c("#440154ff", '#21908dff', '#fde725ff'),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('#fde725ff',0.3)),
  cex = 0.5,
  fontfamily = "sans",
  cat.cex = 0.3,
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  cat.col = c("#440154ff", '#21908dff', '#fde725ff'),
  rotation = 1
)

############## PACKING CIRLCE
#install.packages("packcircles")
library(packcircles)
library(ggplot2)

weighted_long <- weighted_mean %>% select(c(countrycode, cov_c_mitigation_a, cov_c_mitigation_b, cov_c_mitigation_c, cov_c_mitigation_d,
                                            cov_c_mitigation_e, cov_c_mitigation_f))

weighted_long <- weighted_long %>% pivot_longer(cols = starts_with("cov_c_"),
                                                names_to = "category",
                                                values_to = "value")

# Create data

# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout((weighted_long$value)^2, sizetype='area')

# We can add these packing information to the initial data frame
weighted_long <- cbind(weighted_long, packing)

# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
# plot(data$radius, data$value)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot
ggplot(data = dat.gg) + 
  geom_polygon(aes(x=x, y=y, group = id, fill=as.factor(id)), colour = "white", alpha = 0.6) +
  # Add text in the center of each bubble + control its size
  geom_text(data = weighted_long, aes(x, y, size=10, label = category)) +
  geom_text(data = weighted_long, aes(x, y-10, size=10, label = value)) +
  scale_size_continuous(range = c(1,4)) +
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()


################ VEN DIAGRAM TUTORIAL
library(VennDiagram)

set.seed(2)

#install.packages("eulerr")
library("eulerr")
set.seed(10) #this seed changes the orientation of the sets         


df <- test
df <- distinct(df, countrycode, school_weight, .keep_all = TRUE)

colnames(df)
df <- df %>%select(c(cov_c_mitigation_a, cov_c_mitigation_b,cov_c_mitigation_c, cov_c_mitigation_d, cov_c_mitigation_e, cov_c_mitigation_f))



df <- df %>% filter(!is.na(cov_c_mitigation_a))
plot(euler(df), counts = T, fontface = 1)

sp_euler = with(df,
                euler(c("a"=sum(df$cov_c_mitigation_a),
                        "b"=sum(df$cov_c_mitigation_b),
                        "c"=sum(cov_c_mitigation_c),
                        "d" = sum(cov_c_mitigation_d),
                        "e" = sum(cov_c_mitigation_e),
                        "f" = sum(cov_c_mitigation_f),
                        "ab" =sum(cov_c_ab),
                        "abc" = sum(cov_c_abc),
                        "abcd" = sum(cov_c_abcd),
                        "abcde" = sum(cov_c_abcde),
                        "abcdef" = sum(cov_c_abcdef),
                        "bc"= sum(cov_c_bc),
                        "bcd" = sum(cov_c_bcd),
                        "bcde" = sum(cov_c_bcde),
                        "bcdf" = sum(cov_c_bcdf),
                        "cd" = sum(cov_c_cd),
                        "cde" = sum(cov_c_de),
                        "cdef" = sum(cov_c_cdef),
                        "de" = sum(cov_c_de),
                        "def" = sum(cov_c_def),
                        "ef" = sum(cov_c_ef)), 
                      input = "union"))




colnames(df)

plot(sp_euler,  counts = T, fontface = 1)


## Other venn diagram
library(ggVennDiagram)

x <- list(A=test$cov_c_mitigation_a, 
          B=test$cov_c_mitigation_b,
          C=test$cov_c_mitigation_c, 
          D=test$cov_c_mitigation_d,
          E=test$cov_c_mitigation_e, 
          F=test$cov_c_mitigation_f)

venn <- Venn(x)
data <- process_data(venn)

ggplot() +
  geom_sf(aes(fill=count), data = venn_region(data), alpha=0.3) +
  geom_sf(size = 2, lty = "dashed", color = "grey", data = venn_setedge(data), show.legend = F) +
  geom_sf_text(aes(label = name), data = venn_setlabel(data)) +
  geom_sf_label(aes(label=id), fontface = "bold", data = venn_region(data)) +
  theme_void()
#ggVennDiagram(x)




## Circles
#install.packages("ggforce")
library(ggforce)

values <- c(weighted_mean$cov_c_mitigation_a, weighted_mean$cov_c_mitigation_b, weighted_mean$cov_c_mitigation_c, weighted_mean$cov_c_mitigation_d, weighted_mean$cov_c_mitigation_e, weighted_mean$cov_c_mitigation_f)
labels <- c("Delivery of \n printed learning materials", "Internet-based resources", 
            "Access to \n digital devices", "Recommendations for teachers \n about how to provide \n online instruction", "Technical support \nfor teachers", "Access to \nDigital Devices \nfor Teachers")

id <- c("A", "B", "C", "D", "E", "F")
#set.seed(20)
circles2 <- data.frame(
  x0 = c(0, -1.5, 1.5, -2.5, 2.5, 0), #c(5.3,1.4, 18.2, 19.8, 7,13.5),   #runif(6, 0, 20), #c(2,9,13,15,18,3),
  y0 =   c(0, 1.5, 1.5, -1.5, -1.5, -3), #c(6.6,8.9,16.7,3.7,15,9.5), #runif(6, 0, 20),
  r = values,
  labels = labels,
  id = id
)


# Behold some circles
ggplot(circles2) +
  geom_circle(aes(x0 = x0, y0 = y0, r = (r)/45, fill = as.factor(r)), data = circles2, alpha=0.3, color="white") +
  geom_text(aes(x=x0,y=y0), label=labels) +
  geom_text(data = subset(circles2, id=="A"), aes(x = x0, y = y0, label = round(r),digits=1), vjust=4) +
  geom_text(data = subset(circles2, id=="B" | id == "C"), aes(x = x0, y = y0, label = round(r),digits=1), vjust = -12) +
  geom_text(data = subset(circles2, id=="D"), aes(x = x0, y = y0, label = round(r),digits=1),hjust=10) +
  geom_text(data = subset(circles2, id=="E"), aes(x = x0, y = y0, label = round(r),digits=1),hjust=-7.5) +
  geom_text(data = subset(circles2, id=="F"), aes(x = x0, y = y0, label = round(r),digits=1),vjust=12) +
  #geom_text(aes(x=x0,y=y0, label=as.integer(r)),vjust=-2) +
  geom_text(data=weighted_mean, aes(x=-0.5,y=0.5,label=as.integer(cov_c_ab*100))) +
  geom_text(data=weighted_mean, aes(x=0.1,y=1,label=as.integer(cov_c_abc*100))) +
  geom_text(data=weighted_mean, aes(x=0.1,y=1.8,label=as.integer(cov_c_bc*100))) +
  #  geom_text(data=weighted_mean, aes(x=0.5,y=0.5,label=as.integer(cov_c_ac))) +
  coord_fixed() + 
  theme_void() +
  theme(legend.position="none")
?group_by

#####

### PIE CHART ####
df <- distinct(test, countrycode, school_weight, .keep_all = TRUE)
df <- df %>% mutate(number = cov_c_mitigation_a+cov_c_mitigation_b+cov_c_mitigation_c+cov_c_mitigation_d+cov_c_mitigation_e+cov_c_mitigation_f)
#df <- df %>% select(c(school_weight, number))
df <- df[complete.cases(df),]

df2 <- df %>% group_by(number) %>% summarise(a = sum(cov_c_mitigation_a*school_weight), b=sum(cov_c_mitigation_b*school_weight),
                                             c=sum(cov_c_mitigation_c*school_weight), d=sum(cov_c_mitigation_d*school_weight),
                                             e=sum(cov_c_mitigation_e*school_weight), f=sum(cov_c_mitigation_f*school_weight))

df2 <- df2 %>% mutate(total=a+b+c+d+e+f)

sum <- sum(df2$total)

sum(df2$ashare)
df2 <- df2 %>% mutate(totalshare = (total/sum)*100,
                      ashare=(a/total)*100,
                      bshare=(b/total)*100,
                      cshare=(c/total)*100,
                      dshare=(d/total)*100,
                      eshare=(e/total)*100,
                      fshare=(f/total)*100)


df2 <- df2 %>% mutate(across(everything(), ~if_else(is.nan(.), 0, .)))

df2 <- df2 %>% select(c(number, totalshare, ashare, bshare, cshare,dshare,eshare,fshare))

df2 <- df2 %>%
  gather(key = variable, value = value, -number, -totalshare)

df2 <- df2 %>% filter(value>0.0000)

df3 <- df2
df3$totalshare[duplicated(df3$totalshare)] <- NA

colors <- c('#ff0000', '#ef8b52', '#d1ce98', '#ffa59e', '#dd4c65', '#93003a')

# Donut Chart

## Normal pie chart
ggplot(data=df2, aes(x="", y=value, group=variable, fill=variable, width=sqrt(totalshare)), colour="white") +
  geom_bar(width = 2, stat = "identity") +
  coord_polar("y", start=pi/3) + 
  facet_wrap(~number, ncol=3) +theme_void()+
  geom_text(data=df2 %>% filter(number!=6), aes(label = paste0(as.integer(value), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3) +
  geom_text(data=df3%>% filter(!is.na(totalshare)), aes(label = paste0(as.integer(totalshare), "%")), 
            position = position_stack(vjust = 0.5), hjust=2.0, 
            color = "black", size = 4) +
  scale_fill_manual(values = colors,
                    labels = c("Delivery of Printed\n Learning Material", "Internet Based \n resources for students", "Access to Digital Devices",
                               "Recommendations for teachers \n about how to provide  \n remote instruction", "Technical support for teachers",
                               "Access to digital devices for teachers")) +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.key.size = unit(1, "cm"),
        panel.spacing = unit(-2, "lines")) #+
#  labs(subtitle="Each circle represents the number of mitigation supports. \nThe size and enclosure of the circle represents the share in that number, and each color represents the support type")



## DONUT CHART ####
df2$ymax <- cumsum(df2$value)
df2$ymin <- c(0, head(df2$ymax, n=-1))


df2 <- df2 %>% mutate(name=ifelse(variable=="a", "Printed Learning Material",
                                  ifelse(variable=="b", "Internet Resources",
                                         ifelse(variable=="c", "Access to Digital Devices",
                                                ifelse(variable=="d", "Recommendations for teachers",
                                                       ifelse(variable=="e", "Technical support for teachers",
                                                              ifelse(variable=="f", "Access to digital devices for teachers","")))))))

df2 <- df %>% group_by(number) %>% summarise(a = sum(cov_c_mitigation_a*school_weight), b=sum(cov_c_mitigation_b*school_weight),
                                             c=sum(cov_c_mitigation_c*school_weight), d=sum(cov_c_mitigation_d*school_weight),
                                             e=sum(cov_c_mitigation_e*school_weight), f=sum(cov_c_mitigation_f*school_weight))


df2 <- df2 %>%
  gather(key = variable, value = value, -number)


PieDonut(df2, aes(number, name, count=value),r0 = 0.45, r1 = 0.8,start=3*pi/2,labelpositionThreshold=0.1)



?PieDonut
######################
wide_df <- df %>%
  pivot_wider(id_cols = school_weight, names_from = number, values_from = number)

wide_df <- wide_df %>%
  mutate(across(everything(), ~replace_na(., 0)))

wide_df <- wide_df %>% rename_with(~ paste0("num_", .), starts_with(c("0", "1", "2", "3", "4", "5", "6")))

wide_df <- wide_df %>%
  mutate(across(.cols = starts_with("num_"), ~ if_else(. != 0, 1, .)))

wide_df <- wide_df %>%
  mutate(across(starts_with("num_"), ~ weighted.mean(., w = school_weight, na.rm = TRUE), .names = "{.col}"))
#table(wide_df)
wide_df <- distinct(wide_df, num_1, num_2, num_3, num_4, num_5, num_6, num_0, .keep_all = TRUE)

wide_df <- wide_df %>% pivot_longer(cols = starts_with("num_"),
                                    names_to = "number",
                                    values_to = "share")


# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout((wide_df$share), sizetype='area')

# We can add these packing information to the initial data frame
df_l <- cbind(wide_df, packing)

df_l <- df_l %>% mutate(across(everything(), ~replace_na(., 0.01)))

# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
# plot(data$radius, data$value)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot
ggplot(data = dat.gg) + 
  geom_polygon(aes(x=x, y=y, group = id, fill=(id)), colour = "white", alpha = 0.6) +
  # Add text in the center of each bubble + control its size
  geom_text(data = df_l, aes(x, y, size=10, label = number)) +
  geom_text(data = df_l, aes(x, y, size=10, label = as.integer(share*100)), vjust=2) +
  #scale_size_continuous(range = c(1,4)) +
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()


wl_subset <- weighted_mean %>% select(cov_c_mitigation_a,cov_c_mitigation_b,cov_c_mitigation_c,cov_c_mitigation_d,cov_c_mitigation_e,cov_c_mitigation_f)

wl_subset <- wl_subset %>% pivot_longer(cols = starts_with("cov_c_"),
                                        names_to = "category",
                                        values_to = "value")

ggplot(data = wl_subset, aes(x=category)) + 
  geom_bar(aes(y = value), stat = "identity", fill = "blue", width = 0.15, position = position_dodge(0.3)) +
  ylab("Value") +
  xlab("Category") +
  ggtitle("Bar Chart") +
  theme_minimal()





########################## Map school closures ##############################################
unesco <- function(i) {
  unesco_full <- read_excel(paste0(folder, "/071_rawdata/unesco_closure.xlsx"))
  
  unesco_full <- unesco_full %>% select(-c(date_text2, day, date))
  
  unesco_long <- unesco_full %>% group_by(week, countryid) %>%
    count(status) %>%
    ungroup()
  
  
  unesco_full <- unesco_full %>% mutate(status=ifelse(status==-9,9,status))
  
  #unesco_wide <- unesco_full %>% group_by(week, countryid) %>%
  #  count(status) %>%
  #  pivot_wider(names_from = status, values_from = n, values_fill = 0) %>%
  #  ungroup()
  
  
  
  # for (i in unique(ld21$countrycode)) {
  # the right plot
  unesco_cnt <- unesco_long %>% filter(countryid==i)
  #unesco_cntw <- unesco_wide %>% filter(countryid==i)
  #unesco_cntw <- unesco_cntw %>% select(c("0","50","100","9"))
  
  #x = seq(1, nrow(unesco_cntw), by = 1) - 0.5  # middle points of bars
  #y = data.matrix(unesco_cntw)
  #y = y/rowSums(y)
  
  # 0, 50, 100,9
  my_colors <- c("#d8ffcf","#f47740", "#c9dda9", "grey80")
  ##e1af75,#c9dda9,#d8ffcf
  
  # 1. Open jpeg file
  # png(paste0(folder, "/073_outputs/g5b",i, ".png", dpi = 300, width = 8, height = 6, units = "in"))
  
  #  spiral_initialize(xlim = c(0, nrow(unesco_cntw)))
  #  spiral_track(background=FALSE, background_gp = gpar(col = NA, fill = NA))
  #  spiral_bars(x, y, gp = gpar(fill = my_colors, col = NA))
  #  spiral_text(x=1, y=1.1, text="2020",
  #              facing = "curved_inside", just = "right",
  #              gp=gpar(cex=1, fontfamily="Courier"))
  #  spiral_text(x=48, y=1.1, text="2021",
  #              facing = "curved_inside", just = "right",
  #              gp=gpar(cex=1, fontfamily="Courier"))
  #  spiral_text(x=99, y=1.1, text="2022",
  #              facing = "curved_inside", just = "right",
  #              gp=gpar(cex=1, fontfamily="Courier"))
  
  
  # 3. Close the file
  #  dev.off()
  
  
  # legend("bottomright", legend = c("Closed", "Partial", "Open 3", "Break"),
  #       fill = c("#5784BA","#a8d3ef", "#efc4a8", "grey60"), bg = "white", border = "black")
  # Create legend
  
  
  #spiral_initialize()
  #spiral_track()
  #spiral_lines(unesco_cnt$week, unesco_cnt$n, gp = gpar(col = ifelse(unesco_cnt$status==-9, "red", "blue")))
  
  ggplot(unesco_cnt, aes(fill=as.factor(status), y=n, x=week)) + 
    geom_bar(position="fill", stat="identity", width=0.95)+
    theme_void() +
    theme(legend.position="bottom") +
    scale_fill_manual(values = c( "grey80","#f47740","#e1af75", "#c9dda9"),
                      labels = c("Break", "Closed", "Partial", "Open"),
                      guide = guide_legend( keyheight = unit(7, units = "mm"), 
                                            keywidth=unit(18, units = "mm"), 
                                            label.position = "bottom", nrow=1,
                                            legend.key.size = unit(1, "lines"),
                                            
                                            
                      ),
    ) +
    theme(legend.position="bottom",
          legend.title = element_blank(),
          legend.spacing.y = unit(0.2, 'cm'),
          legend.spacing.x = unit(-0.05, 'cm'),
          legend.text = element_text(size=20)) 
  
  ggsave(paste0(folder, "/073_outputs/g5",i, ".png"), width=8, height=8, bg="white")
  
  
  
  #} 
}

