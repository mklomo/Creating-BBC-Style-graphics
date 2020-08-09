#installing the bbplot package from Github
devtools::install_github("bbc/bbplot")

#install and load required packages one by one
library(pacman)
p_load("tidyverse", "gapminder", 
       "ggalt", "R.utils",
       "png", "gridExtra", "ggpubr", "scales",
       "bbplot")

#Data from the gapminder package
Ghana_line_df <- gapminder %>%
  filter(country == "Ghana")

#Making the plot
Life_Expectancy_Ghana_plot_1 <- ggplot(data = Ghana_line_df, aes(x = year, y = lifeExp)) +
  geom_line(colour = "#1380A1", size  = 1) +
  geom_hline(yintercept = 0, size = 2, colour = "#333333") +
  labs(title = "Living Longer",
       subtitle = "The story of life expectancy in Ghana 1952-2007") +
  bbc_style()

#Making the plot 2
Life_Expectancy_Ghana_plot_2 <- ggplot(data = Ghana_line_df, aes(x = year, y = lifeExp)) +
  geom_line(colour = "#1380A1", size  = 1) +
  geom_hline(yintercept = 0, size = 2, colour = "#333333") +
  labs(title = "Living Longer",
       subtitle = "The story of life expectancy in Ghana 1952-2007") +
  bbc_style() +
  theme(panel.grid.major.x = element_line(color = "#cbcbcb"),
        panel.grid.major.y = element_blank())

#Saving out your finished chart
finalise_plot(plot_name = Life_Expectancy_Ghana_plot_1,
              source = "Source: Gapminder",
              save_filepath = "/Users/marvinlomo/github/Creating BBC Style graphics/Plots.png",
              width_pixels = 640,
              height_pixels = 550)

finalise_plot(plot_name = Life_Expectancy_Ghana_plot_2,
              source = "Source: Gapminder",
              save_filepath = "/Users/marvinlomo/github/Creating BBC Style graphics/Plot2.png",
              width_pixels = 640,
              height_pixels = 550)


#Making the plot 3
Norway_line_df <- gapminder %>%
  filter(country == "Norway")

Life_Expectancy_Norway_plot_1 <- ggplot(data = Norway_line_df, aes(x = year, y = lifeExp)) +
  geom_line(colour = "#1380A1", size = 1) +
  geom_hline(yintercept = 0, size = 2, colour = "#333333") +
  labs(title = "Living Longer",
       subtitle = "The story of life expectancy in Norway 1952-2007") +
  bbc_style()
  
#Finalizing plot
finalise_plot(plot_name = Life_Expectancy_Norway_plot_1,
              source = "Source: Gapminder",
              save_filepath = "/Users/marvinlomo/github/Creating BBC Style graphics/Life_Expectancy_Norway.png")


#Making a multiple line chart 4
comp <- c("Ghana", "Nigeria")

Ghana_Nigeria_Life_Exp <- gapminder %>%
  filter(country == comp)

Ghana_Nigeria_Life_Exp_Comp <- ggplot(data = Ghana_Nigeria_Life_Exp, aes(x = year, y = lifeExp, colour = country)) +
  geom_line(size = 1 ) +
  geom_hline(yintercept = 0, size = 2, colour = "#333333") +
  scale_color_manual(values = c("#FAAB18", "#1380A1")) +
  labs(title = "Living longer",
       subtitle = "Life expectancy in Ghana and Nigeria 1952-2007") +
  bbc_style()
  
finalise_plot(plot_name = Ghana_Nigeria_Life_Exp_Comp,
              source = "Source: Gapminder",
              save_filepath = "/Users/marvinlomo/github/Creating BBC Style graphics/Ghana_Nigeria_Life_Exp_Comp.png")

#Making a bar chart 5
#Prepare data
bar_df_2007 <- gapminder %>%
  filter(year == 2007 & continent == "Africa") %>%
  arrange(desc(lifeExp)) %>%
  head(6)


#Make the plot
bars_2007 <- ggplot(data = bar_df_2007, aes(reorder(x = country, -lifeExp), y = lifeExp, fill = country)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, size = 2, colour = "#333333") +
  labs(title = "Reunion is highest",
       subtitle = "Highest African life expectancy, 2007") +
  scale_fill_brewer(palette = "Set1") +
  bbc_style()
  
finalise_plot(plot_name = bars_2007,
              source = "Source: Gapminder",
              save_filepath = "/Users/marvinlomo/github/Creating BBC Style graphics/bars_2007.png")


#Make a Grouped bar chart 6
#Prepare Data
grouped_bar_df <- gapminder %>%
  filter(year == 1967 | year == 2007) %>%
  select(country, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp) %>%
  mutate(gap = `2007` - `1967`) %>%
  arrange(desc(gap)) %>%
  head(6) %>%
  pivot_longer(cols = c(-country, -gap), names_to = "Year", values_to = "LifeExp")
  
#Making the plot  
grouped_df <- ggplot(data = grouped_bar_df, aes(x = country, y = LifeExp, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, size = 2, colour = "#333333") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18")) +
  labs(title = "We are living longer",
       subtitle = "Biggest life expectency rise, 1967-2007") +
  bbc_style()
  
#Publishing the plot
finalise_plot(plot_name = grouped_df,
              source = "Source: Gapminder",
              save_filepath = "/Users/marvinlomo/github/Creating BBC Style graphics/grouped_df.png")

#Colour bar chart conditionally
#To do this, we use the ifelse() command 
#Step 1 - Prepare the data
cond_bar_chart_df <- gapminder %>%
  filter(continent == "Asia" & year == "2007") %>%
  arrange(desc(lifeExp)) %>%
  head(5)
  

#Step 2 - Make the plot
cond_bar_chart <- ggplot(data = cond_bar_chart_df, aes(reorder(x = country, lifeExp), y = lifeExp)) +
  geom_bar(stat = "identity", position = "identity", fill = ifelse(cond_bar_chart_df$country == "Singapore", "#1380A1", "#dddddd")) +
  geom_hline(yintercept = 0, size = 3, colour = "#333333") +
  geom_label(aes(x = country, y = lifeExp, label = round(lifeExp, 0)), hjust = 1, vjust = 0.5,
           colour = "white", family = "Helvetica", size = 6, fill = NA, label.size = NA) +
  labs(title = "Singapore", 
             subtitle = "4th Ranking in Asia") +
  coord_flip() +
  bbc_style() +
  theme(panel.grid.major.y = element_blank())


#Step 3 - Publish the plot
finalise_plot(plot_name = cond_bar_chart,
              source = "Source: Gapminder",
              save_filepath = "/Users/marvinlomo/github/Creating BBC Style graphics/cond_bar_chart.png")


#Making a Dumbbell chart 7
#Preparing the data
dumbbell_df <- gapminder %>%
  filter(year == 1967 | year == 2007) %>%
  select(country, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp) %>%
  mutate(gap = `2007` - `1967`) %>%
  arrange(desc(gap)) %>%
  head(12)

#Make plot
dumbbell_plot <- ggplot(data = dumbbell_df, aes(x = `1967`, xend = `2007`,
                               y = reorder(country, gap))) +
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
  labs(title = "We are living longer",
       subtitle = "Biggest life Expectancy rise, 1967-2007") +
  bbc_style()


#Publish the plot
finalise_plot(plot_name = dumbbell_plot,
              source = "Source: Gapminder",
              save_filepath = "/Users/marvinlomo/github/Creating BBC Style graphics/dumbbell_plot.png")

#Make a histogram 8
#Prepare the data
hist_df <- gapminder %>%
  filter(year == 2007)

#Make the plot
hist_plot <- ggplot(data = hist_df, aes(x = lifeExp)) +
  geom_histogram(colour = "white", fill = "#1380A1", binwidth = 5) +
  geom_hline(yintercept = 0, size =3, colour = "#333333") +
  coord_cartesian(xlim = c(35, 95)) +
  scale_x_continuous(breaks = seq(40, 90, by = 10),
                     labels = c("40", "50", "60", "70", "80", "90 years")) +
  labs(title = "How life varies", 
       subtitle = "Distribution of Life Expectancy in 2007") +
  bbc_style()
  
#Publish the plot
finalise_plot(plot_name = hist_plot,
              source = "Source: Gapminder",
              save_filepath = "/Users/marvinlomo/github/Creating BBC Style graphics/hist_plot.png")


#Make a histogram 9
#Prepare the data
World_hist_lifeExp_2007_df <- gapminder %>%
  filter(year == 2007)

Africa_hist_lifeExp_2007_df <- gapminder %>%
  filter(year == 2007 & continent == "Africa")

ave_World_hist_lifeExp_2007 <- gapminder %>%
  filter(year == 2007) %>%
  summarise(mean = mean(lifeExp)) %>%
  .$mean

ave_Africa_hist_lifeExp_2007 <- gapminder %>%
  filter(year == 2007 & continent == "Africa") %>%
  summarise(mean = mean(lifeExp)) %>%
  .$mean


Africa_vs_World <- ggplot(data = World_hist_lifeExp_2007_df, aes(x = lifeExp)) +
  geom_histogram(colour = "white", fill = "#1380A1", alpha = 0.5, binwidth = 5) +
  geom_histogram(data = Africa_hist_lifeExp_2007_df, aes(x = lifeExp), colour = "white", fill = "#1380A1",  binwidth = 5) +
  geom_vline(xintercept = ave_Africa_hist_lifeExp_2007, lty = 2, colour = "#1380A1", size = 2) +
  geom_vline(xintercept = ave_World_hist_lifeExp_2007, lty = 2, colour = "#1380A1", alpha = 0.5, size = 2) +
  coord_cartesian(xlim = c(35, 95)) +
  scale_x_continuous(breaks = seq(40, 90, by = 10),
                     labels = c("40", "50", "60", "70", "80", "90 years")) +
  annotate(geom = "text", label = "Distribution of\nlife expectancy in\nAfrica for the year 2007", x = 35, y = 20, hjust = 0,
            vjust = 0.3, lineheight = 0.8, colour = "#1380A1", family = "Helvetica", size = 5.5) +
  annotate(geom = "text", label = "Distribution of\nlife expectancy in\nthe World for\nthe year 2007", x = 84, y = 30, hjust = 0,
           vjust = 0.5, lineheight = 0.8, colour = "#1380A1", alpha = 0.5, family = "Helvetica", size = 5.5) +
  geom_curve(aes(x = 45, y = 17, xend = 45, yend = 8), colour = "#555555", size = 0.5, curvature = 0.3, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(x = 90, y = 26, xend = 80, yend = 25), colour = "#555555", size = 0.5, curvature = -0.3, arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = "text", label = "54 years", x = 50, y = 31, colour = "#1380A1", family = "Helvetica", size = 7) +
  annotate(geom = "text", label = "67 years", x = 62, y = 31, colour = "#1380A1", alpha = 0.5, family = "Helvetica", size = 7) +
  labs(title = "Africans live shorter lives",
       subtitle = "Distribution of Life Expectancy for Africa and the World in 2007") +
  bbc_style()


#Publish the plot
finalise_plot(plot_name = Africa_vs_World,
              source = "Source: Gapminder",
              save_filepath = "/Users/marvinlomo/github/Creating BBC Style graphics/Africa_vs_World.png")


  
  
  











