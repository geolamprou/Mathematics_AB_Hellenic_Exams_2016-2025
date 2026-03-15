#Libraries

library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(wordcloud2)
library(tidyr)
library(plotly)
library(collapsibleTree)
library(paletteer)
library(forcats)
library(patchwork)
library(gt)


# Import Dataset

stats_a <- read_excel("Statistics_A_B.xlsx", sheet = "Ζητούμενα_Α_Ποσοστά")

stats_b <- read_excel("Statistics_A_B.xlsx", sheet = "Ζητούμενα_Β_Ποσοστά")

type_of_functions_b <- read_excel("Statistics_A_B.xlsx", sheet = "Είδη_Συναρτήσεων_Β")


#Stats_Α

stats_a1 <- stats_a %>%
  filter(Κατηγορία == 'A1')

stats_a1 <- stats_a1 %>%
  mutate(`Θέμα` = fct_reorder(`Θέμα`, `Συχνότητα Εμφάνισης`, .desc = TRUE))

stats_a2_a3 <- stats_a %>%
  filter(Κατηγορία == 'A2-A3')

stats_a2_a3 <- stats_a2_a3 %>%
  mutate(`Θέμα` = fct_reorder(`Θέμα`, `Συχνότητα Εμφάνισης`, .desc = TRUE))

stats_a4_a5 <- stats_a %>%
  filter(Κατηγορία == 'A4-A5')

stats_a4_a5 <- stats_a4_a5 %>%
  mutate(`Θέμα` = fct_reorder(`Θέμα`, `Συχνότητα Εμφάνισης`, .desc = TRUE))

g_1 <- ggplot(stats_a1, aes(x=`Κατηγορία`, y=`Συχνότητα Εμφάνισης`, fill = `Θέμα`)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          geom_label(aes(label = `Χρονιές Εμφάνισης`), 
             position = position_dodge(width = 0.9), 
             vjust = 1.5,
             fontface = "bold",
             colour = "white",
             size = 4, 
             show.legend = FALSE)+
          geom_text(aes(label = `Συχνότητα Εμφάνισης`), 
                    position = position_dodge(width = 0.9), 
                    vjust = -0.3, colour = "black", size = 5) +
          ggtitle("Συχνότητα Αποδείξεων στο Θέμα Α1 - Περίοδος: 2016-2025") +
          theme(plot.title = element_text(hjust = 0.5)) +
          ylab("Συχνότητα") +
          theme_hc() +
          scale_fill_paletteer_d("ggsci::nrc_npg")

g_1

g_3 <- ggplot(stats_a4_a5, aes(x = `Θέμα`, y = `Συχνότητα Εμφάνισης`, fill = `Θέμα`)) +
        geom_bar(stat = 'identity') +
        ggtitle("Συχνότητα Είδους Ερωτημάτων στα Θέματα Α4 και Α5 - Περίοδος: 2016-2025") +
        theme(plot.title = element_text(hjust = 0.5)) +
        ylab("Συχνότητα") +
        geom_label(aes(label = `Χρονιές Εμφάνισης`), 
             position = position_dodge(width = 0.9), 
             vjust = 1.5,
             fontface = "bold",
             colour = "white",
             size = 4, 
             show.legend = FALSE)+
        geom_text(aes(label = `Συχνότητα Εμφάνισης`), vjust = -0.3, colour = "black", size = 5) +
        theme_hc() +
        scale_fill_paletteer_d("ggsci::nrc_npg")

g_3
  


table_a2_a3 <- stats_a2_a3 %>%
                  select(-`Κατηγορία`) %>%
                  gt() %>%
                  tab_header(
                    title = "Συχνότητα Ερωτημάτων Θεωρίας στα Θέματα Α2 και Α3",
                    subtitle = "Περίοδος: 2016-2025"
                  ) %>%
                  cols_align(align = "center") %>%
                  tab_style(
                    style = cell_fill(color = "white"),
                    locations = cells_body(rows = seq(1, nrow(stats_a1), 2))
                  ) %>%
                  tab_style(
                    style = cell_text(weight = "bold"),
                    locations = cells_column_labels()
                  ) %>%
                  opt_stylize(style = 6, color = "cyan")
table_a2_a3

#Stats_b

# Word cloud

wordcloud2(data = stats_b, size = 0.5, widgetsize = c(1200, 800), shape = 'circle' )

#Tree Diagram

tree_plot_functions <- collapsibleTree(type_of_functions_b, c("Έτος", "Είδος Συνάρτησης"), root = "Είδος Συνάρτησης ανά Χρονιά στο Θέμα Β", fontSize = 25)
tree_plot_functions


# Other Visualizations

g_b_1 <- ggplot(data = stats_b, aes(y= reorder(`Ζητούμενο`,`Συχνότητα Εμφάνισης`), x = `Συχνότητα Εμφάνισης`, fill = `Ζητούμενο`))+
  geom_bar(stat = 'identity', position = 'dodge') + 
  ggtitle("Συχνότητα των Ζητημάτων που εμφανίζοναι στο Θέμα Β - Περίοδος: 2016-2025") +
  theme(
    plot.title = element_text(hjust = 0.5, size=17),
    axis.text.y = element_text(size = 16)
  ) +
  ylab("Ζητούμενα")+
  geom_text(aes(label = `Συχνότητα Εμφάνισης`), position = position_dodge(width = 0.9), hjust = -0.3, colour = "black", size=5) +
  theme_hc() +
  scale_fill_paletteer_d("ggsci::nrc_npg")
g_b_1


freqeuncy_of_type_of_functions <- type_of_functions_b %>%
  count(`Είδος Συνάρτησης`)

freqeuncy_of_type_of_functions <- freqeuncy_of_type_of_functions %>%
  rename(`Συχνότητα` = n)

g_b_2 <- ggplot(freqeuncy_of_type_of_functions, aes(y = reorder(`Είδος Συνάρτησης`, `Συχνότητα`), x = `Συχνότητα`, fill = `Είδος Συνάρτησης`)) +
            geom_bar(stat = 'identity', position = 'dodge') + 
            ggtitle("Συχνότητα των Ειδών Συναρτήσεων που εμφανίζοναι στο Θέμα Β - Περίοδος: 2016-2025") +
            theme(
              plot.title = element_text(hjust = 0.5, size=17),
              axis.text.y = element_text(size = 16)
            ) +
            ylab("Είδος Συνάρτησης")+
            geom_text(aes(label = `Συχνότητα`), position = position_dodge(width = 0.9), hjust = -0.3, colour = "black", size=5) +
            theme_hc() +
            scale_fill_paletteer_d("ggsci::nrc_npg")
g_b_2