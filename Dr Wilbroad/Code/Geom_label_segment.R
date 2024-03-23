# Create a data frame with label text and x, y positions
labels_data <- data.frame(
  label = c("Label 1", "Label 2", "Label 3"),
  x = c("Jan\n2020", "Feb\n2020", "Mar\n2020"), # Specify the x positions
  y = c(1.0, 1.0, 1.0)  # Set the y positions to be the same for all labels
)

labels_data <- labels_data %>% mutate(
  x = factor(x, c("Jan\n2020", "Feb\n2020", "Mar\n2020"))
)

# Create the label plot
label_plot <- ggplot(data = labels_data, aes(x = x, y = y, label = label)) +
  geom_text(color = "red") +
  theme_minimal()  # Removes background and axes


label_plot_segment <- ggplot(data = labels_data, aes(x = NULL, y = NULL)) +
  geom_labelsegment(mapping = aes(x = "Jan\n2020", xend = "Mar\n2020",
                                  y = 0.5, yend = 0.5,
                                  color = "red",
                                  label = "Text 1")) +
  theme_minimal()

# Print the label plot
print(label_plot_segment)



#################################################################################################################

quarters_data <- data.frame(
  quarter = c("Q1\n2020", "Q2\n2020", "Q3\n2020", "Q4\n2020", "Q1\n2021", "Q2\n2021", "Q3\n2021", "Q4\n2021")
) %>%
  mutate(quarter = factor(quarter,
                          levels = c("Q1\n2020", "Q2\n2020", "Q3\n2020", "Q4\n2020", "Q1\n2021", "Q2\n2021", "Q3\n2021", "Q4\n2021")))

labels_data_final <- data.frame(
  label = c("Hospital visitation by relatives limited to one person for only 30 minutes and sitting distance in ANC waiting areas (10 april -)",
            "Mandatory wearing of masks for all clients and visitors to the hospital (10 Apr -)",
            "Travel restrictions (11 April - 18 May)",
            "Government declared that there will be no lockdown in Tanzania (11 March)",
            "Government announced three days for prayers for God's protection and healing (17 April)",
            "Government declared that the pandemic was over in Tanzania and people should resume normal lives (4 July)"),
  x = c("Q2\n2020", "Q2\n2020", "Q2\n2020", "Q1\n2020", "Q2\n2020", "Q3\n2020"),
  xend = c("Q4\n2021", "Q4\n2021", "Q4\n2020", "Q4\n2020", "Q4\n2020", "Q4\n2020"),
  y = c(0, 0.5, 1, 1.5, 2, 2.5),
  yend = c(0, 0.5, 1, 1.5, 2, 2.5)
)



label_plot_segment <- ggplot(data = quarters_data, aes(x = quarter, y = NULL)) +
  geom_labelsegment(data = labels_data_final,
                    mapping = aes(x = x, xend = xend,
                                  y = y, yend = yend,
                                  color = "red",
                                  label = label),
                    size = 2.3) +
  theme_void() + theme(aspect.ratio = 0.22,
                       legend.position = "none")

# Print the label plot
print(label_plot_segment)

label_plot_segment/ quarterly_data
