
rm(list = ls())
library(ggplot2)
library(tidyr)


data <- data.frame(
  Risk = c("1", "2", "3"),
  Line1 = c(7.195, 12.712, 5.093),
  Line2 = c(7.175, 12.923, 4.902),
  Line3 = c(7.184, 12.834, 4.982)
)

# Convert data to long format
data_long <- gather(data, Line, Value, -Risk)

# Plot the line chart
p <- ggplot(data_long, aes(x = as.numeric(factor(Risk, levels = c("1", "2", "3"))), y = Value, color = Line, group = Line)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 15, size = 3) +
  scale_x_continuous(breaks = 1:3, labels = c("1", "2", "3")) +
  labs(x = "Risk", y = "Optimal capital allocations") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = "top",  # Move legend to the top
        legend.title = element_blank(),
        legend.background = element_blank(),  # Remove legend box
        legend.spacing.x = unit(0, "cm"),  # Remove horizontal spacing between legend items
        legend.box = "none") +  # Remove legend box
  scale_color_manual(labels = c("N", "ESN: ¦Ë=-20", "ESN: ¦Ë=10"), values = c("blue", "red", "green")) +  # Add legend
  labs(caption = "q=0.98, ¦Â=0.8") +  
  theme(plot.caption = element_text(face = "bold"))  

print(p)

# For penalty \beta-------------
rm(list = ls())


library(ggplot2)
library(tidyr)

# Data for the first plot
data1 <- data.frame(
  Risk = c("0.01", "0.1", "0.8"),
  Line1 = c(12.482, 12.498, 12.514),
  Line2 = c(12.707, 12.731, 12.755),
  Line3 = c(12.607, 12.626, 12.644)
)

# Data for the second plot
data2 <- data.frame(
  Risk = c("0.01", "0.1", "0.8"),
  Line1 = c(12.681, 12.697, 12.712),
  Line2 = c(12.881, 12.902, 12.923),
  Line3 = c(12.792, 12.813, 12.834)
)

# Convert data to long format for the first plot
data_long1 <- gather(data1, Line, Value, -Risk)

# Convert data to long format for the second plot
data_long2 <- gather(data2, Line, Value, -Risk)

# Plot the first line chart
p1 <- ggplot(data_long1, aes(x = Risk, y = Value, color = Line, group = Line)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 15, size = 3) +
  labs(x = "Penalty ¦Â", y = expression("Optimal capital allocation " * p[2]^"*")) +  
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = c(0.85, 0.85),  
        legend.title = element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")) +  
  scale_color_manual(labels = c("N", "ESN: ¦Ë=-20", "ESN: ¦Ë=10"), values = c("blue", "red", "green")) +  
  labs(caption = "q=0.95") +  
  theme(plot.caption = element_text(face = "bold"),
        legend.justification = c(1, 1), legend.position = c(0.99, 0.99))  

# Plot the second line chart
p2 <- ggplot(data_long2, aes(x = Risk, y = Value, color = Line, group = Line)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 15, size = 3) +
  labs(x = "Penalty ¦Â", y = expression("Optimal capital allocation " * p[2]^"*")) +  
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = c(0.85, 0.85),  
        legend.title = element_blank(),
        legend.box = "horizontal",
        legend.background = element_rect(color = "black", size = 0.5, linetype = "solid")) +  
  scale_color_manual(labels = c("N", "ESN: ¦Ë=-20", "ESN: ¦Ë=10"), values = c("blue", "red", "green")) +  
  labs(caption = "q=0.98") +  
  theme(plot.caption = element_text(face = "bold"),
        legend.justification = c(1, 1), legend.position = c(0.99, 0.99))  

# Print both plots
print(p1)
print(p2)




# merge risk q ------------
library(ggplot2)
library(tidyr)
library(cowplot)

# Define data for the first plot
data_plot1 <- data.frame(
  Risk = c("1", "2", "3"),
  Line1 = c(7.216, 12.514, 5.270),
  Line2 = c(7.195, 12.712, 5.093)
)

# Convert data to long format for the first plot
data_long_plot1 <- gather(data_plot1, Line, Value, -Risk)

# Plot the first line chart
p1 <- ggplot(data_long_plot1, aes(x = as.numeric(factor(Risk, levels = c("1", "2", "3"))), y = Value, color = Line)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 15, size = 3) +
  scale_x_continuous(breaks = 1:3, labels = c("1", "2", "3")) +
  labs(x = "Risk", y = "Optimal capital allocations") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = c(0.85, 0.85),  
        legend.title = element_blank(),
        legend.box.margin = margin(0),  
        legend.background = element_blank(),  
        legend.text = element_text(size = 8)) +  
  scale_color_manual(labels = c("N: q=0.95", "N: q=0.98"), values = c("blue", "red")) +
  labs(caption = "¦Â=0.8") + theme(legend.position = "top") +
  theme(plot.caption = element_text(face = "bold"))

# Define data for the second plot
data_plot2 <- data.frame(
  Risk = c("1", "2", "3"),
  Line1 = c(7.194, 12.755, 5.051),
  Line2 = c(7.175, 12.923, 4.902)
)

# Convert data to long format for the second plot
data_long_plot2 <- gather(data_plot2, Line, Value, -Risk)

# Plot the second line chart
p2 <- ggplot(data_long_plot2, aes(x = as.numeric(factor(Risk, levels = c("1", "2", "3"))), y = Value, color = Line)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 15, size = 3) +
  scale_x_continuous(breaks = 1:3, labels = c("1", "2", "3")) +
  labs(x = "Risk", y = "Optimal capital allocations") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = c(0.85, 0.85),  
        legend.title = element_blank(),
        legend.box.margin = margin(0),  
        legend.background = element_blank(),  
        legend.text = element_text(size = 8)) +  
  scale_color_manual(labels = c("ESN: q=0.95", "ESN: q=0.98"), values = c("blue", "red")) +
  labs(caption = "¦Ë=-20, ¦Â=0.8") + theme(legend.position = "top") +
  theme(plot.caption = element_text(face = "bold"))

# Define data for the third plot
data_plot3 <- data.frame(
  Risk = c("1", "2", "3"),
  Line1 = c(7.203, 12.644, 5.152),
  Line2 = c(7.184, 12.834, 4.982)
)

# Convert data to long format for the third plot
data_long_plot3 <- gather(data_plot3, Line, Value, -Risk)

# Plot the third line chart
p3 <- ggplot(data_long_plot3, aes(x = as.numeric(factor(Risk, levels = c("1", "2", "3"))), y = Value, color = Line)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 15, size = 3) +
  scale_x_continuous(breaks = 1:3, labels = c("1", "2", "3")) +
  labs(x = "Risk", y = "Optimal capital allocations") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = c(0.85, 0.85),  
        legend.title = element_blank(),
        legend.box.margin = margin(0),  
        legend.background = element_blank(),  
        legend.text = element_text(size = 8)) +  
  scale_color_manual(labels = c("ESN: q=0.95", "ESN: q=0.98"), values = c("blue", "red")) +
  labs(caption = "¦Ë=10, ¦Â=0.8") + theme(legend.position = "top") +
  theme(plot.caption = element_text(face = "bold"))

# Combine plots using cowplot
combined_plot <- plot_grid(p1, p2, p3, ncol = 3)

# Print the combined plot
print(combined_plot)




# merge p2--------------------
library(ggplot2)
library(tidyr)
library(cowplot)

# Data for the first plot
data1 <- data.frame(
  Risk = c("0.01", "0.1", "0.8"),
  Line1 = c(12.482, 12.498, 12.514),
  Line2 = c(12.707, 12.731, 12.755),
  Line3 = c(12.607, 12.626, 12.644)
)

# Data for the second plot
data2 <- data.frame(
  Risk = c("0.01", "0.1", "0.8"),
  Line1 = c(12.681, 12.697, 12.712),
  Line2 = c(12.881, 12.902, 12.923),
  Line3 = c(12.792, 12.813, 12.834)
)

# Convert data to long format for the first plot
data_long1 <- gather(data1, Line, Value, -Risk)

# Convert data to long format for the second plot
data_long2 <- gather(data2, Line, Value, -Risk)

# Plot the first line chart
p1 <- ggplot(data_long1, aes(x = Risk, y = Value, color = Line, group = Line)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 15, size = 3) +
  labs(x = "Penalty ¦Â", y = expression("Optimal capital allocation " * p[2]^"*"), caption = "q=0.95") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  scale_color_manual(name = "", labels = c("N", "ESN: ¦Ë=-20", "ESN: ¦Ë=10"), values = c("blue", "red", "green")) +
  theme(legend.position = "top")  

# Plot the second line chart
p2 <- ggplot(data_long2, aes(x = Risk, y = Value, color = Line, group = Line)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 15, size = 3) +
  labs(x = "Penalty ¦Â", y = expression("Optimal capital allocation " * p[2]^"*"), caption = "q=0.98") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  scale_color_manual(name = "", labels = c("N", "ESN: ¦Ë=-20", "ESN: ¦Ë=10"), values = c("blue", "red", "green")) +
  theme(legend.position = "top")  

# Combine plots using cowplot
combined_plots <- plot_grid(p1, p2, ncol = 2, align = "h", labels = "AUTO", label_size = 0)

# Print combined plot
print(combined_plots)


#merge p1-----------------
library(ggplot2)
library(tidyr)
library(cowplot)

# Data for the first plot
data1 <- data.frame(
  Risk = c("0.01", "0.1", "0.8"),
  Line1 = c(7.212, 7.212, 7.216),
  Line2 = c(7.189, 7.188, 7.194),
  Line3 = c(7.199, 7.199, 7.203)
)

# Data for the second plot
data2 <- data.frame(
  Risk = c("0.01", "0.1", "0.8"),
  Line1 = c(7.191, 7.191, 7.195),
  Line2 = c(7.170, 7.170, 7.175),
  Line3 = c(7.180, 7.180, 7.184)
)

# Convert data to long format for the first plot
data_long1 <- gather(data1, Line, Value, -Risk)

# Convert data to long format for the second plot
data_long2 <- gather(data2, Line, Value, -Risk)

# Plot the first line chart
p1 <- ggplot(data_long1, aes(x = Risk, y = Value, color = Line, group = Line)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 15, size = 3) +
  labs(x = "Penalty ¦Â", y = expression("Optimal capital allocation " * p[1]^"*"), caption = "q=0.95") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  scale_color_manual(name = "", labels = c("N", "ESN: ¦Ë=-20", "ESN: ¦Ë=10"), values = c("blue", "red", "green")) +
  theme(legend.position = "top")  

# Plot the second line chart
p2 <- ggplot(data_long2, aes(x = Risk, y = Value, color = Line, group = Line)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 15, size = 3) +
  labs(x = "Penalty ¦Â", y = expression("Optimal capital allocation " * p[1]^"*"), caption = "q=0.98") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  scale_color_manual(name = "", labels = c("N", "ESN: ¦Ë=-20", "ESN: ¦Ë=10"), values = c("blue", "red", "green")) +
  theme(legend.position = "top")  

# Combine plots using cowplot
combined_plots <- plot_grid(p1, p2, ncol = 2, align = "h", labels = "AUTO", label_size = 0)

# Print combined plot
print(combined_plots)




#merge p3-----------------
library(ggplot2)
library(tidyr)
library(cowplot)

# Data for the first plot
data1 <- data.frame(
  Risk = c("0.01", "0.1", "0.8"),
  Line1 = c(5.305, 5.290, 5.270),
  Line2 = c(5.104, 5.081, 5.051),
  Line3 = c(5.194, 5.176, 5.152)
)

# Data for the second plot
data2 <- data.frame(
  Risk = c("0.01", "0.1", "0.8"),
  Line1 = c(5.127, 5.112, 5.093),
  Line2 = c(4.949, 4.929, 4.902),
  Line3 = c(5.028, 5.008, 4.982)
)

# Convert data to long format for the first plot
data_long1 <- gather(data1, Line, Value, -Risk)

# Convert data to long format for the second plot
data_long2 <- gather(data2, Line, Value, -Risk)

# Plot the first line chart
p1 <- ggplot(data_long1, aes(x = Risk, y = Value, color = Line, group = Line)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 15, size = 3) +
  labs(x = "Penalty ¦Â", y = expression("Optimal capital allocation " * p[3]^"*"), caption = "q=0.95") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  scale_color_manual(name = "", labels = c("N", "ESN: ¦Ë=-20", "ESN: ¦Ë=10"), values = c("blue", "red", "green")) +
  theme(legend.position = "top")  

# Plot the second line chart
p2 <- ggplot(data_long2, aes(x = Risk, y = Value, color = Line, group = Line)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 15, size = 3) +
  labs(x = "Penalty ¦Â", y = expression("Optimal capital allocation " * p[3]^"*"), caption = "q=0.98") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  scale_color_manual(name = "", labels = c("N", "ESN: ¦Ë=-20", "ESN: ¦Ë=10"), values = c("blue", "red", "green")) +
  theme(legend.position = "top")  

# Combine plots using cowplot
combined_plots <- plot_grid(p1, p2, ncol = 2, align = "h", labels = "AUTO", label_size = 0)

# Print combined plot
print(combined_plots)
















