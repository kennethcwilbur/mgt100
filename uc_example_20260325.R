library(tidyverse)
library(ggrepel)
setwd("/mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/images/")

campus<-c("Berkeley", "Davis", "Irvine", "LA", "Merced", "Riverside", "SD", "SB", "SC")

# cohort 4-year grad rates, https://www.universityofcalifornia.edu/about-us/information-center/ug-outcomes
gradfouryr2021<-c(.807, .684, .734, .856, .498, .666, .751, .730, .626)
gradfouryr2022<-c(.799, .707, .737, .852, .525, .672, .739, .707, .634)
gradfouryr2023<-c(.812, .700, .750, .862, .529, .671, .744, .719, .623)
gradfouryr2024<-c(.787, .678, .749, .873, .475, .605, .778, .726, .640)
gradfouryr2025<-c(.807, .706, .730, .868, .499, .640, .785, .768, .656)


# applications by  California freshmen applicants, https://www.ucop.edu/institutional-research-academic-planning/_files/factsheets/2023/table-1.1-freshman-applications-by-campus-and-residency.pdf
#     and also https://admission.universityofcalifornia.edu/counselors/_files/documents/california-first-year-application-data.pdf
# Note: minor discrepancies between the two sources due to revisions
CAHSFrApp2021<-c(62169,60941,77995,84148,22509,45290,76380,71209,49183)
CAHSFrApp2022<-c(72466,65414,84774,91588,26229,46498,84344,73595,53074)
CAHSFrApp2023<-c(72656,65156,86452,90786,26029,48061,84930,74930,54888)
CAHSFrApp2024<-c(72161,67963,87560,92325,26212,49142,88415,75552,57535)
CAHSFrApp2025<-c(72660,68923,86230,89333,43007,61312,87555,74646,52564)

# admissions of California freshmen applicants, https://www.ucop.edu/institutional-research-academic-planning/_files/factsheets/2023/admission-table-1-1.pdf
#      and also https://admission.universityofcalifornia.edu/counselors/_files/documents/california-first-year-application-data.pdf
# Note: minor discrepancies between the two sources due to revisions
CAHSFrAdm2021<-c(10484,23776,15713,8369,22444,29004,21740,19923,26828)
CAHSFrAdm2022<-c(10483,21130,15656,8425,24252,31095,20117,19643,22856)
CAHSFrAdm2023<-c(10976,23657,17574,8587,23799,32667,20685,21029,33066)
CAHSFrAdm2024<-c(10727,24878,19051,8790,24304,36702,22948,24445,35658)
CAHSFrAdm2025<-c(09872,25667,18619,8573,41764,53562,21609,23943,37410)

# combine 4 vectors into a data frame
df <- data.frame(
  campus, 
  CAHSFrApp2025, CAHSFrApp2025, CAHSFrApp2025,
  CAHSFrApp2024, CAHSFrApp2024, CAHSFrApp2024,
  CAHSFrApp2023, CAHSFrApp2022, CAHSFrApp2021,
  CAHSFrAdm2023, CAHSFrAdm2022, CAHSFrAdm2021,
  gradfouryr2023, gradfouryr2022, gradfouryr2021
) |>
  mutate(CAFrAdmRate2025=CAHSFrAdm2025/CAHSFrApp2025,
         CAFrAdmRate2024=CAHSFrAdm2024/CAHSFrApp2024,
         CAFrAdmRate2023=CAHSFrAdm2023/CAHSFrApp2023,
         CAFrAdmRate2022=CAHSFrAdm2022/CAHSFrApp2022,
         CAFrAdmRate2021=CAHSFrAdm2021/CAHSFrApp2021,
  ) |>
  mutate(GradRate2025=gradfouryr2025,
         GradRate2024=gradfouryr2024,
         GradRate2023=gradfouryr2023,
         GradRate2022=gradfouryr2022,
         GradRate2021=gradfouryr2021
  ) |>
  select(campus, CAFrAdmRate2025, CAFrAdmRate2024, CAFrAdmRate2023, 
         CAFrAdmRate2022, CAFrAdmRate2021, GradRate2025,
         GradRate2024, GradRate2023, GradRate2022, GradRate2021)


gp<-ggplot(df,aes(x=CAFrAdmRate2025, y=GradRate2025, label=campus)) +
  geom_point(color="black", shape=15) +
  geom_text_repel() +
  #  geom_smooth(method = "lm", se = TRUE, color = "blue", size = 1, aes(group=1))+
  labs(title="UC 2025 CA Freshman Admissions/Applications and 4-Year Grad. Rates",
       x="California Freshman Admissions / California Freshman Applications",
       y="4-Year Graduation Rate") +
  theme(    plot.background = element_rect(fill = "white", color = "white"),  # Sets the plot background color
            panel.background = element_rect(fill = "white", color = "white")  # Sets the panel background color
  )+
  theme_classic() +
  # set both axes to run from 0 to 1, labeled as %
  scale_x_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1))
print(gp)


df2 <- data.frame(
  campus, 
  CAHSFrApp2025, CAHSFrApp2024, CAHSFrApp2023, CAHSFrApp2022, CAHSFrApp2021,
  CAHSFrAdm2025, CAHSFrAdm2024, CAHSFrAdm2023, CAHSFrAdm2022, CAHSFrAdm2021,
  gradfouryr2025, gradfouryr2024, gradfouryr2023, gradfouryr2022, gradfouryr2021
) %>%
  pivot_longer(
    cols = -campus,
    names_to = c(".value", "year"),
    names_pattern = "(\\D+)(\\d+)"
  ) %>%
  mutate(
    CAFrAdmRate = as.numeric(CAHSFrAdm) / as.numeric(CAHSFrApp),
    GradRate = as.numeric(gradfouryr)
  ) %>%
  select(campus, year, CAFrAdmRate, GradRate) |>
  arrange(campus, year)


gm <- ggplot(df2, aes(x = CAFrAdmRate, y = GradRate, label = campus, group = campus)) +
  geom_point(aes(color = year, shape=year, group=campus)) +
  geom_line(aes(color = year, group = campus), alpha=.3, show.legend = TRUE) +  
  geom_text_repel(
    data = df2 %>% filter(year == "2024"),  # Filter to include only 2023 data points
    aes(label = campus),
    segment.color = NA,  # Remove line segments
    nudge_x = 0.01,  # Optional: adjust text position
    nudge_y = 0.01,  # Optional: adjust text position
    show.legend = FALSE, size=3,
  ) +
  #  geom_smooth(method = "lm", se = TRUE, color = "blue", size = 1, aes(group=1))+
  scale_color_manual(values = c("2021" = "gray88", "2022" = "gray66", 
                                "2023" = "gray44", "2024" = "gray22","2025" = "black")) +
  scale_shape_manual(values = c(
    "2021" = 3,   # +
    "2022" = 4,   # x
    "2023" = 17,  # filled triangle
    "2024" = 16,  # filled square
    "2025" = 15   # filled circle
  ))  +
  labs(title="CA Freshman Admissions/Applications and 4-Year Grad. Rates",
       subtitle = "More selective campuses were more stable",
       x="California Freshman Admissions / California Freshman Applications",
       y="4-Year Graduation Rate") +
  theme(    plot.background = element_rect(fill = "white", color = "white"),  # Sets the plot background color
            panel.background = element_rect(fill = "white", color = "white")  # Sets the panel background color
  )+
  theme_classic() +
  scale_x_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1))
print(gm)


ggsave("uc_pic.png", plot=gp, width=7, height=6, dpi=300)
ggsave("uc_mov.png", plot=gm, width=8, height=6, dpi=300)

# add a best-fit trend line to our 1-year graph
gps<-ggplot(df,aes(x=CAFrAdmRate2025, y=GradRate2025, label=campus)) +
  geom_point(color="black",shape=15) +
  geom_text_repel() +
  geom_smooth(method="lm", se=FALSE)+
  #  geom_smooth(method = "lm", se = TRUE, color = "blue", size = 1, aes(group=1))+
  labs(title="2025 CA Freshman Admissions/Applications and 4-Year Grad. Rates",
       x="California Freshman Admissions / California Freshman Applications",
       y="4-Year Graduation Rate") +
  theme(    plot.background = element_rect(fill = "white", color = "white"),  # Sets the plot background color
            panel.background = element_rect(fill = "white", color = "white")  # Sets the panel background color
  )+
  theme_classic() +
  # set both axes to run from 0 to 1, labeled as %
  scale_x_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1))
print(gps)
ggsave("uc_gps.png", plot=gps, width=7, height=6, dpi=300)

# next we want to use PCA to collapse these 2 dimensions into a single dimension

library(factoextra)
pca_data <- df[, c("CAFrAdmRate2025", "GradRate2025")]
pca_result <- prcomp(pca_data, scale = FALSE)  # scale = TRUE standardizes the variables
pc1_scores <- pca_result$x[, 1] 
g3<-ggplot(data.frame(campus = df$campus, PC1 = pc1_scores), 
           aes(x = PC1, y = 0, label = campus)) +  # Assign y = 1 
  geom_hline(yintercept = 0, color = "gray70", linetype="dashed", size = 1) +  # Adding a light blue line at y=0
  geom_point(size=3) +
  geom_text_repel(size = 5) + 
  labs(title = "UC Campuses Projected on First Principal Component",
       x = "PC1", y="") +
  theme(axis.text.y=element_blank(),   # Hide y axis text
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(), 
        panel.border = element_rect(fill = NA, color = "white", size = 0.5),
        axis.line = element_line())   # Hide y axis ticks
print(g3)
ggsave("uc_pca.png", plot=g3, width=7, height=3, dpi=300)
