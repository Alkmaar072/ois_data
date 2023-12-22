library(readr)
library(vcd)
library(dplyr)
library(ggplot2)
ois_data <- read_delim("D:/UVA/Jaar 3/Onderzoek van interactieve systemen/ois_data/Ois Experiment_December 17, 2023_10.53.csv", 
                                                    delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                                    skip = 1)
ois_data <- ois_data %>%
  mutate(correct_guess = ifelse(`Was jij aan het chatten met een mens of een Kunstmatige Intelligentie?` == `Daadwerkelijk mens/KI`, 1, 0))

ois_data_mens <- ois_data[ois_data$`Daadwerkelijk mens/KI` == 1, ]
ois_data_KI <- ois_data[ois_data$`Daadwerkelijk mens/KI` == 2, ]
ois_data_man <- ois_data[ois_data$`Wat is je geslacht` == 1, ]
ois_data_vrouw <- ois_data[ois_data$`Wat is je geslacht` == 2, ]

summary(ois_data)
#leeftijd
ggplot(ois_data, aes(x = as.factor(`Hoe oud ben je?`))) +
  geom_bar() +
  labs(title = "Distribution of Ages",
       x = "Age",
       y = "Count")
boxplot(ois_data$`Hoe oud ben je?`)

count_gebruik_KI <- ois_data %>%
  group_by(ois_data$`Maak je zelf gebruik van generative AI?`) %>%
  summarise(Count = n())

# Hoeveel hadden het goed ~ totale set
overeenkomst_count <- sum(ois_data$`Was jij aan het chatten met een mens of een Kunstmatige Intelligentie?` == ois_data$`Daadwerkelijk mens/KI`)
percentage_overeenkomst_all <- (overeenkomst_count / 60) * 100

# Hoeveel hadden het goed ~ totale mens
overeenkomst_count <- sum(ois_data_mens$`Was jij aan het chatten met een mens of een Kunstmatige Intelligentie?` == ois_data_mens$`Daadwerkelijk mens/KI`)
percentage_overeenkomst_mens <- (overeenkomst_count / 32) * 100

# Hoeveel hadden het goed ~ totale KI
overeenkomst_count <- sum(ois_data_KI$`Was jij aan het chatten met een mens of een Kunstmatige Intelligentie?` == ois_data_KI$`Daadwerkelijk mens/KI`)
percentage_overeenkomst_KI <- (overeenkomst_count / 28) * 100

# Hoeveel hadden het goed ~ man
overeenkomst_count <- sum(ois_data_man$`Was jij aan het chatten met een mens of een Kunstmatige Intelligentie?` == ois_data_man$`Daadwerkelijk mens/KI`)
percentage_overeenkomst_man <- (overeenkomst_count / 30) * 100

# Hoeveel hadden het goed ~ vrouw
overeenkomst_count <- sum(ois_data_vrouw$`Was jij aan het chatten met een mens of een Kunstmatige Intelligentie?` == ois_data_vrouw$`Daadwerkelijk mens/KI`)
percentage_overeenkomst_vrouw <- (overeenkomst_count / 30) * 100

# Wilcox / Mann-Whitney U
shapiro.test(ois_data$`Beantwoord de volgende vragen - Ik weet zeker of mijn gesprekpartner een mens is of niet`)
wilcox.test(`Beantwoord de volgende vragen - Ik weet zeker of mijn gesprekpartner een mens is of niet` ~ `Daadwerkelijk mens/KI`, data = ois_data)

shapiro.test(ois_data$`Beantwoord de volgende vragen - De reactietijd was snel`)
wilcox.test(`Beantwoord de volgende vragen - De reactietijd was snel` ~ `Daadwerkelijk mens/KI`, data = ois_data)

# Bereken de contingency coefficient
assocstats(table(ois_data$`Wat is je geslacht`, ois_data$correct_guess))$cramer
assocstats(table(ois_data$`Beantwoord de volgende vragen - Ik weet zeker of mijn gesprekpartner een mens is of niet`, ois_data$correct_guess))$cramer
assocstats(table(ois_data$`Beantwoord de volgende vragen - De reactietijd was snel`, ois_data$correct_guess))$cramer

  #matig tot sterke associatie
assocstats(table(ois_data_KI$`Beantwoord de volgende vragen - Ik weet zeker of mijn gesprekpartner een mens is of niet`, ois_data_KI$correct_guess))$cramer
assocstats(table(ois_data_mens$`Beantwoord de volgende vragen - Ik weet zeker of mijn gesprekpartner een mens is of niet`, ois_data_mens$correct_guess))$cramer























