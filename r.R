library(readr)
ois_data <- read_delim("D:/UVA/Jaar 3/Onderzoek van interactieve systemen/ois_data/Ois Experiment_December 17, 2023_10.53.csv", 
                                                    delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                                    skip = 1)
ois_data_mens <- ois_data[ois_data$`Daadwerkelijk mens/KI` == 1, ]
ois_data_KI <- ois_data[ois_data$`Daadwerkelijk mens/KI` == 2, ]

summary(ois_data)

box

# Hoeveel hadden het goed ~ totale set
overeenkomst_count <- sum(ois_data$`Was jij aan het chatten met een mens of een Kunstmatige Intelligentie?` == ois_data$`Daadwerkelijk mens/KI`)
percentage_overeenkomst_all <- (overeenkomst_count / 60) * 100

# Hoeveel hadden het goed ~ totale mens
overeenkomst_count <- sum(ois_data_mens$`Was jij aan het chatten met een mens of een Kunstmatige Intelligentie?` == ois_data_mens$`Daadwerkelijk mens/KI`)
percentage_overeenkomst_mens <- (overeenkomst_count / 32) * 100

# Hoeveel hadden het goed ~ totale KI
overeenkomst_count <- sum(ois_data_KI$`Was jij aan het chatten met een mens of een Kunstmatige Intelligentie?` == ois_data_KI$`Daadwerkelijk mens/KI`)
percentage_overeenkomst_KI <- (overeenkomst_count / 28) * 100


# Oefenen
shapiro.test(ois_data$`Hoe oud ben je?`)
shapiro.test(ois_data$`Beantwoord de volgende vragen - Ik weet zeker of mijn gesprekpartner een mens is of niet`)

wilcox.test(`Beantwoord de volgende vragen - Ik weet zeker of mijn gesprekpartner een mens is of niet` ~ `Daadwerkelijk mens/KI`, data = ois_data)

shapiro.test(ois_data$`Beantwoord de volgende vragen - De reactietijd was snel`)
wilcox.test(`Beantwoord de volgende vragen - De reactietijd was snel` ~ `Daadwerkelijk mens/KI`, data = ois_data)


# Hoeveel hadden het goed
overeenkomst_count <- sum(ois_data$`Was jij aan het chatten met een mens of een Kunstmatige Intelligentie?` == ois_data$`Daadwerkelijk mens/KI`)
totaal_aantal_rijen <- nrow(ois_data)
percentage_overeenkomst <- (overeenkomst_count / totaal_aantal_rijen) * 100
