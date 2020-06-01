library(dplyr)
library(writexl)
library(tidyverse)


#Definimos la funcion %ni%
`%ni%` <- Negate(`%in%`)

#write_xlsx(clinical, "C:\\MASTER\\TFM\\DATA\\clinical.xlsx")

# las tablas que conectan las intervenciones de los ensayos clinicos con sus grupos
design_group_interventions <- read.table(file = "design_group_interventions.txt",
                                         header = TRUE,
                                         sep = "|",
                                         na.strings = "",
                                         comment.char = "",
                                         quote = "\"",
                                         fill = FALSE,
                                         nrows = 2000000)
# tipo de intervenciones
interventions <- read.table(file = "interventions.txt",
                            header = TRUE,
                            sep = "|",
                            na.strings = "",
                            comment.char = "",
                            quote = "\"",
                            fill = FALSE,
                            nrows = 2000000)
# tipos de grupos
design_groups <- read.table(file = "design_groups.txt",
                            header = TRUE,
                            sep = "|",
                            na.strings = "",
                            comment.char = "",
                            quote = "\"",
                            fill = FALSE,
                            nrows = 2000000)

interventions <- select(interventions, intervention_id = id, nct_id, intervention_type, name)
design_groups <- select(design_groups, design_group_id = id, group_type)
design_group_interventions <- select(design_group_interventions, intervention_id, design_group_id)

grupos_intervenciones <- left_join(interventions, design_group_interventions, by = "intervention_id")
grupos_intervenciones <- left_join(grupos_intervenciones, design_groups, by = "design_group_id")

grupos_intervenciones$nct_id <- as.character(grupos_intervenciones$nct_id)



#####################
#### CONDICIONES ####
#####################
conditions <- read.table(file = "conditions.txt",
                         header = TRUE,
                         sep = "|",
                         na.strings = "",
                         comment.char = "",
                         quote = "\"",
                         fill = FALSE,
                         nrows = 2000000)

conditions$nct_id <- as.character(conditions$nct_id)

clinical <- inner_join(grupos_intervenciones, conditions, by  ="nct_id")

clinical <- select(clinical, nct_id, intervention_type, group_type,
                   interventions = name.x, conditions = name.y)


# Lo convertomos a caracter
clinical$interventions = as.character(clinical$interventions)
clinical$conditions    = as.character(clinical$conditions)

# pasamos todo a minusculas
clinical$interventions = tolower(clinical$interventions)
clinical$conditions    = tolower(clinical$conditions)

clinical <- inner_join(id, clinical)



NCT <- inner_join(NCT, clinical)




View(clinical)




#################
#### STUDIES ####
#################
studies <- read.table(file = "studies.txt",
                      header = TRUE,
                      sep = "|",
                      na.strings = "",
                      comment.char = "",
                      quote = "\"",
                      fill = FALSE,
                      nrows = 2000000)
estudios <- studies
# Convertimos la variable start_date en tipo fecha
estudios$start_date <- as.Date(estudios$start_date)
# Seleccionamos los estudios que esten en: 
estudios$nct_id <- as.character(estudios$nct_id)
#   - Fase 3 y 4
#   - Estudios a partir del 2000
estudios <- filter(estudios, phase %in% c("Phase 3", "Phase 4") & start_date > "2005-01-01")
estudios <- select(estudios, nct_id, study_type, phase, enrollment, overall_status, enrollment_type, number_of_arms, has_dmc)
# Estudios e id son nuestras principales tablas
# Usaremos id para filtrar por hecha y fases
id <- select(estudios, nct_id)



#################
#### DESIGNS ####
#################
designs <- read.table(file = "designs.txt",
                      header = TRUE,
                      sep = "|",
                      na.strings = "",
                      comment.char = "",
                      quote = "\"",
                      fill = FALSE,
                      nrows = 2000000)
disenno <- designs
disenno <- select(disenno, nct_id, allocation, primary_purpose, masking)
disenno$nct_id <- as.character(disenno$nct_id)
disenno <- inner_join(id, disenno)



#######################
#### ELIGIBILITIES ####
#######################
eligibilities <- read.table(file = "eligibilities.txt",
                            header = TRUE,
                            sep = "|",
                            na.strings = "",
                            comment.char = "",
                            quote = "\"",
                            fill = FALSE,
                            nrows = 2000000)
criterio <- eligibilities
criterio <- select(criterio, nct_id, gender, minimum_age, maximum_age, healthy_volunteers)
criterio$nct_id <- as.character(criterio$nct_id)
criterio <- inner_join(id, criterio)

criterio$minimum_age <- as.character(criterio$minimum_age)
criterio$maximum_age <- as.character(criterio$maximum_age)

for (j in 3:4) {
  for (i in 1:NROW(criterio)) {
    aux <- str_split(criterio[i,j], " ", simplify = TRUE)
    if (length(aux) == 2){
      n <- as.double(aux[1])
      if (aux[2] == "Minutes") {
        criterio[i,j] <- (n / 1) * (1 / 60) * (1 / 24)
      } else if (aux[2] == "Hours" | aux[2] == "Hour") {
        criterio[i,j] <- (n / 24)
      } else if (aux[2] == "Weeks" | aux[2] == "Week" ) {
        criterio[i,j] <- (n * 7)
      } else if (aux[2] == "Days" | aux[2] == "Day") {
        criterio[i,j] <- n
      } else if (aux[2] == "Months" | aux[2] == "Month") {
        criterio[i,j] <- (n * 30.417)
      } else if (aux[2] == "Years" | aux[2] == "Year") {
        criterio[i,j] <- (n * 365)
      }
    } else {
      criterio[i,j] <- NA
    }
  }
}
View(criterio)
criterio$minimum_age <- as.double(criterio$minimum_age)
criterio$maximum_age <- as.double(criterio$maximum_age)


####################
#### FACILITIES ####
####################
facilities <- read.table(file = "facilities.txt",
                         header = TRUE,
                         sep = "|",
                         na.strings = "",
                         comment.char = "",
                         quote = "\"",
                         fill = FALSE,
                         nrows = 20000000)
entidades <- facilities
entidades$nct_id <- as.character(entidades$nct_id)
entidades <- inner_join(id, entidades)
entidades <- select(entidades, nct_id, status, country)


##################
#### SPONSORS ####
##################
sponsors <- read.table(file = "sponsors.txt",
                       header = TRUE,
                       sep = "|",
                       na.strings = "",
                       comment.char = "",
                       quote = "\"",
                       fill = FALSE,
                       nrows = 2000000)
directores <- sponsors
directores$nct_id <- as.character(directores$nct_id)
aux <- directores %>% 
  group_by(nct_id) %>%
  summarise(n_collaborators=n()-1)

directores <- inner_join(aux, directores)

directores <- filter(directores, lead_or_collaborator %in% c("lead"))
directores <- select(directores, nct_id, n_collaborators, agency_class)
directores <- inner_join(id, directores)


###################
#### COUNTRIES ####
###################
countries <- read.table(file = "countries.txt",
                        header = TRUE,
                        sep = "|",
                        na.strings = "",
                        comment.char = "",
                        quote = "\"",
                        fill = FALSE,
                        nrows = 2000000)
paises <- countries
paises$nct_id <- as.character(paises$nct_id)

paises <- filter(paises, removed %ni% c("t"))

paises <- paises %>% 
  group_by(nct_id) %>%
  summarise(n_countries=n())

paises <- inner_join(id, paises, by="nct_id")



##########################################
##########################################
NCT <- 0
NCT <- inner_join(estudios, disenno)
NCT <- inner_join(NCT, criterio)
NCT <- inner_join(NCT, directores)
NCT <- inner_join(NCT, paises)
NCT <- inner_join(NCT, entidades)

NCT <- inner_join(NCT, intervenciones)
##########################################
##########################################
View(NCT)









#######################
#### INTERVENTIONS ####
#######################
interventions <- read.table(file = "interventions.txt",
                            header = TRUE,
                            sep = "|",
                            na.strings = "",
                            comment.char = "",
                            quote = "\"",
                            fill = FALSE,
                            nrows = 2000000)
intervenciones <- interventions
intervenciones$nct_id <- as.character(intervenciones$nct_id)
intervenciones <- select(intervenciones, nct_id, intervention_type)
intervenciones <- inner_join(id, intervenciones)



####################
#### CONDITIONS ####
####################
conditions <- read.table(file = "conditions.txt",
                            header = TRUE,
                            sep = "|",
                            na.strings = "",
                            comment.char = "",
                            quote = "\"",
                            fill = FALSE,
                            nrows = 20000000)
condiciones <- conditions
condiciones$nct_id <- as.character(condiciones$nct_id)
condiciones <- select(condiciones, nct_id, conditions_type = name)
condiciones <- inner_join(id, condiciones)








##############################
#### browse_interventions ####
##############################
browse_interventions <- read.table(file = "browse_interventions.txt",
                                   header = TRUE,
                                   sep = "|",
                                   na.strings = "",
                                   comment.char = "",
                                   quote = "\"",
                                   fill = FALSE,
                                   nrows = 2000000)
B_intervenciones <- browse_interventions
B_intervenciones <- select(B_intervenciones, nct_id, 
                         intervenciones = downcase_mesh_term)
B_intervenciones$nct_id <- as.character(B_intervenciones$nct_id)
B_intervenciones <- inner_join(id, B_intervenciones)


###########################
#### browse_conditions ####
###########################
browse_conditions <- read.table(file = "browse_conditions.txt",
                                header = TRUE,
                                sep = "|",
                                na.strings = "",
                                comment.char = "",
                                quote = "\"",
                                fill = FALSE,
                                nrows = 2000000)
B_condiciones <- browse_conditions
B_condiciones <- select(B_condiciones, nct_id,
                      condiciones = downcase_mesh_term)
B_condiciones$nct_id <- as.character(B_condiciones$nct_id)
B_condiciones <- inner_join(id, B_condiciones)




# Unimos ambas trabas, condicion e intervencion
Cond_Inter <- inner_join(B_condiciones, B_intervenciones, by='nct_id')

########## ESTA TABLA ES DE LA PRIMERA PARTE ############
# Creamos la tabla de contingencias
contingencias <- table(Cond_Inter$condiciones, Cond_Inter$intervenciones)
contingencias <- as.data.frame(contingencias)
contingencias <- filter(contingencias, Freq > 25)
View(contingencias)
write_xlsx(contingencias, "C:/MASTER/TFM/DATA/contingencias.xlsx")

cond  <- readxl::read_xlsx(path = "C:/MASTER/TFM/DATA/cond.xlsx", col_types = "text",trim_ws = TRUE)
inter <- readxl::read_xlsx(path = "C:/MASTER/TFM/DATA/inter.xlsx", col_types = "text",trim_ws = TRUE)


clinical <- filter(Cond_Inter, condiciones %in% cond$Var1, intervenciones %in% inter$Var2)




##########################################
##########################################
##########################################
##########################################
aux <- distinct(NCT)
NCT <- filter(NCT, status %in% c("Terminated", "Completed"))


write_xlsx(NCT, "C:/MASTER/TFM/DATA/NCT2.xlsx")


NCT <- distinct(NCT)

summary(NCT$status)







