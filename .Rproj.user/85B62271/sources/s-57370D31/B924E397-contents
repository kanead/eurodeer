#'##########################################################################
#' ame1 project - studying the relation between home range size and activity
#' 2019/10/04
#' Adam Kane, Enrico Pirotta & Barry McMahon
#' https://mecoco.github.io/ame1.html
#' plotting the GPS data
############################################################################

#' Section 2: Plot the data ----
#' using leaflet
library(leaflet)
#' map a sample of the data because the data set is huge
sample_data <- mydata %>% sample_n(size = 100000)
sample_data
#' create a colour palette and colour code by study
pal <- colorFactor(palette = 'Accent',
                   domain = sample_data$study_areas_id)

#' make a map of the sample data that is colour coded by study
m1 <- leaflet(sample_data) %>%
  addTiles()  %>%
  # setView( lat=-27, lng=170 , zoom=4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(
    ~ longitude,
    ~ latitude,
    fillOpacity = 0.7,
    color = ~ pal(study_areas_id),
    radius = 3,
    stroke = FALSE
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~ study_areas_id,
    title = "Study Area",
    opacity = 1
  )
m1

#' we can filter one animal and plot it
#' I've included a scale bar here
m2 <- mydata %>% filter(animals_id == "769") %>%
  leaflet(.) %>%
  addTiles()  %>%
  # setView( lat=-27, lng=170 , zoom=4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(
    ~ longitude,
    ~ latitude,
    fillOpacity = 0.7,
    #  color = ~ pal(study_areas_id),
    radius = 3,
    stroke = FALSE
  )  %>%
  addScaleBar()
m2

#' or filter one study and plot it
#' create a colour palette and colour code by animal
studies <-c("1")
study1 <- mydata %>% filter(study_areas_id %in% studies) %>% droplevels()

pal1 <- colorFactor(palette = 'Set3',
                    domain = study1$animals_id)
m3 <- leaflet(study1) %>%
  addTiles()  %>%
  # setView( lat=-27, lng=170 , zoom=4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(
    ~ longitude,
    ~ latitude,
    fillOpacity = 0.7,
    color = ~ pal1(animals_id),
    radius = 3,
    stroke = FALSE
  )  %>%
  addScaleBar() %>%
  addLegend(
    "bottomright",
    pal = pal1,
    values = ~ animals_id,
    title = "Animal ID",
    opacity = 1
  )
m3