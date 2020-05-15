# Development data Shiny app written by Maria Bolt for CSC3011
# Imports and setup
library(dplyr)
library(reshape2)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(shiny)
library(shinythemes)

# Loading and cleaning data-- do this only when initially launched

# Importing a dataset that has the names and country codes for all the countries of the world as a basis
masterData <- read.table("Data/Countries&Codes.csv", sep=",", header=TRUE)
masterData <- masterData %>%
    select(country.code, name, region, sub.region) %>%
    rename(Country.Code=country.code, Country=name, Continent=region, Region=sub.region)

# Educational attainment by population aged 25 and over by sex: Primary, Secondary and Tertiary school
# Import datasets
ed_attain_primary <- read.table("Data/Ed_Attain_Primary.csv", sep=',', header=TRUE)
ed_attain_secondary <- read.table("Data/Ed_Attain_Secondary.csv", sep=',', header=TRUE)
ed_attain_tertiary <- read.table("Data/Ed_Attain_Tertiary.csv", sep=',', header=TRUE)
# Reshape each dataset to get the most recent attainment levels for each country, with columns for each sex
ed_attain_primary <- ed_attain_primary %>%
    select(Country.Code, Year, Sex, Value) %>%
    dcast(Country.Code+Year~Sex) %>%
    group_by(Country.Code) %>%
    top_n(1, Year) %>%
    rename(Primary.Attain.Both="Both sexes", Primary.Attain.Female=Female, Primary.Attain.Male=Male) %>%
    select(Country.Code, Primary.Attain.Both, Primary.Attain.Female, Primary.Attain.Male)
ed_attain_secondary <- ed_attain_secondary %>%
    select(Country.Code, Year, Sex, Value) %>%
    dcast(Country.Code+Year~Sex) %>%
    group_by(Country.Code) %>%
    top_n(1, Year) %>%
    rename(Secondary.Attain.Both="Both sexes", Secondary.Attain.Female=Female, Secondary.Attain.Male=Male) %>%
    select(Country.Code, Secondary.Attain.Both, Secondary.Attain.Female, Secondary.Attain.Male)
ed_attain_tertiary <- ed_attain_tertiary %>%
    select(Country.Code, Year, Sex, Value) %>%
    dcast(Country.Code+Year~Sex) %>%
    group_by(Country.Code) %>%
    top_n(1, Year) %>%
    rename(Tertiary.Attain.Both="Both sexes", Tertiary.Attain.Female=Female, Tertiary.Attain.Male=Male) %>%
    select(Country.Code, Tertiary.Attain.Both, Tertiary.Attain.Female, Tertiary.Attain.Male)
# Merge the datasets with the master on country code
masterData <- merge(masterData, ed_attain_primary, by="Country.Code", all.x=TRUE)
masterData <- merge(masterData, ed_attain_secondary, by="Country.Code", all.x=TRUE)
masterData <- merge(masterData, ed_attain_tertiary, by="Country.Code", all.x=TRUE)

# Gender parity index for gross enrollment ratios, primary secondary and tertiary school
# Import datasets
genderParityPrimary <- read.table("Data/GenderParityPrimary.csv", sep=',', header=TRUE)
genderParitySecondary <- read.table("Data/GenderParitySecondary.csv", sep=',', header=TRUE)
genderParityTertiary <- read.table("Data/GenderParityTertiary.csv", sep=',', header=TRUE)
# Select only the most recent data for each country and the minumum necessary columns for each dataset
genderParityPrimary <- genderParityPrimary %>%
    select(Country.Code, Year, Value) %>%
    group_by(Country.Code) %>%
    top_n(1, Year) %>%
    rename(Primary.Parity=Value) %>%
    select(Country.Code, Primary.Parity)
genderParitySecondary <- genderParitySecondary %>%
    select(Country.Code, Year, Value) %>%
    group_by(Country.Code) %>%
    top_n(1, Year) %>%
    rename(Secondary.Parity=Value) %>%
    select(Country.Code, Secondary.Parity)
genderParityTertiary <- genderParityTertiary %>%
    select(Country.Code, Year, Value) %>%
    group_by(Country.Code) %>%
    top_n(1, Year) %>%
    rename(Tertiary.Parity=Value) %>%
    select(Country.Code, Tertiary.Parity)
# Merge with the master dataset
masterData <- merge(masterData, genderParityPrimary, by="Country.Code", all.x=TRUE)
masterData <- merge(masterData, genderParitySecondary, by="Country.Code", all.x=TRUE)
masterData <- merge(masterData, genderParityTertiary, by="Country.Code", all.x=TRUE)

# Percent living in poverty by country
# Import data
percentInPoverty <- read.table("Data/Percent_Pop_National_Poverty_Line.csv", sep=",", header=TRUE, fill=TRUE)
# Select only most recent value for each country and necessary columns
percentInPoverty <- percentInPoverty %>%
    select(Country.Code, Year, Value) %>%
    group_by(Country.Code) %>%
    top_n(1, Year) %>%
    rename(Poverty.Percent=Value) %>%
    select(Country.Code, Poverty.Percent)
# Merge with the master data
masterData <- merge(masterData, percentInPoverty, by="Country.Code", all.x=TRUE)

# Core household indicators for ICT
# Import data
ictIndicators <- read.table("Data/ICT_Indicators.csv", sep=",", header=TRUE)
# Funky things happening with the individual computer and mobile values-- have to manually convert to char and then double
ictIndicators <- ictIndicators %>%
    select(Country.Code, HH.Computer, HH.Internet, HH.Mobile, IDV.Computer, IDV.Internet, IDV.Mobile) %>%
    transform(IDV.Computer = as.character(IDV.Computer)) %>%
    transform(IDV.Computer = as.double(IDV.Computer)) %>%
    transform(IDV.Mobile = as.character(IDV.Mobile)) %>%
    transform(IDV.Mobile = as.double(IDV.Mobile))
# Merge with the master data
masterData <- merge(masterData, ictIndicators, by="Country.Code", all.x=TRUE)

# Proportion of adults (15+) with an account at a bank or other financial institution or with a mobile-money-service provider
# Import data
formalBankingData <- read.table("Data/Financial_Institution_Membership.csv", sep=",", header=TRUE)
# Reshape each dataset to get the most recent attainment levels for each country, with columns for each sex
formalBankingData <- formalBankingData %>%
    select(Country.Code, Year, Sex, Value) %>%
    dcast(Country.Code+Year~Sex) %>%
    group_by(Country.Code) %>%
    top_n(1, Year) %>%
    rename(FI.Both="Both sexes", FI.Female=Female, FI.Male=Male) %>%
    select(Country.Code, FI.Both, FI.Female, FI.Male)
# Merge with master data
masterData <- merge(masterData, formalBankingData, by="Country.Code", all.x=TRUE)

# A dataset of population inidicators which includes life expectancy and under 5 mortality
# Import data
lifeData <- read.table("Data/WPP2019_Period_Indicators_Medium.csv", sep=",", header=TRUE)
# Get only 2015-2020 data for the two variables we want, calculate the percent for under 5 mortality
lifeData <- lifeData %>%
    filter(Time=="2015-2020") %>% 
    mutate(Under5.Mortality=Q5/10) %>%
    rename(Life.Expect=LEx) %>%
    select(LocID, Life.Expect, Under5.Mortality)
# Merge with master data
masterData <- merge(masterData, lifeData, by.x="Country.Code", by.y="LocID", all.x=TRUE)

# Youth literacy rates for ages 15-24 by sex
# Import data
literacyData <- read.table("Data/Youth_Literacy_Rate.csv", sep=",", header=TRUE)
# Reshape each dataset to get the most recent attainment levels for each country, with columns for each sex
literacyData <- literacyData %>%
    select(Country.Code, Year, Sex, Value) %>%
    dcast(Country.Code+Year~Sex) %>%
    group_by(Country.Code) %>%
    top_n(1, Year) %>%
    rename(Youth.Literacy.Both="Both sexes", Youth.Literacy.Female=Female, Youth.Literacy.Male=Male) %>%
    select(Country.Code, Youth.Literacy.Both, Youth.Literacy.Female, Youth.Literacy.Male)
# Merge with master data
masterData <- merge(masterData, literacyData, by="Country.Code", all.x=TRUE)

# Ensure that country codes are still unique after all the merges
masterData <- distinct(masterData, Country.Code, .keep_all=TRUE)

# Get the data for a country map of the world
data(country.map, package = "choroplethrMaps")

# Dictionaries for all the select boxes
xVariables <- c(
    "Completed School Percentage" = "Attain",
    "Gender Ratio in School Enrollment" = "Parity",
    "Youth (age 15-24) Literacy Percentage" = "Youth.Literacy",
    "Computer Usage Percentage" = "Computer",
    "Internet Usage Percentage" = "Internet",
    "Mobile Phone Usage Perctentage" = "Mobile",
    "Financial Institution Membership Percentage" = "FI"
)

yVariables <- c(
    "Life Expectancy at Birth" = "Life.Expect",
    "Child Under 5 Mortality Rate" = "Under5.Mortality",
    "Percentage At or Below Poverty Line" = "Poverty.Percent"
)

genderOptions <- c(
    "Female" = "Female",
    "Male" = "Male",
    "Both Genders" = "Both"
)

schoolLevelOptions <- c(
    "Primary School" = "Primary",
    "Secondary School" = "Secondary",
    "Post-Secondary School" = "Tertiary"
)

ictLevelOptions <- c(
    "Percentage of Households" = "HH",
    "Percentage of Individuals" = "IDV"
)

continents <- c(
    "Africa" = "Africa",
    "Americas" = "Americas",
    "Asia" = "Asia",
    "Europe" = "Europe",
    "Oceania" = "Oceania"
)

# Function that builds x variable name
buildXVar <- function(x, school, gender, ict){
    if(x == "Attain") {
        return(paste(school, x, gender, sep="."))
    }
    else if(x == "Parity") {
        return(paste(school, x, sep="."))
    }
    else if(x == "Computer" | x == "Internet" | x == "Mobile") {
        return(paste(ict, x, sep="."))
    }
    else if (x == "FI" | x == "Youth.Literacy") {
        return(paste(x, gender, sep="."))
    }
    else {
        return(x)
    }
}

# Function that gets the key from the value so we can label the axes
getLabel <- function(value, list) {
    i <- 1
    for (item in list) {
        if(item == value)
        {
            return(names(list)[i])
        }
        i <- i + 1
    }
    return("X")
}

# Defines the UI elements of the shiny app
ui <- fluidPage(
    theme = shinythemes::shinytheme("sandstone"),
    
    titlePanel(h1("Development Data Dashboard: Exploring Gender in Education, Information Communication Technology, and Financial Institution Membership", align = "center")
    ),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("xVar", h3("X Variable"), choices = xVariables, selected = 1),
            selectInput("yVar", h3("Y Variable"), choices = yVariables, selected = 1),
            selectInput("schoolLevel", h3("School Level"), choices = schoolLevelOptions, selected = 1),
            helpText("Only applies to Completed School Percentage and Gender Ratio in School Enrollment"),
            selectInput("gender", h3("Gender"), choices = genderOptions, selected = 1),
            helpText("Only applies to Completed School Percentage, Youth Literacy Percentage, and Financial Institution Membership"),
            selectInput("ictLevel", h3("ICT Indicator Level"), choices = ictLevelOptions, selected = 1),
            helpText("Only applies to Computer, Internet, and Mobile Phone Usage Percentage"),
            selectInput("continent", h3("Continent"), choices = continents, selected = 1),
            helpText("Only applies to horizontal barplot"),
            width = 3
        ),
        mainPanel(
            fluidRow(
                column(7, plotOutput("Scatterplot")),
                column(5, plotOutput("HorizontalBarplot"))
            ),
            fluidRow(
                column(12, plotOutput("Choropleth"))
            ),
            width = 9
        )
    )
)

# Server logic for app
server <- function(input, output) {
    
    # Defining the scatterplot
    output$Scatterplot <- renderPlot({
        # Get x and y based on inputs
        xvar <- buildXVar(input$xVar, input$schoolLevel, input$gender, input$ictLevel)
        yvar <- input$yVar
        xlabel <- getLabel(input$xVar, xVariables)
        ylabel <- getLabel(input$yVar, yVariables)
        
        # Filter the data to get what we need
        scatterplotData <- masterData %>%
            select(Country, Continent, xvar, yvar) %>%
            rename(X=xvar, Y=yvar) %>%
            filter(!is.na(X) & !is.na(Y))
        
        # Plot the scatter plot
        ggplot(data=scatterplotData) +
            geom_point(mapping = aes(x=X, y=Y, color=Continent)) +
            scale_color_brewer(palette="Set1") +
            xlab(xlabel) +
            ylab(ylabel)
    })
    
    # Defining the choropleth map
    output$Choropleth <- renderPlot({
        # Get x based on inputs
        xvar <- buildXVar(input$xVar, input$schoolLevel, input$gender, input$ictLevel)
        xlabel <- getLabel(input$xVar, xVariables)
        
        # Build a temporary dataset which will help map the country codes in master data to the 'region' in the map data
        tempdata <- country.map %>%
            select(iso_n3, region) %>%
            distinct(iso_n3, .keep_all = TRUE) %>%
            mutate(Country.Code=gsub("(?<![0-9])0+", "", iso_n3, perl = TRUE)) %>%
            mutate(Country.Code=as.integer(Country.Code))
        
        # Grab the data we want to plot on the map and merge it with the temp data to get region
        choroplethData <- masterData %>%
            select(Country.Code, xvar) %>%
            merge(tempdata, by="Country.Code", all.y=TRUE) %>%
            rename(value=xvar) %>%
            distinct(region, .keep_all = TRUE)
        
        # Plot the map (function returns a ggplot2 map)
        country_choropleth(choroplethData) +
            scale_fill_brewer(palette="YlGnBu") +
            labs(title = paste(xlabel, "by Country"), fill = xlabel)
    })
    
    # Defining the horizontal barplot
    output$HorizontalBarplot <- renderPlot({ 
        # Get y based on inputs
        yvar <- buildXVar(input$xVar, input$schoolLevel, input$gender, input$ictLevel)
        ylabel <- getLabel(input$xVar, xVariables)
        cont <- input$continent
        
        # Color palette will be based on the continent
        color <- switch(cont, "Africa" = "red", "Americas" = "blue", "Asia" = "green", "Europe" = "purple", "Oceania" = "orange")
        
        # Filter master data to get what we need
        barplotData <- masterData %>%
            select(Country, Continent, yvar) %>%
            rename(YVal=yvar) %>%
            filter(Continent==cont & !is.na(YVal))
        
        # Plot the bar graph
        ggplot(data=barplotData) +
            geom_bar(mapping = aes(x=reorder(Country, YVal), y=YVal, fill=YVal), stat='identity', show.legend=FALSE) +
            coord_flip() +
            scale_fill_gradient(low = "gray", high = color) +
            ylab(ylabel) +
            xlab(NULL)
    })
}

# Display the shiny app
shinyApp(ui = ui, server = server)




