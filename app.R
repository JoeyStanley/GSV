library(shiny)
library(shinyBS)
library(ggplot2)
library(readr)
library(dplyr) # rename(), summarise(), select(), group_by(), transform()
library(stringr) # str_match()
library(forcats) # fct_rev()
library(maps)
library(ineq) # Gini()

library(DT)

# Custom function (http://stackoverflow.com/questions/11302985/match-with-negation)
'%ni%' <- Negate('%in%')

this_is_live <- 1

title <- "Gazetteer of Southern Vowels (GSV)"
version <- '1.6.2'
date  <- "March 2, 2020"

# Read in the data, with different paths depending on if it's live or not
if (this_is_live) {
  path_to_NSF              <- "data/DASS_darla_and_fave.csv"
  path_to_metadata         <- "data/metadata.csv"
  path_to_stopwords        <- "data/stopwords.txt"
  path_to_css              <- "www/dassCSS.css"
  path_to_google_analytics <- "google-analytics.js"
} else {
  path_to_NSF              <- "C:\\Users\\Atlas\\Google Drive\\NSF\\ShinyApp\\data\\DASS_darla_and_fave.csv"
  path_to_metadata         <- "C:\\Users\\Atlas\\Google Drive\\NSF\\ShinyApp\\data\\metadata.csv"
  path_to_stopwords        <- "C:\\Users\\Atlas\\Google Drive\\NSF\\ShinyApp\\data\\stopwords.txt"
  path_to_css              <- "C://Users//Atlas//Google Drive//NSF//ShinyApp//www//dassCSS.css"
  path_to_google_analytics <- "C://Users//Atlas//Google Drive//NSF//ShinyApp//google-analytics.js"
}

# Create the colors
colors_11 <- c("#F8766D", "#DB8E00", "#AEA200", "#64B200", "#00BD5C", "#00C1A7", "#00BADE", "#00A6FF", "#B385FF", "#EF67EB", "#FF63B6")
colors_19 <- c("#F8766D", "#DB8E00", "#AEA200", "#64B200", "#00BD5C", "#00C1A7", "#00BADE", "#00A6FF", "#B385FF", "#EF67EB", "#FF63B6", "#FF63B6",
               "#EF67EB", "#F8766D", "#AEA200", "#00A6FF",
               "#F8766D", "#AEA200", "#FF63B6", "#00A6FF", "#EF67EB")


# Create the vowels data frame with the correct order
vowels <- tibble(ARPABET = factor(          c("IY",     "IH",  "EY",   "EH",    "AE",   "AA",  "AO",      "OW",   "UH",   "UW",    "AH"),
                                            levels =  c("IY",     "IH",  "EY",   "EH",    "AE",   "AA",  "AO",      "OW",   "UH",   "UW",    "AH")),
                 IPA     = factor(          c("i",      "ɪ",   "e",    "ɛ",      "æ",    "ɑ",   "ɔ",       "o",    "ʊ",    "u",     "ʌ"),
                                            levels =  c("i",      "ɪ",   "e",    "ɛ",      "æ",    "ɑ",   "ɔ",       "o",    "ʊ",    "u",     "ʌ")),
                 SAMPA   = factor(          c("i",      "I",   "e",    "E",     "{",    "A",   "O",       "o",    "U",    "u",     "V"),
                                            levels =  c("i",      "I",   "e",    "E",     "{",    "A",   "O",       "o",    "U",    "u",     "V")),
                 Wells   = factor(          c("FLEECE", "KIT", "FACE", "DRESS", "TRAP", "LOT", "THOUGHT", "GOAT", "FOOT", "GOOSE", "STRUT"),
                                            levels =  c("FLEECE", "KIT", "FACE", "DRESS", "TRAP", "LOT", "THOUGHT", "GOAT", "FOOT", "GOOSE", "STRUT")))


# Function for taking two colors and making a n_based gradient scale
color_gradienter <- function(hi, lo, shades) {
  
  intervals = shades - 1
  
  # Separate colors into their RGB values.
  # Convert hexidecimal to integer.
  hi_r <- substr(hi, 2, 3) %>% strtoi(base = 16)
  hi_g <- substr(hi, 4, 5) %>% strtoi(base = 16)
  hi_b <- substr(hi, 6, 7) %>% strtoi(base = 16)
  
  lo_r <- substr(lo, 2, 3) %>% strtoi(base = 16)
  lo_g <- substr(lo, 4, 5) %>% strtoi(base = 16)
  lo_b <- substr(lo, 6, 7) %>% strtoi(base = 16)
  
  tibble(shade_num = 0:intervals) %>%
    mutate(r = ifelse(lo_r <= hi_r, lo_r, hi_r) + abs(hi_r - lo_r) / intervals * shade_num,
           g = ifelse(lo_g <= hi_g, lo_g, hi_g) + abs(hi_g - lo_g) / intervals * shade_num,
           b = ifelse(lo_b <= hi_b, lo_b, hi_b) + abs(hi_b - lo_b) / intervals * shade_num) %>%
    mutate_at(vars(r, g, b), round) %>%
    mutate_at(vars(r, g, b), as.hexmode) %>%
    mutate(rgb = paste0("#", r, g, b)) %>%
    pull(rgb)
}



# Read in and prepare data ------------------------------------------------

# Use the paths above to read in the data
NSF <- read_csv(path_to_NSF,
                
                # Order the factors correctly as we read in the file.
                col_types = cols(
                  state       = col_factor(levels=c("Alabama", "Arkansas", "Florida", "Georgia", "Louisiana", "Mississippi", "Tennessee", "Texas")),
                  ethnicity   = col_factor(levels=NULL),
                  sex         = col_factor(levels=c("M", "F")),
                  kurath_type = col_factor(levels=c("1", "2", "3", "AA")),
                  speaker = col_factor(levels=c("025","027","030","040","079","100","105","117","165","166","176","185","252", 
                                                "255","270","289","299","303","312","330","342","364","370B","387","412","434", 
                                                "444","446","456","461","464","472","490","494","503","505","533","543","548", 
                                                "556","579","595","596","604","625","647","657X","662","678","703","741","748", 
                                                "779","791","794","811","847","853","863","888","893","894","911")),
                  ARPABET = col_factor(levels=c("IY", "IH", "EY", "EH", "AE", "AA", "AO", "OW", "UH", "UW", "AH")),
                  Plotnik = col_factor(levels=c("iy", "i",  "ey", "e",  "ae", "o",  "oh", "ow", "u",  "uw", "uh", "@",
                                                "Tuw",
                                                "iyF", "eyF", "owF",
                                                "iyr", "eyr", "ahr", "owr", "uwr", 
                                                "*hr", "aw", "ay", "ay0", "oy")),
                  education_level = col_factor(levels=c("0-7 years", "8-10 years", "11-12 years", "13-16+ years")),
                  social_class = col_factor(levels=c("Upper", "Upper Middle", "Lower Middle", "Lower")),
                  classification = col_factor(levels=c("IA Folk", "IB Folk", "IB Common",
                                                       "IIA Folk", "IIA Common", "IIB Common",
                                                       "IIIA Cultured", "IIIB Cultured")),
                  manner = col_factor(levels = c("stop", "affricate", "fricative", "nasal", "lateral", "rhotic")),
                  place = col_factor(levels = c("labial", "labiodental", "interdental", "apical", "palatal", "velar")),
                  voice = col_factor(levels = c("voiceless", "voiced")),
                  corpus = col_factor(levels = c("DARLA", "FAVE"))
                )) %>% 
  
  # Merge with the vowels data to get the other transcription systems
  left_join(vowels, by = "ARPABET") %>%
  mutate(word = tolower(word)) %>%
  # Just keep the columns I want and reorder them
  dplyr::select(speaker, state, sex, ethnicity, birth_year,
         # education_level, social_class, classification, kurath_type,
         # corpus, word, stress, filter_stdev, filter_joey, filter_mahal,
         # manner, place, voice, pre_seg, fol_seg,
         # F1.50., F2.50., F1.50._lob, F2.50._lob,
         # Bark_height, Bark_backness,
         ARPABET, IPA, SAMPA, Plotnik, Wells) %>%
  print()

# Arbitrary limits
max_F1 <- 1100
min_F1 <- 150
max_F2 <- 2900
min_F2 <- 500
max_F1_lob <-  3.5
min_F1_lob <- -3.5
max_F2_lob <-  3.5
min_F2_lob <- -3.5
max_F1_bark <- 14
min_F1_bark <-  4
max_F2_bark <-  8
min_F2_bark <-  0.25

# For the interactive plot, show all the columns
tibble.width = Inf

# Read in the maps data
metadata <- read_csv(path_to_metadata, 
                     col_types = cols()) %>%
  # Add this in to help with legends in the maps
  mutate(dummy = "dummy")

# Prepare the shapes for the map image
dass_states <- subset(map_data("state"), region %in% c("tennessee", "georgia", "florida", "alabama", "mississippi", "louisiana", "arkansas", "texas"))

# might as well create the bare map now
map <- ggplot(data=dass_states) +
  geom_polygon(aes(x=long, y=lat, group=group), fill="white", color="black") +
  coord_fixed(1.2) +
  guides(fill=F) +
  theme_void()


# Get the stopwords
stopwords <- read_delim(path_to_stopwords, delim = "\t", col_names = "word") %>%
  pull(word) %>%
  sort()

# Get a list of all words in the corpus.
allwords <- NSF %>%
  select(word) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  pull(word) %>%
  sort()









# Function for left and right columns -------------------------------------


# Reuse this code multiple times so it's consistent across pages
selection_tab_panel <- function(side) { # options: left, right, traj, int, grid
  tabsetPanel(
    
    # ____ Speakers tab ---------------------------------------------------------
    
    tabPanel(h3(class = "tab", "Speakers"),
             fluidRow(
               column(6,
                      fluidRow(
                        column(6,
                               selectInput(paste0("state_", side),
                                           label = h4("State"),
                                           choices = levels(NSF$state),
                                           selected = "Georgia",
                                           multiple = T,
                                           selectize = F,
                                           size=8,
                                           width="100%"
                               )
                        ),
                        column(6,
                               selectInput(paste0("ethnicity_", side),
                                           label=h4("Ethnicity"),
                                           choices = levels(NSF$ethnicity),
                                           selected = sample(levels(NSF$ethnicity), 1),
                                           multiple = T,
                                           selectize = F,
                                           size=2,
                                           width="100%"
                               ),
                               selectInput(paste0("sex_", side),
                                           label=h4("Sex"),
                                           choices = c("M", "F"),
                                           selected = sample(c("M", "F"), 1),
                                           multiple = T,
                                           selectize = F,
                                           size=2,
                                           width="100%"
                               )
                        )
                      ),
                      sliderInput(paste0("yob_", side),
                                  sep="", # sep arguement removes thousands separating comma
                                  label = h4("Birth Year"),
                                  min = 1886,
                                  max = 1965,
                                  value = c(1886, 1965),
                                  width="100%"
                      )
               ),
               column(6,
                      fluidRow(
                        column(6,
                               selectInput(paste0("education_", side),
                                           label = h4("Education"),
                                           choices = levels(NSF$education_level),
                                           selected = levels(NSF$education_level),
                                           multiple=T,
                                           selectize = F,
                                           width="100%"
                               ),
                               selectInput(paste0("social_class_", side),
                                           label = h4("Social Class"),
                                           choices = levels(NSF$social_class),
                                           selected = levels(NSF$social_class),
                                           multiple=T,
                                           selectize = F,
                                           width="100%"
                               )
                        ),
                        column(6,
                               selectInput(paste0("classification_", side),
                                           label = h4("Classification"),
                                           choices = levels(NSF$classification),
                                           selected = levels(NSF$classification),
                                           multiple=T,
                                           selectize = F,
                                           width="100%"
                               ),
                               selectInput(paste0("kurath_type_", side),
                                           label = h4("Kurath Type"),
                                           choices = levels(NSF$kurath_type),
                                           selected = levels(NSF$kurath_type),
                                           multiple=T,
                                           selectize = F,
                                           width="100%"
                               )
                        )
                      )
               )
             )
    ),
    
    # ____ Word selection tab ---------------------------------------------------------
    
    tabPanel(h3(class = "tab", "Words"),
             fluidRow(
               column(3,
                      radioButtons(paste0("include_exclude_", side),
                                   label = h3(""),
                                   choices = c("Exclude these words"   = "hide",
                                               "Only show these words" = "show"),
                                   width = "100%"),
                      actionButton(paste0("stopwords_btn_", side),
                                   label = "Default stop words",
                                   width = "100%"),
                      actionButton(paste0("clear_words_btn_", side),
                                   label="Clear list",
                                   width = "100%")
               ),
               column(9,
                      fluidRow(
                        column(12,
                               selectInput(paste0("wordlist_", side),
                                           label = h3(""),
                                           choices = allwords,
                                           selected = stopwords,
                                           multiple  = TRUE,
                                           selectize = TRUE,
                                           width = "100%"
                               )
                        )
                      )
               )
             )
    ),
    
    # ____ Vowel options tab ---------------------------------------------------------
    
    tabPanel(h3(class = "tab", "Vowels"),
             fluidRow(
               column(3,
                      selectInput(paste0("vowel_", side),
                                  label = h4("Vowel"),
                                  choices = levels(vowels$Wells),
                                  selected = ifelse(side=="grid", "GOAT", levels(vowels$Wells)),
                                  multiple=T,
                                  selectize = F,
                                  size = 11,
                                  width="100%"
                      )
               ),
               column(6,
                      h3("Following consonant"),
                      fluidRow(
                        column(6,
                               selectInput(paste0("place_", side),
                                           label= h4("Place of Articulation"),
                                           choices = levels(NSF$place),
                                           selected = levels(NSF$place),
                                           width="100%",
                                           multiple=T,
                                           selectize = F,
                                           size=6
                               ),
                               selectInput(paste0("voice_", side),
                                           label= h4("Voicing"),
                                           choices = levels(NSF$voice),
                                           selected = levels(NSF$voice),
                                           width="100%",
                                           multiple=T,
                                           selectize = F,
                                           size=2
                               )
                               
                        ),
                        column(6,
                               selectInput(paste0("manner_", side),
                                           label= h4("Manner of Articulation"),
                                           choices = levels(NSF$manner),
                                           selected = levels(NSF$manner),
                                           width="100%",
                                           multiple=T,
                                           selectize = F,
                                           size=6
                               ),
                               selectInput(paste0("filter_", side),
                                           label= h4("Filtering Technique"),
                                           choices = c("No filter", "z-score", "Mahalanobis distance", "Joey's method"),
                                           selected = "z-score",
                                           width="100%")
                        )
                      )
               ),
               column(3,
                      selectInput(paste0("stress_", side),
                                  label= h4("Stress"),
                                  choices = c("Primary", "Secondary", "No Stress"),
                                  selected = "Primary",
                                  multiple=T,
                                  selectize = F,
                                  size=3,
                                  width="100%"
                      ),
                      selectInput(paste0("norm_", side),
                                  label= h4("Normalization"),
                                  choices = c("None", "Lobanov", "Bark Difference Metric"),
                                  width="100%"
                      ),
                      selectInput(paste0("trans_", side),
                                  label = h4("Transcription System"),
                                  choices = c("ARPABET", "SAMPA", "Plotnik", "Wells' Lexical Sets"), # IPA
                                  selected = "Wells' Lexical Sets",
                                  width="100%"
                                  
                      )
               )
             )
    ),
    
    # ____ Corpus tab ----------------------------------------------
    
    tabPanel(h3(class= "tab", "Corpus"),
             
             helpText("We have processed the DASS two different ways. Both methods used identical audio files as input, which were transcribed manually and double- and triple-checked for accuracy."),
             hr(),
             helpText(paste0("First, we processed the data using the DARLA pipeline. At the time, DARLA was using ProsodyLab for forced-alignment, and then doing formant extraction with FAVE-Extract. Note that DARLA automatically filters out some data, including (most) stopwords, tokens with a short duration, and those with high bandwidth. Because of this filtering, the DARLA corpus is substantially smaller (", formatC(nrow(NSF[NSF$corpus=="DARLA",]), big.mark=","), " tokens) and the plots will often appear more open because there are fewer points plotted.")),
             hr(),
             helpText(paste0("The other option is to use an in-house pipeline. The audio and transcriptions were force-aligned using the Montreal Forced Aligner and then processed with FAVE-Extract. We did not do any filtering whatsoever, so there is data from every single vowel token (", formatC(nrow(NSF[NSF$corpus=="FAVE",]), big.mark=","), " tokens).")),
             
             radioButtons(paste0("corpus_", side),
                          label = h4("Which corpus would you like to view?"),
                          inline = TRUE,
                          choiceNames = list("ProsodyLab + FAVE via DARLA",
                                             "MFA + FAVE in-house"),
                          choiceValues = list("DARLA", "FAVE")
             )
             
    ),
    
    
    # ____ Plot tab ---------------------------------------------------------
    
    tabPanel(h3(class = "tab", "Plot"),
             fluidRow(
               column(3,
                      checkboxInput(inputId = paste0("points_", side),
                                    label   = h3("Points"),
                                    value   = ifelse(side=="int", FALSE, TRUE)),
                      sliderInput(inputId = paste0("pointsAlpha_", side),
                                  label = "Opacity",
                                  min = 0,
                                  max = 1,
                                  value = 0.5,
                                  width="100%"),
                      sliderInput(inputId = paste0("pointsSize_", side),
                                  label = "Size",
                                  min = 0.01,
                                  max = 10,
                                  value = 1,
                                  width="100%")
               ),
               column(3,
                      checkboxInput(inputId = paste0("ellipses_", side),
                                    label   = h3("Ellipses"),
                                    value   = T),
                      sliderInput(inputId = paste0("ellipsesAlpha_", side),
                                  label = "Opacity",
                                  min = 0,
                                  max = 1,
                                  value = 1,
                                  width="100%"),
                      sliderInput(inputId = paste0("ellipsesSize_", side),
                                  label = "Size",
                                  min = 1,
                                  max = 100,
                                  value = 67,
                                  post = "%",
                                  width="100%")
               ),
               column(3,
                      checkboxInput(inputId = paste0("means_", side),
                                    label   = h3("Means"),
                                    value   = F),
                      sliderInput(inputId = paste0("meansAlpha_", side),
                                  label = "Opacity",
                                  min = 0,
                                  max = 1,
                                  value = 1,
                                  width="100%"),
                      sliderInput(inputId = paste0("meansSize_", side),
                                  label = "Size",
                                  min = 2,
                                  max = 20,
                                  value = 10,
                                  width="100%")
               ),
               column(3,
                      checkboxInput(inputId = paste0("words_", side),
                                    label   = h3("Words"),
                                    value   = ifelse(side=="int", TRUE, FALSE)),
                      sliderInput(inputId = paste0("wordsAlpha_", side),
                                  label = "Opacity",
                                  min = 0,
                                  max = 1,
                                  value = 1,
                                  width="100%"),
                      sliderInput(inputId = paste0("wordsSize_", side),
                                  label = "Size",
                                  min = 0.01,
                                  max = 10,
                                  value = 3,
                                  width="100%")
               )
             )
    ),
    
    # ____ Customization ----------------------------------------
    
    tabPanel(h3(class = "tab", "Customization"),
             
             
             fluidRow(
               column(4,
                      selectInput(paste0("colors_", side),
                                  label = h4("Colors"),
                                  choices = c("Black", "Multicolored (variable)", "Multicolored (persistent)"),
                                  selected = ifelse(side=="grid", "Black", "Multicolored (persistent)")
                      ),
                      numericInput(inputId = paste0("ratio_", side),
                                   label   = "Aspect Ratio",
                                   value   = 2,
                                   step = 0.1)
               ),
               # This arranges the zoom functions into a grid.
               column(8,
                      sliderInput(paste0("F1_range_", side),
                                  sep=",", # sep arguement removes thousands separating comma
                                  label = h4("F1"),
                                  min = round(min(NSF$F1.50.),0),
                                  max = round(max(NSF$F1.50.),0),
                                  value = c(min_F1, max_F1),
                                  width="100%"
                      ),
                      sliderInput(paste0("F2_range_", side),
                                  sep=",", # sep arguement removes thousands separating comma
                                  label = h4("F2"),
                                  min = round(min(NSF$F2.50.),0),
                                  max = round(max(NSF$F2.50.),0),
                                  value = c(min_F2, max_F2),
                                  width="100%"
                      ),
                      fluidRow(
                        # column(4,
                        #        numericInput(paste0("min_F1_", side), label = h4("Min F1"), value = round(min_F1, 0), step=25)
                        # ),
                        column(6,
                               actionButton(inputId = paste0("zoom_out_max_", side),
                                            label = "Fit vowel space",
                                            width = '100%')
                        ),
                        column(6,
                               actionButton(inputId = paste0("zoom_in_max_", side),
                                            label = "Fit displayed data",
                                            width = '100%')
                        )
                      )
                      # fluidRow(
                      #   column(4,
                      #          numericInput(paste0("max_F1_", side), label = h4("Max F1"), value = round(max_F1, 0), step=25)),
                      #   column(4,
                      #          numericInput(paste0("max_F2_", side), label = h4("Max F2"), value = round(max_F2, 0), step=25)),
                      #   column(4,
                      #          numericInput(paste0("min_F2_", side), label = h4("Min F2"), value = round(min_F2, 0), step=25))
                      # )
               )
             ),
             
             
             
             if (side == "grid") {
               fluidRow(
                 hr(),
                 h3("Grid options"),
                 column(4,
                        numericInput("grid_rows",
                                     h4('Rows'),
                                     12,
                                     min = 1, max = 100
                        ),
                        numericInput("grid_cols",
                                     h4('Cols'),
                                     12,
                                     min = 1, max = 100
                        )
                 ),
                 
                 column(4,
                        checkboxInput(inputId = "grid_cell_labels",
                                      label   = "Show cell labels",
                                      value   = T),
                        sliderInput(inputId = "grid_label_alpha",
                                    label = "Opacity",
                                    min = 0,
                                    max = 1,
                                    value = 1,
                                    width="100%"),
                        sliderInput(inputId = "grid_label_size",
                                    label = "Size",
                                    min = 1,
                                    max = 15,
                                    value = 8,
                                    width="100%")
                 ),
                 column(4,
                        selectInput("shading_type",
                                    label = h4("Shading Type"),
                                    choices = c("Discrete", "Continuous", "None (white)"),
                                    selected = "Discrete"),
                        numericInput("grid_shades",
                                     h4('Number of shades'),
                                     4,
                                     min = 2, max = 25),
                        textInput("shade_color",
                                  label = h4("Darkest Shade (RGB)"),
                                  value = "#144387")
                 )
               )
               
             } # endif
    ),
    
    # ____ Download tab -----------------------------------------------------
    
    
    tabPanel(h3(class = "tab", "Download"),
             fluidRow(
               column(4,
                      numericInput(paste0("height_", side),
                                   label = h4("Height (inches)"),
                                   value = 7,
                                   step = 0.1),
                      numericInput(paste0("width_", side),
                                   label = h4("Width (inches)"),
                                   value = 7,
                                   step = 0.1),
                      numericInput(paste0("dpi_", side),
                                   label = h4("DPI"),
                                   value = 300,
                                   step = 50)
               ),
               column(4,
                      textInput(paste0("filename_", side),
                                label = h4("File name"),
                                value = paste0("GSV_plot_", side)),
                      selectInput(paste0("filetype_", side),
                                  label = h4("File type"),
                                  choices = c("JPEG", "PNG", "PDF"),
                                  selected = "JPEG",
                                  multiple = FALSE,
                                  selectize = F,
                                  size = 3,
                                  width = "100%"
                      )
               ),
               
               column(4,
                      downloadButton(paste0("download_", side), "Download", width = "100%")
               )
             )
    ),
    
    
    # ____ Hide tab ---------------------------------------------------------
    
    tabPanel(h3(class = "tab", "Hide"))
    
  ) # end tabsetPanel
}


# UI ----------------------------------------------------------------------

# navbarPage instead of fluidPage (https://shiny.rstudio.com/articles/layout-guide.html)
ui <- fluidPage(
  
  # Google Analytics
  # https://shiny.rstudio.com/articles/google-analytics.html
  tags$head(includeScript(path_to_google_analytics)),
  
  # CSS file
  includeCSS(path_to_css),
  
  # Help button is disabled for now until I can find a way to hide it on the iPad.
  # Hide help button is displayed by default. This changes when clicked.
  actionButton("show_help_button",
               label="Toggle Help",
               icon = icon("question-circle-o"),
               class="toggle-help"),
  # This gets content when the button is clicked.
  uiOutput("hide_help_button"),
  
  # The popovers for the help buttons
  bsPopover(id = "show_help_button", placement = "left", trigger = "hover",
            title = "Show Help",
            options = list(container = "body"),
            content = "Click this button to see more help bubbles like this throughout the site."),
  
  navbarPage(ifelse(this_is_live, "GSV", "GSV (Offline)"),
             inverse=TRUE, # inverts the colors so it's gray on black
             collapsible=TRUE, # for small screens (<960px)
             
             
             
             # ____ About Page -----------------------------------------------------------
             
             tabPanel("About",
                      
                      p(tags$i('Please wait a few moments for the site to load. If the site appears to be frozen, it\'s because the GSV is still loading and processing a bunch of data.')),
                      hr(),
                      
                      h1('Gazetteer of Southern Vowels'),
                      p('This site was created to allow you to interact with data extracted from the Digital Archive of Southern Speech. Please report bugs, suggestions, or comments to Joey Stanley at joeystan@uga.edu.'),
                      
                      # actionButton("load_data_button",
                      #              label="Load Data",
                      #              icon = icon("table"),
                      #              class = "load_data_button"),
                      
                      h2('Where do the data come from?'),
                      p('The Digital Archive of the Southern Speech (DASS) is an audio corpus of semi-spontaneous linguistic atlas interviews (Kretzschmar', tags$i('et al.'), '2013) derived from the Linguistic Atlas of the Gulf States (Pederson', tags$i('et al.'), '1986). It contains speech from 64 natives (34 men and 30 women, born 1886–1965) of 8 Southern US states. This sample contains a mixture of ethnicities, social classes, education levels, and ages.'),
                      p('As of October 2019, transcription, forced alignment, and acoustic analysis of DASS has been completed. For insight into the methods, see Renwick', tags$i('et al.'), '(2017) and Olsen', tags$i('et al.'), '(2017). We use the ', tags$a(href = 'https://montreal-forced-aligner.readthedocs.io/en/latest/', "Montreal Forced Aligner", target='_blank'), ' for forced alignment and FAVE for formant extraction. We have removed all filters from FAVE so that ', tags$i('all'), ' vowel tokens, whether they be from unstressed syllables or stopwords, are included here. Currently, this site displays ', formatC(nrow(NSF[NSF$corpus=="FAVE",]), big.mark=","), " vowel tokens from ", length(unique(NSF$speaker)), " speakers."),
                      p('You may download the audio, transcriptions, TextGrids, speaker bios, and other information at ', tags$a(href = 'http://www.lap.uga.edu/Projects/DASS2019/', "DASS portion", target='_blank'), ' of the Linguistic Atlas Project website (', tags$a(href = 'http://www.lap.uga.edu', "lap.uga.edu", target='_blank'), ').'),
                      p('The corpus can be licensed from the', tags$a(href='https://catalog.ldc.upenn.edu/LDC2012S03', 'Linguistic Data Consortium,',  target='_blank'), 'while the', tags$a(href='http://www.lap.uga.edu/Projects/DASS/', 'Linguistic Atlas Project hosts it', target='_blank'), 'via mp3s, speaker biographies, and more.'),
                      
                      h2('What does this site do?'),
                      p("Currently, the site has four main pages:"),
                      tags$ul(
                        tags$li(class="about-li", tags$i('Vowel Plot Comparison:'), ' On this page, you can subset the DASS data by many demographic attributes and view the corresponding speakers\' vowel tokens plotted in F1, F2 space. You can also subset by stress, vowel, word, and following consonant and choose what normalization technique (if any), filtering, and transcription system should be used. The plots are extremely customizable and you can change how the data is displayed. Two graphs are included on this page to—given a large enough screen size—facilitate side-by-side comparison of subgroups. Below each graph are tables that give basic summaries of the speakers and the vowels selected.'),
                        tags$li(class="about-li", tags$i('Interactive Vowel Plot:'), ' Here you can focus on specific portions of individual speakers\' vowel space and see words rather than just points. If you click on the plot itself, a table at the top will display the five points nearest to where you clicked, showing you exact formant measurements, the word, and the speaker associated with that observation.'),
                        tags$li(class="about-li", tags$i('Point Pattern Analysis:'), ' This is an alternative way of viewing the vowel space, pioneered by Kretzschmar. On this page, you can again subset the data the same as on the other two pages and see a scatterplot in F1, F2 space. The underlaid grid indicates how many observations lie in each cell, with the number of rows and columns in the grid controllable by the user. Below the plot is a chart of the distribution of the grids, plotted in decreasing order of density. The resulting chart follows an Asymptotic Curve (or simply, "A-Curve").'),
                        tags$li(class="about-li", tags$i('Speaker Info:'), ' The speaker info page allows you to explore the metadata and distribution of speakers in DASS. The map has some flexibility as to how various demographic categories are displayed.')
                      ),
                      p('New content is being added regularly, so check back for additional features. See the bottom of this page for updates on recent changes.'),
                      
                      h2('How is this site powered?'),
                      p('This site is built in', tags$i(tags$a(href='https://shiny.rstudio.com', 'Shiny,', target='_blank')), 'a web application framework for R. With Shiny, users can utilize the computational power of the R programming language without having to learn R or install it to their computers. This is all bundled up and put on the web to allow for the interactive capabilities of web browsers. See the bottom of this page for a list of specific packages that are used to process this data and create this site.'),
                      
                      h2('How is this project funded?'),
                      p('This research is supported by:', tags$a(href='https://www.nsf.gov/awardsearch/showAward?AWD_ID=1625680', 'NSF BCS #1625680', target='_blank'), 'to co-PIs Kretzschmar and Renwick, the', tags$a(href='http://grad.uga.edu/', 'University of Georgia Graduate School,', target='_blank'), 'and the', tags$a(href='http://www.americandialect.org/', 'American Dialect Society.', target='_blank')),
                      
                      h2('Who is involved?'),
                      p('The PIs for this project are William Kretzschmar and', tags$a(href='https://faculty.franklin.uga.edu/mrenwick/', 'Margaret E. L. Renwick,', target='_blank'), 'of the University of Georgia. Our team has several graduate student researchers including ', tags$a(href='https://linguistics.uga.edu/directory/people/michael-olsen', 'Mike Olsen,', target='_blank'), tags$a(href='https://linguistics.uga.edu/directory/people/rachel-miller-olsen', 'Rachel Olsen,', target='_blank'), tags$a(href='https://linguistics.uga.edu/directory/people/lisa-lipani', 'Lisa Lipani,', target='_blank'), 'Jeremy Shi, and', tags$a(href='http://www.joeystanley.com', 'Joey Stanley', target='_blank'), ' with assistance from ', tags$a(href='https://linguistics.uga.edu/directory/people/joshua-mcneill', 'Josh McNeill', target='_blank'), ' and ', tags$a(href='https://linguistics.uga.edu/directory/people/keiko-bridwell', 'Keiko Bridwell', target='_blank'), '. We also had several dozen undergraduate student workers, funded by ADS or NSF, who did most of the transcribing work.'),
                      
                      h2('Contact information'),
                      p('For more information, please contact Joey Stanley at joeystan@uga.edu.'),
                      
                      h2('How can I cite this resource?'),
                      p('If you use or refer to this website, you must cite the', tags$i('Gazetteer of Southern Vowels'), 'as follows:'),
                      tags$ul(tags$li(class="ref",
                                      'Stanley, Joseph A., Margaret E. L. Renwick, William A. Kretzschmar Jr., Rachel M. Olsen, & Michael Olsen.',
                                      '(2018).',
                                      '“The Gazetteer of Southern Vowels.”',
                                      'The American Dialect Society Annual Meeting. Salt Lake City, UT.')),
                      # tags$ul(tags$li(class="about-li",
                      #                 'Joseph A. Stanley, William A. Kretzschmar Jr., Margaret E. L. Renwick, Michael L. Olsen, and Rachel M. Olsen. ',
                      #                 '(2017)',
                      #                 tags$i('Gazetteer of Southern Vowels.'),
                      #                 'Linguistic Atlas Project, University of Georgia. http://lap3.libs.uga.edu/u/jstanley/vowelcharts/')),
                      
                      h2("Bibliography"),
                      p("The following is an ongoing list of research that is directly related to DASS or utilizes its data."),
                      h3("Publications (alphabetical)", class="about"),
                      tags$ul(
                        tags$li(class="ref",
                                'Kretzschmar, William A., Paulina Bounds, Jacqueline Hettel, Lee Pederson, Ilkka Juuso, Lisa Lena Opas-Hänninen, and Tapio Seppänen',
                                '(2013).',
                                '"The Digital Archive of Southern Speech (DASS)."',
                                tags$i('Southern Journal of Linguistics,'), tags$b('27'), '(2). 17–38.'),
                        tags$li(class="ref", '
                                Olsen, Rachel M., Michael L. Olsen, & Margaret E. L. Renwick',
                                '(2018).',
                                '"The impact of sub-region on /aɪ/ weakening in the U.S. South."',
                                tags$i('Proceedings of Meetings on Acoustics'),
                                tags$b("31,"), '060005; doi:', tags$a(href='https://doi.org/10.1121/2.0000879', 'https://doi.org/10.1121/2.0000879.', target='_blank')),
                        tags$li(class="ref", '
                                Olsen, Rachel M., Michael Olsen, Joseph A. Stanley, Margaret E. L. Renwick, & William A. Kreztschmar, Jr.',
                                '(2017).',
                                '"Methods for transcription and forced alignment of a legacy speech corpus."',
                                tags$i('Proceedings of Meetings on Acoustics'),
                                tags$b("30,"), '060001; doi:', tags$a(href='http://dx.doi.org/10.1121/2.0000559', 'http://dx.doi.org/10.1121/2.0000559.', target='_blank')),
                        tags$li(class="ref",
                                'Pederson, L., McDaniel, S. L., and Adams, C. M. (Eds.)',
                                '(1986).',
                                tags$i('Linguistic Atlas of the Gulf States,'),
                                'University of Georgia Press, Athens, Georgia, Vols. 1–7.'),
                        tags$li(class="ref",
                                'Renwick, Margaret E. L. & Rachel M. Olsen',
                                '(2017).',
                                '"Analyzing dialect variation in historical speech corpora."',
                                tags$i('The Journal of the Acoustical Society of America'),
                                tags$b('142,'), '406; doi:', tags$a(href='https://doi.org/10.1121/1.4991009', 'https://doi.org/10.1121/1.4991009.', target='_blank')),
                        tags$li(class="ref",
                                'Renwick, Margaret E. L. & Joseph A. Stanley',
                                '(2017).',
                                '“Static and dynamic approaches to vowel shifting in the Digital Archive of Southern Speech.”',
                                tags$i('Proceedings of Meetings on Acoustics'),
                                tags$b("30,"), '060003; doi:', tags$a(href='http://dx.doi.org/10.1121/2.0000582', 'http://dx.doi.org/10.1121/2.0000582.', target='_blank'))
                      ),
                      h3("Conference Presentations (chronological)", class="about"),
                      tags$ul(
                        
                        # LSA 2019 (New Orleans)
                        tags$li(class="ref",
                                'Stanley, Joseph A. & Margaret E. L. Renwick',
                                '(2020).',
                                '"Back vowel distinctions and dynamics in Southern US English."',
                                '94th Annual Meeting of the Linguistic Society of America. New Orleans, LA.'),
                        
                        # ADS 2019 (New Orleans)
                        tags$li(class="ref",
                                'Kretzschmar, William A., Margaret E. L. Renwick, Joseph A. Stanley, Katie Kuiper, Lisa Lipani, Michael Olsen, & Rachel Olsen',
                                '(2020).',
                                '"The View of Southern Vowels from Large-Scale Data."',
                                'The American Dialect Society Annual Meeting. New York City, NY.'),
                        tags$li(class="ref",
                                'Olsen, Rachel Miller',
                                '(2020).',
                                '"Social identity is a pitch: Expressing who you are through prosody."',
                                'The American Dialect Society Annual Meeting. New York City, NY.'),
                        tags$li(class="ref",
                                'Jones, Jonathan & Margaret E. L. Renwick',
                                '(2020).',
                                '"Heterogeneity in Southern speech: Evidence from the Mississippi Delta."',
                                'The American Dialect Society Annual Meeting. New York City, NY.'),
                        
                        # LCUGA6 (Athens)
                        tags$li(class="ref",
                                'Bigott, Bailey & Margaret E. L. Renwick',
                                '(2019).',
                                '"Diving into DASS: A multimedia exploration of Southern Speech."',
                                '6th Annual Linguistics Conference at UGA. Athens, GA.'),
                        tags$li(class="ref",
                                'Stanley, Joseph A.',
                                '(2019).',
                                '"Real Time Vowel Shifts in Georgia English."',
                                '6th Annual Linguistics Conference at UGA. Athens, GA.'),
                        tags$li(class="ref",
                                'Jones, Jonathan & Margaret E. L. Renwick',
                                '(2019).',
                                '"Detecting Southern vowel features with GIS mapping."',
                                '6th Annual Linguistics Conference at UGA. Athens, GA.'),
                        
                        
                        # DJ2019 (Utrecht)
                        tags$li(class="ref",
                                'Kretzschmar Jr., William A. and Joseph A. Stanley',
                                '(2019).',
                                '"Visualization of Big Data phonetics"',
                                'Digital Humanities Conference 2019. Utrecht, the Netherlands.'),
                        
                        
                        # ASA (Louisville)
                        tags$li(class="ref",
                                'Lipani, Lisa, Yuanming Shi, Joshua McNeill, Margaret E. L. Renwick',
                                '(2019).',
                                '"Noise reduction in a legacy speech corpus."',
                                'Poster presentation at the 177th Meeting of the Acoustical Society of America (ASA). Louisville, KY.'),
                        
                        # LSA (New York)
                        tags$li(class="ref",
                                'Stanley, Joseph A. & Margaret E. L. Renwick',
                                '(2019).',
                                '"Social factors in Southern US speech: Acoustic analysis of a large-scale legacy corpus."',
                                '93rd Annual Meeting of the Linguistic Society of America. New York City, NY.'),
                        
                        # ADS (New York)
                        tags$li(class="ref",
                                'Olsen, Rachel, Joseph A. Stanley, Mike Olsen, Lisa Lipani, & Margaret E. L. Renwick.',
                                '(2019).',
                                '"Reconciling perception with production in Southern speech"',
                                'The American Dialect Society Annual Meeting. New York City, NY.'),
                        
                        # LCUGA5 (Athens)
                        tags$li(class="ref",
                                'Stanley, Joseph A. & Margaret E. L. Renwick',
                                '(2018).',
                                '"Finding pockets of social variation in the Digital Archive of Southern Speech."',
                                '5th Annual Linguistics Conference at UGA. Athens, GA.'),
                        
                        
                        # SECOL85 (Blackburg)
                        tags$li(class="ref",
                                'Olsen, Rachel M. & Margaret E. L. Renwick',
                                '(2018).',
                                '"The Impact of Social Factors on Vowel Duration in Natural Southern Speech."',
                                '85th Meeting of the SouthEastern Conference on Linguistics. Blacksburg, VA.'),
                        
                        # ADS (Salt Lake)
                        tags$li(class="ref",
                                'Stanley, Joseph A., Margaret E. L. Renwick, William A. Kretzschmar Jr., Rachel M. Olsen, & Michael Olsen',
                                '(2018).',
                                '“The Gazetteer of Southern Vowels.”',
                                'The American Dialect Society Annual Meeting. Salt Lake City, UT.'),
                        
                        
                        # ASA 174 (New Orleans)
                        tags$li(class="ref",
                                'Foster, Shawn, Joseph A. Stanley, & Margaret E. L. Renwick',
                                '(2017).',
                                '"Vowel Mergers in the American South."',
                                'Poster presentation at the 174th Meeting of the Acoustical Society of America (ASA). New Orleans, LA.'),
                        tags$li(class="ref",
                                'Olsen, Rachel, Michael Olsen, & Margaret E. L. Renwick.',
                                '(2017).',
                                '"Acoustically quantifying /ai/ monophthongization in four southern dialect regions."',
                                'Poster presentation at the 174th Meeting of the Acoustical Society of America (ASA). New Orleans, LA. Charleston, SC.'),
                        
                        
                        # NWAV46 (Madison)
                        tags$li(class="ref",
                                'Olsen, Rachel M. & Michael Olsen',
                                '(2017).',
                                '"Lexical frequency effects on the southern shift in the Digital Archive of Southern Speech."',
                                'New Ways of Analyzing Variation (NWAV) 46. Madison, WI.'),
                        tags$li(class="ref",
                                'Olsen, Rachel M. & Margaret E. L. Renwick',
                                '(2017).',
                                '"Linking acoustic correlates of rhoticity to pereption: How the past informs the present."',
                                'New Ways of Analyzing Variation (NWAV) 46. Madison, WI.'),
                        
                        # ASA 173 (Boston)
                        tags$li(class="ref",
                                'Renwick, Margaret E. L., Michael Olsen, Rachel M. Olsen, & Joseph A. Stanley',
                                '(2017).',
                                '"Transcription and forced alignment of the Digital Archive of Southern Speech."',
                                'Poster presentation at the 173rd Meeting of the Acoustical Society of America (ASA). Boston, MA.'),
                        tags$li(class="ref",
                                'Renwick, Margaret E. L. & Joseph A. Stanley',
                                '(2017).',
                                '“A historical perspective on vowel shifting: Acoustic analysis of the Digital Archive of Southern Speech.”',
                                'Poster presentation at the 173rd Meeting of the Acoustical Society of America (ASA). Boston, MA.'),
                        
                        # SECOL 84 (Charleston)
                        tags$li(class="ref",
                                'Kretzschmar, William A., Joseph A. Stanley, & Katherine Kuiper',
                                '(2017).',
                                '"Automated Large-Scale Phonetic Analysis: DASS."',
                                '84th Meeting of the SouthEastern Conference on Linguistics. Charleston, SC.'),
                        tags$li(class="ref",
                                'Olsen, Rachel M., Michael Olsen, Joseph A. Stanley, & Margaret E. L. Renwick',
                                '(2017).',
                                '"Transcribing the Digital Archive of Southern Speech: Methods and Preliminary Analysis."',
                                '84th Meeting of the SouthEastern Conference on Linguistics. Charleston, SC.'),
                        
                        # IRIS (Athens)
                        tags$li(class="ref",
                                'Olsen, Rachel M., Michael Olsen, Katherine Kuiper, Joseph A. Stanley, Margaret E. L. Renwick, & William A. Kretzschmar, Jr.',
                                '(2017).',
                                '“New Perspectives on Historical Southern Speech.”',
                                'Panel presented at the 2017 Integrative Research and Ideas Symposium. Athens, GA.')
                      ),
                      
                      
                      # Note that these must be based on the packages on the server, not this local computer.
                      h2("R Packages"),
                      tags$ul(
                        tags$li(class = "ref",
                                "Eric Bailey (2015).",
                                tags$a(href='https://CRAN.R-project.org/package=shinyBS', 'shinyBS:', target='_blank'),
                                "Twitter Bootstrap Components for Shiny. R package version 0.61."
                        ),
                        tags$li(class = "ref",
                                "Original S code by Richard A. Becker, Allan R. Wilks. R version by Ray Brownrigg. Enhancements by Thomas P Minka and Alex Deckmyn. (2016).",
                                tags$a(href='https://CRAN.R-project.org/package=maps', 'maps:', target='_blank'),
                                "Draw Geographical Maps. R package version 3.1.1."
                        ),
                        tags$li(class = "ref",
                                "Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2017).",
                                tags$a(href='https://CRAN.R-project.org/package=shiny', 'shiny:', target='_blank'),
                                "Web Application Framework for R. R package version 1.0.e.",
                                "See also",
                                tags$a(href='https://shiny.rstudio.com', 'shiny.rstudio.com', target='_blank')
                        ),
                        tags$li(class = "ref",
                                "Hadley Wickham (2009).",
                                tags$a(href='https://CRAN.R-project.org/package=ggplot2', 'ggplot2:', target='_blank'),
                                "Elegant Graphics for Data Analysis. Springer-Verlag New York. (R package version 2.2.1 is used here.)",
                                "See also",
                                tags$a(href='http://ggplot2.tidyverse.org/', 'ggplot2.tidyverse.org', target='_blank')
                        ),
                        tags$li(class = "ref",
                                "Hadley Wickham (2016).",
                                tags$a(href='https://CRAN.R-project.org/package=stringr', 'stringr:', target='_blank'),
                                "Simple, Consistent Wrappers for Common String Operations. R package version 1.2.0.",
                                "See also",
                                tags$a(href='http://stringr.tidyverse.org/', 'stringr.tidyverse.org', target='_blank')                        ),
                        tags$li(class = "ref",
                                "Hadley Wickham, Romain Francois, Lionel Henry and Kirill Müller (2017).",
                                tags$a(href='https://CRAN.R-project.org/package=dplyr', 'dplyr:', target='_blank'),
                                "A Grammar of Data Manipulation. R package version 0.7.4.",
                                "See also",
                                tags$a(href='http://dplyr.tidyverse.org/', 'dplyr.tidyverse.org', target='_blank')                        ),
                        tags$li(class = "ref",
                                "Hadley Wickham, Jim Hester and Romain Francois (2017).",
                                tags$a(href='https://CRAN.R-project.org/package=readr', 'readr:', target='_blank'),
                                "Read Rectangular Text Data. R package version 1.1.1.",
                                "See also",
                                tags$a(href='http://readr.tidyverse.org/', 'readr.tidyverse.org', target='_blank')                        ),
                        tags$li(class = "ref",
                                "Yihui Xie (2016).",
                                tags$a(href='https://CRAN.R-project.org/package=DT', 'DT:', target='_blank'),
                                "A Wrapper of the JavaScript Library 'DataTables'. R package version 0.2."
                        ),
                        tags$li(class = "ref",
                                "Achim Zeileis (2014).",
                                tags$a(href='https://CRAN.R-project.org/package=ineq', 'ineq:', target='_blank'),
                                "Measuring Inequality, Concentration, and Poverty. R package version 0.2-13."
                        )
                      ),
                      
                      h2("Change Log"),
                      
                      h3(class = "changes", 'Version 1.6.1 (February 14, 2020)'),
                      tags$ul(class = "changes", 
                              tags$li('Due to a clerical error, we thought we had the complete dataset. We were wrong and only about 80% of the data was in the GSV. Now the data should be complete. With the FAVE corpus, it went from about 1.3 million to 1.6 million tokens. An even larger corpus for you to work with!')
                      ),
                      
                      h3(class = "changes", 'Version 1.6 (November 26, 2019)'),
                      tags$ul(class = "changes", 
                              tags$li('DASS has now been fully processed, so the GSV now contains the full dataset. Some minor textual changes have been made to this page to reflect the completed work, including a link to the DASS portion of the Linguistic Atlas website where you can download audio and transcriptions.'),
                              tags$li('In fact, this site now contains two versions of the data, FAVE and DARLA. You can read more about them, and toggle between them using the new "corpus" tab.'),
                              tags$li('Added several new references: LCGAU6, LSA2020, ASA, ADS.'),
                              tags$li('Minor textual changes and bug fixes.')
                      ),
                      
                      h3(class = "changes", 'Version 1.5 (April 25, 2019)'),
                      tags$ul(class = "changes", 
                              tags$li('The underlying dataset has been completely redone! We\'ve completely revamped the methodology so produce what we hope to be a cleaner dataset. The transcriptions have been spell-checked, we\'re using the Montreal Forced Aligner for forced alginment, and an in-house version of FAVE for formant extraction. This brings the total number of observations from about 988,000 to over 1,642,211. We will soon have a complete version of this modified dataset online.'),
                              tags$li('On the Point Pattern Analysis page, you have much more control over the shading of the cells. They are now now discrete by default to make it easier to see the differences (but you can change it to continuous or to no color). The number of discrete shades is 4 by default (the data appears to follow a 75-25 rule), but you can change that. And you can now set the color of the darkest shade; when you do, the other shades will be interpolated.'),
                              tags$li('On the Point Pattern Analysis page, you can now change the size and opacity of the grid labels.'),
                              tags$li('Added several new references: an ASA presentation and a DH2019 presentation.'),
                              tags$li('Minor text changes in the help popovers.')
                      ),
                      
                      h3(class = "changes", 'Version 1.4.2 (October 3, 2018)'),
                      tags$ul(class = "changes", 
                              tags$li('Added several new references: A JASA paper, an LSA presentation, and an ADS presentation.')
                      ),
                      h3(class = "changes", 'Version 1.4.1 (September 12, 2018)'),
                      tags$ul(class = "changes", 
                              tags$li('It is now easier to change the x- and y-axis ranges. Fill-in-the-blank boxes have been converted into sliders with ranges.')
                      ),
                      h3(class = "changes", 'Version 1.4 (September 11, 2018)'),
                      tags$ul(class = "changes", 
                              tags$li('It is now possible to download the images! A new "Download" tab has been created that allows you to specify the height, width, quality, and format.'),
                              tags$li('The "Plot Option" tab was split into "Plot" and "Customization." The Customization tab was rearranged slightly to in preparation for additional options.'),
                              tags$li('Added Stanley & Renwick\'s (2019) LSA presentation to the bibliography.')
                      ),
                      h3(class = "changes", 'Version 1.3 (August 28, 2018)'),
                      tags$ul(class = "changes", 
                              tags$li('We\'ve added measurements from over 200,000 vowels to the corpus, bringing the total to 988,217. All speakers are now represented in the corpus with the exception of speaker 850 who had pretty awful data for some reason.'),
                              tags$li('Updated the "Joey\'s filter" procedure to the latest development.'),
                              tags$li('A third filtering option, the Mahalanobis distance, is now available.')
                      ),
                      h3(class = "changes", 'Version 1.2 (June 5, 2018)'),
                      tags$ul(class = "changes", 
                              tags$li('Updated the PPA page so that the bottom corner of the grid (A1) is in a reasonable place instead of being determined by the max(F1) and max(F2) measurements.'),
                              tags$li('In the "Plot Options" tab, the "Fit all data" button has been changed to "Fit Vowel Space". Instead of zooming WAAAY out to fit all the bad data, it zooms to a comfortable vowel space that is consistent across speakers.'),
                              tags$li('This standard vowel space is the default rather than the axes adjusting to accomodate all the selected data. This will make comparing different subsets easier, and when looking at a single vowel it\'ll provide some context as to what portion of the vowel space it occupies.'),
                              tags$li('Added this change log ;)')
                      )
                      
                      # An acoustic persepctive on legacy data: Vowels in the digitial archive of Southern speech. Renwick & Stanley. JASA 141 (5), 3981-3981
                      
                      
             ),
             
             # ____ Vowel Plots --------------------------------------------------------
             
             tabPanel("Vowel Plot Comparison",
                      fluidRow(
                        
                        # This is the custom function I defined above to render each side.
                        column(6, class="left-column",
                               selection_tab_panel("left"),
                               plotOutput("scatterplot_left", height='600px'),
                               
                               hr(),
                               
                               h3("Speaker Summary"),
                               tableOutput("speaker_stats_left"),
                               
                               h3("Vowel Summary"),
                               tableOutput("vowel_stats_left")
                        ),
                        
                        
                        fluidRow(
                          
                          # This is the custom function I defined above to render each side.
                          column(6, class="right-column",
                                 selection_tab_panel("right"),
                                 plotOutput("scatterplot_right", height='600px'),
                                 
                                 hr(),
                                 
                                 h3("Speaker Summary"),
                                 tableOutput("speaker_stats_right"),
                                 
                                 h3("Vowel Summary"),
                                 tableOutput("vowel_stats_right")
                          )
                          
                        ) # end fluidRow
                      )
             ), # end Vowels tab
            
             
             
             # ____ Vowel Trajectories --------------------------------------------------

             tabPanel("Vowel Trajectories",
                      fluidRow(
                        selection_tab_panel("traj")
                      ),
                      
                      hr(),
                      
                      plotOutput("traj_plot", height="600px")
             ),
             
             
             # ____ Grid Charts --------------------------------------------------
             
             
             tabPanel("Point Pattern Analysis",
                      fluidRow(class="grid-column",
                               selection_tab_panel("grid"),
                               
                               hr(),
                               
                               plotOutput("scatterplot_grid", height="600px"),
                               plotOutput("a_curve", height="600px"),
                               
                               fluidRow(
                                 column(6,
                                        h3("Speaker Summary"),
                                        tableOutput("speaker_stats_grid")
                                 ),
                                 column(6,
                                        h3("Vowel Summary"),
                                        tableOutput("vowel_stats_grid")
                                 )
                               ), # end fluidRow
                               
                               hr(),
                               
                               h3("Cell Summary"),
                               DT::dataTableOutput("cell_summary")
                               
                               
                               
                      ) # end fluidRow
             ), # end Vowels tab
             
             # ____ Speaker Info --------------------------------------------------
             
             tabPanel("Speaker Info",
                      
                      fluidRow(
                        column(2, class="speaker-info",
                               selectInput("map_color",
                                           label = h4("Color"),
                                           choices = c("none", "birth year", "sex", "ethnicity", "sector", "education level", "social class", "classification", "land region")),
                               selectInput("map_shape",
                                           label = h4("Shape"),
                                           choices = c("none", "sex", "ethnicity", "education level", "social class", "land region")),
                               checkboxInput("speaker_nums_map",
                                             label=h4("Speaker Numbers"),
                                             value=T)
                        ),
                        
                        column(10,
                               plotOutput("map", height='800px')
                               
                        )
                      ),
                      fluidRow(
                        column(12,
                               DT::dataTableOutput("speaker_table")
                        )
                      )
             ), # end Speaker Info tabPanel
             
             # ____ Signature --------------------------------------------------
             
             footer = tags$footer(p('This is version', version, 'of the', title, 'written by', tags$a(href='http://www.joeystanley.com', 'Joey Stanley.', target='_blank')),
                                  p('Last update: ', date))
             
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, clientData, session) {
  
  
  # ____ Load Data
  
  #observeEvent(input$load_data_button, {
  
  # Add all the data here, if I can get that to work.
  
  #})
  
  # ____ Prep Data  --------------------------------------------------
  
  # Store the datasets in an oject for multiple uses (https://shiny.rstudio.com/gallery/reactivity.html)
  # Only run this if the dataset changes (i.e. aesthetic changes don't run this = saves time)
  
  # ________ Left Vowel Plot Data  --------------------------------------------------
  datasetInput_left <- reactive({
    
    left_data <- NSF %>%
      
      
      # Add the vowel column depending on the input
      mutate(vowel = if (input$trans_left == "SAMPA") { SAMPA }
             else if (input$trans_left == "Plotnik") { Plotnik }
             else if (input$trans_left == "Wells' Lexical Sets") { Wells }
             else { ARPABET }) %>%
      
      # Filter, depending on input
      filter(state %in% input$state_left,
             sex %in% input$sex_left,
             ethnicity %in% input$ethnicity_left,
             education_level %in% input$education_left,
             social_class %in% input$social_class_left,
             classification %in% input$classification_left,
             kurath_type %in% input$kurath_type_left,
             birth_year >= input$yob_left[1] & birth_year <= input$yob_left[2],
             vowel  %in% input$vowel_left,
             stress %in% input$stress_left,
             place  %in% input$place_left,
             voice  %in% input$voice_left,
             manner %in% input$manner_left,
             corpus %in% input$corpus_left)
    if (input$filter_left == "z-score") {
      left_data <- left_data %>% 
        filter(filter_stdev == TRUE)
    } else if (input$filter_left == "Joey's method") {
      left_data <- left_data %>% 
        filter(filter_joey == FALSE)
    } else if (input$filter_left == "Mahalanobis distance") {
      left_data <- left_data %>% 
        filter(filter_mahal == TRUE)
    }
    
    if (input$include_exclude_left == "show") {
      left_data <- left_data %>%
        filter(word %in% input$wordlist_left)
    } else {
      left_data <- left_data %>%
        filter(word %ni% input$wordlist_left)
    }
    
    left_data
    
  })
  
  
  # ________ right Vowel Plot Data  --------------------------------------------------
  datasetInput_right <- reactive({
    
    right_data <- NSF %>%
      
      
      # Add the vowel column depending on the input
      mutate(vowel = if (input$trans_right == "SAMPA") { SAMPA }
             else if (input$trans_right == "Plotnik") { Plotnik }
             else if (input$trans_right == "Wells' Lexical Sets") { Wells }
             else { ARPABET }) %>%
      
      # Filter, depending on input
      filter(state %in% input$state_right,
             sex %in% input$sex_right,
             ethnicity %in% input$ethnicity_right,
             education_level %in% input$education_right,
             social_class %in% input$social_class_right,
             classification %in% input$classification_right,
             kurath_type %in% input$kurath_type_right,
             birth_year >= input$yob_right[1] & birth_year <= input$yob_right[2],
             vowel %in% input$vowel_right,
             stress %in% input$stress_right,
             place %in% input$place_right,
             voice %in% input$voice_right,
             manner %in% input$manner_right,
             corpus %in% input$corpus_right)
    if (input$filter_right == "z-score") {
      right_data <- right_data %>% 
        filter(filter_stdev == TRUE)
    } else if (input$filter_right == "Joey's method") {
      right_data <- right_data %>% 
        filter(filter_joey == FALSE)
    } else if (input$filter_right == "Mahalanobis distance") {
      right_data <- right_data %>% 
        filter(filter_mahal == TRUE)
    }
    
    if (input$include_exclude_right == "show") {
      right_data <- right_data %>%
        filter(word %in% input$wordlist_right)
    } else {
      right_data <- right_data %>%
        filter(word %ni% input$wordlist_right)
    }
    
    right_data
    
  })
  
  
  # ________ int Vowel Plot Data  --------------------------------------------------
  
  datasetInput_traj <- reactive({
    
    traj_data <- NSF %>%
      
      
      # Add the vowel column depending on the input
      mutate(vowel = if (input$trans_traj == "SAMPA") { SAMPA }
             else if (input$trans_traj == "Plotnik") { Plotnik }
             else if (input$trans_traj == "Wells' Lexical Sets") { Wells }
             else { ARPABET }) %>%
      
      # Filter, depending on input
      filter(state %in% input$state_traj,
             sex %in% input$sex_traj,
             ethnicity %in% input$ethnicity_traj,
             education_level %in% input$education_traj,
             social_class %in% input$social_class_traj,
             classification %in% input$classification_traj,
             kurath_type %in% input$kurath_type_traj,
             birth_year >= input$yob_traj[1] & birth_year <= input$yob_traj[2],
             vowel %in% input$vowel_traj,
             stress %in% input$stress_traj,
             place %in% input$place_traj,
             voice %in% input$voice_traj,
             manner %in% input$manner_traj,
             corpus %in% input$corpus_traj)
    if (input$filter_traj == "z-score") {
      traj_data <- traj_data %>% 
        filter(filter_stdev == TRUE)
    } else if (input$filter_traj == "Joey's method") {
      traj_data <- traj_data %>% 
        filter(filter_joey == FALSE)
    } else if (input$filter_traj == "Mahalanobis distance") {
      traj_data <- traj_data %>% 
        filter(filter_mahal == TRUE)
    }
    
    if (input$include_exclude_traj == "show") {
      traj_data <- traj_data %>%
        filter(word %in% input$wordlist_traj)
    } else {
      traj_data <- traj_data %>%
        filter(word %ni% input$wordlist_traj)
    }
    
    traj_data
    
  })
  
  # ________ Grid Plot Data  --------------------------------------------------
  datasetInput_grid <- reactive({
    
    
    grid_data <- NSF %>%
      
      # Add the vowel column depending on the input
      mutate(vowel = if (input$trans_grid == "SAMPA") { SAMPA }
             else if (input$trans_grid == "Plotnik") { Plotnik }
             else if (input$trans_grid == "Wells' Lexical Sets") { Wells }
             else { ARPABET }) %>%
      
      # Filter, depending on input
      filter(state %in% input$state_grid,
             sex %in% input$sex_grid,
             ethnicity %in% input$ethnicity_grid,
             education_level %in% input$education_grid,
             social_class %in% input$social_class_grid,
             classification %in% input$classification_grid,
             kurath_type %in% input$kurath_type_grid,
             birth_year >= input$yob_grid[1] & birth_year <= input$yob_grid[2],
             vowel %in% input$vowel_grid,
             stress %in% input$stress_grid,
             place %in% input$place_grid,
             voice %in% input$voice_grid,
             manner %in% input$manner_grid,
             corpus %in% input$corpus_grid)
    
    if (input$filter_grid == "z-score") {
      grid_data <- grid_data %>% filter(filter_stdev == TRUE)
    } else if (input$filter_grid == "Joey's method") {
      grid_data <- grid_data %>% filter(filter_joey == FALSE)
    } else if (input$filter_grid == "Mahalanobis distance") {
      grid_data <- grid_data %>% filter(filter_mahal == TRUE)
    }
    
    if (input$include_exclude_grid == "show") {
      grid_data <- grid_data %>%
        filter(word %in% input$wordlist_grid)
    } else {
      grid_data <- grid_data %>%
        filter(word %ni% input$wordlist_grid)
    }
    
    
    
    # Normalization technique
    if(input$norm_grid == "Lobanov") {
      grid_data <- grid_data %>% mutate(F1 = F1.50._lob, F2 = F2.50._lob)
      this_max_F1 <- max_F1_lob
      this_min_F1 <- min_F1_lob
      this_max_F2 <- max_F2_lob
      this_min_F2 <- min_F2_lob
      
    } else if (input$norm_grid == "Bark Difference Metric") {
      grid_data <- grid_data %>% mutate(F1 = Bark_height, F2 = Bark_backness)
      this_max_F1 <- max_F1_bark
      this_min_F1 <- min_F1_bark
      this_max_F2 <- max_F2_bark
      this_min_F2 <- min_F2_bark
    } else {
      grid_data <- grid_data %>% mutate(F1 = F1.50., F2 = F2.50.)
      this_max_F1 <- max_F1
      this_min_F1 <- min_F1
      this_max_F2 <- max_F2
      this_min_F2 <- min_F2
    }
    
    
    # Get the cell information
    grid_data$F1_groups <- cut(grid_data$F1,
                               breaks = seq(from = this_min_F1,
                                            to   = this_max_F1,
                                            length.out = input$grid_rows+1),
                               include.lowest=T,
                               ordered_result=T)
    
    grid_data$F2_groups <- cut(grid_data$F2,
                               breaks = seq(from = this_min_F2,
                                            to   = this_max_F2,
                                            length.out = input$grid_cols+1),
                               include.lowest=T,
                               ordered_result=T)
    
    grid_data$cell <- paste0(cut(grid_data$F2,
                                 breaks = seq(from = this_min_F2,
                                              to   = this_max_F2,
                                              length.out = input$grid_cols+1),
                                 labels=LETTERS[input$grid_cols:1],
                                 include.lowest=T,
                                 ordered_result=T),
                             cut(grid_data$F1,
                                 breaks = seq(from = this_min_F1,
                                              to   = this_max_F1,
                                              length.out = input$grid_rows+1),
                                 labels = input$grid_rows:1,
                                 include.lowest=T,
                                 ordered_result=T))
    
    grid_data
    
  })
  
  # ________ Cells Data  --------------------------------------------------
  
  datasetCells <- reactive({
    # Import the data
    dataset <- datasetInput_grid()
    
    # Get midpoints, max, and min for each cell
    dataset$F1_cell_min <- as.numeric(str_match(dataset$F1_groups, ".(.*),")[,2])
    dataset$F1_cell_max <- as.numeric(str_match(dataset$F1_groups, ",(.*).")[,2])
    dataset$F1_cell_mid <- (dataset$F1_cell_min + dataset$F1_cell_max) / 2
    dataset$F2_cell_min <- as.numeric(str_match(dataset$F2_groups, ".(.*),")[,2])
    dataset$F2_cell_max <- as.numeric(str_match(dataset$F2_groups, ",(.*).")[,2])
    dataset$F2_cell_mid <- (dataset$F2_cell_min + dataset$F2_cell_max) / 2
    
    
    # Create a dataset just about the cells
    cells <- dataset %>%
      group_by(cell) %>%
      summarise(n=n(),
                F1_min = mean(F1_cell_min), F1_mid = mean(F1_cell_mid), F1_max = mean(F1_cell_max),
                F2_min = mean(F2_cell_min), F2_mid = mean(F2_cell_mid), F2_max = mean(F2_cell_max))
    cells
  })
  
  
  # ____ Dynamically update UI ---------------------------------------------------------
  
  # ________ Change Transcriptions in dropdown  --------------------------------------------------
  
  # Left chart
  observe({
    
    if (input$trans_left == "SAMPA") {
      updateSelectizeInput(session, "vowel_left",
                           choices=c(levels(vowels$SAMPA)),
                           selected=c(levels(vowels$SAMPA)))
    } else if (input$trans_left == "Plotnik") {
      updateSelectizeInput(session, "vowel_left",
                           choices=c(levels(NSF$Plotnik)),
                           selected=c(levels(NSF$Plotnik)))
    } else if (input$trans_left == "Wells' Lexical Sets") {
      updateSelectizeInput(session, "vowel_left",
                           choices=c(levels(vowels$Wells)),
                           selected=c(levels(vowels$Wells)))
    } else {
      updateSelectizeInput(session, "vowel_left",
                           choices=c(levels(vowels$ARPABET)),
                           selected=c(levels(vowels$ARPABET)))
    }
  })
  
  # Right chart
  observe({
    
    if (input$trans_right == "SAMPA") {
      updateSelectizeInput(session, "vowel_right",
                           choices=c(levels(vowels$SAMPA)),
                           selected=c(levels(vowels$SAMPA)))
    } else if (input$trans_right == "Plotnik") {
      updateSelectizeInput(session, "vowel_right",
                           choices=c(levels(NSF$Plotnik)),
                           selected=c(levels(NSF$Plotnik)))
    } else if (input$trans_right == "Wells' Lexical Sets") {
      updateSelectizeInput(session, "vowel_right",
                           choices=c(levels(vowels$Wells)),
                           selected=c(levels(vowels$Wells)))
    } else {
      updateSelectizeInput(session, "vowel_right",
                           choices=c(levels(vowels$ARPABET)),
                           selected=c(levels(vowels$ARPABET)))
    }
  })
  
  # Interactive chart
  observe({
    
    if (input$trans_traj == "SAMPA") {
      updateSelectizeInput(session, "vowel_traj",
                           choices=c(levels(vowels$SAMPA)),
                           selected=c(levels(vowels$SAMPA)))
    } else if (input$trans_traj == "Plotnik") {
      updateSelectizeInput(session, "vowel_traj",
                           choices=c(levels(NSF$Plotnik)),
                           selected=c(levels(NSF$Plotnik)))
    } else if (input$trans_traj == "Wells' Lexical Sets") {
      updateSelectizeInput(session, "vowel_traj",
                           choices=c(levels(vowels$Wells)),
                           selected=c(levels(vowels$Wells)))
    } else {
      updateSelectizeInput(session, "vowel_traj",
                           choices=c(levels(vowels$ARPABET)),
                           selected=c(levels(vowels$ARPABET)))
    }
  })
  
  # Grid chart
  observe({
    
    if (input$trans_grid == "SAMPA") {
      updateSelectizeInput(session, "vowel_grid",
                           choices=c(levels(vowels$SAMPA)),
                           selected=c(levels(vowels$SAMPA)))
    } else if (input$trans_grid == "Plotnik") {
      updateSelectizeInput(session, "vowel_grid",
                           choices=c(levels(NSF$Plotnik)),
                           selected=c(levels(NSF$Plotnik)))
    } else if (input$trans_grid == "Wells' Lexical Sets") {
      updateSelectizeInput(session, "vowel_grid",
                           choices=c(levels(vowels$Wells)),
                           selected=c(levels(vowels$Wells)))
    } else {
      updateSelectizeInput(session, "vowel_grid",
                           choices=c(levels(vowels$ARPABET)),
                           selected=c(levels(vowels$ARPABET)))
    }
  })
  
  # ________ Change Normalization Numbers in Zoom  --------------------------------------------------
  
  # Left Scatterplot
  observe({
    
    dataset <- datasetInput_left() %>%
      select(F1.50., F2.50., F1.50._lob, F2.50._lob, Bark_height, Bark_backness)
    
    if (input$norm_left == "Lobanov") {
      updateSliderInput(session, "F1_range_left",
                        min = min(NSF$F1.50._lob), 
                        max = max(NSF$F1.50._lob),
                        value = c(min_F1_lob, max_F1_lob),
                        step = 0.1)
      updateSliderInput(session, "F2_range_left",
                        min = min(NSF$F1.50._lob), 
                        max = max(NSF$F1.50._lob),
                        value = c(min_F1_lob, max_F1_lob),
                        step = 0.1)
    } else if (input$norm_left == "Bark Difference Metric") {
      updateSliderInput(session, "F1_range_left",
                        min = 0, 
                        max = max(NSF$Bark_height),
                        value = c(min_F1_bark, max_F1_bark),
                        step = 0.1)
      updateSliderInput(session, "F2_range_left",
                        min = 0, 
                        max = max(NSF$Bark_backness),
                        value = c(min_F2_bark, max_F2_bark),
                        step = 0.1)
    } else {
      updateSliderInput(session, "F1_range_left",
                        min = round(min(NSF$F1.50.),0), 
                        max = round(max(NSF$F1.50.),0),
                        value = c(min_F1, max_F1),
                        step = 1)
      updateSliderInput(session, "F2_range_left",
                        min = round(min(NSF$F2.50.),0), 
                        max = round(max(NSF$F2.50.),0),
                        value = c(min_F2, max_F2),
                        step = 1)
    }
  })
  
  # Right Scatterplot
  observe({
    
    dataset <- datasetInput_right() %>%
      select(F1.50., F2.50., F1.50._lob, F2.50._lob, Bark_height, Bark_backness)
    
    if (input$norm_right == "Lobanov") {
      updateSliderInput(session, "F1_range_right",
                        min = min(NSF$F1.50._lob), 
                        max = max(NSF$F1.50._lob),
                        value = c(min_F1_lob, max_F1_lob),
                        step = 0.1)
      updateSliderInput(session, "F2_range_right",
                        min = min(NSF$F1.50._lob), 
                        max = max(NSF$F1.50._lob),
                        value = c(min_F1_lob, max_F1_lob),
                        step = 0.1)
    } else if (input$norm_right == "Bark Difference Metric") {
      updateSliderInput(session, "F1_range_right",
                        min = 0, 
                        max = max(NSF$Bark_height),
                        value = c(min_F1_bark, max_F1_bark),
                        step = 0.1)
      updateSliderInput(session, "F2_range_right",
                        min = 0, 
                        max = max(NSF$Bark_backness),
                        value = c(min_F2_bark, max_F2_bark),
                        step = 0.1)
    } else {
      updateSliderInput(session, "F1_range_right",
                        min = round(min(NSF$F1.50.),0), 
                        max = round(max(NSF$F1.50.),0),
                        value = c(min_F1, max_F1),
                        step = 1)
      updateSliderInput(session, "F2_range_right",
                        min = round(min(NSF$F2.50.),0),
                        max = round(max(NSF$F2.50.),0),
                        value = c(min_F2, max_F2),
                        step = 1)
    }
  })
  
  # Grid Chart
  observe({
    
    dataset <- datasetInput_grid() %>%
      select(F1.50., F2.50., F1.50._lob, F2.50._lob, Bark_height, Bark_backness)
    
    if (input$norm_grid == "Lobanov") {
      updateSliderInput(session, "F1_range_grid",
                        min = min(NSF$F1.50._lob), 
                        max = max(NSF$F1.50._lob),
                        value = c(min_F1_lob, max_F1_lob),
                        step = 0.1)
      updateSliderInput(session, "F2_range_grid",
                        min = min(NSF$F1.50._lob), 
                        max = max(NSF$F1.50._lob),
                        value = c(min_F1_lob, max_F1_lob),
                        step = 0.1)
    } else if (input$norm_grid == "Bark Difference Metric") {
      updateSliderInput(session, "F1_range_grid",
                        min = 0, 
                        max = max(NSF$Bark_height),
                        value = c(min_F1_bark, max_F1_bark),
                        step = 0.1)
      updateSliderInput(session, "F2_range_grid",
                        min = 0, 
                        max = max(NSF$Bark_backness),
                        value = c(min_F2_bark, max_F2_bark),
                        step = 0.1)
    } else {
      updateSliderInput(session, "F1_range_grid",
                        min = round(min(NSF$F1.50.),0), 
                        max = round(max(NSF$F1.50.),0),
                        value = c(min_F1, max_F1),
                        step = 1)
      updateSliderInput(session, "F2_range_grid",
                        min = round(min(NSF$F2.50.),0), 
                        max = round(max(NSF$F2.50.),0),
                        value = c(min_F2, max_F2),
                        step = 1)
    }
  })
  
  # ________ Zoom in/out buttons  ---------------------------------------------------
  
  
  # Left PLot
  observeEvent(input$zoom_in_max_left, {
    dataset <- datasetInput_left()
    
    # Find what values to use based on the normalization.
    if (input$norm_left == "Lobanov") {
      dataset <- dataset %>%
        mutate(F1 = F1.50._lob,
               F2 = F2.50._lob)
    } else if (input$norm_left == "Bark Difference Metric") {
      dataset <- dataset %>%
        mutate(F1 = Bark_height,
               F2 = Bark_backness)
    } else {
      dataset <- dataset %>%
        mutate(F1 = F1.50.,
               F2 = F2.50.)
    }
    
    updateSliderInput(session, "F2_range_left",
                      value = c(round(min(dataset$F2),0), round(max(dataset$F2),0)),
                      step = 0.1)
    updateSliderInput(session, "F1_range_left",
                      value = c(round(min(dataset$F1),0), round(max(dataset$F1),0)),
                      step = 0.1)
  })
  
  observeEvent(input$zoom_out_max_left, {
    dataset <- NSF
    
    # Find what values to use based on the normalization.
    if (input$norm_left == "Lobanov") {
      updateSliderInput(session, "F1_range_left",
                        min = min(NSF$F1.50._lob), 
                        max = max(NSF$F1.50._lob),
                        value = c(min_F1_lob, max_F1_lob),
                        step = 0.1)
      updateSliderInput(session, "F2_range_left",
                        min = min(NSF$F1.50._lob), 
                        max = max(NSF$F1.50._lob),
                        value = c(min_F1_lob, max_F1_lob),
                        step = 0.1)
    } else if (input$norm_left == "Bark Difference Metric") {
      updateSliderInput(session, "F1_range_left",
                        min = 0, 
                        max = max(NSF$Bark_height),
                        value = c(min_F1_bark, max_F1_bark),
                        step = 0.1)
      updateSliderInput(session, "F2_range_left",
                        min = 0, 
                        max = max(NSF$Bark_backness),
                        value = c(min_F2_bark, max_F2_bark),
                        step = 0.1)
    } else {
      updateSliderInput(session, "F1_range_left",
                        min = round(min(NSF$F1.50.),0), 
                        max = round(max(NSF$F1.50.),0),
                        value = c(min_F1, max_F1),
                        step = 1)
      updateSliderInput(session, "F2_range_left",
                        min = round(min(NSF$F2.50.),0), 
                        max = round(max(NSF$F2.50.),0),
                        value = c(min_F2, max_F2),
                        step = 1)
    }
  })
  
  
  # Right plot
  observeEvent(input$zoom_in_max_right, {
    dataset <- datasetInput_right()
    
    # Find what values to use based on the normalization.
    if (input$norm_right == "Lobanov") {
      dataset <- dataset %>%
        mutate(F1 = F1.50._lob,
               F2 = F2.50._lob)
    } else if (input$norm_right == "Bark Difference Metric") {
      dataset <- dataset %>%
        mutate(F1 = Bark_height,
               F2 = Bark_backness)
    } else {
      dataset <- dataset %>%
        mutate(F1 = F1.50.,
               F2 = F2.50.)
    }
    
    updateSliderInput(session, "F2_range_right",
                      value = c(round(min(dataset$F2),0), round(max(dataset$F2),0)),
                      step = 0.1)
    updateSliderInput(session, "F1_range_right",
                      value = c(round(min(dataset$F1),0), round(max(dataset$F1),0)),
                      step = 0.1)
  })
  
  observeEvent(input$zoom_out_max_right, {
    dataset <- NSF
    
    # Find what values to use based on the normalization.
    if (input$norm_right == "Lobanov") {
      updateSliderInput(session, "F1_range_right",
                        min = min(NSF$F1.50._lob), 
                        max = max(NSF$F1.50._lob),
                        value = c(min_F1_lob, max_F1_lob),
                        step = 0.1)
      updateSliderInput(session, "F2_range_right",
                        min = min(NSF$F1.50._lob), 
                        max = max(NSF$F1.50._lob),
                        value = c(min_F1_lob, max_F1_lob),
                        step = 0.1)
    } else if (input$norm_right == "Bark Difference Metric") {
      updateSliderInput(session, "F1_range_right",
                        min = 0, 
                        max = max(NSF$Bark_height),
                        value = c(min_F1_bark, max_F1_bark),
                        step = 0.1)
      updateSliderInput(session, "F2_range_right",
                        min = 0, 
                        max = max(NSF$Bark_backness),
                        value = c(min_F2_bark, max_F2_bark),
                        step = 0.1)
    } else {
      updateSliderInput(session, "F1_range_right",
                        min = round(min(NSF$F1.50.),0), 
                        max = round(max(NSF$F1.50.),0),
                        value = c(min_F1, max_F1),
                        step = 1)
      updateSliderInput(session, "F2_range_right",
                        min = round(min(NSF$F2.50.),0), 
                        max = round(max(NSF$F2.50.),0),
                        value = c(min_F2, max_F2),
                        step = 1)
    }
  })
  
  
  # Grid plot
  observeEvent(input$zoom_in_max_grid, {
    dataset <- datasetInput_grid()
    
    # Find what values to use based on the normalization.
    if (input$norm_grid == "Lobanov") {
      dataset <- dataset %>%
        mutate(F1 = F1.50._lob,
               F2 = F2.50._lob)
    } else if (input$norm_grid == "Bark Difference Metric") {
      dataset <- dataset %>%
        mutate(F1 = Bark_height,
               F2 = Bark_backness)
    } else {
      dataset <- dataset %>%
        mutate(F1 = F1.50.,
               F2 = F2.50.)
    }
    
    updateSliderInput(session, "F2_range_grid",
                      value = c(round(min(dataset$F2),0), round(max(dataset$F2),0)),
                      step = 0.1)
    updateSliderInput(session, "F1_range_grid",
                      value = c(round(min(dataset$F1),0), round(max(dataset$F1),0)),
                      step = 0.1)
  })
  
  observeEvent(input$zoom_out_max_grid, {
    dataset <- NSF
    
    # Find what values to use based on the normalization.
    if (input$norm_grid == "Lobanov") {
      updateSliderInput(session, "F1_range_grid",
                        min = min(NSF$F1.50._lob), 
                        max = max(NSF$F1.50._lob),
                        value = c(min_F1_lob, max_F1_lob),
                        step = 0.1)
      updateSliderInput(session, "F2_range_grid",
                        min = min(NSF$F1.50._lob), 
                        max = max(NSF$F1.50._lob),
                        value = c(min_F1_lob, max_F1_lob),
                        step = 0.1)
    } else if (input$norm_grid == "Bark Difference Metric") {
      updateSliderInput(session, "F1_range_grid",
                        min = 0, 
                        max = max(NSF$Bark_height),
                        value = c(min_F1_bark, max_F1_bark),
                        step = 0.1)
      updateSliderInput(session, "F2_range_grid",
                        min = 0, 
                        max = max(NSF$Bark_backness),
                        value = c(min_F2_bark, max_F2_bark),
                        step = 0.1)
    } else {
      updateSliderInput(session, "F1_range_grid",
                        min = round(min(NSF$F1.50.),0), 
                        max = round(max(NSF$F1.50.),0),
                        value = c(min_F1, max_F1),
                        step = 1)
      updateSliderInput(session, "F2_range_grid",
                        min = round(min(NSF$F2.50.),0), 
                        max = round(max(NSF$F2.50.),0),
                        value = c(min_F2, max_F2),
                        step = 1)
    }
  })
  
  
  
  # ________ Change Aspect Ratio  --------------------------------------------------
  
  # Left Scatterplot
  observe({
    
    if (input$norm_left == "Lobanov") {
      updateNumericInput(session, "ratio_left", value=0.5)
    } else if (input$norm_left == "Bark Difference Metric") {
      updateNumericInput(session, "ratio_left", value=0.5)
    } else {
      updateNumericInput(session, "ratio_left", value=2)
    }
  })
  
  # right Scatterplot
  observe({
    
    if (input$norm_right == "Lobanov") {
      updateNumericInput(session, "ratio_right", value=0.5)
    } else if (input$norm_right == "Bark Difference Metric") {
      updateNumericInput(session, "ratio_right", value=0.5)
    } else {
      updateNumericInput(session, "ratio_right", value=2)
    }
  })
  
  # interactive Scatterplot
  observe({
    
    if (input$norm_traj == "Lobanov") {
      updateNumericInput(session, "ratio_traj", value=0.5)
    } else if (input$norm_traj == "Bark Difference Metric") {
      updateNumericInput(session, "ratio_traj", value=0.5)
    } else {
      updateNumericInput(session, "ratio_traj", value=2)
    }
  })
  
  # grid Scatterplot
  observe({
    
    if (input$norm_grid == "Lobanov") {
      updateNumericInput(session, "ratio_grid", value=0.5)
    } else if (input$norm_grid == "Bark Difference Metric") {
      updateNumericInput(session, "ratio_grid", value=0.5)
    } else {
      updateNumericInput(session, "ratio_grid", value=2)
    }
  })
  
  # ________ Default function words button -----------------------------------------------
  
  # Add the default list of stopwords
  observeEvent(input$stopwords_btn_left, {
    updateSelectInput(session, "wordlist_left", selected = stopwords)
  })
  observeEvent(input$stopwords_btn_right, {
    updateSelectInput(session, "wordlist_right", selected = stopwords)
  })
  observeEvent(input$stopwords_btn_traj, {
    updateSelectInput(session, "wordlist_traj", selected = stopwords)
  })
  observeEvent(input$stopwords_btn_grid, {
    updateSelectInput(session, "wordlist_grid", selected = stopwords)
  })
  
  # Clear the list of words
  observeEvent(input$clear_words_btn_left, {
    updateSelectInput(session, "wordlist_left", selected = "")
  })
  observeEvent(input$clear_words_btn_right, {
    updateSelectInput(session, "wordlist_right", selected = "")
  })
  observeEvent(input$clear_words_btn_traj, {
    updateSelectInput(session, "wordlist_traj", selected = "")
  })
  observeEvent(input$clear_words_btn_grid, {
    updateSelectInput(session, "wordlist_grid", selected = "")
  })
  
  # ____ Plots -------------------------------------------------------------------
  
  # The general plot function
  plot_input <- function(side) {
    
    # Retrieve the dataset (I don't think I can do this more elegantly)
    if (side == "left") {
      dataset <- datasetInput_left()
    } else if (side == "right") {
      dataset <- datasetInput_right()
    } else if (side == "int") {
      dataset <- datasetInput_traj()
    } else if (side == "grid") {
      dataset <- datasetInput_grid()
      cells <- datasetCells()
    }
    
    # Normalization technique
    if(input[[paste0("norm_", side)]] == "Lobanov") {
      dataset <- dataset %>% mutate(F1 = F1.50._lob, F2 = F2.50._lob)
      major_breaks_x <- seq(-10, 10, 1)
      minor_breaks_x <- seq(-10, 10, 0.25)
      major_breaks_y <- seq(-10, 10, 1)
      minor_breaks_y <- seq(-10, 10, 0.25)
      plot <- ggplot(dataset, aes(x=F2, y=F1, color=vowel)) +
        xlab("Lobanov Normalized F2 (midpoint)") +
        ylab("Lobanov Normalized F1 (midpoint)")
    } else if (input[[paste0("norm_", side)]] == "Bark Difference Metric") {
      dataset <- dataset %>% mutate(F1 = Bark_height, F2 = Bark_backness)
      major_breaks_x <- seq(0, 20, 1)
      minor_breaks_x <- seq(0, 20, 0.25)
      major_breaks_y <- seq(0, 20, 1)
      minor_breaks_y <- seq(0, 20, 0.25)
      plot <- ggplot(dataset, aes(x=F2, y=F1, color=vowel)) +
        xlab("Bark Difference Metric (bark(F3) - bark(F2))") +
        ylab("Bark Difference Metric (bark(F3) - bark(F1))")
    } else {
      dataset <- dataset %>% mutate(F1 = F1.50., F2 = F2.50.)
      major_breaks_x <- seq(0, 5000, 500)
      minor_breaks_x <- seq(0, 5000, 100)
      major_breaks_y <- seq(0, 5000, 200)
      minor_breaks_y <- seq(0, 5000, 50)
      
      plot <- ggplot(dataset, aes(x=F2, y=F1, color=vowel)) +
        xlab("F2 (midpoint)") +
        ylab("F1 (midpoint)")
    }
    
    # Add the grid
    if (side == "grid") {
      
      # If it's discrete shading
      if (input$shading_type == "Discrete") {
        cells <- cells %>%
          mutate(n_cat = cut(n, breaks = input$grid_shades,
                             include.lowest = FALSE,
                             ordered_result = TRUE),
                 n_cat = fct_rev(n_cat))
        plot <- plot +
          geom_tile(data=cells, aes(x=F2_mid, y=F1_mid, fill=n_cat), color="grey25") +
          scale_fill_manual(name = "tokens per cell", 
                            values = color_gradienter(hi = input$shade_color, lo = "#ffffff", shades = input$grid_shades))
        
        
        # If it's continuous shading
      } else if (input$shading_type == "Continuous") {
        plot <- plot + 
          geom_tile(data=cells, aes(x=F2_mid, y=F1_mid, fill=n), color="grey25") +
          scale_fill_continuous(low="white", high=input$shade_color)
        
        
        # If there's no shading
      } else {
        plot <- plot +
          geom_tile(data=cells, aes(x=F2_mid, y=F1_mid), fill = "white", color="grey25")
        
      }
    }
    
    
    # Persistent colors, regardless of subset, if this is checked
    if (input[[paste0("colors_", side)]] == "Multicolored (persistent)") {
      
      # https://stackoverflow.com/questions/35279570/assign-point-color-depending-on-data-frame-column-value-r
      col <- as.character(colors_11)
      
      if (input[[paste0("trans_", side)]] == "ARPABET") {
        names(col) <- as.character(vowels$ARPABET)
      } else if (input[[paste0("trans_", side)]] == "SAMPA") {
        names(col) <- as.character(vowels$SAMPA)
      } else if (input[[paste0("trans_", side)]] == "Plotnik") {
        col <- as.character(colors_19)
        names(col) <- as.character(levels(NSF$Plotnik))
      } else if (input[[paste0("trans_", side)]] == "Wells' Lexical Sets") {
        names(col) <- as.character(vowels$Wells)
      }
      
      plot <- plot + scale_color_manual(values=col)
      
    } else if (input[[paste0("colors_", side)]] == "Black") {
      plot <- plot + scale_color_manual(values=rep("black", 20))
    }
    
    # Add ellipses
    if(input[[paste0("ellipses_", side)]] == TRUE) {
      plot <- plot + stat_ellipse(aes(group=vowel),
                                  level = input[[paste0("ellipsesSize_", side)]]/100, 
                                  size=1, 
                                  linetype=1, 
                                  alpha = input[[paste0("ellipsesAlpha_", side)]])
    }
    
    # Add the means
    if(input[[paste0("means_", side)]] == T) {
      means_temp <- dataset %>%
        group_by(vowel) %>%
        summarise(mean_F1 = mean(F1),
                  mean_F2 = mean(F2))
      
      plot <- plot + 
        geom_text(data = means_temp, 
                  aes(x = mean_F2, y = mean_F1, label = vowel, color = vowel),
                  size  = input[[paste0("meansSize_",  side)]], 
                  alpha = input[[paste0("meansAlpha_", side)]])
    }
    
    
    # Add the points and transparency
    if (input[[paste0("points_", side)]] == TRUE) {
      plot <- plot + 
        geom_point(alpha = input[[paste0("pointsAlpha_", side)]],
                   size  = input[[paste0("pointsSize_", side)]])
    }
    
    # Add the text
    if (input[[paste0("words_", side)]]) {
      plot <- plot + 
        geom_text(aes(label = word), 
                  alpha = input[[paste0("wordsAlpha_", side)]], 
                  size  = input[[paste0("wordsSize_", side)]])
    }
    
    
    if (side == "grid") {
      if(input$grid_cell_labels == TRUE) {
        plot <- plot +
          geom_text(data=cells, aes(x=F2_mid, y=F1_mid,label=cell), 
                    size = input$grid_label_size, 
                    alpha = input$grid_label_alpha,
                    color="black")
      }
    }
    
    
    
    # Other (global) plot parameters.
    plot <- plot + 
      # x and y limits change depending on transcription system already.
      coord_fixed(ratio = input[[paste0("ratio_", side)]], 
                  # These will change when the zoom changes.
                  xlim = c(min(input[[paste0("F2_range_", side)]]), 
                           max(input[[paste0("F2_range_", side)]])), 
                  ylim = c(min(input[[paste0("F1_range_", side)]]), 
                           max(input[[paste0("F1_range_", side)]]))) + 
      scale_x_reverse(breaks = major_breaks_x,
                      minor_breaks = minor_breaks_x) + 
      scale_y_reverse(breaks = major_breaks_y,
                      minor_breaks = minor_breaks_y) + 
      theme_bw()
    
    plot
  }
  
  
  # The general function to handle the downloading.
  download_image <- function(side) {
    downloadHandler(
      
      filename = function() {
        paste0(input[[paste0("filename_", side)]], ".", tolower(input[[paste0("filetype_", side)]]))
      },
      content = function(file) {
        ggsave(file, 
               plot   = plot_input(side = side),
               height = input[[paste0("height_", side)]],
               width  = input[[paste0("width_", side)]],
               dpi    = input[[paste0("dpi_", side)]])
      }
    )
  }
  
  
  # ________ Left Scatterplot  --------------------------------------------------
  
  output$scatterplot_left <- renderPlot({ 
    plot_input(side = "left") 
  })
  
  
  output$download_left <- download_image(side = "left")
  
  # ________ Right Scatterplot  --------------------------------------------------
  
  output$scatterplot_right <- renderPlot({ 
    plot_input(side = "right") 
  })
  
  output$download_right <- download_image(side = "right")
  
  # ________ Interactive Scatterplot  --------------------------------------------------
  
  output$interactive_plot <- renderPlot({ 
    plot_input(side = "int") 
  })
  
  output$download_traj <- download_image(side = "int")
  
  # ________ Grid Chart -------------------------------------------------------------
  
  output$scatterplot_grid <- renderPlot({ 
    plot_input(side = "grid") 
  })
  
  output$download_grid <- download_image(side = "grid")
  
  
  # ________ A-Curve -------------------------------------------------------------
  
  
  output$a_curve <- renderPlot({
    
    dataset <- datasetInput_grid()
    
    text_x <- length(unique(dataset$cell)) * 0.85
    text_y <- max(table(dataset$cell)) * 0.85
    
    dataset$cell <- factor(dataset$cell,
                           levels=names(sort(table(dataset$cell), decreasing=TRUE)))
    ggplot(dataset, aes(cell)) +
      geom_bar(width = 0.75, fill="#144387") +
      annotate("rect", fill="white", color="black",
               xmin = text_x + 0.1*text_x, xmax = text_x - 0.1*text_x,
               ymin = text_y + 0.1*text_y, ymax = text_y - 0.1*text_y) +
      annotate("text", x = text_x, y = text_y, size=7,
               label = paste("Gini Coefficient\nG =", round(Gini(dataset$cell),3))) +
      theme_bw()
    
  })
  
  
  # ____ Summary Tables --------------------------------------------------
  
  # ________ Click/Hover Stats -------------------------------------------------------------
  
  # The click returns the x and y coordinates of the plot. I'll have find the point
  # in the database with the closest Euclidean distance to that point in order to
  # display information about that point.
  output$click_info <- renderPrint({
    
    if (!is.null(input$plot_click$x) & !is.null(input$plot_click$y)) {
      
      # Alternative, using built-in functions, but they're not as good.
      # point <- nearPoints(datasetInput_left(), input$plot_click_left, addDist = TRUE)
      # subset(point, select=c(speaker, word, F1.50., F2.50.))
      
      # Get the click coordinates (originally comes as "num 1.23" so I have to remove the text)
      x <- as.numeric(sub("num ", "", input$plot_click$x))
      y <- as.numeric(sub("num ", "", input$plot_click$y))
      
      # Get the dataset, and create temporary normalized points
      dataset <- datasetInput_traj() %>%
        mutate(temp_normF1 = (F1.50. - mean(F1.50.))/sd(F1.50.),
               temp_normF2 = (F2.50. - mean(F2.50.))/sd(F2.50.))
      
      # Have to use normalized data or else F1 closeless overpowers F2
      
      # Get normalized click coordinates
      x_norm <- (x - mean(dataset$F2.50.)) / sd(dataset$F2.50.)
      y_norm <- (y - mean(dataset$F1.50.)) / sd(dataset$F1.50.)
      
      # Get euclidean distance based on normalized stuff
      dataset %>%
        mutate(dist = sqrt((x_norm - temp_normF2)^2 +
                             (y_norm - temp_normF1)^2)) %>%
        arrange(dist) %>%
        
        # Select the columns and number of rows.
        select(speaker, word, vowel, stress, F1.50., F2.50., F1.50._lob, F2.50._lob, Bark_height, Bark_backness) %>%
        rename(`F1 (Hz)` = F1.50.,
               `F2 (Hz)` = F2.50.,
               `F1 (norm)` = F1.50._lob,
               `F2 (norm)` = F2.50._lob,
               `Bark(F3)-Bark(F1)` = Bark_height,
               `Bark(F3)-Bark(F2)` = Bark_backness) %>%
        print(n = input$display_n_rows, width = 1000)
    } else {
      "Click the plot to populate this table."
    }
    
  })
  
  # Same as above, but with hover
  # output$hover_info <- renderPrint({
  #   
  #   if (!is.null(input$plot_hover$x) & !is.null(input$plot_hover$y)) {
  #     
  #     x <- as.numeric(sub("num ", "", input$plot_hover$x))
  #     y <- as.numeric(sub("num ", "", input$plot_hover$y))
  #     
  #     dataset <- datasetInput_int() %>%
  #       mutate(temp_normF1 = (F1.50. - mean(F1.50.))/sd(F1.50.),
  #              temp_normF2 = (F2.50. - mean(F2.50.))/sd(F2.50.))
  #     
  #     x_norm <- (x - mean(dataset$F2.50.)) / sd(dataset$F2.50.)
  #     y_norm <- (y - mean(dataset$F1.50.)) / sd(dataset$F1.50.)
  #     
  #     dataset <- dataset %>%
  #       mutate(dist = sqrt((x_norm - temp_normF2)^2 +
  #                            (y_norm - temp_normF1)^2)) %>%
  #       arrange(dist)
  #     
  #     subset(head(dataset, n=5),
  #            select=c(speaker, word, vowel, F1.50., F2.50.))
  #     
  #   } else {
  #     "Hover over plot to populate this table."
  #   }
  #   
  # })
  # 
  # # Find the nearest means to the double click.
  # output$dblclick_info <- renderPrint({
  #   
  #   if(exists('vowel1') & exists('vowel2')) {
  #     paste0("Vowel 1 is ", vowel1, " and vowel 2 is ", vowel2, ".")
  #     
  #   } else if (exists('vowel1')) {
  #     paste0("Vowel 1 is ", vowel1, ". Click again for another vowel.")
  #     
  #   } else {
  #     
  #     vowel1 = c("AE")
  #     "Double click two vowels to measure their overlap. (Coming Soon!)"
  #   }
  #   
  #   # if (!is.null(input$plot_dblclick$x) & !is.null(input$plot_dblclick$y)) {
  #   #   
  #   #   x <- as.numeric(sub("num ", "", input$plot_dblclick$x))
  #   #   y <- as.numeric(sub("num ", "", input$plot_dblclick$y))
  #   #   
  #   #   dataset <- datasetInput_int() %>%
  #   #     mutate(temp_normF1 = (F1.50. - mean(F1.50.))/sd(F1.50.),
  #   #            temp_normF2 = (F2.50. - mean(F2.50.))/sd(F2.50.))
  #   #   
  #   #   means <- dataset %>%
  #   #     group_by(vowel) %>%
  #   #     summarise(mean_F1 = mean(F1.50.),
  #   #               mean_F2 = mean(F2.50.),
  #   #               mean_F1_norm = mean(temp_normF1),
  #   #               mean_F2_norm = mean(temp_normF2))
  #   #   
  #   #   
  #   #   x_norm <- (x - mean(dataset$F2.50.)) / sd(dataset$F2.50.)
  #   #   y_norm <- (y - mean(dataset$F1.50.)) / sd(dataset$F1.50.)
  #   #    
  #   #   means <- means %>%
  #   #     mutate(dist = sqrt((x_norm - mean_F2_norm)^2 +
  #   #                          (y_norm - mean_F1_norm)^2)) %>%
  #   #     arrange(dist)
  #   #   
  #   #   head(means, n=5)
  #   # 
  #   #   
  #   # } else {
  #   #   "Double click two vowels to measure their overlap."
  #   # }
  #   
  # })
  
  # ________ Speaker Stats -------------------------------------------------------------
  
  output$speaker_stats_left <- renderTable({
    dataset <- datasetInput_left()
    
    #dataset <- subset(NSF, speaker %in% c("100", "40", "434"))
    #summary(dataset)
    
    # Just keep some metadata columns
    speaker_summary <- unique(subset(dataset,
                                     select=c(speaker, birth_year, sex, ethnicity, state)))
    
    # Summary information
    speaker_counts <- dataset %>%
      group_by(speaker) %>%
      summarize(observations=n())
    # alt approach: as.data.frame(table(dataset$speaker))
    
    # Merge with metadata
    speaker_merged <- merge(speaker_summary, speaker_counts, by.x="speaker", by.y="speaker")
    
    # Put it in the correct order (https://stackoverflow.com/questions/29381069/implicit-sorting-in-tidyrspread-and-dplyrsummarise)
    speaker_merged <- speaker_merged %>% arrange(speaker)
  })
  
  output$speaker_stats_right <- renderTable({
    dataset <- datasetInput_right()
    
    # Just keep some metadata columns
    speaker_summary <- unique(subset(dataset,
                                     select=c(speaker, birth_year, sex, ethnicity, state)))
    
    # Summary information
    speaker_counts <- dataset %>% group_by(speaker) %>% summarize(observations=n())
    # alt approach: as.data.frame(table(dataset$speaker))
    
    # Merge with metadata
    speaker_merged <- merge(speaker_summary, speaker_counts, by.x="speaker", by.y="speaker")
    
    # Put it in the right order
    speaker_merged <- speaker_merged %>% arrange(speaker)
  })
  
  output$speaker_stats_grid <- renderTable({
    dataset <- datasetInput_grid()
    
    # Just keep some metadata columns
    speaker_summary <- unique(subset(dataset, select=c(speaker, birth_year, sex, ethnicity, state)))
    
    # Summary information
    speaker_counts <- dataset %>% group_by(speaker) %>% summarize(observations=n())
    # alt approach: as.data.frame(table(dataset$speaker))
    
    # Merge with metadata
    speaker_merged <- merge(speaker_summary, speaker_counts, by.x="speaker", by.y="speaker")
    
    # Put it in the grid order
    speaker_merged <- speaker_merged %>% arrange(speaker)
  })
  
  # ________ Cell Summary -------------------------------------------------------------
  
  output$cell_summary <- DT::renderDataTable({
    cells <- datasetCells()
    cells <- cells %>%
      select(-F1_mid, -F2_mid) %>%
      arrange(-n)
    
    DT::datatable(cells,
                  fillContainer = F,
                  autoHideNavigation = T)
  })
  
  
  # ________ Vowel Stats -------------------------------------------------------------
  
  output$vowel_stats_left <- renderTable({
    dataset <- datasetInput_left()
    
    # summary information
    vowel_summary <- summarise(group_by(dataset, vowel),
                               "F1 mean" = mean(F1.50.),
                               "F2 mean" = mean(F2.50.),
                               "F1 stdev" = sd(F1.50.),
                               "F2 stdev" = sd(F2.50.))
    
    # vowel summary information
    vowel_counts <- dataset %>% group_by(vowel) %>% summarize(observations=n())
    
    # merge the data
    vowel_merged <- merge(vowel_summary, vowel_counts, by="vowel")
    vowel_merged <- vowel_merged %>% arrange(vowel)
  })
  
  output$vowel_stats_right <- renderTable({
    dataset <- datasetInput_right()
    
    # summary information
    vowel_summary <- summarise(group_by(dataset, vowel),
                               "F1 mean" = mean(F1.50.),
                               "F2 mean" = mean(F2.50.),
                               "F1 stdev" = sd(F1.50.),
                               "F2 stdev" = sd(F2.50.))
    
    # vowel summary information
    vowel_counts <- dataset %>% group_by(vowel) %>% summarize(observations=n())
    
    # merge the data
    vowel_merged <- merge(vowel_summary, vowel_counts, by="vowel")
    
    vowel_merged <- vowel_merged %>% arrange(vowel)
  })
  
  output$vowel_stats_grid <- renderTable({
    dataset <- datasetInput_grid()
    
    # summary information
    vowel_summary <- summarise(group_by(dataset, vowel),
                               "F1 mean" = mean(F1.50.),
                               "F2 mean" = mean(F2.50.),
                               "F1 stdev" = sd(F1.50.),
                               "F2 stdev" = sd(F2.50.))
    
    # vowel summary information
    vowel_counts <- dataset %>% group_by(vowel) %>% summarize(observations=n())
    
    # merge the data
    vowel_merged <- merge(vowel_summary, vowel_counts, by="vowel")
    
    vowel_merged <- vowel_merged %>% arrange(vowel)
  })
  
  
  # ____ Maps -------------------------------------------------------------
  
  output$map <- renderPlot({
    
    # Make a copy so that "map" contains the original still.
    this_map <- map
    
    # Show speaker numbers?
    if(input$speaker_nums_map == TRUE) {
      this_map <- map + geom_text(data=metadata,
                                  aes(x=Longitude, y=Latitude, label=speaker),
                                  nudge_x = 0, nudge_y = -0.25)
    }
    
    # Based on the input, get the column in the data frame.
    # This is mostly to go from spaces to underscores.
    if (input$map_shape == "ethnicity") {
      shape <- "ethnicity"
    } else if (input$map_shape == "sex") {
      shape <- "sex"
    } else if (input$map_shape == "education level") {
      shape <- "education_level"
    } else if (input$map_shape == "social class") {
      shape <- "social_class"
    } else if (input$map_shape == "land region") {
      shape <- "land_region"
      
      # If no shape is selected, make the shape "dummy" and remove the shape portion of the plot's legend.
    } else {
      shape <- "dummy"
      this_map <- this_map +
        guides(shape=FALSE)
    }
    
    # Same as above, convert from spaces to underscores.
    # This time, specific colors are sometimes indicated.
    if (input$map_color == "birth year") {
      colour = "birth_year"
      this_map <- this_map +
        scale_color_gradient2(midpoint = 1930, low="yellow", mid="green", high="blue")
      
    } else if (input$map_color == "ethnicity") {
      colour = "ethnicity"
      this_map <- this_map +
        scale_colour_manual(values=c("darkorange2", "dodgerblue3"))
      
    } else if (input$map_color == "sex") {
      colour = "sex"
      this_map <- this_map +
        scale_colour_manual(values=c("darkmagenta", "goldenrod2"))
      
    } else if (input$map_color == "sector") {
      colour = "sector_name"
      
    } else if (input$map_color == "education level") {
      colour <- "education_level"
    } else if (input$map_color == "social class") {
      colour <- "social_class"
    } else if (input$map_color == "classification") {
      colour <- "classification"
    } else if (input$map_color == "land region") {
      colour <- "land_region"
      
      # If no color is selected, turn that portion of the legend off.
    } else {
      colour <- "dummy"
      this_map <- this_map +
        scale_colour_manual(values=c("black")) +
        guides(colour=FALSE)
    }
    
    # Create the map using the above information.
    this_map <- this_map +
      geom_point(data=metadata,
                 aes_string(x = "Longitude", y = "Latitude", color=colour, shape=shape),
                 size=5)
    
    this_map
    
  })
  
  
  # ____ SpeakerTable  -------------------------------------------------------------------
  
  output$speaker_table <- DT::renderDataTable(DT::datatable({ 
    
    metadata %>%
      
      # Rename them to prettier names
      rename(sector = sector_name,
             education = education_level,
             `social class` = social_class,
             `Kurath type` = kurath_type,
             `land region` = land_region,
             `interview year` = interview_year,
             `birth year` = birth_year) %>%
      
      # Put the speakers in the right order.
      mutate(speaker = factor(speaker, levels=c("025","027","030","040","079","100","105","117","165","166","176","185","252", 
                                                "255","270","289","299","303","312","330","342","364","370B","387","412","434", 
                                                "444","446","456","461","464","472","490","494","503","505","533","543","548", 
                                                "556","579","595","596","604","625","647","657X","662","678","703","741","748", 
                                                "779","791","794","811","847","853","863","888","893","894","911"))) %>%
      
      # Reorder
      select(speaker, 
             sex, ethnicity,`birth year`, `interview year`, age, 
             state, sector, county, town, `land region`, locality,
             education, `social class`, classification, `Kurath type`,
             vowels)
    
  }))
  
  
  
  
  # ____ Show/Hide Help Buttons -------------------------------------------------------------
  
  # Offload some of this stuff to other scripts.
  source("script_chunks/hide_help_button.R", local = TRUE)
  source("script_chunks/show_help_button.R", local = TRUE)
  
  
}



# Run the app --------------------
shinyApp(ui = ui, server = server)


# names(NSF)
# table(NSF$speaker)
# NSF %>%
#   filter(speaker == "025", ARPABET == "IY") %>%
#   print() %>%
#   ggplot(aes(x = 50, y = F1.50.)) + 
#   geom_point(alpha = 0.005)
