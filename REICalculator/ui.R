#
# Sets up webpage for Response Entropy Index Calculator.
# Created by Sage Mahannah
# Last Updated 4/26/2021
# Contact sage.mahannah@gmail.com for questions, comments or concerns.
#

library(shiny)
library(foreign)
library(readxl)
library(stringr)
library(markdown)
library(condformat)
library(dplyr)
library(R.utils)

# Define UI for REI Calculator
shinyUI(
    fluidPage(
        
        # App title ----
        navbarPage("Response Entropy Index Calculator",
                   tabPanel("Calculate!",
                            
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(
                                    
                                    # Input: Specifying names of columns to calculate ----
                                    textInput("variables", "Variable Name"),
                                    
                                    # Input: Checkbox if file has header ----
                                    checkboxInput("header", "Header", TRUE),
                                    
                                    tags$p("If checked, reads the first row as variable names. If not, calculates REI using all columns."),
                                    
                                    # Input: Select file type ----
                                    radioButtons("type", "Type of File",
                                                 choices = c(CSV = ".csv",
                                                             SPSS = ".sav",
                                                             Excel = ".xls"),
                                                 selected = ".csv"),
                                    
                                    # Input: Select a file ----
                                    fileInput("file1", "Choose File",
                                              multiple = FALSE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv", ".sav", ".xls", ".xlsx")),
                                    
                                    # Input: Select coloring options 
                                    #selectInput('displaySettings', 'Display Options', c("Solid Colors", "Gradient", "None"), selectize=TRUE),
                                    
                                    tags$p(tags$strong("Note!"), "Please wait up to 30 seconds for results to be calculated."),
                                    # Download the output
                                    downloadButton("downloadData", "Download")
                                ),
                                
                                # Main panel for displaying outputs ----
                                mainPanel(
                                    
                                    # Output: Data file ----
                                    textOutput("message"),
                                    #condformatOutput("contents")
                                    DT::dataTableOutput("contents")
                                    
                                ))), 
                   tabPanel("Directions",
                            tags$h1("Directions"),
                            tags$p(tags$strong("Step One: "),"Enter in the variable names of the columns you would like to consider for your calculations, separated by commas. Variable names are case sensitive. To include all variables starting or ending with a phrase, simply put a dash after or before the phrase accordingly. For example, type 'DEP-' to include columns starting with DEP, type '-r' to include all columns ending with r, or type '-var-' to include all columns with names containing the phrase var. To include all columns, simply include only a dash. At least 2 columns must be found under the search parameters in order to calculate the REI. If your dataset does not include variable names, uncheck the header box. This will automatically calculate the REI using all columns."),
                            tags$p(tags$strong("Step Two: "),"Select the file type for the dataset."),
                            tags$p(tags$strong("Step Three: "),"Select the file to be considered."),
                            tags$p(tags$strong("Step Four: "),"Review your output. REI scores that are both too high and too low",tags$em(" could "), "indicate careless responding.  Row numbers with REI scores in the lower 5th percentile and upper 95th percentile of your sample are flagged in red. Note that should a row contain any missing values, that row's REI will return as 0. Raw data for these rows should be examined manually before a decision is made to remove the data. "),
                            tags$p(tags$strong("Step Five (Optional): "),"Download a datafile with REI values.")
                   ),
                   tabPanel("About", 
                            tags$h1("About"),
                            tags$p("The response entropy index (REI) calculates the balance of proportions of values endorsed by participants on Likert-scaled surveys, and is a method for flagging careless response patterns.  Entropy is the idea that over time a system will move towards disorder as a natural state.  With time, a ceramic cup will chip, crack, and eventually break down into its raw materials.  In measurement, entropy quantifies where a system falls on a continuum of absolute order to absolute disorder.  In this application, entropy is used to determine the degree of disorder in a person’s response set to a set of survey questions.  A purposeful response set is typically neither too ordered nor too disordered.  Consider a person who is moderately to severely depressed; we would reasonably expect this person to respond on a depression inventory with primarily with 5’s, and with some 4’s and 6’s.  A computed entropy score ranges between 0.0 and 1.0, with a maximal entropy score being reached when a person’s responses are as diffused across item options as possible.  In the case of a 20 item scale, each with five response options, a maximal entropy score would be obtained if a person endorsed four “1’s,” four “2’s,” four “3’s,” four “4’s,” and four “5’s.”  A minimal entropy score would be obtained if a person endorsed twenty “1’s” or any other single number.  Thus, high response entropy scores could indicate a person who has either scattered random responses across survey items or constrained responses to a single response type, and in either case, could indicate careless responding. "),
                            tags$p("I have completed two studies (currently under review) examining the psychometric properties of the REI.  In the first study, performance of the REI was compared to other commonly used post-hoc indices for detecting careless responding (CR) such as the Mahalanobis distance (MD) and psychometric synonym index (PSI).  Three different types of Bogus Scores (BS) were generated: 1) uniform random values produced by computer (n = 100); 2) normally distributed random values produced by computer (n = 100); and 3) purposefully careless responses produced by human participants (n = 100).  The BS data were then implanted in a true, cleaned social science dataset (n = 500).  Multinomial logistic regression determined that the REI made independent contributions from other indices to the prediction of BS.  Latent variable analyses suggest that the variability-based REI may be tapping distinct constructs from regression-based indices such as the MD and PSI.  In the second study, potential cultural bias in CR indices was examined with a true social science dataset (n = 302) comprised of racially diverse participants.  Unlike other post-hoc measures of CR, the REI was unrelated to participant race.  Further analyses demonstrated that racial differences in other indices of CR could be accounted for by culturally different styles of survey responding.  For example, Asian American participants’ higher MD scores relative to White participants’ was mediated by a culturally specific acquiescent survey response style. ")
                   ),
                   tabPanel("Code",
                            tags$h1("Code"),
                            tags$p("The code for this calculator can be downloaded here:"),
                            tags$a(href = "https://drive.google.com/drive/folders/1OzRQXO_PNXfV7-i6THn9DZF0QStpgakA?usp=sharing", "Google Drive for REI Calculator Code"),
                            tags$br(),
                            tags$a(href = "https://github.com/SageNM/REICalculator", "Git Repo for REI Calculator Code")
                   )
                   
        )
)
)
