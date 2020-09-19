#
# Uses user inputs to calculate Response Entropy Index for dataset.
# Created by Sage Mahannah
# Last Updated 8/22/2020
# Contact sage.mahannah@gmail.com for questions, comments or concerns.
#

library(shiny)
library(foreign)
library(readxl)
library(stringr)
library(markdown)
library(condformat) # only necessary for coloring options if desired
library(DT)
library(dplyr)
library(R.utils)

# Define server logic for REI calculator
shinyServer(function(input, output) {

    value <- reactiveVal("Please input a file!")  
    
    calculations <- reactive({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        req(input$file1)
        
        # parses file based on file type
        tryCatch(
            {
                if (input$type == ".csv") {
                    df <- read.csv(input$file1$datapath,
                                   header = input$header)
                } else if (input$type == ".sav") {
                    df <- read.spss(input$file1$datapath, use.value.labels = input$header, to.data.frame=TRUE)
                } else {
                    df <- read_excel(input$file1$datapath, col_names = input$header)
                }
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                value("Wrong file type!")
                stop(safeError(e))
            }
        )
        
        # checking that there are variables entered if there is a header
        if (input$variables == "" && input$header == TRUE) {
            value("Please input variables to consider!")
            return(NULL)
        }
        
        # parsing variables entered into a usable list for searching
        iniVariables <- str_split(input$variables, ",")
        print(iniVariables)
        variableNames <- unique(iniVariables[[1]])
        variableNames <- as.array(variableNames)
        variableNames <- sapply(variableNames, trimws)
        variableNames <- unique(variableNames)
        
        # test to make sure names are parsed correctly
        print(variableNames)
        
        # cycling through all the listed variables to find any/all matching column
        df$REI <- 0
        testDF <- df$REI
        df <- subset(df, select = -REI)
        
        # using all columns if not using a header
        if (input$header == FALSE) {
            testDF <- cbind(testDF, df)
        } 
        else { 
            for (i in variableNames) {
                # used for tracking matches
                columnMatches <- NULL
                
                # checking if a variable starts with and/or ends with "-"
                if (startsWith(i, "-") && endsWith(i, "-")) {
                    print("variable starts and ends with -")
                    columnMatches <- grep(trimws(str_remove_all(i, "-")), colnames(df))
                } else if (startsWith(i, "-")) {
                    print("variable starts with -")
                    columnMatches <- grep(paste(trimws(str_remove(i, "-")), "$", sep=""), colnames(df))
                } else if (endsWith(i, "-")) {
                    print("variable ends with -")
                    columnMatches <- grep(paste("^", trimws(str_remove(i, "-")), sep=""), colnames(df))
                } else {
                    print("exact phrase")
                    columnMatches <- grep(paste("^",i,"$", sep=""), colnames(df))
                }
                
                # adding matching columns
                if (length(columnMatches)>0) {
                    testDF <- cbind(testDF, df[, columnMatches])
                    # rename last column if only one column is added
                    if (length(columnMatches)==1) {
                        colnames(testDF)[ncol(testDF)] <- colnames(df)[columnMatches]
                    }
                }
            }
        }
        
        # checking that more than one column is considered in total
        if (NCOL(testDF) == 1) {
            value("No columns found with that phrase.")
            stop("Unable to find column with name.")
        } else if (NCOL(testDF) == 2) {
            value("Please input more than 1 column.")
            stop("Only a single column was found with those search parameters.")
        }
        
        # renaming the first column to be the REI
        colnames(testDF)[1] <- "REI"
        
        # testing to see if the combination worked
        print("Here are all the column names after combining")
        print(colnames(testDF))
        
        # calculating the number of questions asked and range for Likert scale
        numLevels <- input$range[2] - input$range[1] + 1
        numQuestions <- ncol(testDF) - 1
        
        # finding all possible responses for questions, to make tallies of each response accordingly
        # because this is used, the range of the Likert scale does not matter
        getResponses <-function(df) {
            recordedResponses <- unique(as.vector(as.matrix(df)))
            print(recordedResponses)
            tallies <- sapply(recordedResponses,function(x)rowSums(df==x))
            return(tallies)
        }
        
        # attempting to go through all responses to find unique responses
        tryCatch({
            res <- withTimeout({
                tallies <- getResponses(testDF[,-1])
            }, timeout=30)
        }, TimeoutException=function(ex) {
            value("Calculations have taken longer than 30 seconds. Timeout has occured.")
            stop("Timeout after 30 seconds.")
        })
        
        ### IF NEED TO FIND A WAY TO INCLUDE MISSING RESPONSES 
        # that is to say, responses included on the Likert scale but not chosen by any participant
        # this has no impact on the math, which is why it is not included
        # if(ncol(tallies) < numLevels) {
        #  missingResponses <- data.frame(matrix(0, ncol = numLevels - ncol(tallies), nrow = nrow(tallies)))
        #  tallies <- cbind(tallies, missingResponses)
        # }
        
        # remembering each specific response
        if (NCOL(tallies)==1) {
            originalNames <- c("OnlyASingleResponse")
        } else {
            originalNames <- colnames(tallies, do.NULL=FALSE)
        }
        
        # printing out the names of all columns used to calculate the REI
        value(paste("Columns used: ",toString(colnames(testDF)[-1])))
        
        # renaming columns for tallies so they include the response in the name
        if (NCOL(tallies) > 1) {
            colnames(tallies) <- paste("Tally", originalNames, sep = "_")
        }
        
        # calculating the proportion of each response to the number of questions asked for each subject
        proportions <- tallies/numQuestions
        
        if (NCOL(proportions) > 1) {
            colnames(proportions) <- paste("Proportion", originalNames, sep = "_")
        }
        
        # calculating the log of the proportions
        logs <- proportions*log10(proportions)
        logs[is.na(logs)] <- 0
        
        # adding the tallies and proportions to the data frame with the REI and columns to consider
        testDF <- cbind(testDF, tallies)
        testDF <- cbind(testDF, proportions)
        
        # calculating the REI by summing the log of the proportions and multiplying by -1
        testDF[, "REI"] <- rowSums(logs)
        testDF[, "REI"] <- testDF[, "REI"] * -1
        
        # getting final table
        final <- as.data.frame(subset(testDF, select = REI))
        
        # calculating percentile for REI and flagging
        final$Percentile <- round(pnorm(final$REI, mean = mean(final$REI), sd = sd(final$REI)), digits = 2) * 100
        print(final)
        
        # adjust specific values to change what is being flagged
        final$Suspicious <- "No"
        final$Suspicious[final$Percentile <= 10 | final$Percentile >= 90] <- "Maybe"
        final$Suspicious[final$Percentile <= 5 | final$Percentile >= 95] <- "Yes"
        
        # Uncomment to include multiple coloring options for final results
        # if(input$displaySettings == "Gradient") {
        # final <- condformat(final) %>% rule_fill_gradient2(REI, expression = pnorm(REI, mean = mean(final$REI), sd = sd(final$REI)), low = "red", mid = "white", high = "red", midpoint = mean(final$REI))
        #} else if (input$displaySettings == "Solid Colors") {
        #final <- condformat(final) %>% rule_fill_discrete(REI, expression = Suspicious, colours = c("Yes" = "red", "Maybe" = "orange", "No" = "white"))
        #} else {
        #  final <- condformat(final)
        #}
        #final$Percentile <- 0
        #input$range$value
        
        # coloring rows based on suspiciousness 
        final <- datatable(final) %>% formatStyle('Suspicious', target = 'row', backgroundColor = styleEqual(c('Yes', 'Maybe'), c('#FF3333', '#FFFF33')))
        
        return(final)
    })
    
    # final output
    output$message <- renderText({value()})
    output$contents <- DT::renderDataTable({calculations()}, 
                                           options = list(lengthMenu = list(c(50, 100, 200, -1), c('50', '100', '200', 'All')),
                                                          pageLength = 100))
    
    # download a file with the REI calculations
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("REICalculations-", Sys.Date(), ".csv", sep="")
        }, content = function(file) {
            write.csv(calculations(), file)
        }
    )

})
