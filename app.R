# References: ----
    # https://deanattali.com/2015/06/14/mimicking-google-form-shiny/

# load packages ----
    library(shiny)
    library(dplyr)

    # DEFINE GENERAL INFO ----
        fieldsMandatory <- c('agency_code', "agency_name")
        
        labelMandatory <- function(label) {
            tagList(
                label,
                span("*", class = "mandatory_star")
            )
        }
        
        fieldsAll <- c("agency_name", "facility_types", "population", "contact_person", "contact_title")
        responsesDir <- file.path("responses//individual_responses")
        # epochTime <- function() {
        #     as.integer(Sys.time())
        # }
        
        appCSS <- ".mandatory_star { color: red; }"
        
        humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
        
        # Get Facility Info (to be used in populating fields)
            dummy.data <- readr::read_csv('data//Dummy_Data.csv')
            agencies.list <- c('', sort(dummy.data$agency_name))
            # facility_agency <- dummy.data %>% distinct(facility_name, agency_name) %>% arrange(agency_name)
            
        # Get Previous Responses
            previous.responses <- readr::read_csv("responses//_Master.csv")
            
        # get codes
            codes <- readr::read_csv("data//Codes.csv")
            codes <- codes %>% arrange(agency_name)


shinyApp(
    # UI !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ----
    ui = fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        # HEADER ----
            titlePanel("Climate Change Readiness - Questions for Wastewater Collection, Conveyance, and/or Treatment Agencies"),
            hr(style="border: 2px solid black"),
        
        # AGENCY CODE ENTRY ----
            div(
                id = 'agency_code_entry',
                p(tags$b('Enter agency code (recieved via email):')),
                div(
                    textInput(inputId = 'agency_code', 
                              label = NULL, #labelMandatory('Enter agency code (recieved via email):'), 
                              value = '', 
                              placeholder = NA),
                    style="display: inline-block;vertical-align:top;"
                ),
                div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                div(
                    actionButton("show_codes", "Show Agency Codes", class = "btn-primary"), 
                    style="display: inline-block;vertical-align:top;"
                ),
                div(actionButton("submit_code", "Submit Code", class = "btn-primary")
                    ),
            ),
        
        # SELECTED AGENCY
            shinyjs::hidden(
                div(
                    id = 'selected_agency',
                    HTML("<br>"),
                    tags$b('Agency Name: '),
                    p(uiOutput('agencySelect'))
                )
            ),
        
        # AGENCY CODE VALIDATION
            shinyjs::hidden(
                div(
                    id = 'agency_code_validation',
                    p('If this is the correct agency, press the "Continue" button below. Otherwise, enter a different agency code or contact the program administrator. '),
                    div(actionButton("verify_code", "Continue", class = "btn-primary"))
                    )
                ),
        
        # AGENCY SELECTED INFO
            shinyjs::hidden(
                div(
                    id = 'agency_selected_info',
                    h3('Submitting Agency:'),
                    # HTML("<br>"),
                    div(style="display: inline-block", tags$b('Agency Code: ')),
                    div(style="display: inline-block", textOutput('output_agency_code')),
                    div(),
                    div(style="display: inline-block", tags$b('Agency Name: ')),
                    div(style="display: inline-block", textOutput('output_agency_name'))
                )
            ),
               
        # DUPLICATES MESSAGE ----
            div(),
            shinyjs::hidden(
                div(id = 'form_duplicates',
                    tags$br(),
                    hr(style="border: 2px solid black"),
                    textInput(inputId = 'duplicate_reason', 
                              label = p(span('Please describe the reason for entering a new response for this agency (e.g., correcting a particular question, previous response submitted accidentally, etc.):', 
                                             style = "color:red")), 
                              placeholder = 'Enter reason here...', 
                              value = NA, 
                              width = 1000),
                    # radioButtons(inputId = 'overwrite_duplicate', 
                    #              label = p(span('Would you like to overwrite your agency\'s previous response?', style = "color:red")), 
                    #              choices = c('No', 'Yes'),
                    #              selected = 'No',
                    #              inline = TRUE)
                    hr(style="border: 0.5px solid grey"),
                    p('The answers below are from your agency\'s previous response to this survey. If you would like to change any of your previous responses, please edit the relevant question(s) and click the "Submit" button (the most recent response will supercede any previous responses).',
                      style = "color:red")
                )),
        
        # AGENCY INFO ----
            shinyjs::hidden(
                div(id = 'agency_populated_info',
                    hr(style="border: 2px solid black"),
                    h3('Agency Information:'),
                    # User validated fields
                        p('Note: Fields below are auto-populated based on existing information for your agency. Edit these fields as needed.', 
                          style = "color:grey"),
                        div(uiOutput('facilityTypes')),
                        p(tags$b("Approximate population receiving wastewater service from your agency:")),
                        div(uiOutput('approxPop')),
                        # error message if value is not numeric
                        shinyjs::hidden(
                            div(id = 'pop_error_msg',
                                p('Please enter a valid numeric value above (must be between 0-30,000,000)', style = "color:red")
                            )
                        ),
                        div(),
                        div(uiOutput('contactPersonSelection'),style="display: inline-block"),
                        div(style="display: inline-block;vertical-align:top; width: 15px;",HTML("<br>")), # add some space
                        div(uiOutput('contactTitleSelection'),style="display: inline-block"),
                        div(style="display: inline-block;vertical-align:top; width: 15px;",HTML("<br>")), # add some space
                        div(uiOutput('contactPhoneSelection'),style="display: inline-block"),
                        div(style="display: inline-block;vertical-align:top; width: 15px;",HTML("<br>")), # add some space
                        div(uiOutput('contactEmailSelection'),style="display: inline-block"),
                        # dateInput(inputId = 'date_submitted', label = 'Date:', value = Sys.Date()),
                        # uiOutput('dateSelection'),
                        div(actionButton("verify_agency_info", "Continue", class = "btn-primary"))
                )
            ),
        
        # QUESTIONS ----
            shinyjs::hidden(
                div(
                    id = "form", 
                    hr(style="border: 2px solid black"),
                    div(actionButton("go_back_to_agency_info", "Go Back to Agency Information", class = "btn-primary")),
                    h3('Questions:'), 
                    # QUESTION 1 ----
                        div(uiOutput('question_1')),#style="display: inline-block;vertical-align:top;"),
                        div(),
                        # radioButtons(inputId = 'question_1', 
                        #              label = '1. Has your agency conducted facility or infrastructure assessment(s), or prepared an asset 
                        #              management plan that includes new or increased threats from climate change or future extreme weather 
                        #              events (e.g., sea level rise, storm surge, high intensity precipitation, flooding, drought, or 
                        #              extreme heat)?', 
                        #              choices = c('No', 'Yes', 'Work is underway'), 
                        #              width = 1300,
                        #              inline = TRUE),
                        # 1a
                            div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                            div(style="display: inline-block;vertical-align:top;",
                                # radioButtons(inputId = 'question_1a',
                                #              label = '1a. Is there a local or regional assessment or plan (e.g., Climate Action Plan,
                                #              General Plan, Integrated Regional Water Management Plan) that addresses new or increased
                                #              threats to infrastructure resulting from climate change, and includes your system?',
                                #              choices = c('No', 'Yes'),
                                #              width = 1250,
                                #              inline = TRUE)
                                uiOutput('question_1a')
                            ),
                            div(),
                        # 1b
                            div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                            div(style="display: inline-block;vertical-align:top;",
                                # checkboxGroupInput(inputId = 'question_1b',
                                #                    label = '1b. Which components were covered by the assessment/planning? (Check all that apply)',
                                #                    choices = c('Collection System', 'Interceptors', 'Treatment Facilities',
                                #                                'Disinfection', 'Discharge facilities/outfalls', 'Facility Access',
                                #                                'Pump stations', 'Wet weather facilities', 'Power source / Biogas / Cogeneration',
                                #                                'Telecommunications'), width = 850,
                                #                    inline = TRUE)
                                uiOutput('question_1b')
                            ),
                            div(),
                    hr(style="border: 1px solid darkgrey"),
                    
                    # QUESTION 2 ----
                        uiOutput('question_2'),
                        # radioButtons(inputId = 'question_2', 
                        #              label = '2. Are results of the assessment/planning available to the public?', 
                        #              choices = c('No', 'Yes'), 
                        #              inline = TRUE),
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # textInput(inputId = 'question_2text', 
                            #           label = 'If yes, please provide the website, or where the document(s) can be found:', 
                            #           width = 1000, 
                            #           placeholder = "Enter location here...", 
                            #           value = NA)
                            uiOutput('question_2text')
                        ),
                        div(),
                        hr(style="border: 1px solid darkgrey"),
                    
                    # QUESTION 3 ----
                        p(tags$b('3. Select status of  measures your agency is implementing to address new or increased threats to 
                             infrastructure. For measures already in place, indicate the year of completion. For in-progress and 
                             planned measures, indicate the expected year of completion. For multiple facilities with different 
                             status of implementation, please provide notes in item 5.')),
                        # Question 3-1 ----
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # radioButtons(inputId = 'question_3_1', 
                            #              label = 'Expanding capacity:', 
                            #              choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'), 
                            #              inline = TRUE)
                            uiOutput('question_3_1')
                        ),
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # numericInput(inputId = 'question_3_1_year',
                            #              label = 'Completion Year:', min = 1900, max = lubridate::year(Sys.Date()), 
                            #              step = 1, 
                            #              value = NA)
                            uiOutput('question_3_1_year')
                        ),
                        div(),
                        hr(),
                        # Question 3-2 ----
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # radioButtons(inputId = 'question_3_2', 
                            #              label = 'Increasing maintenance or rehabilitation frequency:', 
                            #              choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'), 
                            #              inline = TRUE)
                            uiOutput('question_3_2')
                        ),
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # numericInput(inputId = 'question_3_2_year',
                            #              label = 'Completion Year:', 
                            #              min = 1900, max = lubridate::year(Sys.Date()), 
                            #              step = 1, 
                            #              value = NA)
                            uiOutput('question_3_2_year')
                        ),
                        div(),
                        hr(),
                        # Question 3-3 ----
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # radioButtons(inputId = 'question_3_3', 
                            #              label = 'Modifying treatment capability:', 
                            #              choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'), 
                            #              inline = TRUE)
                            uiOutput('question_3_3')
                        ),
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # numericInput(inputId = 'question_3_3_year',
                            #              label = 'Completion Year:', 
                            #              min = 1900, max = lubridate::year(Sys.Date()), 
                            #              step = 1, 
                            #              value = NA)
                            uiOutput('question_3_3_year')
                        ),
                        div(),
                        hr(),
                        # Question 3-4 ----
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # radioButtons(inputId = 'question_3_4', 
                            #              label = 'Hardening facilities (e.g., installing, increasing, or improving barriers, buffers, or 
                            #          levees; elevating or floodproofing equipment; or sealing doors, sewer mains, or manholes):', 
                            #              choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'), 
                            #              inline = TRUE)
                            uiOutput('question_3_4')
                        ),
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # numericInput(inputId = 'question_3_4_year',
                            #              label = 'Completion Year:', 
                            #              min = 1900, max = lubridate::year(Sys.Date()), 
                            #              step = 1, 
                            #              value = NA)
                            uiOutput('question_3_4_year')
                        ),
                        div(),
                        hr(),
                        # Question 3-5 ----
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # radioButtons(inputId = 'question_3_5', 
                            #              label = 'Improving, upgrading, or relocating electrical components/instrumentation:', 
                            #              choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'), 
                            #              inline = TRUE)
                            uiOutput('question_3_5')
                        ),
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # numericInput(inputId = 'question_3_5_year',
                            #              label = 'Completion Year:', 
                            #              min = 1900, max = lubridate::year(Sys.Date()), 
                            #              step = 1, 
                            #              value = NA)
                            uiOutput('question_3_5_year')
                        ),
                        div(),
                        hr(),
                        # Question 3-6 ----
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # radioButtons(inputId = 'question_3_6', 
                            #              label = 'Updating maintenance procedures:', 
                            #              choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'), 
                            #              inline = TRUE)
                            uiOutput('question_3_6')
                        ),
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # numericInput(inputId = 'question_3_6_year',
                            #              label = 'Completion Year:', 
                            #              min = 1900, max = lubridate::year(Sys.Date()), 
                            #              step = 1, 
                            #              value = NA)
                            uiOutput('question_3_6_year')
                        ),
                        div(),
                        hr(),
                        # Question 3-7 -----
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # radioButtons(inputId = 'question_3_7', 
                            #              label = 'Updating emergency response and recovery procedures:', 
                            #              choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'), 
                            #              inline = TRUE)
                            uiOutput('question_3_7')
                        ),
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # numericInput(inputId = 'question_3_7_year',
                            #              label = 'Completion Year:', 
                            #              min = 1900, max = lubridate::year(Sys.Date()), 
                            #              step = 1, 
                            #              value = NA)
                            uiOutput('question_3_7_year')
                        ),
                        div(),
                        hr(),
                        # Question 3-8 ----
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # radioButtons(inputId = 'question_3_8', 
                            #              label = 'Securing a backup power supply or contracts for an alternative power supply:', 
                            #              choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'), 
                            #              inline = TRUE)
                            uiOutput('question_3_8')
                        ),
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # numericInput(inputId = 'question_3_8_year',
                            #              label = 'Completion Year:', 
                            #              min = 1900, max = lubridate::year(Sys.Date()), 
                            #              step = 1, 
                            #              value = NA)
                            uiOutput('question_3_8_year')
                        ),
                        div(),
                        hr(),
                        # Question 3-9 ----
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # radioButtons(inputId = 'question_3_9', 
                            #              label = 'Relocating facilities:', 
                            #              choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'), 
                            #              inline = TRUE)
                            uiOutput('question_3_9')
                        ),
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # numericInput(inputId = 'question_3_9_year',
                            #              label = 'Completion Year:', 
                            #              min = 1900, max = lubridate::year(Sys.Date()), 
                            #              step = 1, 
                            #              value = NA)
                            uiOutput('question_3_9_year')
                        ),
                        div(),
                        hr(),
                        # Question 3-10 ----
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # radioButtons(inputId = 'question_3_10', 
                            #              label = 'Constructing or installing redundant facilities:', 
                            #              choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'), 
                            #              inline = TRUE)
                            uiOutput('question_3_10')
                        ),
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # numericInput(inputId = 'question_3_10_year',
                            #              label = 'Completion Year:', 
                            #              min = 1900, max = lubridate::year(Sys.Date()), 
                            #              step = 1, 
                            #              value = NA)
                            uiOutput('question_3_10_year')
                        ),
                        div(),
                        hr(),
                        # Question 3-11 ----
                        div(style = "display: inline-block;vertical-align:top; width: 25px;", HTML("<br>")), # add some space
                        div(style = "display: inline-block;",
                            # radioButtons(
                            #     inputId = 'question_3_11',
                            #     label = 'Other (please describe):',
                            #     choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                            #     inline = TRUE)
                            uiOutput('question_3_11')
                        ),
                        div(style = "display: inline-block;vertical-align:top; width: 25px;", HTML("<br>")), # add some space
                        div(style = "display: inline-block;",
                            # numericInput(
                            #     inputId = 'question_3_11_year',
                            #     label = 'Completion Year:',
                            #     min = 1900,
                            #     max = lubridate::year(Sys.Date()),
                            #     step = 1,
                            #     value = NA)
                            uiOutput('question_3_11_year')
                        ),
                        div(style = "display: inline-block;vertical-align:top; width: 25px;", HTML("<br>")), # add some space
                        div(style = "display: inline-block;", 
                            # textInput(inputId = 'question_3_11_text', 
                            #           label = "Description:", 
                            #           placeholder = 'Describe the measure...', 
                            #           width = 500, 
                            #           value = NA)
                            # #resize = 'both')
                            uiOutput('question_3_11_text')
                        ),
                        div(),
                        hr(),
                        # Question 3-12 ----
                        div(style = "display: inline-block; width: 25px;", HTML("<br>")), # add some space
                        div(style = "display: inline-block;",
                            # radioButtons(
                            #     inputId = 'question_3_12',
                            #     label = 'Other (please describe):',
                            #     choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                            #     inline = TRUE)
                            uiOutput('question_3_12')
                        ),
                        div(style = "display: inline-block; width: 25px;", HTML("<br>")), # add some space
                        div(style = "display: inline-block;",
                            # numericInput(
                            #     inputId = 'question_3_12_year',
                            #     label = 'Completion Year:',
                            #     min = 1900,
                            #     max = lubridate::year(Sys.Date()),
                            #     step = 1,
                            #     value = NA)
                            uiOutput('question_3_12_year')
                        ),
                        div(style = "display: inline-block; width: 25px;", HTML("<br>")), # add some space
                        div(style = "display: inline-block;", 
                            # # textAreaInput(inputId = 'question_3_12_text',
                            # textInput(inputId = 'question_3_12_text',
                            #           label = "Description:",
                            #           placeholder = 'Describe the measure...',
                            #           width = 500,
                            #           value = NA)
                            # # resize = 'both')
                            uiOutput('question_3_12_text')
                        ),
                        div(),
                    
                    hr(style="border: 1px solid darkgrey"),
                    
                    # QUESTION 4 ----
                        uiOutput('question_4'),
                        # radioButtons(inputId = 'question_4', 
                        #              label = "4. Can measures to address new or increased threats resulting from climage change be 
                        #          accomplished within your existing or anticipated future budgets?",
                        #              choices = c('No', 'Yes', 'Unknown'), inline = TRUE),
                        div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")), # add some space
                        div(style="display: inline-block;vertical-align:top;",
                            # textInput(inputId = 'question_4a',
                            #           label = 'Please estimate the total financial impact to implement identified measures as a
                            #       percentage of your annual gross revenues:',
                            #           width = 1000,
                            #           placeholder = 'Enter estimated financial impact here...',
                            #           value = NA)
                            uiOutput('question_4a'),
                        ),
                        # error message if value is not numeric
                        shinyjs::hidden(
                            div(id = 'numeric_percent_error_msg_4a',
                                p('Please enter a numeric value above, as a percentage between 0 to 100 percent (e.g., enter "30%" as "30")', style = "color:red")
                            )),
                        
                        div(),
                        hr(style="border: 1px solid darkgrey"),
                    
                    # QUESTION 5 ----
                        p(tags$b('5. Notes (please include any other relevant information, or explain a response that may need clarification):')),
                        # textAreaInput(inputId = 'question_5', 
                        #               label = NULL, 
                        #               width = 1025, 
                        #               resize = 'both', 
                        #               placeholder = 'Enter notes here...'),
                        uiOutput('question_5'),
                    )
                ),

        # COMPLETION ----
            div(),
            shinyjs::hidden(
                div(
                    id = "completion",
                    hr(style="border: 2px solid black"),
                    p("This completes the survey. Please click the button below when you are ready to submit your responses. Thank you."),
                    # SUBMIT BUTTON ---
                    actionButton("submit", "Submit", class = "btn-primary")
                    )
                ),

        # THANK YOU MESSAGE ----
            shinyjs::hidden(
                div(
                    id = "thankyou_msg",
                    h3("Thanks, your response was submitted successfully!"),
                    #actionLink("submit_another", "Submit another response")
                    actionButton("close", "Close", class = "btn-primary")
                )
            ), 

        # OTHER MESSAGES ----
            shinyjs::hidden(
                div(
                    id = "agency_code_error_msg",
                    h4("Invalid agency code entered. Please check the agency code and ensure that it is identical to the code you were provided via email.", 
                       style = "color:red")# ,
                    # actionButton("close_error_msg", "Close Message", class = "btn-primary")
                )
            ), 
            
            shinyjs::hidden(
                div(
                    id = "numeric_error_msg",
                    h3("Invalid entry in a numeric field. Please check to ensure that you've entered a valid numeric value (red text indicates warning messages)."),
                    actionButton("close_numeric_error_msg", "Close Message", class = "btn-primary")
                )
            ), 
            
            shinyjs::hidden(
                div(
                    id = "numeric_pop_error_msg",
                    h3("Invalid entry in a numeric field. Please check to ensure that you've entered a valid numeric value (red text indicates warning messages)."),
                    actionButton("close_numeric_pop_error_msg", "Close Message", class = "btn-primary")
                )
            ), 
            
            shinyjs::hidden(
                div(
                    id = "numeric_percent_error_msg",
                    h3("Invalid entry in a numeric field. Please check to ensure that you've entered a valid numeric value (red text indicates warning messages)."),
                    actionButton("close_numeric_percent_error_msg", "Close Message", class = "btn-primary")
                )
            ), 
            
            shinyjs::hidden(
                div(
                    id = "duplicate_msg",
                    tags$br(),
                    h4("Your agency has already submitted a response, would you like to continue?",
                       style = 'color:red'),
                    # actionLink("submit_duplicate", "Continue")
                    actionButton("submit_duplicate", "Continue", class = "btn-primary"),
                    actionButton("close_2", "Close", class = "btn-primary")
                )
            ),
            
            shinyjs::hidden(
                div(
                    id = "agency_codes_page",
                    h3("Agency Codes"),
                    actionButton("agency_codes_continue", "CLOSE TABLE", class = "btn-primary"),
                    tableOutput('codes_table')
                )
            )
        
        # end of UI ----
    ),
    
    
    # SERVER !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ----
    server = function(input, output, session) {
        # agency info tracker ----
            output$output_agency_code <- renderText(input$agency_code)
            output$output_agency_name <- renderText(input$agency_name)
        
        # Make agency codes table ----
            output$codes_table <- renderTable({
                names(codes) <- c('Agency Name', 'Code')
                codes
            })
        
        # Render agency info data entry boxes ----
            # output$agencySelect <- renderUI({
            #     selectInput(inputId = 'agency",
            #                 label = NULL, # labelMandatory("Select Agency: (Note: Fields below will auto-populate based on selected agency. Edit populated fields as needed.)"),
            #                 choices = as.character((codes %>% filter(code == input$agency_code))[1,1]), # agencies.list,
            #                 selected = if (input$agency_code == '' | is.null(input$agency_code) | input$agency_code %in% codes$code == FALSE) {
            #                     ''
            #                 } else {
            #                         as.character((codes %>% filter(code == input$agency_code))[1,1])
            #                     }
            #     )
            # })
            
            output$agencySelect <- renderUI({
                textInput(inputId = 'agency_name', 
                            label = NULL, # labelMandatory("Select Agency: (Note: Fields below will auto-populate based on selected agency. Edit populated fields as needed.)"),
                            # choices = as.character((codes %>% filter(code == input$agency_code))[1,1]), # agencies.list,
                            value = if (input$agency_code == '' | is.null(input$agency_code) | input$agency_code %in% codes$code == FALSE) {
                                ''
                            } else {
                                as.character((codes %>% filter(code == input$agency_code))[1,1])
                            }
                )
            })
            
            # output$selectedAgency <- renderUI({
            #     textInput(inputId = 'agency_selected', 
            #               label = NULL, 
            #               value = if (input$agency_code == '' | is.null(input$agency_code) | input$agency_code %in% codes$code == FALSE) {
            #                   ''
            #               } else {
            #                   input$agency
            #               }
            #     )
            # })
            # 
            # output$selectedAgencyCode <- renderUI({
            #     textInput(inputId = 'agency_selected', 
            #               label = NULL,
            #               value = if (input$agency_code == '' | is.null(input$agency_code) | input$agency_code %in% codes$code == FALSE) {
            #                   ''
            #               } else {
            #                   input$agency_code
            #               }
            #     )
            # })
            
            output$facilityTypes <- renderUI({
                if (is.null(input$agency_name)) {
                    saved.types = NULL
                } else if (input$agency_name == '') {
                    saved.types = NULL
                } else {
                    saved.types <- dummy.data %>% 
                        filter(agency_name == input$agency_name) %>% 
                        pull(facility_types) %>% 
                        strsplit(split = ' | ', fixed = TRUE)
                    saved.types <- as.character(saved.types[[1]])
                }
                checkboxGroupInput(inputId = "facility_types", label = "Check facilities your agency is responsible for:", 
                                   choices = c("Collection", "Interception", "Treatment", "Disposal"), selected = saved.types, inline = TRUE)
            })
            
            output$approxPop <- renderUI({
                textInput(inputId = "population", label = NULL , #"Approximate population receiving wastewater service from your agency:", 
                             value = if(is.null(input$agency_name)) {''} else {dummy.data %>% filter(agency_name == input$agency_name) %>% pull(approx_pop)}, 
                             )
            })
            
            output$contactPersonSelection <- renderUI({
                textInput(inputId = "contact_person", 
                          label = "Contact Name:", 
                          value = if(is.null(input$agency_name)) {''} else {dummy.data %>% filter(agency_name == input$agency_name) %>% pull(contact_person)}, 
                          placeholder = 'Enter contact name...')
            })
            
            output$contactTitleSelection <- renderUI({
                textInput(inputId = "contact_title", 
                          label = "Contact Title:", 
                          value = if(is.null(input$agency_name)) {''} else {dummy.data %>% filter(agency_name == input$agency_name) %>% pull(contact_title)},
                          placeholder = 'Enter contact title...')
            })
            
            output$contactPhoneSelection <- renderUI({
                textInput(inputId = "contact_phone", 
                          label = "Contact Phone:", 
                          value = if(is.null(input$agency_name)) {''} else {dummy.data %>% filter(agency_name == input$agency_name) %>% pull(contact_phone)},
                          placeholder = 'Enter contact phone...')
            })
            
            output$contactEmailSelection <- renderUI({
                textInput(inputId = "contact_email", 
                          label = "Contact Email:", 
                          value = if (is.null(input$agency_name)) {""} else {dummy.data %>% filter(agency_name == input$agency_name) %>% pull(contact_email)},
                          placeholder = 'Enter contact email...')
            })
            
            # output$dateSelection <- renderUI({
            #     dateInput(inputId = 'date_submitted', 
            #               label = 'Date:', 
            #               value = if (is.null(input$agency_name)) {""} else if (input$agency_name == '') {''} else {Sys.Date()})
            # })
            
        # Render Questions (fill in based on previous entries if available) ----
            # QUESTION 1 ----
                output$question_1 <- renderUI({
                    radioButtons(inputId = 'question_1', 
                                 label = '1. Has your agency conducted facility or infrastructure assessment(s), or prepared an asset 
                                 management plan that includes new or increased threats from climate change or future extreme weather 
                                 events (e.g., sea level rise, storm surge, high intensity precipitation, flooding, drought, or 
                                 extreme heat)?', 
                                 choices = c('No', 'Yes', 'Work is underway'), 
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {previous.responses %>% filter(agency_name == input$agency_name) %>% slice(n()) %>% pull(q1_agency_assessment)} else {'No'},
                                 width = 1300,
                                 inline = TRUE)
                })
                # 1a
                    output$question_1a <- renderUI({ 
                        radioButtons(inputId = 'question_1a', 
                                     label = '1a. Is there a local or regional assessment or plan (e.g., Climate Action Plan, 
                                         General Plan, Integrated Regional Water Management Plan) that addresses new or increased 
                                         threats to infrastructure resulting from climate change, and includes your system?', 
                                     choices = c('No', 'Yes'), 
                                     selected = if (input$agency_name %in% previous.responses$agency_name) {previous.responses %>% filter(agency_name == input$agency_name) %>% slice(n()) %>% pull(q1_a_local_regional_plan)} else {NULL},
                                     width = 1250,
                                     inline = TRUE)
                    })
                # 1b
                    output$question_1b <- renderUI({
                        if (is.null(input$agency_name) | input$agency_name == '') {
                            saved.types_1b = NULL
                        } else if (input$agency_name %in% previous.responses$agency_name == FALSE) {
                            saved.types_1b = NULL
                        } else {
                            saved.types_1b <- previous.responses %>% 
                                filter(agency_name == input$agency_name) %>% 
                                pull(q1_b_assessment_components) %>% 
                                strsplit(split = ' | ', fixed = TRUE)
                            saved.types_1b <- as.character(saved.types_1b[[1]])
                        }
                        checkboxGroupInput(inputId = 'question_1b', 
                                           label = '1b. Which components were covered by the assessment/planning? (Check all that apply)', 
                                           choices = c('Collection System', 'Interceptors', 'Treatment Facilities', 
                                                       'Disinfection', 'Discharge facilities/outfalls', 'Facility Access',
                                                       'Pump stations', 'Wet weather facilities', 'Power source / Biogas / Cogeneration',
                                                       'Telecommunications'), 
                                           selected = saved.types_1b,
                                           width = 850,
                                           inline = TRUE)
                    })
            # QUESTION 2 ----
                output$question_2 <- renderUI({
                    radioButtons(inputId = 'question_2',
                                 label = '2. Are results of the assessment/planning available to the public?', 
                                 choices = c('No', 'Yes'), 
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q2_assessment_public)
                                     } else {'No'},
                                 inline = TRUE)
                })
                output$question_2text <- renderUI({
                    textInput(inputId = 'question_2text', 
                              label = 'If yes, please provide the website, or where the document(s) can be found:', 
                              width = 1000, 
                              placeholder = "Enter location here...", 
                              value = if (input$agency_name %in% previous.responses$agency_name) {
                                  previous.responses %>% 
                                      filter(agency_name == input$agency_name) %>% 
                                      slice(n()) %>% 
                                      pull(q2_a_assessment_location)
                                  } else {NULL}
                              )
                })
            # QUESTION 3 ----
                # Question 3-1 ----
                output$question_3_1 <- renderUI({
                    radioButtons(inputId = 'question_3_1',
                                 label = 'Expanding capacity:',
                                 choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_1_measures_expanded_capacity)
                                 } else {'Not Planned'},
                                 inline = TRUE)
                })
                output$question_3_1_year <- renderUI({
                    numericInput(inputId = 'question_3_1_year',
                                 label = 'Completion Year:', min = 1900, max = lubridate::year(Sys.Date()),
                                 step = 1,
                                 value = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_1_measures_expanded_capacity_year)
                                 } else {NULL}
                                 )
                })
                # Question 3-2 ----
                output$question_3_2 <- renderUI({
                    radioButtons(inputId = 'question_3_2',
                                 label = 'Increasing maintenance or rehabilitation frequency:',
                                 choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_2_measures_maintenance_frequency)
                                 } else {'Not Planned'},
                                 inline = TRUE)
                })
                output$question_3_2_year <- renderUI({
                    numericInput(inputId = 'question_3_2_year',
                                 label = 'Completion Year:', min = 1900, max = lubridate::year(Sys.Date()),
                                 step = 1,
                                 value = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_2_measures_maintenance_frequency_year)
                                 } else {NULL}
                    )
                })
                # Question 3-3 ----
                output$question_3_3 <- renderUI({
                    radioButtons(inputId = 'question_3_3',
                                 label = 'Modifying treatment capability:',
                                 choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_3_measures_treatment_capability)
                                 } else {'Not Planned'},
                                 inline = TRUE)
                })
                output$question_3_3_year <- renderUI({
                    numericInput(inputId = 'question_3_3_year',
                                 label = 'Completion Year:', min = 1900, max = lubridate::year(Sys.Date()),
                                 step = 1,
                                 value = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_3_measures_treatment_capability_year)
                                 } else {NULL}
                    )
                })
                # Question 3-4 ----
                output$question_3_4 <- renderUI({
                    radioButtons(inputId = 'question_3_4',
                                 label = 'Hardening facilities (e.g., installing, increasing, or improving barriers, buffers, or 
                                 levees; elevating or floodproofing equipment; or sealing doors, sewer mains, or manholes):',
                                 choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_4_measures_facility_hardening)
                                 } else {'Not Planned'},
                                 inline = TRUE)
                })
                output$question_3_4_year <- renderUI({
                    numericInput(inputId = 'question_3_4_year',
                                 label = 'Completion Year:', min = 1900, max = lubridate::year(Sys.Date()),
                                 step = 1,
                                 value = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_4_measures_facility_hardening_year)
                                 } else {NULL}
                    )
                })
                # Question 3-5 ----
                output$question_3_5 <- renderUI({
                    radioButtons(inputId = 'question_3_5',
                                 label = 'Improving, upgrading, or relocating electrical components/instrumentation:',
                                 choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_5_measures_electrical_improvements)
                                 } else {'Not Planned'},
                                 inline = TRUE)
                })
                output$question_3_5_year <- renderUI({
                    numericInput(inputId = 'question_3_5_year',
                                 label = 'Completion Year:', min = 1900, max = lubridate::year(Sys.Date()),
                                 step = 1,
                                 value = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_5_measures_electrical_improvements_year)
                                 } else {NULL}
                    )
                })
                # Question 3-6 ----
                output$question_3_6 <- renderUI({
                    radioButtons(inputId = 'question_3_6',
                                 label = 'Updating maintenance procedures:',
                                 choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_6_measures_maintenance_procedures)
                                 } else {'Not Planned'},
                                 inline = TRUE)
                })
                output$question_3_6_year <- renderUI({
                    numericInput(inputId = 'question_3_6_year',
                                 label = 'Completion Year:', min = 1900, max = lubridate::year(Sys.Date()),
                                 step = 1,
                                 value = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_6_measures_maintenance_procedures_year)
                                 } else {NULL}
                    )
                })
                # Question 3-7 ----
                output$question_3_7 <- renderUI({
                    radioButtons(inputId = 'question_3_7',
                                 label = 'Updating emergency response and recovery procedures:',
                                 choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_7_measures_emergency_procedures)
                                 } else {'Not Planned'},
                                 inline = TRUE)
                })
                output$question_3_7_year <- renderUI({
                    numericInput(inputId = 'question_3_7_year',
                                 label = 'Completion Year:', min = 1900, max = lubridate::year(Sys.Date()),
                                 step = 1,
                                 value = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_7_measures_emergency_procedures_year)
                                 } else {NULL}
                    )
                })
                # Question 3-8 ----
                output$question_3_8 <- renderUI({
                    radioButtons(inputId = 'question_3_8',
                                 label = 'Securing a backup power supply or contracts for an alternative power supply:',
                                 choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_8_measures_backup_power)
                                 } else {'Not Planned'},
                                 inline = TRUE)
                })
                output$question_3_8_year <- renderUI({
                    numericInput(inputId = 'question_3_8_year',
                                 label = 'Completion Year:', min = 1900, max = lubridate::year(Sys.Date()),
                                 step = 1,
                                 value = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_8_measures_backup_power_year)
                                 } else {NULL}
                    )
                })
                # Question 3-9 ----
                output$question_3_9 <- renderUI({
                    radioButtons(inputId = 'question_3_9',
                                 label = 'Relocating facilities:',
                                 choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_9_measures_relocating_facilities)
                                 } else {'Not Planned'},
                                 inline = TRUE)
                })
                output$question_3_9_year <- renderUI({
                    numericInput(inputId = 'question_3_9_year',
                                 label = 'Completion Year:', min = 1900, max = lubridate::year(Sys.Date()),
                                 step = 1,
                                 value = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_9_measures_relocating_facilities_year)
                                 } else {NULL}
                    )
                })
                # Question 3-10 ----
                output$question_3_10 <- renderUI({
                    radioButtons(inputId = 'question_3_10',
                                 label = 'Constructing or installing redundant facilities:',
                                 choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_10_measures_redundant_facilities)
                                 } else {'Not Planned'},
                                 inline = TRUE)
                })
                output$question_3_10_year <- renderUI({
                    numericInput(inputId = 'question_3_10_year',
                                 label = 'Completion Year:', min = 1900, max = lubridate::year(Sys.Date()),
                                 step = 1,
                                 value = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_10_measures_redundant_facilities_year)
                                 } else {NULL}
                    )
                })
                # Question 3-11 ----
                output$question_3_11 <- renderUI({
                    radioButtons(inputId = 'question_3_11',
                                 label = 'Other (please describe):',
                                 choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_11_measures_other_1)
                                 } else {'Not Planned'},
                                 inline = TRUE)
                })
                output$question_3_11_year <- renderUI({
                    numericInput(inputId = 'question_3_11_year',
                                 label = 'Completion Year:', min = 1900, max = lubridate::year(Sys.Date()),
                                 step = 1,
                                 value = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_11_measures_other_1_year)
                                 } else {NULL}
                    )
                })
                output$question_3_11_text <- renderUI({
                    textInput(inputId = 'question_3_11_text',
                              label = "Description:",
                              placeholder = 'Describe the measure...',
                              width = 500,
                              value = if (input$agency_name %in% previous.responses$agency_name) {
                                  previous.responses %>% 
                                      filter(agency_name == input$agency_name) %>% 
                                      slice(n()) %>% 
                                      pull(q3_11_measures_other_1_description)
                              } else {NULL})
                })
                # Question 3-12 ----
                output$question_3_12 <- renderUI({
                    radioButtons(inputId = 'question_3_12',
                                 label = 'Other (please describe):',
                                 choices = c('Not Planned', 'Planned', 'In Progress', 'In Place'),
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_12_measures_other_2)
                                 } else {'Not Planned'},
                                 inline = TRUE)
                })
                output$question_3_12_year <- renderUI({
                    numericInput(inputId = 'question_3_12_year',
                                 label = 'Completion Year:', min = 1900, max = lubridate::year(Sys.Date()),
                                 step = 1,
                                 value = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q3_12_measures_other_2_year)
                                 } else {NULL}
                    )
                })
                output$question_3_12_text <- renderUI({
                    textInput(inputId = 'question_3_12_text',
                              label = "Description:",
                              placeholder = 'Describe the measure...',
                              width = 500,
                              value = if (input$agency_name %in% previous.responses$agency_name) {
                                  previous.responses %>% 
                                      filter(agency_name == input$agency_name) %>% 
                                      slice(n()) %>% 
                                      pull(q3_12_measures_other_2_description)
                              } else {NULL})
                })
                
                
            # QUESTION 4 ----
                output$question_4 <- renderUI({
                    radioButtons(inputId = 'question_4',
                                 label = "4. Can measures to address new or increased threats resulting from climage change be 
                                 accomplished within your existing or anticipated future budgets?",
                                 choices = c('No', 'Yes', 'Unknown'),
                                 selected = if (input$agency_name %in% previous.responses$agency_name) {
                                     previous.responses %>% 
                                         filter(agency_name == input$agency_name) %>% 
                                         slice(n()) %>% 
                                         pull(q4_measures_within_budget)
                                 } else {'No'},
                                 inline = TRUE)
                })

                
                output$question_4a <- renderUI({
                    textInput(inputId = 'question_4a',
                              label = 'Please estimate the total financial impact to implement identified measures as a
                          percentage of your annual gross revenues:',
                              width = 1000,
                              placeholder = 'Enter estimated financial impact here...',
                              value = if (input$agency_name %in% previous.responses$agency_name) {
                                  previous.responses %>% 
                                      filter(agency_name == input$agency_name) %>% 
                                      slice(n()) %>% 
                                      pull(q4_a_measures_percent_budget)
                              } else {NULL}
                    )
                })
                
            # QUESTION 5 ----
                output$question_5 <- renderUI({
                    textAreaInput(inputId = 'question_5',
                                  label = NULL,
                                  width = 1025,
                                  resize = 'both',
                                  placeholder = 'Enter notes here...',
                                  value = if (input$agency_name %in% previous.responses$agency_name) {
                                      previous.responses %>% 
                                          filter(agency_name == input$agency_name) %>% 
                                          slice(n()) %>% 
                                          pull(q5_notes)
                                  } else {NULL})
                })


        # Functions to validate inputs ----
            checkCode <- function(input_code) {
                if (input_code %in% codes$code) {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            }

            checkNumeric <- function(input_value) {
                if (!is.na(as.numeric(input_value)) | input_value == '') {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            }

            observe({
                checkNumericPop <- function(input_value) {
                    input_value == '' | (as.numeric(input_value) >= 0 & as.numeric(input_value) <= 30*10^6 & !is.na(as.numeric(input_value)))
                }
                pop_check <- checkNumericPop(input$population)
                shinyjs::toggleState(id = "verify_agency_info", condition = pop_check)
            })

            checkNumericPop <- function(input_value) {
                if ((!is.na(as.numeric(input_value)) & as.numeric(input_value) >= 0 & as.numeric(input_value) <= 30*10^6) | input_value == '') {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            }

            observe({
                checkNumericPercent <- function(input_value) {
                    input_value == '' | (as.numeric(input_value) >= 0 & as.numeric(input_value) <= 100 & !is.na(as.numeric(input_value)))
                }
                percent_check <- checkNumericPercent(input$question_4a)
                shinyjs::toggleState(id = "submit", condition = percent_check)
            })

            checkNumericPercent <- function(input_value) {
                if ((!is.na(as.numeric(input_value)) & as.numeric(input_value) >= 0 & as.numeric(input_value) <= 100) | input_value == '') {
                    return(TRUE)
                } else {
                    return(FALSE)
                }
            }

        # Mandatory Fields (disable the submit button until they are filled) ----
            observe({
                mandatoryFilled <-
                    vapply(fieldsMandatory,
                           function(x) {
                               !is.null(input[[x]]) && input[[x]] != ""
                           },
                           logical(1))
                mandatoryFilled <- all(mandatoryFilled)
                
                shinyjs::toggleState(id = "submit", condition = mandatoryFilled) # & input$agency_code %in% codes$code)
            })  

        # Toggle the state of questions (make options dependent on entered info) ----
            # toggle agency name
            observe({
                shinyjs::toggleState(id = "agencySelect", condition = input$agency_name == '')
            })
            observe({
                shinyjs::toggleState(id = "selectedAgency", condition = input$agency_name == '')
            })
            observe({
                shinyjs::toggleState(id = "selectedAgencyCode", condition = input$agency_name == '')
            })
            
            # observe({
            #     shinyjs::toggleState(id = "agencySelect", condition = !is.na(input$agency_code) & input$agency_code != '' & !is.null(input$agency_code) & input$agency_code %in% codes$code)
            # })
            # 
            # # toggle facility types
            # observe({
            #     shinyjs::toggleState(id = "facilityTypes", condition = !is.na(input$agency_code) & input$agency_code != '' & !is.null(input$agency_code) & input$agency_code %in% codes$code)
            # })
            
            # toggle question 1
            observe({
                shinyjs::toggleState(id = "question_1b", condition = input$question_1a == 'Yes')
            })
            
            # toggle question 2
            observe({
                shinyjs::toggleState(id = "question_2text", condition = input$question_2 == 'Yes')
            })
            
            # toggle question 3
            observe({
                shinyjs::toggleState(id = "question_3_1_year", condition = input$question_3_1 != 'Not Planned')
            })
            observe({
                shinyjs::toggleState(id = "question_3_2_year", condition = input$question_3_2 != 'Not Planned')
            })
            observe({
                shinyjs::toggleState(id = "question_3_3_year", condition = input$question_3_3 != 'Not Planned')
            })
            observe({
                shinyjs::toggleState(id = "question_3_4_year", condition = input$question_3_4 != 'Not Planned')
            })
            observe({
                shinyjs::toggleState(id = "question_3_5_year", condition = input$question_3_5 != 'Not Planned')
            })
            observe({
                shinyjs::toggleState(id = "question_3_6_year", condition = input$question_3_6 != 'Not Planned')
            })
            observe({
                shinyjs::toggleState(id = "question_3_7_year", condition = input$question_3_7 != 'Not Planned')
            })
            observe({
                shinyjs::toggleState(id = "question_3_8_year", condition = input$question_3_8 != 'Not Planned')
            })
            observe({
                shinyjs::toggleState(id = "question_3_9_year", condition = input$question_3_9 != 'Not Planned')
            })
            observe({
                shinyjs::toggleState(id = "question_3_10_year", condition = input$question_3_10 != 'Not Planned')
            })
            observe({
                shinyjs::toggleState(id = "question_3_11_year", condition = input$question_3_11 != 'Not Planned')
            })
            observe({
                shinyjs::toggleState(id = "question_3_11_text", condition = input$question_3_11 != 'Not Planned')
            })
            observe({
                shinyjs::toggleState(id = "question_3_12_year", condition = input$question_3_12 != 'Not Planned')
            })
            observe({
                shinyjs::toggleState(id = "question_3_12_text", condition = input$question_3_12 != 'Not Planned')
            })
        
        # Save data functions ----
            # formData <- reactive({
            #     data <- sapply(fieldsAll, function(x) input[[x]])
            #     data <- c(data, timestamp = epochTime())
            #     # data <- t(data)
            #     data <- as.data.frame(data)
            #     data
            # })
            
            # New Save Data !!!!!!
            formData <- reactive({
                
                previous.responses <- readr::read_csv("responses//_Master.csv")
                duplicate.response <- input$agency_name %in% previous.responses$agency_name
                
                if (is.null(input$facility_types)) {
                    facilitytypes_formatted <- NA
                } else if (length(input$facility_types) > 1) {
                    facilitytypes_formatted <- input$facility_types[1]
                    for (i in 2:length(input$facility_types)) {
                        facilitytypes_formatted <- paste0(facilitytypes_formatted, ' | ', input$facility_types[i])
                    }
                } else if (length(input$facility_types) == 1) {facilitytypes_formatted <- input$facility_types}
                
                # format question 1b
                if (is.null(input$question_1b)) {
                    question_1b_formatted <- NA
                } else if (length(input$question_1b) > 1) {
                    question_1b_formatted <- input$question_1b[1]
                    for (i in 2:length(input$question_1b)) {
                        question_1b_formatted <- paste0(question_1b_formatted, ' | ', input$question_1b[i])
                    }
                } else if (length(input$question_1b) == 1) {question_1b_formatted <- input$question_1b}
                
                data <- data.frame('agency_name' = input$agency_name,
                                   'agency_code' = input$agency_code,
                                   'duplicate_response' = duplicate.response,
                                   'duplicate_reason' = if (input$duplicate_reason == '') {NA} else {input$duplicate_reason},
                                   'overwrite_duplicate' = 'No', # input$overwrite_duplicate,
                                   'facility_types' = facilitytypes_formatted,
                                   'facilities_collection' = TRUE %in% grepl(pattern = 'Collection', x = input$facility_types),
                                   'facilities_interception' = TRUE %in% grepl(pattern = 'Interception', x = input$facility_types),
                                   'facilities_treatment' = TRUE %in% grepl(pattern = 'Treatment', x = input$facility_types),
                                   'facilities_disposal' = TRUE %in% grepl(pattern = 'Disposal', x = input$facility_types),
                                   'population' = as.numeric(input$population),
                                   'contact_person' = input$contact_person,
                                   'contact_title' = input$contact_title,
                                   'contact_phone' = input$contact_phone,
                                   'contact_email' = input$contact_email,
                                   'date_submitted' = Sys.Date(), # as.Date(input$date_submitted),
                                   'question_1' = input$question_1,
                                   'question_1a' = input$question_1a,
                                   # 'question_1b' = input$question_1b,
                                   'question_1b' = question_1b_formatted,
                                   'question_1b_Collection' = TRUE %in% grepl(pattern = 'Collection System', x = question_1b_formatted),
                                   'question_1b_Interceptors' = TRUE %in% grepl(pattern = 'Interceptors', x = question_1b_formatted),
                                   'question_1b_Treatment' = TRUE %in% grepl(pattern = 'Treatment Facilities', x = question_1b_formatted),
                                   'question_1b_Disinfection' = TRUE %in% grepl(pattern = 'Disinfection', x = question_1b_formatted),
                                   'question_1b_Discharge' = TRUE %in% grepl(pattern = 'Discharge facilities/outfalls', x = question_1b_formatted),
                                   'question_1b_FacilityAccess' = TRUE %in% grepl(pattern = 'Facility Access', x = question_1b_formatted),
                                   'question_1b_PumpStations' = TRUE %in% grepl(pattern = 'Pump stations', x = question_1b_formatted),
                                   'question_1b_WetWeatherFacilities' = TRUE %in% grepl(pattern = 'Wet weather facilities', x = question_1b_formatted),
                                   'question_1b_PowerSource' = TRUE %in% grepl(pattern = 'Power source / Biogas / Cogeneration', x = question_1b_formatted),
                                   'question_1b_Telecommunications' = TRUE %in% grepl(pattern = 'Telecommunications', x = question_1b_formatted),
                                   'question_2' = input$question_2,
                                   # 'question_2text' = if (is.null(input$question_2text)) {NA} else {input$question_2text},
                                   # 'question_2text' = if (input$question_2 == 'No') {NA} else {input$question_2text},
                                   'question_2text' = if (input$question_2text == '') {NA} else {input$question_2text},
                                   'question_3_1' = input$question_3_1,
                                   'question_3_1_year' = input$question_3_1_year,
                                   'question_3_2' = input$question_3_2,
                                   'question_3_2_year' = input$question_3_2_year,
                                   'question_3_3' = input$question_3_3,
                                   'question_3_3_year' = input$question_3_3_year,
                                   'question_3_4' = input$question_3_4,
                                   'question_3_4_year' = input$question_3_4_year,
                                   'question_3_5' = input$question_3_5,
                                   'question_3_5_year' = input$question_3_5_year,
                                   'question_3_6' = input$question_3_6,
                                   'question_3_6_year' = input$question_3_6_year,
                                   'question_3_7' = input$question_3_7,
                                   'question_3_7_year' = input$question_3_7_year,
                                   'question_3_8' = input$question_3_8,
                                   'question_3_8_year' = input$question_3_8_year,
                                   'question_3_9' = input$question_3_9,
                                   'question_3_9_year' = input$question_3_9_year,
                                   'question_3_10' = input$question_3_10,
                                   'question_3_10_year' = input$question_3_10_year,
                                   'question_3_11' = input$question_3_11,
                                   'question_3_11_year' = input$question_3_11_year,
                                   # 'question_3_11_text' = input$question_3_11_text,
                                   # 'question_3_11_text' = if (input$question_3_11 == 'Not Planned') {NA} else {input$question_3_11_text},
                                   'question_3_11_text' = if (input$question_3_11_text == '') {NA} else {input$question_3_11_text},
                                   'question_3_12' = input$question_3_12,
                                   'question_3_12_year' = input$question_3_12_year,
                                   # 'question_3_12_text' = input$question_3_12_text,
                                   # 'question_3_12_text' = if (input$question_3_12 == 'Not Planned') {NA} else {input$question_3_12_text},
                                   'question_3_12_text' = if (input$question_3_12_text == '') {NA} else {input$question_3_12_text},
                                   'question_4' = input$question_4,
                                   # 'question_4a' = input$question_4a,
                                   'question_4a' = as.numeric(if (input$question_4a == '') {NA_real_} else {input$question_4a}),
                                   'question_5' = if (input$question_5 == '') {NA} else {input$question_5},
                                   'timestamp' = Sys.time(),
                                   stringsAsFactors = FALSE)
                names_update <- c('agency_name', 'agency_code',
                                  'duplicate_response', 'duplicate_reason', 'overwrite_duplicate', 
                                  'facility_types', 'facilities_collection', 'facilities_interception', 
                                  'facilities_treatment', 'facilities_disposal', 'population', 'contact_person', 
                                  'contact_title', 'contact_phone', 'contact_email', 'date_submitted', 
                                  # Question 1
                                  'q1_agency_assessment', 'q1_a_local_regional_plan', 'q1_b_assessment_components', 
                                  'q1_b_Collection', 'q1_b_Interceptors', 'q1_b_Treatment', 'q1_b_Disinfection', 
                                  'q1_b_Discharge', 'q1_b_FacilityAccess', 'q1_b_PumpStations', 'q1_b_WetWeatherFacilities', 
                                  'q1_b_PowerSource', 'q1_b_Telecommunications', 
                                  # Question 2
                                  'q2_assessment_public', 'q2_a_assessment_location', 
                                  # Question 3
                                  'q3_1_measures_expanded_capacity', 
                                  'q3_1_measures_expanded_capacity_year', 'q3_2_measures_maintenance_frequency', 
                                  'q3_2_measures_maintenance_frequency_year', 'q3_3_measures_treatment_capability', 
                                  'q3_3_measures_treatment_capability_year', 'q3_4_measures_facility_hardening', 
                                  'q3_4_measures_facility_hardening_year', 'q3_5_measures_electrical_improvements', 
                                  'q3_5_measures_electrical_improvements_year', 'q3_6_measures_maintenance_procedures', 
                                  'q3_6_measures_maintenance_procedures_year', 'q3_7_measures_emergency_procedures', 
                                  'q3_7_measures_emergency_procedures_year', 'q3_8_measures_backup_power', 
                                  'q3_8_measures_backup_power_year', 'q3_9_measures_relocating_facilities', 
                                  'q3_9_measures_relocating_facilities_year', 'q3_10_measures_redundant_facilities', 
                                  'q3_10_measures_redundant_facilities_year', 'q3_11_measures_other_1', 'q3_11_measures_other_1_year', 
                                  'q3_11_measures_other_1_description', 'q3_12_measures_other_2', 'q3_12_measures_other_2_year', 
                                  'q3_12_measures_other_2_description', 
                                  # Question 4
                                  'q4_measures_within_budget', 'q4_a_measures_percent_budget', 
                                  # Question 5
                                  'q5_notes', 
                                  # Timestamp
                                  'timestamp')
                names(data) <- names_update
                data
            })
            
            
            
            saveData <- function(data) {
                fileName <- sprintf("%s_%s.csv",
                                    humanTime(),
                                    digest::digest(data))
                readr::write_csv(x = data, path = file.path(responsesDir, fileName))
                # write.csv(x = data, file = file.path(responsesDir, fileName),
                #           row.names = FALSE, quote = TRUE)
            }
            
            updateMaster <- function(data) { # added
                # write.csv(x = temp_data, file = 'responses//_Master.csv')
                # if (input$overwrite_duplicate == 'No') {
                    readr::write_csv(x = data, path = 'responses//_Master.csv', append = TRUE)
                # } else {
                #     current_data <- readr::read_csv('responses//_Master.csv')
                #     current_data <- current_data %>% filter(agency_name != input$agency_name)
                #     write_data <- bind_rows(current_data, data)
                #     readr::write_csv(x = write_data, path = 'responses//_Master.csv')
                # }
            }
            
        # check for duplicates ----
            observe({
            # observeEvent(input$submit_code, {
                previous.responses <- readr::read_csv("responses//_Master.csv")
                
                duplicate.response <- input$agency_name %in% previous.responses$agency_name
                
                # observeEvent(input$submit_code, {
                    if (!is.null(duplicate.response) & !is.null(input$agency_name)) {
                        if (duplicate.response == TRUE) {
                            shinyjs::hide("form")
                            shinyjs::hide('agency_code_validation')
                            shinyjs::hide('agency_code_error_msg')
                            shinyjs::show('selected_agency')
                            shinyjs::show("duplicate_msg")
                        }
                    }
                # })
                
                shinyjs::toggle(id = "duplicate_reason", condition = duplicate.response == TRUE)
                shinyjs::toggle(id = "overwrite_duplicate", condition = duplicate.response == TRUE)
            })
            

            
            
            # action to submit a duplicate responce
            observeEvent(input$submit_duplicate, {
                # shinyjs::show("form")
                shinyjs::hide("duplicate_msg")
                shinyjs::show('form_duplicates')
                shinyjs::show('agency_selected_info')
                shinyjs::hide('agency_code_entry')
                shinyjs::hide('selected_agency')
                shinyjs::hide('agency_code_validation')
                shinyjs::show('agency_populated_info')
                shinyjs::hide('agency_code_error_msg')
                # shinyjs::show('form')
                # shinyjs::show('completion')
            })
            
        # Define actions to take for buttons and entries in certain fields ----
            # show rest of form (including agency info inputs) after agency code is entered
                # observeEvent(input$agency_code, {
                #     if (input$agency_code %in% codes$code) {
                #         shinyjs::show('agency_populated_info')
                #         shinyjs::show('form')
                #         shinyjs::show('completion')
                #     } else {
                #         shinyjs::hide('agency_populated_info')
                #         shinyjs::hide('form')
                #         shinyjs::hide('completion')
                #     }
                # })
            
                observeEvent(input$submit_code, {
                    if (input$agency_code %in% codes$code) {
                        shinyjs::show('selected_agency')
                        shinyjs::show('agency_code_validation')
                        # shinyjs::show('agency_populated_info')
                        shinyjs::hide('agency_code_error_msg')
                        # shinyjs::show('form')
                        # shinyjs::show('completion')
                        shinyjs::hide('duplicate_msg')
                    } else {
                        shinyjs::show('agency_code_error_msg')
                        shinyjs::hide('selected_agency')
                        shinyjs::hide('agency_code_validation')
                        shinyjs::hide('agency_populated_info')
                        shinyjs::hide('form')
                        shinyjs::hide('completion')
                        shinyjs::hide('duplicate_msg')
                    }
                })
            
            observeEvent(input$verify_code, {
                if (input$agency_code %in% codes$code) {
                    shinyjs::show('agency_selected_info')
                    shinyjs::hide('agency_code_entry')
                    shinyjs::hide('selected_agency')
                    shinyjs::hide('agency_code_validation')
                    shinyjs::show('agency_populated_info')
                    shinyjs::hide('agency_code_error_msg')
                    # shinyjs::show('form')
                    # shinyjs::show('completion')
                } else {
                    shinyjs::show('agency_code_entry')
                    shinyjs::show('agency_code_error_msg')
                    shinyjs::hide('selected_agency')
                    shinyjs::hide('agency_code_validation')
                    shinyjs::hide('agency_populated_info')
                    shinyjs::hide('form')
                    shinyjs::hide('completion')
                }
            })
            
            observeEvent(input$verify_agency_info, {
                if (input$agency_code %in% codes$code) {
                    shinyjs::show('agency_selected_info')
                    shinyjs::hide('agency_code_entry')
                    shinyjs::hide('selected_agency')
                    shinyjs::hide('agency_code_validation')
                    shinyjs::hide('agency_populated_info')
                    shinyjs::hide('agency_code_error_msg')
                    shinyjs::show('form')
                    shinyjs::show('completion')
                    # shinyjs::hide('form_duplicates')
                } else {
                    shinyjs::hide('agency_selected_info') 
                    shinyjs::show('agency_code_entry')
                    shinyjs::show('agency_code_error_msg')
                    shinyjs::hide('selected_agency')
                    shinyjs::hide('agency_code_validation')
                    shinyjs::hide('agency_populated_info')
                    shinyjs::hide('form')
                    shinyjs::hide('completion')
                }
            })
            
            observeEvent(input$go_back_to_agency_code_entry, {
                shinyjs::hide('form_duplicates')
                shinyjs::hide('agency_selected_info')
                shinyjs::show('agency_code_entry')
                shinyjs::hide('selected_agency')
                shinyjs::hide('agency_code_validation')
                shinyjs::hide('agency_populated_info')
                shinyjs::hide('agency_code_error_msg')
                shinyjs::hide('form')
                shinyjs::hide('completion')
            })

            observeEvent(input$go_back_to_agency_info, {
                    shinyjs::show('agency_selected_info')
                    shinyjs::hide('agency_code_entry')
                    shinyjs::hide('selected_agency')
                    shinyjs::hide('agency_code_validation')
                    shinyjs::show('agency_populated_info')
                    shinyjs::hide('agency_code_error_msg')
                    shinyjs::hide('form')
                    shinyjs::hide('completion')
            })
            
            # action to take when submit button is pressed
                observeEvent(input$submit, {
                    if (checkCode(input$agency_code) == TRUE & checkNumericPop(input$population) == TRUE & checkNumericPercent(input$question_4a) == TRUE) {
                        saveData(formData())
                        updateMaster(formData()) # added
                        shinyjs::hide('agency_selected_info')
                        shinyjs::hide('agency_code_entry')
                        shinyjs::hide('selected_agency')
                        shinyjs::hide('agency_code_validation')
                        shinyjs::hide('agency_populated_info')
                        shinyjs::hide('agency_code_error_msg')
                        shinyjs::hide('form')
                        shinyjs::hide('completion')
                        # shinyjs::reset("form")
                        shinyjs::show("thankyou_msg")
                    # } else if (checkCode(input$agency_code) == FALSE) {
                    #     shinyjs::hide("form")
                    #     shinyjs::show("agency_code_error_msg") 
                    } else if(checkNumericPop(input$population) == FALSE) {
                        shinyjs::hide("form")
                        shinyjs::show("numeric_pop_error_msg")
                    } else if(checkNumericPercent(input$question_4a) == FALSE) {
                        shinyjs::hide("form")
                        shinyjs::show("numeric_percent_error_msg")
                    }
                })
                # 
                # observeEvent(input$submit_another, {
                #     shinyjs::show("form")
                #     shinyjs::hide("thankyou_msg")
                # })   
            
            # action to take when close button is pressed
                observeEvent(input$close, {
                    stopApp()
                    # shinyjs::onclick("setTimeout(function(){window.close();},500);")
                })
                observeEvent(input$close_2, {
                    stopApp()
                })
            
            # close error messages
                observeEvent(input$close_error_msg, {
                    shinyjs::hide('agency_code_error_msg')
                    # shinyjs::show("form")
                })
                
                observeEvent(input$close_numeric_error_msg, {
                    shinyjs::hide('numeric_error_msg')
                    shinyjs::show("form")
                })
                
                observeEvent(input$close_numeric_pop_error_msg, {
                    shinyjs::hide('numeric_pop_error_msg')
                    shinyjs::show("form")
                })
                
                observeEvent(input$close_numeric_percent_error_msg, {
                    shinyjs::hide('numeric_percent_error_msg')
                    shinyjs::show("form")
                })
            
            # action to take when show codes button is pressed
                observeEvent(input$show_codes, {
                    # shinyjs::hide("form")
                    shinyjs::hide("agency_code_entry")
                    shinyjs::show("agency_codes_page")
                    # shinyjs::hide('duplicate_msg')
                })            
            
            # action to to close the agency codes list
                observeEvent(input$agency_codes_continue, {
                    # shinyjs::show("form")
                    shinyjs::show("agency_code_entry")
                    shinyjs::hide("agency_codes_page")
                })
            
            # check for numeric value in population field
                observeEvent(input$population, {
                    if (checkNumericPop(input$population) == FALSE) {
                        shinyjs::show("pop_error_msg")
                    } else if (checkNumericPop(input$population) == TRUE) {
                        shinyjs::hide("pop_error_msg")
                    }
                })
            
            # check for numeric percent value in question 4a
                observeEvent(input$question_4a, {
                    if (checkNumericPercent(input$question_4a) == FALSE) {
                        shinyjs::show("numeric_percent_error_msg_4a")
                    } else if (checkNumericPercent(input$question_4a) == TRUE) {
                        shinyjs::hide("numeric_percent_error_msg_4a")
                    }
                })
                
                observeEvent(input$submit_code, {
                    duplicate.response <- input$agency_name %in% previous.responses$agency_name
                    
                    if (!is.null(duplicate.response) & !is.null(input$agency_name)) {
                        if (duplicate.response == TRUE) {
                            shinyjs::hide("form")
                            shinyjs::show('selected_agency')
                            shinyjs::hide('agency_code_validation')
                            shinyjs::hide('agency_code_error_msg')
                            shinyjs::show("duplicate_msg")
                        }
                    }
                    
                })
                
# end of server ----      
    }
# end of app ----
)
