library( RCurl )
library( shiny )
library( plyr )


dat <- read.csv("codeviolations.csv")

# Drop dates before 2012
complaint.date <- as.Date( dat$Violation.Date, "%m/%d/%Y" )
gt.2012 <- complaint.date > "2011-12-31"
dat <- dat[ gt.2012 , ]

complaint.date <- as.Date( dat$Violation.Date, "%m/%d/%Y" )

# this creates a factor for month-year
month.year <- cut( complaint.date, breaks="month" )

# this creates pretty names
month.year.name <- format( complaint.date, "%b-%Y" )

# table( dat$Complaint.Type, month.year )
dat$month.year <- month.year

complaint.types <- c("Property Maintenance-Int", 
                     "Trash/Debris-Private, Occ", 
                     "Bed Bugs", 
                     "Property Maintenance-Ext", 
                     "Building W/O Permit",
                     "Overgrowth: Private, Occ",
                     "Zoning Violations",
                     "Fire Safety",
                     "Fire Alarm",
                     "Unsafe Conditions",
                     "Infestation",
                     "Other (FPB)")

# I tried to keep the syntax as close to yours as possible for the corresponding violation data

dat.v <- read.csv("codeviolations.csv")

# Drop dates before 2012
vio.date <- as.Date( dat.v$Violation.Date, "%m/%d/%Y" )
vpost.2012 <- vio.date > "2011-12-31"
dat.v <- dat.v[ vpost.2012, ]

vio.date <- as.Date( dat.v$Violation.Date, "%m/%d/%Y" )

m.year <- cut( vio.date, breaks="month" )
m.y.name <- format( vio.date, "%b-%Y" )

# table( dat$Complaint.Type, month.year )
dat.v$m.year <- m.year

violation.types <- c("Section 305.3 - Interior surfaces",
                     "Section 27-72 (f) - Overgrowth",
                     "Section 27-72 (e) -Trash & Debris",
                     "325: General",
                     "Section 308.1 - Infestation",
                     "Section 27-32 (d) Protective coating for wood surfaces",
                     "252: General",
                     "Section 27-31 (c) Structural members",
                     "Section 304.13 - Window, skylight and door frames",
                     "Section 27-32 (b) Stairs, porches and railings"
)

#########################################################UI##########################################################


ui <- fluidPage(
  
  fluidRow(
    column( checkboxGroupInput("show_comps", 
                               label = h3("Complaint types:"), 
                               choices = complaint.types),
            title="Complaints Over Time", 
            width=3 ),
    column( plotOutput( "complaints" ),
            width=9 )),
  
  fluidRow(
    column( checkboxGroupInput("show_vios",
                               label= h3("Violation types:"),
                               choices= violation.types),
            title="Violations Over Time",
            width=3 ),
    column( plotOutput( "violations" ),
            width=9 ))
)


######################################################SERVER#########################################################

server <- function(input,output){
  
  output$complaints <- renderPlot({
    
    dat.sub <- dat[ dat$Complaint.Type %in% input$show_comps , ]
    
    # Create chart for a subset of data
    complaint.sub <- tapply( dat.sub$Complaint.Type, dat.sub$month.year, length )
    complaint.sub[ is.na(complaint.sub) ] <- 0
    
    # Set maximum y limit
    complaint.sub.df <- as.data.frame(complaint.sub)
    max.ylim <- round_any((1.1*max(complaint.sub.df[ , 1 ] )), 10, f = ceiling)
    
    # If there is no max y limit, then set it to 1
    if(max.ylim == 0) { 
      max.ylim = 1
      }
    
    # Set pretty names
    pretty.names <- format( as.Date(names(complaint.sub)), "%b-%Y" )
    month.labels <- format( as.Date(names(complaint.sub)), "%b" )
    
    # If month has no complaints, then that month's label is null
    month.labels[ complaint.sub == 0 ] <- ""
    
    # Plot
    plot( complaint.sub, type="b", pch=19, xaxt="n", bty="n", col="steelblue", 
          ylab = "Number of Complaints",
          xlab = "",
          ylim = c(0, max.ylim)
    )
    axis( side=1, at=(1:length(complaint.sub))[c(T,F,F)], labels=pretty.names[c(T,F,F)], cex.axis=0.8, las=2 )
    text( 1:length(complaint.sub), complaint.sub, month.labels, pos=3, cex=0.7 )
    
  })
  
  output$violations <- renderPlot({
    
    vdat.sub <- dat.v[ dat.v$Code %in% input$show_vios , ]
    
    # Create chart for a subset of data
    
    violation.sub <- tapply( vdat.sub$Code, vdat.sub$m.year, length )
    violation.sub[ is.na(violation.sub) ] <- 0
    
    # Set maximum y limit
    
    violation.sub.df <- as.data.frame(violation.sub)
    max.ylim <- round_any((1.1*max(violation.sub.df[ , 1 ] )), 10, f = ceiling)
    
    # If there is no max y limit, then set it to 1
    if(max.ylim == 0) { 
      max.ylim = 1
    }
    
    # Set pretty names
    
    vpretty.names <- format( as.Date(names(violation.sub)), "%b-%Y" )
    vmonth.labels <- format( as.Date(names(violation.sub)), "%b" )
    
    # If month has no violations, then that month's label is null
    vmonth.labels[ violation.sub == 0 ] <- ""
    
    plot( violation.sub, type="b", pch=19, xaxt="n", bty="n", col="goldenrod", 
          ylab = "Number of Violations",
          xlab = "",
          ylim = c(0, max.ylim)
    )
    
    axis( side=1, at=(1:length(violation.sub))[c(T,F,F)], labels=vpretty.names[c(T,F,F)], cex.axis=0.8, las=2 )
    
    text( 1:length(violation.sub), violation.sub, vmonth.labels, pos=3, cex=0.7 )
    
  })
}

#####################################################SHINYapp#######################################################

shinyApp( ui=ui, server=server )