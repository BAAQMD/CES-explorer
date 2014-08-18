suppressPackageStartupMessages({
  library(shinyIncubator)
  library(dplyr)
  library(tidyr)
  library(CalEnviroScreen)
  library(leaflet)
  library(RColorBrewer)
})

###############################################################################
# Define server logic
###############################################################################

msg <- function (...) if (FALSE) message(...)

shinyServer(function(input, output, session) {

  output$map <- reactive(TRUE)
  map <- createLeafletMap(session, "map")

  # session$onFlushed is necessary to work around a bug in the Shiny/Leaflet
  # integration; without it, the addCircle commands arrive in the browser
  # before the map is created.
  session$onFlushed(once=TRUE, function() {
    paintObs <- observe({
      withProgress(session, {
        setProgress(message = "Updating, please wait",
                    detail = "Calculating scores ...")

        dt <- .impacted_scores()
        i <- dt$FIPS

        x <- unname(unlist(poly_x[i]))
        y <- unname(unlist(poly_y[i]))

        ids <- unlist(poly_id[i])
        col <- color_ramp(dt$Range)

        setProgress(detail = "Rendering map ...")
        map$clearShapes()
        # Bug in Shiny causes this to error out when user closes browser
        # before we get here
        try({
          map$addPolygon(y, x,
                         layerId = seq(1, length(na.omit(x))), #seq_along(ids),
                         options = lapply(col, function(x) list(fillColor = x)),
                         defaultOptions = defaultOptions)
        })
      })
    })
    # TIL this is necessary in order to prevent the observer from
    # attempting to write to the websocket after the session is gone.
    session$onSessionEnded(paintObs$suspend)

  })

  .region_boundary <- reactive({
    msg(".region_boundary()")
    region <- CA_regions[[input$region_name]]
    region$boundary
  })

  .region_FIPS <- reactive({
    msg(".region_FIPS()")
    CA_regions[[input$region_name]]$FIPS
  })

  .selected_variables <- reactive({
    msg(".selected_variables()")
    c(input$pollution_vars, input$popchar_vars)
  })

  .pollution_weights <- reactive({
    msg(".pollution_weights()")
    c(Ozone=input$Ozone, PM25=input$PM25, DieselPM=input$DieselPM, DrinkWat=input$DrinkWat, PestUse=input$PestUse, ToxRel=input$ToxRel, Traffic=input$Traffic,
      Cleanup=input$Cleanup, GndWat=input$GndWat, HazWaste=input$HazWaste, ImpWat=input$ImpWat, SolWaste=input$SolWaste)
  })

  .popchar_weights <- reactive({
    msg(".popchar_weights()")
    c(Age=input$Age, Asthma=input$Asthma, LBW=input$LBW, Edu=input$Edu, LingIso=input$LingIso, Poverty=input$Poverty, Unemp=input$Unemp)
  })

  .weight_tbl <- reactive({
    msg(".weight_tbl()")
    w <- c(.pollution_weights(), .popchar_weights())
    msg("w is: ", dput(w))
    msg("names(w) is: ", dput(names(w)))
    result <- as.tbl(data.frame(Variable = names(w), Weight = w))
    return(result)
    })

  .subscore_tbl <- reactive({
    msg("entering .subscore_tbl()")

    if (length(.selected_variables()) == 0) {
      subscores <- as.tbl(data.frame(FIPS=character(0), Pollution=numeric(0), PopChar=numeric(0)))
    } else {
      min_obs <- Reduce(min, c(4, length(input$pollution_vars), length(input$popchar_vars)))
      subscores <- CES2_tbl %>%
        inner_join(.weight_tbl(), by = "Variable") %>%
        filter(Variable %in% .selected_variables()) %>%
        group_by(FIPS, Group) %>%
        compute_CES2_subscores(min_obs = min_obs)
    }

    msg("exiting .subscore_tbl()")
    return(subscores)
  })

  .score_tbl <- reactive({
    msg(".score_tbl()")
    if (input$method == "CES 2.0") {
      subscores <- .subscore_tbl()
      if (is.null(subscores$PopChar)) {
        subscores$PopChar <- 1
      } else {
        if (is.null(subscores$Pollution))
          subscores$Pollution <- 1
      }
      scores <- subscores %>% compute_CES2_scores()
    } else {
      cut_quantile <- function (x, ...) {
        q <- quantile(x, seq(0, 1, len=21))
        cut(x, breaks = q, labels = names(q)[-1])
      }
      summarise_rank_product <- function (.data) {
        .data %>%
          filter(!is.na(Value)) %>%
          group_by(Variable) %>%
          mutate(Rank = rank(-Value) + 1, Frac = Rank / n()) %>%
          ungroup() %>%
          group_by(FIPS) %>%
          dplyr::summarise(Score = mean(-log(Frac))) %>%
          mutate(Percentile = 100 * normalize(rank(Score)),
                 Range = cut_quantile(Score, n=20))
      }
      scores <- CES2_data %>%
        gather(Variable, Value, -FIPS) %>%
        filter(Variable %in% .selected_variables()) %>%
        summarise_rank_product()
    }
    scores %>% arrange(desc(Score)) %>% with_region()
  })

  .impacted_percentile <- reactive({
    msg(".impacted_percentile()")
    100 - extract_numeric(input$impacted_percentile) # reverse the scale
  })

  .pollution_maximum <- reactive({
    msg(".pollution_maximum()")
    with(.score_tbl(), max(Pollution, na.rm=TRUE))
  })

  .popchar_maximum <- reactive({
    msg(".popchar_maximum()")
    with(.score_tbl(), max(PopChar, na.rm=TRUE))
    })

  .score_cutoff <- reactive({
    msg(".score_cutoff()")
    with(.score_tbl(), quantile(Score, .impacted_percentile() / 100, na.rm=TRUE))
    })

  .popchar_intercept <- reactive({
    msg(".popchar_intercept()")
    .score_cutoff() / .pollution_maximum()
    })

  .tally <- reactive({
    msg(".tally()")
    .score_tbl() %>%
      group_by(Region) %>%
      dplyr::summarise(Tracts=n(), Yes=sum(Percentile > .impacted_percentile()), No=Tracts-Yes)
  })

  .scatterplot <- reactive({
    msg(".scatterplot()")
    fig_tbl <- .score_tbl() %>% mutate(Sampled = as.logical(rbinom(n(), 1, prob = input$SampleTracts / 100)))
    cutoff_function <- function (x) {
      ifelse(x < .popchar_intercept() | x > .popchar_maximum(), NA, .score_cutoff() / x)
    }
    fig <- ggplot(fig_tbl, aes(PopChar, Pollution)) +
      coord_equal() + scale_x_score("Population Characteristics\n") + scale_y_score("Pollution Burden") +
      scale_color_regions() + scale_fill_regions() +
      geom_abline(slope=1, intercept=0, alpha=0.15) +
      geom_point(aes(color=Region), alpha=I(0.3), size=I(2), subset=.(Sampled)) +
      scale_alpha(range=c(0.3, 0.6)) +
      stat_function(fun=cutoff_function, geom="line", linetype="dashed", alpha=0.7) +
      annotate("text", x=.popchar_intercept(), y=.pollution_maximum(), label=str_c("Top ", 100 - .impacted_percentile(), "%"), hjust=1.1, vjust=1.1) +
      guides(alpha=FALSE, color=guide_legend("Region", override.aes = list(alpha = 0.5)), fill=FALSE)
    if (input$DensityPath) {
      fig <- fig + stat_density2d(aes(color=Region, alpha=..level..), subset=.(Region != "Other"))
    }
    if (input$DensityFill) {
      fig <- fig + stat_density2d(aes(fill=Region), alpha=I(0.03), color=NA, geom="polygon", subset=.(Region != "Other"))
    }
    fig <- fig + theme(legend.position="bottom", legend.direction="horizontal")
    return(fig)
  })

  #.barchart <- reactive({
  #  require(reshape2)
  #  .tally() %>%
  #    reshape2::melt(measure.vars = c("No", "Yes"), variable.name = "Impacted", value.name = "Freq") %>%
  #    transform(Frac = Freq / Tracts) %>%
  #    ggplot(aes(x=Region, y=Freq)) +
  #    geom_bar(aes(fill=Impacted), stat="identity") +
  #    scale_fill_manual(values=c(gray(0.7), gray(0.4))) +
  #    scale_y_continuous(limits=c(0, 4500), expand=c(0, 0)) +
  #    geom_text(aes(y=Tracts, label=str_c(percent(round(Yes/Tracts, 2)), " (n=", Yes, ")")), data=.tally(), vjust=-0.5) +
  #    theme(legend.position="none", axis.title=element_blank())
  #})

  .impacted_scores <- reactive({
    msg(".impacted_scores()")
    .score_tbl() %>% filter(Percentile > .impacted_percentile())
  })

  .impacted_FIPS <- reactive({
    msg(".impacted_FIPS()")
    .impacted_scores()$FIPS
  })

  output$tally <- renderDataTable(.tally())
  output$scatterplot <- renderPlot(show(.scatterplot()))
  output$barchart <- renderPlot(show(.barchart()))

  output$pctl_tbl <- renderDataTable(pctl_tbl, options = list(bSortClasses=TRUE, iDisplayLength=10))
  output$data_tbl <- renderDataTable(
    if (input$show_percentiles) {
      inner_join(pctl_tbl, .score_tbl(), by = "FIPS")
    } else {
      .score_tbl()
    }, options = list(bSortClasses=TRUE, iDisplayLength=10))

  output$subscore_tbl <- renderDataTable(.subscore_tbl())

  output$download_csv <- downloadHandler(
    filename = function () {
      return('CES-explorer.csv')
    },
    content = function (file) {
      write.csv(.score_tbl(), file, row.names = FALSE)
    }
  )

  output$download_shp <- downloadHandler(
    filename = function () {
      return("CES-explorer.zip")
    },
    content = function (file) {
      dsn <- file.path(tempdir(), "CES-explorer")
      dir.create(dsn)
      df <- as.data.frame(.score_tbl() %>% select(FIPS, Score, Range))
      df <- mutate(df, Range = as.character(Range))
      row.names(df) <- df$FIPS
      spobj <- suppressWarnings(merge(CA_tracts, df))
      #row.names(spobj) <- spobj$FIPS
      writeOGR(spobj, dsn, layer = "scores", driver = "ESRI Shapefile", overwrite_layer = TRUE)
      old_wd <- getwd()
      setwd(dsn)
      zip(zipfile = "CES-explorer.zip", files = dir(dsn))
      setwd(old_wd)
      file.copy(file.path(dsn, "CES-explorer.zip"), file)
      file.remove(dir(dsn, full.names = TRUE))
      file.remove(dsn)

    }
  )

})
