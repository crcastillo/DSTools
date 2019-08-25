#' Determine Beta for 1p Prop Test
#'
#' This function determines the Beta values for a given set of
#' parameters fed into a one sample proportion test. The given
#' number of observations is a list of potential sample sizes.
#'
#' @keywords internal
#'
#' @noRd
fx_Beta <- function(
  siglevel
  , obs
  , alternative
  , nullp
  , altp
){

  #* Load required libraries
  library(pwr) # Will throw an error if pwr cannot be found

  #* Ensure obs is sorted in ascending order
  obs <- sort(obs)

  #* Define effect size
  EffectSize <- pwr::ES.h(
    p1 = altp
    , p2 = nullp
  )

  #* Store vector of Power results
  Power <- unlist(
    lapply(
      1:length(Obs)
      , function(x){
        pwr::pwr.p.test(
          h = EffectSize
          , n = obs[x]
          , sig.level = siglevel
          , power = NULL
          , alternative = alternative
        )$power
      }
    ) # Close lapply
  )

  #* Return Beta - probability of Type II Error - False Negative
  return(1 - Power)

} # Close Function


#' Determine Alpha for 1p Prop Test
#'
#' This function determines the Alpha values for a given set of
#' parameters fed into a one sample proportion test. The given
#' number of observations is a list of potential sample sizes.
#'
#' @keywords internal
#'
#' @noRd
fx_Alpha <- function(
  obs
  , alternative
  , nullp
  , altp
){

  #* Ensure obs is sorted in ascending order
  obs <- sort(obs)

  #* Store vector of Power results
  Alpha <- unlist(
    lapply(
      1:length(obs)
      , function(x){
        stats::prop.test(
          x = altp * obs[x]
          , n = obs[x]
          , p = nullp
          , alternative = alternative
          , correct = FALSE
        )$p.value
      }
    ) # Close lapply
  )

  #* Return Alpha
  return(Alpha)

} # Close Function


#' Determine Alpha/Beta values for 1-sample Proportion Test
#'
#' This function takes a provided set of constraints for a one sample proportion test including a vector of test observation counts
#' and determines the number of observations (from the provided vector) required to surpass the provided Alpha and Beta thresholds.
#' Essentially, this is to combine both significance testing and power analysis.
#'
#' @param siglevel The desired Alpha level, i.e. Type I Error - probability of a false positive
#' @param betalevel The desired Beta level, i.e. Type II Error - probability of a false negative
#' @param obs Vector of potential sample sizes
#' @param alternative The method of determining a difference in provided proportions: "greater", "less", "two.sided"
#' @param nullp The null hypothesis proportion
#' @param altp The alternative hypothesis proportion
#'
#' @return A named list that includes: a data.frame with alpha, beta, and power values for the given vector of observations;
#' a plot that shows the aforementioned data.frame as well as the observation counts that meet the given alpha and beta
#' thresholds; the observation counts that meet the given alpha and beta thresholds.
#'
#' @export
fx_PropTest_1p <- function(
  siglevel
  , betalevel
  , obs
  , alternative
  , nullp
  , altp
){

  #* Load required libraries
  library(dplyr) # Will throw an error if dplyr cannot be found
  library(ggplot2) # Will throw an error if ggplot2 cannot be found


  #* Store Alpha vector
  Alpha <- fx_Alpha(
    obs = obs
    , alternative = alternative
    , nullp = nullp
    , altp = altp
  )

  #* Store Beta vector
  Beta <- fx_Beta(
    siglevel = siglevel
    , obs = obs
    , alternative = alternative
    , nullp = nullp
    , altp = altp
  )

  #* Create dataframe with Alpha & Beta results
  df_Power <- data.frame(
    n = obs
    , alpha = Alpha
    , beta = Beta
    , power = 1 - Beta
  )

  #* Return objects in names list form
  return(
    list(
      df = df_Power

      , plot = ggplot(
        data = df_Power
        , aes(
          x = n
        )
      ) +
        geom_line(
          size = 1.5
          , aes(
            y = alpha
            , color = "Alpha"
          )
        ) +
        geom_line(
          size = 1.5
          , aes(
            y = power
            , color = "Beta"
          )
        ) +
        scale_y_continuous(
          sec.axis = sec_axis(
            trans = ~ 1 - .
            , name = "Beta = Probability of Type II Error (False Negative)"
            , labels = scales::percent
          )
          , limits = c(0, 1)
          , labels = scales::percent
        ) +
        geom_point(
          aes(
            x = min(df_Power$n[siglevel - df_Power$alpha >= 0])
            , y = max(df_Power$alpha[siglevel - df_Power$alpha >= 0])
            , color = paste0("Alpha ~ ", siglevel)
          )
          , shape = 4
          , size = 4
          , stroke = 2
        ) +
        geom_point(
          aes(
            x = min(df_Power$n[betalevel - df_Power$beta >= 0])
            , y = min(df_Power$power[betalevel - df_Power$beta >= 0])
            , color = paste0("Beta ~ ", betalevel)
          )
          , shape = 4
          , size = 4
          , stroke = 2
        ) +
        theme(
          legend.position = "bottom"
        ) +
        scale_color_manual(
          values = c("blue", "deepskyblue1", "red", "lightcoral")
          , name =
        ) +
        xlab("Number of Observations") +
        ylab("Alpha = Probability of Type I Error (False Positive)") +
        labs(
          color = "Legend"
        )

      , min_alpha_obs = min(df_Power$n[siglevel - df_Power$alpha >= 0])

      , min_beta_ob = min(df_Power$n[betalevel - df_Power$beta >= 0])

    ) # Close list

  ) # Close return

} # Close Function
