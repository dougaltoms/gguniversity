#' theme_exeter()
#'
#' @return ggplot theme based on University of Exeter branding
#' @export
#'
#' @examples
#' ggplot(iris, aes(x = Petal.Length, fill = Species)) +
#'  geom_density(alpha = 0.8)+
#'  scale_fill_manual(values=exeter_palette(3))+
#'  labs(title="Sepal length vs width", subtitle = "University of Exeter")+
#'  theme_exeter()
#'
#'
theme_exeter <- function() {

  PrimaryColor <- "#003c3c"
  SecondaryColor <- "#00dca5"
  HighlightColor <- "#ed3f37"

  if(!require(showtext)){
    stop("Package 'showtext' not installed")
  }

  font_add_google("Outfit", family="Exeter")
  showtext_auto()



    # Update default geoms

    ggplot2::update_geom_defaults("area", list(colour = PrimaryColor, fill=SecondaryColor))
    ggplot2::update_geom_defaults("abline", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("bar", list(colour = PrimaryColor, fill=PrimaryColor))
    ggplot2::update_geom_defaults("boxplot", list(colour = PrimaryColor, fill=PrimaryColor))
    ggplot2::update_geom_defaults("col", list(colour = PrimaryColor, fill=PrimaryColor))
    ggplot2::update_geom_defaults("density", list(colour = PrimaryColor, fill=PrimaryColor))
    ggplot2::update_geom_defaults("hex", list(colour = PrimaryColor, fill=PrimaryColor))
    ggplot2::update_geom_defaults("hline", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("line", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("path", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("point", list(colour = SecondaryColor))
    ggplot2::update_geom_defaults("rect", list(colour = SecondaryColor, fill=SecondaryColor))
    ggplot2::update_geom_defaults("smooth", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("text", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("vline", list(colour = PrimaryColor))
    ggplot2::update_geom_defaults("violin", list(colour = PrimaryColor, fill=PrimaryColor))

    # Customise plot
    theme <- theme_classic()+
                theme(text = element_text(family = "Exeter", size = 14),
                  plot.title = element_text(family = "Exeter", size = 28,
                                            color = PrimaryColor,hjust = 0),
                  plot.subtitle = element_text(family = "Exeter", size = 18,
                                               color = SecondaryColor,hjust = 0),
                  plot.background = element_blank()
                  )

    # Customise axes
    theme <- theme +
                theme(
                  axis.title = element_text(family = "Exeter", size = 14,
                                            color = PrimaryColor),
                  axis.text = element_text(family = "Exeter", size = 14,
                                            color = PrimaryColor),
                  axis.line.x.bottom =element_line(color = PrimaryColor,
                                                  size = 1),
                  axis.line.y.left = element_line(color = PrimaryColor,
                                                  size = 1)
                  )

    # Customise panel
    theme <- theme +
                theme(
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank()
                )


  }


#' exeter_palette
#'
#' @param n number of colours to generate in palette
#'
#' @return vector of colour
#' @export
#'
#'
exeter_palette <- function(n){

  if (missing(n)){
    n <- 5
  }

  colours <- c("#022020","#003c3c","#007d69","#00c896","#00dca5")
  pal <- grDevices::colorRampPalette(colours)(n)

  return(pal)

}
