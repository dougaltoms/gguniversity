#' theme_oxford()
#'
#' @return ggplot theme based on University of Oxford branding
#' @export
#'
#' @examples
#' ggplot(iris, aes(x = Petal.Length, fill = Species)) +
#'  geom_density(alpha = 0.8)+
#'  scale_fill_manual(values=oxford_palette(3))+
#'  labs(title="Sepal length vs width", subtitle = "University of Oxford")+
#'  theme_oxford()
#'
#'
theme_oxford <- function() {

  PrimaryColor <- "#041e42"
  SecondaryColor <- "#418fde"
  HighlightColor <- "#862633"

  if(!require(showtext)){
    stop("Package 'showtext' not installed")
  }

  font_add_google("PT Serif", family="Oxford")
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
    theme(text = element_text(family = "Oxford", size = 14),
          plot.title = element_text(family = "Oxford", size = 28,
                                    color = PrimaryColor,hjust = 0),
          plot.subtitle = element_text(family = "Oxford", size = 18,
                                       color = SecondaryColor,hjust = 0),
          plot.background = element_blank()
    )

  # Customise axes
  theme <- theme +
    theme(
      axis.title = element_text(family = "Oxford", size = 14,
                                color = PrimaryColor),
      axis.text = element_text(family = "Oxford", size = 14,
                               color = PrimaryColor),
      axis.line.x.bottom =element_line(color = PrimaryColor,
                                       size = 1),
      axis.line.y.left = element_line(color = PrimaryColor,
                                      size = 1),
      axis.ticks = element_line(color = PrimaryColor)
    )

  # Customise panel
  theme <- theme +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )

}


#' oxford_palette
#'
#' @param n number of colours to generate in palette
#'
#' @return vector of colour
#' @export
#'
#'
oxford_palette <- function(n){

  if (missing(n)){
    n <- 5
  }

  colours <- c("#001c3d","#193658","#3277ae","#70a9d6","#a1c4d0")
  pal <- grDevices::colorRampPalette(colours)(n)

  return(pal)

}
