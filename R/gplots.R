#' A custom theme inspired by theme_ipsum
#'
#' This function print all rows of a data frame
#' @keywords ggplot2, theme
#' @importFrom ggplot2 theme element_text element_blank margin element_line ggplot geom_point aes
#' @export

gtheme <- function(base_family = "Arial Narrow", base_size = 14, legend = FALSE){
    thm = theme(axis.title.x = element_text(hjust = 1, size = base_size*.90, family = base_family))
    thm = thm + theme(axis.title.y = element_text(hjust = 1, size = base_size*.90, family = base_family))
    thm = thm + theme(axis.line.x = element_line(color = "#cccccc", size = 0.15))
    thm = thm + theme(axis.line.y = element_line(color = "#cccccc", size = 0.15))
    thm = thm + theme(axis.ticks = element_blank())
    thm = thm + theme(axis.text.x = element_text(size = base_size*.90, margin = margin(t = 0)))
    thm = thm + theme(axis.text.y = element_text(size = base_size*.90, margin = margin(r = 0)))
    thm = thm + theme(legend.background = element_blank())
    thm = thm + theme(legend.key = element_blank())
    thm = thm + theme(panel.background = element_blank())
    thm = thm + theme(panel.border = element_blank())
    thm = thm + theme(plot.background = element_blank())
    thm = thm + theme(legend.title = element_blank())
    if(!legend){
        thm = thm + theme(legend.position = "none")
    }
    thm = thm + theme(panel.grid = element_blank())
    thm = thm + theme(panel.grid.major.x = element_blank())
    thm = thm + theme(panel.grid.major.y = element_blank())
    thm = thm + theme(panel.grid.minor.x = element_blank())
    thm = thm + theme(panel.grid.minor.y = element_blank())
    thm = thm + theme(plot.title = element_text(face = "plain", hjust = 0, size = base_size*1.3))
    thm = thm + theme(plot.caption = element_text(hjust = 0, size = base_size*.75, margin = margin(t = 0), family = base_family, face = "italic"))
    thm = thm + theme(strip.background = element_blank())
    thm = thm + theme(strip.text = element_text(hjust = 0, face = "plain", size = base_size*.90, family = base_family))
    thm
}

#' Some custom color palettes
#'
#' Tired of ugly default colors in ggplot
#' @param
#' @keywords color, fill manual
#' @export
#' @examples
#' gcolors$p1

gcolors <- list(
  google = c("#4885ed","#3cba54","#f4c20d","#db3236"),
  p1 = c("#F0E4D7","#F5C0C0","#FF7171","#9FD8DF"),
  p2 = c("#393E46","#00ADB5","#EEEEEE","#222831"),
  p3 = c("#F9ED69","#F08A5D","#B83B5E","#6A2C70"),
  p4 = c("#2D4059","#EA5455","#F07B3F","#FFD460"),
  p5 = c("#FFE162","#FF6464","#91C483","#EEEEEE"),
  p6 = c("#51C4D3","#D8E3E7","#4885ed","#6A2C70")
)


