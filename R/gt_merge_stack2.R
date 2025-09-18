#' Stacks two columns in a gt object
#'
#' I found this function on stackoverflow, it's super useful
#' @param x a gt object
#' @param col1 the variable on top
#' @param col2 the variable on bottom
#' @keywords gt, stacking
#' @export

gt_merge_stack2 <-
function (gt_object, col1, col2, palette = c("black", "grey"),
    ..., small_cap = TRUE, font_size = c("14px", "10px"), font_weight = c("bold",
        "bold"))
{
    stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
        class(gt_object))
    stopifnot(`There must be two colors` = length(palette) ==
        2)
    stopifnot(`There must be two 'font_size'` = length(font_size) ==
        2)
    stopifnot(`There must be two 'font_weight'` = length(font_weight) ==
        2)
    stopifnot(`'font_size' must be a string with 'px'` = all(grepl(x = font_size,
        pattern = "px")))
    stopifnot(`'font_weight' must be a 'bold', 'normal' or 'lighter'` = font_weight %in%
        c("bold", "normal", "lighter"))
    colors <- scales::col2hcl(palette, ...)
    col1_bare <- rlang::enexpr(col1) %>% rlang::as_string()
    row_name_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] ==
        "stub")]
    data_in <- gt_index(gt_object, column = {
        {
            col2
        }
    })
    data_in <- ifelse(is.na(data_in),"",data_in)
    gt_object %>% text_transform(locations = if (isTRUE(row_name_var ==
        col1_bare)) {
        cells_stub(rows = gt::everything())
    }
    else {
        cells_body(columns = {
            {
                col1
            }
        })
    }, fn = function(x) {
        if (small_cap) {
            font_variant <- "small-caps"
        }
        else {
            font_variant <- "normal"
        }
        glue::glue("<div style='line-height:{font_size[1]}'><span style='font-weight:{font_weight[1]};font-variant:{font_variant};color:{colors[1]};font-size:{font_size[1]}'>{x}</span></div>\n        <div style='line-height:{font_size[2]}'><span style ='font-weight:{font_weight[2]};color:{colors[2]};font-size:{font_size[2]}'>{data_in}</span></div>")
    }) %>% cols_hide(columns = {
        {
            col2
        }
    })
}
