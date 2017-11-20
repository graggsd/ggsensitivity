#' Generate a grid of Odds Ratios
#'
#' A function to compare odds ratios, visualize their effect size, and
#' detect significance between different testing conditions.
#'
#' @param dat The input data
#' @param x Column mapping to x
#' @param y Column mapping to y
#' @param OR Column containing odds ratios
#' @param p Column in p-values
#' @param alpha The significance cutoff
#' @param breaks Break points for color-coding effect sizes
#' @param label_size The size of labels indicating effect size
#' @param coord_flip Whether or not to flip the coordinates
#' @return Returns a grid color-coded and labeled by effect size
#' and indicating significance
#' @export
#' @seealso \code{\link[ggsensitivity]{OR_grid_large}}
OR_grid <- function(dat,
                    x,
                    y,
                    OR = "OR",
                    p = "p",
                    alpha = 0.05,
                    breaks = NULL,
                    label_size = NULL,
                    coord_flip = FALSE) {

    if (is.null(breaks)) {
        breaks <- get_OR_breaks(dat[, OR])
    }

    dat <- add_OR_meta(dat,
                       OR = OR,
                       p = p,
                       alpha = alpha,
                       breaks = breaks)

    if (is.factor(dat[, x])) {
        if (coord_flip) {
            dat[, x] <- factor(dat[, x], rev(levels(dat[, x])))
        } else {
            dat[, y] <- factor(dat[, y], rev(levels(dat[, y])))
        }
    }

    plot <- dat %>%
        ggplot(aes_string(x = x, y = y)) +
        theme_linedraw() +
        theme(panel.grid.major = element_line("lightgrey"),
              panel.grid.minor = element_blank()) +
        geom_tile(aes(fill=OR.cat), color = "black") +
        scale_fill_brewer(palette = "PRGn", name = "Odds Ratio")

    if (!is.null(label_size)) {
        plot <- plot +
            geom_text(aes_string(label = OR,
                                 col = "Significant"),
                      size = as.numeric(label_size))
    } else {
        plot <- plot + geom_text(aes_string(label = OR, col = "Significant"))
    }

    plot <- plot +
        scale_color_manual(values = c("black", "red")) +
        theme(axis.text.x = element_text(angle = 90,
                                         hjust=1,
                                         vjust = 0.5),
              axis.title = element_blank())

    if (coord_flip) {
        plot <- plot + coord_flip()
    }
    return(plot)
}


#' Generate an htmlTable from tidy data
#'
#' A wrapper script for \code{htmlTable} that will build an htmlTable using
#' tidy data and mapping elements of the table to specific columns.
#'
#' @inheritParams OR_grid
#' @return Returns a grid color-coded by effect size indicating significance
#' with red asterix. This table is better suited to presenting larger numbers
#' of outcomes than \code{OR_grid}
#' @export
#' @seealso \code{\link[ggsensitivity]{OR_grid}}
OR_grid_large <- function(dat,
                          x,
                          y,
                          OR = "OR",
                          p = "p",
                          alpha = 0.05,
                          breaks = NULL,
                          label_size = NULL,
                          coord_flip = FALSE) {


    if (is.null(breaks)) {
        breaks <- get_OR_breaks(dat[, OR])
    }

    if (is.factor(dat[, x])) {
        if (coord_flip) {
            dat[, x] <- factor(dat[, x], rev(levels(dat[, x])))
        } else {
            dat[, y] <- factor(dat[, y], rev(levels(dat[, y])))
        }
    }

    dat <- add_OR_meta(dat,
                       OR = OR,
                       p = p,
                       alpha = alpha,
                       breaks = breaks,
                       big = TRUE)

    p <- dat %>%
        ggplot(aes_string(x = x, y = y, label = "Significant")) +
        theme_linedraw() +
        theme(panel.grid.major = element_line("lightgrey"),
              panel.grid.minor = element_blank())+
        geom_tile(aes(fill=OR.cat), color = "black") +
        scale_fill_brewer(palette = "PRGn", name = "Odds Ratio") +
        guides(col = FALSE) +
        theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5),
              axis.title = element_blank())

    if (!coord_flip) {
        p <- p +
            geom_text(col = "red") +
            theme(legend.position = "top") +
            guides(fill = guide_legend(nrow = 1,
                                       label.position = "bottom",
                                       title.position = "top"))

    } else  {
        p <- p +
            geom_text(col = "red", angle = 90) +
            coord_flip()
    }

    return(p)
}


add_OR_meta <- function(dat,
                        OR = "OR",
                        p = "p",
                        alpha = 0.05,
                        breaks = c(0, .3, .7, 1, 2, 5, Inf),
                        big = FALSE) {

    dat[, "OR.cat"] <- cut(dat[, OR], breaks = breaks)
    if (!big) {
        dat[, "Significant"] <- ifelse(dat[,p] < alpha, "Yes", "No")
    } else {
        dat[, "Significant"] <- ifelse(dat[,p] < alpha, "*", "")
    }
    dat[, OR] <- round(dat[, OR], 1)

    big.idx <- which(dat[, OR] > 99)
    small.idx <- which(dat[, OR] < 0.1)

    dat[big.idx, OR] <- ">99"
    dat[small.idx, OR] <- "<0.1"

    return(dat)
}

get_OR_breaks <- function(x, n.categories = 3, midpoint = 1) {

    bottom_breaks <- quantile(x[which(x < midpoint)],
                              probs = seq(0, 1, 1/n.categories))
    bottom_breaks <- bottom_breaks[-c(1, length(bottom_breaks))]
    top_breaks <- quantile(x[which(x > midpoint)],
                           probs = seq(0, 1, 1/n.categories))
    top_breaks <- top_breaks[-c(1, length(top_breaks))]
    return(c(0, bottom_breaks, 1, top_breaks, Inf))
}


