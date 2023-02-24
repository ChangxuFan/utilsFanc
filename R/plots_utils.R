plot.name.construct <- function(outlist, root.name.internal = NULL, sub.name = NULL) {
  plot.file=outlist$plot.file
  plot.dir=outlist$plot.dir
  root.name.external=outlist$root.name.external
  # root.name.internal=outlist$root.name.internal
  # sub.name=outlist$sub.name

  if (!is.null(plot.file))
    return(plot.file)
  if (! is.null(plot.dir) && !is.null(sub.name) ) {
    if (is.null(root.name.external) && is.null(root.name.internal))
      root.name <-""
    else
      root.name <- ifelse(!is.null(root.name.external), root.name.external, root.name.internal)
    system(paste0("mkdir -p ", plot.dir))
    return(paste0(plot.dir, "/", root.name, sub.name))
  }
  stop("more info needed to generate output file!")
}

getDensity <- function (x = NULL, y = NULL, n = 100, sample = NULL, densityMax = 0.95) {
    # copied from ArchR::.getDensity
    df <- data.frame(x = x, y = y)
    dens <- MASS::kde2d(x = x, y = y, n = n)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    df$density <- dens$z[ii]
    df$density[df$density > quantile(unique(df$density), densityMax)] <- quantile(unique(df$density),
        densityMax)
    if (!is.null(sample)) {
        df <- df[sample(nrow(df), min(sample, nrow(df))), ]
    }
    return(df)
}

png.wrap <- function (pngs, add.title = F,
                      plot.out, height, width, ...) {
  if (is.null(names(pngs))) {
    names(pngs) <- basename(pngs) %>% sub(".png", "",.)
  }
  rl = lapply(pngs, png::readPNG)
  if (add.title) {
    gl <- lapply(names(rl), function(name) {
      r <- rl[[name]]
      g <- grid::rasterGrob(r)
      title <- grid::textGrob(name, x = 0.5, y = 0.9 )
      tree <- grid::gTree(children = grid::gList(g, title))
      return(tree)
    })
  } else {
    gl = lapply(rl, grid::rasterGrob)
  }

  p <- gridExtra::grid.arrange(grobs = gl)
  system(paste0("mkdir -p ", dirname(plot.out)))
  ggsave(plot.out, p, width = width, height = height, ...)
  return(p)
}

# barplot.pub.1 <- function(df, x, y, fill.by, shape.by = NULL,
#                           pt.size = 0.3,
#                           jitter.width = 0.25) {
#   if (is.null(shape.by)) {
#     shape.by <- "shape"
#     df$shape <- "shape"
#   }
#   suppressMessages(library(ggpubr))
#   p <- ggbarplot(
#     data = df, x = x, y = y,
#     fill = fill.by, position = position_dodge(width = 0.75),
#     color = "black", alpha = 0.5, add = "mean_sd",
#     add.params = list(size = 0.2), size = 0.2) +
#     geom_point(
#       aes_string(fill = fill.by, shape = shape.by),
#       position = position_jitterdodge(
#         dodge.width = 0.8, jitter.width = jitter.width, jitter.height = 0),
#       size = pt.size, color = "grey25")
#   return(p)
# }
# barplot.pub.2 <- function(df, x, y,
#                           pt.size = 0.8, stroke.size = 0.4,
#                           spread.seed = 0, spread.width = 0.75,
#                           darken.by = 0.3) {
#   suppressMessages(library(ggnewscale))
#   line.size <- 0.2
#   if (!is.factor(df[, x])) {
#     df[, x] <- as.factor(df[, x])
#   }
#   df$fac <- as.numeric(df[, x])
#
#   df <- df %>% split(., f= .$fac) %>%
#     lapply(function(x) {
#       spread.pos <- round(
#         spread.width*(1:nrow(x))/nrow(x) - spread.width/2 - spread.width/(2 * nrow(x)), digits = 2)
#       set.seed(seed = spread.seed)
#       x$spread <- x$fac + sample(spread.pos, size = length(spread.pos), replace = T)
#       return(x)
#     }) %>% do.call(rbind, .)
#
#   fill.by <- x
#   cats <- df[, fill.by] %>% unique() %>% sort()
#   color.map <- utilsFanc::color.hue.R4(n = length(cats))
#   color.dark <- colorspace::darken(color.map, amount = darken.by)
#   names(color.map) <- cats
#   names(color.dark) <- cats
#
#   df.sum <- df %>% dplyr::group_by(!!as.name(x)) %>%
#     dplyr::summarise(mean = mean(!!as.name(y)),
#                      sd = sd(!!as.name(y))) %>%
#     dplyr::ungroup() %>% as.data.frame()
#   p <- ggplot(df.sum, aes_string(x = x, y = "mean", fill = fill.by)) +
#     geom_bar(stat = "identity", position = position_dodge(width = 0.75),
#              size = line.size, alpha = 1, color = "black") +
#     scale_fill_manual(values = color.map)
#
#   p <- p + new_scale_fill()
#   p <- p + geom_point(
#     data = df,
#     aes_string(x = "spread", y = y, fill = fill.by), alpha = 1,
#     shape = 21, stroke = stroke.size,
#     # position = position_jitterdodge(
#     #   dodge.width = 0.75, jitter.width = jitter.width, jitter.height = 0, seed = jitter.seed),
#     size = pt.size, color = "black", show.legend = F) +
#     scale_fill_manual(values = color.dark)
#   p <- p + geom_errorbar(
#     data = df.sum,
#     aes(ymin=mean-sd, ymax=mean+sd),
#     position = position_dodge(width = 0.75),
#     width = 0.4, size = line.size)
#
#   # p <- ggbarplot(
#   #   data = df, x = x, y = y,
#   #   fill = fill.by, position = position_dodge(width = 0.75),
#   #   color = "black", alpha = 0.5, add = "mean_sd",
#   #   add.params = list(size = 0.2), size = 0.2) +
#
#   # return(p)
#   return(p)
# }


barplot.pub.3 <- function(df, x, y, color.by = NULL,
                          shape.by = NULL, black.dots = F, jitter.dots = F,
                          palette.fc = "R4.fc2",
                          genomic.x = NULL, genomic.rescale = T, genomic.scale.to = NULL,
                          bar.width = 0.9, dodge.width = 1,
                          pt.size = 1.4, stroke.size = 0.4,
                          bar.line.size = 0.8,
                          error.bar.line.size = 0.2,error.bar.width = 0.6,
                          spread.seed = 0, spread.width = 0.5, spread.bin.size = NULL,
                          add.pval = F, pval.adjust = NULL,
                          pval.group.1, pval.group.2, pval.use.star = T,
                          pval.bar.y.nudge = 0.08, pval.text.y.nudge = 0.14,
                          pval.same.y = F,
                          ...) {
  # dodge.width is the width of all dodging bars together.
  # fac initially meant factorized x. But now it's just x converted to numbers.
  # genomic.x: convert x to genomic coordinates. accepts something like:
  # df <- data.frame(c("Ly49f", "Ly49d", "Ly49h"), c(129001002, 129008007, 129888888));
  # colnames(df) <- c(x, "coord")
  # genomic.scale.to: by default, if 10 regions are present, their genomic locations will be
  # linearly scaled to 1-10.
  suppressMessages(library(ggnewscale))
  n.x <- df[, x] %>% unique() %>% length()
  if (!is.null(genomic.x) && n.x > 1) {
    utilsFanc::check.intersect(c(x, "coord"), "required columns for genomic.x",
                               colnames(genomic.x), "colnames(genomic.x)")
    utilsFanc::check.intersect(sort(unique(df[, x])), "x",
                               genomic.x[, x], "genomic.x[, x]")
    dups <- genomic.x[, x] %>% .[duplicated(.)]
    if (length(dups) > 0) {
      stop(paste0("some of the elements in genomic.x$", x, " is duplicated: ",
                  paste0(unique(dups), collapse = "; ")))
    }
    df <- dplyr::left_join(df, genomic.x, by = x)
    df <- utilsFanc::change.name.fanc(df = df, cols.from = "coord", cols.to = "fac")
    if (genomic.rescale) {
      if (is.null(genomic.scale.to)) {
        genomic.scale.to <- n.x
      }
      gs <- genomic.scale.to
      Min <- min(df$fac)
      Max <- max(df$fac)
      df$fac <- round((df$fac - Min) * (gs - 1)/(Max - Min) + 1, digits = 2)
      df$fac <- df$fac - 1
    }

  } else {
    if (!is.factor(df[, x])) {
      df[, x] <- as.factor(df[, x])
    }
    df$fac <- as.numeric(df[, x])
  }

  if (is.null(color.by)) {
    color.by <- "color"
    df$color <- df[, x]
  }
  if (!is.factor(df[, color.by])) {
    df[, color.by] <- as.factor(df[, color.by])
  }
  df <- df %>% split(., f= .$fac) %>%
    lapply(function(df) {
      n.cats <- length(unique(df[, color.by]))
      m <- median(1:n.cats)
      edge <- -0.5 * dodge.width + 0.5 * dodge.width/n.cats
      nudge.df <- data.frame(
        sort(unique(df[, color.by])),
        seq(from = edge, to = -1 * edge, length.out = n.cats))
      colnames(nudge.df) <- c(color.by, "nudge")
      df <- dplyr::left_join(df, nudge.df, by = color.by)
      df$spread <- df$fac + df$nudge
      df <- df %>% split(., f = as.character(.[, color.by])) %>%
        lapply(function(df) {
          if (is.null(spread.bin.size)) {
            df$bin <- 1
          } else {
            Min <- min(df[, y])
            df$bin <- floor((df[,y] - Min)/spread.bin.size) + 1
          }
          df <- df %>% split(., f = .$bin) %>%
            lapply(function(df) {
              spread.pos <- round(
                spread.width*(1:nrow(df))/nrow(df) - spread.width/2 - spread.width/(2 * nrow(df)), digits = 2)
              set.seed(seed = spread.seed)
              df$spread <- df$spread + sample(spread.pos, size = length(spread.pos), replace = F)
              return(df)
            }) %>% do.call(rbind, .)
          return(df)
        }) %>% do.call(rbind, .)
      return(df)
    }) %>% do.call(rbind, .)

  cats <- df[, color.by] %>% unique() %>% sort()
  color.map <- utilsFanc::color.hue.fc(n = length(cats), palette = palette.fc)
  names(color.map) <- cats

  df.sum <- df %>% dplyr::group_by(fac, !!as.name(color.by)) %>%
    dplyr::summarise(mean = mean(!!as.name(y)),
                     sd = sd(!!as.name(y))) %>%
    dplyr::ungroup() %>% as.data.frame()
  p <- ggplot(df.sum, aes_string(x = "fac", y = "mean", color = color.by, fill = color.by))
  bar.params <- list(
    stat = "identity", position = position_dodge(width = dodge.width),
    size = bar.line.size, width = bar.width
  )
  if (jitter.dots) {
    # bar.params$fill <- "white"
  }
  p <- p + do.call("geom_bar", bar.params) +
    scale_color_manual(values = color.map) +
    scale_fill_manual(values = alpha(color.map, 0.3))

  if (jitter.dots) {
    p <- p + new_scale_fill() +
      geom_point(
        data = df,
        aes_string(x = "fac", y = y, color = color.by),
        position = position_jitterdodge(dodge.width = dodge.width),
        size = pt.size,
        stroke = 0, shape = 16,
        show.legend = F)
  } else {
    point.params <- list(
      data = df,
      aes_string(x = "spread", y = y, fill = color.by, shape = shape.by), alpha = 1,
      shape = 21, stroke = stroke.size,
      # position = position_jitterdodge(
      #   dodge.width = 0.75, jitter.width = jitter.width, jitter.height = 0, seed = jitter.seed),
      size = pt.size, color = "black", show.legend = F)
    if (!is.null(shape.by)) {
      point.params$shape <- NULL
      point.params$show.legend <- T
    }
    if (black.dots) {
      point.params$shape <- 19
    }
    if (pt.size < 0.4) {
      point.params$shape <- 16
      point.params$stroke <- 0
      # I thought this would make the points smaller.. but didn't really work.
    }

    p <- p + new_scale_fill() +
      do.call("geom_point", point.params) +
      scale_fill_manual(values = color.map)
  }

  p <- p +
    geom_errorbar(
    data = df.sum,
    aes(x = fac, ymin = mean-sd, ymax=mean+sd),
    position = position_dodge(width = dodge.width),
    width = error.bar.width, size = error.bar.line.size, color = "black")

  p <- p + scale_x_continuous(breaks = unique(df$fac), labels = unique(df[, x]),
                              expand = expansion(add = 0.2))

  if (add.pval) {
    pval.df <- pvalue.cal(df = df, adjust = pval.adjust,
                          x = "spread", y = y,
                          split.by = x, group.by = color.by,
                          group.1 = pval.group.1, group.2 = pval.group.2,
                          pval.bar.y.nudge = pval.bar.y.nudge,
                          pval.text.y.nudge = pval.text.y.nudge, ...)
    if (pval.same.y) {
      pval.df$y.bar <- max(pval.df$y.bar)
      pval.df$y.text <- max(pval.df$y.text)
    }
    p <- p + geom_segment(
      data = pval.df,
      aes(x = x.start, xend = x.end, y = y.bar, yend = y.bar), size = 0.2,
      inherit.aes = F)

    p <- p + geom_text(
      data = pval.df,
      aes_string(x = "x.mid", y = "y.text",
                 label = ifelse(pval.use.star, "star", "p.sci")),
      family = "Arial", size = 2,
      inherit.aes = F
    )
  }
  return(p)
}



theme.fc.1 <- function(p, text.size = 6, rm.x.ticks = F, rotate.x.45 = F,
                       italic.x = T, no.guides.order = F) {
  p <- p + theme_classic() +
    theme(text = element_text(size = text.size, family = "Arial", color = "black"),
          # legend.position = "none",
          legend.position = "bottom",
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "in"),
          legend.box.margin = margin(t = -0.12, r = -0.1, b = 0, l = -0.1, unit = "in"),
          legend.background = element_blank(),
          legend.spacing = unit(0.02, "in"),
          legend.key.size = unit(0.05, "in"),
          legend.box = "vertical",
          legend.title = element_text(size = text.size, family = "Arial"),
          axis.title = element_blank(),
          axis.text.x = element_text(
            face = ifelse(italic.x, "italic", "plain"), size = text.size, family = "Arial", color = "black"),
          axis.text.y = element_text(
            size = text.size, family = "Arial", color = "black"),
          axis.line.x.bottom = element_line(size = 0.2),
          axis.line.y.left = element_line(size = 0.2),
          axis.ticks.x = element_line(size = 0.2),
          axis.ticks.length.x = unit(ifelse(rm.x.ticks, 0, 0.02), "in"),
          axis.ticks.y = element_line(size = 0.2),
          axis.ticks.length.y = unit(0.02, "in"),
          plot.margin = unit(c(0.04, 0.04, 0.01, 0), units = "in"))
  if (rotate.x.45) {
    p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  }
  if (!no.guides.order)
  p <- p + guides(fill = guide_legend(order=1),
           shape = guide_legend(order=2))
  return(p)
}

pvalue.cal <- function(df, external.p.df = NULL, adjust = NULL,
                       x, y, split.by = NULL, splits = NULL,
                       group.by, group.1, group.2,
                       ymax = NULL, nudge.by.fraction = T,
                       pval.bar.y.nudge = 0.05,
                       pval.text.y.nudge = 0.1) {
  # the idea is to compare group.1 vs group.2 using a t.test().
  # note we are testing group.2 against group.1.
  # sometimes we need to split the data first (for example, when we have multiple genes)
  # alternatively, you can put external pvalues into a dataframe and feed it in...
  # but do note that external.p.df needs to have the required columns:
  # eg. external.p.df <- data.frame(marker = "Ly49A", group.1 = WT, group.2 = "KO", p = 0.001)
  # here "marker" is also split.by.

  # x is used to calculate where the 2 ends of the pvalue bar should land.
  # y is used to calculate where the bar should be placed in terms of the y axis.
  # y is also the value to be compared
  # eg: group.by = "genotype", group.1 = "WT", group.2 = "KO"
  # scientific.thresh: p values smaller than this will be converted to scientific notations.
  # nudge values: the bars and texts should be higher than points!
  # they are specified as fractions of the y range (specified by ymax). when ymax
  # is missing, it's calculated from the data.
  if (!is.null(external.p.df)) {
    utilsFanc::check.intersect(
      c("group.1", "group.2", "p"), "required columns (group.1, group.2, p)",
      colnames(external.p.df), "colnames(external.p.df)")
  }
  if (is.null(split.by)) {
    split.by <- "tmp"
    df$tmp <- "tmp"
    if (!is.null(external.p.df)) {
      external.p.df$tmp <- "tmp"
    }
  }
  if (!is.null(splits)) {
    df <- df[df[, split.by] %in% splits, ]
  }
  res <- df %>% split(., f = .[, split.by]) %>%
    lapply(function(df) {
      utilsFanc::check.intersect(
        c(group.1, group.2), "groups",
        df[, group.by], paste0("the groups of split ", df[1, split.by]))
      groups <- list(g1 = group.1, g2 = group.2)
      ys <- lapply(groups, function(group) return(df[df[, group.by] == group, y]))
      xs <- lapply(groups, function(group) return(df[df[, group.by] == group, x]))
      res <- list()
      if (!is.null(external.p.df)) {
        p <- external.p.df[
          external.p.df[, split.by] == df[, split.by][1] &
            external.p.df[, "group.1"] == group.1 &
            external.p.df[, "group.2"] == group.2,
          "p"]
        if (length(p) != 1) {
          stop(paste0("no pvalue or more than 1 pvalue found in external.p.df for split: ",
                      df[, split.by][1]))
        }
        res$p <- p
      } else {
        t.res <- t.test(ys$g2, ys$g1)
        res$p <- t.res$p.value
      }
      x.means <- sapply(xs, mean)
      res$x.start <- min(x.means)
      res$x.end <- max(x.means)
      res$x.mid <- mean(c(res$x.start, res$x.end))
      res$y.loc <- max(unlist(ys))
      res[[split.by]] <- df[, split.by][1]
      res <- as.data.frame(res)
      return(res)
    }) %>% do.call(rbind, .)
  if (split.by == "tmp") {
    res$tmp <- NULL
  }
  if (!is.null(adjust)) {
    res$p <- res$p %>% p.adjust(method = adjust)
  }

  # now we calculate where the bar and the text should be displayed.
  if (is.null(ymax)) {
    ymax <- max(res$y.loc)
  }
  if (nudge.by.fraction) {
    pval.bar.y.nudge <- pval.bar.y.nudge * ymax
    pval.text.y.nudge <- pval.text.y.nudge * ymax
  }
  res$y.bar <- res$y.loc + pval.bar.y.nudge
  res$y.text <- res$y.loc + pval.text.y.nudge

  # format p values for presentation.
  res$p.sci <- res$p %>% sapply(function(x) {
    if (x < 0.0001) {
      x <- format(x, scientific = T)
    } else {
      x <- round(x, digits = 4)
    }
  })
  # pvalues lower than 0.0001 will be converted to scientific annotations

  # convert p values to stars:
  breaks <- c(1, 0.05, 0.01, 0.001, 0.0001, 0)
  star <- data.frame(level = 1:5, star = c("****", "***", "**", "*", "ns"))
  res$level <- cut(res$p, breaks = breaks, include.lowest = T, right = F) %>% as.integer()
  res <- dplyr::left_join(res, star, by = "level")
  return(res)
}
