##' Significance Plot with D3 (vertical)
##'
##' Makes an errorbar plot using the D3.js library with a hover-over
##' effect that highlights all cagtegories that are significantly differnet
##' than the one being hovered over.
##'
##' @aliases sigd3.default sigd3.mcmc.list sigd3.mcmc
##'
##' @param object A model that has a categorical predictor.  The function
##' uses both \code{ggpredict} from the \pkg{ggeffects} package and
##' \code{factorplot} from the \pkg{factorplot} package (with the
##' variable given by the \code{factor.var=term} argument), so the object
##' must be amenable to these manipulations. Alternatively, it could be
##' an mcmc object in which case all columns will be plotted.
##' @param term The term from the model to be plotted.
##' @param fname Name of the file to which the html code will be written.
##' @param height Height of the plot in pixels
##' @param width Width of the plot in pixels
##' @param return_iFrame Logical indicating whether an \code{iframe}
##' should be returned to hold the plot.  If \code{FALSE} only the html
##' file is produced and the function prpduces no other visible output.
##' @param ylab Label to put on y-axis.
##' @param level Confidence level at which to do the test (or in the mcmc case, credibility level for the credible intervals).
##' @param axfont Font size for axis tick mark labels (in px).
##' @param labfont Font size for y-axis label (in px).
##' @param labfam Font-family for y-axis label (must be one of "serif" or "sans-serif").
##' @param colors Vector of three colors the first for the selected items, the second for the un-selected items and the third for when no points are hovered over.
##' @param ptSize Vector of two point sizes - the first for selected observations and the second for unselected ones.
##' @param lineSize Vector of line widths - the first for selected observations and the second for unselected ones.
##' @param names An optional vector of names for the parameters.
##' @param order How should parameters be organized by size-ascending, size-descending or natural (i.e., the order in the data).
##'
##' @importFrom ggeffects ggpredict
##' @importFrom factorplot factorplot
##' @importFrom dplyr mutate rename select bind_cols arrange
##' @importFrom magrittr %>%
##' @importFrom jsonlite toJSON
##' @importFrom htmltools tags
##' @importFrom stats quantile
##' @importFrom utils combn
##' @importFrom rlang .data
##'
##' @return Either an html file or an iframe linked to the html file.
##' @export


sigd3 <-
  function(object,
           term,
           fname,
           height = 500,
           width = 625,
           return_iFrame = TRUE,
           level = .95,
           ylab = paste0("Effect of ", term),
           axfont = 12,
           labfont = 12,
           labfam = c("sans-serif", "serif"),
           colors = c("#B80000", "#000000", "#000000"),
           ptSize = c(7, 5),
           lineSize = c(3, 1.5),
           names = NULL,
           order = c("size-descending", "size-ascending", "natural")) {
    UseMethod("sigd3")
  }



##' @method sigd3 default
##' @export
sigd3.default <-
  function(object,
           term,
           fname,
           height = 500,
           width = 625,
           return_iFrame = TRUE,
           level = .95,
           ylab = paste0("Effect of ", term),
           axfont = 12,
           labfont = 12,
           labfam = c("sans-serif", "serif"),
           colors = c("#B80000", "#000000", "#000000"),
           ptSize = c(7, 5),
           lineSize = c(3, 1.5),
           names = NULL,
           order = c("size-descending", "size-ascending", "natural")) {

    labfam <- match.arg(labfam)
    order <- match.arg(order)
    g <- ggpredict(object, terms = term)
    g <- g %>% mutate(obs = 1:nrow(g)) %>%
      rename("y" = "predicted", "ylow" = "conf.low", "yup" = "conf.high") %>%
      select("x", "y", "ylow", "yup", "obs")

    if (!is.null(names)) {
      if (length(names) == nrow(g)) {
        g <- mutate(x = names)
      } else{
        stop(
          paste0("mismatch between vector lengths names: (", length(names),
                 ") and parameters: (", nrow(g), ").\n")
        )
      }
    }

    fp <- factorplot(object, factor.var = term, pval = 1 - level)
    mat <- matrix(0, nrow = nrow(fp$pval) + 1, ncol = ncol(fp$pval) + 1)
    mat[upper.tri(mat, diag = FALSE)] <-
      fp$pval[upper.tri(fp$pval, diag = TRUE)]
    mat <- t(mat)
    mat[upper.tri(mat, diag = FALSE)] <-
      fp$pval[upper.tri(fp$pval, diag = TRUE)]
    mat <- ifelse(mat < .05, 1, 0)
    diag(mat) <- 0

    mat <- as.data.frame(mat)
    names(mat) <- paste0("g", 1:ncol(mat))
    g <- g %>% bind_cols(mat)

    if (order == "size-ascending") {
      g <- g %>% arrange(.data$y)
    } else if (order == "size-descending") {
      g <- g %>% arrange(-.data$y)
    }

    h <- '<!doctype html>

<html lang="en">
<head>
  <meta charset="utf-8">

  <title>sigd3.default</title>
  <script src="https://d3js.org/d3-selection.v1.min.js"></script>
  <script src="https://d3js.org/d3.v4.js"></script>
  <script src="https://code.jquery.com/jquery-3.5.1.min.js"></script>

</head>
<body>

  <script>'


    b <- paste0('  </script>
  <script>
    let height = ',height,', width = ',width,',
      margin = {top: height * .02, right: width * .15, bottom: height * 0.15, left: width * .1},
      adjustedWidth = width - margin.left - margin.right,
      adjustedHeight = height - margin.top - margin.bottom,
      colors = ["',colors[1],'", "',colors[2],'", "',colors[3],'"],
      pointSize = [',ptSize[1],', ',ptSize[2],'], lineSize = [',lineSize[1],', ',lineSize[2],'],
      labelFontFamily = "',labfam[1],'", yLabel = "',ylab,'",
      labelFont = ',labfont,', axesFont = ',axfont,';

    // Append svg to the body
    let svg = d3.select("body")
      .append("svg")
      .attr("width", adjustedWidth + margin.left + margin.right)
      .attr("height", adjustedHeight + margin.top + margin.bottom)
      .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    // Various mapping functions
    const xMap = (d) => x(d.x);
    const yMap = (d) => y(d.y);
    const ylMap = (d) => y(d.ylow);
    const yuMap = (d) => y(d.yup);

    // Axes initialization
    let rgX = [0];
    let deltaX = adjustedWidth / (data.length - 1)
    for (let j = 1; j < data.length; j++) {
      rgX.push(j * deltaX);
    }

    const x = d3.scaleOrdinal()
      .domain(Array.from(data, el => el.x))
      .range(rgX);

    const y = d3.scaleLinear()
      .domain([d3.min(data, (d) => d.ylow), d3.max(data, (d) => d.yup)])
      .range([adjustedHeight, 0]);

    svg.append("g")
      .attr("transform", "translate(0," + adjustedHeight * 1.075 + ")")
      .call(d3.axisBottom(x));

    svg.append("g")
      .attr("transform", `translate(${-adjustedWidth * .035},0)`)
      .call(d3.axisLeft(y));

    d3.selectAll(".tick").style("font-size", `${axesFont}px`)

    // Mouseover highlight function
    const highlight = ((d) => {
      for (let i = 0; i < data.length; i++) {
        if (data[i][`g${d.obs}`] === 1) {
          d3.selectAll(`circle[cx="${xMap(data[i])}"]`)
            .transition()
            .duration(50)
            .style("fill", colors[0])
            .attr("r", pointSize[0])
          d3.select(`line[x1="${xMap(data[i])}"]`)
            .transition()
            .duration(50)
            .style("stroke", colors[0])
            .attr("stroke-width", lineSize[0])
        }
      }
    });

    // Mouseleave highlight function
    const doNotHighlight = function () {
      d3.selectAll(".dot")
        .transition()
        .duration(200)
        .style("fill", colors[2])
        .attr("r", pointSize[1])

      d3.selectAll(".line")
        .transition()
        .duration(200)
        .style("stroke", colors[2])
        .attr("stroke-width", lineSize[1])
    }

    // Vertical label
    svg.append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", 0 - margin.left)
      .attr("x", 0 - (adjustedHeight / 2))
      .attr("dy", "1em")
      .style("text-anchor", "middle")
      .style("font-size", `${labelFont}px`)
      .text(yLabel)
      .style("font-family", labelFontFamily);

    // Graph dot lines
    for (let m = 0; m < data.length; m++) {
      svg.append("line")
        .attr("class", "line")
        .attr("x1", xMap(data[m]))
        .attr("y1", ylMap(data[m]))
        .attr("x2", xMap(data[m]))
        .attr("y2", yuMap(data[m]))
        .attr("stroke", colors[2])
        .attr("stroke-width", lineSize[1])
    }

    // Graph points
    svg.selectAll(".dot")
      .data(data)
      .enter()
      .append("circle")
      .attr("class", "dot")
      .attr("r", pointSize[1])
      .attr("cx", xMap)
      .attr("cy", yMap)
      .on("mouseover", highlight)
      .on("mouseleave", doNotHighlight)

  </script>
</body>
</html>')

    cat(h, "\n", sep = "", file = fname, append = FALSE)
    cat("let data = ", toJSON(g), ";\n", sep = "", file = fname, append = TRUE)
    cat(b, sep = "", file = fname, append = TRUE)

    if (return_iFrame) {
      tags$iframe(src = fname, height = height * 1.05, width = width * 1.05, style = "border: none")
    }

    r2d3(data=g, options = list(axfont = 12, colors = c("#B80000", "#000000", "#000000"), ptSize = c(7,5), lineSize = c(3,1.5), labfont = 12, ylab = "Effect of Region", labfam = c("sans-serif", "serif")), script = "sigd3.js", height="500", width="750")
  }



##' @method sigd3 mcmc.list
##' @export
sigd3.mcmc.list <-
  function(object,
           term,
           fname,
           height = 500,
           width = 625,
           return_iFrame = TRUE,
           level = .95,
           ylab = paste0("Effect of ", term),
           axfont = 12,
           labfont = 12,
           labfam = c("sans-serif", "serif"),
           colors = c("#B80000", "#000000", "#000000"),
           ptSize = c(7, 5),
           lineSize = c(3, 1.5),
           names = NULL,
           order = c("size-descending", "size-ascending", "natural")) {
    object <- do.call(rbind, object)
    sigd3.mcmc(
      object,
      term = term,
      height = height,
      width = width,
      return_iFrame = return_iFrame,
      level = level,
      ylab = ylab,
      axfont = axfont,
      labfont = labfont,
      labfam = labfam,
      colors = colors,
      ptSize = ptSize,
      lineSize = lineSize,
      names = names,
      order = order
    )
  }




##' @method sigd3 mcmc
##' @export
sigd3.mcmc <-
  function(object,
           term,
           fname,
           height = 500,
           width = 625,
           return_iFrame = TRUE,
           level = .95,
           ylab = paste0("Effect of ", term),
           axfont = 12,
           labfont = 12,
           labfam = c("sans-serif", "serif"),
           colors = c("#B80000", "#000000", "#000000"),
           ptSize = c(7, 5),
           lineSize = c(3, 1.5),
           names = NULL,
           order = c("size-descending", "size-ascending", "natural")) {
    labfam <- match.arg(labfam)
    order <- match.arg(order)

    if (!is.null(names)) {
      if (length(names) == ncol(object)) {
        colnames(object) = names
      } else {
        stop(
          paste0(
            "mismatch between vector lengths names: )", length(names),
            ") and object: (", ncol(object), ").\n"
          )
        )
      }
    }

    g <- data.frame(
      x = colnames(object),
      y = colMeans(object),
      ylow = apply(object, 2, quantile, (1 - level) / 2),
      yup = apply(object, 2, quantile, 1 - (1 - level) / 2),
      obs = 1:ncol(object)
    )

    combs <- combn(nrow(g), 2)
    m1 <- object[, combs[1, ]]
    m2 <- object[, combs[2, ]]
    diffs <- m2 - m1
    g0 <- apply(diffs, 2, function(x)
      mean(x > 0))

    mat <- matrix(0, nrow = ncol(object), ncol = ncol(object))
    mat[cbind(combs[1, ], combs[2, ])] <- g0
    mat <- t(mat)
    mat[cbind(combs[1, ], combs[2, ])] <- g0
    mat <- ifelse(mat > level | mat < (1 - level), 1, 0)
    diag(mat) <- 0
    mat <- as.data.frame(mat)
    names(mat) <- paste0("g", 1:ncol(mat))
    g <- g %>% bind_cols(mat)

    if (order == "size-ascending") {
      g <- g %>% arrange(.data$y)
    } else  if (order == "size-descending") {
      g <- g %>% arrange(-.data$y)
    }

    h <- '<!doctype html>

<html lang="en">
<head>
  <meta charset="utf-8">

  <title>sigd3.default</title>
  <script src="https://d3js.org/d3-selection.v1.min.js"></script>
  <script src="https://d3js.org/d3.v4.js"></script>
  <script src="https://code.jquery.com/jquery-3.5.1.min.js"></script>

</head>
<body>

  <script>'

    b <- paste0('  </script>
  <script>
    let height = ',height,', width = ',width,',
      margin = {top: height * .02, right: width * .15, bottom: height * 0.15, left: width * .1},
      adjustedWidth = width - margin.left - margin.right,
      adjustedHeight = height - margin.top - margin.bottom,
      colors = ["',colors[1],'", "',colors[2],'", "',colors[3],'"],
      pointSize = [',ptSize[1],', ',ptSize[2],'], lineSize = [',lineSize[1],', ',lineSize[2],'],
      labelFontFamily = "',labfam[1],'", yLabel = "',ylab,'",
      labelFont = ',labfont,', axesFont = ',axfont,';

    // Append svg to the body
    let svg = d3.select("body")
      .append("svg")
      .attr("width", adjustedWidth + margin.left + margin.right)
      .attr("height", adjustedHeight + margin.top + margin.bottom)
      .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    // Various mapping functions
    const xMap = (d) => x(d.x);
    const yMap = (d) => y(d.y);
    const ylMap = (d) => y(d.ylow);
    const yuMap = (d) => y(d.yup);

    // Axes initialization
    let rgX = [0];
    let deltaX = adjustedWidth / (data.length - 1)
    for (let j = 1; j < data.length; j++) {
      rgX.push(j * deltaX);
    }

    const x = d3.scaleOrdinal()
      .domain(Array.from(data, el => el.x))
      .range(rgX);

    const y = d3.scaleLinear()
      .domain([d3.min(data, (d) => d.ylow), d3.max(data, (d) => d.yup)])
      .range([adjustedHeight, 0]);

    svg.append("g")
      .attr("transform", "translate(0," + adjustedHeight * 1.075 + ")")
      .call(d3.axisBottom(x));

    svg.append("g")
      .attr("transform", `translate(${-adjustedWidth * .035},0)`)
      .call(d3.axisLeft(y));

    d3.selectAll(".tick").style("font-size", `${axesFont}px`)

    // Mouseover highlight function
    const highlight = ((d) => {
      for (let i = 0; i < data.length; i++) {
        if (data[i][`g${d.obs}`] === 1) {
          d3.selectAll(`circle[cx="${xMap(data[i])}"]`)
            .transition()
            .duration(50)
            .style("fill", colors[0])
            .attr("r", pointSize[0])
          d3.select(`line[x1="${xMap(data[i])}"]`)
            .transition()
            .duration(50)
            .style("stroke", colors[0])
            .attr("stroke-width", lineSize[0])
        }
      }
    });

    // Mouseleave highlight function
    const doNotHighlight = function () {
      d3.selectAll(".dot")
        .transition()
        .duration(200)
        .style("fill", colors[2])
        .attr("r", pointSize[1])

      d3.selectAll(".line")
        .transition()
        .duration(200)
        .style("stroke", colors[2])
        .attr("stroke-width", lineSize[1])
    }

    // Vertical label
    svg.append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", 0 - margin.left)
      .attr("x", 0 - (adjustedHeight / 2))
      .attr("dy", "1em")
      .style("text-anchor", "middle")
      .style("font-size", `${labelFont}px`)
      .text(yLabel)
      .style("font-family", labelFontFamily);

    // Graph dot lines
    for (let m = 0; m < data.length; m++) {
      svg.append("line")
        .attr("class", "line")
        .attr("x1", xMap(data[m]))
        .attr("y1", ylMap(data[m]))
        .attr("x2", xMap(data[m]))
        .attr("y2", yuMap(data[m]))
        .attr("stroke", colors[2])
        .attr("stroke-width", lineSize[1])
    }

    // Graph points
    svg.selectAll(".dot")
      .data(data)
      .enter()
      .append("circle")
      .attr("class", "dot")
      .attr("r", pointSize[1])
      .attr("cx", xMap)
      .attr("cy", yMap)
      .on("mouseover", highlight)
      .on("mouseleave", doNotHighlight)

  </script>
</body>
</html>'
    )

    cat(h, "\n", sep = "", file = fname, append = FALSE)
    cat("var data = ", toJSON(g), ";\n", sep = "", file = fname, append = TRUE)
    cat(b, sep = "", file = fname, append = TRUE)

    if (return_iFrame) {
      tags$iframe(src = fname, height = height * 1.05, width = width * 1.05, style = "border: none")
    }

    r2d3(data=g, options = list(axfont = 12, colors = c("#B80000", "#000000", "#000000"), ptSize = c(7,5), lineSize = c(3,1.5), labfont = 12, ylab = "Effect of Region", labfam = c("sans-serif", "serif")), script = "sigd3.js", height="500", width="750")
  }

##' Significance Plot with D3 (horizontal)
##'
##' Makes an errorbar plot using the D3.js library with a hover-over
##' effect that highlights all cagtegories that are significantly differnet
##' than the one being hovered over. The orientation of this plot is
##' horizontal rather than vertical
##'
##' @aliases sigd3h.default sigd3h.mcmc.list sigd3h.mcmc
##'
##' @param object A model that has a categorical predictor.  The function
##' uses both \code{ggpredict} from the \pkg{ggeffects} package and
##' \code{factorplot} from the \pkg{factorplot} package (with the
##' variable given by the \code{factor.var=term} argument), so the object
##' must be amenable to these manipulations.  Alternatively, it could be
##' an mcmc object in which case all columns will be plotted.
##' @param term The term from the model to be plotted.
##' @param fname Name of the file to which the html code will be written.
##' @param height Height of the plot in pixels
##' @param width Width of the plot in pixels
##' @param return_iFrame Logical indicating whether an \code{iframe}
##' should be returned to hold the plot.  If \code{FALSE} only the html
##' file is produced and the function prpduces no other visible output.
##' @param ylab Label to put on y-axis.
##' @param level Confidence level at which to do the test (or in the mcmc case, credibility level for the credible intervals).
##' @param axfont Font size for axis tick mark labels (in px).
##' @param labfont Font size for y-axis label (in px).
##' @param lmexpand Factor by which the left-margin should be expanded to accommodate tick mark labels.
##' @param labfam Font-family for y-axis label (must be one of "serif" or "sans-serif").
##' @param colors Vector of three colors the first for the selected items, the second for the un-selected items and the third for when no points are hovered over.
##' @param ptSize Vector of two point sizes - the first for selected observations and the second for unselected ones.
##' @param lineSize Vector of line widths - the first for selected observations and the second for unselected ones.
##' @param names An optional vector of names for the parameters.
##' @param order How should parameters be organized by size-ascending, size-descending or natural (i.e., the order in the data).

##'
##' @importFrom ggeffects ggpredict
##' @importFrom factorplot factorplot
##' @importFrom dplyr mutate rename select bind_cols
##' @importFrom magrittr %>%
##' @importFrom jsonlite toJSON
##' @importFrom htmltools tags
##' @importFrom stats quantile
##' @importFrom utils combn
##'
##' @return Either an html file or an iframe linked to the html file.
##' @export
sigd3h <-
  function(object,
           term,
           fname,
           height = 500,
           width = 625,
           return_iFrame = TRUE,
           level = .95,
           ylab = paste0("Effect of ", term),
           axfont = 12,
           labfont = 12,
           lmexpand = .15,
           labfam = c("sans-serif", "serif"),
           colors = c("#B80000", "#000000", "#000000"),
           ptSize = c(7, 5),
           lineSize = c(3, 1.5),
           names = NULL,
           order = c("size-descending", "size-ascending", "natural")) {
    UseMethod("sigd3h")
  }

##' @method sigd3h default
##' @export
sigd3h.default <-
  function(object,
           term,
           fname,
           height = 500,
           width = 625,
           return_iFrame = TRUE,
           level = .95,
           ylab = paste0("Effect of ", term),
           axfont = 12,
           labfont = 12,
           lmexpand = .15,
           labfam = c("sans-serif", "serif"),
           colors = c("#B80000", "#000000", "#000000"),
           ptSize = c(7, 5),
           lineSize = c(3, 1.5),
           names = NULL,
           order = c("size-descending", "size-ascending", "natural")) {
    labfam = match.arg(labfam)
    order <- match.arg(order)
    g <- ggpredict(object, terms = term)
    g <- g %>% mutate(obs = 1:nrow(g)) %>%
      rename(
        "y" = "x",
        "x" = "predicted",
        "xlow" = "conf.low",
        "xup" = "conf.high"
      ) %>%
      select("x", "y", "xlow", "xup", "obs")
    if (!is.null(names)) {
      if (length(names) == nrow(g)) {
        g <- mutate(y = names)
      } else{
        stop(
          paste0(
            "names vector (length ",
            length(names),
            " has to be the same as the number of parameters (length, ",
            nrow(g),
            ").\n"
          )
        )
      }
    }
    fp <- factorplot(object, factor.var = term, pval = 1 - level)
    mat <- matrix(0, nrow = nrow(fp$pval) + 1, ncol = ncol(fp$pval) + 1)
    mat[upper.tri(mat, diag = FALSE)] <-
      fp$pval[upper.tri(fp$pval, diag = TRUE)]
    mat <- t(mat)
    mat[upper.tri(mat, diag = FALSE)] <-
      fp$pval[upper.tri(fp$pval, diag = TRUE)]
    mat <- ifelse(mat < .05, 1, 0)
    diag(mat) <- 0

    mat <- as.data.frame(mat)
    names(mat) <- paste0("g", 1:ncol(mat))
    g <- g %>% bind_cols(mat)
    if (order == "size-ascending") {
      g <- g %>% arrange(.data$x)
    }
    if (order == "size-descending") {
      g <- g %>% arrange(-.data$x)
    }

    h <- '
    <meta charset="utf-8">
    <head>
    </head>
    <body>
    <script src="https://d3js.org/d3-selection.v1.min.js"></script>
    <script src="https://d3js.org/d3.v4.js"></script>
    <script src="https://code.jquery.com/jquery-3.5.1.min.js"></script>

    <script>'

    b <- paste0(
      '
  </script>

  <script>
    var  h=',
      height,
      ';
    var w =',
      width,
      ';
    var margin = {top: h*.02, left: w*',
      lmexpand,
      ', bottom: h*',
      (0.1 * (axfont / 8)),
      ', right: w*.05},
      width = w - margin.left - margin.right,
      height = h - margin.top - margin.bottom;

    var rgY = [0];
    var deltaY = height/(data.length-1);
    var j;
    for(j =1; j<data.length; j++){
      rgY.push(j*deltaY);
    }

// append the svg object to the body of the page
var svg = d3.select("body")
  .append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform",
        "translate(" + margin.left + "," + margin.top + ")");


  var xValue = function(d){return d.x};
  var yValue = function(d){return d.y};
  var xlValue = function(d){return d.xlow};
  var xuValue = function(d){return d.xup};


  var yDom =[];
  data.forEach(d => yDom.push(d.y));

  var y = d3.scaleOrdinal()
  .domain(yDom)
  .range(rgY);

  var x = d3.scaleLinear()
  .domain([d3.min(data, xlValue), d3.max(data, xuValue)])
  .range([0, width]);


  var xMap = function(d){return x(xValue(d))}
  var xlMap = function(d){return x(xlValue(d))}
  var xuMap = function(d){return x(xuValue(d))}
  var yMap = function(d){return y(yValue(d))}


var highlight = function(d){
  var sel = d.obs;
  var gvar = `g${sel}`;
  var i;
  for(i = 0; i < data.length; i++){
    if(data[i][gvar] === 1){
      document.querySelector(`circle[cy="${yMap(data[i])}"]`).classList.add("dot-selected");
      document.querySelector(`circle[cy="${yMap(data[i])}"]`).classList.remove("dot-unselected");
      document.querySelector(`line[y1="${yMap(data[i])}"]`).classList.add("line-selected");
      document.querySelector(`line[y1="${yMap(data[i])}"]`).classList.remove("line-unselected");
    }else{
      document.querySelector(`circle[cy="${yMap(data[i])}"]`).classList.remove("dot-selected");
      document.querySelector(`circle[cy="${yMap(data[i])}"]`).classList.add("dot-unselected");
      document.querySelector(`line[y1="${yMap(data[i])}"]`).classList.remove("line-selected");
      document.querySelector(`line[y1="${yMap(data[i])}"]`).classList.add("line-unselected");
    }
  }

  d3.selectAll(".dot-selected")
  .transition()
  .duration(50)
  .style("fill", "',
      colors[1],
      '")
  .attr("r",',
      ptSize[1],
      ')

  d3.selectAll(".line-selected")
  .transition()
  .duration(50)
  .style("stroke", "',
      colors[1],
      '")
  .attr("stroke-width",',
      lineSize[1],
      ')

  d3.selectAll(".dot-unselected")
  .transition()
  .duration(50)
  .style("fill", "',
      colors[2],
      '")
  .attr("r",',
      ptSize[2],
      ')

  d3.selectAll(".line-unselected")
  .transition()
  .duration(50)
  .style("stroke", "',
      colors[2],
      '")
  .attr("stroke-width",',
      lineSize[2],
      ')
}




// Highlight the specie that is hovered
var doNotHighlight = function(){
  d3.selectAll(".dot")
  .transition()
  .duration(200)
  .style("fill", "', colors[3], '")
  .attr("r",', ptSize[2], ')

  d3.selectAll(".line")
  .transition()
  .duration(200)
  .style("stroke", "', colors[3], '")
  .attr("stroke-width",', lineSize[2], ')

}

svg.append("g")
.attr("transform", "translate(0," + height*1.025 + ")")
.call(d3.axisBottom(x).tickSizeOuter(0));

svg
.append("g")
.attr("transform", `translate(${-width*.035},0)`)
.call(d3.axisLeft(y));

svg.append("text")
.attr("y", 0 - margin.bottom)
.attr("y2",0 - width/2)
.attr("dy", "1em")
.style("text-anchor", "middle")
.style("font-size","', labfont, 'px")
.text("', ylab, '")
.style("font-family", "', labfam, '");

var m;
for(m=0; m<data.length; m++){
  svg
  .append("line")
  .attr("class", "line")
  .attr("x1", xlMap(data[m]))
  .attr("y1", yMap(data[m]))
  .attr("x2", xuMap(data[m]))
  .attr("y2", yMap(data[m]))
  .attr("stroke", "', colors[3], '")
  .attr("stroke-width",', lineSize[2], ')
}

svg
.selectAll(".dot")
.data(data)
.enter()
.append("circle")
.attr("class", "dot")
.attr("r",', ptSize[2], ')
.attr("cx", xMap)
.attr("cy", yMap)
.on("mouseover", highlight)
.on("mouseleave", doNotHighlight )

d3.selectAll(".tick").style("font-size", "', axfont, 'px")

</script>
</body>
')

    cat(h,
        "\n",
        sep  =  "",
        file  =  fname,
        append  =  FALSE)

    cat(
      "var data = ",
      toJSON(g),
      ";\n",
      sep  =  "",
      file  =  fname,
      append  =  TRUE
    )

    cat(b,
        sep  =  "",
        file  =  fname,
        append  =  TRUE)

    if (return_iFrame) {
      tags$iframe(
        src  =  fname,
        height  =  height  *  1.05,
        width  =  width  *  1.05,
        style  =  "border: none"
      )
    }
  }



##' @method sigd3h mcmc.list
##' @export
sigd3h.mcmc.list <-
  function(object,
           term,
           fname,
           height = 500,
           width = 625,
           return_iFrame = TRUE,
           level = .95,
           ylab = paste0("Effect of ", term),
           axfont = 12,
           labfont = 12,
           lmexpand = .15,
           labfam = c("sans-serif", "serif"),
           colors = c("#B80000", "#000000", "#000000"),
           ptSize = c(7, 5),
           lineSize = c(3, 1.5),
           names = NULL,
           order = c("size-descending", "size-ascending", "natural")) {
    object <- do.call(rbind, object)

    sigd3h.mcmc(
      object = object,
      term = term,
      fname = fname,
      height = height,
      width = width,
      return_iFrame = return_iFrame,
      level = level,
      ylab = ylab,
      axfont = axfont,
      labfont = labfont,
      lmexpand = lmexpand,
      labfam = labfam,
      colors = colors,
      ptSize = ptSize,
      lineSize = lineSize,
      names = names,
      order = order
    )
  }




##' @method sigd3h mcmc
##' @export
sigd3h.mcmc <-
  function(object,
           term,
           fname,
           height = 500,
           width = 625,
           return_iFrame = TRUE,
           level = .95,
           ylab = paste0("Effect of ", term),
           axfont = 12,
           labfont = 12,
           lmexpand = .15,
           labfam = c("sans-serif", "serif"),
           colors = c("#B80000", "#000000", "#000000"),
           ptSize = c(7, 5),
           lineSize = c(3, 1.5),
           names = NULL,
           order = c("size-descending", "size-ascending", "natural")) {
    labfam = match.arg(labfam)
    order <- match.arg(order)

    if (!is.null(names)) {
      if (length(names) == ncol(object)) {
        colnames(object) = names
      } else{
        stop(
          paste0("mismatch between vector length names: (", length(names), ") and object: (", ncol(object), ").\n")
        )
      }

    }
    g <- data.frame(
      y = colnames(object),
      x = colMeans(object),
      xlow = apply(object, 2, quantile, (1 - level) / 2),
      xup = apply(object, 2, quantile, 1 - (1 - level) / 2),
      obs = 1:ncol(object)
    )

    combs <- combn(ncol(object), 2)
    m1 <- object[, combs[1, ]]
    m2 <- object[, combs[2, ]]
    diffs <- m2 - m1
    g0 <- apply(diffs, 2, function(x)
      mean(x > 0))

    mat <- matrix(0, nrow = ncol(object), ncol = ncol(object))
    mat[cbind(combs[1, ], combs[2, ])] <- g0
    mat <- t(mat)
    mat[cbind(combs[1, ], combs[2, ])] <- g0
    mat <- ifelse(mat > level | mat < (1 - level), 1, 0)
    diag(mat) <- 0
    mat <- as.data.frame(mat)
    names(mat) <- paste0("g", 1:ncol(mat))
    g <- g %>% bind_cols(mat)
    if (order == "size-ascending") {
      g <- g %>% arrange(.data$x)
    } else if (order == "size-descending") {
      g <- g %>% arrange(-.data$x)
    }

    h <- '<!doctype html>

<html lang="en">
<head>
  <meta charset="utf-8">

  <title>sigd3h.default plot</title>
  <script src="https://d3js.org/d3-selection.v1.min.js"></script>
  <script src="https://d3js.org/d3.v4.js"></script>
  <script src="https://code.jquery.com/jquery-3.5.1.min.js"></script>

</head>
<body>

  <script>'

    b <- paste0('  </script>
  <script>
    // Variables
    let height = ',height,', width = ',width,',
      margin = {top: height * 0.02, right: width * 0.15, bottom: height * 0.2, left: width * 0.15},
      adjustedWidth = width - margin.left - margin.right,
      adjustedHeight = height - margin.top - margin.bottom,
      colors = ["',colors[1],'", "',colors[2],'", "',colors[3],'"],
      pointSize = [',ptSize[1],', ',ptSize[2],'], lineSize = [',lineSize[1],', ',lineSize[2],'],
      labelFontFamily = "',labfam[1],'", xLabel = "',ylab,'",
      labelFont = ',labfont,', axesFont = ',axfont,';

    // Append svg to the body
    let svg = d3.select("body")
      .append("svg")
        .attr("width", adjustedWidth + margin.left + margin.right)
        .attr("height", adjustedHeight + margin.top + margin.bottom)
      .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    // Mapping functions
    const xMap = (el) => x(el.x);
    const xlMap = (el) => x(el.xlow);
    const xuMap = (el) => x(el.xup);
    const yMap = (el) => y(el.y);

    // Axes initialization
    let rgY = [0];
    const deltaY = adjustedHeight/(data.length-1);
    for(let i = 1; i < data.length; i++) {
      rgY.push(i * deltaY);
    }

    let y = d3.scaleOrdinal()
      .domain(Array.from(data, el => el.y))
      .range(rgY);

    let x = d3.scaleLinear()
      .domain([d3.min(data, (el) => el.xlow), d3.max(data, (el) => el.xup)])
      .range([0, adjustedWidth]);

    svg.append("g")
      .attr("transform", "translate(0," + adjustedHeight * 1.025 + ")")
      .call(d3.axisBottom(x).tickSizeOuter(0));

    svg.append("g")
      .attr("transform", `translate(${-adjustedWidth * 0.035},0)`)
      .call(d3.axisLeft(y));

    d3.selectAll(".tick").style("font-size", `${axesFont}px`)

    // Mouseover highlight function
    const highlight = (d) => {
      for (let i = 0; i < data.length; i++){
        if (data[i][`g${d.obs}`] === 1){
          d3.selectAll(`circle[cy="${yMap(data[i])}"]`)
           .transition()
           .duration(50)
           .style("fill", colors[0])
           .attr("r", pointSize[0])

          d3.selectAll(`line[y1="${yMap(data[i])}"]`)
           .transition()
           .duration(50)
           .style("stroke", colors[0])
           .attr("stroke-width", lineSize[0])
        } else {
          d3.selectAll(`circle[cy="${yMap(data[i])}"]`)
           .transition()
           .duration(50)
           .style("fill", colors[1])
           .attr("r", pointSize[1])

          d3.selectAll(`line[y1="${yMap(data[i])}"]`)
           .transition()
           .duration(50)
           .style("stroke", colors[1])
           .attr("stroke-width", lineSize[1])
        }
      }
    }

    // Mouseleave highlight function
    const doNotHighlight = () => {
      d3.selectAll(".dot")
       .transition()
       .duration(200)
       .style("fill", colors[2])
       .attr("r", pointSize[1])

      d3.selectAll(".line")
       .transition()
       .duration(200)
       .style("stroke", colors[2])
       .attr("stroke-width", lineSize[1])
    }

    // Horizontal label
    svg.append("text")
      .attr("y", adjustedHeight + margin.bottom*0.4)
      .attr("x", adjustedWidth / 2)
      .attr("dy", "1em")
      .style("text-anchor", "middle")
      .style("font-size", `${labelFont}px`)
      .text(xLabel)
      .style("font-family", labelFontFamily);

    // Graph dot lines
    for (let i = 0; i < data.length; i++) {
      svg.append("line")
        .attr("class", "line")
        .attr("x1", xlMap(data[i]))
        .attr("y1", yMap(data[i]))
        .attr("x2", xuMap(data[i]))
        .attr("y2", yMap(data[i]))
        .attr("stroke", colors[2])
        .attr("stroke-width", lineSize[1])
    }

    // Graph points
    svg.selectAll(".dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("class", "dot")
        .attr("r", pointSize[1])
        .attr("cx", xMap)
        .attr("cy", yMap)
      .on("mouseover", highlight)
      .on("mouseleave", doNotHighlight )

  </script>
</body>
</html>'
)

    cat(h, "\n", sep  =  "", file  =  fname, append  =  FALSE)
    cat("var data = ", toJSON(g), ";\n", sep  =  "", file  =  fname, append  =  TRUE)
    cat(b, sep  =  "", file  =  fname, append  =  TRUE)

    if (return_iFrame) {
      tags$iframe(src  =  fname, height  =  height  *  1.05, width  =  width  *  1.05, style  =  "border: none")
    }

    r2d3(data=g, options = list(axfont = axfont, colors = colors, labfont = labfont, xlab = ylab, labfam = labfam, script = "sigd3h.js", height = height, width = width)
  }

#' Corporatism
#'
#' A dataset from Alvarez, Garrett and Lange's 1991 APSR article.
#'
#' @format A list with the following variables for 16 countries over 14 years:
#' \describe{
#'   \item{y}{Economic growth}
#'   \item{country}{Country indicator variable}
#'   \item{imports}{Imports price movement}
#'   \item{exports}{Export price movement}
#'   \item{left}{Leftist government}
#'   \item{demand}{Demand}
#'   \item{growth.lag}{Lag of economic growth}
#'   \item{labor.org}{Labor organizations prevalence, only one observation per country}
#' }
#' @source \url{https://spia.uga.edu/faculty_pages/rbakker/bayes/POLS%20Bayes.htm}
"corp"
