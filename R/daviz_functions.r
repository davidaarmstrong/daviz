##' Significance Plot with D3
##'
##' Makes an errorbar plot using the D3.js library with a hover-over
##' effect that highlights all cagtegories that are significantly differnet
##' than the one being hovered over.
##'
##' @param object A model that has a categorical predictor.  The function
##' uses both \code{ggpredict} from the \pkg{ggeffects} package and
##' \code{factorplot} from the \pkg{factorplot} package (with the
##' variable given by the \code{factor.var=term} argument, so the object
##' must be amenable to these manipulations. )
##' @param term The term from the model to be plotted.
##' @param fname Name of the file to which the html code will be written.
##' @param height Height of the plot in pixels
##' @param width Width of the plot in pixels
##' @param return_iFrame Logical indicating whether an \code{iframe}
##' should be returned to hold the plot.  If \code{FALSE} only the html
##' file is produced and the function prpduces no other visible output.
##' @param ylab Label to put on y-axis.
##' @param axfont Font size for axis tick mark labels (in px).
##' @param labfont Font size for y-axis label (in px).
##' @param labfam Font-family for y-axis label (must be one of "serif" or "sans-serif").
##' @param colors Vector of three colors the first for the selected items, the second for the un-selected items and the third for when no points are hovered over.
##'
##' @importFrom ggeffects ggpredict
##' @importFrom factorplot factorplot
##' @importFrom dplyr mutate rename select bind_cols
##' @importFrom magrittr %>%
##' @importFrom jsonlite toJSON
##' @importFrom htmltools tags
##'
##' @return Either an html file or an iframe linked to the html file.
##' @export

sigd3 <- function(object, term, fname, height=500, width=625, return_iFrame = TRUE,
                  ylab = paste0("Effect of ", term), axfont = 12, labfont=12,
                  labfam = c("sans-serif", "serif"), colors=c("red", "black", "black")){
labfam = match.arg(labfam)
g <- ggpredict(object, terms=term)
g <- g %>% mutate(obs=1:nrow(g)) %>%
  rename("y"="predicted",
         "ylow" = "conf.low",
         "yup" = "conf.high") %>%
  select("x","y","ylow","yup","obs")
fp <- factorplot(object, factor.var=term)
mat <- matrix(0, nrow=nrow(fp$pval)+1, ncol=ncol(fp$pval)+1)
mat[upper.tri(mat, diag=FALSE)] <- fp$pval[upper.tri(fp$pval, diag=TRUE)]
mat <- t(mat)
mat[upper.tri(mat, diag=FALSE)] <- fp$pval[upper.tri(fp$pval, diag=TRUE)]
mat <- ifelse(mat < .05, 1, 0)
diag(mat) <- 0

mat <- as.data.frame(mat)
names(mat) <- paste0("g", 1:ncol(mat))
g <- g %>% bind_cols(mat)

  h <- '
    <meta charset="utf-8">
    <head>
    </head>
    <body>
    <script src="https://d3js.org/d3-selection.v1.min.js"></script>
    <script src="https://d3js.org/d3.v4.js"></script>
    <script src="https://code.jquery.com/jquery-3.5.1.min.js"></script>

    <script>'

  b <- paste0('
  </script>

  <script>
    var h=', height, ';
    var w =', width, ';
    var margin = {top: h*.02, right: w*.15, bottom: h*', round(.1*(axfont/8), 2), ', left: w*.1},
        width = w - margin.left - margin.right,
        height = h - margin.top - margin.bottom;

    var rgX = [0];
    var deltaX = width/(data.length-1)
    var j;
    for(j =1; j<data.length; j++){
        rgX.push(j*deltaX);
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
    var ylValue = function(d){return d.ylow};
    var yuValue = function(d){return d.yup};


    var xDom =[];
    data.forEach(d => xDom.push(d.x));

    var rgX = [0];
    var deltaX = width/(data.length-1)
    var j;
    for(j =1; j<data.length; j++){
        rgX.push(j*deltaX);
    }

    var x = d3.scaleOrdinal()
        .domain(xDom)
        .range(rgX);

      // Add Y axis
    var y = d3.scaleLinear()
        .domain([d3.min(data, ylValue), d3.max(data, yuValue)])
        .range([ height, 0]);


    var xMap = function(d){return x(xValue(d))}
    var yMap = function(d){return y(yValue(d))}
    var ylMap = function(d){return y(ylValue(d))}
    var yuMap = function(d){return y(yuValue(d))}


     var highlight = function(d){

        var sel = d.obs;
        var gvar = `g${sel}`;
        var i;
        for(i = 0; i < data.length; i++){
            if(data[i][gvar] === 1){
               document.querySelector(`circle[cx="${xMap(data[i])}"]`).classList.add("dot-selected");
               document.querySelector(`circle[cx="${xMap(data[i])}"]`).classList.remove("dot-unselected");
               document.querySelector(`line[x1="${xMap(data[i])}"]`).classList.add("line-selected");
               document.querySelector(`line[x1="${xMap(data[i])}"]`).classList.remove("line-unselected");
            }else{
               document.querySelector(`circle[cx="${xMap(data[i])}"]`).classList.remove("dot-selected");
               document.querySelector(`circle[cx="${xMap(data[i])}"]`).classList.add("dot-unselected");
               document.querySelector(`line[x1="${xMap(data[i])}"]`).classList.remove("line-selected");
               document.querySelector(`line[x1="${xMap(data[i])}"]`).classList.add("line-unselected");
            }
        }

        // d3.selectAll(".dot-unselected")
        //   .transition()
        //   .duration(50)
        //   //.style("fill", "lightgrey")
        //   .attr("r", 3)

        d3.selectAll(".dot-selected")
          .transition()
          .duration(50)
          .style("fill", "', colors[1], '")
          .attr("r", 7)

        d3.selectAll(".line-selected")
          .transition()
          .duration(50)
          .style("stroke", "', colors[1], '")
          .attr("stroke-width", 3)

        d3.selectAll(".dot-unselected")
          .transition()
          .duration(50)
          .style("fill", "', colors[2], '")
          .attr("r", 5)

        d3.selectAll(".line-unselected")
          .transition()
          .duration(50)
          .style("stroke", "', colors[2], '")
          .attr("stroke-width", 2)
        }




      // Highlight the specie that is hovered
      var doNotHighlight = function(){
        d3.selectAll(".dot")
          .transition()
          .duration(200)
          .style("fill", "', colors[3], '")
          .attr("r", 5 )

        d3.selectAll(".line")
          .transition()
          .duration(200)
          .style("stroke", "', colors[3], '")
          .attr("stroke-width", 2)

      }

      svg.append("g")
        .attr("transform", "translate(0," + height*1.075 + ")")
        .call(d3.axisBottom(x));

    svg
        .append("g")
        .attr("transform", `translate(${-width*.035},0)`)
        .call(d3.axisLeft(y));

    svg.append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", 0 - margin.left)
      .attr("x",0 - (height / 2))
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
                .attr("x1", xMap(data[m]))
                .attr("y1", ylMap(data[m]))
                .attr("x2", xMap(data[m]))
                .attr("y2", yuMap(data[m]))
                .attr("stroke", "black")
                .attr("stroke-width", 1.5)
    }

    svg
        .selectAll(".dot")
        .data(data)
        .enter()
        .append("circle")
          .attr("class", "dot")
          .attr("r", 3.5)
          .attr("cx", xMap)
          .attr("cy", yMap)
        .on("mouseover", highlight)
        .on("mouseleave", doNotHighlight )

    d3.selectAll(".tick").style("font-size", "', axfont, 'px")

    </script>
  </body>
')
  cat(h, "\n", sep="", file=fname, append=FALSE)

  cat("var data = ", toJSON(g), ";\n", sep="",
      file=fname, append=TRUE)

  cat(b, sep="", file=fname, append=TRUE)

  tags$iframe(src=fname, height=height*1.05, width=width*1.05, style="border: none")
}
