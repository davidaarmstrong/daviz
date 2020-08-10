// preview r2d3 data=[{"x":"E","y":15.3667,"ylow":13.9153,"yup":16.818,"obs":1,"g1":0,"g2":1,"g3":1,"g4":1},{"x":"MW","y":12.2786,"ylow":11.1149,"yup":13.4422,"obs":2,"g1":1,"g2":0,"g3":1,"g4":0},{"x":"W","y":10.5167,"ylow":8.7391,"yup":12.2942,"obs":4,"g1":1,"g2":0,"g3":1,"g4":0},{"x":"S","y":7.7357,"ylow":6.5721,"yup":8.8994,"obs":3,"g1":1,"g2":1,"g3":0,"g4":1}], d3_version = 4, options = list(axfont = 12, colors = c("#B80000", "#000000", "#000000"), ptSize = c(7,5), lineSize = c(3,1.5), labfont=12, ylab="Effect of Region", labfam=c("sans-serif","serif")), dependencies = list("d3-selection.v1.min.js", "jquery-3.5.1.min.js"), viewer="browser"

let
  margin = {top: height * 0.02, right: width * 0.15, bottom: height * 0.15, left: width * 0.1},
  adjustedWidth = width - margin.left - margin.right,
  adjustedHeight = height - margin.top - margin.bottom,
  colors = options.colors,
  pointSize = options.ptSize, lineSize = options.lineSize,
  labelFontFamily = options.labfam, yLabel = options.ylab,
  labelFont = options.labfont, axesFont = options.axfont;

// Append svg to the body
svg.attr("width", adjustedWidth + margin.left + margin.right + 200)
  .attr("height", adjustedHeight + margin.top + margin.bottom)
  .append("g")
  .attr("transform", "translate(" + 100 + "," + 100 + ")");

// Various mappping functions
const xMap = (d) => x(d.x);
const yMap = (d) => y(d.y);
const ylMap = (d) => y(d.ylow);
const yuMap = (d) => y(d.yup);

// Axes initialization
let rgX = [0];
let deltaX = width / (data.length - 1)
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
  .attr("transform", "translate(100," + adjustedHeight * 1.075 + ")")
  .call(d3.axisBottom(x));

svg.append("g")
  .attr("transform", "translate(75,0)")
  .call(d3.axisLeft(y));

// onRender Callback executes whenever data changes
r2d3.onRender((data, svg, width, height, options) => {

  // Mouseover highlight function
  const highlight = ((d) => {
    for (let i = 0; i < data.length; i++) {
      if (data[i][`g${d.obs}`] == 1) {
        svg.selectAll(`circle[cx="${xMap(data[i])}"]`)
          .transition()
          .duration(50)
          .style("fill", colors[0])
          .attr("r", pointSize[0])
        svg.select(`line[x1="${xMap(data[i])}"]`)
          .transition()
          .duration(50)
          .style("stroke", colors[0])
          .attr("stroke-width", lineSize[0])
      }
    }
  });

  // Mouseleave highlight function
  const doNotHighlight = (() => {
    svg.selectAll(".dot")
      .transition()
      .duration(200)
      .style("fill", colors[2])
      .attr("r", pointSize[1])

    svg.selectAll(".line")
      .transition()
      .duration(200)
      .style("stroke", colors[2])
      .attr("stroke-width", lineSize[1])
  })

  // Vertical label
  svg.append("text")
    .attr("transform", "rotate(-90)")
    .attr("y", 15)
    .attr("x", 0 - (adjustedHeight / 2))
    .attr("dy", "1em")
    .style("text-anchor", "middle")
    .style("font-size", `${labelFont}px`)
    .text(yLabel)
    .style("font-family", labelFontFamily[0]);

  // Graph dot lines
  for (let m = 0; m < data.length; m++) {
    svg.append("line")
      .attr("class", "line")
      .attr("transform", "translate(100,0)")
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
    .attr("transform", "translate(100,0)")
    .attr("r", pointSize[1])
    .attr("cx", xMap)
    .attr("cy", yMap)
    .on("mouseover", highlight)
    .on("mouseleave", doNotHighlight)
});
