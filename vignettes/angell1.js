// Dimension variables
let height = 500, width = 750,
  margin = {top: height * .02, right: width * .15, bottom: height * 0.15, left: width * .1},
  adjustedWidth = width - margin.left - margin.right,
  adjustedHeight = height - margin.top - margin.bottom,
  colors = ["#B80000", "#000000", "#000000"],
  pointSize = [7, 5], lineSize = [3, 1.5],
  labelFontFamily = "sans-serif", yLabel = "Effect of Region",
  labelFont = 12, axesFont = 12;

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

