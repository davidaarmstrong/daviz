let
  margin = {top: height * 0.02, right: width * 0.15, bottom: height * 0.2, left: width * 0.15},
  adjustedWidth = width - margin.left - margin.right, adjustedHeight = height - margin.top - margin.bottom,
  offsetX = 50, offsetY = 10, colors = options.colors,
  pointSize = options.ptSize, lineSize = options.lineSize,
  labelFontFamily = options.labfam, xLabel = options.xlab,
  labelFont = options.labfont, axesFont = options.axfont;

// Append svg to the body
svg.attr("width", adjustedWidth + margin.left + margin.right + 200)
  .attr("height", adjustedHeight + margin.top + margin.bottom)
  .append("g")
  .attr("transform", "translate(" + 100 + "," + 100 + ")");

// Various mappping functions
const xMap = (el) => x(el.x);
const xlMap = (el) => x(el.xlow);
const xuMap = (el) => x(el.xup);
const yMap = (el) => y(el.y);

// Axes initialization
let rgY = [0];
const deltaY = adjustedHeight / (data.length - 1)
for (let i = 1; i < data.length; i++) {
  rgY.push(i * deltaY);
}

const y = d3.scaleOrdinal()
           .domain(Array.from(data, el => el.y))
           .range(rgY);

const x = d3.scaleLinear()
           .domain([d3.min(data, (d) => d.xlow), d3.max(data, (d) => d.xup)])
           .range([0, adjustedWidth]);

svg.append("g")
  .attr("transform", `translate(${offsetX}, ${adjustedHeight * 1.025 + offsetY})`)
  .call(d3.axisBottom(x).tickSizeOuter(0));

svg.append("g")
  .attr("transform", `translate(${offsetX * 0.8}, ${offsetY})`)
  .call(d3.axisLeft(y));

svg.selectAll(".tick").style("font-size", `${axesFont}px`)

// onRender Callback executes whenever data changes
r2d3.onRender((data, svg, width, height, options) => {

  // Mouseover highlight function
  const highlight = (d) => {
    for (let i = 0; i < data.length; i++) {
      if (data[i][`g${d.obs}`] == 1) {
        svg.selectAll(`circle[cy="${yMap(data[i])}"]`)
          .transition()
          .duration(50)
          .style("fill", colors[0])
          .attr("r", pointSize[0])

        svg.select(`line[y1="${yMap(data[i])}"]`)
          .transition()
          .duration(50)
          .style("stroke", colors[0])
          .attr("stroke-width", lineSize[0])
      } else {
        svg.selectAll(`circle[cy="${yMap(data[i])}"]`)
          .transition()
          .duration(50)
          .style("fill", colors[1])
          .attr("r", pointSize[1])

        svg.selectAll(`line[y1="${yMap(data[i])}"]`)
          .transition()
          .duration(50)
          .style("stroke", colors[1])
          .attr("stroke-width", lineSize[1])
      }
    }
  }

  // Mouseleave highlight function
  const doNotHighlight = () => {
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
  }

  // Horizontal label
  svg.append("text")
    .attr("y", adjustedHeight + margin.bottom * 0.4)
    .attr("x", adjustedWidth / 2 + offsetX)
    .attr("dy", "1em")
    .style("text-anchor", "middle")
    .style("font-size", `${labelFont}px`)
    .text(xLabel)
    .style("font-family", labelFontFamily);

  // Graph dot lines
  for (let i = 0; i < data.length; i++) {
    svg.append("line")
      .attr("class", "line")
      .attr("transform", `translate(${offsetX},${offsetY})`)
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
      .attr("transform", `translate(${offsetX},${offsetY})`)
      .attr("r", pointSize[1])
      .attr("cx", xMap)
      .attr("cy", yMap)
    .on("mouseover", highlight)
    .on("mouseleave", doNotHighlight)
});
