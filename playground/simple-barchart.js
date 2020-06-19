// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)

svg.selectAll('rect')
  .data(data)
  .enter().append('rect')
  .attr('width', function (d) {return d * 400;})
  .attr('height', '58px')
  .attr('y', function (d, i) {return i * 60;})
  .attr('fill', 'steelblue');
