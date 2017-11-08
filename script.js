
// set canvas
var margin = {top: 100, right: 75, bottom: 125, left: 50}
var width = 800 - margin.left - margin.right;
var height = 400 - margin.top - margin.bottom;

// create color palette
// from colorbrewer: http://colorbrewer2.org/?type=qualitative&scheme=Set2&n=7
var colors = ['#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f','#e5c494']

d3.json("hisp_uninsured_rates_json.json", function(error, data) {
      if (error) {
        console.log(error); //if error, load error to console
      } else {
        dataset = data,
        console.log(dataset),
        makeBargraph(); //make a barchart
      }
    });

// code source: https://www.pshrmn.com/tutorials/d3/bar-charts/

function makeBargraph() {
  // create and dertermine size of SVG
  var svg = d3.select("#chart")
    .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append('g')
  	.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  // setup x scale
  var xScale = d3.scaleBand()
    .domain(d3.range(dataset.length))
    .range([0, width])
    .padding(0.1);
  
  // width of the bar is determined by the x scale
  var bandwidth = xScale.bandwidth()
  
  // setup y scale
  var yScale = d3.scaleLinear()
    .domain([0, d3.max(dataset, function (d) { return d.percent_hisp_uninsured_year; })])
    .range([height, 0])
    .nice();

  var xAxis = d3.axisBottom(xScale);
  var yAxis = d3.axisLeft(yScale);

  // draw the axes
  svg.append('g')
  	.classed('x axis', true)
  	.attr('transform', 'translate(0,' + height + ')')
  	.call(xAxis);

  var yAxisEle = svg.append('g')
  	.classed('y axis', true)
  	.call(yAxis);

  // add a label to the yAxis
  var yText = yAxisEle.append('text')
  	.attr('transform', 'rotate(-90)translate(-' + height/2 + ',0)')
  	.style('text-anchor', 'middle')
  	.style('fill', 'black')
  	.attr('dy', '-2.5em')
  	.style('font-size', 14)
  	.text('Hispanic Uninsured Rate');
  
  var barHolder = svg.append('g')
  	.classed('bar-holder', true);

  var bars = barHolder.selectAll('rect.bar')
    .data(dataset)
  .enter().append('rect')
    .classed('bar', true)
    .attr('x', function(d, i) {
      // the x value is determined using the
      // year of the datum
      return xScale(d.year)
    })
    .attr('width', bandwidth)
    .attr('y', function(d) {
      // the y position is determined by the datum's uninsured rate
      // this value is the top edge of the rectangle
      return yScale(d.percent_hisp_uninsured_year);
    })
    .attr('height', function(d) {
      // the bar's height should align it with the base of the chart (y=0)
      return height - yScale(d.percent_hisp_uninsured_year);
    });
};