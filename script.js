
// set canvas
var margin = {top: 100, right: 75, bottom: 125, left: 50}
var width = 800 - margin.left - margin.right;
var height = 400 - margin.top - margin.bottom;

// create color palette
// from colorbrewer: http://colorbrewer2.org/?type=qualitative&scheme=Set2&n=7
var colors = ['#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f','#e5c494']

d3.json("hisp_uninsured_rates_json.json", function(error, data) {
	dataset = data;
	makeBargraph();
});

function makeBargraph() {
  // create and dertermine size of SVG
  var svg = d3.select("#chart")
    .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom);

  // setup x scale
  var xScale = d3.scaleBands()
    .domain(d3.extent(dataset, function(d) {return d.year; }))
    .range([0, width])
    .nice();
  
  // setup y scale
  var yScale = d3.scaleLinear()
    .domain([d3.min(dataset, function (d) { return d.percent_hisp_uninsured_year; }), 
            d3.max(dataset, function (d) { return d.percent_hisp_uninsured_year; })])
    .range([height, 0])
    .nice();

  var g = svg.append('g')
  	.attr('transform', 'translate(' + margin + ',' + margin + ')');

};