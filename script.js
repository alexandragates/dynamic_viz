
// set canvas
var margin = {top: 100, right: 75, bottom: 125, left: 50}
var width = 800 - margin.left - margin.right;
var height = 400 - margin.top - margin.bottom;

d3.json('hisp_uninsured_rates_json.json', function(error, data) {
      if (error) {
        console.log(error); //if error, load error to console
      } else {
        console.log(data); //if data is correct, load data to console
        dataset = data.sort(function(x, y){
          return d3.ascending(x.year, y.percent_hisp_uninsured_year); }) //for the record: I would never put this data in ascending order like this.
        makeBargraph(); //make a barchart
      }
    });

function makeBargraph() {
  
  // create and dertermine size of SVG
  var svg = d3.select('#chart')
    .append('svg')
    .attr('width', width + margin.left + margin.right)
    .attr('height', height + margin.top + margin.bottom)
    .append('g')
  	.attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');

  // create a dataset of just the values that will populate the x axis
  var years = dataset.map(function(d) {
  	return d.year
  });

  // setup x and y scales
  var xScale = d3.scaleBand()
    .domain(years)
    .range([0, width])
    .padding(0.1);
  
  var yScale = d3.scaleLinear()
    .domain([0, d3.max(dataset, function (d) { return d.percent_hisp_uninsured_year; })])
    .range([0, height])
    .nice();

  // draw the axes
  var xAxisDraw = svg.append('g')
  	.attr('transform', 'translate(0,' + 30 + ')')
  	.attr('class', 'xaxis')
  	.call(d3.axisTop(xScale));

  var yAxisDraw = svg.append('g')
  	.attr('transform', 'translate(0,' + 30 + ')')
  	.attr('class', 'yaxis')
  	.call(d3.axisLeft(yScale));

  // add labels to the xAxis and yAxis
  var xText = svg.append('text')             
    .attr('transform', 'translate(' + (width/2) + ' ,' + (0) + ')')
    .attr('class', 'xText')
    .style('text-anchor', 'middle')
    .text('Year');
  
  var yText = yAxisDraw.append('text')
  	.attr('transform', 'rotate(-90)translate(-' + height/2 + ',0)')
  	.attr('class', 'yText')
  	.attr('dy', '-2.5em')
  	.style('text-anchor', 'middle')
  	.text('Hispanic Uninsured Rate (%)');

  // add title
  var title = svg.append('text')
  	.attr('transform', 'translate(' + (-40) + ' ,' + (-60) + ')')
  	.attr('class', 'title')
  	.text('The Hispanic uninsured rate has decreased since 2012.')

  // add subtitle
  var subtitle = svg.append('text')
  	.attr('transform', 'translate(' + (-40) + ' ,' + (-35) + ')')
  	.attr('class', 'subtitle')
  	.text('Percent of Hispanics who are Uninsured, 2012 - 2016')
  
  // draw the bars!!!
  var bars = svg.selectAll('bar')
  	.data(dataset)
    .enter().append('rect')
  	.attr('x', function(d, i) {return xScale(d.year)})
    .attr('width', xScale.bandwidth())
    .attr('y', function(d) {return 30;}) 
    .attr('height', function(d) {return yScale(d.percent_hisp_uninsured_year);}) 
    .classed('colorme', function(d)
    	{if (d.percent_hisp_uninsured_year < 20) { return true}
    	else { return false}})
    .classed('dontcolorme', function(d)
    	{if (d.percent_hisp_uninsured_year < 20) { return false}
    	else { console.log("true"); return true}})

var legend1 = svg.append('g')
 	.attr('class', 'legend')
  	.attr('height', 100)
  	.attr('width', 100)

 legend1.selectAll('rect')
  	.data(dataset)
  	.enter().append('rect')
  	.attr('x', width - 550)
  	.attr('y', height + 60)
  	.attr('width', 15)
  	.attr('height', 15)
  	.classed('colorme', true)

 legend1.selectAll('text')
	.data(dataset)
	.enter().append('text')
	.attr('x', width - 525)
  	.attr('y', height + 73)
  	.attr('class', 'ltext')
  	.text('Lowest Uninsured Rate')

var legend2 = svg.append('g')
 	.attr('class', 'legend')
  	.attr('height', 100)
  	.attr('width', 100)

 legend2.selectAll('rect')
  	.data(dataset)
  	.enter().append('rect')
  	.attr('x', width - 300)
  	.attr('y', height + 60)
  	.attr('width', 15)
  	.attr('height', 15)
  	.classed('dontcolorme', true)

 legend2.selectAll('text')
	.data(dataset)
	.enter().append('text')
	.attr('x', width - 275)
  	.attr('y', height + 73)
  	.attr('class', 'ltext')
  	.text('Not Lowest Uninsured Rate')

};