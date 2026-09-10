// 1. Limpiar el lienzo
svg.selectAll("*").remove();

var variedades = data.variedades;
var meses = data.meses;
var matrizDatos = data.matriz; // Contiene objetos: {v: ..., m: ..., total: ..., semanas: [...] }

// 2. Márgenes y Dimensiones
var margin = {top: 50, right: 80, bottom: 40, left: 110},
    plotWidth = width - margin.left - margin.right,
    plotHeight = height - margin.top - margin.bottom;

var g = svg.append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

// 3. Escalas de posición espacial
var x = d3.scaleBand()
    .range([0, plotWidth])
    .domain(d3.range(meses.length))
    .padding(0.05);

var y = d3.scaleBand()
    .range([0, plotHeight])
    .domain(d3.range(variedades.length))
    .padding(0.05);

// 4. Dibujar Ejes Estilizados
var xAxis = g.append("g")
    .attr("transform", "translate(0, 0)")
    .call(d3.axisTop(x).tickFormat(i => meses[i]));

xAxis.selectAll("text")
    .style("font-size", "0.8em")
    .style("font-family", "Inter, sans-serif")
    .style("fill", "#333");

xAxis.select(".domain").style("stroke", "#CBD5E1");

var yAxis = g.append("g")
    .call(d3.axisLeft(y).tickFormat(i => variedades[i]));

yAxis.selectAll("text")
    .style("font-size", "0.75em")
    .style("font-family", "Inter, sans-serif")
    .style("fill", "#333");

yAxis.select(".domain").remove();

// 5. Escala de color térmica: Rojo -> Amarillo -> Verde basado en el total
var minVal = d3.min(matrizDatos, d => d.total) || 0;
var maxVal = d3.max(matrizDatos, d => d.total) || 100;

var colorScale = d3.scaleLinear()
    .domain([minVal, (minVal + maxVal) / 2, maxVal])
    .range(["#f0f1f5", "#89a2fa", "#042bba"]);

// 6. Tooltip Flotante con micrográfico de barras horizontales
var tooltip = d3.select("body").selectAll(".hc-tooltip").data([1]);
tooltip = tooltip.enter()
    .append("div")
    .attr("class", "hc-tooltip")
    .style("position", "absolute")
    .style("visibility", "hidden")
    .style("background-color", "white")
    .style("box-shadow", "0px 4px 12px rgba(0,0,0,0.15)")
    .style("padding", "12px")
    .style("font-family", "Inter, sans-serif")
    .style("font-size", "12px")
    .style("pointer-events", "none")
    .style("border-radius", "6px")
    .style("z-index", "9999")
    .merge(tooltip);

// 7. Dibujar las Celdas del Heatmap
var cells = g.selectAll(".cell")
    .data(matrizDatos)
    .enter()
    .append("g");

cells.append("rect")
    .attr("x", d => x(d.m))
    .attr("y", d => y(d.v))
    .attr("width", x.bandwidth())
    .attr("height", y.bandwidth())
    .style("fill", d => colorScale(d.total))
    .style("stroke", "#ffffff")
    .style("stroke-width", "2px")
    .style("cursor", "pointer")
    .on("mouseover", function(event, d) {
        tooltip.style("visibility", "visible")
               .style("border-left", "4px solid " + colorScale(d.total));
    })
    .on("mousemove", function(event, d) {
        var maxSemana = d3.max(d.semanas) || 1;
        var maxBarWidth = 120;
        
        var barsHtml = d.semanas.map((val, idx) => {
            var w = Math.max(4, Math.round((val / maxSemana) * maxBarWidth));
            return `<div style="display: flex; align-items: center; margin-bottom: 5px;">
                      <span style="font-size: 10px; font-weight: 700; color: #374151; width: 22px;">S${idx+1}</span>
                      <div style="background-color: #1E6B34; height: 10px; width: ${w}px; border-radius: 2px; margin: 0 6px;"></div>
                      <span style="font-size: 10px; font-weight: 600; color: #4B5563; white-space: nowrap;">${val.toLocaleString()}</span>
                    </div>`;
        }).join('');

        tooltip.html(`
            <div style="font-family: Inter, sans-serif; min-width: 200px;">
                <b>Variedad:</b> ${variedades[d.v]}<br>
                <b>Mes:</b> ${meses[d.m]}<br>
                <b>Producción Total:</b> ${d.total.toLocaleString()} tallos<br>
                <div style="margin-top: 8px; border-top: 1px solid #E5E7EB; padding-top: 6px;">
                    <div style="font-size: 10px; font-weight: 800; color: #374151; margin-bottom: 6px; text-transform: uppercase;">Desglose Semanal:</div>
                    <div>${barsHtml}</div>
                </div>
            </div>
        `)
        .style("top", (event.pageY - 110) + "px")
        .style("left", (event.pageX + 15) + "px");
    })
    .on("mouseout", function() {
        tooltip.style("visibility", "hidden");
    })
    .on("click", function(event, d) {
        Shiny.setInputValue("celda_heatmap_clic", {
            variedad: variedades[d.v],
            mes: meses[d.m],
            produccion: d.total
        }, {priority: "event"});
        
        d3.selectAll("rect").style("stroke", "#ffffff").style("stroke-width", "2px");
        d3.select(this).style("stroke", "#1E6B34").style("stroke-width", "3px");
    });

// 8. Valores numéricos internos en las celdas
cells.append("text")
    .attr("x", d => x(d.m) + x.bandwidth() / 2)
    .attr("y", d => y(d.v) + y.bandwidth() / 2 + 4)
    .attr("text-anchor", "middle")
    .style("font-family", "Inter, sans-serif")
    .style("font-size", "0.75em")
    .style("font-weight", "700")
    .style("pointer-events", "none")
    .style("fill", d => d.total > ((minVal + maxVal) / 2) ? "#ffffff" : "#1F2937")
    .text(d => d.total.toLocaleString());

// 9. Barra de Leyenda Lateral Derecha
var legendWidth = 14,
    legendHeight = plotHeight;

var legendGroup = g.append("g")
    .attr("transform", "translate(" + (plotWidth + 20) + ", 0)");

var defs = svg.append("defs");
var linearGradient = defs.append("linearGradient")
    .attr("id", "thermal-gradient")
    .attr("x1", "0%")
    .attr("y1", "100%")
    .attr("x2", "0%")
    .attr("y2", "0%");

linearGradient.append("stop").attr("offset", "0%").attr("stop-color", "#f0f1f5");
linearGradient.append("stop").attr("offset", "50%").attr("stop-color", "#89a2fa");
linearGradient.append("stop").attr("offset", "100%").attr("stop-color", "#042bba");

legendGroup.append("rect")
    .attr("width", legendWidth)
    .attr("height", legendHeight)
    .style("fill", "url(#thermal-gradient)")
    .style("stroke", "#E5E7EB")
    .style("stroke-width", "1px");

var legendScale = d3.scaleLinear()
    .domain([minVal, maxVal])
    .range([legendHeight, 0]);

var legendAxis = d3.axisRight(legendScale)
    .ticks(5)
    .tickSize(4);

legendGroup.append("g")
    .attr("transform", "translate(" + legendWidth + ", 0)")
    .call(legendAxis)
    .selectAll("text")
    .style("font-size", "0.7em");