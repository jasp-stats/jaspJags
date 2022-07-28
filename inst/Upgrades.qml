import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName:		"NetworkAnalysis"
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

		ChangeRename	{	from:	"aggregateChains"; 					to: 	"aggregatedChains"						}
		ChangeRename	{	from:	"showLegend";		 				to: 	"legend"								}
		ChangeRename	{	from:	"plotDensity";						to:		"densityPlot"							}
		ChangeRename	{	from:	"plotHistogram";					to:		"histogramPlot"							}
		ChangeRename	{	from:	"plotTrace";						to:		"tracePlot"								}

		ChangeRename	{	from:	"plotAutoCor";						to:		"autoCorPlot"							}
		ChangeRename	{	from:	"acfType"	;						to:		"autoCorPlotType"							}

		ChangeJS
		{
			name: "autoCorPlotType"
			jsFunction: function(options)
			{
				switch (options["autoCorPlotType"])
				{
					case "acfLines":			return "lines";
					case "acfBars":				return "bars";
				}
			}
		}

		ChangeRename	{	from:	"plotBivarHex";						to:		"bivariateScatterPlot"					}

		ChangeRename	{	from:	"bivariateScatterDiagType";			to:		"bivariateScatterDiagonalType"			}
		ChangeJS
		{
			name: "bivariateScatterDiagonalType"
			jsFunction: function(options)
			{
				switch (options["bivariateScatterDiagonalType"])
				{
					case "dens":				return "density";
					case "hist":				return "histogram";
				}
			}
		}

		ChangeRename	{	from:	"bivariateScatterOffDiagType";		to:		"bivariateScatterOffDiagonalType"		}
		ChangeJS
		{
			name: "bivariateScatterOffDiagonalType"
			jsFunction: function(options)
			{
				switch (options["bivariateScatterOffDiagonalType"])
				{
					case "hex":					return "hexagon";
					case "scatter":				return "contour";
				}
			}
		}

		ChangeRename	{	from:	"noSamples";						to:		"samples"								}
		ChangeRename	{	from:	"noBurnin";							to:		"burnin"								}
		ChangeRename	{	from:	"noThinning";						to:		"thinning"								}
		ChangeRename	{	from:	"noChains";							to:		"chains"								}

		ChangeRename	{	from:	"showResultsFor";					to:		"resultsFor"							}
		ChangeJS
		{
			name: "bivariateScatterDiagonalType"
			jsFunction: function(options)
			{
				switch (options["bivariateScatterDiagonalType"])
				{
					case "monitorAllParameters":				return "allParameters";
					case "monitorSelectedParameters":			return "selectedParameters";
				}
			}
		}

		ChangeRename	{	from:	"showDeviance";						to:		"deviance"								}

	}
}