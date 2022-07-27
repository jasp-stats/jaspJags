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

		ChangeJS
		{
			name: "acfType"
			jsFunction: function(options)
			{
				switch (options["acfType"])
				{
					case "acfLines":			return "lines";
					case "acfBars":				return "bars";
				}
			}
		}

		ChangeRename	{	from:	"plotBivarHex";						to:		"bivariateScatterPlot"					}

		ChangeRename	{	from:	"bivariateScatterDiagType"			to:		"bivariateScatterDiagonalType"			}
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

		ChangeRename	{	from:	"bivariateScatterOffDiagType"		to:		"bivariateScatterOffDiagonalType"		}
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

		ChangeRename	{	from:	"showResultsFor"			to:		"resultsFor"			}
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

		ChangeRename	{	from:	"showDeviance"						to:		"deviance"								}

	}
}