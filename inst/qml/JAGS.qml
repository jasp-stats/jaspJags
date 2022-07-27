//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick.Layouts	1.3
import QtQuick			2.8

import JASP.Widgets		1.0
import JASP.Controls	1.0

Form
{
	columns: 1
	JAGSTextArea
	{
		id:			jagsModel
		title:		qsTr("Enter JAGS model below")
		name:		"model"
		text:		"model{\n\n}"
	}

	VariablesForm
	{
		visible: !allParameters.checked
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList
		{
			name: "parametersList";
			title: qsTr("Parameters in model")
			source: [{ name: "model", discard: { name: "userData", use: "Parameter"}}]
		}
		AssignedVariablesList   { name: "monitoredParametersList";   title: qsTr("Monitor these parameters"); }
	}

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList
		{
			id:		monitoredParametersList2
			name:	"monitoredParametersList2"
			title:	allParameters.checked ? qsTr("Parameters in model") : qsTr("Monitored parameters")
			source:	allParameters.checked ? [{ name: "model", discard: { name: "userData", use: "Parameter"}}] : ["monitoredParametersList"]
		}
		AssignedVariablesList   { name: "parametersShown";		title: qsTr("Show results for these parameters")}
	}

	Section
	{
		title: qsTr("Plots")
		Group
		{
			DropDown
			{
				name: "colorScheme"
				indexDefaultValue: 0
				label: qsTr("Color scheme for plots:")
				values:
					[
					{ label: qsTr("Colorblind"),		value: "colorblind"		},
					{ label: qsTr("Colorblind Alt."),	value: "colorblind2"	},
					{ label: qsTr("Viridis"),			value: "viridis"		},
					{ label: qsTr("Blue"),				value: "blue"			},
					{ label: qsTr("Gray"),				value: "gray"			}
				]
			}
			CheckBox { name: "aggregatedChains";	label: qsTr("Aggregate chains for densities and histograms");	checked:true	}
			CheckBox { name: "legend";				label: qsTr("Show legends");									checked:true	}
			CheckBox { name: "densityPlot";			label: qsTr("Density")															}
			CheckBox { name: "histogramPlot";		label: qsTr("Histogram")														}
			CheckBox { name: "tracePlot";			label: qsTr("Trace");															}
		}
		Group
		{
			CheckBox { label: qsTr("Autocorrelation");	name: "autoCorPlot"; id: autoCorrelation
				IntegerField
				{
					name: "noLags"
					label: qsTr("No. lags")
					defaultValue: 20
					min: 1
					max: 100
				}
				RadioButtonGroup
				{
					name: "acfType"
					title: qsTr("Type")
					RadioButton { value: "lines";	label: qsTr("line"); checked:true	}
					RadioButton { value: "bars";	label: qsTr("bar")					}
				}
			}
			CheckBox { label: qsTr("Bivariate scatter");  name: "bivariateScatterPlot"; id: bivariateScatter
				RadioButtonGroup
				{
					name: "bivariateScatterDiagonalType"
					title: qsTr("Diagonal plot type")
					RadioButton { value: "density";		label: qsTr("Density"); checked:true	}
					RadioButton { value: "histogram";	label: qsTr("Histogram")				}
				}
				RadioButtonGroup
				{
					name: "bivariateScatterOffDiagonalType"
					title: qsTr("Off-diagonal plot type")
					RadioButton { value: "hexagon";		label: qsTr("Hexagonal"); checked:true	}
					RadioButton { value: "contour";		label: qsTr("Contour")					}
				}
			}
		}
	}

	Section
	{
		title: qsTr("Initial Values")
		JagsTableView
		{
			name				:	"initialValues"
			isFirstColEditable	:	false
			showButtons			:	false
			source				:	[{ name: "model", discard: { name: "userData", use: "Parameter"}}]
		}
	}

	Section
	{
		title: qsTr("Observed Values")
		JagsTableView
		{
			name		:	"userData"
		}
	}

	Section
	{
		title: qsTr("Advanced")
		columns: 2
		Group
		{
			title: qsTr("MCMC parameters")
			IntegerField
			{
				id: noSamples
				name: "noSamples"
				label: qsTr("No. samples")
				defaultValue: 2e3
				min: 10
				max: 1e9
				fieldWidth: 100
			}
			IntegerField
			{
				name: "noBurnin"
				label: qsTr("No. burnin samples")
				defaultValue: 500
				min: 1
				max: 1e9
				fieldWidth: 100
			}
			IntegerField
			{
				name: "noThinning"
				label: qsTr("Thinning")
				defaultValue: 1
				min: 1
				max: Math.floor(noSamples.value / 2)
				fieldWidth: 100
			}
			IntegerField
			{
				name: "noChains"
				label: qsTr("No. chains")
				defaultValue: 3
				min: 1
				max: 50
				fieldWidth: 100
			}
		}

		RadioButtonGroup
		{
			name: "resultsFor"
			title: qsTr("Show results for")
			RadioButton { value: "allParameters";		label: qsTr("all monitored parameters"); checked: true; id: allParameters	}
			RadioButton { value: "selectedParameters";	label: qsTr("selected parameters")											}
		}

		SetSeed{}

		CheckBox {	name: "deviance";	label: qsTr("Show Deviance");	checked: false	}
	}
}
