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

import QtQuick			2.8
import QtQuick.Layouts	1.3


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
		AssignedVariablesList   { name: "monitoredParameters";   title: qsTr("Monitor these parameters"); }
	}

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList
		{
			id:		monitoredParametersList2
			name:	"monitoredParametersList2"
			title:	allParameters.checked ? qsTr("Parameters in model") : qsTr("Monitored parameters")
			source:	allParameters.checked ? [{ name: "model", discard: { name: "userData", use: "Parameter"}}] : ["monitoredParameters"]
		}
		AssignedVariablesList   { name: "monitoredParametersShown";		title: qsTr("Show results for these parameters")}
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
					name: "autoCorPlotLags"
					label: qsTr("No. lags")
					defaultValue: 20
					min: 1
					max: 100
				}
				RadioButtonGroup
				{
					name: "autoCorPlotType"
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
		title: qsTr("Customizable Inference")

		TabView
		{
			id:					customizablePlots
			name:				"customPlots"
			maximumItems:		10
			newItemName:		qsTr("Plot 1")
			optionKey:			"customPlotOptions"
			Layout.columnSpan:	2

			content: Group
			{

				Group // Parameter selection
				{
					indent: true
					columns: 1

					Group
					{
						columns: 1
						title: qsTr("Parameter selection")

						DropDown
						{
							label: 				qsTr("Parameter")
							name: 				"customizablePlotsParameter"
							fieldWidth:			200 * preferencesModel.uiScale
							source:				{
								let m = allParameters.checked ? [{ name: "model", discard: { name: "userData", use: "Parameter"}}] : ["monitoredParameters"];
//									m.insert(0,
//										{
//											// TODO: I'd like this to be displayed differently as a placeholder value!
//											label: "<i style=\"color:%1;\">%2</i>".arg(jaspTheme.textDisabled).arg(qsTr("Select a parameter")),
//											value: ""
//										}
//									)
								m
							}
							controlMinWidth:	200 * preferencesModel.uiScale
						}

						TextField
						{
							id:						factorName
							label:					qsTr("Parameter subset")
							name:					"customizablePlotsParameterSubset"
							placeholderText:		qsTr("Optional subset, e.g., 1:4 or 1, 3, 5:8 ")
							fieldWidth:				200 * preferencesModel.uiScale
							useExternalBorder:		false
							showBorder:				true
						}


						// TODO: this should just always be true
						CheckBox { name: "showResultsInTable"; label: qsTr("Show table with results"); checked: true }
					}
				}

				Group // Plots
				{
					indent: true
					columns: 1

					Group
					{
						title: qsTr("Plots")
						columns: 1

						DropDown
						{
							label: 				qsTr("Plot type")
							name: 				"customizablePlotsType"
							indexDefaultValue:	0
							values:
							[
								{ label: qsTr("Stacked density"),		value: "stackedDensity"		},
								{ label: qsTr("Density"),				value: "density"			}
							]
						}

						Group
						{
							columns: 2

							RadioButtonGroup
							{
								name: "customizablePlotsParameterOrder"
								title: qsTr("Order parameters by")
								RadioButton { value: "orderMean";		label: qsTr("Mean");	checked:true	}
								RadioButton { value: "orderMedian";		label: qsTr("Median")					}
								RadioButton { value: "orderSubset";		label: qsTr("Subset")					}
							}

							RadioButtonGroup
							{
								title: qsTr("Intervals")
								name: "customizablePlotsParameterInterval"
								RadioButton
								{
									value: "credibleIntervalShown"; label: qsTr("Credible Interval")
									childrenOnSameRow: true
									CIField { name: "credibleIntervalValue" }
								}
								RadioButton
								{
									value: "hdiShown"; label: qsTr("HDI")
									childrenOnSameRow: true
									CIField { name: "hdiValue" }
								}
								RadioButton
								{
									name:				"customizableShade"
									label:				qsTr("Shade area where")
									childrenOnSameRow:	true

									FormulaField
									{
										id:				probTableValueLow
										name:			"probTableValueLow"
										label:			""
										defaultValue:	0.0
										fieldWidth: 	40
	//									Layout.rightMargin: 0
										anchors.rightMargin: jaspTheme.labelSpacing
		//								anchors
		//								{
		//									rightMargin: 0
		//								}
									}

									Label
									{
										Layout.leftMargin:	0
										Layout.rightMargin: 0
		//								anchors
		//								{
		//									leftMargin: 0
		//									rightMargin: 0
		//								}
										// TODO: use \theta!
										text: qsTr("< \u03B8 <")
									}

									FormulaField
									{
										Layout.leftMargin:	0
		//								anchors
		//								{
		//									leftMargin: 0
		//								}
										name:			"probTableValueHigh"
										label:			""
										defaultValue:	1.0
										fieldWidth: 	40
									}
								}
							}
						}

						Group
						{
							title: qsTr("x-axis range")
							FormulaField
							{
								id:					minX
								label:				qsTr("min")
								name:				"customizablePlotsMinX"
								value:				"-Inf"
								max:				maxX.value
								inclusive:			JASP.None
								useExternalBorder:	false
								showBorder:			true
								controlXOffset:		6 * preferencesModel.uiScale
								fieldWidth:			40 * preferencesModel.uiScale
							}
							FormulaField
							{
								id:					maxX
								label:				qsTr("max")
								name:				"customizablePlotsMaxX"
								value:				"Inf"
								min:				minX.value
								inclusive:			JASP.None
								useExternalBorder:	false
								showBorder:			true
								controlXOffset:		6 * preferencesModel.uiScale
								fieldWidth:			40 * preferencesModel.uiScale
							}
						}
					}
				}

				Group // Estimation
				{
					indent: true
					columns: 1

					Group
					{
						title: qsTr("Estimation")
						columns: 2

						Group
						{
							title: qsTr("Summary statistics")
							CheckBox { name: "mean";	label: qsTr("Mean");	checked: true	}
							CheckBox { name: "median";	label: qsTr("Median");	checked: true	}
							CheckBox { name: "sd";		label: qsTr("SD");		checked: true	}

						}

						Group
						{
							title: qsTr("Intervals")
							CheckBox
							{
								name: "inferenceCredibleIntervalShown"; label: qsTr("Credible Interval")
								childrenOnSameRow: true
								CIField { name: "inferenceCredibleIntervalValue" }
							}
							CheckBox
							{
								name: "inferenceHdiShown"; label: qsTr("HDI")
								childrenOnSameRow: true
								CIField { name: "inferenceHdiValue" }
							}
							CheckBox
							{
								name:				"inferenceCustomizableShade"
								label:				qsTr("Shade area where")
								childrenOnSameRow:	true

								FormulaField
								{
									id:				inferenceCustomLow
									name:			"inferenceCustomLow"
									label:			""
									defaultValue:	0.0
									fieldWidth: 	40
									Layout.rightMargin: 0
	//								anchors
	//								{
	//									rightMargin: 0
	//								}
								}

								Label
								{
									Layout.leftMargin:	0
									Layout.rightMargin: 0
	//								anchors
	//								{
	//									leftMargin: 0
	//									rightMargin: 0
	//								}
									// TODO: use \theta!
									text: qsTr("< \u03B8 <")
								}

								FormulaField
								{
									Layout.leftMargin:	0
	//								anchors
	//								{
	//									leftMargin: 0
	//								}
									name:			"inferenceCustomHigh"
									label:			""
									defaultValue:	1.0
									fieldWidth: 	40
								}
							}
						}

					}
				}

				Group // Testing
				{
					indent: true
					columns: 1

					Group
					{
						title: qsTr("Testing")
						columns: 1

						CheckBox
						{
							id:					customizableSavageDickey
							name:				"customizableSavageDickey"
							label:				qsTr("Savage-Dickey")
							childrenOnSameRow:	true
							FormulaField
							{
								label:				""
								name:				"customizableSavageDickeyPoint"
								value:				"0"
								inclusive:			JASP.None
								useExternalBorder:	false
								showBorder:			true
								fieldWidth:			40 * preferencesModel.uiScale
								controlXOffset:		6 * preferencesModel.uiScale
							}
						}

						RadioButtonGroup
						{
							enabled: customizableSavageDickey.checked
							name: "customizableSavageDickeyPriorMethod"
							title: qsTr("Determine prior height with")
							// TODO: figure out if we can do without this!
//							RadioButton
//							{
//								value: "sampledParameter";		label: qsTr("Parameter");	checked:true
//								childrenOnSameRow:	true
//								DropDown
//								{
//									label: 				""
//									name: 				"customizableSavageDickeySamplingType"
//									indexDefaultValue:	0
//									values:
//									[
//										{ label: qsTr("Normal kernel"),		value: "normalKernel"		},
//										{ label: qsTr("Splines"),			value: "splines"			}
//									]
//								}
//							}
							RadioButton
							{
								value: "sampling";		label: qsTr("Sampling");	checked:true
								childrenOnSameRow:	true
								DropDown
								{
									label: 				""
									name: 				"customizableSavageDickeySamplingType"
									indexDefaultValue:	0
									values:
									[
										{ label: qsTr("Normal kernel"),		value: "normalKernel"		},
										{ label: qsTr("Splines"),			value: "splines"			}
									]
								}
							}
							RadioButton
							{
								value: "manual";		label: qsTr("Manual value")
								childrenOnSameRow:	true
								FormulaField
								{
									label:				""
									name:				"customizableSavageDickeyPriorHeight"
									value:				"0"
									inclusive:			JASP.None
									useExternalBorder:	false
									showBorder:			true
									fieldWidth:			40 * preferencesModel.uiScale
									controlXOffset:		6 * preferencesModel.uiScale
								}
							}
						}

						RadioButtonGroup
						{
							enabled: customizableSavageDickey.checked
							name: "customizableSavageDickeyPosteriorMethod"
							title: qsTr("Determine posterior height with")
							RadioButton
							{
								value: "samplingPosteriorPoint";		label: qsTr("Sampling");	checked:true
								childrenOnSameRow:	true
								DropDown
								{
									label: 				""
									name: 				"customizableSavageDickeyPosteriorSamplingType"
									indexDefaultValue:	0
									values:
									[
										{ label: qsTr("Normal kernel"),		value: "normalKernel"		},
										{ label: qsTr("Splines"),			value: "splines"			}
									]
								}
							}
							RadioButton { value: "normalApproximation";		label: qsTr("Normal approximation") }
						}


					}
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
				id: samples
				name: "samples"
				label: qsTr("No. samples")
				defaultValue: 2e3
				min: 10
				max: 1e9
				fieldWidth: 100
			}
			IntegerField
			{
				name: "burnin"
				label: qsTr("No. burnin samples")
				defaultValue: 500
				min: 1
				max: 1e9
				fieldWidth: 100
			}
			IntegerField
			{
				name: "thinning"
				label: qsTr("Thinning")
				defaultValue: 1
				min: 1
				max: Math.floor(samples.value / 2)
				fieldWidth: 100
			}
			IntegerField
			{
				name: "chains"
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

		FileSelector
		{
			name:		"exportSamplesFile"
			label:		qsTr("Export samples:")
			filter:		"*.csv"
			save:		true
		}

		// Same setup as e.g., DOE in jaspProcessControl
		// use the button to toggle a hidden checkbox that controls syncing
		Button
		{
			anchors.right:		parent.right
			anchors.bottom:		parent.bottom
			text: 				actualExporter.checked ? qsTr("Sync Samples: On") : qsTr("Sync Samples: Off")
			onClicked: 			actualExporter.click()
		}

		CheckBox
		{
			id:					actualExporter
			name:				"actualExporter"
			visible:			false
		}

	}
}
