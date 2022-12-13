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
import "./common" as Common

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
			name:				"customInference"
			maximumItems:		10
			newItemName:		qsTr("Plot 1")
			optionKey:			"customInferenceOptions"
			Layout.columnSpan:	2

			content: Group
			{

				Group // Parameter selection
				{
					indent: true
					columns: 1

					Group
					{
						columns: 2
						Group
						{
							title: qsTr("Parameter selection")

							DropDown
							{
								label: 				qsTr("Parameter")
								name: 				"customInferenceParameter"
								fieldWidth:			200 * preferencesModel.uiScale
								source:				allParameters.checked ? ["monitoredParametersShown"] : ["monitoredParameters"]
								controlMinWidth:	200 * preferencesModel.uiScale
								addEmptyValue: 		true
								placeholderText: 	qsTr("Select a parameter")
							}

							TextField
							{
								id:						factorName
								label:					qsTr("Parameter subset")
								name:					"customInferenceParameterSubset"
								placeholderText:		qsTr("Optional subset, e.g., 1:4 or 1, 3, 5:8 ")
								fieldWidth:				200 * preferencesModel.uiScale
								useExternalBorder:		false
								showBorder:				true
							}
						}

						Group
						{

							enabled: mainWindow.dataAvailable
							title: qsTr("Superimpose data")

							DropDown
							{
								id:						customInferenceData
								label:					qsTr("Data")
								name:					"customInferenceData"
								fieldWidth:				200 * preferencesModel.uiScale
								controlMinWidth:		200 * preferencesModel.uiScale
								showVariableTypeIcon: 	true
								addEmptyValue: 			true
								placeholderText: 		qsTr("None")
							}

							DropDown
							{
								label:					qsTr("Split by")
								name:					"customInferenceDataSplit"
								fieldWidth:				200 * preferencesModel.uiScale
								controlMinWidth:		200 * preferencesModel.uiScale
								showVariableTypeIcon: 	true
								addEmptyValue: 			true
								placeholderText: 		qsTr("None")
							}
						}

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
							name: 				"customInferencePlotsType"
							indexDefaultValue:	0
							values:
							[
								{ label: qsTr("Stacked density"),		value: "stackedDensity"		}//,
								// TODO: more options
//								{ label: qsTr("Density"),				value: "density"			}
							]
							addEmptyValue: true
							placeholderText: 		qsTr("None")
						}

						Group
						{
							columns: 2

							RadioButtonGroup
							{
								name: "customInferenceParameterOrder"
								title: qsTr("Order parameters by")
								RadioButton { value: "orderMean";		label: qsTr("Mean");	checked:true	}
								RadioButton { value: "orderMedian";		label: qsTr("Median")					}
								RadioButton { value: "orderSubset";		label: qsTr("Subset")					}
							}

							CheckBox
							{
								label: qsTr("Shade interval")
								name : "shadeIntervalInPlot"

								RadioButtonGroup
								{
									title: qsTr("Intervals")
									name: "customInferencePlotInterval"
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
										label:				qsTr("Area where")
										childrenOnSameRow:	true

										Common.TwoInputField
										{
											name1:			"customInferencePlotCustomLow"
											name2:			"customInferencePlotCustomHigh"
											leftLabel:		""
											middleLabel:	qsTr("< \u03B8 < ")
											rightLabel:		""
										}
									}
								}
							}
							RadioButtonGroup
							{
								enabled:	customInferenceData.currentValue !== ""
								title:		qsTr("Plot type for superimposed data")
								name:		"customInferenceOverlayGeomType"
								RadioButton { value: "density"; label: qsTr("Density")	}
								RadioButton {
									value: "histogram"
									label: qsTr("Histogram")
									DropDown {
										name: "customInferenceOverlayHistogramBinWidthType"
										label: qsTr("Bin width type")
										indexDefaultValue: 0
										values: [
											{ label: qsTr("Sturges"),				value: "sturges"	},
											{ label: qsTr("Scott"),					value: "scott"		},
											{ label: qsTr("Doane"),					value: "doane"		},
											{ label: qsTr("Freedman-Diaconis"),		value: "fd"			},
											{ label: qsTr("Manual"),				value: "manual"		}
										]
										id: binWidthType
									}
									IntegerField
									{
										name:			"customInferenceOverlayHistogramManualNumberOfBins"
										label:			qsTr("Number of bins")
										defaultValue:	30
										min:			3;
										max:			10000;
										enabled:		binWidthType.currentValue === "manual"
									}
								}
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
							CheckBox { name: "mean";	label: qsTr("Mean");					checked: true	}
							CheckBox { name: "median";	label: qsTr("Median");					checked: true	}
							CheckBox { name: "sd";		label: qsTr("SD");						checked: true	}
							CheckBox { name: "rhat";	label: qsTr("R-hat");					checked: true	}
							CheckBox { name: "ess";		label: qsTr("Effective sample size");	checked: true	}
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
								label:				qsTr("Probability of")
								childrenOnSameRow:	true

								Common.TwoInputField
								{
									name1:			"inferenceCustomLow"
									name2:			"inferenceCustomHigh"
									leftLabel:		""
									middleLabel:	qsTr("< \u03B8 < ")
									rightLabel:		""
								}
							}
						}

					}
				}

				Group // Testing
				{
					// This part is not functional yet and therefore hidden and disabled
					visible: false
					enabled: false

					indent: true
					columns: 1				

					Group
					{
						title: qsTr("Testing")
						columns: 1

						CheckBox
						{
							id:					customInferenceSavageDickey
							name:				"customInferenceSavageDickey"
							label:				qsTr("Savage-Dickey")
							childrenOnSameRow:	true
							FormulaField
							{
								label:				""
								name:				"customInferenceSavageDickeyPoint"
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
							enabled: customInferenceSavageDickey.checked
							name: "customInferenceSavageDickeyPriorMethod"
							title: qsTr("Determine prior height with")
							// TODO for inference: figure out if we can do without this!
//							RadioButton
//							{
//								value: "sampledParameter";		label: qsTr("Parameter");	checked:true
//								childrenOnSameRow:	true
//								DropDown
//								{
//									label: 				""
//									name: 				"customInferenceSavageDickeySamplingType"
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
									name: 				"customInferenceSavageDickeySamplingType"
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
									name:				"customInferenceSavageDickeyPriorHeight"
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
							enabled: customInferenceSavageDickey.checked
							name: "customInferenceSavageDickeyPosteriorMethod"
							title: qsTr("Determine posterior height with")
							RadioButton
							{
								value: "samplingPosteriorPoint";		label: qsTr("Sampling");	checked:true
								childrenOnSameRow:	true
								DropDown
								{
									label: 				""
									name: 				"customInferenceSavageDickeyPosteriorSamplingType"
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
