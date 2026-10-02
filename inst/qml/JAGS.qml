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

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls
import "./common" as Common

Form
{
	info: qsTr("JAGS -- Just Another Gibbs Sampler -- is software for general purpose Bayesian inference. One can specify a model using JAGS syntax and let JAGS draw samples from the posterior distribution.")
	infoBottom: "## " + qsTr("References") + "\n"
				+	"- Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical models using Gibbs sampling. In K. Hornik, F. Leisch, & A. Zeileis (Eds.), *Proceedings of the 3rd international workshop on distributed statistical computing.* Vienna, Austria." + "\n"
				+ "\n---\n"
				+ "## " + qsTr("R Packages") + "\n"
				+	"- coda\n"
				+	"- ggplot2\n"
				+	"- graphics\n"
				+	"- hexbin\n"
				+	"- rjags\n"
				+	"- stats\n"
				+	"- stringr\n"
	columns: 1
	JAGSTextArea
	{
		id:			jagsModel
		title:		qsTr("Enter JAGS model below")
		name:		"model"
		info:		qsTr("Enter the desired model. Columns in the data can be directly referred to. If these contain spaces, then the reference must also contain spaces.")
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
			info: qsTr("A box that automatically detects which parameters the model contains.")
			source: [{ name: "model", discard: { name: "userData", use: "Parameter"}}]
		}
		AssignedVariablesList   { name: "monitoredParameters";   title: qsTr("Monitor these parameters"); info: qsTr("The parameters for which the MCMC samples are stored. Only available when 'Show results for' is set to 'selected parameters'.") }
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
		AssignedVariablesList   { name: "monitoredParametersShown";		title: qsTr("Show results for these parameters"); info: qsTr("Determines which parameters are shown in tables and plots.") }
	}

	Section
	{
		title: qsTr("Observed Values")
		info: qsTr("Sometimes it is practical to directly specify the observed data without loading a data set. Here such observations can be listed.")
		JagsTableView
		{
			name		:	"userData"
			info		:	qsTr("Each row has a 'Parameter', the name to be used in the JAGS model code, and an 'R Code', the value for the data. This value can also be R code.")
		}
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
				info: qsTr("Determines the color scheme of the plots.")
				values:
					[
					{ label: qsTr("Colorblind"),		value: "colorblind"		},
					{ label: qsTr("Colorblind Alt."),	value: "colorblind2"	},
					{ label: qsTr("Viridis"),			value: "viridis"		},
					{ label: qsTr("Blue"),				value: "blue"			},
					{ label: qsTr("Gray"),				value: "gray"			}
				]
			}
			CheckBox { name: "aggregatedChains";	label: qsTr("Aggregate chains for densities and histograms");	checked:true;	info: qsTr("If checked, the samples of different chains are aggregated in density plots and histograms. If unchecked, there are separate colors per chain.")	}
			CheckBox { name: "legend";				label: qsTr("Show legends");									checked:true;	info: qsTr("Show a legend in the plots.")	}
			CheckBox { name: "densityPlot";			label: qsTr("Density");	info: qsTr("Show the marginal density of the posterior samples for each parameter selected under 'Show results for these parameters'.")	}
			CheckBox { name: "histogramPlot";		label: qsTr("Histogram");	info: qsTr("Show the marginal histogram of the posterior samples for each parameter selected under 'Show results for these parameters'.")	}
			CheckBox { name: "tracePlot";			label: qsTr("Trace");		info: qsTr("Show a trace plot of the posterior samples for each parameter selected under 'Show results for these parameters'.")	}
		}
		Group
		{
			CheckBox { label: qsTr("Autocorrelation");	name: "autoCorPlot"; id: autoCorrelation
				info: qsTr("Plot the autocorrelation of the posterior samples for each parameter selected under 'Show results for these parameters'.")
				IntegerField
				{
					name: "autoCorPlotLags"
					label: qsTr("No. lags")
					info: qsTr("The maximum number of lags to show in the autocorrelation plot.")
					defaultValue: 20
					min: 1
					max: 100
				}
				RadioButtonGroup
				{
					name: "autoCorPlotType"
					title: qsTr("Type")
					info: qsTr("Whether to display the autocorrelation as a bar at each lag, or as a line that connects subsequent lags.")
					RadioButton { value: "lines";	label: qsTr("line"); checked:true	}
					RadioButton { value: "bars";	label: qsTr("bar")					}
				}
			}
			CheckBox { label: qsTr("Bivariate scatter");  name: "bivariateScatterPlot"; id: bivariateScatter
				info: qsTr("Show a matrix plot of all pairs of parameters. Only shows output when more than 1 parameter is sampled.")
				RadioButtonGroup
				{
					name: "bivariateScatterDiagonalType"
					title: qsTr("Diagonal plot type")
					info: qsTr("Show a density plot or a histogram on the diagonal entries of the scatter plot.")
					RadioButton { value: "density";		label: qsTr("Density"); checked:true	}
					RadioButton { value: "histogram";	label: qsTr("Histogram")				}
				}
				RadioButtonGroup
				{
					name: "bivariateScatterOffDiagonalType"
					title: qsTr("Off-diagonal plot type")
					info: qsTr("Show a hexagonal bivariate density plot, or a contour plot on the off-diagonal entries of the scatter plot.")
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
			info:				qsTr("Each tab specifies a set of custom results (a plot and a table) for one parameter. Up to 10 tabs can be added.")
			maximumItems:		10
			newItemName:		qsTr("Plot 1")
			optionKey:			"name"
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
							info: qsTr("For which parameter to show custom results.")

							DropDown
							{
								label: 				qsTr("Parameter")
								name: 				"parameter"
								info:				qsTr("Select a parameter from the model.")
								fieldWidth:			200 * preferencesModel.uiScale
								source:				allParameters.checked ? ["monitoredParametersShown"] : ["monitoredParameters"]
								addEmptyValue: 		true
								placeholderText: 	qsTr("Select a parameter")
							}

							TextField
							{
								id:						factorName
								label:					qsTr("Parameter subset")
								name:					"parameterSubset"
								info:					qsTr("Optionally specify a subset of the parameter, e.g., 1:4 or 1, 3, 5:8.")
								placeholderText:		qsTr("Optional subset, e.g., 1:4 or 1, 3, 5:8 ")
								fieldWidth:				200 * preferencesModel.uiScale
								useExternalBorder:		false
								showBorder:				true
							}
						}

						Group
						{

							enabled: dataSetInfo.dataAvailable
							title: qsTr("Superimpose data")
							info: qsTr("Optionally, superimpose data on the plots. Only available when a data set is loaded.")

							DropDown
							{
								id:						customInferenceData
								label:					qsTr("Data")
								name:					"inferenceData"
								info:					qsTr("A scale variable in the data set to superimpose on the plots.")
								fieldWidth:				200 * preferencesModel.uiScale
								showVariableTypeIcon: 	true
								addEmptyValue: 			true
								placeholderText: 		qsTr("None")
								allowedColumns:			"scale"
							}

							DropDown
							{
								label:					qsTr("Split by")
								name:					"dataSplit"
								info:					qsTr("A nominal variable in the data set used to split the superimposed data.")
								fieldWidth:				200 * preferencesModel.uiScale
								showVariableTypeIcon: 	true
								addEmptyValue: 			true
								placeholderText: 		qsTr("None")
								allowedColumns:			"nominal"
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
							name: 				"plotsType"
							info:				qsTr("The type of plot to show. Currently, only 'None' or 'Stacked density' are supported.")
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
								name: "parameterOrder"
								title: qsTr("Order parameters by")
								info: qsTr("The metric used to order the parameters in the stacked density plot.")
								RadioButton { value: "orderMean";		label: qsTr("Mean");	checked:true	}
								RadioButton { value: "orderMedian";		label: qsTr("Median")					}
								RadioButton { value: "orderSubset";		label: qsTr("Subset")					}
							}

							CheckBox
							{
								label: qsTr("Shade interval")
								name : "shadeIntervalInPlot"
								info: qsTr("Whether to shade an interval in the plot.")

								RadioButtonGroup
								{
									title: qsTr("Intervals")
									name: "plotInterval"
									info: qsTr("The interval to shade: a credible interval, a highest density interval (HDI), or a custom interval.")
									RadioButton
									{
										value: "ci"; label: qsTr("Credible Interval")
										childrenOnSameRow: true
										CIField { name: "ciLevel" }
									}
									RadioButton
									{
										value: "hdi"; label: qsTr("HDI")
										childrenOnSameRow: true
										CIField { name: "hdiLevel" }
									}
									RadioButton
									{
										name:				"manual"
										label:				qsTr("Area where")
										info:				qsTr("Shade the area between the lower and the upper bound.")
										childrenOnSameRow:	true

										Common.TwoInputField
										{
											name1:			"plotCustomLow"
											name2:			"plotCustomHigh"
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
								info:		qsTr("The type of plot to show for the superimposed data. Only available when data are superimposed.")
								name:		"overlayGeomType"
								RadioButton { value: "density"; label: qsTr("Density")	}
								RadioButton {
									value: "histogram"
									label: qsTr("Histogram")
									DropDown {
										name: "overlayHistogramBinWidthType"
										label: qsTr("Bin width type")
										info: qsTr("The method used to determine the bin width of the histogram.")
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
										name:			"overlayHistogramManualNumberOfBins"
										info:			qsTr("The number of bins, when the bin width type is 'Manual'.")
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
						info: qsTr("Specify which statistics are computed for each parameter and shown in a table.")
						columns: 2

						Group
						{
							title: qsTr("Summary statistics")
							CheckBox { name: "mean";	label: qsTr("Mean");					checked: true; info: qsTr("The average value of the posterior samples.")	}
							CheckBox { name: "median";	label: qsTr("Median");					checked: true; info: qsTr("The middle value of the posterior samples.")	}
							CheckBox { name: "mode";	label: qsTr("Mode");					checked: true; info: qsTr("The value of the posterior distribution with the highest density.")	}
							CheckBox { name: "sd";		label: qsTr("SD");						checked: true; info: qsTr("The standard deviation of the posterior samples.")	}
							CheckBox { name: "rhat";	label: qsTr("R-hat");					checked: true; info: qsTr("The potential scale reduction factor, used to diagnose convergence of the MCMC chains.")	}
							CheckBox { name: "ess";		label: qsTr("Effective sample size");	checked: true; info: qsTr("An estimate of the number of independent samples from the posterior distribution.")	}
						}

						Group
						{
							title: qsTr("Intervals")
							CheckBox
							{
								name: "inferenceCi"; label: qsTr("Credible Interval")
								info: qsTr("An interval within which a certain percentage of the posterior samples fall.")
								childrenOnSameRow: true
								CIField { name: "inferenceCiLevel" }
							}
							CheckBox
							{
								name: "inferenceHdi"; label: qsTr("HDI")
								info: qsTr("Highest Density Interval: the narrowest interval containing a specified percentage of the posterior samples.")
								childrenOnSameRow: true
								CIField { name: "inferenceHdiLevel" }
							}

							CheckBox
							{
								name:				"inferenceManual"
								info:				qsTr("The proportion of the posterior samples that fall within a user-defined interval.")
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
							name:				"savageDickey"
							label:				qsTr("Savage-Dickey")
							childrenOnSameRow:	true
							FormulaField
							{
								label:				""
								name:				"savageDickeyPoint"
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
							name: "savageDickeyPriorMethod"
							title: qsTr("Determine prior height with")
							// TODO for inference: figure out if we can do without this!
//							RadioButton
//							{
//								value: "sampledParameter";		label: qsTr("Parameter");	checked:true
//								childrenOnSameRow:	true
//								DropDown
//								{
//									label: 				""
//									name: 				"savageDickeySamplingType"
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
									name: 				"savageDickeySamplingType"
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
								value: "manualSavageDickey";		label: qsTr("Manual value")
								childrenOnSameRow:	true
								FormulaField
								{
									label:				""
									name:				"savageDickeyPriorHeight"
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
							name: "savageDickeyPosteriorMethod"
							title: qsTr("Determine posterior height with")
							RadioButton
							{
								value: "samplingPosteriorPoint";		label: qsTr("Sampling");	checked:true
								childrenOnSameRow:	true
								DropDown
								{
									label: 				""
									name: 				"savageDickeyPosteriorSamplingType"
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
		info: qsTr("For each parameter in the model, it is possible to specify an initial value. Initial values can be numbers, but also R code. The R code is evaluated separately for each chain, so `rnorm(1)` yields different initial values for each chain.")
		JagsTableView
		{
			name				:	"initialValues"
			info				:	qsTr("Each row has a 'Parameter' of the model and an 'R Code', its initial value.")
			isFirstColEditable	:	false
			showButtons			:	false
			source				:	[{ name: "model", discard: { name: "userData", use: "Parameter"}}]
		}
	}

	Section
	{
		title: qsTr("Advanced")
		columns: 2
		Group
		{
			title: qsTr("MCMC parameters")
			info: qsTr("Unlike some other software programs, JASP first draws 'No. burnin samples' from the posterior distribution and afterward 'No. samples', keeping every nth sample as specified by 'Thinning'.")
			IntegerField
			{
				id: samples
				name: "samples"
				info: qsTr("The number of samples to draw from the posterior distribution that are used for results (tables, plots).")
				label: qsTr("No. samples")
				defaultValue: 2e3
				min: 10
				max: 1e9
				fieldWidth: 100
			}
			IntegerField
			{
				name: "burnin"
				info: qsTr("The number of samples to draw from the posterior distribution and immediately discard.")
				label: qsTr("No. burnin samples")
				defaultValue: 500
				min: 1
				max: 1e9
				fieldWidth: 100
			}
			IntegerField
			{
				name: "thinning"
				info: qsTr("Every nth value of 'No. samples' is kept for the results, where n is given by 'Thinning'.")
				label: qsTr("Thinning")
				defaultValue: 1
				min: 1
				max: Math.floor(samples.value / 2)
				fieldWidth: 100
			}
			IntegerField
			{
				name: "chains"
				info: qsTr("The number of MCMC chains to run.")
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
			info: qsTr("By default, 'all monitored parameters' is selected which implies that JASP stores the MCMC samples for all parameters in the model. However, for large JAGS models storing all MCMC samples may take too much memory. By selecting 'selected parameters', one can first decide for which parameters the MCMC samples should be stored, and in a next box, decide which of these parameters should be shown in the results.")
			title: qsTr("Show results for")
			RadioButton { value: "allParameters";		label: qsTr("all monitored parameters"); checked: true; id: allParameters	}
			RadioButton { value: "selectedParameters";	label: qsTr("selected parameters")											}
		}

		SetSeed { info: qsTr("Set a seed for JAGS. The same seed and model specification yields the same results.") }

		CheckBox {	name: "deviance";	label: qsTr("Show Deviance");	checked: false;	info: qsTr("Show the Deviance statistic.")	}

		FileSelector
		{
			name:		"exportSamplesFile"
			info:		qsTr("The CSV file to save the MCMC samples to. The samples are only written when 'Sync Samples' is on.")
			label:		qsTr("Export samples:")
			filter:		"*.csv"
			save:		true
		}

		// Same setup as e.g., DOE in jaspProcessControl
		// use the button to toggle a hidden checkbox that controls syncing
		Button
		{
			Layout.alignment: 	Qt.AlignBottom | Qt.AlignRight
			text: 				actualExporter.checked ? qsTr("Sync Samples: On") : qsTr("Sync Samples: Off")
			onClicked: 			actualExporter.click()
		}

		CheckBox
		{
			id:					actualExporter
			name:				"actualExporter"
			infoLabel:			qsTr("Sync Samples")
			info:				qsTr("When on, the MCMC samples are written to the export file, and written again each time the analysis is rerun. Toggled with the 'Sync Samples' button.")
			visible:			false
		}

	}
}
