import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name:			"jaspJags"
	title : 		qsTr("JAGS")
	description:	qsTr("Implement Bayesian graphical models with Martyn Plummer’s JAGS program for Markov chain Monte Carlo")
	version:		"0.15"
	author:			"Don van den Bergh"
	maintainer:		"Don van den Bergh <d.vandenBergh@jasp-stats.com>"
	website:		"https://github.com/vandenman/BayesianReliability"
	license:		"GPL (>= 2)"
	icon:     		"analysis-JAGS.svg"
	requiresData: 	false

	Analysis
	{
		title   : qsTr("JAGS")
		qml     : "JAGS.qml"
		func: "JAGS"
	}
}
