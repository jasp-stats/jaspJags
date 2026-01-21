import QtQuick
import JASP.Module

Description
{
	title : 		qsTr("JAGS")
	description:	qsTr("Implement Bayesian graphical models with Martyn Plummer’s JAGS program for Markov chain Monte Carlo")
	icon:     		"analysis-JAGS.svg"
	requiresData: 	false
	hasWrappers: 	true
	preloadData: 	false

	Analysis
	{
		title   : qsTr("JAGS")
		qml     : "JAGS.qml"
		func: "JAGS"
	}
}
