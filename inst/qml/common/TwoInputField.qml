//
// Copyright (C) 2013-2023 University of Amsterdam
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
import JASP				1.0
import JASP.Widgets		1.0
import JASP.Controls	1.0

Item
{
	property	alias	name1:			leftInput.name;
	property	alias	name2:			rightInput.name;
	property	alias	leftLabel:		leftInput.label;
	property	alias	middleLabel:	leftInput.afterLabel;
	property	alias	rightLabel:		rightInput.afterLabel;

	property	int		fieldWidth:		.5 * jaspTheme.textFieldWidth / 2 // .5 * default of FormulaField


	id: twoInputField

	FormulaField
	{
		id:					leftInput

		defaultValue:		"0"
		value:				"0"
		min:				-Infinity
		max:				rightInput.value
		// default validator for FormulaField does not seem to reset the value if it's incorrect?
		validator:			JASPDoubleValidator { id: doubleValidatorLeft; bottom: leftInput.min; top: leftInput.max ; decimals: 50; notation: DoubleValidator.StandardNotation }
		fieldWidth:			twoInputField.fieldWidth

		// NOTE: this is not ideal, but seems necessary for childrenOnSameRow
		anchors
		{
			top:		parent.top
			topMargin:	parent.topMargin
			left:		parent.left
			leftMargin: parent.leftMargin
		}
	}

	FormulaField
	{
		id:					rightInput
		anchors.left:		leftInput.right
		anchors.top:		leftInput.top

		defaultValue:		"1"
		value:				"1"
		min:				leftInput.value
		max:				Infinity
		validator:			JASPDoubleValidator { id: doubleValidatorRight; bottom: rightInput.min; top: rightInput.max ; decimals: 50; notation: DoubleValidator.StandardNotation }
		fieldWidth:			twoInputField.fieldWidth
	}
}
