/* ************ *
 * INSTRUCTIONS *
 * ************ */

function Instruction(j, theta, phi, b, a) {
  this.j = j
  this.theta = theta
  this.phi = phi
  this.b = b
  this.a = a
}

var INSTRUCTION_TABLE_HEADER =
  "<tr>\n" +
  "  <th class=\"instruction-table-cell\">j</th>\n" +
  "  <th class=\"instruction-table-cell\">&theta;<sub>j</sub></th>\n" +
  "  <th class=\"instruction-table-cell\">&phi;<sub>j</sub></th>\n" +
  "  <th class=\"instruction-table-cell\">b<sub>j</sub></th>\n" +
  "  <th class=\"instruction-table-cell\">a<sub>j</sub></th>\n" +
  "</tr>"

function instructionsToTable(j, instructions) {
  return "<table class='instruction-table'>"
    + INSTRUCTION_TABLE_HEADER
    + instructions.map(function (i) { return instructionToRow(j, i) }).join("\n")
    + "</table>"
}

function instructionToRow(j, instruction) {
  return (instruction.j === j ? "<tr class='current-instruction'>\n" : "<tr>\n")
    + "  <td class='instruction-table-cell'>\n" + instruction.j + "</td>\n"
    + "  <td class='instruction-table-cell'>\n" + instructionStringToText(instruction.theta) + "</td>\n"
    + "  <td class='instruction-table-cell'>\n" + instructionStringToText(instruction.phi) + "</td>\n"
    + "  <td class='instruction-table-cell'>\n" + instruction.b + "</td>\n"
    + "  <td class='instruction-table-cell'>\n" + instruction.a + "</td>\n"
    + "</tr>"
}

function instructionStringToText(string) {
    return string.length === 0 ? "(empty)" : string
}

function renderInstructionTable(j, instructions) {
  document.getElementById("instructions-table-container").innerHTML =
    instructionsToTable(j, instructions)
}

var GCD_INSTRUCTIONS =
  [ new Instruction(0, "ab", "", 1, 2),
    new Instruction(1, "", "c", 0, 0),
    new Instruction(2, "a", "b", 2, 3),
    new Instruction(3, "c", "a", 3, 4),
    new Instruction(4, "b", "b", 0, 5)
  ]

/* ***** *
 * STATE *
 * ***** */

var GCD_INITIAL_STATE = new State("aaaaaabb", 0)

function State(sigma, j) {
  this.sigma = sigma
  this.j = j
}

function stateToTable(state) {
  return "<table class='state-table'>\n"
    + "  <tr><th>(&sigma;, j)</th></tr>\n"
    + "  <tr><td>(" + state.sigma + ", " + state.j + ")</td></tr>\n"
    + "</table>"
}

function renderState(state) {
  document.getElementById("state-table-container").innerHTML =
    stateToTable(state)
}

/* ******* *
 * BUTTONS *
 * ******* */

var STEP_BUTTON = "<button id=\"step-button\" onclick=\"handleStep()\">Step</button>"
var RESTART_BUTTON = "<button id=\"restart-button\" onclick=\"handleRestart()\">Restart</button>"

function renderButtons(currentInstructions, currentState) {
    document.getElementById("buttons").innerHTML =
        buttonsHtml(currentInstructions, currentState)
}

function buttonsHtml(currentInstructions, currentState) {
    if (findInstruction(currentState.j, currentInstructions)) {
        return STEP_BUTTON + "\n" + RESTART_BUTTON
    } else {
        return RESTART_BUTTON
    }
}

/* ********* *
 * EVALUATOR *
 * ********* */

var instructions = GCD_INSTRUCTIONS

var initialState = GCD_INITIAL_STATE
var currentState = GCD_INITIAL_STATE

function renderEvaluator() {
    renderInstructionTable(currentState.j, instructions)
    renderState(currentState)
    renderButtons(instructions, currentState)
}

function handleStep() {
    currentState = eval(instructions, currentState)
    renderEvaluator()
}

function handleRestart() {
    currentState = initialState
    renderEvaluator()
}

function eval(instructions, state) {
    var instruction = findInstruction(state.j, instructions)
    return instruction === null
        ? state
        : evalInstruction(instruction, state)
}

function findInstruction(j, instructions) {
    for (var i = 0; i < instructions.length; i++) {
        if (instructions[i].j === j) {
            return instructions[i]
        }
    }

    return null
}

function evalInstruction(instruction, state) {
    var split = splitTheta(instruction.theta, state.sigma)
    if (split) {
        return new State(split.before + instruction.phi + split.after, instruction.b)
    } else {
        return new State(state.sigma, instruction.a)
    }
}

function splitTheta(theta, input) {
    var index = input.indexOf(theta)
    if (index === -1) {
        return null
    } else {
        return {
            before: input.substring(0, index),
            after: input.substring(index + theta.length)
        }
    }
}

renderEvaluator(GCD_INSTRUCTIONS, GCD_INITIAL_STATE)
