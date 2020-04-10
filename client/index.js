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

function EvalState(currentState, instructions) {
  this.currentState = currentState
  this.instructions = instructions
}

/* ************ *
 * INSTRUCTIONS *
 * ************ */

function instructionsToTable(j, instructions) {
  return "<table class='instruction-table'>"
    + INSTRUCTION_TABLE_HEADER
    + instructions.map(function (i) { return instructionToRow(j, i) }).join("\n")
    + "</table>"
}

function instructionToRow(j, instruction) {
  console.log("instruction: ", instruction)
  return (instruction.j === j ? "<tr class='current-instruction'>\n" : "<tr>\n")
    + "  <td class='instruction-table-cell'>\n" + instruction.j + "</td>\n"
    + "  <td class='instruction-table-cell'>\n" + instruction.theta + "</td>\n"
    + "  <td class='instruction-table-cell'>\n" + instruction.phi + "</td>\n"
    + "  <td class='instruction-table-cell'>\n" + instruction.b + "</td>\n"
    + "  <td class='instruction-table-cell'>\n" + instruction.a + "</td>\n"
    + "</tr>"
}

function renderInstructionTable(j, instructions) {
  document.getElementById("instructions-table-container").innerHTML =
    instructionsToTable(j, instructions)
}

var GCD_INSTRUCTIONS =
  [ new Instruction(0, "ab", "(empty)", 1, 2),
    new Instruction(1, "(empty)", "c", 0, 0),
    new Instruction(2, "a", "b", 2, 3),
    new Instruction(3, "c", "a", 3, 4),
    new Instruction(4, "b", "b", 0, 5)
  ]

renderInstructionTable(0, GCD_INSTRUCTIONS)