'use strict';
const CodeMirror = require('codemirror');
const rustWebAssembly = require('./pkg');

require('codemirror/addon/mode/simple.js');
require('codemirror/addon/lint/lint.js');

const assemblerModeName = "sailar";

CodeMirror.defineSimpleMode(assemblerModeName, {
    start: [
        { regex: /(?:\.format|identifier)\b/, token: 'keyword' },
        { regex: /[a-zA-Z$][a-zA-Z$]+/, token: 'atom' },
        { regex: /;.*/, token: 'comment' },
        { regex: /(0x[0-9a-fA-F][0-9a-fA-F_]*)|(0b[01][01_]*)|([0-9][0-9_]*)/, token: 'number' },
    ],
    meta: {
        lineComment: ';',
    }
});

let globalAssemblyLinter;

// TODO: Could accept extra parameter to assembler and other state here (see javascript-lint).
CodeMirror.registerHelper("lint", assemblerModeName, function(text) {
    // TODO: Figure out if call to asm.assemble can go here.
    if (globalAssemblyLinter !== undefined) {
        return globalAssemblyLinter(text);
    } else {
        return [];
    }
});

document.addEventListener('DOMContentLoaded', async (_) => {
    const output = document.getElementById('output').appendChild(document.createElement("pre"));
    output.style = "width: 100%; height: 100%; margin: 0";

    const editor = CodeMirror((e) => { document.getElementById('input').appendChild(e); }, {
        gutters: ["CodeMirror-lint-markers"],
        lineNumbers: true,
        lint:  true,
        mode: assemblerModeName,
        value: '.format major 0\n.format minor 18\n\n; Add additional directives below\n\n',
    });

    const refreshCodeEditor = setTimeout(() => editor.refresh(), 0);

    const initializeAssembler = rustWebAssembly.then((asm) => {
        function update() {
            output.innerHTML = '';

            let errors = [];

            function appendOutputMessage(message) {
                if (errors.length === 0) {
                    output.innerHTML += message;
                }
            }

            function appendOutputError(error, locations) {
                output.innerHTML += 'error';

                if (locations !== null) {
                    output.innerHTML += `(${locations[0]},${locations[1]})-(${locations[2]},${locations[3]})`;

                    // Assembler location numbers start at 1, while CodeMirror expects numbers starting at 0.
                    errors.push({
                        from: CodeMirror.Pos(locations[0] - 1, locations[1] - 1),
                        to: CodeMirror.Pos(locations[2] - 1, locations[3] - 1),
                        message: error,
                        severity: "error",
                    });
                }

                output.innerHTML += ': ' + error + '\n';
            }

            asm.assemble(editor.getValue(), appendOutputMessage, appendOutputError);
            
            return errors;
        }

        asm.register_panic_hook();
        globalAssemblyLinter = update;
        update();
    }).catch(console.error);

    await initializeAssembler;
    await refreshCodeEditor;
})
