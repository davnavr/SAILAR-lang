'use strict';
const CodeMirror = require('codemirror');
const CodeMirrorSimpleMode = require('codemirror/addon/mode/simple.js');
//const codeMirrorLinter = require('codemirror/addon/lint/lint.js');
const rustWebAssembly = require('./pkg');

const assemblerModeName = "sailar";

CodeMirror.defineSimpleMode(assemblerModeName, {
    start: [
        { regex: /(?:\.format|identifier)\b/, token: 'keyword' },
        { regex: /[a-zA-Z$][a-zA-Z$]+/, token: 'atom' },
        { regex: /;.*/, token: 'comment' },
        { regex: /(0x[0-9a-fA-F][0-9a-fA-F_]*)|(0b[01][01_]*)|([0-9][0-9_]*)/, token: 'number' },
    ],
    meta: {
        lineComment: ';'
    }
});

document.addEventListener('DOMContentLoaded', async (event) => {
    const output = document.getElementById('output').appendChild(document.createElement("pre"));
    output.style = "width: 100%; height: 100%; margin: 0";

    const editor = CodeMirror((e) => { document.getElementById('input').appendChild(e); }, {
        gutters: ["CodeMirror-lint-markers"],
        lineNumbers: true,
        lint: true,
        mode: assemblerModeName,
        value: '.format major 0\n.format minor 18\n\n; Add additional directives below\n',
    });

    const refreshCodeEditor = setTimeout(() => editor.refresh(), 0);

    const initializeAssembler = rustWebAssembly.then((asm) => {
        function appendOutputMessage(message) {
            output.innerHTML += message;
        }

        function appendOutputError(error, locations) {
            output.innerHTML += 'error';
            if (locations !== null) {
                output.innerHTML += `(${locations[0]},${locations[1]})-(${locations[2]},${locations[3]})`;
                //editor.markText({ line: locations[0] - 1, ch: locations[1] - 1, }, { line: locations[2] - 1, ch: locations[3] - 1 }, errorMarkOptions);
            }
            output.innerHTML += ': ' + error + '\n';
        }

        function update() {
            output.innerHTML = '';

            // TODO: Clear old marks with getAllMarks()?

            asm.assemble(editor.getValue(), appendOutputMessage, appendOutputError);
        }

        asm.register_panic_hook();
        editor.on('change', update);
        update();
    }).catch(console.error);

    await initializeAssembler;
    await refreshCodeEditor;
})
