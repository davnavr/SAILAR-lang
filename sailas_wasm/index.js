'use strict';
const CodeMirror = require('codemirror');
const rustWebAssembly = require('./pkg');

document.addEventListener('DOMContentLoaded', async (event) => {
    const output = document.getElementById('output').appendChild(document.createElement("pre"));
    output.style = "width: 100%; height: 100%; margin: 0";

    const editor = CodeMirror((e) => { document.getElementById('input').appendChild(e); }, {
        value: '.format major 0\n.format minor 18\n\n; Add additional directives below\n',
        lineNumbers: true,
    });

    const refreshCodeEditor = setTimeout(() => editor.refresh(), 0);

    const initializeAssembler = rustWebAssembly.then((asm) => {
        let outputContentCache = '';

        function appendOutputMessage(message) {
            outputContentCache += message;
        }

        function appendOutputError(error, locations) {
            outputContentCache += 'error';
            if (locations !== null) {
                outputContentCache += `(${locations[0]},${locations[1]})-(${locations[2]},${locations[3]})`;
            }
            outputContentCache += ': ' + error + '\n';
        }

        function update(editor) {
            outputContentCache = '';

            asm.assemble(editor.getValue(), appendOutputMessage, appendOutputError);

            output.innerHTML = outputContentCache;
        }

        editor.on('update', update);

        update(editor);
    }).catch(console.error);

    await initializeAssembler;
    await refreshCodeEditor;
})
