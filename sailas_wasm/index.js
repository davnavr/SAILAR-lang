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
        asm.register_panic_hook();

        let updating = false;

        const errorMarkOptions = {
            className: 'cm-error',
        };

        function appendOutputMessage(message) {
            output.innerHTML += message;
        }

        function appendOutputError(error, locations) {
            output.innerHTML += 'error';
            if (locations !== null) {
                output.innerHTML += `(${locations[0]},${locations[1]})-(${locations[2]},${locations[3]})`;
                editor.markText({ line: locations[0] - 1, ch: locations[1] - 1, }, { line: locations[2] - 1, ch: locations[3] - 1 }, errorMarkOptions);
            }
            output.innerHTML += ': ' + error + '\n';
        }

        function update() {
            if (!updating) {
                updating = true;
                output.innerHTML = '';

                // TODO: Clear old marks with getAllMarks()?

                asm.assemble(editor.getValue(), appendOutputMessage, appendOutputError);
                updating = false;
            }
        }

        editor.on('update', update);

        update();
    }).catch(console.error);

    await initializeAssembler;
    await refreshCodeEditor;
})
