'use strict';
const CodeMirror = require('codemirror');
const rustWebAssembly = require('./pkg');

require('codemirror/addon/mode/simple.js');
require('codemirror/addon/lint/lint.js');

const assemblerModeName = "sailar";
const hexDigits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'];

CodeMirror.defineSimpleMode(assemblerModeName, {
    start: [
        { regex: /\.[a-zA-Z]+/, token: 'keyword' },
        { regex: /[a-zA-Z$][a-zA-Z$]+/, token: 'atom' },
        { regex: /;.*/, token: 'comment' },
        { regex: /(0x[0-9a-fA-F][0-9a-fA-F_]*)|(0b[01][01_]*)|([0-9][0-9_]*)/, token: 'number' },
        { regex: /"([a-zA-Z !:\?0-9\-_\+\*\/]|\\[trn\\"'])*"/, token: 'string' },
        { regex: /@[a-zA-Z_0-9]+/, token: 'variable-2' }
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
    /**
     * @type {HTMLSelectElement}
     */
    const outputTypeSelection = document.getElementById('output-type');

    /**
     * @type {HTMLButtonElement}
     */
    const outputDownloadButton = document.getElementById('output-download');

    const output = document.getElementById('output-area').appendChild(document.createElement('pre'));
    output.style = 'width: 100%; height: 100%; margin: 0';

    const editor = CodeMirror((e) => { document.getElementById('input').appendChild(e); }, {
        gutters: ['CodeMirror-lint-markers'],
        lineNumbers: true,
        lint:  true,
        mode: assemblerModeName,
        value: '.format major 0\n.format minor 18\n.metadata id "MyModule" 1.0.0\n; Add additional directives below\n\n',
    });

    const refreshCodeEditor = setTimeout(() => editor.refresh(), 0);

    const initializeAssembler = rustWebAssembly.then((asm) => {
        function update() {
            let errors = [];

            function appendOutputError(error, locations) {
                if (errors.length === 0) {
                    output.innerHTML = '';
                }

                outputDownloadButton.onclick = null;
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

            /**
             * @param {Uint8Array} module 
             */
            function writeAssemblyOutput(module) {
                const blob = new Blob([ module.buffer ], { type: 'application/octet-stream' });
                
                outputDownloadButton.onclick = () => {
                    const url = URL.createObjectURL(blob);
                    
                    /**
                     * @type {HTMLAnchorElement}
                     */
                    const blobAnchorElement = document.body.appendChild(document.createElement('a'));
                    blobAnchorElement.href = url;
                    blobAnchorElement.download = 'module.sail';

                    blobAnchorElement.click();
                    document.body.removeChild(blobAnchorElement);
                    URL.revokeObjectURL(url);
                };

                output.innerHTML = '';

                switch (outputTypeSelection.selectedIndex) {
                    case 0:
                        const table = output.appendChild(document.createElement('table'));
                        const header = table.appendChild(document.createElement('tr'));

                        function appendTableHeader(title) {
                            header.appendChild(document.createElement('th')).innerText = title;
                        }

                        table.id = 'byte-table';
                        appendTableHeader('Address');
                        hexDigits.forEach((v) => appendTableHeader('0' + v));
                        appendTableHeader('ASCII');

                        let row;
                        let rowAsciiCharacters = [];

                        for (let index = 0; index < module.length; index++) {
                            if (index % 16 === 0) {
                                row = table.appendChild(document.createElement('tr'));
                                row.appendChild(document.createElement('td')).innerText = index
                                    .toString(16)
                                    .toUpperCase()
                                    .padStart(8, '0');

                                rowAsciiCharacters = [];
                            }

                            row.appendChild(document.createElement('td')).innerText = module[index]
                                .toString(16)
                                .toUpperCase()
                                .padStart(2, '0');
                        }

                        break;
                    default:
                        output.innerHTML = 'unknown output type';
                }
            }

            asm.assemble(editor.getValue(), appendOutputError, writeAssemblyOutput);
            
            return errors;
        }

        asm.register_panic_hook();
        globalAssemblyLinter = update;
        update();
    }).catch(console.error);

    await initializeAssembler;
    await refreshCodeEditor;
})
