'use strict';
const CodeMirror = require('codemirror');
const assembler = require('./pkg');

document.addEventListener('DOMContentLoaded', async(event) => {
    const editor = CodeMirror((e) => { document.getElementById('input').appendChild(e); }, {
        value: '.format major 0\n.format minor 18\n\n; Add additional directives below\n',
        lineNumbers: true,
    });

    setTimeout(() => editor.refresh(), 0);

    const output = document.getElementById('output').appendChild(document.createElement("p"));
    output.style = "width: 100%; height: 100%; margin: 0";
})
