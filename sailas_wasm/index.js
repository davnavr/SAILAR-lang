'use strict';

document.addEventListener('DOMContentLoaded', async(event) => {
    const CodeMirror = require('codemirror');
    const editor = CodeMirror.fromTextArea(document.getElementById('input'), {
        lineNumbers: true,
    });
})
