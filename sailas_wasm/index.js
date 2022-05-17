'use strict';

document.addEventListener('DOMContentLoaded', async(event) => {
    const CodeMirror = require('codemirror');
    const editor = CodeMirror((e) => { document.getElementById('input').appendChild(e); }, {
        lineNumbers: true,
    });
})
