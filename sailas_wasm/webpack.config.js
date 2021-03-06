const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const HtmlWebpackTagsPlugin = require('html-webpack-tags-plugin');
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

module.exports = {
    entry: './index.js',
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'index.js',
    },
    plugins: [
        new HtmlWebpackPlugin({
            title: 'Interactive Assembler',
            template: 'index.html'
        }),
        new HtmlWebpackTagsPlugin({
            tags: [
                'index.css',
                './node_modules/codemirror/lib/codemirror.css',
                './node_modules/codemirror/addon/lint/lint.css'
            ],
            append: true
        }),
        new WasmPackPlugin({
            crateDirectory: path.resolve(__dirname, ".")
        }),
    ],
    mode: 'development',
    experiments: {
        asyncWebAssembly: true,
    },
};
