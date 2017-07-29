// tslint:disable
const CodeMirror = require('codemirror')

require('codemirror/mode/markdown/markdown')
require('codemirror/mode/stex/stex')
require('./multiplex')

export default function() {
  CodeMirror.defineMode('texmd', (config) =>
    CodeMirror.multiplexingMode(
      CodeMirror.getMode(config, 'markdown'),
      {open: '$$', close: '$$',
       mode: CodeMirror.getMode(config, 'stex'),
       parseDelimiters: true},
      {open: '$', close: '$',
       mode: CodeMirror.getMode(config, 'stex'),
       parseDelimiters: true}
    )
  )
}
