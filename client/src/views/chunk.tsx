import * as React from 'react'
import CodeMirrorReact = require('react-codemirror')
import {Doc} from './doc'
import * as api from '../api'

export interface ChunkProps {
  content: string
  num: number
  project: Doc
}

export interface ChunkState {
  editMode: boolean
  src: string
}

export class Chunk extends React.Component<ChunkProps, ChunkState> {
  public refs: {
    'textarea': HTMLTextAreaElement
    [key: string]: React.ReactInstance
  }

  constructor (props: ChunkProps) {
    super(props)
    this.state = {
      editMode: false,
      src: ''
    }
  }

  public render () {
    return (
      this.state.editMode ?
      <CodeMirrorReact ref="textarea" value={this.state.src}
        onFocusChange={this.focusChange.bind(this)}
        onChange={this.handleChange.bind(this)}
        options={{
          theme: 'elegant',
          mode: 'texmd',
          lineWrapping: true,
          autofocus: true,
          viewportMargin: Infinity,
        }}
      /> :
      <chunk dangerouslySetInnerHTML={{__html: this.props.content}}
             onDoubleClick={this.edit.bind(this)}
      />
    )
  }

  private async edit () {
    this.setState({
      src: await api.getSource(this.props.project.props.name, this.props.num),
      editMode: true
    })
    this.resizeTextArea()
  }

  private async focusChange (focused: boolean) {
    if (!focused) {
      await api.update(
        this.props.project.props.name, this.props.num,
        this.state.src
      )
      this.setState({editMode: false})
      this.props.project.update()
    }
  }

  private handleChange (value: string) {
    this.resizeTextArea()
    this.setState({src: value})
  }

  private resizeTextArea () {
    // const str: string = this.refs.textarea.value
    // const cols: number = this.refs.textarea.cols = 80
    // const linecount = str.split('\n').reduce((p, l) => p + 1 + Math.floor( l.length / cols ), 0)
    // this.refs.textarea.rows = linecount + 1
  }
}
