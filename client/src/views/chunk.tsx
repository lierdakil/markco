import * as React from 'react'
import CodeMirrorReact = require('react-codemirror')
import * as api from '../api'

export interface ChunkProps {
  content: string
  num: number
  projectName: string
  onUpdated: () => void
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
      src: await api.getSource(this.props.projectName, this.props.num),
      editMode: true
    })
  }

  private async focusChange (focused: boolean) {
    if (!focused && this.state.editMode) {
      await api.update(
        this.props.projectName, this.props.num, this.state.src
      )
      this.setState({editMode: false})
      this.props.onUpdated()
    }
  }

  private handleChange (value: string) {
    this.setState({src: value})
  }
}
