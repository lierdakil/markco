import * as React from 'react'
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
      <textarea ref="textarea" value={this.state.src}
        onBlur={this.blur.bind(this)}
        onChange={this.handleChange.bind(this)}
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
    this.refs.textarea.focus()
    this.resizeTextArea()
  }

  private async blur () {
    await api.update(
      this.props.project.props.name, this.props.num,
      this.refs.textarea.value
    )
    this.setState({editMode: false})
    this.props.project.update()
  }

  private handleChange (event: React.ChangeEvent<HTMLTextAreaElement>) {
    this.resizeTextArea()
    this.setState({src: event.target.value})
  }

  private resizeTextArea () {
    const str: string = this.refs.textarea.value
    const cols: number = this.refs.textarea.cols = 80
    const linecount = str.split('\n').reduce((p, l) => p + 1 + Math.floor( l.length / cols ), 0)
    this.refs.textarea.rows = linecount + 1
  }
}
