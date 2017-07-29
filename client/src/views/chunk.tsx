import * as React from 'react'
import CodeMirrorReact = require('react-codemirror')
import * as api from '../api'

export interface ChunkProps {
  content: string
  num?: number
  projectName: string
  onUpdated: () => void
}

export interface ChunkState {
  editMode: boolean
  dragHover: boolean
  src: string
}

export class Chunk extends React.Component<ChunkProps, ChunkState> {
  public refs: {
    'textarea': ReactCodeMirror.ReactCodeMirror
    [key: string]: React.ReactInstance
  }

  constructor (props: ChunkProps) {
    super(props)
    this.state = {
      editMode: false,
      dragHover: false,
      src: ''
    }
  }

  public render () {
    const classes = ['chunk-container']
    this.state.dragHover && classes.push('drag-hover')
    this.props.num === undefined && classes.push('last-chunk')
    return (
      this.state.editMode ?
      <div onPaste={this.imagePaste.bind(this)}>
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
        />
      </div> :
      <div className={classes.join(' ')}
           onDragEnter={this.dragEnter.bind(this)}
           onDragLeave={this.dragLeave.bind(this)}
           onDragOver={this.dragOver.bind(this)}
           onDrop={this.dropOnChunk.bind(this)}>
        <chunk dangerouslySetInnerHTML={{__html: this.props.content}} />
        <div className="control-btns">
          <button className="btn btn-edit" onClick={this.edit.bind(this)} />
          <button className="btn btn-delete" onClick={this.delete.bind(this)} />
        </div>
      </div>
    )
  }

  private async getSrc () {
    return this.props.num
      ? await api.getSource(this.props.projectName, this.props.num)
      : ''
  }

  private async edit () {
    this.setState({
      src: await this.getSrc(),
      editMode: true
    })
  }

  private async imagePaste (ev: React.ClipboardEvent<HTMLDivElement>) {
    if (ev.type === 'paste') {
      const data = ev.nativeEvent.clipboardData
      const reader = new FileReader()
      for (const file of Array.from(data.files)) {
        reader.readAsArrayBuffer(file)
        await new Promise((resolve) => {
          reader.onload = async () => {
            const filename = await api.upload(this.props.projectName, reader.result)
            const doc = this.refs.textarea.getCodeMirror().getDoc()
            const cursor = doc.getCursor()
            doc.replaceRange(`![](${filename})`, cursor)
          }
          reader.onloadend = () => resolve()
        })
      }
    }
  }

  private async delete () {
    if (confirm('You sure you want to delete?') === true) {
      this.update('')
    }
  }

  private async update (content = this.state.src) {
    this.props.num !== undefined
    ? await api.update(this.props.projectName, this.props.num, content)
    : await api.appendChunk(this.props.projectName, content)
    this.props.onUpdated()
  }

  private async focusChange (focused: boolean) {
    if (!focused && this.state.editMode) {
      this.update()
      this.setState({editMode: false})
    }
  }

  private handleChange (value: string) {
    this.setState({src: value})
  }

  private withDragData<T> (
    ev: React.DragEvent<HTMLDivElement>,
    func: (fileName: string) => T
  ): T | undefined {
    if (ev.dataTransfer.types.includes('application/x-filename')) {
      return func(ev.dataTransfer.getData('application/x-filename'))
    }
  }

  private dragOver (ev: React.DragEvent<HTMLDivElement>) {
    this.withDragData(ev, () => {
      ev.preventDefault()
    })
  }

  private dragEnter (ev: React.DragEvent<HTMLDivElement>) {
    this.withDragData(ev, () => {
      this.setState({ dragHover: true })
      ev.preventDefault()
    })
  }

  private dragLeave () {
    if (this.state.dragHover) {
      this.setState({ dragHover: false })
    }
  }

  private async dropOnChunk (ev: React.DragEvent<HTMLDivElement>) {
    this.withDragData(ev, async (fileName) => {
      await this.update(`${await this.getSrc()}\n\n![](${fileName})`)
      this.setState({ dragHover: false })
    })
  }
}
