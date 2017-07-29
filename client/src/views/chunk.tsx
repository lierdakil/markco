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
    'textarea': ReactCodeMirror.ReactCodeMirror
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
      <div className="chunk-container">
        <chunk dangerouslySetInnerHTML={{__html: this.props.content}} />
        <div className="control-btns">
          <button className="btn btn-edit" onClick={this.edit.bind(this)} />
          <button className="btn btn-delete" onClick={this.delete.bind(this)} />
        </div>
      </div>
    )
  }

  private async edit () {
    this.setState({
      src: await api.getSource(this.props.projectName, this.props.num),
      editMode: true
    })
  }

  private async imagePaste (ev: React.ClipboardEvent<HTMLDivElement>) {
    if (ev.type === 'paste') {
      const data = ev.nativeEvent.clipboardData
      const reader = new FileReader()
      reader.addEventListener('load', async () => {
        const filename = await api.upload(this.props.projectName, reader.result)
        const doc = this.refs.textarea.getCodeMirror().getDoc()
        const cursor = doc.getCursor()
        doc.replaceRange(`![](${filename})`, cursor)
      })
      for (const file of Array.from(data.files)) {
        reader.readAsArrayBuffer(file)
      }
    }
  }

  private async delete () {
    if (confirm('You sure you want to delete?') === true) {
      this.update('')
    }
  }

  private async update (content = this.state.src) {
    await api.update(
      this.props.projectName, this.props.num, content
    )
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
}
