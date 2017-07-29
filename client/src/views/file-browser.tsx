import * as React from 'react'
import { RouteComponentProps } from 'react-router-dom'
import * as api from '../api'
import Dropzone = require('react-dropzone')

export interface DocProps {
  name: string
}

export interface DocState {
  files: api.FileInfo[]
}

export class FileBrowser extends React.Component<RouteComponentProps<DocProps>, DocState> {
  constructor (props: RouteComponentProps<DocProps>) {
    super(props)
    this.state = { files: [] }
    this.update()
  }

  public render () {
    return (
      <div className="file-list">
        <div>
          {this.state.files.map(({fileName, fileURI}) =>
            <div className="file-list-item"
                 style={{'background-image': `url(${fileURI})`}}
                 draggable={true}
                 onDragStart={(ev) => {
                   ev.dataTransfer.setData(
                     'application/x-filename', fileName
                   )
                 }}
                 >
              <div className="control-btns">
                <button className="btn btn-delete" onClick={async () => this.delete(fileName)} />
              </div>
            </div>
          )}
          <Dropzone className="file-list-item file-list-dropzone" onDrop={this.drop.bind(this)}/>
        </div>
      </div>
    )
  }

  public componentWillUpdate (nextProps: RouteComponentProps<DocProps>) {
    if (nextProps.match.params.name !== this.props.match.params.name) {
      this.update(nextProps.match.params.name)
    }
  }

  public componentDidUpdate () {
    MathJax.Hub && MathJax.Hub.Queue(['Typeset', MathJax.Hub])
  }

  public async update (name = this.props.match.params.name) {
    this.setState({files: await api.fileList(name)})
  }

  private async drop (files: Dropzone.ImageFile[]) {
    if (files.length === 0) { return }
    const reader = new FileReader()
    for (const file of files) {
      reader.readAsArrayBuffer(file)
      await new Promise((resolve, reject) => {
        reader.onload = async () => resolve(api.upload(this.props.match.params.name, reader.result))
        reader.onabort = (ev) => reject(ev)
        reader.onerror = (ev) => reject(ev)
      })
    }
    this.update()
  }

  private async delete (fileName: string) {
    if (confirm('You sure you want to delete?') === true) {
      await api.deleteFile(this.props.match.params.name, fileName)
      this.update()
    }
  }
}
