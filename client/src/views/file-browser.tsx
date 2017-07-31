import * as React from 'react'
import * as api from '../api'
import {Row, Col, Image, Button, Glyphicon} from 'react-bootstrap'
import Dropzone = require('react-dropzone')
import {ControlBtns} from './control-btns'

export interface Props {
  name: string
}

export interface State {
  files: api.FileInfo[]
}

export class FileBrowser extends React.Component<Props, State> {
  constructor (props: Props) {
    super(props)
    this.state = { files: [] }
    this.update()
  }

  public render () {
    return (
      <Row>
        {this.state.files.map(({fileName, fileURI}, idx) =>
          <Col xs={12} md={6}>
            <ControlBtns onDelete={() => {this.delete(fileName)}}>
              <Image tabIndex={idx + 1000000}
                src={fileURI} thumbnail draggable={true}
                onDragStart={(ev) => {
                  ev.dataTransfer.setData(
                    'application/x-filename', fileName
                  )
                }}
                />
            </ControlBtns>
          </Col>
        )}
        <Dropzone className="file-list-item file-list-dropzone" onDrop={this.drop.bind(this)}>
          <Col xs={12}>
            <Button className="btn-block" bsStyle="primary">
              <Glyphicon glyph="plus" />
            </Button>
          </Col>
        </Dropzone>
      </Row>
    )
  }

  public componentWillUpdate (nextProps: Props) {
    if (nextProps.name !== this.props.name) {
      this.update(nextProps.name)
    }
  }

  public componentDidUpdate () {
    MathJax.Hub && MathJax.Hub.Queue(['Typeset', MathJax.Hub])
  }

  public async update (name = this.props.name) {
    this.setState({files: await api.fileList(name)})
  }

  private async drop (files: Dropzone.ImageFile[]) {
    if (files.length === 0) { return }
    const reader = new FileReader()
    for (const file of files) {
      reader.readAsArrayBuffer(file)
      await new Promise((resolve, reject) => {
        reader.onload = async () => resolve(api.upload(this.props.name, reader.result))
        reader.onabort = (ev) => reject(ev)
        reader.onerror = (ev) => reject(ev)
      })
    }
    this.update()
  }

  private async delete (fileName: string) {
    await api.deleteFile(this.props.name, fileName)
    this.update()
  }
}
