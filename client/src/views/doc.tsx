import * as React from 'react'
import {Chunk} from './chunk'
import * as api from '../api'
import {Button, Glyphicon} from 'react-bootstrap'

export interface Props {
  name: string
  onPaste: () => void
}

export interface ChunkPartData {
  chunkHtml: string
  chunkSrc: string
  chunkNum?: number
  startEditMode?: boolean
}

export interface State {
  chunks: ChunkPartData[]
}

export class Doc extends React.Component<Props, State> {
  constructor (props: Props) {
    super(props)
    this.state = { chunks: [] }
    this.update()
  }

  public render () {
    return (
      <doc>
        {this.state.chunks.map((chunk) =>
          <Chunk
            projectName={this.props.name}
            content={chunk.chunkHtml}
            src={chunk.chunkSrc}
            num={chunk.chunkNum}
            startEditMode={chunk.startEditMode}
            onUpdated={this.update.bind(this)}
            onPaste={this.props.onPaste}
            />)}
        <Button bsStyle="primary" className="btn-block" onClick={this.addChunk.bind(this)}>
          <Glyphicon glyph="plus" />
        </Button>
      </doc>
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
    this.setState({chunks: await api.render(name)})
  }

  private addChunk () {
    this.setState({
      chunks: this.state.chunks.concat([{chunkHtml: 'New', chunkSrc: '', startEditMode: true}])
    })
  }
}
