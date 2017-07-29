import * as React from 'react'
import { RouteComponentProps } from 'react-router-dom'
import {Chunk} from './chunk'
import * as api from '../api'

export interface DocProps {
  name: string
}

export interface DocState {
  chunks: string[]
}

export class Doc extends React.Component<RouteComponentProps<DocProps>, DocState> {
  constructor (props: RouteComponentProps<DocProps>) {
    super(props)
    this.state = { chunks: [] }
    this.update()
  }

  public render () {
    return (
      <project>
        {this.state.chunks.map((chunk, idx) =>
          <Chunk
            projectName={this.props.match.params.name}
            content={chunk}
            num={idx}
            onUpdated={this.update.bind(this)}
            />)}
      </project>
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
    this.setState({chunks: await api.render(name)})
  }
}
