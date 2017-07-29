// tslint:disable: no-unbound-method
import * as React from 'react'
import {Chunk} from './chunk'
import * as api from '../api'

export interface DocProps {
  name: string
}

export interface DocState {
  chunks: string[]
}

export class Doc extends React.Component<DocProps, DocState> {
  constructor (public props: DocProps) {
    super(props)
  }

  public render () {
    return (
      <project>
        {this.state.chunks.map((chunk, idx) => <Chunk project={this} content={chunk} num={idx}/>)}
      </project>
    )
  }

  public componentDidUpdate () {
    MathJax.Hub && MathJax.Hub.Queue(['Typeset', MathJax.Hub])
  }

  public async update () {
    this.setState({chunks: await api.render(this.props.name)})
  }
}
