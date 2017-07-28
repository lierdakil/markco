// tslint:disable: no-unbound-method
import etch = require('etch')
import {Chunk} from './chunk'
import * as api from '../api'

export interface DocProps extends JSX.Props {
  name: string
}

export class Doc implements JSX.ElementClass {
  public element: HTMLElement
  private chunks: string[] = []
  constructor (public props: DocProps, children?: JSX.Element[]) {
    etch.initialize(this)
    this.update(props)
  }

  public render () {
    return (
      <project>
        {this.chunks.map((chunk, idx) => <Chunk project={this} content={chunk} num={idx}/>)}
      </project>
    )
  }

  public async update (props: DocProps, children?: JSX.Element[]) {
    this.chunks = await api.render(this.props.name)
    await etch.update(this)
    if (MathJax) { MathJax.Hub.Queue(['Typeset', MathJax.Hub]) }
  }
}
