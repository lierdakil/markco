// tslint:disable: no-unbound-method
import {Doc} from './doc'
import etch = require('etch')
import * as api from '../api'

export interface ChunkProps extends JSX.Props {
  content: string
  num: number
  project: Doc
}

export class Chunk implements JSX.ElementClass {
  private editMode: boolean = false
  private src: string = ''
  private refs: {[k: string]: any}
  constructor (public props: ChunkProps) {
    etch.initialize(this)
  }
  public async update (props: ChunkProps) {
    this.props = props
    return etch.update(this)
  }
  public render () {
    return (
      this.editMode ?
      <textarea ref="textarea" value={this.src} on={{blur: this.blur, keyup: this.keyup}}
      /> :
      <chunk innerHTML={this.props.content}
             on={{dblclick: this.edit}}
      />
    )
  }

  private async edit () {
    this.src = await api.getSource(this.props.project.props.name, this.props.num)
    this.editMode = true
    await this.update(this.props)
    this.refs.textarea.focus()
    this.keyup()
  }

  private async blur () {
    await api.update(
      this.props.project.props.name, this.props.num,
      this.refs.textarea.value)
    this.editMode = false
    this.props.project.update(this.props.project.props)
  }

  private async keyup () {
    const str: string = this.refs.textarea.value
    const cols: number = this.refs.textarea.cols = 80

    const linecount = str.split('\n').reduce((p, l) => p + 1 + Math.floor( l.length / cols ), 0)
    this.refs.textarea.rows = linecount + 1
  }
}
