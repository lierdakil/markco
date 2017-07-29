import * as React from 'react'
import { RouteComponentProps } from 'react-router-dom'
import * as api from '../api'

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
        {this.state.files.map(({fileName, fileURI}) =>
          <div className="file-list-item" style={{background: `url(${fileURI})`}} data-file={fileName} />
        )}
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
}
