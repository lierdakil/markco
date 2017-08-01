import * as React from 'react'
import * as api from '../api'
import {Nav, NavItem, Glyphicon, Button} from 'react-bootstrap'
import {LinkContainer} from 'react-router-bootstrap'
import {ControlBtns} from './control-btns'

export interface State {
  projects: string[]
}

export class ProjectList extends React.Component<{}, State> {
  constructor () {
    super()
    this.state = { projects: [] }
    this.update()
  }

  public render () {
    return (
      <Nav bsStyle="pills" stacked activeKey={1}>
        { this.state.projects.map(
          (prjName, idx) =>
            <LinkContainer to={`/docs/${prjName}`}>
                <NavItem tabIndex={2000000 + idx} eventKey={idx}>
                    <ControlBtns onDelete={() => { this.delete(prjName) }}/>
                    {prjName}
                </NavItem>
            </LinkContainer>
        ) }
        <Button bsStyle="primary" className="btn-block" onClick={this.create.bind(this)}>
          <Glyphicon glyph="plus"/>
        </Button>
      </Nav>
    )
  }

  public async update () {
    this.setState({ projects: await api.listProjects() })
  }

  private async create () {
    const newName = prompt('New project name:')
    if (newName) {
      await api.createProject(newName, `# ${newName}\n\nNew project`)
      this.update()
    }
  }

  private async delete (name: string) {
    await api.deleteProject(name)
    this.update()
  }
}
