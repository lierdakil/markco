import * as React from 'react'
import * as api from '../api'
import { NavLink } from 'react-router-dom'

export interface PLState {
  projects: string[]
}

export class ProjectList extends React.Component<{}, PLState> {
  constructor () {
    super()
    this.state = { projects: [] }
    this.update()
  }

  public render () {
    return (
      <div className="project-list">
        <ul>
          { this.state.projects.map((prjName) =>
            <li>
              <NavLink to={`/docs/${prjName}`}>
                {prjName}
              </NavLink>
              <div className="control-btns">
                <button className="btn btn-delete" onClick={async () => this.delete(prjName)} />
              </div>
            </li>) }
          <li className="last-project">
            <a onClick={this.create.bind(this)}></a>
          </li>
        </ul>
      </div>
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
    if (confirm('You sure you want to delete?') === true) {
      await api.deleteProject(name)
      this.update()
    }
  }
}
