// tslint:disable: no-null-keyword
import * as React from 'react'
import {Button, Glyphicon, ButtonGroup, OverlayTrigger, Modal} from 'react-bootstrap'

export interface Props {
  onEdit?: () => void
  onDelete?: () => void
  placement?: 'left' | 'right' | 'top' | 'bottom'
}

export interface State {
  showDialog: boolean
}

export class ControlBtns extends React.Component<Props, State> {
  constructor (props: Props) {
    super(props)
    this.state = {
      showDialog: false
    }
  }

  public render () {
    return (
      <div>
        <OverlayTrigger
            trigger={['focus']}
            placement={this.props.placement}
            overlay={this.btnOverlay()}>
          {this.props.children}
        </OverlayTrigger>
        {this.modalConfirmDialog()}
      </div>
    )
  }

  private btnOverlay () {
    return (
      <ButtonGroup style={{position: 'absolute'}}>
        {this.props.onEdit ?
          <Button onClick={this.props.onEdit}>
            <Glyphicon glyph="pencil"/>
          </Button>
        : null}
        {this.props.onDelete ?
          <Button onClick={this.open.bind(this)}>
            <Glyphicon glyph="trash"/>
          </Button>
        : null}
      </ButtonGroup>
    )
  }

  private modalConfirmDialog () {
    return (
      <Modal show={this.state.showDialog} onHide={this.close.bind(this)}>
        <Modal.Header closeButton>
          <Modal.Title>Confirmation</Modal.Title>
        </Modal.Header>
        <Modal.Body>
          Are you sure you want to delete?
        </Modal.Body>
        <Modal.Footer>
          <Button onClick={this.confirm.bind(this)}>Yes</Button>
          <Button onClick={this.close.bind(this)}>No</Button>
        </Modal.Footer>
      </Modal>
    )
  }

  private confirm () {
    this.close()
    this.props.onDelete && this.props.onDelete()
  }

  private open () {
    this.setState({ showDialog: true })
  }

  private close () {
    this.setState({ showDialog: false })
  }
}
