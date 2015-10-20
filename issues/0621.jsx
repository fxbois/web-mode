// Same results if Label and Input are not imported
import { React, Component } from 'react'
import { Label, Input } from 'react-bootstrap'

export default class Test extends Component {
  render() {
    return (
      <Label
          name="foo"
      >
        Foo
        <Input
            type="select"
        >
        </Input>
      </Label>
    )
  }
}
