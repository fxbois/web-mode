import React, {Component} from 'react';
class Confirm extends Component {
  render() {
    const project = this.props.project;
    return (
      <Foo
        someLongValue='x'
        text={
          <Text>
            text
          </Text>
        }
      />
      <div>
        <OtherComponent/>
        <OtherComponent class="toto"
                        {...props}
                        id="fr" />
      </div>
    )
  }
}
