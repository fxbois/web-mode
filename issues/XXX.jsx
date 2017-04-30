import React, {Component} from 'react';
class Confirm extends Component {
  render() {
    const project = this.props.project;
    return (
      <Switch>
        <Route
          path={'/home'}
          render={ (props) => (
            <Home {...props} />
            <Home {...props} />
          )} />
      </Switch>
    )
  }
}
