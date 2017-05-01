import React, {Component} from 'react';
class Confirm extends Component {
  render() {
    const project = this.props.project;
    return (
      <Route exac path='/' render={(matchProps) => (
        <DefaultLayout>
          <Home {...matchProps} />
        </DefaultLayout>
      )} />
    )
  }
}
