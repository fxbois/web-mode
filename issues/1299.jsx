export function Component(): JSX.Element {
  return (
    <p>
      You can disable this mechanism with
      <a href="http://tinyurl.com/CSP-disable">Chrome Extension</a> ,
      but you do this on your own risk.
    </p>
  );
}

function hello() {
  return (
    <input
      type="file"
      ref={component => {
        this.inputFile = component;
      }}
    />
  );
}

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
