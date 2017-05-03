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

import React, {Component} from 'react';
class Confirm extends Component {
  render() {
    const project = this.props.project;
    return (
      <Route
        exact
        path="/"
        render={ matchProps => (
          <Home {...matchProps} />)
        }
      />
    )
  }
};

class Test extends Component {
  render() {
    return (
      <Router>
        <Switch>
          <Route
            exact
            path="/"
            render={matchProps => (
              <DefaultLayout>
                <Home {...matchProps} />
              </DefaultLayout>
            )} />
          <Route
            path="/settings"
            render={matchProps => (
              <DefaultLayout>
                <Settings {...matchProps} />
              </DefaultLayout>
            )} />
        </Switch>
      </Router>
    )
  }
}
