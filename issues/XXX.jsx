class Confirm extends Component {
  render() {
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
