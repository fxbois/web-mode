import React from 'react';

class App extends React.Component<AppProps, MyState, { MyOptions: MyOptions }> {

  render() {
    return <div className='page-wrapper'>
      <Switch>

        <Route exact path={'/' + sections.overview}
               render={(props) =>
                 <TestPlots {...props}
                            data={data}
                            arg1={arg1}
                            arg2={arg2}
                            arg3={arg3}
                            arg4={arg4}
                 />
               }>
        </Route>

      </Switch>
    </div>;
  }
}
