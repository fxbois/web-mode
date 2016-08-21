import React from 'react';
import ReactDOM from 'react-dom';

/* var React = require('react');*/

class App extends React.Component {
  render() {
    return (
      <div>
        Hello World!!!<br />
		{1 + 1}
		{/* Here is some kind of comment in React */}
      </div>
    );
  }
}

export default App;

/* export default (props) => {
 *   return <h1>Hello, World!</h1>;
 * }*/


ReactDOM.render(<App />, document.getElementById('example'));
