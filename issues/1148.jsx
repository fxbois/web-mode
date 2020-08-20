import React from 'react';
import { observable } from 'mobx';
import _ from 'lodash';

const state = observable({
  tasks: [],
  existsActiveTask: function() {
    const task = _(this.tasks).last();
    if (!task) return false;

    return !!task.finishedAt;
  },
  startTask: function(description) {
    if (_(tasks).last)
      tasks.push({description});
  }
})

function App() {
  return (
    <div>
      <header>
        <p className="text-center">
          Edit <code>src/App.js</code> and save to reload.
        </p>
        <a
          href="https://reactjs.org"
          target="_blank"
          rel="noopener noreferrer"
        >
          Learn React
        </a>
      </header>
    </div>
  );
}

export default App;
