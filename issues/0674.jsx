import React, {
  Component
} from 'react';

class CheckList extends Component {
  myTestFunc() {
    if (test) {
      if (test2) {
        console.log("Broken");
      }
    }
  }
  render() {
    let tasks = this.props.tasks.map(
      (task) => (
        <li className = "checklist__task">
          <input type="checkbox" defaultChecked = {task.done} />
          {task.name}
          <a className = "checklist__task--remove" href = "#" />
        </li>
      ));
    return (
      <div className = "checklist" >
        <ul > {tasks} </ul>
        <input className="checklist--add-task" type="text" placeholder="Type and hit enter to add task!" />
      </div>
    );
  }
}

export default CheckList;
