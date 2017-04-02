import React, {Component} from 'react';
class Confirm extends Component {
  render() {
    const project = this.props.project;
    return (
      <form onSubmit="">
        <fieldset>
          {project.foo && (
             <div>Foo</div>
           )}
          {project.bar && (
             <p>Bar</p>
           )}
        </fieldset>
      </form>
    );
  }
}
export default Confirm;
