var React = require('react')

return React.render(
  <div className="fooBar">
    {
      conditional &&
      <myComponent className="myComponent" />
    }
    <label>{labelText}</label>
    <div>
      <span>Hello</span>
    </div>
  </div>
);

React.createClass({
  render() {
    return (
      <ul class="cscdd" data-toto={ return 1 + "cdxs" }> cqsd
        { (this.props.scope.get('type') !== "home") ?
          <BreadcrumbItem href="#/messages/{this.props.scope.get('scope') || this.props.scope.get('parent')}">
            {this.props.scope.get('scope') || this.props.scope.get('parent')}
          </BreadcrumbItem> : null}
      </ul>
    );
  }
});
