var s = "xs";
React.createClass({
  render() {
    return (
      <ul class="cscdd" data-toto={ return 1 + "cdxs" }> cqsd
        {
          if (true) {
            <BreadcrumbItem href="#/messages/{ this.props.scope.get('scopes') || this.props.scope.get('parent') }">
              hello
            </BreadcrumbItem>
          }
        }
      </ul>
    );
  }
});
