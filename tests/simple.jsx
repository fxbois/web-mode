
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


React.createClass({
  render() {
    return (
      <ul class="cscdd" data-toto={ return 1 + "cdxs" }> cqsd
        { this.props.list.map(function(element) {
            var x = 1;
            return <li class="dxs" x="xs">{ "aa" + this.getModel().get('bb') }</li>
          }) } cqsdc { return "cd"; }
      </ul>
    )
  }
});

return (
  <li>
    <ModalTrigger test="xs" test="dccd" modal={ <InviteForm> }>
      <a href="#">Invite Representative</a>
    </ModalTrigger>
  </li>
);


return (
  <li>
    <ModalTrigger modal={ <InviteForm auth={this.props.auth} /> }>
      <a href="#">Invite Representative</a>
    </ModalTrigger>
  </li>
);

var MyClass = react.createClass({
  render: function() {
    return
    <div>
      <OtherComponent/>
      <OtherComponent class="toto"
                      {...props}
                      id="fr" />
    </div>;
  }
});
