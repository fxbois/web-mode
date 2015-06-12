React.createClass({
  render: function() {
    const elem = [
      <CSSTransitionGroup transitionName='reveal'>{this.renderModalOnTop()}</CSSTransitionGroup>
    ];
    return elem[0];
  }
});
