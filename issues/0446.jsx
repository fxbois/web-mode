/*jshint esnext: true*/

export default React.createClass({
  getInitialState() {
    return { num: this.getRandomNumber() };
  },

  getRandomNumber(): number {
    return Math.ceil(Math.random() * 6);
  },
  render(): any {
    return
    <div>
      Your dice roll:
      {this.state.num}
    </div>;
  }
});
