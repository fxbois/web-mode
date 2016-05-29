export default React.createClass({
  getInitialState() {
    return { num: this.getRandomNumber() };
  },

  getRandomNumber(): number {
    return Math.ceil(Math.random() * 6);
  },
  render(): any {
    return
    <Input
      inputProps={{
        type: 'text',
        name: 'first-name',
        placeholder: 'First name',
        required: true
      }}
      validator={Input.validators['not-empty']}
    />;
  }
});
