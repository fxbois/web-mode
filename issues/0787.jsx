export default class Header extends Component {
  render() {
    return (
      <div {...attributes}
           var={}
           onClick={this.onClicked}
           onKeyDown={this.onKeyDowned}>
        {this.getChildren()}
      </div>);
  }
}
