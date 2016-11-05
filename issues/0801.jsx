import React from 'react'

class Example extends Component {
  render() {
    return (
      <div>
        {this.props.condition ? (
           <ContentIfTrue />
         ) : (
          <ContentIfFalse />
         )}
      </div>
    )
  }
}
