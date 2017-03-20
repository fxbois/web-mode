import * as React from 'react';

interface Props {
  name: string
}

class MyThing extends React.Component<Props, {}> {
  render() {
    return <span>hi</span>;
  }
}

export default MyThing as React.ComponentClass<Props>;
