return (
  <Inline {... this.props}>
    <a href={this.props.href} style={{textDecoration: 'none'}}>
      {this.props.children}
    </a>
  </Inline>
);
