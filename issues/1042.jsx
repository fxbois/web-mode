class NS {
  render() {
    return (
      <div>
        {loop.map(item => {
          return (
            <Tag
              key={item.id}
            >
              <Link to={link}>{item.name}</Link>
            </Tag>
          );
        })}
      </div>
    );
  }
}
