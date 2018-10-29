function hello() {

  return (
    <input
      type="file"
      ref={component => {
        this.inputFile = component;
      }}
    />
  );
}
