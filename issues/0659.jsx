function foo() {
  return <label>
    First Name:
    <Input name="name"
           type="text"
           validators={[
             required('You must supply a first name!'),
             (value) => value > 15 ? 'too long!': null
           ]} />
  </label>;
}
