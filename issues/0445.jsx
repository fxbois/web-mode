function hello() {
  var x = 1;
}

function foo() {
  return (
    <div>
      <h1>Events</h1>
      <table>
        <thead>
          <tr>
            <th>{/*Sign up link*/}</th>
            <th>{/*Name */}</th>
            <th>Places available</th>
          </tr>
        </thead>
        <tbody>
          {events}
        </tbody>
      </table>
    </div>
  );
}
