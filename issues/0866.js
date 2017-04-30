import gql from 'graphql-tag';

const query = gql`
  {
    user(id: 5) {
      firstName
      lastName
    }
  }
`

new Vue({

  el: '#toto',

  template: `
    <div>
      <span>
        toto
        <hr/>
        toto
      </span>
    </div>
  `

});
