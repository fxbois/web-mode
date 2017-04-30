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
      titi
      <span>
        toto
        <img src="/toto.gif"
             data-x="3"
        />
        <hr/>
        toto
      </span>
    </div>
  `

});
