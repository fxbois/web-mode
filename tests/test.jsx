/** @jsx React.DOM **/
var React = require('react/addons');
var Component = require('./component');

export default React.createClass({
  getInitialState() {
    return { num: this.getRandomNumber() };
  },

  getRandomNumber(): number {
    return Math.ceil(Math.random() * 6);
  },
  render(): any {
    return
    <div>
      Your dice roll:
      {this.state.num}
    </div>;
  }
});


'use strict';

var React = require('react'),
    Explore = require('./components/Explore'),
    DocumentTitle = require('react-document-title'),
    { RouteHandler } = require('react-router'),
    { PropTypes } = React;

var App = React.createClass({
  propTypes: {
    params: PropTypes.object.isRequired,
    query: PropTypes.object.isRequired
  },

  render() {
    return (
      <DocumentTitle title='Sample App'>
        <div className='App'>
          <Explore />
          <hr />
          <RouteHandler {...this.props} />
        </div>
      </DocumentTitle>
    );
  }
});

module.exports = App;

module.exports = React.createClass({
  render() {
    return (
      <div>
        <Component attr="cqs" xs="xs"
                   xsxs="sxx" />
        <Component />
        <Component />
      </div>
    );
  }
});

Landing = React.createClass({
  render: function() {
    return (
      <ul>
        {for(e in list){
          console.log("not aligned");
         }}
      </ul>
    );
  }
});

var Component = React.createClass({
  render : function() {
    var stuff = this.props.formElements.map(function(elem) {
      switch(elem) {
        case "textarea" :
          return ( <textarea rows="4" /> );
          break;
        case "text" :
          return ( <input type="text" /> );
        default :
          return null;
      }
    }
    );
  }
});

define(function (require) {
  'use strict';

  var React = require('react');
  var ModalTrigger = require('react-bootstrap').ModalTrigger;
  var InviteForm = require('components/InviteForm');

  var InviteLink = React.createClass({
    render: function() {
      if (this.props.auth.loggedIn()) {
        return (
          <li>
            <ModalTrigger modal={<InviteForm auth={this.props.auth} />}>
              <a href="#">Invite Representative</a>
            </ModalTrigger>
          </li>
        );
      } else {
        return <span></span>;
      }
    }
  });

  return InviteLink;
});

function hello() {

  return (
    <div>
      {somevar}
      <div className="row">
        <div className="col-md-4">
          Column1
        </div>
        <div className="col-md-4">
          Column2
        </div>
      </div>
    </div>
  );

}
