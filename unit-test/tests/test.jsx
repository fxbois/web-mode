/** @jsx React.DOM **/
var React = require('react/addons');
var Component = require('./component');

module.exports = React.createClass({
  render() {
    return (
      <div>
        <Component />
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
