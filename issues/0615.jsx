import React from 'react';
import {Row, Col, Glyphicon, Input,
        Breadcrumb, BreadcrumbItem} from 'react-bootstrap';

export default React.createClass({
  render: function () {
    const breadcrumb = (
      <Breadcrumb>
        <BreadcrumbItem href="#">
          <Glyphicon glyph="home"/>
        </BreadcrumbItem>
        { (this.props.scope.get('type') !== "home") ?
          <BreadcrumbItem href="#/messages/{this.props.scope.get('scope') || this.props.scope.get('parent')}">
            {this.props.scope.get('scope') || this.props.scope.get('parent')}
          </BreadcrumbItem> : null}
          { this.props.scope.get('type') === "topic" ?
            <BreadcrumbItem href="#/messages/{this.props.scope.get('value')}">
              {this.props.scope.get('value')}
            </BreadcrumbItem> : null
          }
      </Breadcrumb>
    );
    const innerGlyphicon = <Glyphicon glyph="search" />;

    return (
      <Row className="Header">
        <Col xs={6}>
          {breadcrumb}
        </Col>
        <Col className="search-bar" xs={6}>
          <Input type="text" addonBefore={innerGlyphicon} />
        </Col>
      </Row>
    );
  }
});
