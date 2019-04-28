import { html, render } from 'lit-html';

const myTemplate = (el) => html`
  <style>
   h1 {
     font-size: ${el.titleSize}em;
   }
   .square {
     height: ${el.size}px;
     width: ${el.size}px;
     background-color: ${el.color};
   }
  </style>
  <div>
    <h1> My Web Component </h1>
    <div class="square">
    </div>
  </div>
`

export class MyComponent extends HTMLElement {
  constructor() {
    super();
    this.root = this.attachShadow({mode: 'open'});
    this.color = "blue";
    this.size = 12
    this.titleSize = 1.2;
    this.render();
  }

  render() {
    render(myTemplate(this), this.root)
  }
}
customElements.define('my-component', MyComponent)
