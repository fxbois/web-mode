web-mode.el  [![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/thing/1849746/web-mode-el)
=========

web-mode.el is an emacs major mode for editing **web templates** aka HTML files embedding parts (CSS/JavaScript) and blocks (pre rendered by client/server side engines).

web-mode.el is compatible with many template engines: PHP, JSP, ASP, Django, Twig, Jinja(2), ERB, FreeMarker, Velocity, Cheetah, Smarty, CTemplate, Mustache, Blade, ErlyDTL, Go Template, Dust.js, Google Closure (soy),  etc.

More infos on http://web-mode.org/

![ScreenShot](http://web-mode.org/web-mode.png?v=2)

## Contribution

Install [Cask](https://github.com/cask/cask) if you haven't already,
then:

    $ cd /path/to/web-mode.el
    $ cask

Run the unit tests with:

    $ make unit

The integration tests with:

    $ make ecukes

And all tests with:

    $ make
