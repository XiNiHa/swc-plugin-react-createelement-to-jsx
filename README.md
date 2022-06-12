# swc-plugin-react-createelement-to-jsx

[![npm version](https://badge.fury.io/js/swc-plugin-react-createelement-to-jsx.svg)](https://badge.fury.io/js/swc-plugin-react-createelement-to-jsx)

Transforms React.createElement calls to JSX syntax.

It's mostly just a direct port of [babel-plugin-transform-react-createelement-to-jsx](https://github.com/flying-sheep/babel-plugin-transform-react-createelement-to-jsx/blob/master/src/index.js) to SWC.

**Note that SWC itself doesn't support emitting JSX in output files and then just transforms it with the React JSX transform. Therefore JSXes produced by this plugin should be consumed by other plugins before SWC apply the React JSX transform. Otherwise it'll have no effect.**
