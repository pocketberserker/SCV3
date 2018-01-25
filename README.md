# SCV3

## 画像

画像は[公式ページ](https://www.kadokawa.co.jp/product/321706000100/)からダウンロードし、`public`ディレクトリに配置してください。

## Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 2.0 or higher
* [node.js](https://nodejs.org) 6.11 or higher
* A JS package manager: [yarn](https://yarnpkg.com) or [npm](http://npmjs.com/)

## Building and running the app

> In the commands below, yarn is the tool of choice. If you want to use npm, just replace `yarn` by `npm` in the commands.

* Install JS dependencies: `yarn install`
* Install F# dependencies: `dotnet restore`
* **Move to `src` folder**: `cd src`
* Start Fable daemon and [Webpack](https://webpack.js.org/) dev server: `dotnet fable yarn-start`
* In your browser, open: http://localhost:8080/

> `dotnet fable yarn-start` (or `npm-start`) is used to start the Fable daemon and run a script in package.json concurrently. It's a shortcut of `yarn-run [SCRIPT_NAME]`, e.g. `dotnet fable yarn-run start`.

If you are using VS Code + [Ionide](http://ionide.io/), you can also use the key combination: Ctrl+Shift+B (Cmd+Shift+B on macOS) instead of typing the `dotnet fable yarn-start` command. This also has the advantage that Fable-specific errors will be highlighted in the editor along with other F# errors.

Any modification you do to the F# code will be reflected in the web page after saving. When you want to output the JS code to disk, run `dotnet fable yarn-build` and you'll get a minified JS bundle in the `public` folder.

## JS Output

This template uses [babel-preset-env](http://babeljs.io/env) to output JS code whose syntax is compatible with a wide range of browsers. Currently it's set to support browsers with at least 1% of market share. To change this (for example, if you don't need to support IE), [replace this line](https://github.com/fable-compiler/fable-templates/blob/7b9352cdaeb77ecd600b45ed4eab2f41c73b85e4/simple/Content/webpack.config.js#L13) with a query understood by [browserl.ist](http://browserl.ist/?q=%3E+1%25).

To replace objects and APIs that may be missing in old browsers, the `index.html` file [submits a request](https://github.com/fable-compiler/fable-templates/blob/7b9352cdaeb77ecd600b45ed4eab2f41c73b85e4/simple/Content/public/index.html#L8) to [cdn.polyfill.io](https://polyfill.io/v2/docs/) that tailors the polyfill according to the user's browser.
