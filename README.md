## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Tests

```
stack test --flag github-yesod:library-only --flag github-yesod:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.

## Diego Molamphy - 18326635

* Unfortunately, I wasn't able to link the API to the yesod webpage properly to visual the data from the API. In hindsight, trying to do this project in Haskell was a mistake due to my general inexperience in working with 
  Haskell. This made the project partiularly tricky. Using Python for this project, which I am more familiar with, would have probably been a better choice.

* As a result, I have created a yesod webpage that demonstates what I would have liked to achieve with this project. I wanted my project to be able to get a repo from a GitHub user and display the name of each contributor
  to the repository and the number of commits they made to that repository. I have visualized this data using an ordered barchart (highest number of commits to lowest number of commits) with the names of the contributors 
  on the x-axis and the number of commits by each contributor on the y-axis. The repository name is the header of the webpage. The data is randomized in home.hs. 

* Although we know that the number of commits by a developer isn't the be all and end all in measuring the sofware engineering process,  I believe had I successfully implemented my idea for this project it would have provided 
  some valuable insights.

* 1. stack build
  2. stack run
  3. Browse to localhost:3000
