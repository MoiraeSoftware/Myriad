# How to build:
1. Make sure you have version of `dotnet` specified in [global.json](global.json)
2. Run `dotnet tool restore` to install required local tools
3. Run `dotnet paket restore` to restore Paket-managed dependencies
4. Run `dotnet build build.proj -c Release -t:Build`

# How to build documentation:
The documentation is built using Hugo and automatically deployed to GitHub Pages on every push to the master branch.

To build the documentation locally:
1. Install [Hugo](https://gohugo.io/getting-started/installing/) (version 0.126.3 or later)
2. Navigate to the `docs` directory: `cd docs`
3. Initialize the theme submodule: `git submodule update --init --recursive`
4. Build the site: `hugo --minify`
5. The generated site will be in the `docs/public` directory
6. To preview locally: `hugo server` (available at http://localhost:1313/Myriad/)

The documentation is automatically published to https://moiraesoftware.github.io/Myriad/ via GitHub Actions.

# How to debug:
- uncomment line `<!-- <MyriadSdkWaitForDebugger>true</MyriadSdkWaitForDebugger> -->` in test\Myriad.IntegrationPluginTests\Myriad.IntegrationPluginTests.fsproj
- `dotnet build test\Myriad.IntegrationPluginTests -v n` the test project
- myriad will start but wait for a debugger to attach (so the dotnet build is not yet done)
- in VSCode/Rider, attach to the process that contains myriad.dll (I added a vscode task in debugger window, ready to use)
- debugger will stop at beginning of the main of myriad (edited)

With this, you can just modify a test, and attach in debugger to see it, no need to copy the cli args
