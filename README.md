# ralph-lsp

Language server for Ralph.

Currently supports text document level events and diagnostics.

# Dependency on `ralphc`

Checkout [this lsp-tester](https://github.com/alephium/dev-alephium/tree/lsp_tester) branch and publish it locally.

```shell
sbt publishLocal
```

Update the version for `alephium-ralphc` in this repo's `build.sbt` file with the published version.

# Build the jar

```shell
sbt "project lsp-server; assembly;"
```

Look in `target` folder: `.../ralph-lsp/lsp-server/target/scala-2.13/ralph-lsp.jar`

# Run LSP in VSCode

Update the jar
location [here](plugin-vscode/src/extension.ts).

Run the IDE:

```shell
cd plugin-vscode
code .
```

![img.png](docs/img_2.png)

# Run LSP in IntelliJ (Ultimate)

Update the jar
location [here](plugin-intellij/src/main/scala/org/alephium/ralph/lsp/plugin/intellij/RalphLspServerDescriptor.scala).
Yep, this will eventually be automatically configured via sbt.

Run the IDE:

```shell
sbt "project plugin-intellij; runIDE"
```

## Error highlighting

Note: Currently this is implemented for when files are
opened. See [didOpen()](lsp-server/src/main/scala/org/alephium/ralph/lsp/server/service/RalphTextDocumentService.scala).

![img.png](docs/img.png)

## Code completion

![img.png](docs/img_1.png)
