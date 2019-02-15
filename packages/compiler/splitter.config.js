const path = require("path");

module.exports = {
    entry: resolve("src/Ikigai.Compiler.fsproj"),
    outDir: resolve("build"),
    babel: {
        plugins: ["@babel/plugin-transform-modules-commonjs"]
    },
    //   fable: {
    //     define: ["DEBUG"]
    //   },
    // allFiles: true
    //   cli: { path: resolve("../../Fable/src/Fable.Cli") },
};

function resolve(p) {
    return path.join(__dirname, p);
}