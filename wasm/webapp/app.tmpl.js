let wasm = {};

//
// BEGIN COPY-PASTE from wasm-bindgen generated code with import/export removed.
//

/* CONTENT */

//
// END COPY-PASTE
//

var importObject = {'./basic' : {__wbindgen_throw : __wbindgen_throw}};

function run() {
  const program = document.getElementById("programTxt").value.trim();
  const output = execute(program);

  const outputDiv = document.getElementById("output");
  outputDiv.textContent = output.stdout() + '\n';
  outputDiv.textContent += output.stderr() + '\n';
}

// load wasm code
WebAssembly.instantiateStreaming(fetch('basic_bg.wasm'), importObject)
    .then(obj => {
      // re-export symbols into the `wasm` object
      var exports = obj.instance.exports;
      Object.getOwnPropertyNames(exports).forEach(
          (val, idx, array) => { wasm[val] = exports[val]; });

      const btn = document.getElementById("runBtn");
      console.log("BASIC interpreter loaded");
      btn.onclick = run;
      btn.disabled = false;
    })
    .catch(e => console.error(e));