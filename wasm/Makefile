.PHONY: webapp/app.js
webapp/app.js: webapp/basic.js webapp/app.tmpl.js
	sed -i '.bak' 's/import.*$$//g' webapp/basic.js
	sed -i '.bak' 's/export\(.*\)$$/\1/g' webapp/basic.js
	sed '7r webapp/basic.js' webapp/app.tmpl.js > webapp/app.js

webapp/basic_bg.wasm webapp/basic.js: ../target/wasm32-unknown-unknown/release/basic.wasm
	wasm-bindgen $< --out-dir webapp

../target/wasm32-unknown-unknown/release/basic.wasm:
	cargo build --target wasm32-unknown-unknown --release

