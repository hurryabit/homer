with import <nixpkgs-unstable> {
    crossSystem = {
      config = "wasm32-unknown-wasi";
      useLLVM = true;
    };
};

let host-nixpkgs = import <nixpkgs-unstable> {};
in
  mkShell {
      nativeBuildInputs = [ host-nixpkgs.wabt ];
  }
