with import <nixpkgs-unstable> {
    crossSystem = {
      config = "wasm32-unknown-wasi";
      useLLVM = true;

      # config = "wasm32-wasi";
      # arch = "wasm32";
      # libc = null;
      # useLLVM = true;
      # disableDynamicLinker = true;
      # thread-model = "single";
    };
};

let host-nixpkgs = import <nixpkgs-unstable> {};
in
  mkShell {
      nativeBuildInputs = [ host-nixpkgs.wabt ];
  }
