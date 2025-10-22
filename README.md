Playground Ocaml
----------------
Simple, Fast interactive Playground for functional stuffs.

### Syntax 
    - Variable is immutable by default : need ref / := / !var for mut mode.
    - comma ; in the end of every statement.
    - comment as (* COMMENT *).
      
    - let .. in : for every declare.
    - let rec .. in : recursive fn.
    
    - while .. do .. done : as loop.
    - if .. then .. else = fixed default.
      
    - % is mod in Ocaml.
    - ^ is string concat.
    
    - Ctypes.CArray : for c interop / FFI stuffs -> get/set/length()
    
    - Ocaml use Dune to build / exec / run project.
    - Ocaml use Opam as package manager.

### Installation on Ubuntu 25.04
I assume a stable version by now is `ocaml 5.4.0` + dev-repo of `ocamlformat ocaml-lsp-server` 
1. Install via APT instead of curl :

        sudo apt install opam

2. Fix Ocaml Language Server in Editors :

        opam pin add ocaml-lsp-server https://github.com/ocaml/ocaml-lsp.git

3. Update `ocamlformat` to fit `5.4.0` : 

        opam pin --dev-repo ocamlformat


### Experience 
**Ocaml** have some quite tricky stuffs when I transfer from imperative language. Even when I have used F# like 10 years ago with more forgiveness, still cost me a night to figure out differences. Perhaps Rust eased a lot of things for me like **mut** stuffs. Although a lot of example/pratices were shorter than Odin, they tend to take me more time to fix stuffs I already have forgotten about ML, again, Rust spoiled me way too much for its convenient syntax rules (except borrow-checker & eyes-hurt stuffs).
