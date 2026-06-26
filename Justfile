sync:
    @echo "Syncing..."
    @git pull --recurse-submodules --ff-only
    @git submodule sync --recursive
    @git submodule update --init --recursive
    @echo "Building drones…"
    @make build
    @echo "Done!"

update:
    @echo "Updating submodules..."
    @git submodule foreach 'git pull origin $(git symbolic-ref --short HEAD) || :'
    @echo "Building drones..."
    @make build
    @echo "Done!"

pragmatapro-ligatures-file := "https://raw.githubusercontent.com/fabrizioschiavi/pragmatapro/refs/heads/master/useful_files/All_ligatures.txt"
font:
    @curl -L -o assets/all-ligatures.txt {{pragmatapro-ligatures-file}}

treesit: tree-sitter-dockerfile tree-sitter-julia tree-sitter-just tree-sitter-lua tree-sitter-nix tree-sitter-python tree-sitter-rust tree-sitter-toml tree-sitter-yaml

tree-sitter-dockerfile: (_install-tree-sitter-grammar "dockerfile")

tree-sitter-julia: (_install-tree-sitter-grammar "julia")

tree-sitter-just: (_install-tree-sitter-grammar "just")

tree-sitter-lua: (_install-tree-sitter-grammar "lua")

tree-sitter-nix: (_install-tree-sitter-grammar "nix")

tree-sitter-python: (_install-tree-sitter-grammar "python")

tree-sitter-rust: (_install-tree-sitter-grammar "rust")

tree-sitter-toml: (_install-tree-sitter-grammar "toml")

tree-sitter-yaml: (_install-tree-sitter-grammar "yaml")

emacs_tree_sitter_home := `pwd` / "tree-sitter"
tree-sitter-abi-version := "15"

[macos]
_install-tree-sitter-grammar lang libname=("libtree-sitter-" + lang) dylib=(libname + ".dylib"):
    #!/usr/bin/env sh
    printf "%-.40s " "Compiling {{libname}} ................................"
    cd tree-sitter/tree-sitter-{{lang}}/src
    zig cc -fPIC -c -I. parser.c
    zig cc -fPIC -c -I. scanner.c
    zig cc -fPIC -shared *.o -o {{dylib}}
    mv {{dylib}} {{emacs_tree_sitter_home}}
    echo "Installed!"

[linux]
_install-tree-sitter-grammar lang libname=("libtree-sitter-" + lang) soname=(libname + ".so") :
    #!/usr/bin/env sh
    printf "%-.40s " "Compiling {{libname}} ................................"
    cd tree-sitter/tree-sitter-{{lang}}/src
    zig cc -fPIC -c -I. parser.c
    zig cc -fPIC -c -I. scanner.c
    zig cc -fPIC -shared *.o -o {{soname}}
    mv {{soname}} {{emacs_tree_sitter_home}}/{{soname}}.{{tree-sitter-abi-version}}
    echo "Installed!"
