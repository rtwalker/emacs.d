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

font:
    @curl -L -o assets/all-ligatures.txt https://raw.githubusercontent.com/fabrizioschiavi/pragmatapro/refs/heads/master/useful_files/All_ligatures.txt
