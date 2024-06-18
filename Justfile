update:
    @echo "Updating submodules..."
    @git submodule foreach 'git pull origin $(git symbolic-ref --short HEAD)'
    @echo "Building drones..."
    @make build
    @echo "Done!"