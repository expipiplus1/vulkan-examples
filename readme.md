# *These examples have moved to the [vulkan repo](https://github.com/expipiplus1/vulkan)*


# vulkan-examples

Some examples for using the
[vulkan](https://hackage.haskell.org/package/vulkan) bindings.

At the moment it just creates and destroys a Vulkan instance; it'll print '0'
to the terminal if there were no problems.

## Building

This requires GHC 8.

I've included a couple of `.nix` files to set up this environment. There's no
Nix package for Vulkan at the moment, so you'll have to do a couple of really
horrible things to get this to build. These are the steps on my Linux machine
(having installed the LunarG SDK and vulkan driver)

    nix-shell
    cabal configure --extra-lib-dirs=/usr/lib/x86_64-linux-gnu
    cabal build
    LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu ./dist/build/vulkan-example/vulkan-example

The `vulkan` package currently takes ages and ages to build without -O0, so you
might want to specify that too.

### Building on Ubuntu with Stack

Note, you need modern drivers to support vulkan api,
e.g. `nvidia-367` that is available at `ppa:graphics-drivers/ppa` currently.
If your drivers are ok, install `libvulkan-dev` and run examples using `stack`:
```bash
sudo apt-get install libvulkan-dev
# run this from a folder containing cloned vulkan-examples
stack setup
stack install
```

## Contributing

It would be awesome if you could try and get this to work on your system and
raise an issue (or if you're amazing a pull request) if it doesn't work for
you.

I can be reached by email or as `jophish` on Freenode.
