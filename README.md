# Left4DeadRL - a Left4Dead Roguelike

# EXAMPLE

```console
$ git submodule init
$ git submodule update
$ cd hscharm/
hscharm$ git submodule init
hscharm$ git submodule update
hscharm$ cd ..
$ make

$ ./left4deadrl
    #    #  #                      #        # #   #                 #         #
 #  zz    #   z# # z  #         #     z                   #                # #
                       #       #   #                   #   #
    #                #      #       #          # # #   #    #
     #    z #             #        #                          #          z   #
           #     #      #         ##        z            ###  #      #
    #  ###z   #    #   z     #     #     #        #           #     #     #
         #       #       #           #         z            #         z  #
 z            z     #            z       #             #     #  z        #
    #                     #     z     #          z                #  #  #   #
[     z                #                                 #         #   #       @
   # #    #     z  #   #    z                 #   z    z # # #      # # #
             #                         zz                       #z     z #
  #    #      #      #                             #        #       ## #
     #                  # z        #                                   #
        #       #     #  z #  # #z                      z                #
   z                    #          #   #                z      #        z     #
   #  #                   ##     z      #                            # z
       ##    z #       ##       #   z#                #     #  #     #  #
    z       #       #                   #                    #      #
       #z                #           #     #              ##                  #


                    Get to the safehouse on the other side.
```

# RUNTIME REQUIREMENTS

(None)

# BUILDTIME REQUIREMENTS

* [GHC Haskell](http://www.haskell.org/) 8+
* [hscharm](https://github.com/mcandre/hscharm)

## Recommended

* [shake](https://shakebuild.com/) (e.g., `cabal install shake`)
* [hlint](https://hackage.haskell.org/package/hlint) (e.g., `cabal install happy; cabal install hlint`)

# BUILD

```console
$ cabal install --only-dependencies --enable-documentation
$ cabal install --only-dependencies --enable-tests
$ shake
```

# LINT

```console
$ shake lint
```

# PUBLISH

```console
$ shake publish
```
