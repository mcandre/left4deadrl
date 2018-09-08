# Left4DeadRL - a Left4Dead Roguelike

# EXAMPLE

```console
$ left4deadrl
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

# DOCUMENTATION

https://hackage.haskell.org/package/left4deadrl

# RUNTIME REQUIREMENTS

(None)

# BUILDTIME REQUIREMENTS

* [GHC Haskell](http://www.haskell.org/) 8+
* [happy](https://hackage.haskell.org/package/happy) (e.g., `cabal install happy`)

# BUILD

```console
$ cabal update
$ cabal install --force-reinstalls --only-dependencies --enable-documentation
$ cabal install --force-reinstalls --only-dependencies --enable-tests
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
