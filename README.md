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
