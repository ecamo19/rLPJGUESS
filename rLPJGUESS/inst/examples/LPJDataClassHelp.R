\dontrun{
### Serial run ###
# After running rLPJ or getLPJData
result <-   runLPJ(mainDir,  settings= settings)

  result

    class              : LPJData
    run directory      : /some/absolute/paht/runDirectory
    LPJ template 1     : global.ins
    LPJ template 2     : global_cf.ins
    grid cells         : 1 cell(s)
    0  Somewhere
    LPJ model outputs  : 39 output(s)
    aaet agpp aiso amon anpp cflux clitter cmass cpool cton_leaf dens
    firert fpc speciesheight lai maet mevap mgpp mintercep miso mlai mmon
    mnee mnpp mpet mra mrh mrunoff mwcont_lower mwcont_upper nflux ngases
    nlitter nmass npool nsources nuptake runoff vmaxnlim

### Parallel run ###
# After running runLPJ (20 simulations)
str(result,1)

    List of 20

    $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots

    $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots

    $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots

    $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots

    $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots

    $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots

    [...]


## LPJData options
# Access the model outputs
result["dataTypes"]

# Access speficic outputs
result["lai"]

# Acces the runInfo
result["runInfo"]

# Access specific runInfo like template name
result["template1"]

# Access specific runInfo like template content
result["template1Mem"]

# Check what other information is available in each slot
names(result["runInfo"])
names(result["dataTypes"])

}
