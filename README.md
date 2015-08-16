Lua linter. 

Goals:

- Style checking
- Expression replacement suggestions à la [hlint](http://community.haskell.org/~ndm/hlint/)
- Dynamic analysis, e.g. shadowing, uninitialized variables, etc; à la [luacheck](https://github.com/mpeterv/luacheck)
- Type inference and type checking (ha)

Todos:

- Everything

_____

##### Building

    stack build

##### Installing to local PATH

    stack install

##### Running

    llint <filename>
    
or, if built but not installed,

    stack exec llint <filename>

##### Testing

    stack test
