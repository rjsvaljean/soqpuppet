autocmd BufWritePost *.hs GhcModCheckAndLintAsync

map <buffer> <LocalLeader>t :GhcModType<Return>
map <buffer> <LocalLeader>T :GhcModTypeInsert<Return>
map <buffer> <LocalLeader>i :GhcModInfo<Return>

let g:ghcmod_ghc_options = ['-package-db=' . fnamemodify('cabal-dev/packages-7.6.3.conf', ':p')]
let b:ghc_staticoptions = '-package-db=' . fnamemodify('cabal-dev/packages-7.6.3.conf', ':p')

" CtrlP options
set wildignore+=*cabal-dev/*,*dist/*

