# Shelly Font Installer

I pretty much just use this to install source-code-pro for dev environments: 
<pre> ./shelly_font_installer.hs install source-code-pro </pre>

Project for learning Haskell libs...
* [shelly](http://hackage.haskell.org/package/shelly)
* [optparse-applicative](https://github.com/pcapriotti/optparse-applicative)
* [Text.PrettyPrint.ANSI.Leijen](http://hackage.haskell.org/package/ansi-wl-pprint-0.6.8.2/docs/Text-PrettyPrint-ANSI-Leijen.html)
* [mtl](http://hackage.haskell.org/package/mtl)
* [MonadCatch](http://hackage.haskell.org/package/exceptions-0.10.0/docs/Control-Monad-Catch.html)
* basic lenses

## Usage
<pre>
shelly font installer - a dumb font installer

Usage: shelly_font_installer.hs COMMAND [--localpath LOCAL FONT PATH]
                                ([--syspath SYSTEM FONT PATH] | [--nosyspath])
  installs fonts from the shell, with way too many Haskell dependencies

Available options:
  --localpath LOCAL FONT PATH
                           Local font
                           path (default: FilePath "/home/bzhang/.local/share/fonts")
  --syspath SYSTEM FONT PATH
                           System font
                           path (default: FilePath "/usr/local/share/fonts")
  --nosyspath              No system font path
  -h,--help                Show this help text

Available commands:
  status                   
  install        
</pre>
